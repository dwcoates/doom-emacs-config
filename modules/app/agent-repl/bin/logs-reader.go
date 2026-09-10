// logs-reader.go parses, merges, filters, follows, and harvests agent-repl JSONL.
// bin/logs.sh owns path and workspace discovery; this helper owns record semantics.
package main

import (
	"bufio"
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"os/signal"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"syscall"
	"text/tabwriter"
	"time"
)

const retainedGenerations = 5

var levelRank = map[string]int{"debug": 0, "info": 1, "warn": 2, "error": 3}
var runtimeNames = map[string]struct{}{"emacs": {}, "daemon": {}, "shim": {}, "webapp": {}, "sidecar": {}, "store": {}}

type repeatedFlag []string

func (f *repeatedFlag) String() string { return strings.Join(*f, ",") }
func (f *repeatedFlag) Set(value string) error {
	*f = append(*f, value)
	return nil
}

type options struct {
	mode              string
	since             string
	until             string
	minLevel          string
	runtimes          string
	follow            bool
	bases             repeatedFlag
	baseRuntimes      repeatedFlag
	baseWorkspaceIDs  repeatedFlag
	baseWorkspaceDirs repeatedFlag
	baseKinds         repeatedFlag
	harvestFrom       string
	harvestTo         string
}

type sink struct {
	base             string
	runtime          string
	workspaceID      string
	workspaceDir     string
	canonicalSymlink bool
}

type sinkFinding struct {
	sink    sink
	message string
}

type record struct {
	Timestamp          string         `json:"timestamp"`
	Runtime            string         `json:"runtime"`
	Level              string         `json:"level"`
	Verbosity          string         `json:"verbosity"`
	Operation          string         `json:"operation"`
	Message            string         `json:"message"`
	Context            map[string]any `json:"context"`
	WorkspaceDir       string         `json:"workspace_dir"`
	WorkspaceID        string         `json:"workspace_id"`
	AgentReplSessionID string         `json:"agent_repl_session_id"`
	ClaudeSessionID    string         `json:"claude_session_id"`
	RequestID          string         `json:"request_id"`
	ConnectionID       string         `json:"connection_id"`
	PID                *int64         `json:"pid"`

	instant time.Time
	raw     []byte
	seq     int64
}

type filters struct {
	since    *time.Time
	until    *time.Time
	minLevel int
	runtimes map[string]struct{}
}

type followState struct {
	sink   sink
	path   string
	info   os.FileInfo
	offset int64
	line   int
}

func main() {
	if err := run(); err != nil {
		fmt.Fprintf(os.Stderr, "logs-reader: %v\n", err)
		os.Exit(2)
	}
}

func run() error {
	var opts options
	flag.StringVar(&opts.mode, "mode", "human", "output mode: human, json, or harvest")
	flag.StringVar(&opts.since, "since", "", "inclusive lower time bound")
	flag.StringVar(&opts.until, "until", "", "inclusive upper time bound")
	flag.StringVar(&opts.minLevel, "level", "debug", "minimum level")
	flag.StringVar(&opts.runtimes, "runtime", "", "comma-separated runtime filter")
	flag.BoolVar(&opts.follow, "follow", false, "follow current generations")
	flag.StringVar(&opts.harvestFrom, "harvest-from", "", "harvest lower bound")
	flag.StringVar(&opts.harvestTo, "harvest-to", "", "harvest upper bound")
	flag.Var(&opts.bases, "base", "canonical current-generation path")
	flag.Var(&opts.baseRuntimes, "base-runtime", "runtime owning the corresponding base")
	flag.Var(&opts.baseWorkspaceIDs, "base-workspace-id", "workspace ID owning the corresponding base")
	flag.Var(&opts.baseWorkspaceDirs, "base-workspace-dir", "workspace directory owning the corresponding base")
	flag.Var(&opts.baseKinds, "base-kind", "base kind: file or symlink")
	flag.Parse()
	if flag.NArg() != 0 {
		return fmt.Errorf("unexpected positional argument %q", flag.Arg(0))
	}
	if len(opts.bases) == 0 {
		return errors.New("no log paths were selected")
	}
	sinks, err := makeSinks(opts)
	if err != nil {
		return err
	}
	if opts.mode != "human" && opts.mode != "json" && opts.mode != "harvest" {
		return fmt.Errorf("unknown output mode %q", opts.mode)
	}
	if opts.follow && opts.mode == "harvest" {
		return errors.New("harvest cannot follow")
	}

	now := time.Now()
	if opts.mode == "harvest" {
		opts.since, opts.until = opts.harvestFrom, opts.harvestTo
		opts.minLevel = "warn"
	}
	filter, err := makeFilters(opts, now)
	if err != nil {
		return err
	}

	records, findings, readable, sequence, states, err := readInitial(sinks, filter)
	if err != nil {
		return err
	}
	sortRecords(records)
	if err := emit(records, findings, opts.mode); err != nil {
		return err
	}
	if readable == 0 {
		return errors.New("none of the selected log sinks could be read")
	}
	if !opts.follow {
		return nil
	}
	return follow(opts.mode, states, filter, sequence)
}

func makeSinks(opts options) ([]sink, error) {
	counts := map[string]int{
		"--base":               len(opts.bases),
		"--base-runtime":       len(opts.baseRuntimes),
		"--base-workspace-id":  len(opts.baseWorkspaceIDs),
		"--base-workspace-dir": len(opts.baseWorkspaceDirs),
		"--base-kind":          len(opts.baseKinds),
	}
	for name, count := range counts {
		if count != len(opts.bases) {
			return nil, fmt.Errorf("%s count %d does not match --base count %d", name, count, len(opts.bases))
		}
	}
	sinks := make([]sink, 0, len(opts.bases))
	for index, base := range opts.bases {
		runtime := opts.baseRuntimes[index]
		if _, ok := runtimeNames[runtime]; !ok {
			return nil, fmt.Errorf("base %q has unknown runtime %q", base, runtime)
		}
		kind := opts.baseKinds[index]
		if kind != "file" && kind != "symlink" {
			return nil, fmt.Errorf("base %q has unknown kind %q", base, kind)
		}
		workspaceID, workspaceDir := opts.baseWorkspaceIDs[index], opts.baseWorkspaceDirs[index]
		if kind == "symlink" && workspaceDir == "" {
			return nil, fmt.Errorf("workspace base %q has no workspace directory", base)
		}
		if kind == "file" && (workspaceID != "" || workspaceDir != "") {
			return nil, fmt.Errorf("central base %q carries workspace attribution", base)
		}
		sinks = append(sinks, sink{base, runtime, workspaceID, workspaceDir, kind == "symlink"})
	}
	return sinks, nil
}

func makeFilters(opts options, now time.Time) (filters, error) {
	minimum, ok := levelRank[opts.minLevel]
	if !ok {
		return filters{}, fmt.Errorf("level must be debug, info, warn, or error, not %q", opts.minLevel)
	}
	from, err := parseBound(opts.since, now, true)
	if err != nil {
		return filters{}, fmt.Errorf("invalid lower bound %q: %w", opts.since, err)
	}
	to, err := parseBound(opts.until, now, false)
	if err != nil {
		return filters{}, fmt.Errorf("invalid upper bound %q: %w", opts.until, err)
	}
	if from != nil && to != nil && from.After(*to) {
		return filters{}, errors.New("lower time bound is after upper time bound")
	}
	runtimes := make(map[string]struct{})
	if opts.runtimes != "" {
		for _, runtime := range strings.Split(opts.runtimes, ",") {
			if runtime == "" {
				return filters{}, errors.New("runtime list contains an empty value")
			}
			runtimes[runtime] = struct{}{}
		}
	}
	return filters{since: from, until: to, minLevel: minimum, runtimes: runtimes}, nil
}

func parseBound(value string, now time.Time, durationAllowed bool) (*time.Time, error) {
	if value == "" {
		return nil, nil
	}
	if durationAllowed {
		if duration, err := time.ParseDuration(value); err == nil {
			if duration <= 0 {
				return nil, errors.New("duration must be positive")
			}
			instant := now.Add(-duration)
			return &instant, nil
		}
	}
	instant, err := time.Parse(time.RFC3339Nano, value)
	if err != nil {
		if durationAllowed {
			return nil, errors.New("expected RFC3339 or a positive Go duration such as 15m")
		}
		return nil, errors.New("expected RFC3339")
	}
	return &instant, nil
}

func readInitial(sinks []sink, filter filters) ([]record, []sinkFinding, int, int64, []followState, error) {
	var records []record
	var findings []sinkFinding
	var sequence int64
	states := make([]followState, 0, len(sinks))
	seen := make(map[string]struct{})
	readable := 0
	for _, selectedSink := range sinks {
		files, current, discovered := generationFiles(selectedSink)
		findings = append(findings, discovered...)
		state := followState{sink: selectedSink, path: current, line: 1}
		sinkReadable := false
		for _, path := range files {
			if _, duplicate := seen[path]; duplicate {
				sinkReadable = true
				continue
			}
			loaded, next, nextLine, err := readFile(path, sequence, filter)
			if err != nil {
				if unreadable, ok := err.(*unreadableLogError); ok {
					findings = append(findings, unreadableFinding(selectedSink, path, unreadable.err))
					continue
				}
				return nil, nil, 0, 0, nil, err
			}
			seen[path] = struct{}{}
			sinkReadable = true
			records = append(records, loaded...)
			sequence = next
			if path == current {
				state.line = nextLine
			}
		}
		if current != "" {
			info, err := os.Stat(current)
			if err != nil {
				findings = append(findings, unreadableFinding(selectedSink, current, err))
			} else {
				state.info, state.offset = info, info.Size()
			}
		}
		if sinkReadable {
			readable++
		}
		states = append(states, state)
	}
	return records, findings, readable, sequence, states, nil
}

func generationFiles(selectedSink sink) ([]string, string, []sinkFinding) {
	current := selectedSink.base
	var findings []sinkFinding
	if selectedSink.canonicalSymlink {
		info, err := os.Lstat(selectedSink.base)
		if os.IsNotExist(err) {
			return nil, "", []sinkFinding{canonicalAbsentFinding(selectedSink)}
		}
		if err != nil {
			return nil, "", []sinkFinding{canonicalUnreadableFinding(selectedSink, err)}
		}
		if info.Mode()&os.ModeSymlink == 0 {
			return nil, "", []sinkFinding{{selectedSink, "sink not a symlink: " + selectedSink.base}}
		}
		target, err := os.Readlink(selectedSink.base)
		if err != nil {
			return nil, "", []sinkFinding{canonicalUnreadableFinding(selectedSink, err)}
		}
		// WHY: EvalSymlinks loses the named target when it is absent, which is
		// exactly the expected sink state this reader must report and continue past.
		if !filepath.IsAbs(target) {
			target = filepath.Join(filepath.Dir(selectedSink.base), target)
		}
		current = filepath.Clean(target)
	}
	files := make([]string, 0, retainedGenerations+1)
	for generation := retainedGenerations; generation >= 1; generation-- {
		path := current + "." + strconv.Itoa(generation)
		if present, finding := regularFilePresent(selectedSink, path, false); finding != nil {
			findings = append(findings, *finding)
		} else if present {
			files = append(files, path)
		}
	}
	if present, finding := regularFilePresent(selectedSink, current, true); finding != nil {
		findings = append(findings, *finding)
	} else if present {
		files = append(files, current)
		return files, current, findings
	}
	return files, "", findings
}

func regularFilePresent(selectedSink sink, path string, required bool) (bool, *sinkFinding) {
	info, err := os.Stat(path)
	if os.IsNotExist(err) {
		if required {
			finding := absentFinding(selectedSink, path)
			return false, &finding
		}
		return false, nil
	}
	if err != nil {
		finding := unreadableFinding(selectedSink, path, err)
		return false, &finding
	}
	if !info.Mode().IsRegular() {
		finding := unreadableFinding(selectedSink, path, errors.New("not a regular file"))
		return false, &finding
	}
	return true, nil
}

func sinkLocation(selectedSink sink, path string) string {
	if selectedSink.canonicalSymlink {
		return selectedSink.base + " -> " + path
	}
	return path
}

func absentFinding(selectedSink sink, path string) sinkFinding {
	return sinkFinding{selectedSink, "sink absent: " + sinkLocation(selectedSink, path)}
}

func canonicalAbsentFinding(selectedSink sink) sinkFinding {
	return sinkFinding{selectedSink, "sink absent: " + selectedSink.base}
}

func unreadableFinding(selectedSink sink, path string, cause error) sinkFinding {
	return sinkFinding{selectedSink,
		fmt.Sprintf("sink unreadable: %s: %v", sinkLocation(selectedSink, path), cause)}
}

func canonicalUnreadableFinding(selectedSink sink, cause error) sinkFinding {
	return sinkFinding{selectedSink,
		fmt.Sprintf("sink unreadable: %s: %v", selectedSink.base, cause)}
}

type unreadableLogError struct {
	err error
}

func (e *unreadableLogError) Error() string { return e.err.Error() }
func (e *unreadableLogError) Unwrap() error { return e.err }

func readFile(path string, sequence int64, filter filters) ([]record, int64, int, error) {
	file, err := os.Open(path)
	if err != nil {
		return nil, sequence, 1, &unreadableLogError{fmt.Errorf("open log %q: %w", path, err)}
	}
	records, next, nextLine, scanErr := scanRecords(file, path, 1, sequence, filter)
	closeErr := file.Close()
	if scanErr != nil {
		if closeErr != nil {
			return nil, sequence, 1, errors.Join(scanErr,
				&unreadableLogError{fmt.Errorf("close log %q: %w", path, closeErr)})
		}
		return nil, sequence, 1, scanErr
	}
	if closeErr != nil {
		return nil, sequence, 1, &unreadableLogError{fmt.Errorf("close log %q: %w", path, closeErr)}
	}
	return records, next, nextLine, nil
}

func scanRecords(reader io.Reader, path string, firstLine int, sequence int64, filter filters) ([]record, int64, int, error) {
	scanner := bufio.NewScanner(reader)
	scanner.Buffer(make([]byte, 64*1024), 16*1024*1024)
	var records []record
	line := firstLine
	for scanner.Scan() {
		raw := bytes.TrimSpace(scanner.Bytes())
		if len(raw) == 0 {
			return nil, sequence, line, fmt.Errorf("%s:%d: malformed JSONL: empty line", path, line)
		}
		record, err := parseRecord(raw, sequence)
		if err != nil {
			return nil, sequence, line, fmt.Errorf("%s:%d: malformed JSONL: %w", path, line, err)
		}
		sequence++
		if filter.accepts(record) {
			records = append(records, record)
		}
		line++
	}
	if err := scanner.Err(); err != nil {
		return nil, sequence, line, &unreadableLogError{fmt.Errorf("read log %q: %w", path, err)}
	}
	return records, sequence, line, nil
}

func parseRecord(raw []byte, sequence int64) (record, error) {
	var rec record
	if err := json.Unmarshal(raw, &rec); err != nil {
		return record{}, err
	}
	required := []struct {
		name  string
		value string
	}{
		{"timestamp", rec.Timestamp},
		{"runtime", rec.Runtime},
		{"level", rec.Level},
		{"verbosity", rec.Verbosity},
		{"operation", rec.Operation},
		{"message", rec.Message},
	}
	for _, field := range required {
		if field.value == "" {
			return record{}, fmt.Errorf("required field %s is absent or empty", field.name)
		}
	}
	if rec.Context == nil {
		return record{}, errors.New("required field context is absent or is not an object")
	}
	instant, err := time.Parse(time.RFC3339Nano, rec.Timestamp)
	if err != nil {
		return record{}, fmt.Errorf("timestamp is not RFC3339: %w", err)
	}
	if _, ok := levelRank[rec.Level]; !ok {
		return record{}, fmt.Errorf("unknown level %q", rec.Level)
	}
	if _, ok := runtimeNames[rec.Runtime]; !ok {
		return record{}, fmt.Errorf("unknown runtime %q", rec.Runtime)
	}
	if rec.Verbosity != "normal" && rec.Verbosity != "verbose" {
		return record{}, fmt.Errorf("unknown verbosity %q", rec.Verbosity)
	}
	rec.instant = instant
	rec.raw = append([]byte(nil), raw...)
	rec.seq = sequence
	return rec, nil
}

func (f filters) accepts(rec record) bool {
	if recRank := levelRank[rec.Level]; recRank < f.minLevel {
		return false
	}
	if len(f.runtimes) != 0 {
		if _, ok := f.runtimes[rec.Runtime]; !ok {
			return false
		}
	}
	if f.since != nil && rec.instant.Before(*f.since) {
		return false
	}
	if f.until != nil && rec.instant.After(*f.until) {
		return false
	}
	return true
}

func sortRecords(records []record) {
	sort.SliceStable(records, func(i, j int) bool {
		if records[i].instant.Equal(records[j].instant) {
			return records[i].seq < records[j].seq
		}
		return records[i].instant.Before(records[j].instant)
	})
}

func emit(records []record, findings []sinkFinding, mode string) error {
	switch mode {
	case "json":
		for _, rec := range records {
			if _, err := fmt.Fprintln(os.Stdout, string(rec.raw)); err != nil {
				return err
			}
		}
	case "human":
		for _, rec := range records {
			contextJSON, err := json.Marshal(rec.Context)
			if err != nil {
				return fmt.Errorf("encode context for %s: %w", rec.Operation, err)
			}
			if _, err := fmt.Fprintf(os.Stdout, "%s %-5s %-7s %s %s%s context=%s\n",
				rec.instant.Local().Format("2006-01-02T15:04:05.000000-07:00"),
				strings.ToUpper(rec.Level), rec.Runtime, rec.Operation,
				oneLine(rec.Message), compactIdentity(rec), contextJSON); err != nil {
				return err
			}
		}
	case "harvest":
		return emitHarvest(records, findings)
	}
	if len(findings) != 0 {
		if _, err := fmt.Fprintf(os.Stderr, "logs-reader: %d sink finding(s)\n", len(findings)); err != nil {
			return err
		}
		for _, finding := range findings {
			identity := finding.sink.workspaceDir
			if finding.sink.workspaceID != "" {
				identity = finding.sink.workspaceID + " " + identity
			}
			if identity == "" {
				identity = "central"
			}
			if _, err := fmt.Fprintf(os.Stderr, "logs-reader: %s [%s]\n", finding.message, identity); err != nil {
				return err
			}
		}
	}
	return nil
}

func compactIdentity(rec record) string {
	var fields []string
	appendString := func(name, value string) {
		if value != "" {
			fields = append(fields, name+"="+oneLine(value))
		}
	}
	appendString("workspace_id", rec.WorkspaceID)
	appendString("workspace_dir", rec.WorkspaceDir)
	appendString("agent_repl_session_id", rec.AgentReplSessionID)
	appendString("claude_session_id", rec.ClaudeSessionID)
	appendString("request_id", rec.RequestID)
	appendString("connection_id", rec.ConnectionID)
	if rec.PID != nil {
		fields = append(fields, "pid="+strconv.FormatInt(*rec.PID, 10))
	}
	if len(fields) == 0 {
		return ""
	}
	return " " + strings.Join(fields, " ")
}

func oneLine(value string) string {
	replacer := strings.NewReplacer("\n", "\\n", "\r", "\\r", "\t", "\\t")
	return replacer.Replace(value)
}

type harvestKey struct {
	workspaceID  string
	workspaceDir string
	level        string
	runtime      string
	operation    string
	message      string
}

func emitHarvest(records []record, findings []sinkFinding) error {
	counts := make(map[harvestKey]int)
	for _, rec := range records {
		workspaceID, workspaceDir := rec.WorkspaceID, rec.WorkspaceDir
		if workspaceID == "" && workspaceDir == "" {
			workspaceID, workspaceDir = "central", "-"
		} else if workspaceID == "" || workspaceDir == "" {
			return fmt.Errorf("harvest record %s at %s has incomplete workspace attribution: workspace_id=%q workspace_dir=%q",
				rec.Operation, rec.Timestamp, workspaceID, workspaceDir)
		}
		counts[harvestKey{workspaceID, workspaceDir, rec.Level, rec.Runtime, rec.Operation, oneLine(rec.Message)}]++
	}
	for _, finding := range findings {
		workspaceID, workspaceDir := finding.sink.workspaceID, finding.sink.workspaceDir
		if workspaceID == "" && workspaceDir == "" {
			workspaceID, workspaceDir = "central", "-"
		} else if workspaceID == "" || workspaceDir == "" {
			return fmt.Errorf("harvest finding has incomplete workspace attribution: workspace_id=%q workspace_dir=%q message=%q",
				workspaceID, workspaceDir, finding.message)
		}
		counts[harvestKey{workspaceID, workspaceDir, "finding", finding.sink.runtime,
			"logs.sink", oneLine(finding.message)}]++
	}
	keys := make([]harvestKey, 0, len(counts))
	for key := range counts {
		keys = append(keys, key)
	}
	sort.Slice(keys, func(i, j int) bool {
		a, b := keys[i], keys[j]
		return strings.Join([]string{a.workspaceID, a.workspaceDir, a.level, a.runtime, a.operation, a.message}, "\x00") <
			strings.Join([]string{b.workspaceID, b.workspaceDir, b.level, b.runtime, b.operation, b.message}, "\x00")
	})
	writer := tabwriter.NewWriter(os.Stdout, 0, 4, 2, ' ', 0)
	if _, err := fmt.Fprintln(writer, "WORKSPACE_ID\tWORKSPACE_DIR\tLEVEL\tRUNTIME\tOPERATION\tMESSAGE\tCOUNT"); err != nil {
		return err
	}
	for _, key := range keys {
		if _, err := fmt.Fprintf(writer, "%s\t%s\t%s\t%s\t%s\t%s\t%d\n",
			key.workspaceID, key.workspaceDir, key.level, key.runtime, key.operation, key.message, counts[key]); err != nil {
			return err
		}
	}
	return writer.Flush()
}

func follow(mode string, states []followState, filter filters, sequence int64) error {
	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt, syscall.SIGTERM)
	defer stop()
	ticker := time.NewTicker(100 * time.Millisecond)
	defer ticker.Stop()
	for {
		select {
		case <-ctx.Done():
			return nil
		case <-ticker.C:
			var batch []record
			for index := range states {
				loaded, next, err := readFollow(&states[index], sequence, filter)
				if err != nil {
					return err
				}
				sequence = next
				batch = append(batch, loaded...)
			}
			sortRecords(batch)
			if err := emit(batch, nil, mode); err != nil {
				return err
			}
		}
	}
}

func readFollow(state *followState, sequence int64, filter filters) ([]record, int64, error) {
	_, current, _ := generationFiles(state.sink)
	if current == "" {
		return nil, sequence, nil
	}
	info, err := os.Stat(current)
	if err != nil {
		return nil, sequence, fmt.Errorf("stat followed log %q: %w", current, err)
	}
	if state.info == nil || state.path != current || !os.SameFile(state.info, info) || info.Size() < state.offset {
		state.path, state.info, state.offset, state.line = current, info, 0, 1
	}
	if info.Size() == state.offset {
		state.info = info
		return nil, sequence, nil
	}
	file, err := os.Open(current)
	if err != nil {
		return nil, sequence, fmt.Errorf("open followed log %q: %w", current, err)
	}
	if _, seekErr := file.Seek(state.offset, io.SeekStart); seekErr != nil {
		closeErr := file.Close()
		if closeErr != nil {
			return nil, sequence, errors.Join(
				fmt.Errorf("seek followed log %q: %w", current, seekErr),
				fmt.Errorf("close followed log %q: %w", current, closeErr))
		}
		return nil, sequence, fmt.Errorf("seek followed log %q: %w", current, seekErr)
	}
	bytesAvailable := info.Size() - state.offset
	data, err := io.ReadAll(io.LimitReader(file, bytesAvailable))
	closeErr := file.Close()
	if err != nil {
		if closeErr != nil {
			return nil, sequence, errors.Join(
				fmt.Errorf("read followed log %q: %w", current, err),
				fmt.Errorf("close followed log %q: %w", current, closeErr))
		}
		return nil, sequence, fmt.Errorf("read followed log %q: %w", current, err)
	}
	if closeErr != nil {
		return nil, sequence, fmt.Errorf("close followed log %q: %w", current, closeErr)
	}
	lastNewline := bytes.LastIndexByte(data, '\n')
	if lastNewline < 0 {
		return nil, sequence, nil
	}
	complete := data[:lastNewline+1]
	records, next, nextLine, err := scanRecords(bytes.NewReader(complete), current, state.line, sequence, filter)
	if err != nil {
		return nil, sequence, err
	}
	state.offset += int64(len(complete))
	state.info = info
	state.line = nextLine
	return records, next, nil
}
