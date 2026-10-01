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
	"regexp"
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
	baseFormats       repeatedFlag
	workspaceNames    repeatedFlag
	harvestFrom       string
	harvestTo         string
	sampleN           int
	fields            string
	width             int
}

// renderOptions carries only what emit and follow need to render a batch,
// resolved once in run() so every emit call site agrees on mode, group
// sample count, message width, and field projection.
type renderOptions struct {
	mode    string
	sampleN int
	width   int
	fields  []string
	// names maps a daemon workspace ID to the workspace's name, so every
	// renderer names a workspace and never shows only its ID.
	names map[string]string
}

type sink struct {
	base             string
	runtime          string
	workspaceID      string
	workspaceDir     string
	canonicalSymlink bool
	// format is "jsonl" for the contract's structured records, "stderr" for
	// unstructured emergency process output (shim-store.err.log and
	// shim-claude-sidecar.err.log), or "messages" for a captured Emacs
	// *Messages* text snapshot named with --messages. The latter two are
	// synthesized into records rather than parsed as JSON.
	format string
}

type sinkFinding struct {
	sink    sink
	message string
}

type record struct {
	Timestamp    string         `json:"timestamp"`
	Runtime      string         `json:"runtime"`
	Level        string         `json:"level"`
	Verbosity    string         `json:"verbosity"`
	Operation    string         `json:"operation"`
	Message      string         `json:"message"`
	Context      map[string]any `json:"context"`
	WorkspaceDir string         `json:"workspace_dir"`
	WorkspaceID  string         `json:"workspace_id"`
	// WorkspaceName is SYNTHETIC: the daemon's current name for WorkspaceID,
	// stamped by nameWorkspaces at emit. A record never writes it itself.
	WorkspaceName      string `json:"workspace_name,omitempty"`
	AgentReplSessionID string `json:"agent_repl_session_id"`
	ClaudeSessionID    string `json:"claude_session_id"`
	RequestID          string `json:"request_id"`
	ConnectionID       string `json:"connection_id"`
	PID                *int64 `json:"pid"`

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
	flag.Var(&opts.baseFormats, "base-format", "base content format: jsonl, stderr, or messages")
	flag.Var(&opts.workspaceNames, "workspace-name", "ID=NAME for one daemon workspace, repeated for every workspace the daemon knows")
	flag.IntVar(&opts.sampleN, "sample-n", 0, "representative records per group (0 disables sampling)")
	flag.StringVar(&opts.fields, "fields", "", "comma-separated field projection")
	flag.IntVar(&opts.width, "width", 120, "message truncation width for compact renderers")
	flag.Parse()
	if flag.NArg() != 0 {
		return fmt.Errorf("unexpected positional argument %q", flag.Arg(0))
	}
	names, err := parseWorkspaceNames(opts.workspaceNames)
	if err != nil {
		return err
	}
	if len(opts.bases) == 0 {
		return errors.New("no log paths were selected")
	}
	sinks, err := makeSinks(opts)
	if err != nil {
		return err
	}
	switch opts.mode {
	case "human", "json", "harvest", "tally", "sample", "timeline", "fields":
	default:
		return fmt.Errorf("unknown output mode %q", opts.mode)
	}
	if opts.follow && (opts.mode == "harvest" || opts.mode == "tally" || opts.mode == "sample") {
		return fmt.Errorf("%s cannot follow", opts.mode)
	}
	if opts.width <= 0 {
		return errors.New("width must be positive")
	}
	fieldNames, err := parseFieldsList(opts.fields)
	if err != nil {
		return err
	}
	if opts.mode == "fields" && len(fieldNames) == 0 {
		return errors.New("fields mode requires --fields")
	}
	if opts.mode == "sample" && opts.sampleN <= 0 {
		return errors.New("sample mode requires --sample-n greater than zero")
	}
	render := renderOptions{mode: opts.mode, sampleN: opts.sampleN, width: opts.width, fields: fieldNames, names: names}

	now := time.Now()
	if opts.mode == "harvest" {
		opts.since, opts.until = opts.harvestFrom, opts.harvestTo
		opts.minLevel = "warn"
	}
	filter, err := makeFilters(opts, now)
	if err != nil {
		return err
	}

	records, findings, readable, sequence, states, orphanCount, err := readInitial(sinks, filter)
	if err != nil {
		return err
	}
	fmt.Fprintf(os.Stderr, "logs-reader: included %d orphan generation(s) left by earlier daemon instances\n", orphanCount)
	sortRecords(records)
	if err := emit(records, findings, render); err != nil {
		return err
	}
	if readable == 0 {
		return errors.New("none of the selected log sinks could be read")
	}
	if !opts.follow {
		return nil
	}
	return follow(render, states, filter, sequence)
}

// parseFieldsList validates a comma-separated --fields value, matching the
// style of the existing --runtime list.
func parseFieldsList(raw string) ([]string, error) {
	if raw == "" {
		return nil, nil
	}
	parts := strings.Split(raw, ",")
	for _, part := range parts {
		if part == "" {
			return nil, errors.New("fields list contains an empty value")
		}
	}
	return parts, nil
}

func makeSinks(opts options) ([]sink, error) {
	counts := map[string]int{
		"--base":               len(opts.bases),
		"--base-runtime":       len(opts.baseRuntimes),
		"--base-workspace-id":  len(opts.baseWorkspaceIDs),
		"--base-workspace-dir": len(opts.baseWorkspaceDirs),
		"--base-kind":          len(opts.baseKinds),
		"--base-format":        len(opts.baseFormats),
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
		format := opts.baseFormats[index]
		if format != "jsonl" && format != "stderr" && format != "messages" {
			return nil, fmt.Errorf("base %q has unknown format %q", base, format)
		}
		if kind == "symlink" && format != "jsonl" {
			return nil, fmt.Errorf("workspace base %q has non-jsonl format %q", base, format)
		}
		workspaceID, workspaceDir := opts.baseWorkspaceIDs[index], opts.baseWorkspaceDirs[index]
		if kind == "symlink" && workspaceDir == "" {
			return nil, fmt.Errorf("workspace base %q has no workspace directory", base)
		}
		if kind == "file" && (workspaceID != "" || workspaceDir != "") {
			return nil, fmt.Errorf("central base %q carries workspace attribution", base)
		}
		sinks = append(sinks, sink{
			base:             base,
			runtime:          runtime,
			workspaceID:      workspaceID,
			workspaceDir:     workspaceDir,
			canonicalSymlink: kind == "symlink",
			format:           format,
		})
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

func readInitial(sinks []sink, filter filters) ([]record, []sinkFinding, int, int64, []followState, int, error) {
	var records []record
	var findings []sinkFinding
	var sequence int64
	states := make([]followState, 0, len(sinks))
	seen := make(map[string]struct{})
	readable := 0
	orphanCount := 0
	for _, selectedSink := range sinks {
		files, current, discovered, orphans := generationFiles(selectedSink)
		findings = append(findings, discovered...)
		orphanCount += orphans
		state := followState{sink: selectedSink, path: current, line: 1}
		sinkReadable := false
		for _, path := range files {
			if _, duplicate := seen[path]; duplicate {
				sinkReadable = true
				continue
			}
			loaded, next, nextLine, err := readFile(selectedSink, path, sequence, filter)
			if err != nil {
				if unreadable, ok := err.(*unreadableLogError); ok {
					findings = append(findings, unreadableFinding(selectedSink, path, unreadable.err))
					continue
				}
				return nil, nil, 0, 0, nil, 0, err
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
	return records, findings, readable, sequence, states, orphanCount, nil
}

func generationFiles(selectedSink sink) ([]string, string, []sinkFinding, int) {
	current := selectedSink.base
	var findings []sinkFinding
	if selectedSink.canonicalSymlink {
		info, err := os.Lstat(selectedSink.base)
		if os.IsNotExist(err) {
			return nil, "", []sinkFinding{canonicalAbsentFinding(selectedSink)}, 0
		}
		if err != nil {
			return nil, "", []sinkFinding{canonicalUnreadableFinding(selectedSink, err)}, 0
		}
		if info.Mode()&os.ModeSymlink == 0 {
			return nil, "", []sinkFinding{{selectedSink, "sink not a symlink: " + selectedSink.base}}, 0
		}
		target, err := os.Readlink(selectedSink.base)
		if err != nil {
			return nil, "", []sinkFinding{canonicalUnreadableFinding(selectedSink, err)}, 0
		}
		// WHY: EvalSymlinks loses the named target when it is absent, which is
		// exactly the expected sink state this reader must report and continue past.
		if !filepath.IsAbs(target) {
			target = filepath.Join(filepath.Dir(selectedSink.base), target)
		}
		current = filepath.Clean(target)
	}
	orphans, orphanFindings := orphanSiblings(selectedSink, current)
	findings = append(findings, orphanFindings...)
	files := make([]string, 0, retainedGenerations+1+len(orphans))
	files = append(files, orphans...)
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
		return files, current, findings, len(orphans)
	}
	return files, "", findings, len(orphans)
}

// orphanSiblings answers the sibling unique targets an earlier daemon
// instance minted for this same workspace sink before append-on-restart
// (logging-contract.md) replaced per-restart minting. Every daemon-owned
// workspace target is named "agent-repl-<workspaceID>-<runtime>-*.log"
// (daemon/internal/dlog/sink.go createTarget); a workspace bounced across
// several daemon instances before that fix can carry several such files
// beside the one the canonical symlink currently names. They are distinct
// files, never generations of one another, so every match is included and
// none is deduplicated against another.
func orphanSiblings(selectedSink sink, current string) ([]string, []sinkFinding) {
	if !selectedSink.canonicalSymlink || selectedSink.workspaceID == "" || current == "" {
		return nil, nil
	}
	dir := filepath.Dir(current)
	entries, err := os.ReadDir(dir)
	if err != nil {
		if os.IsNotExist(err) {
			return nil, nil
		}
		return nil, []sinkFinding{unreadableFinding(selectedSink, dir, err)}
	}
	prefix := "agent-repl-" + selectedSink.workspaceID + "-" + selectedSink.runtime + "-"
	const suffix = ".log"
	var orphans []string
	for _, entry := range entries {
		if entry.IsDir() {
			continue
		}
		name := entry.Name()
		if !strings.HasPrefix(name, prefix) || !strings.HasSuffix(name, suffix) {
			continue
		}
		path := filepath.Join(dir, name)
		if path == current {
			continue
		}
		orphans = append(orphans, path)
	}
	sort.Strings(orphans)
	return orphans, nil
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

func readFile(selectedSink sink, path string, sequence int64, filter filters) ([]record, int64, int, error) {
	file, err := os.Open(path)
	if err != nil {
		return nil, sequence, 1, &unreadableLogError{fmt.Errorf("open log %q: %w", path, err)}
	}
	var records []record
	var next int64
	var nextLine int
	var scanErr error
	if selectedSink.format == "jsonl" {
		records, next, nextLine, scanErr = scanRecords(file, path, 1, sequence, filter)
	} else {
		info, statErr := file.Stat()
		if statErr != nil {
			scanErr = &unreadableLogError{fmt.Errorf("stat log %q: %w", path, statErr)}
		} else {
			records, next, nextLine, scanErr = scanTextRecords(file, path, selectedSink, info.ModTime(), 1, sequence, filter)
		}
	}
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

// scanTextRecords reads a non-JSONL sink (stderr or a captured Messages
// snapshot) and synthesizes one record per interesting line, so the two text
// sources the contract permits alongside structured JSONL are queryable
// through the exact same tally/sample/timeline/fields/human/json renderers.
// See AGENTS.md "Logs" for the synthetic fields this manufactures.
func scanTextRecords(reader io.Reader, path string, selectedSink sink, mtime time.Time, firstLine int, sequence int64, filter filters) ([]record, int64, int, error) {
	scanner := bufio.NewScanner(reader)
	scanner.Buffer(make([]byte, 64*1024), 16*1024*1024)
	var records []record
	line := firstLine
	for scanner.Scan() {
		text := scanner.Text()
		var rec record
		var ok bool
		switch selectedSink.format {
		case "stderr":
			rec, ok = synthesizeStderrRecord(selectedSink, text, mtime, sequence)
		case "messages":
			rec, ok = synthesizeMessagesRecord(selectedSink, text, mtime, sequence)
		default:
			return nil, sequence, line, fmt.Errorf("%s:%d: unknown text sink format %q", path, line, selectedSink.format)
		}
		sequence++
		line++
		if ok && filter.accepts(rec) {
			records = append(records, rec)
		}
	}
	if err := scanner.Err(); err != nil {
		return nil, sequence, line, &unreadableLogError{fmt.Errorf("read log %q: %w", path, err)}
	}
	return records, sequence, line, nil
}

// stderrErrorRe recognizes a line that plainly names an error, so a caller
// filtering `--level error` sees the emergency lines that actually claim to
// be one rather than every byte the process ever wrote to its stderr sink.
var stderrErrorRe = regexp.MustCompile(`(?i)\berror\b`)

// synthesizeStderrRecord treats every non-blank line of a service's
// `.err.log` as a finding: the contract permits this output only when the
// canonical structured sink could not record the process's own failure, so
// presence here is itself notable and defaults to `warn`.
func synthesizeStderrRecord(selectedSink sink, line string, mtime time.Time, seq int64) (record, bool) {
	trimmed := strings.TrimSpace(line)
	if trimmed == "" {
		return record{}, false
	}
	level := "warn"
	if stderrErrorRe.MatchString(trimmed) {
		level = "error"
	}
	return buildSyntheticRecord(selectedSink, "stderr", level, trimmed, mtime, seq), true
}

// messageLinePattern is one severity shape recognized in a captured Emacs
// *Messages* snapshot, mirroring e2e/realtest/messages.go's messagePatterns
// so the two readers agree on what counts as an interesting line.
type messageLinePattern struct {
	re    *regexp.Regexp
	level string
}

var messageLinePatterns = []messageLinePattern{
	{regexp.MustCompile(`\bERROR:`), "error"},
	{regexp.MustCompile(`\bWARNING:`), "warn"},
	{regexp.MustCompile(`^Warning \(|^⛔ Warning \(`), "warn"},
	{regexp.MustCompile(`error in process (filter|sentinel)|Error running timer|error during redisplay|Error in post-command-hook|Error in pre-command-hook`), "error"},
	{regexp.MustCompile(`Wrong type argument|Symbol's (value as variable|function definition) is void|Args out of range|Invalid function|Wrong number of arguments|Attempt to modify a read-only|Selecting deleted buffer|Invalid face`), "error"},
	{regexp.MustCompile(`^Debugger entered`), "error"},
	{regexp.MustCompile(`Failed to load|failed to load|could not be loaded|Doom encountered an error`), "error"},
}

// synthesizeMessagesRecord returns a record only for a line matching one of
// the known severity shapes; every other buffer line is prose and is
// skipped, the same way e2e/realtest/messages.go's HarvestMessages does.
func synthesizeMessagesRecord(selectedSink sink, line string, mtime time.Time, seq int64) (record, bool) {
	trimmed := strings.TrimSpace(line)
	if trimmed == "" {
		return record{}, false
	}
	for _, pattern := range messageLinePatterns {
		if pattern.re.MatchString(trimmed) {
			return buildSyntheticRecord(selectedSink, "messages", pattern.level, trimmed, mtime, seq), true
		}
	}
	return record{}, false
}

// buildSyntheticRecord manufactures a record for a text line that never
// carried the contract's fields: `operation` is a stable synthetic tag
// ("stderr" or "messages"), `runtime` names the emitting service or "emacs",
// `timestamp` is the sink file's modification time (the best available
// instant for a line with none of its own), and `context` stays an empty
// object so every renderer that expects one keeps working unchanged.
func buildSyntheticRecord(selectedSink sink, operation, level, message string, mtime time.Time, seq int64) record {
	rec := record{
		Timestamp:    mtime.UTC().Format(time.RFC3339Nano),
		Runtime:      selectedSink.runtime,
		Level:        level,
		Verbosity:    "normal",
		Operation:    operation,
		Message:      message,
		Context:      map[string]any{},
		WorkspaceDir: selectedSink.workspaceDir,
		WorkspaceID:  selectedSink.workspaceID,
		instant:      mtime,
		seq:          seq,
	}
	raw, err := json.Marshal(rec)
	if err != nil {
		raw = []byte("{}")
	}
	rec.raw = raw
	return rec
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

func emit(records []record, findings []sinkFinding, render renderOptions) error {
	if err := nameWorkspaces(records, render.names); err != nil {
		return err
	}
	switch render.mode {
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
	case "tally":
		if err := emitTally(records, render.sampleN, render.width); err != nil {
			return err
		}
	case "sample":
		if err := emitSampleOnly(records, render.sampleN, render.width); err != nil {
			return err
		}
	case "timeline":
		if err := emitTimeline(records, render.width); err != nil {
			return err
		}
	case "fields":
		if err := emitFields(records, render.fields); err != nil {
			return err
		}
	}
	if len(findings) != 0 {
		if _, err := fmt.Fprintf(os.Stderr, "logs-reader: %d sink finding(s)\n", len(findings)); err != nil {
			return err
		}
		for _, finding := range findings {
			identity := finding.sink.workspaceDir
			if name := render.names[finding.sink.workspaceID]; name != "" {
				identity = name
			} else if finding.sink.workspaceID != "" {
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
	// A NAMED WORKSPACE IS SHOWN BY ITS NAME ALONE; its ID and directory are
	// shown only when the daemon has no name for it.
	if rec.WorkspaceName != "" {
		appendString("workspace", rec.WorkspaceName)
	} else {
		appendString("workspace_id", rec.WorkspaceID)
		appendString("workspace_dir", rec.WorkspaceDir)
	}
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

// groupKey is how --tally and --sample group records: level, runtime, and
// operation, with no workspace or message in the key, so the same three
// questions ("what happened, of what severity, on what runtime") answer for
// one workspace or for --all in a single small table.
type groupKey struct {
	level     string
	runtime   string
	operation string
}

func groupCounts(records []record) map[groupKey]int {
	counts := make(map[groupKey]int)
	for _, rec := range records {
		counts[groupKey{rec.Level, rec.Runtime, rec.Operation}]++
	}
	return counts
}

// sortedGroupKeys orders groups by count descending — the cheapest way to
// see "what happened most" — breaking ties deterministically so repeated
// runs against the same window render identically.
func sortedGroupKeys(counts map[groupKey]int) []groupKey {
	keys := make([]groupKey, 0, len(counts))
	for key := range counts {
		keys = append(keys, key)
	}
	sort.Slice(keys, func(i, j int) bool {
		a, b := keys[i], keys[j]
		if counts[a] != counts[b] {
			return counts[a] > counts[b]
		}
		if a.level != b.level {
			return a.level < b.level
		}
		if a.runtime != b.runtime {
			return a.runtime < b.runtime
		}
		return a.operation < b.operation
	})
	return keys
}

// emitTally prints the count table and, when sampleN is positive, follows it
// with up to sampleN representative records per group in the table's own
// order.
func emitTally(records []record, sampleN, width int) error {
	counts := groupCounts(records)
	keys := sortedGroupKeys(counts)
	writer := tabwriter.NewWriter(os.Stdout, 0, 4, 2, ' ', 0)
	if _, err := fmt.Fprintln(writer, "COUNT\tLEVEL\tRUNTIME\tOPERATION"); err != nil {
		return err
	}
	for _, key := range keys {
		if _, err := fmt.Fprintf(writer, "%d\t%s\t%s\t%s\n", counts[key], key.level, key.runtime, key.operation); err != nil {
			return err
		}
	}
	if err := writer.Flush(); err != nil {
		return err
	}
	if sampleN <= 0 {
		return nil
	}
	if _, err := fmt.Println(); err != nil {
		return err
	}
	return emitSamplesForGroups(records, keys, sampleN, width)
}

// emitSampleOnly is --sample used without --tally: the same per-group
// representative lines, with no count table ahead of them.
func emitSampleOnly(records []record, sampleN, width int) error {
	counts := groupCounts(records)
	keys := sortedGroupKeys(counts)
	return emitSamplesForGroups(records, keys, sampleN, width)
}

// emitSamplesForGroups prints, for each group in order, the first sampleN
// records belonging to it (records are already time-sorted by the caller),
// so a reader sees the earliest representative occurrence of each kind of
// event without scanning the whole window.
func emitSamplesForGroups(records []record, order []groupKey, sampleN, width int) error {
	for _, key := range order {
		taken := 0
		for _, rec := range records {
			if rec.Level != key.level || rec.Runtime != key.runtime || rec.Operation != key.operation {
				continue
			}
			if err := printSampleLine(rec, width); err != nil {
				return err
			}
			taken++
			if taken >= sampleN {
				break
			}
		}
	}
	return nil
}

func printSampleLine(rec record, width int) error {
	_, err := fmt.Printf("%s %-5s %-7s %s %s\n",
		rec.instant.Local().Format("15:04:05.000000"),
		strings.ToUpper(rec.Level), rec.Runtime, rec.Operation, truncate(rec.Message, width))
	return err
}

// emitTimeline prints one line per record in time order: time, level,
// operation, and the truncated message, for "what happened during this
// window" without runtime or context noise.
func emitTimeline(records []record, width int) error {
	for _, rec := range records {
		if _, err := fmt.Printf("%s %-5s %s %s\n",
			rec.instant.Local().Format("15:04:05.000000"),
			strings.ToUpper(rec.Level), rec.Operation, truncate(rec.Message, width)); err != nil {
			return err
		}
	}
	return nil
}

// emitFields prints exactly the named fields, resolved from the record's top
// level or, failing that, its context map, so a caller gets nothing else.
func emitFields(records []record, fieldNames []string) error {
	for _, rec := range records {
		parts := make([]string, 0, len(fieldNames))
		for _, name := range fieldNames {
			parts = append(parts, name+"="+oneLine(fieldValue(rec, name)))
		}
		if _, err := fmt.Println(strings.Join(parts, " ")); err != nil {
			return err
		}
	}
	return nil
}

// fieldValue resolves one named field against a record's top-level columns
// first, then its context map. An absent field renders as an empty value
// rather than an error, since not every record in a selection carries every
// context key.
func fieldValue(rec record, name string) string {
	switch name {
	case "timestamp":
		return rec.Timestamp
	case "runtime":
		return rec.Runtime
	case "level":
		return rec.Level
	case "verbosity":
		return rec.Verbosity
	case "operation":
		return rec.Operation
	case "message":
		return rec.Message
	case "workspace_dir":
		return rec.WorkspaceDir
	case "workspace_id":
		return rec.WorkspaceID
	case "workspace_name":
		return rec.WorkspaceName
	case "agent_repl_session_id":
		return rec.AgentReplSessionID
	case "claude_session_id":
		return rec.ClaudeSessionID
	case "request_id":
		return rec.RequestID
	case "connection_id":
		return rec.ConnectionID
	case "pid":
		if rec.PID != nil {
			return strconv.FormatInt(*rec.PID, 10)
		}
		return ""
	}
	if rec.Context == nil {
		return ""
	}
	value, ok := rec.Context[name]
	if !ok {
		return ""
	}
	if text, ok := value.(string); ok {
		return text
	}
	encoded, err := json.Marshal(value)
	if err != nil {
		return fmt.Sprintf("%v", value)
	}
	return string(encoded)
}

// truncate shortens a one-line message to width runes, marking the cut with
// an ellipsis, so --width bounds every compact renderer's line length.
func truncate(value string, width int) string {
	value = oneLine(value)
	runes := []rune(value)
	if len(runes) <= width {
		return value
	}
	if width <= 3 {
		return string(runes[:width])
	}
	return string(runes[:width-3]) + "..."
}

func follow(render renderOptions, states []followState, filter filters, sequence int64) error {
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
			if err := emit(batch, nil, render); err != nil {
				return err
			}
		}
	}
}

func readFollow(state *followState, sequence int64, filter filters) ([]record, int64, error) {
	_, current, _, _ := generationFiles(state.sink)
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
	var records []record
	var next int64
	var nextLine int
	if state.sink.format == "jsonl" {
		records, next, nextLine, err = scanRecords(bytes.NewReader(complete), current, state.line, sequence, filter)
	} else {
		records, next, nextLine, err = scanTextRecords(bytes.NewReader(complete), current, state.sink, info.ModTime(), state.line, sequence, filter)
	}
	if err != nil {
		return nil, sequence, err
	}
	state.offset += int64(len(complete))
	state.info = info
	state.line = nextLine
	return records, next, nil
}

// parseWorkspaceNames reads the repeated --workspace-name ID=NAME pairs. A
// pair with no "=", an empty ID or name, or an ID named twice is a usage
// error: logs.sh builds the pairs from the daemon's own workspace table,
// where IDs are unique and every workspace has a name.
func parseWorkspaceNames(pairs []string) (map[string]string, error) {
	names := make(map[string]string, len(pairs))
	for _, pair := range pairs {
		id, name, ok := strings.Cut(pair, "=")
		if !ok || id == "" || name == "" {
			return nil, fmt.Errorf("--workspace-name %q is not ID=NAME", pair)
		}
		if _, seen := names[id]; seen {
			return nil, fmt.Errorf("--workspace-name names workspace %q twice", id)
		}
		names[id] = name
	}
	return names, nil
}

// nameWorkspaces stamps each record's synthetic workspace_name from its
// workspace_id, into the decoded record and into the raw JSON the json mode
// prints, so every output mode can name the workspace. The name is appended
// as the object's last key, leaving the record's own bytes and key order
// untouched. A record whose ID the daemon does not know keeps no name.
func nameWorkspaces(records []record, names map[string]string) error {
	for i := range records {
		name := names[records[i].WorkspaceID]
		if name == "" || records[i].WorkspaceName != "" {
			continue
		}
		records[i].WorkspaceName = name
		raw := bytes.TrimRight(records[i].raw, " \t\r\n")
		if len(raw) < 2 || raw[len(raw)-1] != '}' {
			return fmt.Errorf("record %s at %s is not a JSON object", records[i].Operation, records[i].Timestamp)
		}
		encoded, err := json.Marshal(name)
		if err != nil {
			return fmt.Errorf("encode workspace name %q: %w", name, err)
		}
		body := bytes.TrimRight(raw[:len(raw)-1], " \t\r\n")
		separator := ","
		if len(body) > 0 && body[len(body)-1] == '{' {
			separator = ""
		}
		named := append(append([]byte(nil), body...), []byte(separator+`"workspace_name":`)...)
		named = append(named, encoded...)
		records[i].raw = append(named, '}')
	}
	return nil
}
