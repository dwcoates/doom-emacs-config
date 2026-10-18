// profile-report.go summarizes one runtime profiling session for
// bin/profile-agent-repl.sh: per-process CPU and memory from the sampler, the
// hottest frames of every runtime's profile, and the user-visible latencies
// read off real INFO log edges, ranked by how far each is over the budget a
// user perceives for that interaction.
//
// Usage:
//
//	profile-report --session DIR [--logs FILE] [--startup SINCE]
//
// --logs is the JSONL `bin/logs.sh --all --json` printed for the session's
// window ("-" is stdin; absent is no latency table). --startup adds the
// startup assessment: launch to every workspace loaded, then every switch.
package main

import (
	"bufio"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"time"
)

// budget is how long a user can wait on an interaction before it is felt.
type budget struct {
	name   string
	limit  time.Duration
	reason string
}

// profileKinds are the interactions pairLatencies measures from log edges.
// Each one's budget is read from proto/vocab/latencies.json (--latencies), the
// vocabulary the always-on metrics share, so the profile and the metrics hold
// one threshold per latency.
var profileKinds = []string{"prompt_echo", "switch_repeat", "switch_first", "page_boot", "workspace_open", "prompt_first_output"}

// loadBudgets reads every profile kind's budget from the latency vocabulary.
// A kind the vocabulary lacks is an error, never a missing row.
func loadBudgets(path string) (map[string]budget, error) {
	raw, err := os.ReadFile(path)
	if err != nil {
		return nil, fmt.Errorf("read the latency vocabulary: %w", err)
	}
	var vocab struct {
		Latencies []struct {
			Name        string `json:"name"`
			Description string `json:"description"`
			ThresholdMs int64  `json:"threshold_ms"`
			Reason      string `json:"reason"`
		} `json:"latencies"`
	}
	if err := json.Unmarshal(raw, &vocab); err != nil {
		return nil, fmt.Errorf("decode the latency vocabulary %s: %w", path, err)
	}
	byName := map[string]budget{}
	for _, l := range vocab.Latencies {
		byName[l.Name] = budget{l.Description, time.Duration(l.ThresholdMs) * time.Millisecond, l.Reason}
	}
	out := make(map[string]budget, len(profileKinds))
	for _, kind := range profileKinds {
		b, ok := byName[kind]
		if !ok || b.limit <= 0 {
			return nil, fmt.Errorf("the latency vocabulary %s has no budget for %s", path, kind)
		}
		out[kind] = b
	}
	return out, nil
}

// record is the part of a log record the latency table reads.
type record struct {
	Timestamp   string         `json:"timestamp"`
	Runtime     string         `json:"runtime"`
	Operation   string         `json:"operation"`
	Message     string         `json:"message"`
	WorkspaceID string         `json:"workspace_id"`
	Context     map[string]any `json:"context"`
	at          time.Time
}

// sample is one sampler tick for one process.
type sample struct {
	T     float64 `json:"t"`
	PID   int     `json:"pid"`
	CPU   float64 `json:"cpu_s"`
	RSSKB int64   `json:"rss_kb"`
	Name  string  `json:"name"`
}

func main() {
	session := flag.String("session", "", "the session directory")
	logs := flag.String("logs", "", "the session's logs as JSONL (- is stdin)")
	startup := flag.String("startup", "", "RFC3339 instant the editor was launched at: add the startup assessment")
	latencies := flag.String("latencies", "", "proto/vocab/latencies.json: every interaction's budget")
	flag.Parse()
	if *session == "" {
		fail(errors.New("--session is required"))
	}
	if *latencies == "" {
		fail(errors.New("--latencies is required"))
	}
	budgets, err := loadBudgets(*latencies)
	if err != nil {
		fail(err)
	}
	out := bufio.NewWriter(os.Stdout)
	defer out.Flush()
	if err := report(out, *session, *logs, *startup, budgets); err != nil {
		out.Flush()
		fail(err)
	}
}

func fail(err error) {
	fmt.Fprintln(os.Stderr, "profile-report:", err)
	os.Exit(1)
}

func report(w io.Writer, session, logs, startup string, budgets map[string]budget) error {
	fmt.Fprintf(w, "# Profile session %s\n\n", filepath.Base(session))
	if err := processTable(w, session); err != nil {
		return err
	}
	if err := frames(w, session); err != nil {
		return err
	}
	if logs == "" {
		return nil
	}
	records, err := readRecords(logs)
	if err != nil {
		return err
	}
	latencyTable(w, records, budgets)
	switchPhaseTable(w, records)
	switchTourTables(w, records)
	if startup != "" {
		since, err := time.Parse(time.RFC3339, startup)
		if err != nil {
			return fmt.Errorf("--startup: %w", err)
		}
		startupTable(w, records, since)
	}
	return nil
}

// processTable prints CPU-seconds and RSS per sampled process.
func processTable(w io.Writer, session string) error {
	f, err := os.Open(filepath.Join(session, "samples.jsonl"))
	if errors.Is(err, os.ErrNotExist) {
		fmt.Fprintln(w, "## Processes\n\nNo samples were recorded.")
		return nil
	}
	if err != nil {
		return fmt.Errorf("open the samples: %w", err)
	}
	defer f.Close()
	type acc struct {
		name               string
		first, last        float64
		seen               int
		rssSum, rssPeak    int64
		firstT, lastT      float64
		bornDuringSampling bool
	}
	byPID := map[int]*acc{}
	var firstTick float64
	scanner := bufio.NewScanner(f)
	line := 0
	for scanner.Scan() {
		line++
		var s sample
		if err := json.Unmarshal(scanner.Bytes(), &s); err != nil {
			return fmt.Errorf("samples.jsonl line %d: %w", line, err)
		}
		if firstTick == 0 {
			firstTick = s.T
		}
		a, ok := byPID[s.PID]
		if !ok {
			a = &acc{name: s.Name, first: s.CPU, firstT: s.T, bornDuringSampling: s.T > firstTick}
			byPID[s.PID] = a
		}
		a.last, a.lastT = s.CPU, s.T
		a.seen++
		a.rssSum += s.RSSKB
		if s.RSSKB > a.rssPeak {
			a.rssPeak = s.RSSKB
		}
	}
	if err := scanner.Err(); err != nil {
		return fmt.Errorf("read the samples: %w", err)
	}
	type row struct {
		name          string
		pid           int
		cpu           float64
		avgMB, peakMB float64
		seconds       float64
	}
	var rows []row
	byName := map[string]float64{}
	for pid, a := range byPID {
		cpu := a.last - a.first
		if a.bornDuringSampling {
			cpu = a.last
		}
		r := row{a.name, pid, cpu, float64(a.rssSum) / float64(a.seen) / 1024, float64(a.rssPeak) / 1024, a.lastT - a.firstT}
		rows = append(rows, r)
		byName[a.name] += cpu
	}
	sort.Slice(rows, func(i, j int) bool { return rows[i].cpu > rows[j].cpu })
	fmt.Fprintln(w, "## Processes (by CPU-seconds in the session)")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "| process | pid | CPU-s | avg RSS MB | peak RSS MB | seen s |")
	fmt.Fprintln(w, "|---|---|---|---|---|---|")
	for _, r := range rows {
		fmt.Fprintf(w, "| %s | %d | %.2f | %.0f | %.0f | %.0f |\n", r.name, r.pid, r.cpu, r.avgMB, r.peakMB, r.seconds)
	}
	names := make([]string, 0, len(byName))
	for n := range byName {
		names = append(names, n)
	}
	sort.Slice(names, func(i, j int) bool { return byName[names[i]] > byName[names[j]] })
	fmt.Fprintln(w)
	fmt.Fprintln(w, "| runtime | CPU-s |")
	fmt.Fprintln(w, "|---|---|")
	for _, n := range names {
		fmt.Fprintf(w, "| %s | %.2f |\n", n, byName[n])
	}
	fmt.Fprintln(w)
	return nil
}

// frames prints the hottest frames of every profile in the session.
func frames(w io.Writer, session string) error {
	entries, err := os.ReadDir(session)
	if err != nil {
		return fmt.Errorf("read the session: %w", err)
	}
	fmt.Fprintln(w, "## Hottest frames per runtime")
	fmt.Fprintln(w)
	for _, e := range entries {
		path := filepath.Join(session, e.Name())
		var text string
		switch {
		case strings.HasSuffix(e.Name(), ".pprof"):
			out, err := exec.Command("go", "tool", "pprof", "-top", "-nodecount=12", path).CombinedOutput()
			if err != nil {
				return fmt.Errorf("go tool pprof %s: %w: %s", e.Name(), err, out)
			}
			text = string(out)
		case strings.HasSuffix(e.Name(), ".cpuprofile"):
			text, err = cpuprofileTop(path, 12)
		case strings.HasSuffix(e.Name(), ".heapprofile"):
			text, err = heapprofileTop(path, 12)
		case strings.HasSuffix(e.Name(), ".txt"):
			text, err = head(path, 15)
		case strings.HasSuffix(e.Name(), ".lag.json"):
			text, err = lagSummary(path)
		default:
			continue
		}
		if err != nil {
			return err
		}
		fmt.Fprintf(w, "### %s\n\n```\n%s\n```\n\n", e.Name(), strings.TrimRight(text, "\n"))
	}
	return nil
}

// lagSummary answers a page's main-thread stalls: how long it was stalled,
// its longest stall, and the stalls past 100ms with when they happened.
func lagSummary(path string) (string, error) {
	var p struct {
		OK        bool     `json:"ok"`
		Why       string   `json:"why"`
		WallMS    int      `json:"wall_ms"`
		StalledMS int      `json:"stalled_ms"`
		LongestMS int      `json:"longest_ms"`
		Stalls    [][2]int `json:"stalls"`
	}
	if err := readJSON(path, &p); err != nil {
		return "", err
	}
	if !p.OK {
		return "the page answered no samples: " + p.Why, nil
	}
	var b strings.Builder
	fmt.Fprintf(&b, "main thread stalled %dms of %dms sampled (%d stalls past one frame), longest %dms\n",
		p.StalledMS, p.WallMS, len(p.Stalls), p.LongestMS)
	for _, s := range p.Stalls {
		if s[1] > 100 {
			fmt.Fprintf(&b, "  +%dms: stalled %dms\n", s[0], s[1])
		}
	}
	return b.String(), nil
}

func head(path string, n int) (string, error) {
	raw, err := os.ReadFile(path)
	if err != nil {
		return "", fmt.Errorf("read %s: %w", path, err)
	}
	lines := strings.SplitN(string(raw), "\n", n+1)
	if len(lines) > n {
		lines = lines[:n]
	}
	return strings.Join(lines, "\n"), nil
}

type callFrame struct {
	FunctionName string `json:"functionName"`
	URL          string `json:"url"`
	LineNumber   int    `json:"lineNumber"`
}

func (c callFrame) label() string {
	name := c.FunctionName
	if name == "" {
		name = "(anonymous)"
	}
	return fmt.Sprintf("%s %s:%d", name, filepath.Base(c.URL), c.LineNumber+1)
}

// cpuprofileTop answers a V8 CPU profile's functions by self samples.
func cpuprofileTop(path string, n int) (string, error) {
	var p struct {
		Nodes []struct {
			CallFrame callFrame `json:"callFrame"`
			HitCount  int       `json:"hitCount"`
		} `json:"nodes"`
	}
	if err := readJSON(path, &p); err != nil {
		return "", err
	}
	self := map[string]int{}
	total := 0
	for _, node := range p.Nodes {
		self[node.CallFrame.label()] += node.HitCount
		total += node.HitCount
	}
	return topLines(self, total, n, "samples"), nil
}

// heapprofileTop answers a V8 sampling heap profile's functions by self size.
func heapprofileTop(path string, n int) (string, error) {
	type heapNode struct {
		CallFrame callFrame  `json:"callFrame"`
		SelfSize  int        `json:"selfSize"`
		Children  []heapNode `json:"children"`
	}
	var p struct {
		Head heapNode `json:"head"`
	}
	if err := readJSON(path, &p); err != nil {
		return "", err
	}
	self := map[string]int{}
	total := 0
	var walk func(heapNode)
	walk = func(h heapNode) {
		self[h.CallFrame.label()] += h.SelfSize
		total += h.SelfSize
		for _, c := range h.Children {
			walk(c)
		}
	}
	walk(p.Head)
	return topLines(self, total, n, "bytes"), nil
}

func topLines(self map[string]int, total, n int, unit string) string {
	keys := make([]string, 0, len(self))
	for k, v := range self {
		if v > 0 {
			keys = append(keys, k)
		}
	}
	sort.Slice(keys, func(i, j int) bool { return self[keys[i]] > self[keys[j]] })
	if len(keys) > n {
		keys = keys[:n]
	}
	var b strings.Builder
	fmt.Fprintf(&b, "total %d %s\n", total, unit)
	for _, k := range keys {
		pct := 0.0
		if total > 0 {
			pct = 100 * float64(self[k]) / float64(total)
		}
		fmt.Fprintf(&b, "%6.1f%%  %d  %s\n", pct, self[k], k)
	}
	return b.String()
}

func readJSON(path string, v any) error {
	raw, err := os.ReadFile(path)
	if err != nil {
		return fmt.Errorf("read %s: %w", path, err)
	}
	if err := json.Unmarshal(raw, v); err != nil {
		return fmt.Errorf("parse %s: %w", path, err)
	}
	return nil
}

// readRecords reads the session's JSONL log records, in time order.
func readRecords(path string) ([]record, error) {
	var r io.Reader = os.Stdin
	if path != "-" {
		f, err := os.Open(path)
		if err != nil {
			return nil, fmt.Errorf("open the logs: %w", err)
		}
		defer f.Close()
		r = f
	}
	var out []record
	scanner := bufio.NewScanner(r)
	scanner.Buffer(make([]byte, 1<<20), 16<<20)
	line := 0
	for scanner.Scan() {
		line++
		var rec record
		if err := json.Unmarshal(scanner.Bytes(), &rec); err != nil {
			return nil, fmt.Errorf("logs line %d: %w", line, err)
		}
		at, err := time.Parse(time.RFC3339Nano, rec.Timestamp)
		if err != nil {
			return nil, fmt.Errorf("logs line %d: timestamp %q: %w", line, rec.Timestamp, err)
		}
		rec.at = at
		out = append(out, rec)
	}
	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("read the logs: %w", err)
	}
	sort.SliceStable(out, func(i, j int) bool { return out[i].at.Before(out[j].at) })
	return out, nil
}

// The edges the latency table pairs, by operation (or message prefix for
// Emacs, whose operation is derived from its format string).
func isEditorSwitch(r record) bool {
	return r.Runtime == "emacs" && strings.HasPrefix(r.Message, "elisp.host.editor-switch ")
}
func isWebviewOnSwitch(r record) bool {
	return r.Runtime == "emacs" && strings.HasPrefix(r.Message, "elisp.panels.webview-on-switch: ")
}

// isWebhostShow is Emacs telling the webhost to show a page, recorded at the
// call. (elisp.panels.webview-on-switch is recorded on the idle turn after the
// redisplay, after the page may already have been told, so it ends no phase.)
func isWebhostShow(r record) bool {
	return r.Runtime == "emacs" && strings.HasPrefix(r.Message, "elisp.webhost.show ")
}
func isFeedPainted(r record) bool { return r.Runtime == "webapp" && r.Operation == "feed.page-placed" }
func isActivationWon(r record) bool {
	return r.Runtime == "emacs" && strings.HasPrefix(r.Message, "elisp.panels.switch-activation-won ")
}
func isPageSwitchedTo(r record) bool {
	return r.Runtime == "webapp" && r.Operation == "page.switched-to"
}
func isPageShownFrame(r record) bool {
	return r.Runtime == "webapp" && r.Operation == "page.shown-frame"
}
func isPageBoot(r record) bool { return r.Runtime == "webapp" && r.Operation == "main.boot" }
func isTabOpen(r record) bool {
	return r.Runtime == "emacs" && strings.HasPrefix(r.Message, "elisp.roster.tab-open: ")
}
func isSessionUp(r record) bool {
	return r.Runtime == "daemon" && r.Operation == "daemon.workspace.bring_up" && r.Message == "the session is up"
}
func isSubmit(r record) bool {
	return r.Runtime == "emacs" && strings.HasPrefix(r.Message, "elisp.input.submit ")
}
func isPromptDrawn(r record) bool {
	return r.Runtime == "webapp" && r.Operation == "feed.draw-user-prompt"
}
func isFirstOutput(r record) bool {
	return r.Runtime == "webapp" && (r.Operation == "feed.draw-text-block" || r.Operation == "feed.draw-response")
}

func connection(r record) string {
	s, _ := r.Context["connection_id"].(string)
	return s
}

// within bounds how far after its start an edge's end may be and still pair.
const within = 30 * time.Second

// nextFor answers the first record at or after index i satisfying match for
// workspace ws inside the pairing window of start.
func nextFor(records []record, i int, ws string, start time.Time, match func(record) bool) (record, bool) {
	for j := i; j < len(records); j++ {
		r := records[j]
		if r.at.Sub(start) > within {
			break
		}
		// An edge is never earlier than the one before it: records of one
		// instant from two runtimes can sort either way.
		if r.at.Before(start) {
			continue
		}
		if r.WorkspaceID == ws && match(r) {
			return r, true
		}
	}
	return record{}, false
}

type latency struct {
	kind      string
	workspace string
	at        time.Time
	took      time.Duration
}

// pairLatencies pairs every interaction's start with its end edge.
func pairLatencies(records []record) []latency {
	var out []latency
	for i, r := range records {
		switch {
		case isEditorSwitch(r):
			shown, ok := nextFor(records, i+1, r.WorkspaceID, r.at, isWebviewOnSwitch)
			if !ok {
				continue
			}
			if strings.Contains(shown.Message, " view=first ") {
				end := shown.at
				if painted, ok := nextFor(records, i+1, r.WorkspaceID, r.at, isFeedPainted); ok && painted.at.After(end) {
					end = painted.at
				}
				out = append(out, latency{"switch_first", r.WorkspaceID, r.at, end.Sub(r.at)})
			} else if frame, ok := nextFor(records, i+1, r.WorkspaceID, r.at, isPageShownFrame); ok {
				// A SHOWN WORKSPACE'S SWITCH ENDS AT THE PAGE'S FIRST SHOWN FRAME,
				// what the user actually sees; Emacs showing its view is only the
				// half of it the editor owns (switchPhaseTable splits the rest).
				out = append(out, latency{"switch_repeat", r.WorkspaceID, r.at, frame.at.Sub(r.at)})
			}
		case isPageBoot(r):
			conn := connection(r)
			for j := i + 1; j < len(records); j++ {
				p := records[j]
				if p.at.Sub(r.at) > within {
					break
				}
				if isFeedPainted(p) && connection(p) == conn {
					out = append(out, latency{"page_boot", r.WorkspaceID, r.at, p.at.Sub(r.at)})
					break
				}
			}
		case isTabOpen(r):
			if up, ok := nextFor(records, i+1, r.WorkspaceID, r.at, isSessionUp); ok {
				out = append(out, latency{"workspace_open", r.WorkspaceID, r.at, up.at.Sub(r.at)})
			}
		case isSubmit(r):
			if drawn, ok := nextFor(records, i+1, r.WorkspaceID, r.at, isPromptDrawn); ok {
				out = append(out, latency{"prompt_echo", r.WorkspaceID, r.at, drawn.at.Sub(r.at)})
			}
			if first, ok := nextFor(records, i+1, r.WorkspaceID, r.at, isFirstOutput); ok {
				out = append(out, latency{"prompt_first_output", r.WorkspaceID, r.at, first.at.Sub(r.at)})
			}
		}
	}
	return out
}

func percentile(sorted []time.Duration, p float64) time.Duration {
	if len(sorted) == 0 {
		return 0
	}
	idx := int(p * float64(len(sorted)-1))
	return sorted[idx]
}

// spread is a set of durations' count, median, 90th percentile and maximum,
// the columns every latency table of the report prints.
type spread struct {
	n             int
	p50, p90, max time.Duration
}

// spreadOf sorts ds in place and answers its spread. Every caller holds at
// least one duration, so an empty ds is a bug and panics.
func spreadOf(ds []time.Duration) spread {
	if len(ds) == 0 {
		panic("spreadOf: no durations")
	}
	sort.Slice(ds, func(i, j int) bool { return ds[i] < ds[j] })
	return spread{len(ds), percentile(ds, 0.5), percentile(ds, 0.9), ds[len(ds)-1]}
}

// latencyTable prints each interaction's latencies ranked by how far its p90
// is over budget.
func latencyTable(w io.Writer, records []record, budgets map[string]budget) {
	byKind := map[string][]time.Duration{}
	for _, l := range pairLatencies(records) {
		byKind[l.kind] = append(byKind[l.kind], l.took)
	}
	type row struct {
		kind string
		spread
		over float64
	}
	var rows []row
	for kind, ds := range byKind {
		sp := spreadOf(ds)
		rows = append(rows, row{kind, sp, float64(sp.p90) / float64(budgets[kind].limit)})
	}
	sort.Slice(rows, func(i, j int) bool { return rows[i].over > rows[j].over })
	fmt.Fprintln(w, "## User-visible latency (real log edges), worst over budget first")
	fmt.Fprintln(w)
	fmt.Fprintln(w, "| interaction | budget | n | p50 | p90 | max | p90 / budget |")
	fmt.Fprintln(w, "|---|---|---|---|---|---|---|")
	for _, r := range rows {
		b := budgets[r.kind]
		fmt.Fprintf(w, "| %s | %s | %d | %s | %s | %s | %.1fx |\n", b.name, b.limit, r.n, ms(r.p50), ms(r.p90), ms(r.max), r.over)
	}
	var missing []string
	for kind := range budgets {
		if _, ok := byKind[kind]; !ok {
			missing = append(missing, budgets[kind].name)
		}
	}
	sort.Strings(missing)
	if len(missing) > 0 {
		fmt.Fprintf(w, "\nNot exercised in this session: %s.\n", strings.Join(missing, "; "))
	}
	fmt.Fprintln(w)
}

// The phases a switch to a shown workspace crosses, in order, each named for
// who owns it, between the edge that ends the phase before it and its own.
var switchPhases = []struct {
	name string
	end  func(record) bool
}{
	{"Emacs, from the key to the webhost told to show the page (its redisplay, hooks and switch pass)", isWebhostShow},
	{"the webhost, to the page told it is shown", isPageSwitchedTo},
	{"the page, to its first shown frame", isPageShownFrame},
}

// switchPhaseTable splits every switch to a shown workspace into the phases
// it crossed, each from the edge before it, so a slow switch names its owner.
// A switch missing any edge is left out, and counted.
func switchPhaseTable(w io.Writer, records []record) {
	took, incomplete := switchPhaseDurations(records)
	fmt.Fprintln(w, "## Switch to a shown workspace, by phase")
	fmt.Fprintln(w)
	printPhaseRows(w, took, incomplete)
}

// switchPhaseDurations answers each phase's durations over every switch to a
// shown workspace in RECORDS that crossed every edge, and how many did not.
func switchPhaseDurations(records []record) ([][]time.Duration, int) {
	took := make([][]time.Duration, len(switchPhases))
	incomplete := 0
	for i, r := range records {
		if !isEditorSwitch(r) {
			continue
		}
		if shown, ok := nextFor(records, i+1, r.WorkspaceID, r.at, isWebviewOnSwitch); !ok || strings.Contains(shown.Message, " view=first ") {
			continue
		}
		from, at := r.at, i+1
		var phase []time.Duration
		for _, p := range switchPhases {
			end, ok := nextFor(records, at, r.WorkspaceID, from, p.end)
			if !ok {
				break
			}
			phase = append(phase, end.at.Sub(from))
			from = end.at
		}
		if len(phase) != len(switchPhases) {
			incomplete++
			continue
		}
		for k, d := range phase {
			took[k] = append(took[k], d)
		}
	}
	return took, incomplete
}

// printPhaseRows prints the phase table of TOOK, and how many switches were
// left out for a missing edge.
func printPhaseRows(w io.Writer, took [][]time.Duration, incomplete int) {
	fmt.Fprintln(w, "| phase | n | p50 | p90 | max |")
	fmt.Fprintln(w, "|---|---|---|---|---|")
	for k, p := range switchPhases {
		ds := took[k]
		if len(ds) == 0 {
			continue
		}
		sp := spreadOf(ds)
		fmt.Fprintf(w, "| %s | %d | %s | %s | %s |\n", p.name, sp.n, ms(sp.p50), ms(sp.p90), ms(sp.max))
	}
	if incomplete > 0 {
		fmt.Fprintf(w, "\n%d switches left out: an edge of theirs is missing from the logs.\n", incomplete)
	}
	fmt.Fprintln(w)
}

// A switch tour (lisp/profiling.el) names itself in the records it writes.
func tourStarted(r record) (label string, ok bool) {
	if r.Runtime != "emacs" || !strings.HasPrefix(r.Message, "profiling.switch-tour-started ") {
		return "", false
	}
	return messageField(r.Message, "label"), true
}

func tourDone(r record, label string) bool {
	return r.Runtime == "emacs" && strings.HasPrefix(r.Message, "profiling.switch-tour-done ") && messageField(r.Message, "label") == label
}

func tourKey(r record, label string) bool {
	return r.Runtime == "emacs" && strings.HasPrefix(r.Message, "profiling.switch-tour-key ") && messageField(r.Message, "label") == label
}

// messageField answers KEY's value in a "op key=value key=value" message.
func messageField(message, key string) string {
	for _, field := range strings.Fields(message) {
		if value, ok := strings.CutPrefix(field, key+"="); ok {
			return value
		}
	}
	return ""
}

// tourSettle is how long after a tour's last key its switches may still be
// landing, and are still the tour's, unless the next tour starts first.
const tourSettle = 3 * time.Second

// switchTourTables prints a section per switch tour in RECORDS: the keys it
// queued, what Emacs, the webhost and the pages did with them, each switch's
// phases, and the main-thread stalls and rail draws inside it.
func switchTourTables(w io.Writer, records []record) {
	for i, r := range records {
		label, ok := tourStarted(r)
		if !ok {
			continue
		}
		end := time.Time{}
		for j, later := range records[i+1:] {
			if tourDone(later, label) {
				end = later.at.Add(tourSettle)
				// A tour's window ends where the next tour starts, if
				// that is sooner: the next tour's switches are its own.
				for _, next := range records[i+1+j+1:] {
					if !next.at.Before(end) {
						break
					}
					if _, starts := tourStarted(next); starts {
						end = next.at.Add(-time.Nanosecond)
						break
					}
				}
				break
			}
		}
		fmt.Fprintf(w, "## Switch tour %s (%s each way, %ss apart)\n\n", label, messageField(r.Message, "count"), messageField(r.Message, "interval"))
		if end.IsZero() {
			fmt.Fprintln(w, "The tour never recorded its end: it was cancelled or its Emacs went away. Nothing of it is summarized.")
			fmt.Fprintln(w)
			continue
		}
		var inside []record
		for _, x := range records[i:] {
			if x.at.After(end) {
				break
			}
			inside = append(inside, x)
		}
		var keys, switches, won, superseded, told, frames int
		var stalls, drawMs []time.Duration
		perRuntime := map[string]int{}
		for _, x := range inside {
			perRuntime[x.Runtime]++
			switch {
			case tourKey(x, label):
				keys++
			case isEditorSwitch(x):
				switches++
			case isActivationWon(x):
				won++
			case x.Runtime == "emacs" && strings.HasPrefix(x.Message, "elisp.panels.switch-activation-superseded "):
				superseded++
			case isPageSwitchedTo(x):
				told++
			case isPageShownFrame(x):
				frames++
			case x.Runtime == "webapp" && x.Operation == "webhost.stall":
				stalls = append(stalls, contextMillis(x, "blocked_ms"))
			case x.Runtime == "webapp" && x.Operation == "sidebar.roster-draw":
				drawMs = append(drawMs, contextMillis(x, "draw_ms"))
			}
		}
		fmt.Fprintln(w, "| keys queued | editor switches | switch passes run | passes superseded | pages told shown | first shown frames |")
		fmt.Fprintln(w, "|---|---|---|---|---|---|")
		fmt.Fprintf(w, "| %d | %d | %d | %d | %d | %d |\n\n", keys, switches, won, superseded, told, frames)
		var blocked, longest time.Duration
		for _, d := range stalls {
			blocked += d
			if d > longest {
				longest = d
			}
		}
		fmt.Fprintf(w, "Main-thread stalls: %d, %s blocked in all, the longest %s.\n", len(stalls), ms(blocked), ms(longest))
		if len(drawMs) > 0 {
			drawn := spreadOf(drawMs)
			fmt.Fprintf(w, "Rail draws: %d, script %s median, %s at most.\n", drawn.n, ms(drawn.p50), ms(drawn.max))
		} else {
			fmt.Fprintln(w, "Rail draws: none.")
		}
		fmt.Fprintln(w)
		recordsPerKey(w, perRuntime, keys)
		took, incomplete := switchPhaseDurations(inside)
		printPhaseRows(w, took, incomplete)
	}
}

// recordsPerKey prints how many log records each runtime wrote per key the
// tour pressed, most first: the log volume a switch costs, at the session's
// level.
func recordsPerKey(w io.Writer, perRuntime map[string]int, keys int) {
	if keys == 0 {
		fmt.Fprintln(w, "Records per key: no keys were pressed.")
		fmt.Fprintln(w)
		return
	}
	names := make([]string, 0, len(perRuntime))
	total := 0
	for name, n := range perRuntime {
		names = append(names, name)
		total += n
	}
	sort.Slice(names, func(i, j int) bool {
		if perRuntime[names[i]] != perRuntime[names[j]] {
			return perRuntime[names[i]] > perRuntime[names[j]]
		}
		return names[i] < names[j]
	})
	parts := make([]string, 0, len(names))
	for _, name := range names {
		parts = append(parts, fmt.Sprintf("%s %d", name, perRuntime[name]/keys))
	}
	fmt.Fprintf(w, "Records per key: %d in all (%s).\n\n", total/keys, strings.Join(parts, ", "))
}

// contextMillis answers a record's context number KEY as milliseconds; zero
// when it is absent or not a number.
func contextMillis(r record, key string) time.Duration {
	v, _ := r.Context[key].(float64)
	return time.Duration(v * float64(time.Millisecond))
}

// startupTable prints the startup assessment: launch to each workspace's
// session up and page painted, and every switch after.
func startupTable(w io.Writer, records []record, since time.Time) {
	fmt.Fprintf(w, "## Startup assessment from %s\n\n", since.Format(time.RFC3339))
	var firstDaemon time.Time
	up := map[string]time.Time{}
	painted := map[string]time.Time{}
	for _, r := range records {
		if r.at.Before(since) {
			continue
		}
		if r.Runtime == "daemon" && firstDaemon.IsZero() {
			firstDaemon = r.at
		}
		if isSessionUp(r) {
			if _, ok := up[r.WorkspaceID]; !ok {
				up[r.WorkspaceID] = r.at
			}
		}
		if isFeedPainted(r) {
			if _, ok := painted[r.WorkspaceID]; !ok {
				painted[r.WorkspaceID] = r.at
			}
		}
	}
	if firstDaemon.IsZero() {
		fmt.Fprintln(w, "No daemon record after the launch instant.")
	} else {
		fmt.Fprintf(w, "Daemon's first record: +%s\n\n", ms(firstDaemon.Sub(since)))
	}
	workspaces := map[string]struct{}{}
	for ws := range up {
		workspaces[ws] = struct{}{}
	}
	for ws := range painted {
		workspaces[ws] = struct{}{}
	}
	ids := make([]string, 0, len(workspaces))
	for ws := range workspaces {
		ids = append(ids, ws)
	}
	sort.Slice(ids, func(i, j int) bool {
		return latest(up[ids[i]], painted[ids[i]]).Before(latest(up[ids[j]], painted[ids[j]]))
	})
	fmt.Fprintln(w, "| workspace | session up | page painted |")
	fmt.Fprintln(w, "|---|---|---|")
	var allLoaded time.Time
	for _, ws := range ids {
		fmt.Fprintf(w, "| %s | %s | %s |\n", ws, since0(up[ws], since), since0(painted[ws], since))
		allLoaded = latest(allLoaded, latest(up[ws], painted[ws]))
	}
	if !allLoaded.IsZero() {
		fmt.Fprintf(w, "\nEvery workspace loaded: +%s after launch.\n", ms(allLoaded.Sub(since)))
	}
	fmt.Fprintln(w, "\n| switch | workspace | took |")
	fmt.Fprintln(w, "|---|---|---|")
	for _, l := range pairLatencies(records) {
		if (l.kind == "switch_first" || l.kind == "switch_repeat") && !l.at.Before(since) {
			fmt.Fprintf(w, "| %s | %s | %s |\n", strings.TrimPrefix(l.kind, "switch_"), l.workspace, ms(l.took))
		}
	}
	fmt.Fprintln(w)
}

func latest(a, b time.Time) time.Time {
	if a.After(b) {
		return a
	}
	return b
}

func since0(t, since time.Time) string {
	if t.IsZero() {
		return "never"
	}
	return "+" + ms(t.Sub(since))
}

func ms(d time.Duration) string { return fmt.Sprintf("%dms", d.Milliseconds()) }
