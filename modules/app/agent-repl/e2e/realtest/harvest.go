//go:build realtest

package realtest

import (
	"bufio"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"syscall"
	"time"
)

// THE REMEDIATION BAR. A realtest is remediated if and only if every warning
// and every error across every log is resolved, with NO allowlist
// (docs/REALTEST-PLAN.md). The harvester is what makes that a verdict rather
// than an intention: it reads every source named in AGENTS.md's "Logs" section,
// keeps every record inside the run window whose level is warn or worse,
// surfaces every line that is not a record at all, and reports the count. A
// non-zero count fails the run.
//
// It reads by BYTE OFFSET, snapshotted before the run. Three reasons, all
// measured rather than anticipated:
//
//  1. The service logs reach multiple gigabytes. Filtering a whole file by
//     timestamp would read all of it.
//  2. A file that ROTATED or was truncated during the run is detectable
//     exactly here — its size went DOWN — and that is itself worth reporting,
//     because the records that were in it are gone.
//  3. A source the run itself created snapshots at zero and reads whole, with
//     no second enumeration pass.
//
// The window is applied on top of the offset, not instead of it: a log the
// daemon shares across runs can hold, at the offset, records a previous run
// wrote microseconds before this one began.

// Level spellings the harvester treats as a finding. The logging contract
// reserves `warn` and `error`; `warning`, `fatal` and `panic` are accepted too
// because a runtime that spells its own severity differently must not be able
// to hide behind the spelling.
var findingLevels = map[string]bool{
	"warn":    true,
	"warning": true,
	"error":   true,
	"fatal":   true,
	"panic":   true,
}

// FindingKind says what sort of evidence a finding is, because the three do not
// mean the same thing and a report that collapsed them would read wrong.
type FindingKind int

const (
	// KindRecord is a structured record at warn or worse. The ordinary case.
	KindRecord FindingKind = iota
	// KindMalformed is a line in a structured log that is not a record: it
	// does not parse as JSON, or it parses but carries no usable timestamp or
	// no level. It is NOT skipped. Something wrote to a structured log that
	// should not have, and a harvester that quietly dropped it would be
	// hiding the one class of log defect nothing else can see.
	KindMalformed
	// KindStderrLine is a line appended to a service's `.err.log` during the
	// window. There is no level to judge: a healthy service writes nothing
	// there at all, so the line's existence is the finding.
	KindStderrLine
	// KindMessagesLine is a line in Emacs's own *Messages* buffer matching one
	// of the severity shapes in messages.go.
	KindMessagesLine
	// KindAttributionConflict is a record whose own workspace fields disagree
	// with the workspace its SINK belongs to. Neither value is trustworthy
	// after that, and the routing invariant lisp/core.el enforces has been
	// broken somewhere, so it is reported in its own right.
	KindAttributionConflict
	// KindRotation is a source whose size shrank during the run: it rotated or
	// was truncated, and whatever it held at the snapshot offset is gone.
	KindRotation
	// KindDangling is a canonical sink LINK that exists but resolves to no
	// file: its target was removed while the link was left behind, so any
	// records it would have named are unreachable. It is distinct from a sink
	// path that was simply never created, which is an ordinary "this workspace
	// has no records yet" and no finding at all.
	KindDangling
)

func (k FindingKind) String() string {
	switch k {
	case KindRecord:
		return "record"
	case KindMalformed:
		return "malformed"
	case KindStderrLine:
		return "stderr"
	case KindMessagesLine:
		return "messages"
	case KindAttributionConflict:
		return "attribution-conflict"
	case KindRotation:
		return "rotation"
	case KindDangling:
		return "dangling"
	default:
		return fmt.Sprintf("FindingKind(%d)", int(k))
	}
}

// GlobalWorkspace is the attribution for a record that names no workspace.
// Spelled out rather than left as the empty string so a report never shows a
// blank cell where an answer belongs.
const GlobalWorkspace = "global"

// Finding is one piece of evidence, verbatim.
//
// Raw is the whole original line and is never reconstructed from the parsed
// fields: the owner rules on the evidence, and a paraphrase is not evidence.
type Finding struct {
	Kind      FindingKind
	Source    string
	Path      string
	Line      int
	Timestamp time.Time
	Level     string
	Operation string
	Message   string
	Workspace string
	Raw       string
	// Note carries the harvester's own remark, for the kinds where the finding
	// is about the line rather than in it (a parse failure, a conflict, a
	// rotation).
	Note string
}

// record is the subset of the logging contract the harvester reads. Every
// system's records share it; see AGENTS.md "Logs".
type record struct {
	Timestamp       string          `json:"timestamp"`
	Runtime         string          `json:"runtime"`
	Level           string          `json:"level"`
	Verbosity       string          `json:"verbosity"`
	Operation       string          `json:"operation"`
	Message         string          `json:"message"`
	WorkspaceID     string          `json:"workspace_id"`
	WorkspaceDir    string          `json:"workspace_dir"`
	PseudoWorkspace string          `json:"pseudo_workspace"`
	Context         json.RawMessage `json:"context"`
}

// Snapshot is where every source stood before the run.
//
// IT IS KEYED BY INODE, not by path, and that is the whole design. Two things
// this module does routinely move a log's bytes from one path to another while
// a run is in flight:
//
//	The daemon ROTATES `daemon.run.log` ON OPEN (docs/LOGGING.md records this as
//	a gap: a bounce loop evicts history). A cold start that spawns a daemon
//	therefore renames the file this run snapshotted to `daemon.run.log.1` and
//	creates a fresh one. Path-keyed offsets would read the new file's whole
//	content as this run's — which it is — and ALSO report the old path as having
//	shrunk, which is a fabricated finding about a file that is still intact one
//	name over.
//
//	A restarting runtime REPLACES a workspace's canonical symlink with a new
//	target, exactly as logging-contract.md specifies. The link's path is
//	unchanged and its bytes are completely different.
//
// An inode is the identity of the BYTES. Keyed by it, a file that was renamed
// is read from the offset it had under its old name, a file that is new is read
// whole, and the only thing that reads as a loss is the one thing that IS one:
// the same inode with fewer bytes in it than before, which is a truncation in
// place.
type Snapshot struct {
	// Sizes is inode identity -> byte size at snapshot time. A key absent
	// from the map names bytes this run has not seen before, read from zero.
	Sizes map[string]int64
	// Resolved is source path -> the path it resolved to at snapshot time,
	// so a relinked source can still be read at its OLD target for whatever
	// the outgoing runtime wrote just before the swap.
	Resolved map[string]string
	Taken    time.Time
}

// inodeKey identifies a file by its bytes rather than by its name.
//
// Device and inode together, because an inode number is only unique within a
// filesystem and this run reads across at least two — the state root on the
// boot volume and the module log under the OS temporary directory.
func inodeKey(info os.FileInfo) (string, bool) {
	stat, ok := info.Sys().(*syscall.Stat_t)
	if !ok {
		return "", false
	}
	return fmt.Sprintf("%d:%d", stat.Dev, stat.Ino), true
}

// TakeSnapshot records every enumerated source's identity and size.
//
// A source that does not exist is recorded as absent rather than as zero, and
// the distinction matters: absent means "read the whole file, the run created
// it", while a recorded zero means "these bytes existed and there were none of
// them" — and only the second one makes a later shrink a truncation.
func TakeSnapshot(sources []Source) Snapshot {
	snap := Snapshot{
		Sizes:    make(map[string]int64, len(sources)),
		Resolved: make(map[string]string, len(sources)),
		Taken:    time.Now(),
	}
	for _, src := range sources {
		resolved, err := filepath.EvalSymlinks(src.Path)
		if err != nil {
			continue
		}
		info, err := os.Stat(resolved)
		if err != nil || info.IsDir() {
			continue
		}
		snap.Resolved[src.Path] = resolved
		if key, ok := inodeKey(info); ok {
			snap.Sizes[key] = info.Size()
		}
	}
	return snap
}

// sourceRead is one file standing behind a source and the offset its bytes are
// read from.
//
// shrankFrom is non-zero when the SAME inode now holds fewer bytes than the
// snapshot recorded: a truncation in place, where offset is reset to zero and
// the records the file held past shrankFrom are gone from every path. size is
// the file's current length, carried so the caller can name it in the finding.
type sourceRead struct {
	path       string
	offset     int64
	shrankFrom int64
	size       int64
}

// resolveReads applies the log-symlink re-resolution invariant to one source
// and returns every file to read behind it, each with the offset its bytes are
// read from.
//
// THE INVARIANT (docs/REALTEST-JUDGEMENT-CALLS.md, realtest 1, row 28): a
// per-workspace sink is a SYMLINK whose IDENTITY is the link and whose target
// can change between the run-start snapshot and the read. A new Emacs instance
// can append to the standing target (same inode, grown), mint a fresh one (new
// inode, read whole), or a cap rotation can replace it (new inode, the old
// bytes moved aside). A reader that seeks to a byte offset taken against the
// symlink PATH would seek past the end of a replaced or truncated target and
// read nothing — which is precisely what made run 3 report a startup that had
// drawn every tab as "no tab drawn". So the offset is taken from the resolved
// target's INODE, never from the path: an inode the snapshot knew is read from
// where this run left it (unless it shrank, which reads from zero and is
// reported), and an inode the snapshot never saw is read whole from zero. When
// a relink has moved the link since the snapshot, the prior target is read too,
// because a restarting runtime writes to the old target right up to the swap.
//
// The second return value is true when the link itself exists but resolves to
// no file — a dangling sink — which the caller reports in its own right. A path
// that was never created at all is neither a read nor a dangling finding: it is
// an ordinary absent sink.
func resolveReads(src Source, snap Snapshot) ([]sourceRead, bool, error) {
	candidates := make([]string, 0, 2)
	dangling := false

	resolved, err := filepath.EvalSymlinks(src.Path)
	switch {
	case err == nil:
		candidates = append(candidates, resolved)
	case os.IsNotExist(err):
		// The link resolves to nothing. A link that EXISTS but names a gone
		// target is dangling and worth reporting; a path that was never created
		// is an ordinary absent sink and is not.
		if info, lerr := os.Lstat(src.Path); lerr == nil && info.Mode()&os.ModeSymlink != 0 {
			dangling = true
		}
	default:
		return nil, false, fmt.Errorf("resolve the log %s: %w", src.Path, err)
	}

	if prior, ok := snap.Resolved[src.Path]; ok && prior != resolved {
		candidates = append(candidates, prior)
	}

	var reads []sourceRead
	for _, candidate := range candidates {
		info, err := os.Stat(candidate)
		if err != nil {
			if os.IsNotExist(err) {
				continue
			}
			return nil, false, fmt.Errorf("stat the log %s: %w", candidate, err)
		}
		read := sourceRead{path: candidate, size: info.Size()}
		if key, ok := inodeKey(info); ok {
			if known, seen := snap.Sizes[key]; seen {
				read.offset = known
				if info.Size() < known {
					read.shrankFrom = known
					read.offset = 0
				}
			}
		}
		reads = append(reads, read)
	}
	return reads, dangling, nil
}

// Window is the interval a run's records must fall in.
//
// Both ends are INCLUSIVE. A record whose timestamp equals Start or End is
// inside: the alternative is a half-open interval whose excluded end can drop
// the very last record a run wrote, which is routinely the one that says why
// it ended.
type Window struct {
	Start time.Time
	End   time.Time
}

func (w Window) contains(t time.Time) bool {
	return !t.Before(w.Start) && !t.After(w.End)
}

// Harvest is the whole result of one run's log harvest.
type Harvest struct {
	Findings []Finding
	// InfoCounts is source -> operation -> count, over records inside the
	// window at info level. Unexpected info is REPORTED, never failed: a count
	// that jumps is a lead, and there is no expected set to compare against.
	InfoCounts map[string]map[string]int
	// SourcesRead is every source that contributed bytes, with how many.
	SourcesRead map[string]int64
}

// Count is how many findings the run produced. The run fails when it is
// non-zero.
func (h Harvest) Count() int { return len(h.Findings) }

// HarvestSources reads every source from its snapshot offset and returns every
// finding inside the window.
//
// Sources are re-enumerated by the CALLER before this runs, so rotation
// siblings the run itself created are included; this function only reads what
// it is handed.
func HarvestSources(sources []Source, snap Snapshot, window Window, workspaces []Workspace) (Harvest, error) {
	out := Harvest{
		InfoCounts:  make(map[string]map[string]int),
		SourcesRead: make(map[string]int64),
	}
	index := newWorkspaceIndex(workspaces)

	for _, src := range sources {
		reads, findings, err := plan(src, snap, index)
		if err != nil {
			return out, err
		}
		out.Findings = append(out.Findings, findings...)
		for _, r := range reads {
			read, found, err := harvestOne(src, r.path, r.offset, window, index, out.InfoCounts)
			if err != nil {
				return out, err
			}
			if read > 0 {
				out.SourcesRead[r.path] = read
			}
			out.Findings = append(out.Findings, found...)
		}
	}

	sortFindings(out.Findings)
	return out, nil
}

// readPlan is one file to read, and from where.
type readPlan struct {
	path   string
	offset int64
}

// plan decides which files stand behind one source and from what offset each is
// read, and reports the one case that is a real loss.
//
// Two files can stand behind one source: whatever it resolves to now, and
// whatever it resolved to at snapshot time when a relink has moved it. Both are
// read, because a restarting runtime writes to the old target right up to the
// swap.
//
// Each file's offset comes from its INODE, so a log that was RENAMED — the
// daemon's rotate-on-open — is read from where this run left it under its old
// name, with nothing reported. The only finding is the same inode holding fewer
// bytes than before: a truncation in place, where the records between the
// offset and the new end no longer exist anywhere.
func plan(src Source, snap Snapshot, index *workspaceIndex) ([]readPlan, []Finding, error) {
	reads, dangling, err := resolveReads(src, snap)
	if err != nil {
		return nil, nil, err
	}

	var plans []readPlan
	var findings []Finding
	if dangling {
		findings = append(findings, Finding{
			Kind:      KindDangling,
			Source:    src.Name,
			Path:      src.Path,
			Workspace: sourceWorkspace(src, index),
			Note: "the canonical sink link exists but resolves to no file: its target was removed while " +
				"the link was left behind, so any records it would have named are unreachable",
		})
	}
	for _, r := range reads {
		if r.shrankFrom > 0 {
			findings = append(findings, Finding{
				Kind:      KindRotation,
				Source:    src.Name,
				Path:      r.path,
				Workspace: sourceWorkspace(src, index),
				Note: fmt.Sprintf(
					"these bytes were %d long at snapshot and are %d long now, under the same inode: the file was truncated in place during the run, and the records it held past %d are gone from every path",
					r.shrankFrom, r.size, r.size),
			})
		}
		plans = append(plans, readPlan{path: r.path, offset: r.offset})
	}
	return plans, findings, nil
}

// harvestOne reads ONE source from offset to end.
func harvestOne(src Source, path string, offset int64, window Window, index *workspaceIndex, infoCounts map[string]map[string]int) (int64, []Finding, error) {
	file, err := os.Open(path)
	if err != nil {
		if os.IsNotExist(err) {
			return 0, nil, nil
		}
		return 0, nil, fmt.Errorf("open the log %s: %w", path, err)
	}
	defer file.Close()

	if offset > 0 {
		if _, err := file.Seek(offset, io.SeekStart); err != nil {
			return 0, nil, fmt.Errorf("seek %s to the snapshot offset %d: %w", path, offset, err)
		}
	}

	var findings []Finding
	scanner := bufio.NewScanner(file)
	// A single record can be long: a daemon record carrying a git diff or a
	// shim record carrying a tool result routinely exceeds bufio's 64KiB
	// default, and a truncated line would surface as a bogus parse failure.
	scanner.Buffer(make([]byte, 0, 1<<20), 1<<24)

	var read int64
	line := 0
	for scanner.Scan() {
		text := scanner.Text()
		read += int64(len(text)) + 1
		line++
		if strings.TrimSpace(text) == "" {
			continue
		}
		if src.Kind == KindStderr {
			// No level, no timestamp, no window: a healthy service writes
			// nothing here, so every appended line is the finding.
			findings = append(findings, Finding{
				Kind:      KindStderrLine,
				Source:    src.Name,
				Path:      path,
				Line:      line,
				Workspace: sourceWorkspace(src, index),
				Raw:       text,
				Note:      "a line was appended to this service's stderr during the run window; a healthy service writes nothing here",
			})
			continue
		}
		finding, counted, ok := classify(src, path, line, text, window, index)
		if counted != "" {
			bucket := infoCounts[src.Name]
			if bucket == nil {
				bucket = make(map[string]int)
				infoCounts[src.Name] = bucket
			}
			bucket[counted]++
		}
		if ok {
			findings = append(findings, finding)
		}
	}
	if err := scanner.Err(); err != nil {
		return read, findings, fmt.Errorf("read the log %s: %w", path, err)
	}
	return read, findings, nil
}

// classify decides what one structured line is.
//
// It returns the finding (when there is one), the operation to count as info
// (when the line is an in-window info record), and whether the finding is real.
func classify(src Source, path string, line int, text string, window Window, index *workspaceIndex) (Finding, string, bool) {
	base := Finding{
		Source: src.Name,
		Path:   path,
		Line:   line,
		Raw:    text,
	}

	var rec record
	if err := json.Unmarshal([]byte(text), &rec); err != nil {
		// NOT SKIPPED. A line that is not JSON in a structured log means
		// something wrote to it that should not have — a panic, a stray
		// printf, a partial write — and it is exactly the class of defect
		// nothing else in the system would notice.
		base.Kind = KindMalformed
		base.Workspace = sourceWorkspace(src, index)
		base.Note = fmt.Sprintf("the line does not parse as a log record: %v", err)
		return base, "", true
	}

	ts, tsErr := time.Parse(time.RFC3339Nano, rec.Timestamp)
	if tsErr != nil {
		base.Kind = KindMalformed
		base.Level = rec.Level
		base.Operation = rec.Operation
		base.Message = rec.Message
		base.Workspace = attribute(src, rec, index, &base)
		base.Note = fmt.Sprintf("the record carries no usable timestamp (%q): it cannot be placed in or out of the run window", rec.Timestamp)
		return base, "", true
	}
	base.Timestamp = ts

	if !window.contains(ts) {
		// Outside the run. Dropped, and deliberately not reported: the shared
		// logs hold months of records and a run is answerable only for its
		// own window.
		return Finding{}, "", false
	}

	if rec.Level == "" {
		base.Kind = KindMalformed
		base.Operation = rec.Operation
		base.Message = rec.Message
		base.Workspace = attribute(src, rec, index, &base)
		base.Note = "the record carries no level: it cannot be judged against the remediation bar"
		return base, "", true
	}

	base.Level = rec.Level
	base.Operation = rec.Operation
	base.Message = rec.Message
	base.Workspace = attribute(src, rec, index, &base)

	if findingLevels[strings.ToLower(rec.Level)] {
		base.Kind = KindRecord
		return base, "", true
	}
	if base.Kind == KindAttributionConflict {
		// The record's severity is fine but its routing is not, and
		// `attribute` already said so.
		return base, "", true
	}
	if strings.EqualFold(rec.Level, "info") {
		return Finding{}, operationOrUnnamed(rec.Operation), false
	}
	return Finding{}, "", false
}

func operationOrUnnamed(operation string) string {
	if operation == "" {
		return "(no operation)"
	}
	return operation
}

// workspaceIndex resolves a record's or a sink's workspace to one canonical id.
//
// It exists because the same workspace is named three ways across the logs —
// the full id in a record's `workspace_id`, a SHORTER PREFIX in a state-root
// log's file name, and the project directory in `workspace_dir` — and a report
// that listed them as three workspaces would be wrong about the one thing it is
// for.
type workspaceIndex struct {
	byID  map[string]Workspace
	byDir map[string]Workspace
	all   []Workspace
}

func newWorkspaceIndex(workspaces []Workspace) *workspaceIndex {
	index := &workspaceIndex{
		byID:  make(map[string]Workspace, len(workspaces)),
		byDir: make(map[string]Workspace, len(workspaces)),
		all:   workspaces,
	}
	for _, ws := range workspaces {
		if ws.ID != "" {
			index.byID[ws.ID] = ws
		}
		if ws.Dir != "" {
			index.byDir[strings.TrimSuffix(ws.Dir, "/")] = ws
		}
	}
	return index
}

// resolveID maps an id, which may be a prefix of a known one or have a known
// one as its prefix, onto the canonical id. An id that matches nothing is
// returned unchanged: it names a workspace the state database does not hold,
// which is a fact about the run worth carrying into the report rather than
// erasing.
func (index *workspaceIndex) resolveID(id string) string {
	if id == "" {
		return ""
	}
	if ws, ok := index.byID[id]; ok {
		return ws.ID
	}
	for _, ws := range index.all {
		if ws.ID == "" {
			continue
		}
		if strings.HasPrefix(ws.ID, id) || strings.HasPrefix(id, ws.ID) {
			return ws.ID
		}
	}
	return id
}

func (index *workspaceIndex) resolveDir(dir string) string {
	if dir == "" {
		return ""
	}
	if ws, ok := index.byDir[strings.TrimSuffix(dir, "/")]; ok {
		return ws.ID
	}
	return ""
}

// sourceWorkspace is the workspace a SINK belongs to, for the finding kinds
// that have no record to read it from.
func sourceWorkspace(src Source, index *workspaceIndex) string {
	if src.Workspace == "" {
		return GlobalWorkspace
	}
	return index.resolveID(src.Workspace)
}

// attribute decides which workspace a record belongs to, and reports a
// disagreement between the record and its sink as a finding of its own by
// setting finding.Kind.
//
// The order is the record first, the sink second, global last. The record is
// preferred because lisp/core.el makes the record's identity fields and the
// sink path agree by construction — so when they DON'T, the record is the one
// carrying the daemon's or the shim's own answer, and the mismatch is the news.
func attribute(src Source, rec record, index *workspaceIndex, finding *Finding) string {
	fromRecord := index.resolveID(rec.WorkspaceID)
	if fromRecord == "" {
		fromRecord = index.resolveDir(rec.WorkspaceDir)
	}
	fromSink := ""
	if src.Workspace != "" {
		fromSink = index.resolveID(src.Workspace)
	}

	if fromRecord != "" && fromSink != "" && fromRecord != fromSink {
		finding.Kind = KindAttributionConflict
		finding.Note = fmt.Sprintf(
			"the record names workspace %s (workspace_id=%q workspace_dir=%q) but its sink belongs to workspace %s: the log routing invariant is broken somewhere and neither attribution can be trusted",
			fromRecord, rec.WorkspaceID, rec.WorkspaceDir, fromSink)
		return fromRecord
	}
	if fromRecord != "" {
		return fromRecord
	}
	if fromSink != "" {
		return fromSink
	}
	// A pseudo-perspective owns no sink and is not a workspace, so its records
	// belong to `global` — but the name it carries is preserved on the finding
	// so the line still says which perspective it is about.
	if rec.PseudoWorkspace != "" && finding.Note == "" {
		finding.Note = fmt.Sprintf("attributed to the pseudo-perspective %q, which owns no workspace sink", rec.PseudoWorkspace)
	}
	return GlobalWorkspace
}

// sortFindings orders a report: by workspace, then by time, then by source.
//
// Workspace first because the owner reads the report per workspace, and a
// finding with no timestamp (a malformed line, a rotation) sorts before the
// timed ones in its group rather than being scattered through them.
func sortFindings(findings []Finding) {
	sort.SliceStable(findings, func(i, j int) bool {
		a, b := findings[i], findings[j]
		if a.Workspace != b.Workspace {
			if a.Workspace == GlobalWorkspace {
				return true
			}
			if b.Workspace == GlobalWorkspace {
				return false
			}
			return a.Workspace < b.Workspace
		}
		if !a.Timestamp.Equal(b.Timestamp) {
			return a.Timestamp.Before(b.Timestamp)
		}
		if a.Source != b.Source {
			return a.Source < b.Source
		}
		return a.Line < b.Line
	})
}

// A REPEATED FINDING IS ONE FINDING, READ MANY TIMES. Realtest 1's first run
// produced 5926 findings, of which about six thousand lines of the manifest
// were the same two classes: every shim record in one workspace flagged for the
// same attribution conflict, and the sidecar's `discover-meta` repeated 4988
// times. A document nobody can read is not evidence the owner can rule on, and
// the reason each of those is a finding is identical in every copy.
//
// So the manifest carries one line per CLASS with the count and one sample,
// and every record is kept verbatim in HARVEST-FULL.jsonl beside it. The count
// is what keeps this from being a filter: nothing is dropped, and the total per
// class is in the document the owner reads.

// FindingClass is one repeated finding, with how many times it occurred.
//
// The key is (source, operation, level, kind) — the runtime that wrote it, the
// call site it came from, how bad it said it was, and what sort of evidence it
// is. Two records that agree on all four are the same defect seen twice; the
// message's own arguments differ between them and are exactly what the sample
// and the full file are for.
type FindingClass struct {
	Source    string
	Operation string
	Level     string
	Kind      FindingKind
	Workspace string
	// Count is how many findings fell in this class. One means the class is
	// a single finding and is reported in full.
	Count int
	// Sample is the first finding of the class, verbatim.
	Sample Finding
}

// CollapseFindings groups findings into classes, in first-appearance order.
func CollapseFindings(findings []Finding) []FindingClass {
	var out []FindingClass
	at := make(map[string]int)
	for _, finding := range findings {
		key := strings.Join([]string{
			finding.Source,
			operationOrUnnamed(finding.Operation),
			finding.Level,
			finding.Kind.String(),
			finding.Workspace,
		}, "\x00")
		if index, ok := at[key]; ok {
			out[index].Count++
			continue
		}
		at[key] = len(out)
		out = append(out, FindingClass{
			Source:    finding.Source,
			Operation: operationOrUnnamed(finding.Operation),
			Level:     finding.Level,
			Kind:      finding.Kind,
			Workspace: finding.Workspace,
			Count:     1,
			Sample:    finding,
		})
	}
	return out
}

// fullHarvestName is the sibling file every finding is written to, verbatim.
const fullHarvestName = "HARVEST-FULL.jsonl"

// WriteFullHarvest writes every finding to dir/HARVEST-FULL.jsonl, one JSON
// object per line, in harvest order.
//
// It is written even when the manifest reports every finding in full, so a
// reader never has to work out whether the file exists for this run.
func WriteFullHarvest(dir string, findings []Finding) (string, error) {
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return "", fmt.Errorf("create the run directory %s: %w", dir, err)
	}
	path := filepath.Join(dir, fullHarvestName)
	var body strings.Builder
	for _, finding := range findings {
		line, err := json.Marshal(struct {
			Kind      string `json:"kind"`
			Source    string `json:"source"`
			Path      string `json:"path"`
			Line      int    `json:"line"`
			Timestamp string `json:"timestamp"`
			Level     string `json:"level"`
			Operation string `json:"operation"`
			Message   string `json:"message"`
			Workspace string `json:"workspace"`
			Note      string `json:"note"`
			Raw       string `json:"raw"`
		}{
			Kind:      finding.Kind.String(),
			Source:    finding.Source,
			Path:      finding.Path,
			Line:      finding.Line,
			Timestamp: findingTimestamp(finding),
			Level:     finding.Level,
			Operation: finding.Operation,
			Message:   finding.Message,
			Workspace: finding.Workspace,
			Note:      finding.Note,
			Raw:       finding.Raw,
		})
		if err != nil {
			return "", fmt.Errorf("render a finding from %s as JSON: %w", finding.Path, err)
		}
		body.Write(line)
		body.WriteString("\n")
	}
	if err := os.WriteFile(path, []byte(body.String()), 0o644); err != nil {
		return "", fmt.Errorf("write %s: %w", path, err)
	}
	return path, nil
}

func findingTimestamp(finding Finding) string {
	if finding.Timestamp.IsZero() {
		return ""
	}
	return finding.Timestamp.Format(time.RFC3339Nano)
}
