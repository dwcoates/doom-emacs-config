//go:build realtest

package realtest

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// The harvester's unit tests. They touch no real path and start no process:
// every one builds its logs under t.TempDir() and runs the same code the run
// does. That is the whole reason Env states its roots explicitly.

const (
	windowStart = "2026-09-10T12:00:00.000000-04:00"
	windowEnd   = "2026-09-10T12:01:00.000000-04:00"
)

func testWindow(t *testing.T) Window {
	t.Helper()
	start, err := time.Parse(time.RFC3339Nano, windowStart)
	if err != nil {
		t.Fatalf("parse the fixture window start: %v", err)
	}
	end, err := time.Parse(time.RFC3339Nano, windowEnd)
	if err != nil {
		t.Fatalf("parse the fixture window end: %v", err)
	}
	return Window{Start: start, End: end}
}

// rec renders one JSONL record the way every runtime writes it.
func rec(timestamp, runtime, level, operation, message string, extra string) string {
	base := fmt.Sprintf(
		`{"timestamp":%q,"runtime":%q,"level":%q,"verbosity":"normal","operation":%q,"message":%q,"context":{}`,
		timestamp, runtime, level, operation, message)
	if extra != "" {
		base += "," + extra
	}
	return base + "}"
}

func writeLines(t *testing.T, path string, lines ...string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("create %s: %v", filepath.Dir(path), err)
	}
	body := ""
	for _, line := range lines {
		body += line + "\n"
	}
	if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
		t.Fatalf("write %s: %v", path, err)
	}
}

func appendLines(t *testing.T, path string, lines ...string) {
	t.Helper()
	file, err := os.OpenFile(path, os.O_APPEND|os.O_WRONLY|os.O_CREATE, 0o644)
	if err != nil {
		t.Fatalf("open %s to append: %v", path, err)
	}
	defer file.Close()
	for _, line := range lines {
		if _, err := file.WriteString(line + "\n"); err != nil {
			t.Fatalf("append to %s: %v", path, err)
		}
	}
}

// harvestOneSource is the arrange/act shared by the cases below: one source,
// snapshotted, appended to, harvested.
func harvestOneSource(t *testing.T, src Source, before, after []string, workspaces []Workspace) Harvest {
	t.Helper()
	writeLines(t, src.Path, before...)
	sources := []Source{src}
	snapshot := TakeSnapshot(sources)
	appendLines(t, src.Path, after...)
	harvest, err := HarvestSources(sources, snapshot, testWindow(t), workspaces)
	if err != nil {
		t.Fatalf("harvest %s: %v", src.Path, err)
	}
	return harvest
}

func TestHarvestKeepsAWarningInsideTheWindow(t *testing.T) {
	// Arrange.
	src := Source{Name: "daemon.global", Path: filepath.Join(t.TempDir(), "daemon.run.log"), Kind: KindJSONL}
	inside := rec("2026-09-10T12:00:30.000000-04:00", "daemon", "warn", "daemon.db.slow", "a statement was slow", "")

	// Act.
	harvest := harvestOneSource(t, src, nil, []string{inside}, nil)

	// Assert.
	if harvest.Count() != 1 {
		t.Fatalf("harvested %d finding(s), want 1: %+v", harvest.Count(), harvest.Findings)
	}
	if harvest.Findings[0].Kind != KindRecord {
		t.Errorf("finding kind is %s, want %s", harvest.Findings[0].Kind, KindRecord)
	}
	if harvest.Findings[0].Raw != inside {
		t.Errorf("the finding's raw line is %q, want the original line verbatim %q", harvest.Findings[0].Raw, inside)
	}
}

func TestHarvestDropsARecordOutsideTheWindow(t *testing.T) {
	// Arrange.
	src := Source{Name: "daemon.global", Path: filepath.Join(t.TempDir(), "daemon.run.log"), Kind: KindJSONL}
	outside := rec("2026-09-10T11:59:59.000000-04:00", "daemon", "error", "daemon.link.lost", "the link went away", "")

	// Act.
	harvest := harvestOneSource(t, src, nil, []string{outside}, nil)

	// Assert.
	if harvest.Count() != 0 {
		t.Fatalf("harvested %d finding(s) from a record one second before the window, want 0: %+v",
			harvest.Count(), harvest.Findings)
	}
}

func TestHarvestAttributesByTheSinkPath(t *testing.T) {
	// Arrange: a per-workspace sink whose record carries no workspace fields
	// at all. The sink path is the only thing that says which workspace it is.
	dir := t.TempDir()
	workspace := Workspace{ID: "99808d49", Dir: dir, Name: "explanation-engine"}
	src := Source{
		Name:      "workspace.daemon.log",
		Path:      filepath.Join(dir, ".claude", "emacs", "daemon.log"),
		Kind:      KindJSONL,
		Workspace: workspace.ID,
	}
	line := rec("2026-09-10T12:00:10.000000-04:00", "daemon", "warn", "daemon.shim.dial", "the shim would not answer", "")

	// Act.
	harvest := harvestOneSource(t, src, nil, []string{line}, []Workspace{workspace})

	// Assert.
	if harvest.Count() != 1 {
		t.Fatalf("harvested %d finding(s), want 1: %+v", harvest.Count(), harvest.Findings)
	}
	if got := harvest.Findings[0].Workspace; got != workspace.ID {
		t.Errorf("the finding is attributed to %q, want the sink's own workspace %q", got, workspace.ID)
	}
}

func TestHarvestAttributesAGlobalSinkRecordToGlobal(t *testing.T) {
	// Arrange.
	src := Source{Name: "shim-store.global", Path: filepath.Join(t.TempDir(), "shim-store.log"), Kind: KindJSONL}
	line := rec("2026-09-10T12:00:20.000000-04:00", "store", "warn", "store.db.slow-query", "a statement exceeded the threshold", "")

	// Act.
	harvest := harvestOneSource(t, src, nil, []string{line}, nil)

	// Assert.
	if got := harvest.Findings[0].Workspace; got != GlobalWorkspace {
		t.Errorf("a global sink's record is attributed to %q, want %q", got, GlobalWorkspace)
	}
}

func TestHarvestSurfacesAMalformedLine(t *testing.T) {
	// Arrange: the logging contract forbids human-formatted persisted records,
	// so a line that is not JSON means something wrote to a structured log
	// that should not have.
	src := Source{Name: "daemon.global", Path: filepath.Join(t.TempDir(), "daemon.run.log"), Kind: KindJSONL}
	garbage := "panic: runtime error: invalid memory address"

	// Act.
	harvest := harvestOneSource(t, src, nil, []string{garbage}, nil)

	// Assert.
	if harvest.Count() != 1 {
		t.Fatalf("harvested %d finding(s) from an unparseable line, want 1: %+v", harvest.Count(), harvest.Findings)
	}
	if harvest.Findings[0].Kind != KindMalformed {
		t.Errorf("finding kind is %s, want %s", harvest.Findings[0].Kind, KindMalformed)
	}
	if harvest.Findings[0].Raw != garbage {
		t.Errorf("the finding's raw line is %q, want %q verbatim", harvest.Findings[0].Raw, garbage)
	}
}

func TestHarvestReadsOnlyPastTheSnapshotOffset(t *testing.T) {
	// Arrange: an in-window warning written BEFORE the snapshot belongs to
	// whoever wrote it, not to this run.
	src := Source{Name: "daemon.global", Path: filepath.Join(t.TempDir(), "daemon.run.log"), Kind: KindJSONL}
	earlier := rec("2026-09-10T12:00:05.000000-04:00", "daemon", "warn", "daemon.a.b", "before the snapshot", "")
	later := rec("2026-09-10T12:00:15.000000-04:00", "daemon", "warn", "daemon.a.b", "after the snapshot", "")

	// Act.
	harvest := harvestOneSource(t, src, []string{earlier}, []string{later}, nil)

	// Assert.
	if harvest.Count() != 1 {
		t.Fatalf("harvested %d finding(s), want only the one written after the snapshot: %+v",
			harvest.Count(), harvest.Findings)
	}
	if harvest.Findings[0].Raw != later {
		t.Errorf("harvested %q, want the line written after the snapshot", harvest.Findings[0].Raw)
	}
}

func TestHarvestReportsATruncationInPlace(t *testing.T) {
	// Arrange: the same path, smaller than it was. The records it held at the
	// snapshot offset are gone and the report has to say so.
	path := filepath.Join(t.TempDir(), "daemon.run.log")
	src := Source{Name: "daemon.global", Path: path, Kind: KindJSONL}
	writeLines(t, path,
		rec("2026-09-10T12:00:01.000000-04:00", "daemon", "info", "daemon.a.b", "a long line to make the file big", ""),
		rec("2026-09-10T12:00:02.000000-04:00", "daemon", "info", "daemon.a.b", "another long line", ""))
	snapshot := TakeSnapshot([]Source{src})
	writeLines(t, path, rec("2026-09-10T12:00:30.000000-04:00", "daemon", "info", "daemon.a.b", "x", ""))

	// Act.
	harvest, err := HarvestSources([]Source{src}, snapshot, testWindow(t), nil)
	if err != nil {
		t.Fatalf("harvest after a truncation: %v", err)
	}

	// Assert.
	found := false
	for _, finding := range harvest.Findings {
		if finding.Kind == KindRotation {
			found = true
		}
	}
	if !found {
		t.Fatalf("a truncated log produced no rotation finding: %+v", harvest.Findings)
	}
}

func TestHarvestFollowsARelinkedWorkspaceSink(t *testing.T) {
	// Arrange: the canonical link is REPLACED mid-run, which is what the
	// logging contract has a restarting runtime do. Both targets hold records
	// belonging to this run.
	root := t.TempDir()
	link := filepath.Join(root, ".claude", "emacs", "emacs.log")
	first := filepath.Join(root, "target-1.log")
	second := filepath.Join(root, "target-2.log")
	writeLines(t, first, rec("2026-09-10T12:00:05.000000-04:00", "emacs", "info", "elisp.a.b", "before the relink", ""))
	if err := os.MkdirAll(filepath.Dir(link), 0o755); err != nil {
		t.Fatalf("create the sink directory: %v", err)
	}
	if err := os.Symlink(first, link); err != nil {
		t.Fatalf("install the canonical link: %v", err)
	}
	src := Source{Name: "workspace.emacs.log", Path: link, Kind: KindJSONL, Workspace: "ws1"}
	snapshot := TakeSnapshot([]Source{src})

	oldTarget := rec("2026-09-10T12:00:10.000000-04:00", "emacs", "warn", "elisp.a.b", "written to the old target", "")
	newTarget := rec("2026-09-10T12:00:20.000000-04:00", "emacs", "error", "elisp.a.b", "written to the new target", "")
	appendLines(t, first, oldTarget)
	writeLines(t, second, newTarget)
	if err := os.Remove(link); err != nil {
		t.Fatalf("remove the old link: %v", err)
	}
	if err := os.Symlink(second, link); err != nil {
		t.Fatalf("install the replaced link: %v", err)
	}

	// Act.
	harvest, err := HarvestSources([]Source{src}, snapshot, testWindow(t), []Workspace{{ID: "ws1", Dir: root}})
	if err != nil {
		t.Fatalf("harvest across a relink: %v", err)
	}

	// Assert: both, and no rotation finding — a relink is expected, not a loss.
	if harvest.Count() != 2 {
		t.Fatalf("harvested %d finding(s) across a relink, want both targets' records: %+v",
			harvest.Count(), harvest.Findings)
	}
	for _, finding := range harvest.Findings {
		if finding.Kind == KindRotation {
			t.Errorf("a relink was reported as a rotation: %s", finding.Note)
		}
	}
}

func TestHarvestReportsAnAttributionConflict(t *testing.T) {
	// Arrange: a record in one workspace's sink naming another workspace.
	// Neither attribution is trustworthy after that.
	dir := t.TempDir()
	mine := Workspace{ID: "aaaa1111", Dir: dir, Name: "mine"}
	theirs := Workspace{ID: "bbbb2222", Dir: filepath.Join(dir, "other"), Name: "theirs"}
	src := Source{
		Name:      "workspace.daemon.log",
		Path:      filepath.Join(dir, ".claude", "emacs", "daemon.log"),
		Kind:      KindJSONL,
		Workspace: mine.ID,
	}
	line := rec("2026-09-10T12:00:30.000000-04:00", "daemon", "info", "daemon.a.b", "an ordinary info record", `"workspace_id":"bbbb2222"`)

	// Act.
	harvest := harvestOneSource(t, src, nil, []string{line}, []Workspace{mine, theirs})

	// Assert.
	if harvest.Count() != 1 {
		t.Fatalf("harvested %d finding(s) from a misrouted record, want 1: %+v", harvest.Count(), harvest.Findings)
	}
	if harvest.Findings[0].Kind != KindAttributionConflict {
		t.Errorf("finding kind is %s, want %s", harvest.Findings[0].Kind, KindAttributionConflict)
	}
}

func TestHarvestCountsInfoWithoutFailing(t *testing.T) {
	// Arrange.
	src := Source{Name: "emacs.global", Path: filepath.Join(t.TempDir(), "doom-agent-repl.log"), Kind: KindJSONL}
	lines := []string{
		rec("2026-09-10T12:00:11.000000-04:00", "emacs", "info", "elisp.link.up", "elisp.link.up address=x", ""),
		rec("2026-09-10T12:00:12.000000-04:00", "emacs", "info", "elisp.link.up", "elisp.link.up address=x", ""),
		rec("2026-09-10T12:00:13.000000-04:00", "emacs", "debug", "elisp.roster.reconcile", "elisp.roster.reconcile: tabs=2", ""),
	}

	// Act.
	harvest := harvestOneSource(t, src, nil, lines, nil)

	// Assert.
	if harvest.Count() != 0 {
		t.Fatalf("info and debug records produced %d finding(s), want 0: %+v", harvest.Count(), harvest.Findings)
	}
	if got := harvest.InfoCounts["emacs.global"]["elisp.link.up"]; got != 2 {
		t.Errorf("counted %d `elisp.link.up` info record(s), want 2", got)
	}
	if _, counted := harvest.InfoCounts["emacs.global"]["elisp.roster.reconcile"]; counted {
		t.Errorf("a debug record was counted as info")
	}
}

func TestHarvestReportsEveryStderrLine(t *testing.T) {
	// Arrange: the contract permits emergency output only when the canonical
	// sink cannot record its own failure, so a healthy service writes nothing
	// here and the line's existence is the finding.
	src := Source{Name: "shim-store.stderr", Path: filepath.Join(t.TempDir(), "shim-store.err.log"), Kind: KindStderr}

	// Act.
	harvest := harvestOneSource(t, src, nil, []string{"fatal error: all goroutines are asleep"}, nil)

	// Assert.
	if harvest.Count() != 1 {
		t.Fatalf("harvested %d finding(s) from one stderr line, want 1: %+v", harvest.Count(), harvest.Findings)
	}
	if harvest.Findings[0].Kind != KindStderrLine {
		t.Errorf("finding kind is %s, want %s", harvest.Findings[0].Kind, KindStderrLine)
	}
}

func TestHarvestSurfacesARecordWithNoLevel(t *testing.T) {
	// Arrange: a record that parses but cannot be judged against the bar.
	src := Source{Name: "daemon.global", Path: filepath.Join(t.TempDir(), "daemon.run.log"), Kind: KindJSONL}
	line := `{"timestamp":"2026-09-10T12:00:30.000000-04:00","runtime":"daemon","operation":"daemon.a.b","message":"no level"}`

	// Act.
	harvest := harvestOneSource(t, src, nil, []string{line}, nil)

	// Assert.
	if harvest.Count() != 1 || harvest.Findings[0].Kind != KindMalformed {
		t.Fatalf("a record with no level produced %+v, want one malformed finding", harvest.Findings)
	}
}

func TestHarvestSurfacesARecordWithNoTimestamp(t *testing.T) {
	// Arrange: without a timestamp the record cannot be placed in or out of
	// the window, so it can neither be kept on merit nor dropped on merit.
	src := Source{Name: "daemon.global", Path: filepath.Join(t.TempDir(), "daemon.run.log"), Kind: KindJSONL}
	line := `{"runtime":"daemon","level":"info","operation":"daemon.a.b","message":"no timestamp"}`

	// Act.
	harvest := harvestOneSource(t, src, nil, []string{line}, nil)

	// Assert.
	if harvest.Count() != 1 || harvest.Findings[0].Kind != KindMalformed {
		t.Fatalf("a record with no timestamp produced %+v, want one malformed finding", harvest.Findings)
	}
}

func TestHarvestFollowsARenamedLogWithoutReportingALoss(t *testing.T) {
	// Arrange: the daemon ROTATES daemon.run.log ON OPEN, so a cold start that
	// spawns a daemon renames the file this run snapshotted to `.1` and creates
	// a fresh one. The bytes are intact one name over; nothing was lost, and a
	// harvester that reported a loss here would fabricate a finding on every
	// daemon-spawning cold start.
	dir := t.TempDir()
	current := filepath.Join(dir, "daemon.run.log")
	rotated := filepath.Join(dir, "daemon.run.log.1")
	writeLines(t, current,
		rec("2026-09-10T12:00:01.000000-04:00", "daemon", "info", "daemon.boot", "the previous daemon", ""))
	sources := []Source{
		{Name: "daemon.global", Path: current, Kind: KindJSONL},
		{Name: "daemon.global", Path: rotated, Kind: KindJSONL},
	}
	snapshot := TakeSnapshot(sources)

	// The rotation: the snapshotted bytes move to `.1`, a fresh log takes the
	// name, and both hold records belonging to this run.
	appendLines(t, current,
		rec("2026-09-10T12:00:05.000000-04:00", "daemon", "warn", "daemon.exit", "the outgoing daemon complained", ""))
	if err := os.Rename(current, rotated); err != nil {
		t.Fatalf("rotate the daemon log: %v", err)
	}
	writeLines(t, current,
		rec("2026-09-10T12:00:09.000000-04:00", "daemon", "error", "daemon.boot", "the incoming daemon complained", ""))

	// Act.
	harvest, err := HarvestSources(sources, snapshot, testWindow(t), nil)
	if err != nil {
		t.Fatalf("harvest across a rename: %v", err)
	}

	// Assert: both records, and NO rotation finding.
	if harvest.Count() != 2 {
		t.Fatalf("harvested %d finding(s) across a rename, want the two records: %+v",
			harvest.Count(), harvest.Findings)
	}
	for _, finding := range harvest.Findings {
		if finding.Kind == KindRotation {
			t.Errorf("a rename was reported as a loss: %s", finding.Note)
		}
	}
}

func TestSnapshotOffsetForFollowsARenamedFile(t *testing.T) {
	// Arrange: the phase reader asks the snapshot where its own records start,
	// and it asks by PATH. After a rename the path holds different bytes, and
	// the honest answer is zero — all of them are this run's.
	dir := t.TempDir()
	path := filepath.Join(dir, "doom-agent-repl.log")
	writeLines(t, path, rec("2026-09-10T12:00:01.000000-04:00", "emacs", "info", "elisp.a.b", "old", ""))
	snapshot := TakeSnapshot([]Source{{Name: "emacs.global", Path: path, Kind: KindJSONL}})
	before := snapshot.OffsetFor(path)
	if before == 0 {
		t.Fatalf("the snapshot reports offset 0 for a file it saw with content")
	}
	if err := os.Rename(path, path+".prev"); err != nil {
		t.Fatalf("rotate the module log: %v", err)
	}
	writeLines(t, path, rec("2026-09-10T12:00:09.000000-04:00", "emacs", "info", "elisp.a.b", "new", ""))

	// Act.
	after := snapshot.OffsetFor(path)

	// Assert.
	if after != 0 {
		t.Errorf("the snapshot reports offset %d for a freshly created file, want 0", after)
	}
	if got := snapshot.OffsetFor(path + ".prev"); got != before {
		t.Errorf("the renamed file's offset is %d, want the %d it had under its old name", got, before)
	}
}

func collapseFixture(source, operation, level, message string, count int) []Finding {
	var findings []Finding
	for i := 0; i < count; i++ {
		findings = append(findings, Finding{
			Kind:      KindRecord,
			Source:    source,
			Path:      "/tmp/" + source + ".log",
			Line:      i + 1,
			Level:     level,
			Operation: operation,
			Message:   fmt.Sprintf("%s #%d", message, i),
			Workspace: "aaaa",
			Raw:       fmt.Sprintf(`{"message":"%s #%d"}`, message, i),
		})
	}
	return findings
}

func TestCollapseFindingsCountsARepeatedClassOnce(t *testing.T) {
	// Arrange: the sidecar's discover-meta flood — one class, many records.
	findings := collapseFixture("sidecar", "sidecar.discover-meta", "warn", "no meta", 4988)

	// Act.
	classes := CollapseFindings(findings)

	// Assert.
	if len(classes) != 1 {
		t.Fatalf("the flood collapsed to %d classes, want 1", len(classes))
	}
	if classes[0].Count != 4988 {
		t.Errorf("the class counts %d records, want 4988", classes[0].Count)
	}
}

func TestCollapseFindingsKeepsTheFirstRecordAsTheSample(t *testing.T) {
	// Arrange: the sample is evidence, so it is a real record rather than a
	// synthesized one.
	findings := collapseFixture("shim", "shim.engine.session", "warn", "routing", 3)

	// Act.
	classes := CollapseFindings(findings)

	// Assert.
	if classes[0].Sample.Raw != findings[0].Raw {
		t.Errorf("the sample reads %q, want the first record %q", classes[0].Sample.Raw, findings[0].Raw)
	}
}

func TestCollapseFindingsSeparatesTwoLevelsOfTheSameOperation(t *testing.T) {
	// Arrange: a warn and an error from one call site are different news.
	findings := append(
		collapseFixture("daemon", "daemon.serve", "warn", "slow", 2),
		collapseFixture("daemon", "daemon.serve", "error", "refused", 2)...)

	// Act.
	classes := CollapseFindings(findings)

	// Assert.
	if len(classes) != 2 {
		t.Fatalf("a warn and an error collapsed into %d classes, want 2", len(classes))
	}
}

func TestCollapseFindingsSeparatesTwoRuntimesOfTheSameOperation(t *testing.T) {
	// Arrange: which runtime wrote it is part of what a class is.
	findings := append(
		collapseFixture("shim", "session.keepalive", "warn", "keepalive", 2),
		collapseFixture("daemon", "session.keepalive", "warn", "keepalive", 2)...)

	// Act.
	classes := CollapseFindings(findings)

	// Assert.
	if len(classes) != 2 {
		t.Fatalf("two runtimes collapsed into %d classes, want 2", len(classes))
	}
}

func TestWriteFullHarvestKeepsEveryRecord(t *testing.T) {
	// Arrange: the manifest reports a class once, so the sibling file is the
	// only place every record survives.
	dir := t.TempDir()
	findings := collapseFixture("sidecar", "sidecar.discover-meta", "warn", "no meta", 12)

	// Act.
	path, err := WriteFullHarvest(dir, findings)
	if err != nil {
		t.Fatalf("write the full harvest: %v", err)
	}

	// Assert.
	body, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read %s: %v", path, err)
	}
	lines := 0
	for _, line := range strings.Split(strings.TrimSpace(string(body)), "\n") {
		if line != "" {
			lines++
		}
	}
	if lines != 12 {
		t.Errorf("%s holds %d records, want all 12", path, lines)
	}
}

func TestWriteFullHarvestNamesTheFileBesideTheManifest(t *testing.T) {
	// Arrange/Act: the manifest points at this name, so the name is asserted.
	dir := t.TempDir()
	path, err := WriteFullHarvest(dir, nil)
	if err != nil {
		t.Fatalf("write the full harvest: %v", err)
	}

	// Assert.
	if filepath.Base(path) != "HARVEST-FULL.jsonl" {
		t.Errorf("the full harvest landed at %s, want HARVEST-FULL.jsonl", path)
	}
}

func TestManifestReportsARepeatedClassOnceWithItsCount(t *testing.T) {
	// Arrange: 6000 lines of one class is a document nobody can rule on.
	dir := t.TempDir()
	manifest := Manifest{
		Title:    "fixture",
		Started:  testWindow(t).Start,
		Ended:    testWindow(t).End,
		Findings: collapseFixture("shim", "shim.engine.session", "warn", "routing", 2500),
	}

	// Act.
	path, err := manifest.Write(dir)
	if err != nil {
		t.Fatalf("write the manifest: %v", err)
	}
	body, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read %s: %v", path, err)
	}

	// Assert.
	if !strings.Contains(string(body), "**2500 records**") {
		t.Errorf("the manifest does not report the class's count")
	}
	if strings.Count(string(body), "shim.engine.session") > 3 {
		t.Errorf("the manifest still reproduces the whole class: %d mentions",
			strings.Count(string(body), "shim.engine.session"))
	}
}

func TestManifestWritesTheFullHarvestBesideItself(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	manifest := Manifest{Title: "fixture", Findings: collapseFixture("shim", "op", "warn", "m", 4)}

	// Act.
	if _, err := manifest.Write(dir); err != nil {
		t.Fatalf("write the manifest: %v", err)
	}

	// Assert.
	if _, err := os.Stat(filepath.Join(dir, "HARVEST-FULL.jsonl")); err != nil {
		t.Errorf("the manifest was written without the full harvest beside it: %v", err)
	}
}


func TestManifestSaysTheLaunchMethodOnTheFocusLine(t *testing.T) {
	// Arrange: whether focus moved is a fact ABOUT a launch method.
	dir := t.TempDir()
	manifest := Manifest{
		Title: "fixture",
		Runs: []ManifestRun{{
			Index:       1,
			Method:      MethodDirectRestore,
			FrontBefore: "Google Chrome",
			FrontAfter:  "Emacs",
			Disturbed:   true,
		}},
	}

	// Act.
	path, err := manifest.Write(dir)
	if err != nil {
		t.Fatalf("write the manifest: %v", err)
	}
	body, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read %s: %v", path, err)
	}

	// Assert.
	for _, line := range strings.Split(string(body), "\n") {
		if strings.Contains(line, "FOCUS MOVED") {
			if !strings.Contains(line, string(MethodDirectRestore)) {
				t.Errorf("the focus line %q does not name the launch method", line)
			}
			return
		}
	}
	t.Errorf("the manifest has no focus line at all")
}
