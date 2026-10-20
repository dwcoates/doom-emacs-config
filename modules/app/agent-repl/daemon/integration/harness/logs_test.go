package harness

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strings"
	"testing"
	"time"
)

func TestTimestampPatternAcceptsTheContractedShapes(t *testing.T) {
	tests := []struct {
		name  string
		value string
	}{
		{name: "utc seconds", value: "2026-08-29T14:44:00Z"},
		{name: "utc fractional", value: "2026-08-29T14:44:00.123456789Z"},
		{name: "offset", value: "2026-08-29T14:44:00-07:00"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act / Assert
			if !TimestampPattern.MatchString(tc.value) {
				t.Fatalf("TimestampPattern rejected %q, want it accepted", tc.value)
			}
		})
	}
}

func TestTimestampPatternRejectsOtherShapes(t *testing.T) {
	tests := []struct {
		name  string
		value string
	}{
		{name: "empty", value: ""},
		{name: "epoch millis", value: "1756480000000"},
		{name: "date only", value: "2026-08-29"},
		{name: "space separated", value: "2026-08-29 14:44:00Z"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act / Assert
			if TimestampPattern.MatchString(tc.value) {
				t.Fatalf("TimestampPattern accepted %q, want it rejected", tc.value)
			}
		})
	}
}

func TestWorkspaceLogPathIsTheContractedSymlink(t *testing.T) {
	// Arrange / Act
	got := WorkspaceLogPath("/w/one", "daemon")

	// Assert
	want := filepath.Join("/w/one", ".claude", "emacs", "daemon.log")
	if got != want {
		t.Fatalf("WorkspaceLogPath = %q, want %q", got, want)
	}
}

// declared licenses exactly the named operations, as ExpectWarnings would.
func declared(operations ...string) func(LogRecord) bool {
	return func(r LogRecord) bool { return slices.Contains(operations, r.Operation) }
}

func TestUnexpectedWarningsWithNothingDeclaredFlagsEveryWarningRecord(t *testing.T) {
	// Arrange: the set StartDaemon arms every daemon with.
	records := []LogRecord{
		{Level: "warn", Operation: "daemon.shimclient.redial"},
		{Level: "error", Operation: "daemon.workspace.open"},
	}

	// Act
	got := unexpectedWarnings(records, declared())

	// Assert
	if len(got) != 2 {
		t.Fatalf("unexpectedWarnings with nothing declared = %d records, want 2", len(got))
	}
}

func TestUnexpectedWarningsSkipsADeclaredOperation(t *testing.T) {
	// Arrange
	records := []LogRecord{
		{Level: "warn", Operation: "daemon.shimclient.redial"},
		{Level: "error", Operation: "daemon.workspace.open"},
	}

	// Act
	got := unexpectedWarnings(records, declared("daemon.shimclient.redial"))

	// Assert
	if len(got) != 1 || got[0].Operation != "daemon.workspace.open" {
		t.Fatalf("unexpectedWarnings with one declared = %v, want only daemon.workspace.open", got)
	}
}

func TestUnexpectedWarningsIgnoresRecordsBelowAWarningLevel(t *testing.T) {
	// Arrange
	records := []LogRecord{
		{Level: "debug", Operation: "daemon.merge.answer_queue_offer"},
		{Level: "info", Operation: "daemon.rollout.relaunch"},
	}

	// Act
	got := unexpectedWarnings(records, declared())

	// Assert
	if len(got) != 0 {
		t.Fatalf("unexpectedWarnings over debug and info records = %v, want none", got)
	}
}

// adWorkspaceWithTargets lays out the on-disk shape internal/dlog/sink.go
// produces: one uniquely named target per daemon runtime under the state
// root's logs directory, and the workspace's canonical symlink pointing at
// the LAST runtime's target.
func adWorkspaceWithTargets(t *testing.T, lines ...[]string) string {
	t.Helper()
	root := t.TempDir()
	logsDir := filepath.Join(root, "logs")
	if err := os.MkdirAll(logsDir, 0o755); err != nil {
		t.Fatalf("arrange the logs directory: %v", err)
	}
	linkDir := filepath.Join(root, "ws", ".claude", "emacs")
	if err := os.MkdirAll(linkDir, 0o755); err != nil {
		t.Fatalf("arrange the link directory: %v", err)
	}
	var last string
	for i, runtimeLines := range lines {
		target := filepath.Join(logsDir, fmt.Sprintf("agent-repl-74f6a5cf-daemon-%d.log", 1000+i))
		if err := os.WriteFile(target, []byte(strings.Join(runtimeLines, "\n")+"\n"), 0o600); err != nil {
			t.Fatalf("arrange the target %s: %v", target, err)
		}
		if err := os.Chtimes(target, time.Unix(int64(1_700_000_000+i), 0), time.Unix(int64(1_700_000_000+i), 0)); err != nil {
			t.Fatalf("arrange the target's modtime: %v", err)
		}
		last = target
	}
	if last != "" {
		if err := os.Symlink(last, filepath.Join(linkDir, "daemon.log")); err != nil {
			t.Fatalf("arrange the canonical link: %v", err)
		}
	}
	return filepath.Join(root, "ws")
}

func adRecordLine(op string) string {
	return `{"timestamp":"2026-09-04T11:26:05Z","runtime":"daemon","level":"info","operation":"` + op + `","message":"m"}`
}

func TestWorkspaceLogTargetsSpansEveryRuntime(t *testing.T) {
	tests := []struct {
		name    string
		runtime [][]string
		want    int
	}{
		{name: "no sink at all", runtime: nil, want: 0},
		{name: "one runtime", runtime: [][]string{{adRecordLine("op.a")}}, want: 1},
		{name: "a crash and a cold boot", runtime: [][]string{{adRecordLine("op.a")}, {adRecordLine("op.a")}}, want: 2},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			ws := adWorkspaceWithTargets(t, tc.runtime...)

			// Act
			got := WorkspaceLogTargets(t, ws, "daemon")

			// Assert
			if len(got) != tc.want {
				t.Fatalf("WorkspaceLogTargets = %v (%d targets), want %d", got, len(got), tc.want)
			}
		})
	}
}

func TestWorkspaceLogTargetsOrdersOldestRuntimeFirst(t *testing.T) {
	// Arrange
	ws := adWorkspaceWithTargets(t, []string{adRecordLine("op.first")}, []string{adRecordLine("op.second")})

	// Act
	got := ReadCumulativeWorkspaceLog(t, ws, "daemon")

	// Assert
	if len(got) != 2 || got[0].Operation != "op.first" || got[1].Operation != "op.second" {
		t.Fatalf("ReadCumulativeWorkspaceLog = %v, want the older runtime's record first", got)
	}
}

func TestReadCumulativeWorkspaceLogCountsAcrossACrashAndColdBoot(t *testing.T) {
	// Arrange: the incumbent wrote one record under the operation, the
	// successor two, and the link names only the successor's target.
	ws := adWorkspaceWithTargets(t,
		[]string{adRecordLine("daemon.sessionwatcher.watch_session")},
		[]string{adRecordLine("daemon.sessionwatcher.watch_session"), adRecordLine("daemon.sessionwatcher.watch_session")},
	)

	// Act
	seen := 0
	for _, r := range ReadCumulativeWorkspaceLog(t, ws, "daemon") {
		if r.Operation == "daemon.sessionwatcher.watch_session" {
			seen++
		}
	}

	// Assert
	if seen != 3 {
		t.Fatalf("cumulative count = %d, want 3 (the link alone answers only the successor's 2)", seen)
	}
}

func TestReadLogOfTheLinkAloneMissesThePriorRuntime(t *testing.T) {
	// Arrange
	ws := adWorkspaceWithTargets(t,
		[]string{adRecordLine("daemon.sessionwatcher.watch_session")},
		[]string{adRecordLine("daemon.sessionwatcher.watch_session")},
	)

	// Act
	got := ReadLog(t, WorkspaceLogPath(ws, "daemon"))

	// Assert: the link is re-pointed per runtime, so it is NOT cumulative.
	if len(got) != 1 {
		t.Fatalf("ReadLog of the canonical link = %d records, want 1 (the current runtime's only)", len(got))
	}
}

// TestReadLogSkipsARecordStillBeingWritten is the read race every caller here
// runs into: they poll a LIVE daemon's own sink, so a read can land inside one
// `write(2)` and see the record so far. `TestMcpServerHealths` died 50ms into
// its run on `"message":"no intent manif`.
func TestReadLogSkipsARecordStillBeingWritten(t *testing.T) {
	// Arrange: one whole record, then the beginning of the next.
	path := filepath.Join(t.TempDir(), "daemon.run.log")
	body := adRecordLine("daemon.boot.run") + "\n" + `{"timestamp":"2026-08-29T14:44:00Z","operation":"daemon.rollout.reconcile","message":"no intent manif`
	if err := os.WriteFile(path, []byte(body), 0o600); err != nil {
		t.Fatalf("write the log: %v", err)
	}

	// Act
	got := ReadLog(t, path)

	// Assert
	if len(got) != 1 || got[0].Operation != "daemon.boot.run" {
		t.Fatalf("ReadLog = %v, want only the record whose newline had arrived", got)
	}
}

// The tolerance is for the UNTERMINATED tail alone: a line that has its
// newline and does not parse is still a defect in the log, and must still fail.
func TestReadLogStillFailsOnATerminatedLineThatDoesNotParse(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "daemon.run.log")
	if err := os.WriteFile(path, []byte("not json at all\n"), 0o600); err != nil {
		t.Fatalf("write the log: %v", err)
	}

	// Act
	fake := &testing.T{}
	done := make(chan struct{})
	go func() {
		defer close(done)
		defer func() { _ = recover() }()
		ReadLog(fake, path)
	}()
	<-done

	// Assert
	if !fake.Failed() {
		t.Fatal("ReadLog accepted a newline-terminated line that is not JSON; a malformed record must still fail the test")
	}
}

// appendLog appends text to the log at path.
func appendLog(t *testing.T, path, text string) {
	t.Helper()
	f, err := os.OpenFile(path, os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0o600)
	if err != nil {
		t.Fatalf("open the log: %v", err)
	}
	defer f.Close()
	if _, err := f.WriteString(text); err != nil {
		t.Fatalf("append to the log: %v", err)
	}
}

func operations(records []LogRecord) []string {
	var out []string
	for _, r := range records {
		out = append(out, r.Operation)
	}
	return out
}

// TestReadLogReadsWhatWasAppendedSinceItsLastRead: the decoded records are
// kept, and a later read adds the complete lines appended after them --
// including a record whose newline arrived only after the first read.
func TestReadLogReadsWhatWasAppendedSinceItsLastRead(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "daemon.run.log")
	appendLog(t, path, adRecordLine("daemon.boot.run")+"\n"+`{"operation":"daemon.boot.serv`)
	first := ReadLog(t, path)
	appendLog(t, path, `ing"}`+"\n"+adRecordLine("daemon.boot.ready")+"\n")

	// Act
	got := ReadLog(t, path)

	// Assert
	if want := []string{"daemon.boot.run"}; !slices.Equal(operations(first), want) {
		t.Fatalf("first ReadLog = %v, want %v", operations(first), want)
	}
	if want := []string{"daemon.boot.run", "daemon.boot.serving", "daemon.boot.ready"}; !slices.Equal(operations(got), want) {
		t.Fatalf("second ReadLog = %v, want %v", operations(got), want)
	}
}

// TestReadLogReadsAReplacedFileFromItsStart: a file renamed over the path is
// another file, and its records are its own.
func TestReadLogReadsAReplacedFileFromItsStart(t *testing.T) {
	// Arrange
	dir := t.TempDir()
	path := filepath.Join(dir, "daemon.run.log")
	appendLog(t, path, adRecordLine("daemon.boot.run")+"\n"+adRecordLine("daemon.boot.ready")+"\n")
	ReadLog(t, path)
	staged := filepath.Join(dir, "staged.log")
	appendLog(t, staged, adRecordLine("daemon.boot.run")+"\n"+adRecordLine("daemon.boot.ready")+"\n"+adRecordLine("daemon.cmd.exit")+"\n")
	if err := os.Rename(staged, path); err != nil {
		t.Fatalf("replace the log: %v", err)
	}

	// Act
	got := ReadLog(t, path)

	// Assert
	if want := []string{"daemon.boot.run", "daemon.boot.ready", "daemon.cmd.exit"}; !slices.Equal(operations(got), want) {
		t.Fatalf("ReadLog of the replaced file = %v, want %v", operations(got), want)
	}
}

// TestReadLogReadsATruncatedFileFromItsStart: a file now shorter than what
// was read of it is read again whole.
func TestReadLogReadsATruncatedFileFromItsStart(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "daemon.run.log")
	appendLog(t, path, adRecordLine("daemon.boot.run")+"\n"+adRecordLine("daemon.boot.ready")+"\n")
	ReadLog(t, path)
	if err := os.WriteFile(path, []byte(adRecordLine("daemon.cmd.exit")+"\n"), 0o600); err != nil {
		t.Fatalf("truncate the log: %v", err)
	}

	// Act
	got := ReadLog(t, path)

	// Assert
	if want := []string{"daemon.cmd.exit"}; !slices.Equal(operations(got), want) {
		t.Fatalf("ReadLog of the truncated file = %v, want %v", operations(got), want)
	}
}

// TestReadLogsRecordsSurviveACallersAppend: the records are shared between
// reads, so a caller appending to what it was given must not write into them.
func TestReadLogsRecordsSurviveACallersAppend(t *testing.T) {
	// Arrange
	path := filepath.Join(t.TempDir(), "daemon.run.log")
	appendLog(t, path, adRecordLine("daemon.boot.run")+"\n")
	mine := ReadLog(t, path)
	_ = append(mine, LogRecord{Operation: "a caller's own record"})
	appendLog(t, path, adRecordLine("daemon.boot.ready")+"\n")

	// Act
	got := ReadLog(t, path)

	// Assert
	if want := []string{"daemon.boot.run", "daemon.boot.ready"}; !slices.Equal(operations(got), want) {
		t.Fatalf("ReadLog after a caller's append = %v, want %v", operations(got), want)
	}
}

// TestAwaitLogRecordReturnsTheFirstRecordThePredicateAccepts covers the one
// poll loop Daemon.AwaitLogRecord and AwaitRunLogRecordFromAnyProcess share:
// it reads every process's records, and the callers decide whose count.
func TestAwaitLogRecordReturnsTheFirstRecordThePredicateAccepts(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "daemon.run.log")
	lines := `{"timestamp":"2026-09-24T18:06:04Z","level":"info","pid":1,"operation":"daemon.a","message":"the incumbent's"}
{"timestamp":"2026-09-24T18:06:05Z","level":"info","pid":2,"operation":"daemon.a","message":"the successor's"}
`
	if err := os.WriteFile(path, []byte(lines), 0o600); err != nil {
		t.Fatalf("write the log: %v", err)
	}
	wait, cancel := context.WithTimeout(context.Background(), DefaultTimeout)
	defer cancel()

	// Act.
	got := awaitLogRecord(t, wait, path, "the successor's record", func(r LogRecord) bool { return r.PID == 2 })

	// Assert.
	if got.Message != "the successor's" {
		t.Fatalf("record = %+v, want the successor's", got)
	}
}

func TestAwaitLogRecordAfterSkipsAMatchWrittenBeforeTheAnchor(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "shim.log")
	lines := `{"timestamp":"2026-09-24T18:06:04Z","level":"info","pid":1,"operation":"a","message":"closed","context":{"n":1}}
{"timestamp":"2026-09-24T18:06:05Z","level":"info","pid":1,"operation":"a","message":"anchor"}
{"timestamp":"2026-09-24T18:06:06Z","level":"info","pid":1,"operation":"a","message":"closed","context":{"n":2}}
`
	if err := os.WriteFile(path, []byte(lines), 0o600); err != nil {
		t.Fatalf("write the log: %v", err)
	}
	wait, cancel := context.WithTimeout(context.Background(), DefaultTimeout)
	defer cancel()

	// Act.
	got := awaitLogRecordAfter(t, wait, path, "the close after the anchor",
		func(r LogRecord) bool { return r.Message == "anchor" },
		func(r LogRecord) bool { return r.Message == "closed" })

	// Assert.
	if got.Context["n"] != float64(2) {
		t.Fatalf("record = %+v, want the close written after the anchor", got)
	}
}

func TestAwaitLogRecordAfterAcceptsTheAnchorItself(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "shim.log")
	lines := `{"timestamp":"2026-09-24T18:06:05Z","level":"info","pid":1,"operation":"a","message":"anchor"}
`
	if err := os.WriteFile(path, []byte(lines), 0o600); err != nil {
		t.Fatalf("write the log: %v", err)
	}
	wait, cancel := context.WithTimeout(context.Background(), DefaultTimeout)
	defer cancel()

	// Act.
	got := awaitLogRecordAfter(t, wait, path, "the anchor",
		func(r LogRecord) bool { return r.Message == "anchor" },
		func(r LogRecord) bool { return r.Message == "anchor" })

	// Assert.
	if got.Message != "anchor" {
		t.Fatalf("record = %+v, want the anchor", got)
	}
}

func TestLogTailOfAnEmptyLogSaysSo(t *testing.T) {
	// Act
	got := logTail(nil, 3)

	// Assert
	if got != "the log held no records" {
		t.Fatalf("logTail(nil) = %q", got)
	}
}

func TestLogTailKeepsOnlyTheLastRecordsOldestFirst(t *testing.T) {
	// Arrange
	records := []LogRecord{
		{Timestamp: "t1", Level: "info", Operation: "op.a", Message: "first"},
		{Timestamp: "t2", Level: "warn", Operation: "op.b", Message: "second"},
		{Timestamp: "t3", Level: "error", Operation: "op.c", Message: "third"},
	}

	// Act
	got := logTail(records, 2)

	// Assert
	want := "last 2 of 3 records at info or above (0 debug records not shown):\n  t2 warn op.b: second\n  t3 error op.c: third"
	if got != want {
		t.Fatalf("logTail = %q, want %q", got, want)
	}
}

func TestLogTailShorterThanItsBoundPrintsEveryRecord(t *testing.T) {
	// Arrange
	records := []LogRecord{{Timestamp: "t1", Level: "info", Operation: "op.a", Message: "only"}}

	// Act
	got := logTail(records, 40)

	// Assert
	if got != "last 1 of 1 records at info or above (0 debug records not shown):\n  t1 info op.a: only" {
		t.Fatalf("logTail = %q", got)
	}
}

func TestLogTailCarriesEachRecordsContext(t *testing.T) {
	// Arrange
	records := []LogRecord{{Timestamp: "t1", Level: "info", Operation: "op.a", Message: "linked", Context: map[string]any{"state": "connected"}}}

	// Act
	got := logTail(records, 40)

	// Assert
	if got != `last 1 of 1 records at info or above (0 debug records not shown):
  t1 info op.a: linked {"state":"connected"}` {
		t.Fatalf("logTail = %q", got)
	}
}

func TestLogTailPassesOverDebugRecordsAndCountsThem(t *testing.T) {
	// Arrange
	records := []LogRecord{
		{Timestamp: "t1", Level: "info", Operation: "op.a", Message: "decided"},
		{Timestamp: "t2", Level: "debug", Operation: "op.poll", Message: "read"},
		{Timestamp: "t3", Level: "debug", Operation: "op.poll", Message: "read"},
	}

	// Act
	got := logTail(records, 40)

	// Assert
	if got != "last 1 of 1 records at info or above (2 debug records not shown):\n  t1 info op.a: decided" {
		t.Fatalf("logTail = %q", got)
	}
}

func TestAnchorAccountSaysWhetherTheAnchorWasWritten(t *testing.T) {
	isAnchor := func(r LogRecord) bool { return r.Message == "anchor" }
	isAwaited := func(r LogRecord) bool { return r.Message == "awaited" }
	tests := []struct {
		name    string
		records []LogRecord
		want    string
	}{
		{"no anchor", []LogRecord{{Message: "awaited"}}, "the anchor was never written in 1 records; 1 awaited-shape records were (first at indexes [0])"},
		{"an anchor after the awaited records", []LogRecord{{Message: "awaited"}, {Message: "anchor", Operation: "op", Timestamp: "t"}},
			`the anchor is record 1 of 2 (t op "anchor"); 1 awaited-shape records, first at indexes [0]`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := anchorAccount(tt.records, isAnchor, isAwaited)

			// Assert
			if got != tt.want {
				t.Fatalf("anchorAccount = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestAwaitLogRecordWithinReturnsTheRecordThePredicateAccepts(t *testing.T) {
	// Arrange: a workspace log beside a bare Daemon carrying its run budget.
	d := &Daemon{t: t, ctx: context.Background(), StateDir: t.TempDir(), daemonProc: &daemonProc{}}
	path := filepath.Join(t.TempDir(), "webapp.log")
	lines := `{"timestamp":"2026-10-06T12:00:00Z","level":"info","pid":7,"operation":"webapp-layer.other","message":"not this"}
{"timestamp":"2026-10-06T12:00:01Z","level":"info","pid":7,"operation":"webapp-layer.handover.page-mounted","message":"mounted"}
`
	if err := os.WriteFile(path, []byte(lines), 0o600); err != nil {
		t.Fatalf("write the log: %v", err)
	}

	// Act.
	got := d.AwaitLogRecordWithin(path, "the mounted marker", DefaultTimeout,
		func(r LogRecord) bool { return r.Operation == "webapp-layer.handover.page-mounted" })

	// Assert.
	if got.Message != "mounted" {
		t.Fatalf("record = %+v, want the mounted marker", got)
	}
}

// TestEveryTurnEndedWaitNamesOpTurnEnded pins that the integration suite waits
// on a turn's durable close through OpTurnEnded alone: a test that spelled the
// operation by hand would keep passing on a stale spelling after a rename.
func TestEveryTurnEndedWaitNamesOpTurnEnded(t *testing.T) {
	// Arrange
	files, err := filepath.Glob(filepath.Join("..", "*_test.go"))
	if err != nil || len(files) == 0 {
		t.Fatalf("glob the integration tests = %v, %v, want files", files, err)
	}

	// Act
	var spelled []string
	for _, f := range files {
		body, err := os.ReadFile(f)
		if err != nil {
			t.Fatalf("read %s: %v", f, err)
		}
		if strings.Contains(string(body), fmt.Sprintf("%q", OpTurnEnded)) {
			spelled = append(spelled, f)
		}
	}

	// Assert
	if len(spelled) != 0 {
		t.Fatalf("integration tests spell %q by hand instead of harness.OpTurnEnded: %v", OpTurnEnded, spelled)
	}
}

// TestExpectWorkspaceRestartWarningsDeclaresTheWholeRestartSet covers the
// helper's one guarantee: every operation of WorkspaceRestartWarnings is
// licensed, and nothing else is.
func TestExpectWorkspaceRestartWarningsDeclaresTheWholeRestartSet(t *testing.T) {
	// Arrange
	d := &Daemon{t: t, expected: map[string]bool{}, daemonProc: &daemonProc{}}

	// Act
	d.ExpectWorkspaceRestartWarnings()

	// Assert
	if len(d.expected) != len(WorkspaceRestartWarnings) {
		t.Fatalf("expected = %v, want exactly %v", d.expected, WorkspaceRestartWarnings)
	}
	for _, op := range WorkspaceRestartWarnings {
		if !d.expected[op] {
			t.Fatalf("expected = %v, want %q declared", d.expected, op)
		}
	}
}

// TestNoIntegrationTestSpellsOutTheWorkspaceRestartWarnings covers the call
// sites: a test that declares the whole restart set by hand would drift from
// WorkspaceRestartWarnings unseen, so it must call
// ExpectWorkspaceRestartWarnings instead.
func TestNoIntegrationTestSpellsOutTheWorkspaceRestartWarnings(t *testing.T) {
	// Arrange
	files, err := filepath.Glob(filepath.Join("..", "*_test.go"))
	if err != nil || len(files) == 0 {
		t.Fatalf("glob the integration tests = (%v, %v), want files", files, err)
	}
	call := regexp.MustCompile(`(?s)ExpectWarnings(For)?\((.*?)\)\n`)

	for _, file := range files {
		raw, err := os.ReadFile(file)
		if err != nil {
			t.Fatalf("read %s: %v", file, err)
		}

		// Act
		for _, match := range call.FindAllStringSubmatch(string(raw), -1) {
			// Assert
			spelled := 0
			for _, op := range WorkspaceRestartWarnings {
				if strings.Contains(match[2], `"`+op+`"`) {
					spelled++
				}
			}
			if spelled == len(WorkspaceRestartWarnings) {
				t.Fatalf("%s declares the whole workspace-restart set by hand; call ExpectWorkspaceRestartWarnings:\n%s", file, match[0])
			}
		}
	}
}
