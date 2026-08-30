package dlog

import (
	"encoding/json"
	"errors"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// readRecords parses a JSONL file into records. A path that does not exist is
// no records, which is what "nothing was written here" looks like.
func readRecords(t *testing.T, path string) []map[string]any {
	t.Helper()
	raw, err := os.ReadFile(path)
	if err != nil {
		if os.IsNotExist(err) {
			return nil
		}
		t.Fatalf("read %s: %v", path, err)
	}
	var out []map[string]any
	for _, line := range strings.Split(strings.TrimSpace(string(raw)), "\n") {
		if line == "" {
			continue
		}
		var rec map[string]any
		if err := json.Unmarshal([]byte(line), &rec); err != nil {
			t.Fatalf("parse %q from %s: %v", line, path, err)
		}
		out = append(out, rec)
	}
	return out
}

// workspaceRecords reads one workspace sink through its canonical link.
func workspaceRecords(t *testing.T, dir, name string) []map[string]any {
	t.Helper()
	return readRecords(t, filepath.Join(dir, ".claude", "emacs", name+".log"))
}

// hasOperation reports whether any record carries the operation.
func hasOperation(records []map[string]any, operation string) bool {
	for _, rec := range records {
		if rec["operation"] == operation {
			return true
		}
	}
	return false
}

// testSurfaces opens real surfaces over temp paths with a discarded terminal.
func testSurfaces(t *testing.T) (*surfaces, string) {
	t.Helper()
	runLogPath := filepath.Join(t.TempDir(), "logs", "daemon.run.log")
	s, err := openSurfaces(runLogPath, true, io.Discard)
	if err != nil {
		t.Fatalf("openSurfaces: %v", err)
	}
	t.Cleanup(func() { s.Close() })
	return s, runLogPath
}

func TestOpenSurfacesFailsWhenTheRunLogCannotBeOpened(t *testing.T) {
	// Arrange: a regular file where the logs directory must be.
	root := t.TempDir()
	blocker := filepath.Join(root, "logs")
	if err := os.WriteFile(blocker, nil, 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}

	// Act.
	s, err := openSurfaces(filepath.Join(blocker, "daemon.run.log"), false, io.Discard)

	// Assert: the caller must treat this as a boot fatal, so it must be an
	// error and not a degraded surface.
	if err == nil {
		s.Close()
		t.Fatalf("openSurfaces succeeded; an unopenable run log is a boot fatal")
	}
}

func TestGlobalRecordLandsInTheRunLog(t *testing.T) {
	// Arrange.
	s, runLogPath := testSurfaces(t)

	// Act.
	s.Global().Info("daemon.boot.started", "the daemon is starting", Context{"pid": 1})

	// Assert.
	if !hasOperation(readRecords(t, runLogPath), "daemon.boot.started") {
		t.Fatalf("the run log does not carry the global record")
	}
}

func TestWorkspaceRecordLandsInTheWorkspaceSink(t *testing.T) {
	// Arrange.
	s, _ := testSurfaces(t)
	dir := t.TempDir()
	log, err := s.Workspace(dir)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}

	// Act.
	log.Info("daemon.workspace.opened", "opened", nil)

	// Assert.
	if !hasOperation(workspaceRecords(t, dir, "daemon"), "daemon.workspace.opened") {
		t.Fatalf("the workspace sink does not carry the record")
	}
}

func TestWorkspaceRecordNeverLandsInTheGlobalSink(t *testing.T) {
	// Arrange.
	s, runLogPath := testSurfaces(t)
	dir := t.TempDir()
	log, err := s.Workspace(dir)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}

	// Act.
	log.Info("daemon.workspace.opened", "opened", nil)

	// Assert.
	if hasOperation(readRecords(t, runLogPath), "daemon.workspace.opened") {
		t.Fatalf("a workspace record reached the global sink")
	}
}

func TestWorkspaceStampsTheIdentityFields(t *testing.T) {
	// Arrange.
	s, _ := testSurfaces(t)
	dir := t.TempDir()
	want, err := LogWorkspaceID(dir)
	if err != nil {
		t.Fatalf("LogWorkspaceID: %v", err)
	}
	log, err := s.Workspace(dir)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}

	// Act.
	log.Info("daemon.workspace.opened", "opened", nil)

	// Assert.
	records := workspaceRecords(t, dir, "daemon")
	if len(records) != 1 {
		t.Fatalf("records = %d, want 1", len(records))
	}
	if records[0]["workspace_id"] != want {
		t.Fatalf("workspace_id = %v, want %q", records[0]["workspace_id"], want)
	}
	if records[0]["workspace_dir"] != filepath.Clean(dir) {
		t.Fatalf("workspace_dir = %v, want %q", records[0]["workspace_dir"], filepath.Clean(dir))
	}
}

func TestWorkspaceRefusesAnUnresolvableWorkspace(t *testing.T) {
	tests := []struct {
		name string
		dir  func(t *testing.T) string
	}{
		{name: "empty", dir: func(*testing.T) string { return "" }},
		{name: "absent", dir: func(t *testing.T) string { return filepath.Join(t.TempDir(), "gone") }},
		{name: "a regular file", dir: func(t *testing.T) string {
			p := filepath.Join(t.TempDir(), "file")
			if err := os.WriteFile(p, nil, 0o644); err != nil {
				t.Fatalf("write: %v", err)
			}
			return p
		}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			s, runLogPath := testSurfaces(t)

			// Act.
			log, err := s.Workspace(tc.dir(t))

			// Assert: a routing invariant violation, never a global write.
			if err == nil {
				t.Fatalf("Workspace(%v) succeeded, returning %v", tc.name, log)
			}
			if len(readRecords(t, runLogPath)) != 0 {
				t.Fatalf("the unresolvable workspace produced a global record")
			}
		})
	}
}

func TestShimSinkBorrowsTheOpenShimLog(t *testing.T) {
	// Arrange.
	s, _ := testSurfaces(t)
	dir := t.TempDir()

	// Act.
	borrow, err := s.ShimSink(dir)
	if err != nil {
		t.Fatalf("ShimSink: %v", err)
	}

	// Assert: the descriptor is real and the canonical link exists.
	if borrow.File() == 0 {
		t.Fatalf("File() = 0, want the open descriptor for fd 3")
	}
	link := filepath.Join(dir, ".claude", "emacs", "shim.log")
	if _, err := os.Lstat(link); err != nil {
		t.Fatalf("lstat %s: %v", link, err)
	}
}

func TestClientLogLandsInTheClientsOwnSink(t *testing.T) {
	tests := []struct {
		name string
		kind string
		sink string
	}{
		{name: "webapp", kind: RuntimeWebapp, sink: "webapp"},
		{name: "sidecar", kind: RuntimeSidecar, sink: "sidecar"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			s, _ := testSurfaces(t)
			dir := t.TempDir()

			// Act.
			err := s.ClientLog(dir, ClientRecord{
				ClientKind: tc.kind,
				Level:      LevelInfo,
				Operation:  tc.kind + ".test.op",
				Message:    "forwarded",
			})

			// Assert.
			if err != nil {
				t.Fatalf("ClientLog: %v", err)
			}
			if !hasOperation(workspaceRecords(t, dir, tc.sink), tc.kind+".test.op") {
				t.Fatalf("%s.log does not carry the forwarded record", tc.sink)
			}
		})
	}
}

func TestClientLogConvertsAForeignTimestampToTheLocalZone(t *testing.T) {
	// Arrange: the instant from the vocab file's worked example, in UTC.
	s, _ := testSurfaces(t)
	dir := t.TempDir()
	pattern := vocabTimestampPattern(t)
	instant := "2026-07-28T16:34:56.789Z"
	want, err := time.Parse(time.RFC3339, instant)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}

	// Act.
	if err := s.ClientLog(dir, ClientRecord{
		ClientKind: RuntimeWebapp,
		Level:      LevelInfo,
		Operation:  "webapp.test.op",
		Message:    "forwarded",
		Timestamp:  instant,
	}); err != nil {
		t.Fatalf("ClientLog: %v", err)
	}

	// Assert: the same instant, rendered in the local zone in the contract's
	// representation.
	records := workspaceRecords(t, dir, "webapp")
	if len(records) != 1 {
		t.Fatalf("records = %d, want 1", len(records))
	}
	got, _ := records[0]["timestamp"].(string)
	if !pattern.MatchString(got) {
		t.Fatalf("timestamp = %q, does not match the vocab pattern", got)
	}
	if strings.HasSuffix(got, "Z") {
		t.Fatalf("timestamp = %q, want the local zone with a numeric offset", got)
	}
	parsed, err := time.Parse(time.RFC3339, got)
	if err != nil {
		t.Fatalf("parse persisted timestamp %q: %v", got, err)
	}
	if !parsed.Equal(want) {
		t.Fatalf("persisted instant = %v, want the client's %v", parsed, want)
	}
}

func TestClientLogRefusesAnUnparseableTimestamp(t *testing.T) {
	// Arrange.
	s, _ := testSurfaces(t)
	dir := t.TempDir()

	// Act.
	err := s.ClientLog(dir, ClientRecord{
		ClientKind: RuntimeWebapp,
		Level:      LevelInfo,
		Operation:  "webapp.test.op",
		Message:    "forwarded",
		Timestamp:  "last tuesday",
	})

	// Assert: surfaced, never substituted with an instant of the daemon's own.
	if err == nil {
		t.Fatalf("ClientLog accepted an unparseable timestamp")
	}
	if len(workspaceRecords(t, dir, "webapp")) != 0 {
		t.Fatalf("the record was persisted with a substituted instant")
	}
}

func TestClientLogStampsArrivalWhenTheClientSendsNoTimestamp(t *testing.T) {
	// Arrange.
	s, _ := testSurfaces(t)
	dir := t.TempDir()

	// Act.
	if err := s.ClientLog(dir, ClientRecord{
		ClientKind: RuntimeWebapp,
		Level:      LevelInfo,
		Operation:  "webapp.test.op",
		Message:    "forwarded",
	}); err != nil {
		t.Fatalf("ClientLog: %v", err)
	}

	// Assert: the record says whose clock it is rather than passing the
	// daemon's instant off as the client's.
	records := workspaceRecords(t, dir, "webapp")
	if len(records) != 1 {
		t.Fatalf("records = %d, want 1", len(records))
	}
	ctx, _ := records[0]["context"].(map[string]any)
	if ctx["timestamp_source"] != "daemon_arrival" {
		t.Fatalf("context = %v, want timestamp_source=daemon_arrival", ctx)
	}
}

func TestClientLogRefusesAKindTheDaemonDoesNotOwn(t *testing.T) {
	tests := []struct {
		name string
		kind string
	}{
		{name: "emacs owns emacs.log", kind: "emacs"},
		{name: "the shim writes shim.log itself", kind: RuntimeShim},
		{name: "unknown", kind: "something"},
		{name: "empty", kind: ""},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			s, _ := testSurfaces(t)
			dir := t.TempDir()

			// Act.
			err := s.ClientLog(dir, ClientRecord{
				ClientKind: tc.kind,
				Level:      LevelInfo,
				Operation:  "x.y",
				Message:    "m",
			})

			// Assert.
			if err == nil {
				t.Fatalf("ClientLog accepted the kind %q", tc.kind)
			}
		})
	}
}

func TestClientLogRefusesALevelOutsideTheContract(t *testing.T) {
	// Arrange.
	s, _ := testSurfaces(t)
	dir := t.TempDir()

	// Act.
	err := s.ClientLog(dir, ClientRecord{
		ClientKind: RuntimeWebapp,
		Level:      "fatal",
		Operation:  "webapp.test.op",
		Message:    "m",
	})

	// Assert.
	if err == nil {
		t.Fatalf("ClientLog accepted the level \"fatal\"")
	}
}

func TestClientLogKeepsTheClientsOwnRuntime(t *testing.T) {
	// Arrange.
	s, _ := testSurfaces(t)
	dir := t.TempDir()

	// Act.
	if err := s.ClientLog(dir, ClientRecord{
		ClientKind: RuntimeWebapp,
		Level:      LevelInfo,
		Operation:  "webapp.test.op",
		Message:    "m",
	}); err != nil {
		t.Fatalf("ClientLog: %v", err)
	}

	// Assert.
	records := workspaceRecords(t, dir, "webapp")
	if records[0]["runtime"] != RuntimeWebapp {
		t.Fatalf("runtime = %v, want %q", records[0]["runtime"], RuntimeWebapp)
	}
}

func TestEvictReleasesTheWorkspacesSinks(t *testing.T) {
	// Arrange.
	s, _ := testSurfaces(t)
	dir := t.TempDir()
	log, err := s.Workspace(dir)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	log.Info("daemon.workspace.opened", "opened", nil)

	// Act.
	if err := s.Evict(dir); err != nil {
		t.Fatalf("Evict: %v", err)
	}

	// Assert: the evicted logger refuses further records rather than writing
	// through a closed descriptor.
	s.mu.Lock()
	_, still := s.workspaces[filepath.Clean(dir)]
	s.mu.Unlock()
	if still {
		t.Fatalf("the workspace is still in the sink map after eviction")
	}
}

func TestEvictLeavesTheCanonicalLinkAndTargetOnDisk(t *testing.T) {
	// Arrange.
	s, _ := testSurfaces(t)
	dir := t.TempDir()
	log, err := s.Workspace(dir)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	log.Info("daemon.workspace.opened", "opened", nil)

	// Act.
	if err := s.Evict(dir); err != nil {
		t.Fatalf("Evict: %v", err)
	}

	// Assert: a reader keeps resolving what it was reading.
	if !hasOperation(workspaceRecords(t, dir, "daemon"), "daemon.workspace.opened") {
		t.Fatalf("the evicted workspace's records are no longer readable through the canonical link")
	}
}

func TestEvictingAnUnknownWorkspaceIsSuccess(t *testing.T) {
	// Arrange.
	s, _ := testSurfaces(t)

	// Act.
	err := s.Evict(t.TempDir())

	// Assert.
	if err != nil {
		t.Fatalf("Evict on a workspace with no open sinks = %v, want success", err)
	}
}

func TestScanOnceReportsAPoisonedSinkAgainstItsWorkspace(t *testing.T) {
	// Arrange: the shim grows shim.log past the cap and the workspace has
	// redirected the canonical link, so maintenance must refuse.
	s, runLogPath := testSurfaces(t)
	dir := t.TempDir()
	if _, err := s.Workspace(dir); err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if _, err := s.ShimSink(dir); err != nil {
		t.Fatalf("ShimSink: %v", err)
	}
	_, shim, err := s.resolve(dir, "shim")
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	if err := shim.f.Truncate(CapBytes + 1); err != nil {
		t.Fatalf("grow: %v", err)
	}
	if err := os.Remove(shim.link); err != nil {
		t.Fatalf("remove link: %v", err)
	}

	// Act.
	s.scanOnce()

	// Assert.
	if !hasOperation(workspaceRecords(t, dir, "daemon"), "daemon.dlog.sink_poisoned") {
		t.Fatalf("the poisoned sink was not reported against its workspace")
	}
	if hasOperation(readRecords(t, runLogPath), "daemon.dlog.sink_poisoned") {
		t.Fatalf("the poisoned sink was reported globally; it is workspace-attributed")
	}
}

func TestDurableWriteDoesNotWaitOnAStalledMirror(t *testing.T) {
	// Arrange: the terminal wedges inside its first write.
	terminal := newBlockingWriter()
	runLogPath := filepath.Join(t.TempDir(), "logs", "daemon.run.log")
	s, err := openSurfaces(runLogPath, true, terminal)
	if err != nil {
		t.Fatalf("openSurfaces: %v", err)
	}
	defer func() { close(terminal.release); s.Close() }()
	log := s.Global()
	log.Info("daemon.boot.first", "first", nil)
	<-terminal.entered // the mirror goroutine is now stuck

	// Act: a durable write while the terminal is still stuck.
	log.Info("daemon.boot.second", "second", nil)

	// Assert: it completed and is on disk, with the mirror still wedged.
	records := readRecords(t, runLogPath)
	if !hasOperation(records, "daemon.boot.first") || !hasOperation(records, "daemon.boot.second") {
		t.Fatalf("the durable sink is missing records a stalled terminal must not have delayed: %v", records)
	}
	select {
	case <-terminal.release:
		t.Fatalf("the test released the terminal early; the assertion proves nothing")
	default:
	}
}

func TestVerboseRecordIsPersistedButNotMirroredWhenQuiet(t *testing.T) {
	// Arrange: verbose off.
	terminal := newCollectingWriter()
	runLogPath := filepath.Join(t.TempDir(), "logs", "daemon.run.log")
	s, err := openSurfaces(runLogPath, false, terminal)
	if err != nil {
		t.Fatalf("openSurfaces: %v", err)
	}
	log := s.Global()

	// Act.
	log.Debug("daemon.boot.branch", "the ordinary path", nil)
	log.Info("daemon.boot.milestone", "a milestone", nil)
	<-terminal.written
	if err := s.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Assert: both persisted, only the normal one mirrored.
	records := readRecords(t, runLogPath)
	if !hasOperation(records, "daemon.boot.branch") {
		t.Fatalf("the verbose record was not persisted; verbosity gates only the terminal")
	}
	for _, line := range terminal.all() {
		if strings.Contains(string(line), "daemon.boot.branch") {
			t.Fatalf("the verbose record reached a quiet terminal")
		}
	}
}

func TestVerboseRecordIsMirroredWhenVerbose(t *testing.T) {
	// Arrange.
	terminal := newCollectingWriter()
	runLogPath := filepath.Join(t.TempDir(), "logs", "daemon.run.log")
	s, err := openSurfaces(runLogPath, true, terminal)
	if err != nil {
		t.Fatalf("openSurfaces: %v", err)
	}
	defer s.Close()

	// Act.
	s.Global().Debug("daemon.boot.branch", "the ordinary path", nil)
	<-terminal.written

	// Assert.
	var seen bool
	for _, line := range terminal.all() {
		if strings.Contains(string(line), "daemon.boot.branch") {
			seen = true
		}
	}
	if !seen {
		t.Fatalf("the verbose record did not reach the verbose terminal")
	}
}

func TestCloseIsIdempotent(t *testing.T) {
	// Arrange.
	s, _ := testSurfaces(t)
	if err := s.Close(); err != nil {
		t.Fatalf("first Close: %v", err)
	}

	// Act.
	err := s.Close()

	// Assert.
	if err != nil {
		t.Fatalf("second Close = %v, want success", err)
	}
}

func TestWorkspaceRefusesAfterClose(t *testing.T) {
	// Arrange.
	s, _ := testSurfaces(t)
	dir := t.TempDir()
	if err := s.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Act.
	_, err := s.Workspace(dir)

	// Assert.
	if err == nil {
		t.Fatalf("Workspace succeeded after Close")
	}
}

func TestResolveReusesTheTargetWithinTheRuntime(t *testing.T) {
	// Arrange.
	s, _ := testSurfaces(t)
	dir := t.TempDir()
	_, first, err := s.resolve(dir, "daemon")
	if err != nil {
		t.Fatalf("first resolve: %v", err)
	}

	// Act.
	_, second, err := s.resolve(dir, "daemon")
	if err != nil {
		t.Fatalf("second resolve: %v", err)
	}

	// Assert.
	if first != second {
		t.Fatalf("a second resolve made a new sink; the target is reused from memory for the runtime's life")
	}
}

func TestClientInstantRefusesGarbage(t *testing.T) {
	// Arrange, Act.
	_, _, err := clientInstant("nonsense", time.Now)

	// Assert.
	if err == nil {
		t.Fatalf("clientInstant accepted \"nonsense\"")
	}
}

func TestSinkPoisonSurfacesThroughShimSink(t *testing.T) {
	// Arrange.
	s, _ := testSurfaces(t)
	dir := t.TempDir()
	_, shim, err := s.resolve(dir, "shim")
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	if err := os.Remove(shim.link); err != nil {
		t.Fatalf("remove link: %v", err)
	}
	shim.size = CapBytes
	if err := shim.write([]byte("x\n")); err == nil {
		t.Fatalf("the sink was not poisoned")
	}

	// Act.
	_, err = s.ShimSink(dir)

	// Assert: a poisoned sink is never handed to a spawning shim.
	if err == nil {
		t.Fatalf("ShimSink handed out a poisoned sink")
	}
	if !errors.Is(err, ErrPoisoned) {
		t.Fatalf("error = %v, want ErrPoisoned", err)
	}
}
