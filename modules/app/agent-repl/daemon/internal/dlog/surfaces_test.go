package dlog

import (
	"crypto/md5"
	"encoding/hex"
	"encoding/json"
	"errors"
	"io"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"syscall"
	"testing"
	"time"
)

// mintedIDWidth is wsm.IDLength. It is spelled out rather than imported: wsm
// imports dlog, so a test import of wsm would be a cycle.
const mintedIDWidth = 16

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

// mintedTestID stands in for the daemon-minted ids.WorkspaceID of one
// directory: 16 hex characters, as wsm mints, derived from the directory only
// so a test can predict it. Production resolves the real roster.
func mintedTestID(dir string) string {
	sum := md5.Sum([]byte("minted:" + filepath.Clean(dir)))
	return hex.EncodeToString(sum[:])[:mintedIDWidth]
}

// bindTestWorkspaceIDs installs the minted-id lookup every workspace-owned
// record needs. Unbound surfaces REFUSE, which is its own test.
func bindTestWorkspaceIDs(s *surfaces) {
	s.BindWorkspaceIDs(func(dir string) (string, error) { return mintedTestID(dir), nil })
}

// testSurfaces opens real surfaces over temp paths with a discarded terminal.
func testSurfaces(t *testing.T) (*surfaces, string) {
	t.Helper()
	runLogPath := filepath.Join(t.TempDir(), "logs", "daemon.run.log")
	s, err := openSurfaces(runLogPath, LevelDebug, io.Discard)
	if err != nil {
		t.Fatalf("openSurfaces: %v", err)
	}
	bindTestWorkspaceIDs(s)
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
	s, err := openSurfaces(filepath.Join(blocker, "daemon.run.log"), LevelDebug, io.Discard)

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
	want := mintedTestID(dir)
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

func TestWorkspaceOrCentralWritesToTheWorkspaceSinkWhenItResolves(t *testing.T) {
	// Arrange.
	s, runLogPath := testSurfaces(t)
	dir := t.TempDir()

	// Act.
	s.WorkspaceOrCentral(dir).Info("daemon.workspace.opened", "opened", nil)

	// Assert.
	if !hasOperation(workspaceRecords(t, dir, "daemon"), "daemon.workspace.opened") {
		t.Fatalf("the record did not reach the workspace's own sink")
	}
	if hasOperation(readRecords(t, runLogPath), "daemon.workspace.opened") {
		t.Fatalf("a resolvable workspace's record reached the central sink")
	}
}

func TestWorkspaceOrCentralRoutesAnUnresolvableWorkspaceCentrally(t *testing.T) {
	// Arrange: a workspace directory that does not exist.
	s, runLogPath := testSurfaces(t)
	dir := filepath.Join(t.TempDir(), "gone")

	// Act.
	s.WorkspaceOrCentral(dir).Info("daemon.workspace.opened", "opened", nil)

	// Assert.
	records := readRecords(t, runLogPath)
	if !hasOperation(records, "daemon.workspace.opened") {
		t.Fatalf("the record did not reach the central sink")
	}
}

func TestWorkspaceOrCentralNamesTheWorkspaceOnACentrallyRoutedRecord(t *testing.T) {
	// Arrange.
	s, runLogPath := testSurfaces(t)
	dir := filepath.Join(t.TempDir(), "gone")

	// Act.
	s.WorkspaceOrCentral(dir).Info("daemon.workspace.opened", "opened", nil)

	// Assert: the line still says which workspace it is about.
	for _, rec := range readRecords(t, runLogPath) {
		if rec["operation"] != "daemon.workspace.opened" {
			continue
		}
		context, _ := rec["context"].(map[string]any)
		if context[KeyUnroutableWorkspace] != dir {
			t.Fatalf("unroutable_workspace = %v, want %q", context[KeyUnroutableWorkspace], dir)
		}
		return
	}
	t.Fatalf("the centrally routed record was not written")
}

func TestWorkspaceOrCentralRecordsNoErrorForAnUnresolvableWorkspace(t *testing.T) {
	// Arrange.
	s, runLogPath := testSurfaces(t)
	dir := filepath.Join(t.TempDir(), "gone")

	// Act.
	s.WorkspaceOrCentral(dir).Info("daemon.workspace.opened", "opened", nil)

	// Assert: an unavailable directory is an ordinary outcome, not a fault.
	for _, rec := range readRecords(t, runLogPath) {
		if rec["level"] == LevelError {
			t.Fatalf("an unresolvable workspace produced an error record: %v", rec)
		}
	}
}

func TestWorkspaceOrCentralReportsTheFallbackOncePerWorkspace(t *testing.T) {
	// Arrange.
	s, runLogPath := testSurfaces(t)
	dir := filepath.Join(t.TempDir(), "gone")

	// Act: many records about the one workspace.
	for i := 0; i < 5; i++ {
		s.WorkspaceOrCentral(dir).Info("daemon.workspace.opened", "opened", nil)
	}

	// Assert.
	notices := 0
	for _, rec := range readRecords(t, runLogPath) {
		if rec["operation"] == "daemon.dlog.central_fallback" {
			notices++
		}
	}
	if notices != 1 {
		t.Fatalf("central_fallback notices = %d, want 1", notices)
	}
}

func TestWorkspaceOrCentralReportsTheFallbackAtDebug(t *testing.T) {
	// Arrange.
	s, runLogPath := testSurfaces(t)
	dir := filepath.Join(t.TempDir(), "gone")

	// Act.
	s.WorkspaceOrCentral(dir).Info("daemon.workspace.opened", "opened", nil)

	// Assert.
	for _, rec := range readRecords(t, runLogPath) {
		if rec["operation"] != "daemon.dlog.central_fallback" {
			continue
		}
		if rec["level"] != LevelDebug {
			t.Fatalf("central_fallback level = %v, want %q", rec["level"], LevelDebug)
		}
		return
	}
	t.Fatalf("the fallback was never reported")
}

func TestWorkspaceOrCentralReportsEachUnresolvableWorkspaceSeparately(t *testing.T) {
	// Arrange.
	s, runLogPath := testSurfaces(t)
	root := t.TempDir()
	first, second := filepath.Join(root, "gone-a"), filepath.Join(root, "gone-b")

	// Act.
	s.WorkspaceOrCentral(first).Info("daemon.workspace.opened", "opened", nil)
	s.WorkspaceOrCentral(second).Info("daemon.workspace.opened", "opened", nil)

	// Assert.
	notices := 0
	for _, rec := range readRecords(t, runLogPath) {
		if rec["operation"] == "daemon.dlog.central_fallback" {
			notices++
		}
	}
	if notices != 2 {
		t.Fatalf("central_fallback notices = %d, want 2", notices)
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
	if borrow.File() == nil {
		t.Fatalf("File() = nil, want the open sink handle for fd 3")
	}
	link := filepath.Join(dir, ".claude", "emacs", "shim.log")
	if _, err := os.Lstat(link); err != nil {
		t.Fatalf("lstat %s: %v", link, err)
	}
}

func TestShimSinkSurvivesAGarbageCollectionAfterTheBorrow(t *testing.T) {
	// Arrange: borrow the shim sink, then drop the borrower's own reference.
	// A borrower that wrapped the descriptor in an os.File of its own would
	// leave a second owner behind whose finalizer closes the sink's fd at an
	// arbitrary later moment; the freed fd number is then handed to unrelated
	// opens elsewhere in the process ("bad file descriptor" on a stranger).
	s, _ := testSurfaces(t)
	dir := t.TempDir()
	borrow, err := s.ShimSink(dir)
	if err != nil {
		t.Fatalf("ShimSink: %v", err)
	}
	fd := int(borrow.File().Fd())
	borrow = nil
	_ = borrow

	// Act: give any finalizer every chance to run.
	runtime.GC()
	runtime.GC()

	// Assert: the sink's descriptor is still open and writable.
	if _, err := syscall.Write(fd, []byte("")); err != nil {
		t.Fatalf("writing the shim sink after a GC = %v, want it still open", err)
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

// TestClientLogPromotesTheClientsSessionIdentity pins the join key landing 15
// exists for: the webapp states its session identity inside the record's
// context, and it must land in the persisted record's OWN field, so one grep
// on agent_repl_session_id joins the browser's records to the daemon's, the
// shim's, Emacs's and the sidecar's.
func TestClientLogPromotesTheClientsSessionIdentity(t *testing.T) {
	// Arrange.
	s, _ := testSurfaces(t)
	dir := t.TempDir()

	// Act.
	if err := s.ClientLog(dir, ClientRecord{
		ClientKind: RuntimeWebapp,
		Level:      LevelInfo,
		Operation:  "webapp.test.op",
		Message:    "forwarded",
		Context:    Context{KeyAgentReplSessionID: "sess-1"},
	}); err != nil {
		t.Fatalf("ClientLog: %v", err)
	}

	// Assert.
	records := workspaceRecords(t, dir, "webapp")
	if len(records) != 1 {
		t.Fatalf("webapp records = %d, want exactly one", len(records))
	}
	if got := records[0][KeyAgentReplSessionID]; got != "sess-1" {
		t.Fatalf("agent_repl_session_id = %v, want the client's own", got)
	}
}

// TestClientLogPromotesTheClientsVendorConversation pins the second identity
// the same record carries, for the same join.
func TestClientLogPromotesTheClientsVendorConversation(t *testing.T) {
	// Arrange.
	s, _ := testSurfaces(t)
	dir := t.TempDir()

	// Act.
	if err := s.ClientLog(dir, ClientRecord{
		ClientKind: RuntimeWebapp,
		Level:      LevelInfo,
		Operation:  "webapp.test.op",
		Message:    "forwarded",
		Context:    Context{KeyClaudeSessionID: "claude-1"},
	}); err != nil {
		t.Fatalf("ClientLog: %v", err)
	}

	// Assert.
	records := workspaceRecords(t, dir, "webapp")
	if len(records) != 1 {
		t.Fatalf("webapp records = %d, want exactly one", len(records))
	}
	if got := records[0][KeyClaudeSessionID]; got != "claude-1" {
		t.Fatalf("claude_session_id = %v, want the client's own", got)
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

func TestClientLogPersistsTheClientsVerboseClass(t *testing.T) {
	tests := []struct {
		name    string
		verbose bool
		want    string
	}{
		{name: "normal", verbose: false, want: VerbosityNormal},
		{name: "verbose", verbose: true, want: VerbosityVerbose},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			s, _ := testSurfaces(t)
			dir := t.TempDir()

			// Act.
			if err := s.ClientLog(dir, ClientRecord{
				ClientKind: RuntimeWebapp,
				Level:      LevelInfo,
				Operation:  "webapp.test.op",
				Message:    "forwarded",
				Verbose:    tc.verbose,
			}); err != nil {
				t.Fatalf("ClientLog: %v", err)
			}

			// Assert.
			records := workspaceRecords(t, dir, "webapp")
			if got := records[0]["verbosity"]; got != tc.want {
				t.Fatalf("verbosity = %v, want %q", got, tc.want)
			}
		})
	}
}

func TestClientLogFiltersBelowThresholdBeforePersistence(t *testing.T) {
	// Arrange.
	runLogPath := filepath.Join(t.TempDir(), "logs", "daemon.run.log")
	s, err := openSurfaces(runLogPath, LevelWarn, io.Discard)
	if err != nil {
		t.Fatalf("openSurfaces: %v", err)
	}
	t.Cleanup(func() { s.Close() })
	dir := t.TempDir()

	// Act.
	err = s.ClientLog(dir, ClientRecord{
		ClientKind: RuntimeWebapp,
		Level:      LevelInfo,
		Operation:  "webapp.test.filtered",
		Message:    "filtered",
	})

	// Assert.
	if err != nil {
		t.Fatalf("ClientLog: %v", err)
	}
	if records := workspaceRecords(t, dir, "webapp"); len(records) != 0 {
		t.Fatalf("webapp records = %v, want the INFO record filtered at WARN", records)
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

func TestShimSinkRotatesAMarkedTargetAtTheNextProcessRoll(t *testing.T) {
	// Arrange: fd 3 has carried the shim target past the soft cap and the scan
	// has marked it. A separate reader models the descriptor inherited by the
	// retiring child, which survives the daemon closing its own descriptor.
	s, _ := testSurfaces(t)
	dir := t.TempDir()
	if _, err := s.ShimSink(dir); err != nil {
		t.Fatalf("ShimSink: %v", err)
	}
	_, shim, err := s.resolve(dir, "shim")
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	if err := shim.file.File().Truncate(CapBytes); err != nil {
		t.Fatalf("grow: %v", err)
	}
	held, err := os.Open(shim.target)
	if err != nil {
		t.Fatalf("open the retiring shim's target: %v", err)
	}
	defer held.Close()
	oldInfo, err := held.Stat()
	if err != nil {
		t.Fatalf("stat old target: %v", err)
	}
	s.scanOnce()

	// Act: the replacement shim requests its fd 3.
	borrow, err := s.ShimSink(dir)
	if err != nil {
		t.Fatalf("ShimSink at process roll: %v", err)
	}

	// Assert.
	newInfo, err := borrow.File().Stat()
	if err != nil {
		t.Fatalf("stat fresh target: %v", err)
	}
	if os.SameFile(oldInfo, newInfo) {
		t.Fatal("the replacement shim inherited the retiring shim's inode")
	}
	if generation, err := os.Stat(shim.target + ".1"); err != nil || !os.SameFile(oldInfo, generation) {
		t.Fatalf("generation .1 = %v, %v; want the retiring shim's inode", generation, err)
	}
	if shim.rotatePending {
		t.Fatal("the shim target remains marked after its process roll")
	}
}

func TestShimHardCeilingReportsOnceAndRequestsAForcedRoll(t *testing.T) {
	// Arrange: fd 3 has carried the shim target through the hard ceiling.
	s, _ := testSurfaces(t)
	dir := t.TempDir()
	if _, err := s.ShimSink(dir); err != nil {
		t.Fatalf("ShimSink: %v", err)
	}
	_, shim, err := s.resolve(dir, "shim")
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	if err := shim.file.File().Truncate(HardCapBytes + 1); err != nil {
		t.Fatalf("grow: %v", err)
	}

	// Act: two scans observe the same over-ceiling target.
	s.scanOnce()
	s.scanOnce()

	// Assert: exactly one error and one forced-roll request are emitted, and
	// the managed write path refuses further bytes until the roll happens.
	records := workspaceRecords(t, dir, "daemon")
	seen := 0
	for _, rec := range records {
		if rec["operation"] == "daemon.dlog.shim_hard_ceiling" {
			seen++
		}
	}
	if seen != 1 {
		t.Fatalf("hard-ceiling records = %d, want exactly 1", seen)
	}
	select {
	case req := <-s.ShimRollRequests():
		if req.Dir != filepath.Clean(dir) || req.SizeBytes != HardCapBytes+1 || req.HardBytes != HardCapBytes {
			t.Fatalf("roll request = %+v, want this workspace and its ceiling", req)
		}
	default:
		t.Fatal("the hard ceiling emitted no forced-roll request")
	}
	select {
	case req := <-s.ShimRollRequests():
		t.Fatalf("the repeated scan emitted a second roll request: %+v", req)
	default:
	}
	if err := shim.write([]byte("refused\n")); err == nil {
		t.Fatal("the managed shim write path accepted bytes past the hard ceiling")
	}
}

func TestScanOnceReportsAPoisonedSinkAgainstItsWorkspace(t *testing.T) {
	// Arrange: the shim sink's descriptor fails while the cap scanner owns the
	// observation and therefore owns the error record.
	s, runLogPath := testSurfaces(t)
	dir := t.TempDir()
	if _, err := s.ShimSink(dir); err != nil {
		t.Fatalf("ShimSink: %v", err)
	}
	_, shim, err := s.resolve(dir, "shim")
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	if err := shim.file.Close(); err != nil {
		t.Fatalf("close the descriptor under the scanner: %v", err)
	}

	// Act.
	s.scanOnce()

	// Assert.
	if !hasOperation(workspaceRecords(t, dir, "daemon"), "daemon.dlog.sink_poisoned") {
		t.Fatal("the poisoned sink was not reported against its workspace")
	}
	if hasOperation(readRecords(t, runLogPath), "daemon.dlog.sink_poisoned") {
		t.Fatal("the poisoned sink was reported globally; it is workspace-attributed")
	}
}

func TestDurableWriteDoesNotWaitOnAStalledMirror(t *testing.T) {
	// Arrange: the terminal wedges inside its first write.
	terminal := newBlockingWriter()
	runLogPath := filepath.Join(t.TempDir(), "logs", "daemon.run.log")
	s, err := openSurfaces(runLogPath, LevelDebug, terminal)
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

// TestShimSinkRefusesAfterClose is the OTHER half of the closed-surface rule.
// A workspace LOGGER after Close drops its records rather than fail the caller
// (TestWorkspaceLoggingAfterCloseNeverFailsTheCaller), because logging may
// never fail an rpc. A borrowed SHIM SINK is not logging: it is the real file
// descriptor a spawned shim inherits as fd 3, and there is no such descriptor
// once the surfaces are closed, so it still refuses.
func TestShimSinkRefusesAfterClose(t *testing.T) {
	// Arrange.
	s, _ := testSurfaces(t)
	dir := t.TempDir()
	if err := s.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Act.
	_, err := s.ShimSink(dir)

	// Assert.
	if err == nil {
		t.Fatalf("ShimSink succeeded after Close")
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
	shim.rotatePending = true

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

// TestAReopenedSinkKeepsAppendingToTheEvictedTarget pins that eviction is a
// release of the HANDLE, not of the workspace's log: a record written after
// the eviction joins the ones written before it, through the same canonical
// link.
func TestAReopenedSinkKeepsAppendingToTheEvictedTarget(t *testing.T) {
	// Arrange.
	s, _ := testSurfaces(t)
	dir := t.TempDir()
	log, err := s.Workspace(dir)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	log.Info("daemon.workspace.opened", "opened", nil)
	if err := s.Evict(dir); err != nil {
		t.Fatalf("Evict: %v", err)
	}

	// Act.
	reopened, err := s.Workspace(dir)
	if err != nil {
		t.Fatalf("Workspace after Evict: %v", err)
	}
	reopened.Info("daemon.workspace.close", "closed", nil)

	// Assert.
	records := workspaceRecords(t, dir, "daemon")
	if !hasOperation(records, "daemon.workspace.opened") {
		t.Fatal("the pre-eviction record is no longer reachable through the canonical link")
	}
	if !hasOperation(records, "daemon.workspace.close") {
		t.Fatal("the post-eviction record is not reachable through the canonical link")
	}
}

// TestAWorkspaceSinkTargetLivesUnderTheStateRootsLogsDirectory pins the wiring
// end to end: the surfaces derive the logs directory from the run log's own
// path, so every per-workspace target lands beside it rather than in TMPDIR.
func TestAWorkspaceSinkTargetLivesUnderTheStateRootsLogsDirectory(t *testing.T) {
	// Arrange.
	stateRoot := t.TempDir()
	logsDir := filepath.Join(stateRoot, "logs")
	s, err := openSurfaces(filepath.Join(logsDir, "daemon.run.log"), LevelDebug, io.Discard)
	if err != nil {
		t.Fatalf("openSurfaces: %v", err)
	}
	bindTestWorkspaceIDs(s)
	t.Cleanup(func() { s.Close() })
	dir := t.TempDir()

	// Act.
	if _, err := s.Workspace(dir); err != nil {
		t.Fatalf("Workspace: %v", err)
	}

	// Assert.
	target, err := os.Readlink(filepath.Join(dir, ".claude", "emacs", "daemon.log"))
	if err != nil {
		t.Fatalf("readlink the canonical daemon.log: %v", err)
	}
	if filepath.Dir(target) != logsDir {
		t.Fatalf("daemon.log target = %q, want it under the state root's %q", target, logsDir)
	}
}

// TestWorkspaceKeepsItsSinkAfterTheDirectoryIsRemoved covers the merged
// workspace: the daemon removes a landed merge's worktree itself, and every
// later per-workspace rpc on that workspace still has to resolve the sink it
// already opened rather than fail on the directory the daemon just deleted.
func TestWorkspaceKeepsItsSinkAfterTheDirectoryIsRemoved(t *testing.T) {
	// Arrange: a workspace whose sink is already open.
	s, _ := testSurfaces(t)
	dir := filepath.Join(t.TempDir(), "worktree")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	if _, err := s.Workspace(dir); err != nil {
		t.Fatalf("Workspace: %v", err)
	}

	// Act: the worktree goes away, as a landed merge's teardown removes it.
	if err := os.RemoveAll(dir); err != nil {
		t.Fatalf("remove: %v", err)
	}
	log, err := s.Workspace(dir)

	// Assert.
	if err != nil {
		t.Fatalf("Workspace after the directory was removed = error %v, want the already-open sink", err)
	}
	log.Info("daemon.workspace.after_removal", "recorded", nil)
}

// TestWorkspaceLoggingAfterCloseNeverFailsTheCaller covers the shutdown race a
// handover exposes: the outgoing daemon closes its log surfaces while a request
// for a workspace it just transferred is still in flight. Resolving that
// workspace's sink must NOT fail -- the request has to reach its handler and
// receive the handler's own typed answer -- so a closed surface costs the
// record and nothing else.
func TestWorkspaceLoggingAfterCloseNeverFailsTheCaller(t *testing.T) {
	tests := []struct {
		name     string
		resolved bool // the workspace already had an open sink before Close
	}{
		{name: "a workspace whose sink was already open", resolved: true},
		{name: "a workspace first seen after the close", resolved: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			s, _ := testSurfaces(t)
			dir := filepath.Join(t.TempDir(), "worktree")
			if err := os.MkdirAll(dir, 0o755); err != nil {
				t.Fatalf("mkdir: %v", err)
			}
			if tc.resolved {
				if _, err := s.Workspace(dir); err != nil {
					t.Fatalf("Workspace before the close: %v", err)
				}
			}
			if err := s.Close(); err != nil {
				t.Fatalf("Close: %v", err)
			}

			// Act: the late request resolves its sink.
			log, err := s.Workspace(dir)

			// Assert: a logger, no error, and emitting through it is inert.
			if err != nil {
				t.Fatalf("Workspace after Close = error %v, want a dropping logger and no error", err)
			}
			if log == nil {
				t.Fatalf("Workspace after Close returned a nil logger")
			}
			log.Error("daemon.test.after_close", "dropped", nil)
		})
	}
}

// TestWorkspaceAfterCloseStillRejectsAnUnusableDirectory keeps the closed-surface
// fallback from swallowing a caller's own bad argument: an empty directory is
// the caller's fault and stays an error even after Close.
func TestWorkspaceAfterCloseStillRejectsAnUnusableDirectory(t *testing.T) {
	// Arrange.
	s, _ := testSurfaces(t)
	if err := s.Close(); err != nil {
		t.Fatalf("Close: %v", err)
	}

	// Act.
	_, err := s.Workspace("")

	// Assert.
	if err == nil {
		t.Fatalf("Workspace(\"\") after Close = nil error, want the caller's own argument refused")
	}
}

// A record's workspace_id is the DAEMON-MINTED id — the same id the shim, the
// webapp and the store state — and the directory hash the kernel lock file is
// named after is separate evidence beside it.
func TestWorkspaceRecordCarriesTheMintedIDAndTheDirectoryHashSeparately(t *testing.T) {
	// Arrange.
	s, _ := testSurfaces(t)
	dir := t.TempDir()
	wantID := mintedTestID(dir)
	wantHash, err := WorkspaceDirHash(dir)
	if err != nil {
		t.Fatalf("WorkspaceDirHash: %v", err)
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
	if records[0]["workspace_id"] != wantID {
		t.Fatalf("workspace_id = %v, want the minted %q", records[0]["workspace_id"], wantID)
	}
	ctx, ok := records[0]["context"].(map[string]any)
	if !ok {
		t.Fatalf("context = %v, want an object carrying the directory hash", records[0]["context"])
	}
	if ctx[KeyWorkspaceDirHash] != wantHash {
		t.Fatalf("context.%s = %v, want %q", KeyWorkspaceDirHash, ctx[KeyWorkspaceDirHash], wantHash)
	}
}

// Unbound, the surfaces REFUSE a workspace sink rather than attribute the
// record to anything derived from the path.
func TestWorkspaceRefusesWhenNoMintedIDLookupIsBound(t *testing.T) {
	// Arrange: surfaces with no lookup bound.
	runLogPath := filepath.Join(t.TempDir(), "logs", "daemon.run.log")
	s, err := openSurfaces(runLogPath, LevelDebug, io.Discard)
	if err != nil {
		t.Fatalf("openSurfaces: %v", err)
	}
	t.Cleanup(func() { s.Close() })

	// Act.
	_, err = s.Workspace(t.TempDir())

	// Assert.
	if err == nil {
		t.Fatalf("Workspace succeeded with no id lookup bound; it must refuse")
	}
	if !strings.Contains(err.Error(), "no workspace id lookup is bound") {
		t.Fatalf("error = %v, want the unbound-lookup refusal", err)
	}
}

func TestWorkspaceRefusesWhenTheLookupCannotNameTheWorkspace(t *testing.T) {
	tests := []struct {
		name   string
		lookup WorkspaceIDLookup
		want   string
	}{
		{
			name:   "the lookup failed",
			lookup: func(string) (string, error) { return "", errors.New("roster is closed") },
			want:   "roster is closed",
		},
		{
			name:   "the lookup knows no such workspace",
			lookup: func(string) (string, error) { return "", nil },
			want:   "named no workspace",
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			s, _ := testSurfaces(t)
			s.BindWorkspaceIDs(tc.lookup)

			// Act.
			_, err := s.Workspace(t.TempDir())

			// Assert.
			if err == nil {
				t.Fatalf("Workspace succeeded; an unresolved workspace must be refused")
			}
			if !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("error = %v, want it to carry %q", err, tc.want)
			}
		})
	}
}

// The id scheme every sink name carries is stated in the run log, so a reader
// meeting an older directory-hash-named target beside a minted-id one can
// tell from the log which scheme minted which.
func TestSinkOpenRecordsTheIDScheme(t *testing.T) {
	// Arrange.
	s, runLogPath := testSurfaces(t)
	dir := t.TempDir()

	// Act.
	if _, err := s.Workspace(dir); err != nil {
		t.Fatalf("Workspace: %v", err)
	}

	// Assert.
	var opened map[string]any
	for _, rec := range readRecords(t, runLogPath) {
		if rec["operation"] == "daemon.dlog.sink_opened" {
			opened = rec
		}
	}
	if opened == nil {
		t.Fatalf("the run log carries no daemon.dlog.sink_opened record")
	}
	if opened["workspace_id"] != mintedTestID(dir) {
		t.Fatalf("workspace_id = %v, want the minted %q", opened["workspace_id"], mintedTestID(dir))
	}
	ctx, ok := opened["context"].(map[string]any)
	if !ok {
		t.Fatalf("context = %v, want an object", opened["context"])
	}
	if ctx["id_scheme"] != "daemon_minted_workspace_id" {
		t.Fatalf("context.id_scheme = %v, want daemon_minted_workspace_id", ctx["id_scheme"])
	}
}

// A forwarded client record is attributed with the same minted id.
func TestClientLogStampsTheMintedID(t *testing.T) {
	// Arrange.
	s, _ := testSurfaces(t)
	dir := t.TempDir()

	// Act.
	if err := s.ClientLog(dir, ClientRecord{
		ClientKind: RuntimeWebapp,
		Level:      LevelInfo,
		Operation:  "webapp.test.record",
		Message:    "m",
	}); err != nil {
		t.Fatalf("ClientLog: %v", err)
	}

	// Assert.
	records := workspaceRecords(t, dir, "webapp")
	if len(records) != 1 {
		t.Fatalf("records = %d, want 1", len(records))
	}
	if records[0]["workspace_id"] != mintedTestID(dir) {
		t.Fatalf("workspace_id = %v, want the minted %q", records[0]["workspace_id"], mintedTestID(dir))
	}
}

// TestARetainedWorkspaceLoggerReopensTheSinkAfterEvict pins the fix for the
// realtest-9 sink_failure flood: a component that took its workspace logger
// once and outlives the workspace's close still writes into that workspace's
// own log, because the logger resolves its sink at write time.
func TestARetainedWorkspaceLoggerReopensTheSinkAfterEvict(t *testing.T) {
	// Arrange: one logger, taken before the eviction and held across it.
	s, _ := testSurfaces(t)
	dir := t.TempDir()
	log, err := s.Workspace(dir)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	log.Info("daemon.workspace.opened", "opened", nil)
	link := filepath.Join(dir, ".claude", "emacs", "daemon.log")
	before, err := os.Readlink(link)
	if err != nil {
		t.Fatalf("readlink: %v", err)
	}
	if err := s.Evict(dir); err != nil {
		t.Fatalf("Evict: %v", err)
	}

	// Act: the retained logger writes after the eviction.
	log.Info("daemon.shimclient.kill", "the workspace's session was stopped", nil)

	// Assert: the same target, carrying both records.
	after, err := os.Readlink(link)
	if err != nil {
		t.Fatalf("readlink after eviction: %v", err)
	}
	if after != before {
		t.Fatalf("target = %q, want the remembered %q", after, before)
	}
	records := workspaceRecords(t, dir, "daemon")
	if !hasOperation(records, "daemon.workspace.opened") {
		t.Fatal("the pre-eviction record is gone from the workspace's log")
	}
	if !hasOperation(records, "daemon.shimclient.kill") {
		t.Fatal("the post-eviction record did not append to the workspace's log")
	}
}

// TestARetainedWorkspaceLoggerLandsCentrallyWhenTheDirectoryIsGone covers the
// nuke: the workspace directory the logger names no longer exists, so its sink
// cannot be re-opened. That is the ordinary outcome WorkspaceOrCentral
// documents -- the record goes to the central sink and the condition is
// reported once, at debug -- and never an error.
func TestARetainedWorkspaceLoggerLandsCentrallyWhenTheDirectoryIsGone(t *testing.T) {
	// Arrange.
	s, runLogPath := testSurfaces(t)
	dir := filepath.Join(t.TempDir(), "workspace")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	log, err := s.Workspace(dir)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if err := s.Evict(dir); err != nil {
		t.Fatalf("Evict: %v", err)
	}
	if err := os.RemoveAll(dir); err != nil {
		t.Fatalf("nuke the workspace directory: %v", err)
	}

	// Act.
	log.Info("daemon.shimclient.kill", "the workspace's session was stopped", nil)

	// Assert.
	records := readRecords(t, runLogPath)
	if !hasOperation(records, "daemon.shimclient.kill") {
		t.Fatal("the record of a nuked workspace did not land in the central sink")
	}
	fallbacks := 0
	for _, rec := range records {
		if rec["level"] == LevelError {
			t.Fatalf("a record about a nuked workspace produced an error: %v", rec)
		}
		if rec["operation"] == "daemon.dlog.central_fallback" {
			fallbacks++
			if rec["level"] != LevelDebug {
				t.Fatalf("central_fallback level = %v, want %q", rec["level"], LevelDebug)
			}
		}
	}
	if fallbacks != 1 {
		t.Fatalf("central_fallback notices = %d, want 1", fallbacks)
	}
}

// detachFixture is a workspace whose daemon sink is already open, then detached
// and removed — the state a landed merge's teardown leaves behind.
func detachFixture(t *testing.T) (*surfaces, string) {
	t.Helper()
	s, _ := testSurfaces(t)
	dir := filepath.Join(t.TempDir(), "worktree")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	if _, err := s.Workspace(dir); err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if err := s.DetachDir(dir); err != nil {
		t.Fatalf("DetachDir: %v", err)
	}
	if err := os.RemoveAll(dir); err != nil {
		t.Fatalf("remove: %v", err)
	}
	return s, dir
}

// sidecarRecord is the late forwarded record a merged workspace's sidecar sends.
func sidecarRecord() ClientRecord {
	return ClientRecord{ClientKind: RuntimeSidecar, Level: LevelInfo, Operation: "tail-pickup", Message: "picked up"}
}

// TestALateClientLogNeverRecreatesADetachedDirectory is the forced interleaving
// TestHandoverTransfersAtFreeness lost at random: a record opens a NEW sink of a
// workspace whose worktree the daemon has just removed.
func TestALateClientLogNeverRecreatesADetachedDirectory(t *testing.T) {
	// Arrange.
	s, dir := detachFixture(t)

	// Act.
	err := s.ClientLog(dir, sidecarRecord())

	// Assert.
	if err != nil {
		t.Fatalf("ClientLog on a detached workspace = %v, want the record persisted", err)
	}
	if _, statErr := os.Stat(dir); !os.IsNotExist(statErr) {
		t.Fatalf("the removed worktree %s exists again after a late record (stat = %v)", dir, statErr)
	}
}

// TestALateClientLogOnADetachedDirectoryLandsInItsTarget: the record is not lost,
// it is written to the workspace's own daemon-owned target under logsDir.
func TestALateClientLogOnADetachedDirectoryLandsInItsTarget(t *testing.T) {
	// Arrange.
	s, dir := detachFixture(t)

	// Act.
	if err := s.ClientLog(dir, sidecarRecord()); err != nil {
		t.Fatalf("ClientLog: %v", err)
	}

	// Assert.
	target := s.targets[mintedTestID(dir)+"/sidecar"]
	if target == "" {
		t.Fatal("no sidecar target was minted for the detached workspace")
	}
	if filepath.Dir(target) != s.logsDir {
		t.Fatalf("the target %s is not under the logs directory %s", target, s.logsDir)
	}
	if !hasOperation(readRecords(t, target), "tail-pickup") {
		t.Fatal("the late record is not in the workspace's sidecar target")
	}
}

// TestADetachedDirectoryIsNotCreatedWhileItStillExists: the mark holds from the
// moment DetachDir returns, before the removal has even begun.
func TestADetachedDirectoryIsNotCreatedWhileItStillExists(t *testing.T) {
	// Arrange.
	s, _ := testSurfaces(t)
	dir := filepath.Join(t.TempDir(), "worktree")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	if err := s.DetachDir(dir); err != nil {
		t.Fatalf("DetachDir: %v", err)
	}

	// Act.
	if err := s.ClientLog(dir, sidecarRecord()); err != nil {
		t.Fatalf("ClientLog: %v", err)
	}

	// Assert.
	if _, err := os.Lstat(filepath.Join(dir, ".claude")); !os.IsNotExist(err) {
		t.Fatalf("a sink of a detached directory created %s/.claude (lstat = %v)", dir, err)
	}
}

// TestAnOpenSinkRollsWithoutItsLinkOnceDetached: a detached sink that reaches
// its cap rotates its target and neither verifies nor re-creates the link.
func TestAnOpenSinkRollsWithoutItsLinkOnceDetached(t *testing.T) {
	// Arrange.
	dir := filepath.Join(t.TempDir(), "worktree")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	sk, err := openSinkSized(t.TempDir(), dir, mintedTestID(dir), "daemon", "", true, 8, 2)
	if err != nil {
		t.Fatalf("openSinkSized: %v", err)
	}
	t.Cleanup(func() { sk.close() })
	if err := sk.write([]byte("0123456\n")); err != nil {
		t.Fatalf("first write: %v", err)
	}
	sk.detach()
	if err := os.RemoveAll(dir); err != nil {
		t.Fatalf("remove: %v", err)
	}

	// Act: this write crosses the cap and rolls.
	err = sk.write([]byte("89abcde\n"))

	// Assert.
	if err != nil {
		t.Fatalf("a rolling write on a detached sink = %v, want it accepted", err)
	}
	if _, statErr := os.Stat(dir); !os.IsNotExist(statErr) {
		t.Fatalf("the roll re-created the removed directory (stat = %v)", statErr)
	}
}

// TestAttachDirRestoresTheCanonicalLink: a worktree created again at a detached
// path is a new workspace, and its sinks link into it.
func TestAttachDirRestoresTheCanonicalLink(t *testing.T) {
	// Arrange.
	s, dir := detachFixture(t)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}

	// Act.
	if err := s.AttachDir(dir); err != nil {
		t.Fatalf("AttachDir: %v", err)
	}
	if err := s.ClientLog(dir, sidecarRecord()); err != nil {
		t.Fatalf("ClientLog: %v", err)
	}

	// Assert.
	if !hasOperation(workspaceRecords(t, dir, "sidecar"), "tail-pickup") {
		t.Fatal("the re-attached workspace's sidecar.log link does not carry the record")
	}
}

// TestAttachingADirectoryNeverDetachedChangesNothing: AttachDir runs after every
// worktree creation, and most paths were never detached.
func TestAttachingADirectoryNeverDetachedChangesNothing(t *testing.T) {
	// Arrange.
	s, _ := testSurfaces(t)
	dir := t.TempDir()
	if _, err := s.Workspace(dir); err != nil {
		t.Fatalf("Workspace: %v", err)
	}

	// Act.
	err := s.AttachDir(dir)

	// Assert.
	if err != nil {
		t.Fatalf("AttachDir = %v, want success", err)
	}
	if _, open := s.workspaces[dir]; !open {
		t.Fatal("attaching a never-detached directory dropped its open sinks")
	}
}

// TestDetachDirRefusesAnEmptyDirectory: an empty path names no workspace.
func TestDetachDirRefusesAnEmptyDirectory(t *testing.T) {
	// Arrange.
	s, _ := testSurfaces(t)

	// Act.
	err := s.DetachDir("")

	// Assert.
	if err == nil {
		t.Fatal("DetachDir(\"\") = nil, want a refusal")
	}
}

// TestDetachDirIsRecordedAtDebug: the detachment is the daemon's own teardown
// step, stated where someone reading the run log will look for it.
func TestDetachDirIsRecordedAtDebug(t *testing.T) {
	// Arrange.
	s, runLog := testSurfaces(t)
	dir := t.TempDir()

	// Act.
	if err := s.DetachDir(dir); err != nil {
		t.Fatalf("DetachDir: %v", err)
	}

	// Assert.
	for _, rec := range readRecords(t, runLog) {
		if rec["operation"] == "daemon.dlog.dir_detached" {
			if rec["level"] != LevelDebug {
				t.Fatalf("dir_detached level = %v, want debug", rec["level"])
			}
			return
		}
	}
	t.Fatal("the run log carries no daemon.dlog.dir_detached record")
}

// TestNewWorkspaceEntryRecordsTheMintedEntry pins the one entry constructor.
func TestNewWorkspaceEntryRecordsTheMintedEntry(t *testing.T) {
	// Arrange.
	s, _ := testSurfaces(t)
	dir := filepath.Clean(t.TempDir())

	// Act.
	s.mu.Lock()
	ws, err := s.newWorkspaceEntryLocked(dir, true)
	s.mu.Unlock()

	// Assert.
	if err != nil {
		t.Fatalf("newWorkspaceEntryLocked: %v", err)
	}
	if ws.id != mintedTestID(dir) || !ws.detached || s.workspaces[dir] != ws {
		t.Fatalf("entry = %+v, want the minted, detached entry recorded under its directory", ws)
	}
}

// TestEveryWorkspaceEntryIsBuiltByTheOneConstructor fails a hand-built entry.
func TestEveryWorkspaceEntryIsBuiltByTheOneConstructor(t *testing.T) {
	// Arrange.
	raw, err := os.ReadFile("surfaces.go")
	if err != nil {
		t.Fatalf("read surfaces.go: %v", err)
	}

	// Act.
	built := strings.Count(string(raw), "&workspaceSinks{")

	// Assert.
	if built != 1 {
		t.Fatalf("surfaces.go builds a workspace entry %d times, want once, in newWorkspaceEntryLocked", built)
	}
}

// TestADetachmentSurvivesTheWorkspacesEviction: a merged workspace is closed
// (evicted) after its worktree is removed, and a record still in flight for it
// must not re-create the tree. The retirement this replaced ended at Evict.
func TestADetachmentSurvivesTheWorkspacesEviction(t *testing.T) {
	// Arrange.
	s, dir := detachFixture(t)
	if err := s.Evict(dir); err != nil {
		t.Fatalf("Evict: %v", err)
	}

	// Act.
	err := s.ClientLog(dir, sidecarRecord())

	// Assert.
	if err != nil {
		t.Fatalf("ClientLog after eviction = %v, want the record persisted", err)
	}
	if _, statErr := os.Stat(dir); !os.IsNotExist(statErr) {
		t.Fatalf("an evicted, detached worktree %s exists again (stat = %v)", dir, statErr)
	}
}
