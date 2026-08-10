package dlog

import (
	"crypto/md5"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
	"time"

	"agentrepl/logging"
)

type failingWriter struct{ err error }

func (w failingWriter) Write([]byte) (int, error) { return 0, w.err }

type shortWriter struct {
	limit int
	zero  bool
	data  strings.Builder
}

type partialErrorWriter struct {
	data  strings.Builder
	calls int
}

type targetFileStub struct {
	size        int64
	maxWrite    int
	writeErr    error
	truncateErr error
	writes      int
	data        strings.Builder
}

func (f *targetFileStub) Write(p []byte) (int, error) {
	f.writes++
	n := len(p)
	if f.maxWrite > 0 && n > f.maxWrite {
		n = f.maxWrite
	}
	if n > 0 {
		_, _ = f.data.Write(p[:n])
		f.size += int64(n)
	}
	if f.writeErr != nil {
		return n, f.writeErr
	}
	return n, nil
}

func (f *targetFileStub) Stat() (os.FileInfo, error) {
	return targetFileInfo{size: f.size}, nil
}

func (f *targetFileStub) Truncate(size int64) error {
	if f.truncateErr != nil {
		return f.truncateErr
	}
	f.size = size
	f.data.Reset()
	return nil
}

type targetFileInfo struct{ size int64 }

func (i targetFileInfo) Name() string       { return "target.log" }
func (i targetFileInfo) Size() int64        { return i.size }
func (i targetFileInfo) Mode() os.FileMode  { return 0600 }
func (i targetFileInfo) ModTime() time.Time { return time.Time{} }
func (i targetFileInfo) IsDir() bool        { return false }
func (i targetFileInfo) Sys() any           { return nil }

func (w *partialErrorWriter) Write(p []byte) (int, error) {
	w.calls++
	if w.calls == 1 {
		n := min(7, len(p))
		_, _ = w.data.Write(p[:n])
		return n, errors.New("partial disk failure")
	}
	_, _ = w.data.Write(p)
	return len(p), nil
}

func (w *shortWriter) Write(p []byte) (int, error) {
	if w.zero {
		return 0, nil
	}
	n := min(w.limit, len(p))
	_, _ = w.data.Write(p[:n])
	return n, nil
}

func event() Event {
	return Event{Runtime: RuntimeDaemon, Level: LevelInfo, Operation: "workspace.open", Message: "workspace opened", Context: map[string]any{"branch": "feature/logging"}, AgentReplSessionID: "ar-1", ClaudeSessionID: "cl-1", RequestID: "req-1"}
}

func decodeOne(t *testing.T, output string) Record {
	t.Helper()
	lines := strings.Split(strings.TrimSuffix(output, "\n"), "\n")
	if len(lines) != 1 {
		t.Fatalf("line count = %d, output=%q", len(lines), output)
	}
	var record Record
	if err := json.Unmarshal([]byte(lines[0]), &record); err != nil {
		t.Fatalf("persisted line is not JSON: %v; line=%q", err, lines[0])
	}
	return record
}

func TestEmitNormalEncodesRequiredJSONLWorkspaceRecordWithPID(t *testing.T) {
	var durable, terminal strings.Builder
	scope, err := WorkspaceScope(Workspace{Directory: "/tmp/workspace", ID: "ws-1"})
	if err != nil {
		t.Fatal(err)
	}
	if err := New(&durable, &terminal, false).EmitNormal(scope, event()); err != nil {
		t.Fatal(err)
	}
	record := decodeOne(t, durable.String())
	if record.Timestamp.IsZero() || record.Runtime != RuntimeDaemon || record.Level != LevelInfo || record.Verbosity != Normal || record.Operation != "workspace.open" || record.Message != "workspace opened" || record.PID != os.Getpid() {
		t.Fatalf("record = %#v", record)
	}
	if record.WorkspaceDirectory != "/tmp/workspace" || record.WorkspaceID != "ws-1" || record.AgentReplSessionID != "ar-1" || record.ClaudeSessionID != "cl-1" || record.RequestID != "req-1" || record.Context["branch"] != "feature/logging" {
		t.Fatalf("record attribution = %#v", record)
	}
	decodeOne(t, terminal.String())
}

func TestEmitVerboseSharesDurableSinkAndGatesTerminal(t *testing.T) {
	var durable, terminal strings.Builder
	logger := New(&durable, &terminal, false)
	if err := logger.EmitVerbose(GlobalScope(), event()); err != nil {
		t.Fatal(err)
	}
	if durable.Len() == 0 || terminal.Len() != 0 {
		t.Fatalf("durable=%q terminal=%q", durable.String(), terminal.String())
	}
	if err := New(&durable, &terminal, true).EmitVerbose(GlobalScope(), event()); err != nil {
		t.Fatal(err)
	}
	if terminal.Len() == 0 {
		t.Fatal("verbose record did not reach enabled terminal")
	}
}

func TestEmitWorkspaceVerbosePersistsWorkspaceAttribution(t *testing.T) {
	var durable, terminal strings.Builder
	workspace := Workspace{Directory: "/tmp/workspace", ID: "ws-1"}
	if err := New(&durable, &terminal, false).EmitWorkspaceVerbose(workspace, event()); err != nil {
		t.Fatal(err)
	}
	record := decodeOne(t, durable.String())
	if record.Verbosity != Verbose || record.WorkspaceID != workspace.ID || terminal.Len() != 0 {
		t.Fatalf("record=%#v terminal=%q", record, terminal.String())
	}
}

func TestMalformedContextAndScopeFailBeforePersistence(t *testing.T) {
	for _, workspace := range []Workspace{{}, {Directory: "relative", ID: "id"}, {Directory: "/tmp/ws"}} {
		if _, err := WorkspaceScope(workspace); err == nil {
			t.Fatalf("WorkspaceScope(%#v) succeeded", workspace)
		}
	}
	var durable, terminal strings.Builder
	bad := event()
	bad.Context = nil
	if err := New(&durable, &terminal, false).EmitNormal(GlobalScope(), bad); err == nil {
		t.Fatal("missing context succeeded")
	}
	if durable.Len() != 0 || terminal.Len() != 0 {
		t.Fatalf("invalid event persisted: durable=%q terminal=%q", durable.String(), terminal.String())
	}
}

func TestWorkspaceRoutingFailureDoesNotPersistGlobally(t *testing.T) {
	var durable, terminal strings.Builder
	err := New(&durable, &terminal, false).EmitWorkspaceNormal(Workspace{ID: "ws-1"}, event())
	if err == nil || !strings.Contains(err.Error(), "workspace routing failure") {
		t.Fatalf("error=%v", err)
	}
	if durable.Len() != 0 || terminal.Len() != 0 {
		t.Fatalf("routing failure persisted globally: durable=%q terminal=%q", durable.String(), terminal.String())
	}
}

func TestProcessPIDUsesForwardedOverrideOrDaemonPID(t *testing.T) {
	for _, want := range []int{os.Getpid(), 4242} {
		var durable, terminal strings.Builder
		input := event()
		if want != os.Getpid() {
			input.PID = want
		}
		if err := New(&durable, &terminal, false).EmitNormal(GlobalScope(), input); err != nil {
			t.Fatal(err)
		}
		if got := decodeOne(t, durable.String()).PID; got != want {
			t.Fatalf("PID=%d want=%d", got, want)
		}
	}
}

func TestWebappIdentityRequiresConnectionIDAndOmitsPID(t *testing.T) {
	var durable, terminal strings.Builder
	webapp := event()
	webapp.Runtime, webapp.ConnectionID = RuntimeWebapp, "conn-1"
	if err := New(&durable, &terminal, false).EmitNormal(GlobalScope(), webapp); err != nil {
		t.Fatal(err)
	}
	record := decodeOne(t, durable.String())
	if record.ConnectionID != "conn-1" || record.PID != 0 {
		t.Fatalf("record=%#v", record)
	}
	for _, invalid := range []Event{func() Event { e := webapp; e.ConnectionID = ""; return e }(), func() Event { e := webapp; e.PID = 1; return e }()} {
		durable.Reset()
		terminal.Reset()
		if err := New(&durable, &terminal, false).EmitNormal(GlobalScope(), invalid); err == nil {
			t.Fatalf("invalid webapp event accepted: %#v", invalid)
		}
		if durable.Len() != 0 {
			t.Fatalf("invalid webapp event persisted: %q", durable.String())
		}
	}
}

func TestSinkFailureReturnsErrorAndUsesOnlyEmergencyTerminalPath(t *testing.T) {
	var terminal strings.Builder
	err := New(failingWriter{errors.New("disk full")}, &terminal, false).EmitNormal(GlobalScope(), event())
	if err == nil || !strings.Contains(err.Error(), "disk full") {
		t.Fatalf("error = %v", err)
	}
	emergency := decodeOne(t, terminal.String())
	if emergency.Runtime != RuntimeDaemon || emergency.Level != LevelError || emergency.Operation != "daemon.logging.sink-failure" {
		t.Fatalf("emergency output=%#v", emergency)
	}
}

func TestTerminalSinkFailureReturnsErrorAfterDurableRecord(t *testing.T) {
	var durable strings.Builder
	err := New(&durable, failingWriter{errors.New("terminal closed")}, false).EmitNormal(GlobalScope(), event())
	if err == nil || !strings.Contains(err.Error(), "terminal closed") {
		t.Fatalf("error = %v", err)
	}
	decodeOne(t, durable.String())
}

func TestEmitCompletesShortWritesAndRejectsZeroProgress(t *testing.T) {
	for _, target := range []struct {
		name     string
		durable  io.Writer
		terminal io.Writer
		wantErr  bool
	}{
		{name: "durable short writes", durable: &shortWriter{limit: 3}, terminal: &strings.Builder{}},
		{name: "terminal short writes", durable: &strings.Builder{}, terminal: &shortWriter{limit: 3}},
		{name: "durable zero progress", durable: &shortWriter{zero: true}, terminal: &strings.Builder{}, wantErr: true},
		{name: "terminal zero progress", durable: &strings.Builder{}, terminal: &shortWriter{zero: true}, wantErr: true},
	} {
		t.Run(target.name, func(t *testing.T) {
			err := New(target.durable, target.terminal, false).EmitNormal(GlobalScope(), event())
			if (err != nil) != target.wantErr {
				t.Fatalf("err=%v wantErr=%v", err, target.wantErr)
			}
		})
	}
}

func TestDurablePartialFailurePoisonsSharedLoggerAndPreventsLaterAppend(t *testing.T) {
	durable := &partialErrorWriter{}
	var terminal strings.Builder
	logger := New(durable, &terminal, false)
	if err := logger.EmitNormal(GlobalScope(), event()); err == nil || !strings.Contains(err.Error(), "partial disk failure") {
		t.Fatalf("first emit error=%v", err)
	}
	partial := durable.data.String()
	if partial == "" {
		t.Fatal("partial writer did not retain its unavoidable tail")
	}
	if err := logger.With("operation", "poison-check").EmitNormal(GlobalScope(), event()); err == nil || !strings.Contains(err.Error(), "poisoned") {
		t.Fatalf("second emit error=%v", err)
	}
	if durable.calls != 1 || durable.data.String() != partial {
		t.Fatalf("poisoned sink appended: calls=%d before=%q after=%q", durable.calls, partial, durable.data.String())
	}
	lines := strings.Split(strings.TrimSpace(terminal.String()), "\n")
	if len(lines) != 2 {
		t.Fatalf("emergency terminal line count=%d output=%q", len(lines), terminal.String())
	}
	for _, line := range lines {
		if record := decodeOne(t, line); record.Operation != "daemon.logging.sink-failure" {
			t.Fatalf("emergency terminal output=%#v", record)
		}
	}
}

func TestTargetManagerWorkspaceLoggersSharePoisonAcrossSessions(t *testing.T) {
	workspace := Workspace{Directory: t.TempDir(), ID: "ws-1"}
	manager := NewTargetManager()
	if _, err := manager.OpenWorkspace(workspace); err != nil {
		t.Fatal(err)
	}
	defer manager.Close()
	durable := &partialErrorWriter{}
	manager.targets[targetKey(workspace, RuntimeDaemon)].sink.writer = durable
	var terminal strings.Builder
	first, err := manager.OpenWorkspaceLogger(workspace, &terminal, false)
	if err != nil {
		t.Fatal(err)
	}
	second, err := manager.OpenWorkspaceLogger(workspace, &terminal, false)
	if err != nil {
		t.Fatal(err)
	}
	if first == second {
		t.Fatal("manager returned one logger handle instead of separate session handles")
	}
	if err := first.EmitWorkspaceNormal(workspace, event()); err == nil {
		t.Fatal("partial-error session emit succeeded")
	}
	partial := durable.data.String()
	if err := second.EmitWorkspaceNormal(workspace, event()); err == nil || !strings.Contains(err.Error(), "poisoned") {
		t.Fatalf("second session emit error=%v", err)
	}
	if durable.calls != 1 || durable.data.String() != partial {
		t.Fatalf("second session appended after poison: calls=%d before=%q after=%q", durable.calls, partial, durable.data.String())
	}
}

func TestLegacyCompatibilityWritesJSONL(t *testing.T) {
	var durable, terminal strings.Builder
	Legacy(New(&durable, &terminal, false).With("operation", "legacy.migrate", "session", "s1"))("legacy %d", 7)
	record := decodeOne(t, durable.String())
	if record.Operation != "legacy.migrate" || record.Message != "legacy 7" || record.Context["session"] != "s1" {
		t.Fatalf("record=%#v", record)
	}
}

func TestLegacyErrorWritesAtErrorSeverity(t *testing.T) {
	// Arrange: a subsystem injected with only a Logf callback must still be
	// able to say a record is a fault; at info it is indistinguishable from
	// routine progress.
	var durable, terminal strings.Builder

	// Act.
	LegacyError(New(&durable, &terminal, false).With("operation", "legacy.fault"))("held %d", 3)

	// Assert.
	record := decodeOne(t, durable.String())
	if record.Level != LevelError || record.Message != "held 3" {
		t.Fatalf("record=%#v, want an error-level record", record)
	}
}

func TestLegacyWarnWritesAtWarnSeverity(t *testing.T) {
	// Arrange: a user-visible degradation is not a fault, but at info it is
	// indistinguishable from routine progress and invisible to a level filter.
	var durable, terminal strings.Builder

	// Act.
	LegacyWarn(New(&durable, &terminal, false).With("operation", "legacy.degraded"))("degraded %d", 5)

	// Assert.
	record := decodeOne(t, durable.String())
	if record.Level != LevelWarn || record.Message != "degraded 5" {
		t.Fatalf("record=%#v, want a warn-level record", record)
	}
}

func TestLegacyStillWritesAtInfoSeverity(t *testing.T) {
	// Arrange.
	var durable, terminal strings.Builder

	// Act.
	Legacy(New(&durable, &terminal, false).With("operation", "legacy.progress"))("fine")

	// Assert.
	record := decodeOne(t, durable.String())
	if record.Level != LevelInfo {
		t.Fatalf("record level = %q, want info", record.Level)
	}
}

func TestTargetManagerInstallsExternalTargetAndAtomicallyReplacesHostileEntries(t *testing.T) {
	workspaceDir := t.TempDir()
	link := filepath.Join(workspaceDir, ".claude", "emacs", "daemon.log")
	if err := os.MkdirAll(filepath.Dir(link), 0700); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(link, []byte("hostile regular file"), 0600); err != nil {
		t.Fatal(err)
	}
	manager := NewTargetManager()
	target, err := manager.OpenWorkspace(Workspace{Directory: workspaceDir, ID: "ws-1"})
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { target.Close(); os.Remove(target.Name()) })
	info, err := os.Lstat(link)
	if err != nil {
		t.Fatal(err)
	}
	if info.Mode()&os.ModeSymlink == 0 {
		t.Fatalf("daemon link mode=%v, want symlink", info.Mode())
	}
	resolved, err := os.Readlink(link)
	if err != nil {
		t.Fatal(err)
	}
	if resolved != target.Name() || strings.HasPrefix(resolved, workspaceDir) {
		t.Fatalf("link target=%q, file=%q", resolved, target.Name())
	}
	if _, err := target.WriteString("owned target"); err != nil {
		t.Fatal(err)
	}
	contents, err := os.ReadFile(target.Name())
	if err != nil || string(contents) != "owned target" {
		t.Fatalf("contents=%q err=%v", contents, err)
	}
}

func TestWorkspaceFromDirectoryMatchesEmacsCanonicalMD5(t *testing.T) {
	workspaceDir := t.TempDir()
	alias := filepath.Join(t.TempDir(), "workspace-alias")
	if err := os.Symlink(workspaceDir, alias); err != nil {
		t.Fatal(err)
	}
	workspace, err := WorkspaceFromDirectory(alias)
	if err != nil {
		t.Fatal(err)
	}
	canonical, err := filepath.EvalSymlinks(workspaceDir)
	if err != nil {
		t.Fatal(err)
	}
	sum := md5.Sum([]byte(filepath.Clean(canonical)))
	if workspace.Directory != filepath.Clean(canonical) || workspace.ID != fmt.Sprintf("%x", sum)[:8] {
		t.Fatalf("workspace=%#v canonical=%q", workspace, canonical)
	}
}

func TestTargetManagerSeparatesDaemonAndShimTargets(t *testing.T) {
	workspace := Workspace{Directory: t.TempDir(), ID: "ws-1"}
	manager := NewTargetManager()
	daemon, err := manager.OpenWorkspaceRuntime(workspace, RuntimeDaemon)
	if err != nil {
		t.Fatal(err)
	}
	shim, err := manager.OpenWorkspaceRuntime(workspace, RuntimeShim)
	if err != nil {
		t.Fatal(err)
	}
	if daemon.Name() == shim.Name() {
		t.Fatal("daemon and shim targets share a path")
	}
	if !strings.HasPrefix(filepath.Base(daemon.Name()), "agent-repl-daemon-") || !strings.HasPrefix(filepath.Base(shim.Name()), "agent-repl-shim-") {
		t.Fatalf("runtime target names daemon=%q shim=%q", daemon.Name(), shim.Name())
	}
	for _, runtime := range []Runtime{RuntimeDaemon, RuntimeShim} {
		link := filepath.Join(workspace.Directory, ".claude", "emacs", string(runtime)+".log")
		if got, err := os.Readlink(link); err != nil || (runtime == RuntimeDaemon && got != daemon.Name()) || (runtime == RuntimeShim && got != shim.Name()) {
			t.Fatalf("%s link=%q err=%v", runtime, got, err)
		}
	}
	if err := manager.Close(); err != nil {
		t.Fatal(err)
	}
}

func TestTargetManagerSeparatesForwardedRuntimeTargets(t *testing.T) {
	workspace := Workspace{Directory: t.TempDir(), ID: "ws-1"}
	manager := NewTargetManager()
	webapp, err := manager.OpenWorkspaceRuntime(workspace, RuntimeWebapp)
	if err != nil {
		t.Fatal(err)
	}
	sidecar, err := manager.OpenWorkspaceRuntime(workspace, RuntimeSidecar)
	if err != nil {
		t.Fatal(err)
	}
	if webapp.Name() == sidecar.Name() {
		t.Fatal("webapp and sidecar targets share a path")
	}
	for _, runtime := range []Runtime{RuntimeWebapp, RuntimeSidecar} {
		link := filepath.Join(workspace.Directory, ".claude", "emacs", string(runtime)+".log")
		if info, err := os.Lstat(link); err != nil || info.Mode()&os.ModeSymlink == 0 {
			t.Fatalf("%s canonical target is not a symlink: info=%v err=%v", runtime, info, err)
		}
	}
	if err := manager.Close(); err != nil {
		t.Fatal(err)
	}
}

func TestPersistForwardedPreservesSourceTimestampAndEnrichesIdentity(t *testing.T) {
	workspace := Workspace{Directory: t.TempDir(), ID: "ws-1"}
	manager := NewTargetManager()
	logger, err := manager.OpenWorkspaceRuntimeLogger(workspace, RuntimeWebapp, io.Discard, false)
	if err != nil {
		t.Fatal(err)
	}
	// 123 microseconds: the canonical timestamp layout preserves microseconds.
	timestamp := time.Date(2026, 7, 28, 12, 0, 0, 123_000, time.UTC)
	err = logger.PersistForwarded(workspace, RuntimeWebapp, Record{Timestamp: NewStamp(timestamp), Runtime: RuntimeWebapp, Level: LevelWarn, Verbosity: Normal, Operation: "webapp.render.failed", Message: "render failed", Context: map[string]any{"cause": "x"}, ConnectionID: "connection-1"}, ForwardedIdentity{AgentReplSessionID: "agent-1", ClaudeSessionID: "claude-1", RequestID: "request-1"})
	if err != nil {
		t.Fatal(err)
	}
	link := filepath.Join(workspace.Directory, ".claude", "emacs", "webapp.log")
	raw, err := os.ReadFile(link)
	if err != nil {
		t.Fatal(err)
	}
	var record Record
	if err := json.Unmarshal(raw, &record); err != nil {
		t.Fatal(err)
	}
	if !record.Timestamp.Equal(timestamp) || record.WorkspaceID != workspace.ID || record.AgentReplSessionID != "agent-1" || record.ClaudeSessionID != "claude-1" || record.RequestID != "request-1" {
		t.Fatalf("persisted record=%+v", record)
	}
	if err := manager.Close(); err != nil {
		t.Fatal(err)
	}
}

func TestParseForwardedRecordRejectsTrailingValues(t *testing.T) {
	raw := []byte(`{"timestamp":"2026-07-28T12:00:00Z","runtime":"webapp","level":"info","verbosity":"normal","operation":"webapp.x","message":"x","context":{},"connection_id":"connection-1"} {}`)
	if _, err := ParseForwardedRecord(raw); err == nil {
		t.Fatal("trailing JSON value was accepted")
	}
	if _, err := ParseForwardedRecord([]byte("{\"timestamp\":\"2026-07-28T12:00:00Z\",\"runtime\":\"webapp\",\"level\":\"info\",\"verbosity\":\"normal\",\"operation\":\"webapp.x\",\"message\":\"x\",\"context\":{},\"connection_id\":\"connection-1\"} \n\t")); err != nil {
		t.Fatalf("trailing whitespace rejected: %v", err)
	}
}

func TestTargetManagerRuntimeValidationAndSameInodeTruncation(t *testing.T) {
	workspace := Workspace{Directory: t.TempDir(), ID: "ws-1"}
	manager := NewTargetManager()
	if _, err := manager.OpenWorkspaceRuntime(workspace, RuntimeStore); err == nil {
		t.Fatal("unsupported runtime target accepted")
	}
	shim, err := manager.OpenWorkspaceRuntime(workspace, RuntimeShim)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := shim.WriteString("shim records"); err != nil {
		t.Fatal(err)
	}
	if err := manager.TruncateWorkspaceRuntime(workspace, RuntimeShim); err != nil {
		t.Fatal(err)
	}
	if info, err := shim.Stat(); err != nil || info.Size() != 0 {
		t.Fatalf("truncated shim=%v err=%v", info, err)
	}
	link := filepath.Join(workspace.Directory, ".claude", "emacs", "shim.log")
	if err := os.Remove(link); err != nil {
		t.Fatal(err)
	}
	if err := os.Symlink(t.TempDir(), link); err != nil {
		t.Fatal(err)
	}
	if err := manager.TruncateWorkspaceRuntime(workspace, RuntimeShim); err == nil {
		t.Fatal("truncation accepted a redirected canonical link")
	}
	if err := manager.Close(); err != nil {
		t.Fatal(err)
	}
}

func TestWorkspaceTargetWriterCompletesPartialWritesAndSurfacesErrors(t *testing.T) {
	partial := &targetFileStub{maxWrite: 3}
	writer := &workspaceTargetWriter{file: partial, capBytes: 1024, runtime: RuntimeDaemon}
	payload := []byte("complete-json-line\n")
	n, err := writer.Write(payload)
	if err != nil || n != len(payload) || partial.data.String() != string(payload) || partial.writes < 2 {
		t.Fatalf("partial completion n=%d err=%v writes=%d data=%q", n, err, partial.writes, partial.data.String())
	}

	diskErr := errors.New("disk failure")
	failing := &targetFileStub{maxWrite: 4, writeErr: diskErr}
	writer = &workspaceTargetWriter{file: failing, capBytes: 1024, runtime: RuntimeWebapp}
	n, err = writer.Write(payload)
	if !errors.Is(err, diskErr) || n != 4 || failing.writes != 1 {
		t.Fatalf("write failure n=%d err=%v writes=%d", n, err, failing.writes)
	}
}

func TestTargetManagerMaintainsAllRuntimeCapsInPlaceAtBoundary(t *testing.T) {
	workspace := Workspace{Directory: t.TempDir(), ID: "ws-1"}
	manager := NewTargetManager()
	manager.capBytes = 32
	for _, runtime := range []Runtime{RuntimeDaemon, RuntimeShim, RuntimeWebapp, RuntimeSidecar} {
		target, err := manager.OpenWorkspaceRuntime(workspace, runtime)
		if err != nil {
			t.Fatal(err)
		}
		before, err := target.Stat()
		if err != nil {
			t.Fatal(err)
		}
		if _, err := target.WriteString(strings.Repeat("x", int(manager.capBytes))); err != nil {
			t.Fatal(err)
		}
		if failures := manager.MaintainSizeCaps(); len(failures) != 0 {
			t.Fatalf("%s cap failures=%v", runtime, failures)
		}
		after, err := target.Stat()
		if err != nil {
			t.Fatal(err)
		}
		if !os.SameFile(before, after) || after.Size() != 0 {
			t.Fatalf("%s inode/size before=%v after=%v", runtime, before, after)
		}
		link := filepath.Join(workspace.Directory, ".claude", "emacs", string(runtime)+".log")
		linkInfo, err := os.Lstat(link)
		if err != nil || linkInfo.Mode()&os.ModeSymlink == 0 {
			t.Fatalf("%s canonical link info=%v err=%v", runtime, linkInfo, err)
		}
		if _, err := target.WriteString("after-cap\n"); err != nil {
			t.Fatal(err)
		}
		if got, err := os.ReadFile(link); err != nil || string(got) != "after-cap\n" {
			t.Fatalf("%s post-cap contents=%q err=%v", runtime, got, err)
		}
	}
	if err := manager.Close(); err != nil {
		t.Fatal(err)
	}
}

func TestTargetManagerTargetsUseAppendSemanticsAcrossStaleOffsets(t *testing.T) {
	workspace := Workspace{Directory: t.TempDir(), ID: "ws-1"}
	manager := NewTargetManager()
	target, err := manager.OpenWorkspaceRuntime(workspace, RuntimeShim)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := target.WriteString("first\n"); err != nil {
		t.Fatal(err)
	}
	if _, err := target.Seek(0, io.SeekStart); err != nil {
		t.Fatal(err)
	}
	if _, err := target.WriteString("second\n"); err != nil {
		t.Fatal(err)
	}
	raw, err := os.ReadFile(target.Name())
	if err != nil {
		t.Fatal(err)
	}
	if string(raw) != "first\nsecond\n" {
		t.Fatalf("target did not preserve O_APPEND semantics: %q", raw)
	}
	if err := manager.Close(); err != nil {
		t.Fatal(err)
	}
}

func TestTargetManagerCapRejectsLinkIdentityMismatchAndReportsJSON(t *testing.T) {
	workspace := Workspace{Directory: t.TempDir(), ID: "ws-1"}
	manager := NewTargetManager()
	manager.capBytes = 8
	target, err := manager.OpenWorkspaceRuntime(workspace, RuntimeSidecar)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := target.WriteString("over-cap"); err != nil {
		t.Fatal(err)
	}
	link := filepath.Join(workspace.Directory, ".claude", "emacs", "sidecar.log")
	if err := os.Remove(link); err != nil {
		t.Fatal(err)
	}
	other := filepath.Join(t.TempDir(), "other.log")
	if err := os.WriteFile(other, []byte("other"), 0600); err != nil {
		t.Fatal(err)
	}
	if err := os.Symlink(other, link); err != nil {
		t.Fatal(err)
	}
	failures := manager.MaintainSizeCaps()
	if len(failures) != 1 || failures[0].Runtime != RuntimeSidecar || !strings.Contains(failures[0].Error(), "active inode") {
		t.Fatalf("cap failures=%v", failures)
	}
	var terminal strings.Builder
	if err := manager.ReportTargetCapError(failures[0], &terminal, false); err != nil {
		t.Fatalf("report cap failure: %v", err)
	}
	daemonLink := filepath.Join(workspace.Directory, ".claude", "emacs", "daemon.log")
	raw, err := os.ReadFile(daemonLink)
	if err != nil {
		t.Fatal(err)
	}
	record := decodeOne(t, string(raw))
	if record.Level != LevelError || record.Operation != "daemon.logging.workspace-cap-failed" || record.WorkspaceDirectory != workspace.Directory {
		t.Fatalf("cap failure record=%#v", record)
	}
	if terminal.String() == "" {
		t.Fatal("normal cap failure record was not mirrored to terminal")
	}
	if err := manager.Close(); err != nil {
		t.Fatal(err)
	}
}

func TestDaemonTargetCapMismatchUsesJSONEmergencyAndPoisonsSink(t *testing.T) {
	workspace := Workspace{Directory: t.TempDir(), ID: "ws-1"}
	manager, err := NewTargetManagerWithCap(8)
	if err != nil {
		t.Fatal(err)
	}
	target, err := manager.OpenWorkspace(workspace)
	if err != nil {
		t.Fatal(err)
	}
	if _, err := target.WriteString("12345678"); err != nil {
		t.Fatal(err)
	}
	link := filepath.Join(workspace.Directory, ".claude", "emacs", "daemon.log")
	if err := os.Remove(link); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(link, []byte("hostile regular file"), 0600); err != nil {
		t.Fatal(err)
	}
	failures := manager.MaintainSizeCaps()
	if len(failures) != 1 || !strings.Contains(failures[0].Error(), "not a symlink") {
		t.Fatalf("cap failures=%v", failures)
	}
	var terminal strings.Builder
	if err := manager.ReportTargetCapError(failures[0], &terminal, false); err == nil {
		t.Fatal("poisoned daemon target unexpectedly persisted cap failure")
	}
	emergency := decodeOne(t, terminal.String())
	if emergency.Operation != "daemon.logging.sink-failure" || emergency.Level != LevelError || emergency.WorkspaceDirectory != workspace.Directory {
		t.Fatalf("daemon cap emergency=%#v", emergency)
	}
	if failures := manager.MaintainSizeCaps(); len(failures) != 0 {
		t.Fatalf("poisoned target was retried: %v", failures)
	}
	if err := manager.Close(); err != nil {
		t.Fatal(err)
	}
}

func TestTargetManagerExplicitCapRejectsMalformedBoundary(t *testing.T) {
	for _, capBytes := range []int64{0, -1} {
		if manager, err := NewTargetManagerWithCap(capBytes); err == nil || manager != nil {
			t.Fatalf("cap %d accepted: manager=%v err=%v", capBytes, manager, err)
		}
	}
}

func TestTargetManagerReplacesHostileSymlinkWithoutFollowingIt(t *testing.T) {
	workspaceDir := t.TempDir()
	link := filepath.Join(workspaceDir, ".claude", "emacs", "daemon.log")
	if err := os.MkdirAll(filepath.Dir(link), 0700); err != nil {
		t.Fatal(err)
	}
	hostile := filepath.Join(t.TempDir(), "hostile.log")
	if err := os.WriteFile(hostile, []byte("do not follow"), 0600); err != nil {
		t.Fatal(err)
	}
	if err := os.Symlink(hostile, link); err != nil {
		t.Fatal(err)
	}
	target, err := NewTargetManager().OpenWorkspace(Workspace{Directory: workspaceDir, ID: "ws-1"})
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { target.Close(); os.Remove(target.Name()) })
	if contents, err := os.ReadFile(hostile); err != nil || string(contents) != "do not follow" {
		t.Fatalf("hostile contents=%q err=%v", contents, err)
	}
	if resolved, err := os.Readlink(link); err != nil || resolved != target.Name() {
		t.Fatalf("replacement=%q err=%v", resolved, err)
	}
}

func TestTargetManagerUsesUniqueStageNameWhenLegacyResidueExists(t *testing.T) {
	workspaceDir := t.TempDir()
	directory := filepath.Join(workspaceDir, ".claude", "emacs")
	if err := os.MkdirAll(directory, 0700); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join(directory, "daemon.log.new"), []byte("stale"), 0600); err != nil {
		t.Fatal(err)
	}
	target, err := NewTargetManager().OpenWorkspace(Workspace{Directory: workspaceDir, ID: "ws-1"})
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { target.Close(); os.Remove(target.Name()) })
	if _, err := os.Lstat(filepath.Join(directory, "daemon.log")); err != nil {
		t.Fatal(err)
	}
}

func TestTargetManagerRejectsRedirectingDirectoryComponents(t *testing.T) {
	for _, component := range []string{".claude", filepath.Join(".claude", "emacs")} {
		workspaceDir := t.TempDir()
		redirect := t.TempDir()
		path := filepath.Join(workspaceDir, component)
		if err := os.MkdirAll(filepath.Dir(path), 0700); err != nil {
			t.Fatal(err)
		}
		if err := os.Symlink(redirect, path); err != nil {
			t.Fatal(err)
		}
		manager := NewTargetManager()
		if _, err := manager.OpenWorkspace(Workspace{Directory: workspaceDir, ID: "ws-1"}); err == nil {
			t.Fatalf("redirecting component %q accepted", component)
		}
		if len(manager.targets) != 0 {
			t.Fatalf("targets after rejected %q = %d", component, len(manager.targets))
		}
	}
}

func TestTargetManagerRejectsNonDirectoryComponents(t *testing.T) {
	for _, component := range []string{".claude", filepath.Join(".claude", "emacs")} {
		workspaceDir := t.TempDir()
		path := filepath.Join(workspaceDir, component)
		if err := os.MkdirAll(filepath.Dir(path), 0700); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(path, []byte("not a directory"), 0600); err != nil {
			t.Fatal(err)
		}
		manager := NewTargetManager()
		if _, err := manager.OpenWorkspace(Workspace{Directory: workspaceDir, ID: "ws-1"}); err == nil {
			t.Fatalf("non-directory component %q accepted", component)
		}
		if len(manager.targets) != 0 {
			t.Fatalf("targets after rejected %q = %d", component, len(manager.targets))
		}
	}
}

func TestTargetManagerRepeatedLookupPreservesActiveTargetContents(t *testing.T) {
	workspaceDir := t.TempDir()
	manager := NewTargetManager()
	workspace := Workspace{Directory: workspaceDir, ID: "ws-1"}
	target, err := manager.OpenWorkspace(workspace)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { target.Close(); os.Remove(target.Name()) })
	if _, err := target.WriteString("preserve these logs"); err != nil {
		t.Fatal(err)
	}
	before, err := target.Stat()
	if err != nil {
		t.Fatal(err)
	}
	again, err := manager.OpenWorkspace(workspace)
	if err != nil {
		t.Fatal(err)
	}
	after, err := again.Stat()
	if err != nil {
		t.Fatal(err)
	}
	if !os.SameFile(before, after) {
		t.Fatalf("reopen replaced active inode: before=%v after=%v", before, after)
	}
	if after.Size() == 0 {
		t.Fatalf("repeated lookup erased active target")
	}
	contents, err := os.ReadFile(target.Name())
	if err != nil || string(contents) != "preserve these logs" {
		t.Fatalf("contents=%q err=%v", contents, err)
	}
}

func TestTargetManagerTruncateWorkspacePreservesInodeAndClearsContents(t *testing.T) {
	workspaceDir := t.TempDir()
	manager := NewTargetManager()
	workspace := Workspace{Directory: workspaceDir, ID: "ws-1"}
	target, err := manager.OpenWorkspace(workspace)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { target.Close(); os.Remove(target.Name()) })
	if _, err := target.WriteString("clear these logs"); err != nil {
		t.Fatal(err)
	}
	before, err := target.Stat()
	if err != nil {
		t.Fatal(err)
	}
	if err := manager.TruncateWorkspace(workspace); err != nil {
		t.Fatal(err)
	}
	after, err := target.Stat()
	if err != nil {
		t.Fatal(err)
	}
	if !os.SameFile(before, after) || after.Size() != 0 {
		t.Fatalf("before=%v after=%v", before, after)
	}
	if _, err := target.WriteString("new logs"); err != nil {
		t.Fatal(err)
	}
	contents, err := os.ReadFile(target.Name())
	if err != nil || string(contents) != "new logs" {
		t.Fatalf("contents=%q err=%v", contents, err)
	}
}

func TestTargetManagerTruncateWorkspaceRejectsMissingAndMismatchedWorkspace(t *testing.T) {
	workspaceDir := t.TempDir()
	manager := NewTargetManager()
	workspace := Workspace{Directory: workspaceDir, ID: "ws-1"}
	if err := manager.TruncateWorkspace(workspace); err == nil {
		t.Fatal("missing target truncation succeeded")
	}
	target, err := manager.OpenWorkspace(workspace)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { target.Close(); os.Remove(target.Name()) })
	if err := manager.TruncateWorkspace(Workspace{Directory: workspaceDir, ID: "ws-2"}); err == nil {
		t.Fatal("mismatched target truncation succeeded")
	}
	if err := manager.TruncateWorkspace(Workspace{Directory: "relative", ID: "ws-1"}); err == nil {
		t.Fatal("malformed target truncation succeeded")
	}
	if _, err := target.WriteString("untouched"); err != nil {
		t.Fatal(err)
	}
	if contents, err := os.ReadFile(target.Name()); err != nil || string(contents) != "untouched" {
		t.Fatalf("contents=%q err=%v", contents, err)
	}
}

func TestTargetManagerRejectsMalformedWorkspaceWithoutCreatingState(t *testing.T) {
	manager := NewTargetManager()
	if _, err := manager.OpenWorkspace(Workspace{Directory: "relative", ID: "ws-1"}); err == nil {
		t.Fatal("relative workspace accepted")
	}
	if len(manager.targets) != 0 {
		t.Fatalf("targets=%d after rejected workspace", len(manager.targets))
	}
}

func TestTargetManagerRejectsConflictingWorkspaceIdentity(t *testing.T) {
	workspaceDir := t.TempDir()
	manager := NewTargetManager()
	target, err := manager.OpenWorkspace(Workspace{Directory: workspaceDir, ID: "ws-1"})
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { target.Close(); os.Remove(target.Name()) })
	if _, err := manager.OpenWorkspace(Workspace{Directory: workspaceDir, ID: "ws-2"}); err == nil {
		t.Fatal("conflicting workspace ID accepted")
	}
}

func TestTargetManagerCloseForgetsTargetsAndPreservesSymlinkTarget(t *testing.T) {
	workspaceDir := t.TempDir()
	manager := NewTargetManager()
	target, err := manager.OpenWorkspace(Workspace{Directory: workspaceDir, ID: "ws-1"})
	if err != nil {
		t.Fatal(err)
	}
	link := filepath.Join(workspaceDir, ".claude", "emacs", "daemon.log")
	targetPath := target.Name()
	if err := manager.Close(); err != nil {
		t.Fatal(err)
	}
	if len(manager.targets) != 0 {
		t.Fatalf("targets=%d after Close", len(manager.targets))
	}
	if got, err := os.Readlink(link); err != nil || got != targetPath {
		t.Fatalf("link=%q err=%v", got, err)
	}
	if _, err := os.Stat(targetPath); err != nil {
		t.Fatalf("Close removed target: %v", err)
	}
	t.Cleanup(func() { os.Remove(targetPath) })
}

func TestTargetManagerReportsClosedTargetAndNilManager(t *testing.T) {
	var nilManager *TargetManager
	if err := nilManager.Close(); err == nil {
		t.Fatal("nil manager Close succeeded")
	}
	workspaceDir := t.TempDir()
	manager := NewTargetManager()
	target, err := manager.OpenWorkspace(Workspace{Directory: workspaceDir, ID: "ws-1"})
	if err != nil {
		t.Fatal(err)
	}
	if err := target.Close(); err != nil {
		t.Fatal(err)
	}
	if _, err := manager.OpenWorkspace(Workspace{Directory: workspaceDir, ID: "ws-1"}); err == nil {
		t.Fatal("closed target reopened")
	}
	if err := manager.Close(); err == nil {
		t.Fatal("Close did not report closed owned target")
	}
	if len(manager.targets) != 0 {
		t.Fatalf("targets=%d after Close", len(manager.targets))
	}
	t.Cleanup(func() { os.Remove(target.Name()) })
}

func TestClampAndTagCompatibility(t *testing.T) {
	if got := Clamp(strings.Repeat("x", 20), 10); got != strings.Repeat("x", 10)+" …[clamped]" {
		t.Fatalf("Clamp=%q", got)
	}
	var lines []string
	Tag(func(format string, args ...any) { lines = append(lines, fmt.Sprintf(format, args...)) }, "session", "s1")("hello")
	if len(lines) != 1 || lines[0] != "hello {session=s1}" {
		t.Fatalf("lines=%v", lines)
	}
}

func TestRetainedCompatibilityHelpersCoverNilOddTagFuncAndCallOutcomes(t *testing.T) {
	assertPanics := func(name string, call func()) {
		t.Helper()
		defer func() {
			if recover() == nil {
				t.Errorf("%s did not panic", name)
			}
		}()
		call()
	}
	var output strings.Builder
	assertPanics("nil durable", func() { New(nil, &output, false) })
	assertPanics("nil terminal", func() { New(&output, nil, false) })
	var logger *Logger
	assertPanics("nil With", func() { logger.With("operation", "test") })
	assertPanics("nil Legacy", func() { Legacy(logger) })
	assertPanics("nil Log", func() { logger.Log("record") })
	var lines []string
	logf := func(format string, args ...any) { lines = append(lines, fmt.Sprintf(format, args...)) }
	Tag(logf, "session", "s1", "dangling")("tag")
	Tag(logf)("untagged")
	value := "before"
	TagFunc(logf, func() []any { return []any{"value", value} })("func")
	value = "after"
	TagFunc(logf, func() []any { return nil })("plain")
	done := Call(logf, "call", "session", "s1")
	done("ok", nil)
	done = Call(logf, "call", "session", "s1")
	done("partial", errors.New("failed"))
	var durable, terminal strings.Builder
	New(&durable, &terminal, true).LogVerbose("legacy verbose")
	joined := strings.Join(lines, "\n")
	for _, want := range []string{"tag {session=s1 dangling=!MISSING}", "untagged", "func {value=before}", "plain", "call: call ok in", "call: call FAILED after", "partial"} {
		if !strings.Contains(joined, want) {
			t.Fatalf("missing %q in %q", want, joined)
		}
	}
	if record := decodeOne(t, durable.String()); record.Verbosity != Verbose || terminal.Len() == 0 {
		t.Fatalf("legacy verbose record=%#v terminal=%q", record, terminal.String())
	}
}

// canonicalTimestampPattern is the shared shape every agent-repl runtime emits:
// RFC 3339, 24-hour clock, fixed-width microseconds, explicit numeric offset.
var canonicalTimestampPattern = regexp.MustCompile(`^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{6}[+-]\d{2}:\d{2}$`)

func TestStampMarshalsCanonicalFixedWidthLayout(t *testing.T) {
	// Arrange: a whole second, whose subsecond digits Go's RFC 3339 default would drop.
	stamp := NewStamp(time.Date(2026, 7, 28, 12, 34, 56, 0, time.UTC))

	// Act
	raw, err := json.Marshal(stamp)
	if err != nil {
		t.Fatal(err)
	}

	// Assert
	var text string
	if err := json.Unmarshal(raw, &text); err != nil {
		t.Fatal(err)
	}
	if !canonicalTimestampPattern.MatchString(text) {
		t.Fatalf("timestamp = %q, want canonical layout", text)
	}
}

func TestStampMarshalsInLocalZoneRatherThanUTC(t *testing.T) {
	// Arrange: an instant held in UTC.
	at := time.Date(2026, 7, 28, 12, 34, 56, 789000000, time.UTC)

	// Act
	raw, err := json.Marshal(NewStamp(at))
	if err != nil {
		t.Fatal(err)
	}

	// Assert: the rendered wall clock is the local one, never a "Z" instant.
	var text string
	if err := json.Unmarshal(raw, &text); err != nil {
		t.Fatal(err)
	}
	if text != at.Local().Format(logging.TimestampLayout) || strings.HasSuffix(text, "Z") {
		t.Fatalf("timestamp = %q, want %q", text, at.Local().Format(logging.TimestampLayout))
	}
}

func TestStampUnmarshalsForwardedUTCTimestampAsLocalInstant(t *testing.T) {
	// Arrange: a runtime that has not yet migrated still forwards a "Z" timestamp.
	raw := []byte(`"2026-07-28T12:34:56.789000Z"`)

	// Act
	var stamp Stamp
	if err := json.Unmarshal(raw, &stamp); err != nil {
		t.Fatal(err)
	}

	// Assert: the instant is preserved and re-rendered canonically.
	want := time.Date(2026, 7, 28, 12, 34, 56, 789000000, time.UTC)
	if !stamp.Equal(want) {
		t.Fatalf("instant = %v, want %v", stamp.Time, want)
	}
	if _, offset := stamp.Zone(); offset != localOffset(t, want) {
		t.Fatalf("zone offset = %d, want local", offset)
	}
}

func TestStampUnmarshalRejectsNonRFC3339Timestamp(t *testing.T) {
	// Arrange
	raw := []byte(`"28 Jul 2026 12:34:56"`)

	// Act
	var stamp Stamp
	err := json.Unmarshal(raw, &stamp)

	// Assert
	if err == nil {
		t.Fatal("expected a parse failure for a non-RFC 3339 timestamp")
	}
}

func localOffset(t *testing.T, at time.Time) int {
	t.Helper()
	_, offset := at.Local().Zone()
	return offset
}

// gatedWriter is a durable target writer that parks until it is released. It
// stands in for a workspace whose underlying file write is slow: production saw
// one workspace's log traffic hold up an unrelated workspace's open_workspace
// handler for 8 seconds through the manager's daemon-global lock.
type gatedWriter struct {
	entered chan struct{}
	release chan struct{}
}

func newGatedWriter() *gatedWriter {
	return &gatedWriter{entered: make(chan struct{}, 1), release: make(chan struct{})}
}

func (w *gatedWriter) Write(p []byte) (int, error) {
	select {
	case w.entered <- struct{}{}:
	default:
	}
	<-w.release
	return len(p), nil
}

// ONE WORKSPACE'S PARKED WRITER MUST NOT STALL ANOTHER WORKSPACE.
func TestTargetManagerLogsToOneWorkspaceWhileAnotherWorkspaceWriterIsParked(t *testing.T) {
	// Arrange.
	parked := Workspace{Directory: t.TempDir(), ID: "ws-parked"}
	moving := Workspace{Directory: t.TempDir(), ID: "ws-moving"}
	manager := NewTargetManager()
	defer manager.Close()
	if _, err := manager.OpenWorkspace(parked); err != nil {
		t.Fatal(err)
	}
	if _, err := manager.OpenWorkspace(moving); err != nil {
		t.Fatal(err)
	}
	gate := newGatedWriter()
	manager.targets[targetKey(parked, RuntimeDaemon)].sink.writer = gate
	parkedLogger, err := manager.OpenWorkspaceLogger(parked, io.Discard, false)
	if err != nil {
		t.Fatal(err)
	}
	parkedEmit := make(chan error, 1)
	go func() { parkedEmit <- parkedLogger.EmitWorkspaceNormal(parked, event()) }()
	<-gate.entered

	// Act. A different workspace resolves its logger and persists a record
	// while the first workspace's writer is still parked.
	progress := make(chan error, 1)
	go func() {
		logger, openErr := manager.OpenWorkspaceLogger(moving, io.Discard, false)
		if openErr != nil {
			progress <- openErr
			return
		}
		progress <- logger.EmitWorkspaceNormal(moving, event())
	}()

	// Assert.
	select {
	case err := <-progress:
		if err != nil {
			t.Fatalf("emit for an unrelated workspace: %v", err)
		}
	case <-time.After(5 * time.Second):
		t.Fatal("an unrelated workspace's emit waited on a parked workspace writer; the manager lock is still daemon-global")
	}
	close(gate.release)
	if err := <-parkedEmit; err != nil {
		t.Fatalf("parked workspace emit: %v", err)
	}
}

// ONE ACQUISITION PER RECORD: resolving a logger used to take the manager lock
// twice, doubling contention on the very lock that serialized every workspace.
func TestTargetManagerResolvesALoggerWithOneLockAcquisition(t *testing.T) {
	// Arrange.
	workspace := Workspace{Directory: t.TempDir(), ID: "ws-1"}
	manager := NewTargetManager()
	defer manager.Close()
	if _, err := manager.OpenWorkspace(workspace); err != nil {
		t.Fatal(err)
	}
	before := manager.LockAcquisitionsForTest()

	// Act.
	const records = 10
	for i := 0; i < records; i++ {
		if _, err := manager.OpenWorkspaceLogger(workspace, io.Discard, false); err != nil {
			t.Fatalf("resolve logger %d: %v", i, err)
		}
	}

	// Assert.
	if got := manager.LockAcquisitionsForTest() - before; got != records {
		t.Fatalf("%d records cost %d manager lock acquisitions, want exactly %d", records, got, records)
	}
}
