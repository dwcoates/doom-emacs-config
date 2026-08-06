package server

import (
	"bufio"
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"net"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"sync"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"google.golang.org/protobuf/encoding/protojson"

	"claude-repld/internal/errclass"
	"claude-repld/internal/frontend"
	"claude-repld/internal/registry"
	"claude-repld/internal/session"
	"claude-repld/internal/sessioncontroller"
	"claude-repld/internal/shim"
	"claude-repld/internal/ssm"
	"claude-repld/internal/workspace/merge"
)

type discardFileDiagnosticPersister struct{}

func (discardFileDiagnosticPersister) PersistFileDiagnostic(string, string, *corev1.Event, *corev1.FilePlaneDiagnostic) error {
	return nil
}

// ---------------------------------------------------------------------------
// Harness
//
// Post-cutover the daemon has no live-session hub: it consumes each session's
// UDS shim through a real *sessioncontroller.Manager over a FAKE Spawner/Locator (no
// real node process), renders onto a real frontend.Server + SSM, and treats
// the registry as the source of truth. The harness wires exactly that so HTTP
// routes are exercised against the production plumbing.
// ---------------------------------------------------------------------------

// fakeSpawner records EnsureShim/StopShim calls and never launches anything,
// so create's eager bring-up resolves without a real shim.
type fakeSpawner struct {
	mu      sync.Mutex
	ensured []string
	stopped []string
	stopBy  []shim.Stop
	dropped []string
	err     error
	// stopErr, when set, makes every StopShim report it. A test arranges it
	// after the sessions whose bring-up it needs are already up.
	stopErr error
}

func (f *fakeSpawner) EnsureShim(_ context.Context, sessionID string) (sessioncontroller.SpawnResult, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.ensured = append(f.ensured, sessionID)
	return sessioncontroller.SpawnResult{}, f.err
}

func (f *fakeSpawner) DropResume(sessionID string) (string, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.dropped = append(f.dropped, sessionID)
	return "", nil
}

func (f *fakeSpawner) StopShim(sessionID string, _ int32, by shim.Stop) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.stopped = append(f.stopped, sessionID)
	f.stopBy = append(f.stopBy, by)
	return f.stopErr
}

// stopAttributions returns the attribution each stop carried, so a server-level
// test can prove WHICH teardown reached the shim.
func (f *fakeSpawner) stopAttributions() []shim.Stop {
	f.mu.Lock()
	defer f.mu.Unlock()
	return slices.Clone(f.stopBy)
}

func (f *fakeSpawner) ensuredIDs() []string {
	f.mu.Lock()
	defer f.mu.Unlock()
	return slices.Clone(f.ensured)
}

func (f *fakeSpawner) stoppedIDs() []string {
	f.mu.Lock()
	defer f.mu.Unlock()
	return slices.Clone(f.stopped)
}

// stubMerge / stubLifecycle / stubState satisfy the frontend command handler's
// required collaborators; the daemon's HTTP tests never drive a merge or a
// lifecycle command, so they are inert.
// stubConnSource stands in for the shim listener: the harness never runs a
// real shim, so nothing ever dials in.
type stubConnSource struct{}

func (stubConnSource) Next(ctx context.Context, _ string) (net.Conn, *corev1.ShimHello, error) {
	<-ctx.Done()
	return nil, nil, ctx.Err()
}

type stubMerge struct{}

func (stubMerge) Enqueue(context.Context, merge.Request) (merge.Position, error) {
	return merge.Position{Index: 1, Depth: 1, Repo: "/repo/.git"}, nil
}
func (stubMerge) Resume(context.Context, merge.Request) error { return nil }
func (stubMerge) Abandon(context.Context, string) (bool, error) {
	return false, nil
}
func (stubMerge) Evict(context.Context, string) (int, error) {
	return 0, nil
}

type stubLifecycle struct{}

func (stubLifecycle) Close(context.Context, string) error { return nil }
func (stubLifecycle) Open(context.Context, string) error  { return nil }

// OpenDriveable is the merge's bring-up seam; these command-handler tests drive
// no merge, so it answers exactly as Open does.
func (stubLifecycle) OpenDriveable(context.Context, string) error { return nil }
func (stubLifecycle) OpenForMerge(context.Context, string) error  { return nil }

type stubState struct{ ssm *ssm.Manager }

func (s stubState) Snapshot() *frontendv1.StateSnapshot {
	snap := &frontendv1.StateSnapshot{}
	if states, err := s.ssm.Snapshot(); err == nil {
		snap.Workspaces = states
	}
	return snap
}

type harness struct {
	ts         *httptest.Server
	srv        *Server
	reg        *registry.Registry
	controller *sessioncontroller.Manager
	spawner    *fakeSpawner
	ssm        *ssm.Manager
	fe         *frontend.Server
	// legacyTurnEnds is the durable instant the keep-alive policy measures a
	// record with no last-turn-end from. Zero (the default) reports NO dated
	// fact, which is the same answer an unwired stamper gives, so a test that
	// does not set it sees the staleness check decline exactly as before.
	legacyTurnEnds *fakeLegacyTurnEnds
}

// fakeLegacyTurnEnds is the harness's sessioncontroller.LegacyTurnEndStamper.
// A zero atMs is "this session has no dated activity to measure", never a
// guess at now().
type fakeLegacyTurnEnds struct {
	atMs int64
}

func (f *fakeLegacyTurnEnds) StampLegacyTurnEnd(string, string) (int64, bool) {
	return f.atMs, f.atMs > 0
}

func newHarness(t *testing.T) *harness {
	return newHarnessWith(t, Config{})
}

// newHarnessWith builds a harness, letting a test override selected Config
// fields (RequestShutdown, Accounts, Logins, WidgetAssetsDir,
// BinaryMTime, IdleSweepTicks). Registry, Controller, SSM, and Frontend are always
// wired.
func newHarnessWith(t *testing.T, extra Config) *harness {
	t.Helper()
	logf := func(string, ...any) {}
	reg := registry.Open(filepath.Join(t.TempDir(), "sessions.db"), logf)

	mgr, err := ssm.Open(ssm.Options{
		DBPath:   filepath.Join(t.TempDir(), "state.db"),
		Resolver: NewRegistryResolver(reg),
		Logf:     logf,
	})
	if err != nil {
		t.Fatalf("ssm open: %v", err)
	}
	t.Cleanup(func() { _ = mgr.Close() })

	spawner := &fakeSpawner{}
	legacyTurnEnds := &fakeLegacyTurnEnds{}
	seqStore := NewRegistrySeqStore(reg, logf)
	controller, err := sessioncontroller.New(sessioncontroller.Config{
		Push:              &PushForwarder{Logf: logf},
		SSM:               mgr,
		Spawner:           spawner,
		Locator:           &SessionLocator{Reg: reg},
		Source:            stubConnSource{},
		FileDiagnostics:   discardFileDiagnosticPersister{},
		SeqStore:          seqStore,
		ClearCompactStore: seqStore,
		TurnAccountings:   newTestTurnAccountingStore(),
		// The idle sweep hibernates through the ONE transition, which refuses
		// to sleep a session it cannot record. Wired here for the same reason
		// main wires it: an unrecorded stop is a session the next daemon
		// revives implicitly.
		Hibernations: &RegistryRegistrar{Reg: reg, Logf: logf},
		// The staleness check at acceptance and bring-up measures from this
		// when a record carries no last-turn-end of its own, exactly as main
		// wires server.stampLegacyTurnEnd.
		LegacyTurnEnds:  legacyTurnEnds,
		DaemonVersion:   "test",
		ProtocolVersion: "1",
		Logf:            logf,
	})
	if err != nil {
		t.Fatalf("sessioncontroller new: %v", err)
	}
	t.Cleanup(controller.Close)

	// The command handler needs the session-lifecycle binding, whose *Server
	// target does not exist until New below — bind it after (mirrors main).
	binding := &SessionCommandBinding{Logf: logf}
	handler, err := newCommandHandler(controller, stubMerge{}, stubLifecycle{}, controller, binding, nil, controller, logf)
	if err != nil {
		t.Fatalf("command handler: %v", err)
	}
	fe := frontend.New(frontend.Config{Logf: logf, LogVerbosef: logf, State: stubState{ssm: mgr}, Handler: handler})
	t.Cleanup(func() { _ = fe.Close() })

	cfg := extra
	cfg.Logf = logf
	cfg.Registry = reg
	cfg.Controller = controller
	cfg.SSM = mgr
	cfg.Frontend = fe
	if cfg.ModelCatalogs == nil {
		cfg.ModelCatalogs = NewSessionModelCatalogs()
	}
	srv := New(cfg)
	binding.SetTarget(srv)

	ts := httptest.NewServer(srv.Handler())
	t.Cleanup(ts.Close)

	return &harness{ts: ts, srv: srv, reg: reg, controller: controller, spawner: spawner, ssm: mgr, fe: fe, legacyTurnEnds: legacyTurnEnds}
}

// createSession brings a session up through the create CORE, the same entry
// the createSession FrontendCommand routes to now that POST /sessions is gone.
// body is the historical JSON request shape, kept because it reads compactly at
// the call sites; it decodes straight into CreateOpts.
func createSession(t *testing.T, h *harness, body string) string {
	t.Helper()
	id, err := createSessionErr(t, h, body)
	if err != nil {
		t.Fatalf("CreateSession(%s): %v", body, err)
	}
	return id
}

// markControllerOperational completes the fake harness's bring-up lifecycle.
// The harness deliberately has no shim connection, so tests whose precondition
// is an established route must close the exact generated controller edge
// themselves rather than reviving the retired legacy wiring projection.
func markControllerOperational(t *testing.T, h *harness, workspace string) {
	t.Helper()
	state, found, err := h.ssm.Composite(workspace)
	if err != nil || !found {
		t.Fatalf("Composite(%s) = (%+v, %t, %v), want connecting controller",
			workspace, state, found, err)
	}
	if err := h.ssm.Apply(&corev1.Event{
		SessionId: state.AgentReplSessionID,
		Seq:       1,
		Payload: &corev1.Event_SessionStarted{
			SessionStarted: &corev1.SessionStarted{},
		},
	}); err != nil {
		t.Fatalf("Apply SessionStarted(%s): %v", workspace, err)
	}
	if err := h.ssm.ApplySessionConnectivity(
		workspace,
		state.AgentReplSessionID,
		string(state.ControllerGenerationID),
		ssm.SessionConnectivityOperational,
		"test_shim_ready",
	); err != nil {
		t.Fatalf("ApplySessionConnectivity(%s operational): %v", workspace, err)
	}
}

// createSessionErr is createSession without the fatal, for the tests that
// assert on a typed create failure.
func createSessionErr(t *testing.T, h *harness, body string) (string, error) {
	t.Helper()
	var opts CreateOpts
	if err := json.Unmarshal([]byte(body), &opts); err != nil {
		t.Fatalf("decode create opts %s: %v", body, err)
	}
	return h.srv.CreateSession(context.Background(), opts)
}

// writeTranscript writes an empty transcript for uuid under cfg, at the fixed
// "/w" project slug the resume tests use.
func writeTranscript(t *testing.T, cfg, uuid string) {
	t.Helper()
	path := session.TranscriptPath(session.ClaudeConfigDir(cfg), "/w", uuid)
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatalf("mkdir transcript dir: %v", err)
	}
	if err := os.WriteFile(path, []byte(""), 0o600); err != nil {
		t.Fatalf("write transcript: %v", err)
	}
}

// frontendConn attaches a raw UDS frontend client to the harness's frontend
// server and returns a reader positioned AFTER the connect snapshot. Client
// registration and the snapshot enqueue are atomic under the Broadcast lock,
// so once the snapshot has been read, every later push reaches this reader.
// The 5s read deadline turns a missing expected push into a loud failure
// instead of a hung test.
func frontendConn(t *testing.T, h *harness) *bufio.Reader {
	t.Helper()
	dir, err := os.MkdirTemp("", "fes")
	if err != nil {
		t.Fatalf("mkdtemp: %v", err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(dir) })
	l, err := net.Listen("unix", filepath.Join(dir, "fe.sock"))
	if err != nil {
		t.Fatalf("listen unix: %v", err)
	}
	go func() { _ = h.fe.Serve(l) }()
	conn, err := net.Dial("unix", l.Addr().String())
	if err != nil {
		t.Fatalf("dial unix: %v", err)
	}
	t.Cleanup(func() { _ = conn.Close() })
	if err := conn.SetReadDeadline(time.Now().Add(5 * time.Second)); err != nil {
		t.Fatalf("set read deadline: %v", err)
	}
	r := bufio.NewReader(conn)
	nextPushedFrame(t, r) // the connect snapshot
	return r
}

// nextPushedFrame reads one newline-delimited protojson FrontendFrame.
func nextPushedFrame(t *testing.T, r *bufio.Reader) *frontendv1.FrontendFrame {
	t.Helper()
	line, err := r.ReadBytes('\n')
	if err != nil {
		t.Fatalf("read pushed frame: %v", err)
	}
	frame := &frontendv1.FrontendFrame{}
	if err := protojson.Unmarshal(line, frame); err != nil {
		t.Fatalf("unmarshal pushed frame: %v", err)
	}
	return frame
}

// nextSessionView reads pushed frames until a SessionView arrives, skipping
// unrelated interleaved frames (workspace states, progress).
func nextSessionView(t *testing.T, r *bufio.Reader) *frontendv1.SessionView {
	t.Helper()
	for range 32 {
		if v := nextPushedFrame(t, r).GetSessionView(); v != nil {
			return v
		}
	}
	t.Fatalf("no SessionView within 32 pushed frames")
	return nil
}

// --- Create ---------------------------------------------------------------

func TestCreateSessionReturnsAnSPrefixedID(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	id := createSession(t, h, `{"cwd":"/w"}`)
	// Assert
	if !strings.HasPrefix(id, "s_") {
		t.Errorf("session id = %q, want an s_ id", id)
	}
}

func TestCreateSessionRegistersARecord(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	id := createSession(t, h, `{"cwd":"/w","model":"haiku","config_dir":"/cfg"}`)
	// Assert — the record is the session controller's source of truth for this session.
	rec, ok := h.reg.Get(id)
	if !ok {
		t.Fatalf("no registry record for %s", id)
	}
	if rec.CWD != "/w" || rec.Model != "haiku" || rec.ConfigDir != "/cfg" {
		t.Errorf("record = %+v, want cwd/model/config_dir carried through", rec)
	}
}

func TestCreateSessionStoresThePlaceholderAsAnEmptyModel(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	id := createSession(t, h, `{"cwd":"/w","model":"<synthetic>"}`)
	// Assert — every create path reaches the shared core, so the marker cannot
	// enter durable session state even when the caller bypasses the frontend.
	rec, ok := h.reg.Get(id)
	if !ok {
		t.Fatalf("no registry record for %s", id)
	}
	if rec.Model != "" {
		t.Fatalf("record model = %q, want empty", rec.Model)
	}
}

func TestCreateSessionEagerlyBringsUpTheShim(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act — create resolves the workspace to the just-registered record and
	// asks the session controller to bring its shim up.
	id := createSession(t, h, `{"cwd":"/w"}`)
	// Assert — the session controller's spawner was asked to ensure exactly this session.
	if !slices.Contains(h.spawner.ensuredIDs(), id) {
		t.Fatalf("spawner ensured %v, want it to include %s", h.spawner.ensuredIDs(), id)
	}
}

// TestCreateSessionPushesTheHibernatedSessionViewWhenBringUpMeetsTheRevivalGate
// is the create half of the revival gate: bring-up finds the workspace's
// durable activity past the keep-alive policy's idle cutoff and hibernates the
// record instead of spawning, and the create still delivers the SessionView the
// gate is rendered from. Without that push the client waits for a SessionView
// that never comes and times out on a session that was created successfully.
func TestCreateSessionPushesTheHibernatedSessionViewWhenBringUpMeetsTheRevivalGate(t *testing.T) {
	// Arrange — a workspace that HAS been used (an earlier session left it a
	// settled, resolved state, which is what a restore finds) and whose only
	// dated activity is a day old: well past the six-hour idle cutoff the
	// default keep-alive policy carries.
	h := newHarness(t)
	createSession(t, h, `{"cwd":"/w"}`)
	markControllerOperational(t, h, "/w")
	h.legacyTurnEnds.atMs = time.Now().Add(-24 * time.Hour).UnixMilli()
	r := frontendConn(t, h)

	// Act.
	id, err := createSessionErr(t, h, `{"cwd":"/w"}`)

	// Assert — the record is durably asleep and its SessionView carries both
	// the flag and the account the gate names its cause from.
	if !errors.Is(err, errclass.ErrSessionHibernated) {
		t.Fatalf("CreateSession error = %v, want the revival gate's sentinel", err)
	}
	var view *frontendv1.SessionView
	for range 8 {
		if v := nextSessionView(t, r); v.GetSessionId() == id {
			view = v
			break
		}
	}
	if view == nil {
		t.Fatalf("no SessionView pushed for the hibernated session %s", id)
	}
	if !view.GetHibernated() || view.GetHibernation().GetIdleCutoff() == nil {
		t.Fatalf("SessionView = %+v, want hibernated with the idle-cutoff account", view)
	}
}

// --- Create supersede (one live session per workspace) ---------------------
//
// The controller runs one session per cwd and the locator resolves a cwd to its
// newest non-terminal record, so a create leaves every older record on its
// cwd unreachable-but-live: a leaked shim, and a stale roster entry frontends
// then mis-correlate the cwd to (the every-restore mis-bind of 2026-07-26).
// A create therefore stands its cwd's older records down and SAYS so.

func TestCreateSupersedesTheWorkspacesOlderSession(t *testing.T) {
	// Arrange: a workspace already holding a live resume-less session.
	h := newHarness(t)
	old := createSession(t, h, `{"cwd":"/w"}`)
	// Act: a newer create claims the same workspace.
	newer := createSession(t, h, `{"cwd":"/w"}`)
	// Assert: the older record is terminal, as a planned handover.
	rec, _ := h.reg.Get(old)
	if !rec.Terminal || rec.DeathReason != supersedeReason {
		t.Errorf("old record = %+v, want terminal with the supersede death reason", rec)
	}
	if newRec, _ := h.reg.Get(newer); newRec.Terminal {
		t.Errorf("new record = %+v, want it live", newRec)
	}
}

func TestCreateSupersedeStopsTheOlderSessionsShim(t *testing.T) {
	// Arrange: the older session's shim is live (create brings it up eagerly).
	h := newHarness(t)
	old := createSession(t, h, `{"cwd":"/w"}`)
	// Act
	_ = createSession(t, h, `{"cwd":"/w"}`)
	// Assert: the displaced shim was stopped, not left running unattached.
	if !slices.Contains(h.spawner.stoppedIDs(), old) {
		t.Errorf("stopped = %v, want it to contain the displaced %s", h.spawner.stoppedIDs(), old)
	}
}

func TestCreateLeavesOtherWorkspacesSessionsAlone(t *testing.T) {
	// Arrange
	h := newHarness(t)
	other := createSession(t, h, `{"cwd":"/other"}`)
	// Act
	_ = createSession(t, h, `{"cwd":"/w"}`)
	// Assert: only same-cwd records contend; a different workspace is not.
	if rec, _ := h.reg.Get(other); rec.Terminal {
		t.Errorf("other-workspace record = %+v, want it untouched", rec)
	}
}

func TestCreateSupersedesTheOlderResumeOfATranscript(t *testing.T) {
	// Arrange: two resumes of one uuid in one cwd — the single-writer case.
	h := newHarness(t)
	cfg := t.TempDir()
	writeTranscript(t, cfg, "u-1")
	old := createSession(t, h, fmt.Sprintf(`{"cwd":"/w","config_dir":%q,"resume":"u-1"}`, cfg))
	// Act
	_ = createSession(t, h, fmt.Sprintf(`{"cwd":"/w","config_dir":%q,"resume":"u-1"}`, cfg))
	// Assert: the older writer stood down.
	rec, _ := h.reg.Get(old)
	if !rec.Terminal || rec.DeathReason != supersedeReason {
		t.Errorf("old record = %+v, want terminal with the supersede death reason", rec)
	}
}

func TestCreatePushesTheSupersededSessionsTerminalView(t *testing.T) {
	// Arrange: a connected frontend whose roster lists the old session live.
	h := newHarness(t)
	old := createSession(t, h, `{"cwd":"/w"}`)
	r := frontendConn(t, h)
	// Act
	newer := createSession(t, h, `{"cwd":"/w"}`)
	// Assert: the stand-down is DELIVERED, and before the new session's view —
	// so the roster never holds two live sessions for the cwd, which is the
	// invariant the frontends' cwd->id create correlation keys on.
	v := nextSessionView(t, r)
	if v.GetSessionId() != old || !v.GetTerminal() {
		t.Fatalf("first pushed view = %s (terminal=%v), want terminal %s", v.GetSessionId(), v.GetTerminal(), old)
	}
	v = nextSessionView(t, r)
	if v.GetSessionId() != newer || v.GetTerminal() {
		t.Fatalf("second pushed view = %s (terminal=%v), want live %s", v.GetSessionId(), v.GetTerminal(), newer)
	}
}

func TestCreateSessionRejectsInvalidPermissionMode(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	_, err := createSessionErr(t, h, `{"cwd":"/w","permission_mode":"nonsense"}`)
	// Assert — the typed rejection the command ack surfaces to the frontend.
	var invalid *InvalidCreateError
	if !errors.As(err, &invalid) {
		t.Fatalf("err = %v, want an *InvalidCreateError", err)
	}
}

func TestCreateSessionRejectsUngatedModeWithoutConsent(t *testing.T) {
	// Arrange — bypassPermissions leaves the session with no permission gate
	// at all, and it is one string away from every ordinary create.
	h := newHarness(t)
	// Act
	_, err := createSessionErr(t, h, `{"cwd":"/w","permission_mode":"bypassPermissions"}`)
	// Assert — refused loudly, never downgraded to a gated mode.
	var invalid *InvalidCreateError
	if !errors.As(err, &invalid) {
		t.Fatalf("err = %v, want an *InvalidCreateError", err)
	}
}

func TestCreateSessionRejectionNamesTheMissingConsent(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	_, err := createSessionErr(t, h, `{"cwd":"/w","permission_mode":"bypassPermissions"}`)
	// Assert — the caller must be able to learn what to set from the message.
	if err == nil || !strings.Contains(err.Error(), "allow_ungated") {
		t.Fatalf("err = %v, want it to name allow_ungated", err)
	}
}

func TestCreateSessionRegistersNoRecordForARefusedUngatedCreate(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	_, _ = createSessionErr(t, h, `{"cwd":"/w","permission_mode":"bypassPermissions"}`)
	// Assert — the refusal precedes registration, so no half-created session
	// is left behind for a later rehydration to bring up ungated.
	if got := len(h.reg.All()); got != 0 {
		t.Fatalf("registry holds %d records, want 0 after a refused create", got)
	}
}

func TestCreateSessionAllowsUngatedModeWithExplicitConsent(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	id, err := createSessionErr(t, h, `{"cwd":"/w","permission_mode":"bypassPermissions","allow_ungated":true}`)
	// Assert — the mode is legitimate, it just may not be reached by accident.
	if err != nil {
		t.Fatalf("CreateSession with allow_ungated: %v", err)
	}
	rec, ok := h.reg.Get(id)
	if !ok || rec.PermissionMode != "bypassPermissions" {
		t.Fatalf("record = %+v (ok=%v), want the ungated mode carried through", rec, ok)
	}
}

func TestCreateSessionNeedsNoConsentForAGatedMode(t *testing.T) {
	// Arrange — dontAsk also bypasses canUseTool, but fail-CLOSED, so it
	// grants nothing behind the gate's back and takes no consent.
	h := newHarness(t)
	// Act
	_, err := createSessionErr(t, h, `{"cwd":"/w","permission_mode":"dontAsk"}`)
	// Assert
	if err != nil {
		t.Fatalf("CreateSession(dontAsk): %v", err)
	}
}

func TestCreateSessionHardFailsUnresumableResume(t *testing.T) {
	// Arrange — a resume target whose transcript does not exist.
	h := newHarness(t)
	cfg := t.TempDir()
	// Act
	_, err := createSessionErr(t, h, fmt.Sprintf(`{"cwd":"/w","config_dir":%q,"resume":"gone-uuid"}`, cfg))
	// Assert — a typed hard failure naming the resume target, not a silent
	// fresh start.
	var missing *ResumeTranscriptMissingError
	if !errors.As(err, &missing) {
		t.Fatalf("err = %v, want a *ResumeTranscriptMissingError", err)
	}
	if missing.ResumeID != "gone-uuid" {
		t.Errorf("ResumeID = %q, want gone-uuid", missing.ResumeID)
	}
	if len(missing.SearchedPaths) == 0 {
		t.Error("SearchedPaths is empty, want every path stat'd")
	}
}

func TestCreateSessionKeepsResumableResume(t *testing.T) {
	// Arrange — a resume target WITH a transcript on disk.
	h := newHarness(t)
	cfg := t.TempDir()
	writeTranscript(t, cfg, "uuid-1")
	// Act
	id := createSession(t, h, fmt.Sprintf(`{"cwd":"/w","config_dir":%q,"resume":"uuid-1"}`, cfg))
	// Assert — the record carries the resume id as its claude_session_id.
	rec, _ := h.reg.Get(id)
	if rec.ClaudeSessionID != "uuid-1" {
		t.Errorf("claude_session_id = %q, want uuid-1", rec.ClaudeSessionID)
	}
}

func TestCreateSessionReturnsTheDurableIDWithAResumeBringUpFailure(t *testing.T) {
	// Arrange — resume validation succeeds and registration completes, but the
	// shim spawn fails after the durable identity exists.
	h := newHarness(t)
	cfg := t.TempDir()
	writeTranscript(t, cfg, "uuid-1")
	spawnFailure := errors.New("spawn refused the resumed query")
	h.spawner.err = spawnFailure

	// Act.
	id, err := createSessionErr(t, h, fmt.Sprintf(`{"cwd":"/w","config_dir":%q,"resume":"uuid-1"}`, cfg))

	// Assert — the command boundary needs this id to identify the failed exact
	// resume; an error cannot erase a record that was already registered.
	if !errors.Is(err, spawnFailure) {
		t.Fatalf("CreateSession error = %v, want original spawn failure", err)
	}
	if id == "" {
		t.Fatal("CreateSession id is empty after durable registration")
	}
	rec, ok := h.reg.Get(id)
	if !ok || rec.SessionID != id || rec.ClaudeSessionID != "uuid-1" || rec.CWD != "/w" || rec.ConfigDir != cfg {
		t.Fatalf("registered record = %+v (ok=%t), want failed resume identity for %q", rec, ok, id)
	}
}

// --- List -----------------------------------------------------------------

func getList(t *testing.T, h *harness) map[string]any {
	t.Helper()
	resp, err := http.Get(h.ts.URL + "/sessions")
	if err != nil {
		t.Fatalf("GET /sessions: %v", err)
	}
	defer resp.Body.Close()
	var out map[string]any
	if err := json.NewDecoder(resp.Body).Decode(&out); err != nil {
		t.Fatalf("decode list: %v", err)
	}
	return out
}

func TestListSessionsEnvelopeCarriesBootIdentityAndVersion(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	out := getList(t, h)
	// Assert
	boot, _ := out["boot_id"].(string)
	if !strings.HasPrefix(boot, "b_") {
		t.Errorf("boot_id = %q, want a b_ id", boot)
	}
	if pv, _ := out["protocol_version"].(float64); int(pv) != 2 {
		t.Errorf("protocol_version = %v, want 2", out["protocol_version"])
	}
	if _, ok := out["daemon_binary_mtime"]; !ok {
		t.Error("list envelope missing daemon_binary_mtime")
	}
}

func TestListSessionsIncludesCreatedSession(t *testing.T) {
	// Arrange
	h := newHarness(t)
	id := createSession(t, h, `{"cwd":"/w"}`)
	// Act
	out := getList(t, h)
	// Assert — the created session appears, non-terminal.
	sessions, _ := out["sessions"].([]any)
	found := false
	for _, raw := range sessions {
		m, _ := raw.(map[string]any)
		if m["session_id"] == id {
			found = true
			if m["terminal"] == true {
				t.Error("freshly created session should not be terminal")
			}
		}
	}
	if !found {
		t.Fatalf("created session %s not in list", id)
	}
}

func TestListSessionsEnvelopeReportsBinaryMTime(t *testing.T) {
	// Arrange
	h := newHarnessWith(t, Config{BinaryMTime: 12345})
	// Act
	out := getList(t, h)
	// Assert
	if mt, _ := out["daemon_binary_mtime"].(float64); int64(mt) != 12345 {
		t.Errorf("daemon_binary_mtime = %v, want 12345", out["daemon_binary_mtime"])
	}
}

// --- DaemonView / SessionView shaping (S7) --------------------------------

func TestDaemonViewCarriesIdentity(t *testing.T) {
	// Arrange — a Server with a known version + binary mtime (seconds).
	srv := New(Config{DaemonVersion: "v9", BinaryMTime: 5, Logf: func(string, ...any) {}, ModelCatalogs: NewSessionModelCatalogs()})
	// Act
	dv := srv.DaemonView()
	// Assert — boot id, the frontend.v1 protocol version "1", mtime in millis.
	if !strings.HasPrefix(dv.GetBootId(), "b_") {
		t.Errorf("boot_id = %q, want a b_ id", dv.GetBootId())
	}
	if dv.GetProtocolVersion() != "1" {
		t.Errorf("protocol_version = %q, want 1", dv.GetProtocolVersion())
	}
	if dv.GetDaemonBinaryMtimeMs() != 5000 {
		t.Errorf("daemon_binary_mtime_ms = %d, want 5000 (seconds*1000)", dv.GetDaemonBinaryMtimeMs())
	}
	if dv.GetDaemonVersion() != "v9" {
		t.Errorf("daemon_version = %q, want v9", dv.GetDaemonVersion())
	}
}

func TestSessionViewFromRecordShapesParityFields(t *testing.T) {
	// Arrange — a terminal record with a death reason plus two pending perms.
	rec := registry.Record{
		SessionID:       "s1",
		CWD:             "/w",
		Model:           "sonnet",
		PermissionMode:  "plan",
		ClaudeSessionID: "cli-1",
		Terminal:        true,
		DeathReason:     "delete session",
	}
	// Act
	v := SessionViewFromRecord(nil, rec, []string{"p1", "p2"}, false)
	// Assert — the S7 parity fields plus the pending-permission COUNT.
	if !v.GetTerminal() || v.GetDeath().GetErrorType() != string(errclass.TypeSessionDeleted) {
		t.Errorf("terminal/death = %v/%q", v.GetTerminal(), v.GetDeath().GetErrorType())
	}
	if v.GetPendingPermissions() != 2 {
		t.Errorf("pending_permissions = %d, want 2", v.GetPendingPermissions())
	}
	if v.GetWorkspace() != "/w" || v.GetSessionId() != "s1" || v.GetModel() != "sonnet" {
		t.Errorf("core fields = %+v", v)
	}
	if v.GetRehydratable() || v.GetHibernated() {
		t.Errorf("rehydratable/hibernated should stay false post-cutover")
	}
}

// --- SessionView.death (F4) -------------------------------------------------
//
// death_reason had two producers and ZERO readers, because a frontend could
// not tell what class of failure the string described. These assert the typed
// counterpart, one edge case per test.

func TestSessionViewClassifiesADeletedSession(t *testing.T) {
	// Arrange.
	rec := registry.Record{SessionID: "s1", CWD: "/w", Terminal: true, DeathReason: errclass.DeathReasonDeleted}

	// Act.
	v := SessionViewFromRecord(nil, rec, nil, false)

	// Assert.
	if got := v.GetDeath().GetErrorType(); got != string(errclass.TypeSessionDeleted) {
		t.Fatalf("death error_type = %q, want %q", got, errclass.TypeSessionDeleted)
	}
}

func TestSessionViewClassifiesASupersededSession(t *testing.T) {
	// Arrange.
	rec := registry.Record{SessionID: "s1", CWD: "/w", Terminal: true, DeathReason: errclass.DeathReasonSuperseded}

	// Act.
	v := SessionViewFromRecord(nil, rec, nil, false)

	// Assert.
	if got := v.GetDeath().GetErrorType(); got != string(errclass.TypeSessionSuperseded) {
		t.Fatalf("death error_type = %q, want %q", got, errclass.TypeSessionSuperseded)
	}
}

func TestSessionViewClassifiesAShimDeath(t *testing.T) {
	// Arrange: the reason the registry documented and nothing ever wrote,
	// until the session controller's SessionEnded hook.
	rec := registry.Record{SessionID: "s1", CWD: "/w", Terminal: true, DeathReason: errclass.DeathReasonShimDied}

	// Act.
	v := SessionViewFromRecord(nil, rec, nil, false)

	// Assert.
	if got := v.GetDeath().GetErrorType(); got != string(errclass.TypeSessionShimDied) {
		t.Fatalf("death error_type = %q, want %q", got, errclass.TypeSessionShimDied)
	}
}

func TestSessionViewClassifiesALegacyDeathReasonLoudly(t *testing.T) {
	// Arrange: a registry carried over from a build predating the vocabulary.
	var logged []string
	logf := func(format string, args ...any) { logged = append(logged, fmt.Sprintf(format, args...)) }
	rec := registry.Record{SessionID: "s1", CWD: "/w", Terminal: true, DeathReason: "some ancient reason"}

	// Act.
	v := SessionViewFromRecord(logf, rec, nil, false)

	// Assert.
	if got := v.GetDeath().GetErrorType(); got != string(errclass.TypeSessionEndedUnclassified) {
		t.Fatalf("death error_type = %q, want %q", got, errclass.TypeSessionEndedUnclassified)
	}
	if len(logged) == 0 {
		t.Fatal("an unknown death reason passed SILENTLY; the default must be loud")
	}
}

func TestSessionViewCarriesNoDeathWhileTheSessionLives(t *testing.T) {
	// Arrange: a live session has not died, so it has nothing to explain.
	rec := registry.Record{SessionID: "s1", CWD: "/w"}

	// Act.
	v := SessionViewFromRecord(nil, rec, nil, false)

	// Assert.
	if v.GetDeath() != nil {
		t.Fatalf("death = %v, want nil for a live session", v.GetDeath())
	}
}

func TestSessionViewFromRecordCarriesConfigDir(t *testing.T) {
	// Arrange — a record whose shim runs under a non-default account root.
	rec := registry.Record{SessionID: "s1", CWD: "/w", ConfigDir: "/cfg-work"}

	// Act.
	v := SessionViewFromRecord(nil, rec, nil, false)

	// Assert — the account root rides on the SessionView (S8).
	if v.GetConfigDir() != "/cfg-work" {
		t.Fatalf("config_dir = %q, want /cfg-work", v.GetConfigDir())
	}
}

func TestSessionViewCarriesTheBackfillState(t *testing.T) {
	// Arrange / Act / Assert — the never-blue signal rides every SessionView,
	// so it reaches a reconnecting frontend on the connect snapshot (F2).
	cases := []struct {
		stored string
		want   frontendv1.BackfillState
	}{
		{"", frontendv1.BackfillState_BACKFILL_STATE_UNSPECIFIED},
		{sessioncontroller.BackfillPending, frontendv1.BackfillState_BACKFILL_STATE_PENDING},
		{sessioncontroller.BackfillDone, frontendv1.BackfillState_BACKFILL_STATE_DONE},
		{sessioncontroller.BackfillFailed, frontendv1.BackfillState_BACKFILL_STATE_FAILED},
	}
	for _, c := range cases {
		rec := registry.Record{SessionID: "s1", CWD: "/w", BackfillState: c.stored}
		if got := SessionViewFromRecord(nil, rec, nil, false).GetBackfill(); got != c.want {
			t.Fatalf("stored %q -> %v, want %v", c.stored, got, c.want)
		}
	}
}

func TestAnUnrecognizedBackfillTokenReadsAsUnspecified(t *testing.T) {
	// Arrange — a token from a newer daemon, or a corrupted record.
	rec := registry.Record{SessionID: "s1", CWD: "/w", BackfillState: "teleporting"}

	// Act
	got := SessionViewFromRecord(nil, rec, nil, false).GetBackfill()

	// Assert — UNSPECIFIED is the SAFE direction: it makes the switch-ensure
	// retry rather than skip, so an unreadable token cannot leave a workspace
	// blue by being mistaken for DONE.
	if got != frontendv1.BackfillState_BACKFILL_STATE_UNSPECIFIED {
		t.Fatalf("backfill = %v, want UNSPECIFIED for an unknown token", got)
	}
}

// --- Account switch (S8: webapp-initiated, daemon-executed) ----------------

// accountRoster is the two-account roster the switch tests use.
func accountRoster() []Account {
	return []Account{{Label: "personal", ConfigDir: ""}, {Label: "work", ConfigDir: "/cfg-work"}}
}

// postAccountSwitch POSTs a switch to configDir and returns the response.
func postAccountSwitch(t *testing.T, h *harness, id, configDir string) *http.Response {
	t.Helper()
	resp, err := http.Post(h.ts.URL+"/sessions/"+id+"/account", "application/json",
		bytes.NewBufferString(fmt.Sprintf(`{"config_dir":%q}`, configDir)))
	if err != nil {
		t.Fatalf("POST account: %v", err)
	}
	return resp
}

func TestAccountSwitchGuardsTurnActive(t *testing.T) {
	// Arrange — a session with a turn in flight (the SSM guard).
	h := newHarnessWith(t, Config{Accounts: accountRoster()})
	id := createSession(t, h, `{"cwd":"/w"}`)
	markControllerOperational(t, h, "/w")
	if err := h.ssm.Apply(&corev1.Event{
		SessionId: id,
		Seq:       2,
		Plane:     corev1.Plane_PLANE_STREAM,
		RequestId: "turn-1",
		Payload: &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{
			PromptPreview: "go",
			TurnId:        "turn-1",
		}},
	}); err != nil {
		t.Fatalf("apply turn started: %v", err)
	}

	// Act.
	resp := postAccountSwitch(t, h, id, "/cfg-work")
	defer resp.Body.Close()

	// Assert — 409 and the account is unchanged: a turn is never interrupted by
	// a shim restart.
	if resp.StatusCode != http.StatusConflict {
		t.Fatalf("status = %d, want 409 (turn in flight)", resp.StatusCode)
	}
	rec, _ := h.reg.Get(id)
	if rec.ConfigDir != "" {
		t.Fatalf("config_dir = %q, want unchanged while turn active", rec.ConfigDir)
	}
}

func TestAccountSwitchUpdatesConfigDirAndRespawns(t *testing.T) {
	// Arrange — an idle session under the default account.
	h := newHarnessWith(t, Config{Accounts: accountRoster()})
	id := createSession(t, h, `{"cwd":"/w"}`)

	// Act.
	resp := postAccountSwitch(t, h, id, "/cfg-work")
	defer resp.Body.Close()

	// Assert — 202, the registry root is updated, and the shim was re-ensured
	// under the new root (a fresh UDS spawn with --resume).
	if resp.StatusCode != http.StatusAccepted {
		t.Fatalf("status = %d, want 202", resp.StatusCode)
	}
	rec, _ := h.reg.Get(id)
	if rec.ConfigDir != "/cfg-work" {
		t.Fatalf("registry config_dir = %q, want /cfg-work", rec.ConfigDir)
	}
	if n := len(h.spawner.ensuredIDs()); n < 2 {
		t.Fatalf("EnsureShim calls = %d, want >=2 (create + account-switch respawn)", n)
	}
}

func TestAccountSwitchPushesSessionViewWithNewConfigDir(t *testing.T) {
	// Arrange — a session created under the default account, plus a frontend
	// client connected AFTER create so it only observes the switch's push.
	h := newHarnessWith(t, Config{Accounts: accountRoster()})
	id := createSession(t, h, `{"cwd":"/w"}`)
	r := frontendConn(t, h)

	// Act — the switch pushes the updated SessionView synchronously before the
	// 202 response returns.
	resp := postAccountSwitch(t, h, id, "/cfg-work")
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusAccepted {
		t.Fatalf("status = %d, want 202", resp.StatusCode)
	}

	// Assert — a SessionView carrying the switched config_dir reaches the client.
	view := nextSessionView(t, r)
	if view.GetSessionId() != id {
		t.Fatalf("pushed SessionView id = %q, want %q", view.GetSessionId(), id)
	}
	if view.GetConfigDir() != "/cfg-work" {
		t.Fatalf("pushed SessionView config_dir = %q, want /cfg-work", view.GetConfigDir())
	}
}

// --- Delete (S7: the DELETE /sessions/{id} HTTP route was removed; the
// deleteSession UDS command drives s.DeleteSession, tested directly here) -----

func TestDeleteSessionMarksRecordTerminal(t *testing.T) {
	// Arrange
	h := newHarness(t)
	id := createSession(t, h, `{"cwd":"/w"}`)
	// Act
	if err := h.srv.DeleteSession(id); err != nil {
		t.Fatalf("DeleteSession: %v", err)
	}
	// Assert
	rec, _ := h.reg.Get(id)
	if !rec.Terminal || rec.DeathReason != "delete session" {
		t.Errorf("record = %+v, want terminal with the delete death reason", rec)
	}
}

func TestDeleteSessionRepushesAnAlreadyTerminalRecord(t *testing.T) {
	// Arrange: a superseded record whose stale client still believes it is live.
	h := newHarness(t)
	rec := registry.Record{
		SessionID: "s_old", CWD: "/w", ClaudeSessionID: "vendor-1",
		Terminal: true, DeathReason: errclass.DeathReasonSuperseded,
	}
	if err := h.reg.Put(rec); err != nil {
		t.Fatalf("registry Put: %v", err)
	}
	r := frontendConn(t, h)

	// Act.
	if err := h.srv.DeleteSession(rec.SessionID); err != nil {
		t.Fatalf("DeleteSession: %v", err)
	}

	// Assert: idempotence preserves the first death reason while repairing the
	// client roster and retrying the exact evicted session's process handle.
	view := nextSessionView(t, r)
	if view.GetSessionId() != rec.SessionID {
		t.Fatalf("pushed SessionView id = %q, want %q", view.GetSessionId(), rec.SessionID)
	}
	if !view.GetTerminal() || view.GetDeath().GetErrorType() != string(errclass.TypeSessionSuperseded) {
		t.Fatalf("pushed view = %+v, want terminal superseded", view)
	}
	got, _ := h.reg.Get(rec.SessionID)
	if got.DeathReason != errclass.DeathReasonSuperseded {
		t.Fatalf("death reason = %q, want first reason preserved", got.DeathReason)
	}
	if !slices.Contains(h.spawner.stoppedIDs(), rec.SessionID) {
		t.Fatalf("stopped = %v, want %s", h.spawner.stoppedIDs(), rec.SessionID)
	}
}

// Several records can still share one cwd — create now supersedes its cwd's
// older records, but junk minted by older daemons (and direct registry writes)
// survives in the persisted registry. Deleting one of them must stop ITS shim
// and only its shim: the workspace-keyed teardown this used to do SIGTERMed
// whichever shim was live, so on 2026-07-25 deleting a stale duplicate killed
// the healthy session created 175ms earlier and every prompt after it NACKed
// as "no live session".
func TestDeleteSessionDoesNotStopADifferentLiveSession(t *testing.T) {
	// Arrange: the session that drives the cwd, then a stale non-terminal
	// record sharing it — written directly, the way an older daemon left it
	// (a create would stand it down at mint time now).
	h := newHarness(t)
	live := createSession(t, h, `{"cwd":"/w"}`)
	if err := h.reg.Put(registry.Record{SessionID: "s_stale", CWD: "/w"}); err != nil {
		t.Fatalf("put stale record: %v", err)
	}

	// Act: delete the stale record.
	if err := h.srv.DeleteSession("s_stale"); err != nil {
		t.Fatalf("DeleteSession: %v", err)
	}

	// Assert: the live session's shim was never stopped.
	if slices.Contains(h.spawner.stoppedIDs(), live) {
		t.Fatalf("deleting stale s_stale stopped live session %s (stopped=%v)",
			live, h.spawner.stoppedIDs())
	}
}

func TestDeleteSessionOfATerminalRecordRepushesItsTerminalView(t *testing.T) {
	// Arrange: a session already deleted once. A frontend that retries the
	// delete believes the session is still live — its roster missed the first
	// terminal transition — and the old silent no-op left it retrying forever
	// (the observed 15s delete loop).
	h := newHarness(t)
	id := createSession(t, h, `{"cwd":"/w"}`)
	if err := h.srv.DeleteSession(id); err != nil {
		t.Fatalf("first DeleteSession: %v", err)
	}
	r := frontendConn(t, h)

	// Act: the idempotent retry.
	if err := h.srv.DeleteSession(id); err != nil {
		t.Fatalf("second DeleteSession: %v", err)
	}

	// Assert: the retry re-delivers the terminal view instead of going silent.
	v := nextSessionView(t, r)
	if v.GetSessionId() != id || !v.GetTerminal() {
		t.Fatalf("pushed view = %s (terminal=%v), want terminal %s", v.GetSessionId(), v.GetTerminal(), id)
	}
}

func TestDeleteSessionStopsItsOwnLiveShim(t *testing.T) {
	// Arrange: one session, live for its cwd.
	h := newHarness(t)
	id := createSession(t, h, `{"cwd":"/w"}`)

	// Act
	if err := h.srv.DeleteSession(id); err != nil {
		t.Fatalf("DeleteSession: %v", err)
	}

	// Assert: session-scoping did not cost the delete its own teardown.
	if !slices.Contains(h.spawner.stoppedIDs(), id) {
		t.Fatalf("stopped = %v, want it to contain %s", h.spawner.stoppedIDs(), id)
	}
}

func TestDeleteUnknownSessionErrors(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act / Assert — an unknown id reports the sentinel not-found error.
	if err := h.srv.DeleteSession("s_nope"); !errors.Is(err, errSessionNotFound) {
		t.Errorf("err = %v, want errSessionNotFound", err)
	}
}

// --- ShimArgv / ShimEnv (pure) --------------------------------------------

func TestShimArgvAssemblesAllCreateOpts(t *testing.T) {
	// Arrange / Act
	got := ShimArgv("node", "shim.js", "s1", false,
		CreateOpts{CWD: "/w", Model: "haiku", PermissionMode: "plan", Resume: "uuid-1"})
	// Assert
	want := []string{"node", "shim.js", "--session-id", "s1", "--permission-mode", "plan",
		"--cwd", "/w", "--model", "haiku", "--resume", "uuid-1"}
	if !slices.Equal(got, want) {
		t.Fatalf("argv = %v, want %v", got, want)
	}
}

func TestShimArgvTreatsThePlaceholderLikeAnEmptyModel(t *testing.T) {
	// Arrange / Act
	got := ShimArgv("node", "shim.js", "s1", false, CreateOpts{Model: "<synthetic>"})
	// Assert
	if slices.Contains(got, "--model") {
		t.Fatalf("argv = %v, want no --model for the empty-equivalent marker", got)
	}
}

func TestShimArgvForcesFake(t *testing.T) {
	// Arrange / Act
	got := ShimArgv("node", "shim.js", "s1", true, CreateOpts{})
	// Assert
	if !slices.Contains(got, "--fake") {
		t.Fatalf("argv = %v, want it to carry --fake", got)
	}
}

func TestShimEnvAlwaysMarksOwnership(t *testing.T) {
	if !slices.Contains(ShimEnv(CreateOpts{}, ""), "AGENT_REPL_OWNED=1") {
		t.Fatal("ShimEnv must always mark ownership")
	}
}

func TestShimEnvExportsSessionConfigDir(t *testing.T) {
	if !slices.Contains(ShimEnv(CreateOpts{ConfigDir: "/cfg"}, ""), "CLAUDE_CONFIG_DIR=/cfg") {
		t.Fatal("ShimEnv must export the session's config dir")
	}
}

func TestShimEnvOmitsConfigDirWhenUnset(t *testing.T) {
	for _, kv := range ShimEnv(CreateOpts{}, "") {
		if strings.HasPrefix(kv, "CLAUDE_CONFIG_DIR=") {
			t.Fatalf("ShimEnv exported %q for an unset config dir", kv)
		}
	}
}

func TestShimEnvExportsDaemonAddr(t *testing.T) {
	if !slices.Contains(ShimEnv(CreateOpts{}, "127.0.0.1:9999"), "AGENT_REPL_DAEMON_ADDR=127.0.0.1:9999") {
		t.Fatal("ShimEnv must export the daemon addr when set")
	}
}

func TestShimEnvOmitsDaemonAddrWhenEmpty(t *testing.T) {
	for _, kv := range ShimEnv(CreateOpts{}, "") {
		if strings.HasPrefix(kv, "AGENT_REPL_DAEMON_ADDR=") {
			t.Fatalf("ShimEnv exported %q for an empty daemon addr", kv)
		}
	}
}

// --- chessGamePath (pure) -------------------------------------------------

func TestChessGamePathValidation(t *testing.T) {
	tests := []struct {
		name    string
		cwd     string
		raw     string
		wantErr bool
	}{
		{"valid", "/w", "/w/.claude/emacs/cee-web-widget/chess-game-1.json", false},
		{"outside dir", "/w", "/w/.claude/emacs/chess-game-1.json", true},
		{"wrong prefix", "/w", "/w/.claude/emacs/cee-web-widget/other.json", true},
		{"traversal", "/w", "/w/.claude/emacs/cee-web-widget/../../../etc/passwd", true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			_, err := chessGamePath(tt.cwd, tt.raw)
			if (err != nil) != tt.wantErr {
				t.Fatalf("chessGamePath err = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

// --- Capabilities ---------------------------------------------------------

func getCapabilities(t *testing.T, h *harness) map[string]any {
	t.Helper()
	resp, err := http.Get(h.ts.URL + "/capabilities")
	if err != nil {
		t.Fatalf("GET /capabilities: %v", err)
	}
	defer resp.Body.Close()
	var out map[string]any
	if err := json.NewDecoder(resp.Body).Decode(&out); err != nil {
		t.Fatalf("decode: %v", err)
	}
	return out
}

func TestCapabilitiesReportsWidgetAssetsOffWhenUnconfigured(t *testing.T) {
	h := newHarness(t)
	if getCapabilities(t, h)["widget_assets"] != false {
		t.Error("widget_assets should be false when unconfigured")
	}
}

func TestCapabilitiesReportsWidgetBundlePresent(t *testing.T) {
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, "chess-widget.js"), []byte("//"), 0o600); err != nil {
		t.Fatalf("write bundle: %v", err)
	}
	h := newHarnessWith(t, Config{WidgetAssetsDir: dir})
	out := getCapabilities(t, h)
	if out["widget_assets"] != true || out["widget_bundle_present"] != true {
		t.Errorf("capabilities = %v, want assets on and bundle present", out)
	}
}

func TestCapabilitiesReportsBundleMissingWhenDistLacksIt(t *testing.T) {
	h := newHarnessWith(t, Config{WidgetAssetsDir: t.TempDir()})
	out := getCapabilities(t, h)
	if out["widget_assets"] != true || out["widget_bundle_present"] != false {
		t.Errorf("capabilities = %v, want assets on but bundle missing", out)
	}
}

// The POST /shutdown tests were deleted with the route: the graceful-teardown
// capability is covered on its surviving surface by
// TestCommandHandlerShutdownRoutesToShutdownFunc and
// TestCommandHandlerShutdownUnconfiguredErrors in frontendcmd_test.go.

// --- Unknown-session routing ----------------------------------------------

func TestUnknownSessionRoutesReturn404(t *testing.T) {
	h := newHarness(t)
	for _, path := range []string{
		"/sessions/s_nope/account",
	} {
		resp, err := http.Get(h.ts.URL + path)
		if err != nil {
			t.Fatalf("GET %s: %v", path, err)
		}
		if resp.StatusCode != http.StatusNotFound {
			t.Errorf("GET %s status = %d, want 404", path, resp.StatusCode)
		}
		resp.Body.Close()
	}
}

// --- frontendCommandTranslator (command-strict, S9) ------------------------

// TestFrontendCommandTranslatorRoutesEachCommand proves every FrontendCommand
// the webapp sends over its /stream WebSocket decodes to the SAME command the
// Emacs UDS surface routes: submit/interrupt/permission-answer/create/delete/
// resync/shutdown all pass through the command-strict translator unchanged.
func TestFrontendCommandTranslatorRoutesEachCommand(t *testing.T) {
	h := newHarness(t)
	tr := h.srv.frontendCommandTranslator("/w")
	tests := []struct {
		name string
		cmd  *frontendv1.FrontendCommand
		ok   func(*frontendv1.FrontendCommand) bool
	}{
		{"submit", &frontendv1.FrontendCommand{RequestId: "r1", Command: &frontendv1.FrontendCommand_SubmitPrompt{SubmitPrompt: &frontendv1.SubmitPromptCmd{Text: "hi"}}}, func(c *frontendv1.FrontendCommand) bool { return c.GetSubmitPrompt().GetText() == "hi" }},
		{"interrupt", &frontendv1.FrontendCommand{RequestId: "r2", Command: &frontendv1.FrontendCommand_Interrupt{Interrupt: &frontendv1.InterruptCmd{ConfirmAgents: true}}}, func(c *frontendv1.FrontendCommand) bool { return c.GetInterrupt().GetConfirmAgents() }},
		{"permission-answer", &frontendv1.FrontendCommand{RequestId: "r3", Command: &frontendv1.FrontendCommand_PermissionAnswer{PermissionAnswer: &frontendv1.PermissionAnswerCmd{PermissionRequestId: "perm-1", Allow: true}}}, func(c *frontendv1.FrontendCommand) bool {
			return c.GetPermissionAnswer().GetPermissionRequestId() == "perm-1" && c.GetPermissionAnswer().GetAllow()
		}},
		{"create", &frontendv1.FrontendCommand{RequestId: "r4", Command: &frontendv1.FrontendCommand_CreateSession{CreateSession: &frontendv1.CreateSessionCmd{Cwd: "/w"}}}, func(c *frontendv1.FrontendCommand) bool { return c.GetCreateSession().GetCwd() == "/w" }},
		{"delete", &frontendv1.FrontendCommand{RequestId: "r5", Command: &frontendv1.FrontendCommand_DeleteSession{DeleteSession: &frontendv1.DeleteSessionCmd{SessionId: "s_9"}}}, func(c *frontendv1.FrontendCommand) bool { return c.GetDeleteSession().GetSessionId() == "s_9" }},
		{"resync", &frontendv1.FrontendCommand{RequestId: "r6", Command: &frontendv1.FrontendCommand_Resync{Resync: &frontendv1.ResyncCmd{FromSeq: 5}}}, func(c *frontendv1.FrontendCommand) bool { return c.GetResync().GetFromSeq() == 5 }},
		{"shutdown", &frontendv1.FrontendCommand{RequestId: "r7", Command: &frontendv1.FrontendCommand_Shutdown{Shutdown: &frontendv1.ShutdownCmd{}}}, func(c *frontendv1.FrontendCommand) bool { return c.GetShutdown() != nil }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			raw, err := protojson.Marshal(tc.cmd)
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			// Act.
			got, dispatch, terr := tr(raw)
			// Assert.
			if terr != nil || !dispatch {
				t.Fatalf("translate = (dispatch %v, err %v), want dispatch true no err", dispatch, terr)
			}
			if !tc.ok(got) {
				t.Fatalf("command routed to the wrong arm: %+v", got)
			}
		})
	}
}

func TestFrontendCommandTranslatorStampsScopedWorkspace(t *testing.T) {
	// Arrange — a command that omits the workspace (the URL already scopes it).
	h := newHarness(t)
	raw, err := protojson.Marshal(&frontendv1.FrontendCommand{RequestId: "r1", Command: &frontendv1.FrontendCommand_Interrupt{Interrupt: &frontendv1.InterruptCmd{}}})
	if err != nil {
		t.Fatalf("marshal: %v", err)
	}

	// Act.
	cmd, _, terr := h.srv.frontendCommandTranslator("/w")(raw)

	// Assert — the scoped workspace is stamped on.
	if terr != nil {
		t.Fatalf("translate: %v", terr)
	}
	if cmd.GetWorkspace() != "/w" {
		t.Fatalf("workspace = %q, want the scoped /w stamped on", cmd.GetWorkspace())
	}
}

func TestFrontendCommandTranslatorKeepsExplicitWorkspace(t *testing.T) {
	// Arrange — a command that names its own workspace.
	h := newHarness(t)
	raw, err := protojson.Marshal(&frontendv1.FrontendCommand{Workspace: "/other", RequestId: "r1", Command: &frontendv1.FrontendCommand_Interrupt{Interrupt: &frontendv1.InterruptCmd{}}})
	if err != nil {
		t.Fatalf("marshal: %v", err)
	}

	// Act.
	cmd, _, terr := h.srv.frontendCommandTranslator("/w")(raw)

	// Assert — an explicit workspace is not overwritten by the scope.
	if terr != nil {
		t.Fatalf("translate: %v", terr)
	}
	if cmd.GetWorkspace() != "/other" {
		t.Fatalf("workspace = %q, want the explicit /other preserved", cmd.GetWorkspace())
	}
}

func TestFrontendCommandTranslatorMalformedFrameErrors(t *testing.T) {
	// Arrange / Act.
	h := newHarness(t)
	_, _, err := h.srv.frontendCommandTranslator("/w")([]byte(`{not json`))

	// Assert — a malformed frame surfaces an error so the read loop logs it.
	if err == nil {
		t.Fatal("a malformed frame must surface an error so the read loop logs it")
	}
}

// --- workspaceForSession / known (unit) -----------------------------------

func TestWorkspaceForSessionResolvesNonTerminalRecord(t *testing.T) {
	h := newHarness(t)
	if err := h.reg.Put(registry.Record{SessionID: "s1", CWD: "/w"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	cwd, ok := h.srv.workspaceForSession("s1")
	if !ok || cwd != "/w" {
		t.Fatalf("workspaceForSession = (%q,%v), want (/w,true)", cwd, ok)
	}
}

func TestWorkspaceForSessionMissesTerminalRecord(t *testing.T) {
	h := newHarness(t)
	if err := h.reg.Put(registry.Record{SessionID: "s1", CWD: "/w", Terminal: true}); err != nil {
		t.Fatalf("put: %v", err)
	}
	if _, ok := h.srv.workspaceForSession("s1"); ok {
		t.Fatal("a terminal record must not resolve a driving workspace")
	}
}

func TestKnownReportsNonTerminalPresence(t *testing.T) {
	h := newHarness(t)
	if err := h.reg.Put(registry.Record{SessionID: "s1", CWD: "/w"}); err != nil {
		t.Fatalf("put: %v", err)
	}
	if !h.srv.known("s1") {
		t.Fatal("known should be true for a non-terminal record")
	}
	if h.srv.known("s_absent") {
		t.Fatal("known should be false for an absent record")
	}
}
