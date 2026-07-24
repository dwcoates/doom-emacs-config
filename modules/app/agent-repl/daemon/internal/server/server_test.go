package server

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"sync"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/frontend"
	"claude-repld/internal/registry"
	"claude-repld/internal/session"
	"claude-repld/internal/sessiondrv"
	"claude-repld/internal/ssm"
)

// ---------------------------------------------------------------------------
// Harness
//
// Post-cutover the daemon has no live-session hub: it consumes each session's
// UDS shim through a real *sessiondrv.Manager over a FAKE Spawner/Locator (no
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
}

func (f *fakeSpawner) EnsureShim(_ context.Context, sessionID, _ string) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.ensured = append(f.ensured, sessionID)
	return nil
}

func (f *fakeSpawner) StopShim(sessionID string) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.stopped = append(f.stopped, sessionID)
	return nil
}

func (f *fakeSpawner) ensuredIDs() []string {
	f.mu.Lock()
	defer f.mu.Unlock()
	return slices.Clone(f.ensured)
}

// stubMerge / stubLifecycle / stubState satisfy the frontend command handler's
// required collaborators; the daemon's HTTP tests never drive a merge or a
// lifecycle command, so they are inert.
type stubMerge struct{}

func (stubMerge) Merge(context.Context, string) error  { return nil }
func (stubMerge) Resume(context.Context, string) error { return nil }

type stubLifecycle struct{}

func (stubLifecycle) Close(context.Context, string) error { return nil }
func (stubLifecycle) Open(context.Context, string) error  { return nil }

type stubState struct{ ssm *ssm.Manager }

func (s stubState) Snapshot() *frontendv1.StateSnapshot {
	snap := &frontendv1.StateSnapshot{}
	if states, err := s.ssm.Snapshot(); err == nil {
		snap.Workspaces = states
	}
	return snap
}

type harness struct {
	ts      *httptest.Server
	srv     *Server
	reg     *registry.Registry
	driver  *sessiondrv.Manager
	spawner *fakeSpawner
}

func newHarness(t *testing.T) *harness {
	return newHarnessWith(t, Config{})
}

// newHarnessWith builds a harness, letting a test override selected Config
// fields (Remediator, RequestShutdown, Accounts, Logins, WidgetAssetsDir,
// BinaryMTime, IdleSweepTicks). Registry, Driver, SSM, and Frontend are always
// wired.
func newHarnessWith(t *testing.T, extra Config) *harness {
	t.Helper()
	logf := func(string, ...any) {}
	reg := registry.Open(filepath.Join(t.TempDir(), "sessions.json"), logf)

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
	driver, err := sessiondrv.New(sessiondrv.Config{
		Push:            &PushForwarder{Logf: logf},
		SSM:             mgr,
		Spawner:         spawner,
		Locator:         &SessionLocator{Reg: reg},
		SeqStore:        NewRegistrySeqStore(reg, logf),
		DaemonVersion:   "test",
		ProtocolVersion: "1",
		Logf:            logf,
	})
	if err != nil {
		t.Fatalf("sessiondrv new: %v", err)
	}
	t.Cleanup(driver.Close)

	// The command handler needs the session-lifecycle binding, whose *Server
	// target does not exist until New below — bind it after (mirrors main).
	binding := &SessionCommandBinding{Logf: logf}
	handler, err := newCommandHandler(driver, stubMerge{}, stubLifecycle{}, driver, binding, logf)
	if err != nil {
		t.Fatalf("command handler: %v", err)
	}
	fe := frontend.New(frontend.Config{Logf: logf, State: stubState{ssm: mgr}, Handler: handler})
	t.Cleanup(func() { _ = fe.Close() })

	cfg := extra
	cfg.Logf = logf
	cfg.Registry = reg
	cfg.Driver = driver
	cfg.SSM = mgr
	cfg.Frontend = fe
	srv := New(cfg)
	binding.SetTarget(srv)

	ts := httptest.NewServer(srv.Handler())
	t.Cleanup(ts.Close)

	return &harness{ts: ts, srv: srv, reg: reg, driver: driver, spawner: spawner}
}

// postCreate POSTs body to /sessions and returns the new session id.
func postCreate(t *testing.T, h *harness, body string) string {
	t.Helper()
	resp, err := http.Post(h.ts.URL+"/sessions", "application/json", bytes.NewBufferString(body))
	if err != nil {
		t.Fatalf("POST /sessions: %v", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusCreated {
		t.Fatalf("POST /sessions status = %d, want 201", resp.StatusCode)
	}
	var out struct {
		SessionID string `json:"session_id"`
		StreamURL string `json:"stream_url"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&out); err != nil {
		t.Fatalf("decode create response: %v", err)
	}
	return out.SessionID
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

// --- Create ---------------------------------------------------------------

func TestCreateSessionReturnsIDAndStreamURL(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	resp, err := http.Post(h.ts.URL+"/sessions", "application/json", bytes.NewBufferString(`{"cwd":"/w"}`))
	if err != nil {
		t.Fatalf("POST: %v", err)
	}
	defer resp.Body.Close()
	// Assert
	if resp.StatusCode != http.StatusCreated {
		t.Fatalf("status = %d, want 201", resp.StatusCode)
	}
	var out struct {
		SessionID string `json:"session_id"`
		StreamURL string `json:"stream_url"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&out); err != nil {
		t.Fatalf("decode: %v", err)
	}
	if !strings.HasPrefix(out.SessionID, "s_") {
		t.Errorf("session_id = %q, want an s_ id", out.SessionID)
	}
	if out.StreamURL != "/sessions/"+out.SessionID+"/stream" {
		t.Errorf("stream_url = %q", out.StreamURL)
	}
}

func TestCreateSessionRegistersARecord(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	id := postCreate(t, h, `{"cwd":"/w","model":"haiku","config_dir":"/cfg"}`)
	// Assert — the record is the driver's source of truth for this session.
	rec, ok := h.reg.Get(id)
	if !ok {
		t.Fatalf("no registry record for %s", id)
	}
	if rec.CWD != "/w" || rec.Model != "haiku" || rec.ConfigDir != "/cfg" {
		t.Errorf("record = %+v, want cwd/model/config_dir carried through", rec)
	}
}

func TestCreateSessionEagerlyBringsUpTheShim(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act — create resolves the workspace to the just-registered record and
	// asks the driver to bring its shim up.
	id := postCreate(t, h, `{"cwd":"/w"}`)
	// Assert — the driver's spawner was asked to ensure exactly this session.
	if !slices.Contains(h.spawner.ensuredIDs(), id) {
		t.Fatalf("spawner ensured %v, want it to include %s", h.spawner.ensuredIDs(), id)
	}
}

func TestCreateSessionRejectsInvalidPermissionMode(t *testing.T) {
	// Arrange
	h := newHarness(t)
	// Act
	resp, err := http.Post(h.ts.URL+"/sessions", "application/json",
		bytes.NewBufferString(`{"cwd":"/w","permission_mode":"nonsense"}`))
	if err != nil {
		t.Fatalf("POST: %v", err)
	}
	defer resp.Body.Close()
	// Assert
	if resp.StatusCode != http.StatusBadRequest {
		t.Errorf("status = %d, want 400", resp.StatusCode)
	}
}

func TestCreateSessionHardFailsUnresumableResume(t *testing.T) {
	// Arrange — a resume target whose transcript does not exist.
	h := newHarness(t)
	cfg := t.TempDir()
	// Act
	resp, err := http.Post(h.ts.URL+"/sessions", "application/json",
		bytes.NewBufferString(fmt.Sprintf(`{"cwd":"/w","config_dir":%q,"resume":"gone-uuid"}`, cfg)))
	if err != nil {
		t.Fatalf("POST: %v", err)
	}
	defer resp.Body.Close()
	// Assert — 422 with the machine-detectable code, not a silent fresh start.
	if resp.StatusCode != http.StatusUnprocessableEntity {
		t.Fatalf("status = %d, want 422", resp.StatusCode)
	}
	var out struct {
		Code string `json:"code"`
	}
	if err := json.NewDecoder(resp.Body).Decode(&out); err != nil {
		t.Fatalf("decode: %v", err)
	}
	if out.Code != "resume_transcript_missing" {
		t.Errorf("code = %q, want resume_transcript_missing", out.Code)
	}
}

func TestCreateSessionKeepsResumableResume(t *testing.T) {
	// Arrange — a resume target WITH a transcript on disk.
	h := newHarness(t)
	cfg := t.TempDir()
	writeTranscript(t, cfg, "uuid-1")
	// Act
	id := postCreate(t, h, fmt.Sprintf(`{"cwd":"/w","config_dir":%q,"resume":"uuid-1"}`, cfg))
	// Assert — the record carries the resume id as its claude_session_id.
	rec, _ := h.reg.Get(id)
	if rec.ClaudeSessionID != "uuid-1" {
		t.Errorf("claude_session_id = %q, want uuid-1", rec.ClaudeSessionID)
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
	id := postCreate(t, h, `{"cwd":"/w"}`)
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
	srv := New(Config{DaemonVersion: "v9", BinaryMTime: 5})
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
	v := SessionViewFromRecord(rec, []string{"p1", "p2"})
	// Assert — the S7 parity fields plus the pending-permission COUNT.
	if !v.GetTerminal() || v.GetDeathReason() != "delete session" {
		t.Errorf("terminal/death = %v/%q", v.GetTerminal(), v.GetDeathReason())
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

// --- Delete (S7: the DELETE /sessions/{id} HTTP route was removed; the
// deleteSession UDS command drives s.DeleteSession, tested directly here) -----

func TestDeleteSessionMarksRecordTerminal(t *testing.T) {
	// Arrange
	h := newHarness(t)
	id := postCreate(t, h, `{"cwd":"/w"}`)
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

// --- Remediation ----------------------------------------------------------

type fakeRemediator struct {
	started bool
	err     error
}

func (f *fakeRemediator) Start(string) (bool, error) { return f.started, f.err }

func postRemediation(t *testing.T, h *harness, body string) *http.Response {
	t.Helper()
	resp, err := http.Post(h.ts.URL+"/remediation", "application/json", bytes.NewBufferString(body))
	if err != nil {
		t.Fatalf("POST /remediation: %v", err)
	}
	t.Cleanup(func() { resp.Body.Close() })
	return resp
}

func TestRemediationRefusesALiveSession(t *testing.T) {
	// Arrange
	h := newHarnessWith(t, Config{Remediator: &fakeRemediator{started: true}})
	id := postCreate(t, h, `{"cwd":"/w"}`)
	// Act
	resp := postRemediation(t, h, fmt.Sprintf(`{"session_id":%q}`, id))
	// Assert — a session with a non-terminal record is alive; nothing to do.
	if resp.StatusCode != http.StatusConflict {
		t.Errorf("status = %d, want 409", resp.StatusCode)
	}
}

func TestRemediationDispatchesForAVanishedSession(t *testing.T) {
	// Arrange — no record for the id, so it is genuinely gone.
	h := newHarnessWith(t, Config{Remediator: &fakeRemediator{started: true}})
	// Act
	resp := postRemediation(t, h, `{"session_id":"s_gone"}`)
	// Assert
	if resp.StatusCode != http.StatusAccepted {
		t.Errorf("status = %d, want 202", resp.StatusCode)
	}
}

func TestRemediationRejectsAnEmptySessionId(t *testing.T) {
	h := newHarnessWith(t, Config{Remediator: &fakeRemediator{}})
	resp := postRemediation(t, h, `{"session_id":""}`)
	if resp.StatusCode != http.StatusBadRequest {
		t.Errorf("status = %d, want 400", resp.StatusCode)
	}
}

func TestRemediationReportsAnUnconfiguredRunner(t *testing.T) {
	h := newHarness(t) // no Remediator
	resp := postRemediation(t, h, `{"session_id":"s_gone"}`)
	if resp.StatusCode != http.StatusServiceUnavailable {
		t.Errorf("status = %d, want 503", resp.StatusCode)
	}
}

// --- Shutdown -------------------------------------------------------------

func TestShutdownEndpointTriggersRequestShutdown(t *testing.T) {
	// Arrange
	fired := make(chan struct{}, 1)
	h := newHarnessWith(t, Config{RequestShutdown: func() { fired <- struct{}{} }})
	// Act
	resp, err := http.Post(h.ts.URL+"/shutdown", "application/json", nil)
	if err != nil {
		t.Fatalf("POST /shutdown: %v", err)
	}
	defer resp.Body.Close()
	// Assert
	if resp.StatusCode != http.StatusAccepted {
		t.Fatalf("status = %d, want 202", resp.StatusCode)
	}
	<-fired
}

func TestShutdownEndpointReportsUnconfiguredWhenNoHook(t *testing.T) {
	h := newHarness(t)
	resp, err := http.Post(h.ts.URL+"/shutdown", "application/json", nil)
	if err != nil {
		t.Fatalf("POST /shutdown: %v", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusNotImplemented {
		t.Errorf("status = %d, want 501", resp.StatusCode)
	}
}

// --- Unknown-session routing ----------------------------------------------

func TestUnknownSessionRoutesReturn404(t *testing.T) {
	h := newHarness(t)
	for _, path := range []string{
		"/sessions/s_nope/commands",
		"/sessions/s_nope/status",
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

// --- streamCommandTranslator (unit) ---------------------------------------

func TestStreamTranslatorUserMessageBecomesSubmitPrompt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	tr := h.srv.streamCommandTranslator("/w")
	// Act
	cmd, dispatch, err := tr([]byte(`{"type":"user-message","request_id":"r1","content":"hello"}`))
	// Assert
	if err != nil || !dispatch {
		t.Fatalf("translate = (dispatch %v, err %v), want dispatch true no err", dispatch, err)
	}
	sp := cmd.GetSubmitPrompt()
	if sp == nil || sp.GetText() != "hello" {
		t.Fatalf("command = %+v, want a submit_prompt carrying 'hello'", cmd)
	}
	if cmd.GetWorkspace() != "/w" || cmd.GetRequestId() != "r1" {
		t.Errorf("command ws/request = %q/%q, want /w/r1", cmd.GetWorkspace(), cmd.GetRequestId())
	}
}

func TestStreamTranslatorInterrupt(t *testing.T) {
	h := newHarness(t)
	cmd, dispatch, err := h.srv.streamCommandTranslator("/w")([]byte(`{"type":"interrupt","request_id":"r1"}`))
	if err != nil || !dispatch || cmd.GetInterrupt() == nil {
		t.Fatalf("translate interrupt = (%+v, %v, %v)", cmd, dispatch, err)
	}
}

func TestStreamTranslatorPermissionDecisionAllow(t *testing.T) {
	h := newHarness(t)
	raw := `{"type":"permission-decision","request_id":"perm-1","decision":{"behavior":"allow"}}`
	cmd, dispatch, err := h.srv.streamCommandTranslator("/w")([]byte(raw))
	if err != nil || !dispatch {
		t.Fatalf("translate = (dispatch %v, err %v)", dispatch, err)
	}
	pa := cmd.GetPermissionAnswer()
	if pa == nil || !pa.GetAllow() || pa.GetPermissionRequestId() != "perm-1" {
		t.Fatalf("permission answer = %+v, want allow with request id perm-1", pa)
	}
}

func TestStreamTranslatorPermissionDecisionDenyCarriesMessage(t *testing.T) {
	h := newHarness(t)
	raw := `{"type":"permission-decision","request_id":"perm-1","decision":{"behavior":"deny","message":"no"}}`
	cmd, _, err := h.srv.streamCommandTranslator("/w")([]byte(raw))
	if err != nil {
		t.Fatalf("translate: %v", err)
	}
	pa := cmd.GetPermissionAnswer()
	if pa.GetAllow() || pa.GetDenyMessage() != "no" {
		t.Fatalf("deny answer = %+v, want allow=false with deny message 'no'", pa)
	}
}

func TestStreamTranslatorSupersededCommandIsALoudNoOp(t *testing.T) {
	h := newHarness(t)
	cmd, dispatch, err := h.srv.streamCommandTranslator("/w")([]byte(`{"type":"set-model","request_id":"r1","model":"opus"}`))
	if err != nil {
		t.Fatalf("translate: %v", err)
	}
	if dispatch || cmd != nil {
		t.Fatalf("set-model should be a no-op, got (dispatch %v, cmd %+v)", dispatch, cmd)
	}
}

func TestStreamTranslatorClientLogIsHandledInternally(t *testing.T) {
	h := newHarness(t)
	_, dispatch, err := h.srv.streamCommandTranslator("/w")([]byte(`{"type":"client-log","level":"warn","message":"x"}`))
	if err != nil || dispatch {
		t.Fatalf("client-log = (dispatch %v, err %v), want handled internally", dispatch, err)
	}
}

func TestStreamTranslatorMalformedFrameErrors(t *testing.T) {
	h := newHarness(t)
	_, _, err := h.srv.streamCommandTranslator("/w")([]byte(`{not json`))
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
