package workspace

import (
	"context"
	"errors"
	"os"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/account"
	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/wsm"
)

// fakeClient is a shimclient.Client whose StartSession answer the test
// arranges.
type fakeClient struct {
	shimclient.Client

	requests []*shimv1.StartSessionRequest
	response *shimv1.StartSessionResponse
	startErr error
	pid      int
	kills    []shimclient.KillAttribution
	killErr  error
}

func (c *fakeClient) StartSession(_ context.Context, req *shimv1.StartSessionRequest) (*shimv1.StartSessionResponse, error) {
	c.requests = append(c.requests, req)
	if c.startErr != nil {
		return nil, c.startErr
	}
	return c.response, nil
}

func (c *fakeClient) PID() int { return c.pid }

func (c *fakeClient) Kill(attr shimclient.KillAttribution) error {
	if c.killErr != nil {
		return c.killErr
	}
	c.kills = append(c.kills, attr)
	return nil
}

// fakeSupervisor records which bring-up path the lock probe selected.
type fakeSupervisor struct {
	client   *fakeClient
	spawns   []shimclient.Spec
	adopts   []string
	spawnErr error
	adoptErr error
}

func (s *fakeSupervisor) Spawn(_ context.Context, spec shimclient.Spec) (shimclient.Client, error) {
	if s.spawnErr != nil {
		return nil, s.spawnErr
	}
	s.spawns = append(s.spawns, spec)
	return s.client, nil
}

func (s *fakeSupervisor) Adopt(_ context.Context, _ ids.WorkspaceID, _ string, uds string) (shimclient.Client, error) {
	if s.adoptErr != nil {
		return nil, s.adoptErr
	}
	s.adopts = append(s.adopts, uds)
	return s.client, nil
}

// fakeWatcher is a sessionwatcher.Watcher with no watches behind it.
type fakeWatcher struct {
	sessionwatcher.Watcher
	closed bool
}

func (w *fakeWatcher) Close() error { w.closed = true; return nil }

func (w *fakeWatcher) Connected() bool { return true }

func (w *fakeWatcher) TurnInFlight() *ids.TurnID { return nil }

func (w *fakeWatcher) LiveWork() sessionwatcher.LiveWorkSet { return sessionwatcher.LiveWorkSet{} }

// startedResponse is the success answer a healthy bring-up gets back.
func startedResponse(vendorSessionID string) *shimv1.StartSessionResponse {
	return &shimv1.StartSessionResponse{
		Result: &shimv1.StartSessionResponse_Success{Success: &shimv1.StartSessionSuccess{
			Session: &conversationv1.SessionStarted{
				VendorSessionId: vendorSessionID,
				Runtime:         &conversationv1.SessionRuntime{ShimBuildSha: "sha-1"},
				EffectiveModel:  &conversationv1.AgentModel{Name: "opus"},
				PermissionMode:  permissionMode("plan"),
			},
		}},
	}
}

// coldResponse is the cold refusal a parked conversation gets back.
func coldResponse() *shimv1.StartSessionResponse {
	return &shimv1.StartSessionResponse{
		Result: &shimv1.StartSessionResponse_Failure{Failure: &shimv1.StartSessionFailure{
			Cause: &shimv1.StartSessionFailure_Cold{Cold: &conversationv1.SessionCold{
				ContextTokens:   120_000,
				LastRequestAtMs: 1,
				RequestedModel:  &conversationv1.AgentModel{Name: "opus"},
			}},
			Detail: "the cache lapsed",
		}},
	}
}

// fleetFixture is one arranged Fleet plus the fakes behind it.
type fleetFixture struct {
	fleet      *Fleet
	db         *fakeDB
	accounts   *fakeAccounts
	supervisor *fakeSupervisor
	client     *fakeClient
	feed       *fakeFeed
	footer     *fakeFooter
	log        *fakeSurfaces
	watcher    *fakeWatcher
	links      *recordingLinkSink
	probeState sessionlock.State
	probeErr   error
}

// recordingLinkSink answers the three view sinks' OnLink and nothing else: the
// fleet states the link itself only on the gate-parked bring-up, where no
// watcher opens to state it.
type recordingLinkSink struct {
	links []sessionwatcher.LinkState
}

func (s *recordingLinkSink) note(link sessionwatcher.LinkState) {
	s.links = append(s.links, link)
}

// The three view sinks are separate types because one type cannot embed all
// three interfaces (their method sets overlap).
type footerLinkSink struct {
	sessionwatcher.FooterSink
	rec *recordingLinkSink
}

func (s footerLinkSink) OnLink(_ ids.WorkspaceID, link sessionwatcher.LinkState) { s.rec.note(link) }

type topbarLinkSink struct {
	sessionwatcher.TopbarSink
	rec *recordingLinkSink
}

func (s topbarLinkSink) OnLink(_ ids.WorkspaceID, link sessionwatcher.LinkState) { s.rec.note(link) }

type sidebarLinkSink struct {
	sessionwatcher.SidebarSink
	rec *recordingLinkSink
}

func (s sidebarLinkSink) OnLink(_ ids.WorkspaceID, link sessionwatcher.LinkState) { s.rec.note(link) }

// newFleetFixture arranges a fleet whose lock probe reports free and whose
// bring-up succeeds.
func newFleetFixture(t *testing.T) *fleetFixture {
	t.Helper()
	f := &fleetFixture{
		db:         newFakeDB(),
		accounts:   &fakeAccounts{configDir: "/config", transcript: account.Transcript{Path: "/transcripts/vendor-1.jsonl", ConfigDir: "/config"}},
		client:     &fakeClient{response: startedResponse("vendor-1"), pid: 4242},
		feed:       &fakeFeed{},
		footer:     newFakeFooter(),
		log:        newFakeSurfaces(),
		watcher:    &fakeWatcher{},
		links:      &recordingLinkSink{},
		probeState: sessionlock.StateFree,
	}
	f.supervisor = &fakeSupervisor{client: f.client}

	fleet, err := NewFleet(FleetDeps{
		DB: f.db, Accounts: f.accounts, Supervisor: f.supervisor,
		Feed: f.feed, Footer: f.footer, Log: f.log,
		Sinks: sessionwatcher.Sinks{
			Footer:  footerLinkSink{rec: f.links},
			Topbar:  topbarLinkSink{rec: f.links},
			Sidebar: sidebarLinkSink{rec: f.links},
		},
		SocketPath: func(ws ids.WorkspaceID) string { return "/sock/" + string(ws) + ".sock" },
		LockDir:    t.TempDir(),
		Probe:      func(string, string) (sessionlock.State, error) { return f.probeState, f.probeErr },
		StartWatcher: func(context.Context, ids.WorkspaceID, shimclient.Client, sessionwatcher.Session, sessionwatcher.Sinks, dlog.Logger) (sessionwatcher.Watcher, error) {
			return f.watcher, nil
		},
		Now: func() time.Time { return fixedNow },
	})
	if err != nil {
		t.Fatalf("NewFleet: %v", err)
	}
	f.fleet = fleet
	return f
}

// workspace records one workspace in the fleet fixture's registry.
func (f *fleetFixture) workspace(id ids.WorkspaceID) wsm.Workspace {
	ws := wsm.Workspace{ID: id, Dir: "/tree/" + string(id), Repo: "repo-1"}
	f.db.with(ws)
	return ws
}

func TestNewFleetRefusesMissingCollaborators(t *testing.T) {
	tests := []struct {
		name string
		deps FleetDeps
	}{
		{name: "no state client", deps: FleetDeps{}},
		{name: "no account resolver", deps: FleetDeps{DB: newFakeDB()}},
		{name: "no supervisor", deps: FleetDeps{DB: newFakeDB(), Accounts: &fakeAccounts{}}},
		{
			name: "no socket path",
			deps: FleetDeps{DB: newFakeDB(), Accounts: &fakeAccounts{}, Supervisor: &fakeSupervisor{}},
		},
		{
			name: "no log surfaces",
			deps: FleetDeps{
				DB: newFakeDB(), Accounts: &fakeAccounts{}, Supervisor: &fakeSupervisor{},
				SocketPath: func(ids.WorkspaceID) string { return "" },
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange in the table. Act.
			_, err := NewFleet(tt.deps)
			// Assert.
			if err == nil {
				t.Fatalf("NewFleet(%s) = nil error, want a refusal", tt.name)
			}
		})
	}
}

func TestDecideSourceTable(t *testing.T) {
	deleted := &wsm.SessionTerminal{Kind: "deleted", Detail: "the user deleted it"}
	killed := &wsm.SessionTerminal{Kind: "killed", Detail: "KillWorkspace"}
	tests := []struct {
		name      string
		session   wsm.Session
		exists    bool
		wantFresh bool
		wantID    string
		wantErr   bool
	}{
		{
			name:      "no session record at all is the only proof of a fresh start",
			wantFresh: true,
		},
		{
			name:      "a record with no conversation still starts fresh",
			session:   wsm.Session{},
			exists:    true,
			wantFresh: true,
		},
		{
			name:    "a recorded conversation resumes",
			session: wsm.Session{VendorSessionID: "vendor-1"},
			exists:  true,
			wantID:  "vendor-1",
		},
		{
			name:    "a killed session still resumes its conversation",
			session: wsm.Session{VendorSessionID: "vendor-1", Terminal: killed},
			exists:  true,
			wantID:  "vendor-1",
		},
		{
			name:    "a deleted session refuses resurrection",
			session: wsm.Session{VendorSessionID: "vendor-1", Terminal: deleted},
			exists:  true,
			wantErr: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange in the table. Act.
			got, err := decideSource(tt.session, tt.exists)
			// Assert.
			if tt.wantErr {
				if err == nil {
					t.Fatalf("decideSource() = %+v, want a refusal", got)
				}
				return
			}
			if err != nil {
				t.Fatalf("decideSource: %v", err)
			}
			if got.Fresh != tt.wantFresh || got.VendorSessionID != tt.wantID {
				t.Fatalf("decideSource() = %+v, want fresh=%v id=%q", got, tt.wantFresh, tt.wantID)
			}
		})
	}
}

func TestStartFreshCarriesTheRecordedModelAndMode(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, Model: "opus", PermissionMode: "plan"}

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	fresh := f.client.requests[0].GetFresh()
	if fresh == nil || fresh.GetModel().GetName() != "opus" || fresh.GetPermissionMode().GetPlan() == nil {
		t.Fatalf("StartSession request = %v, want a fresh start carrying opus in plan mode", f.client.requests[0])
	}
}

func TestStartResumesARecordedConversation(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	resume := f.client.requests[0].GetResume()
	if resume == nil || resume.GetVendorSessionId() != "vendor-1" {
		t.Fatalf("StartSession request = %v, want a resume of vendor-1", f.client.requests[0])
	}
}

func TestStartRefusesADeletedSession(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{
		Workspace: ws.ID, VendorSessionID: "vendor-1",
		Terminal: &wsm.SessionTerminal{Kind: "deleted", Detail: "/clear"},
	}

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	asRefusal(t, err, ArmSessionDeleted)
}

func TestStartRefusesAResumeWhoseTranscriptIsMissingBeforeAnySpawn(t *testing.T) {
	// Arrange: a vanished file yields no death evidence, so the redial ladder
	// would otherwise loop forever on an unchangeable fact.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}
	f.accounts.transcriptErr = errors.New("no such file")

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	asRefusal(t, err, ArmTranscriptMissing)
	if len(f.supervisor.spawns) != 0 || len(f.supervisor.adopts) != 0 {
		t.Fatalf("bring-ups = %d spawns, %d adopts; want none before the guard", len(f.supervisor.spawns), len(f.supervisor.adopts))
	}
}

func TestStartFreshSkipsTheResumeGuard(t *testing.T) {
	// Arrange: a fresh start names no conversation, so there is no transcript
	// to guard.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.accounts.transcriptErr = errors.New("no such file")

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if len(f.supervisor.spawns) != 1 {
		t.Fatalf("spawns = %d, want exactly one", len(f.supervisor.spawns))
	}
}

func TestStartSpawnsWhenTheWorkspaceLockIsFree(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if len(f.supervisor.spawns) != 1 || len(f.supervisor.adopts) != 0 {
		t.Fatalf("bring-ups = %d spawns, %d adopts; want one spawn", len(f.supervisor.spawns), len(f.supervisor.adopts))
	}
}

func TestStartAdoptsWhenASurvivingShimHoldsTheLock(t *testing.T) {
	// Arrange: a held lock means a surviving shim already owns the
	// conversation, so a second one is never spawned onto it.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.probeState = sessionlock.StateHeld

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if len(f.supervisor.adopts) != 1 || len(f.supervisor.spawns) != 0 {
		t.Fatalf("bring-ups = %d spawns, %d adopts; want one adopt", len(f.supervisor.spawns), len(f.supervisor.adopts))
	}
}

func TestStartRefusesWhenTheLockProbeCouldNotTell(t *testing.T) {
	// Arrange: "could not tell" is never read as free.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.probeState, f.probeErr = sessionlock.StateUnknown, errors.New("permission denied")

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if err == nil {
		t.Fatal("Start() = nil error, want the unreadable lock refused")
	}
	if len(f.supervisor.spawns) != 0 {
		t.Fatalf("spawns = %d, want none on an unreadable lock", len(f.supervisor.spawns))
	}
}

func TestStartPassesTheRoutedConfigDirToTheSpawn(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if f.supervisor.spawns[0].ConfigDir != "/config" {
		t.Fatalf("spawn config dir = %q, want /config", f.supervisor.spawns[0].ConfigDir)
	}
}

func TestStartRecordsTheSessionFacts(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	session := f.db.sessions[ws.ID]
	if session.VendorSessionID != "vendor-1" || session.ConfigDir != "/config" ||
		session.Model != "opus" || session.PermissionMode != "plan" {
		t.Fatalf("recorded session = %+v, want the vendor identity, config dir, model and mode", session)
	}
}

func TestStartKeepsTheOriginalStartedAtAcrossARespawn(t *testing.T) {
	// Arrange: the session outlives one shim process, so its start instant does
	// not move when the process does.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	original := fixedNow.Add(-2 * time.Hour)
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, StartedAt: original}

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if !f.db.sessions[ws.ID].StartedAt.Equal(original) {
		t.Fatalf("StartedAt = %v, want the original %v", f.db.sessions[ws.ID].StartedAt, original)
	}
}

func TestStartAnswersAColdRefusalWithTheGate(t *testing.T) {
	// Arrange: a cold context is refused with its cost named, never silently
	// paid.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}
	f.client.response = coldResponse()

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if err != nil {
		t.Fatalf("Start: %v", err)
	}
	if !f.footer.coldGates[ws.ID].Standing {
		t.Fatal("the footer does not report a standing cold gate")
	}
	if len(f.feed.synthesized) != 1 || f.feed.synthesized[0].GetColdGate().GetStanding() == nil {
		t.Fatalf("synthesized rows = %v, want one standing gate row", f.feed.synthesized)
	}
}

func TestStartRemembersTheColdGateMenu(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}
	f.client.response = coldResponse()

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	gate, ok := f.fleet.ColdGate(ws.ID)
	if !ok || gate.VendorSessionID != "vendor-1" || len(gate.Models) != 1 || len(gate.Scopes) != 3 {
		t.Fatalf("served cold gate = (%+v, %v), want the menu the row offered", gate, ok)
	}
}

func TestStartSurfacesANonColdStartFailure(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.response = &shimv1.StartSessionResponse{
		Result: &shimv1.StartSessionResponse_Failure{Failure: &shimv1.StartSessionFailure{
			Cause:  &shimv1.StartSessionFailure_VendorStartFailed{VendorStartFailed: &shimv1.StartSessionVendorStartFailed{}},
			Detail: "the vendor binary is missing",
		}},
	}

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if err == nil {
		t.Fatal("Start() = nil error, want the vendor-start failure surfaced")
	}
}

func TestStartIsIdempotentForALiveSession(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("first Start: %v", err)
	}

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("second Start: %v", err)
	}

	// Assert.
	if len(f.supervisor.spawns) != 1 {
		t.Fatalf("spawns = %d, want exactly one", len(f.supervisor.spawns))
	}
}

func TestLiveReportsTheSessionAfterAStart(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if !f.fleet.Live(ws.ID) {
		t.Fatal("Live() reported no session after a successful start")
	}
}

func TestStopClosesTheWatcher(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Act.
	if err := f.fleet.Stop(context.Background(), ws.ID, true); err != nil {
		t.Fatalf("Stop: %v", err)
	}

	// Assert.
	if !f.watcher.closed {
		t.Fatal("Stop() left the watcher open")
	}
}

func TestStopOfAnAbsentSessionIsSuccess(t *testing.T) {
	// Arrange: the caller asked for a state that already holds.
	f := newFleetFixture(t)

	// Act.
	err := f.fleet.Stop(context.Background(), "w1", false)

	// Assert.
	if err != nil {
		t.Fatalf("Stop(absent session) = %v, want success", err)
	}
}

func TestRunningAnswersFalseWithoutASession(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)

	// Act.
	_, live := f.fleet.Running("w1")

	// Assert.
	if live {
		t.Fatal("Running() reported a live session where none exists")
	}
}

func TestHealthReportsTheLinkOfALiveSession(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Act.
	exists, connected := f.fleet.Health(ws.ID)

	// Assert.
	if !exists || !connected {
		t.Fatalf("Health() = (%v, %v), want a live, connected session", exists, connected)
	}
}

func TestShimAnswersFalseWithoutASession(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)

	// Act.
	_, ok := f.fleet.Shim("w1")

	// Assert.
	if ok {
		t.Fatal("Shim() answered a surface for a workspace with no session")
	}
}

func TestLockDirPrefersTheExplicitSetting(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	t.Setenv(LockDirEnv, "/from/env")

	// Act.
	got := f.fleet.lockDir()

	// Assert.
	if got == "/from/env" {
		t.Fatalf("lockDir() = %q, want the explicitly configured directory", got)
	}
}

func TestLockDirFallsBackToTheEnvironmentOverride(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	f.fleet.deps.LockDir = ""
	t.Setenv(LockDirEnv, "/from/env")

	// Act.
	got := f.fleet.lockDir()

	// Assert.
	if got != "/from/env" {
		t.Fatalf("lockDir() = %q, want /from/env", got)
	}
}

func TestPermissionModeRoundTripsEveryArm(t *testing.T) {
	tests := []string{"default", "acceptEdits", "bypassPermissions", "plan", "dontAsk", "auto"}
	for _, name := range tests {
		t.Run(name, func(t *testing.T) {
			// Arrange in the table. Act.
			got := permissionModeName(permissionMode(name))
			// Assert.
			if got != name {
				t.Fatalf("permissionModeName(permissionMode(%q)) = %q", name, got)
			}
		})
	}
}

func TestPermissionModeOfAnUnknownNameIsTheGatedDefault(t *testing.T) {
	// Arrange: an unknown name never resolves to a mode that disables the gate.
	// Act.
	got := permissionMode("something-nobody-implemented")

	// Assert.
	if got.GetDefault() == nil {
		t.Fatalf("permissionMode(unknown) = %v, want the default (gated) mode", got)
	}
}

func TestStartSurfacesAShimSinkFailure(t *testing.T) {
	// Arrange: a spawn without its fd 3 would inherit whatever was there.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.log.shimSinkErr = errors.New("the sink symlink is broken")

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if err == nil {
		t.Fatal("Start() = nil error, want the sink failure surfaced")
	}
}

func TestStartSurfacesAnUnknownWorkspace(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)

	// Act.
	err := f.fleet.Start(context.Background(), "nope")

	// Assert.
	if err == nil {
		t.Fatal("Start(unknown workspace) = nil error, want a failure")
	}
}

// TestMain keeps the vendor guard on for every test in this package, which is
// the standing rule: no test ever calls the vendor.
func TestMain(m *testing.M) {
	os.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "1")
	os.Exit(m.Run())
}

func TestModelOrDefaultAnswersTheRecordedModel(t *testing.T) {
	// Arrange.
	f := &Fleet{deps: FleetDeps{DefaultModel: "opus"}}

	// Act.
	got := f.modelOrDefault("sonnet")

	// Assert.
	if got != "sonnet" {
		t.Fatalf("modelOrDefault(\"sonnet\") = %q, want the recorded model", got)
	}
}

func TestModelOrDefaultFallsBackWhenTheCreateNamedNoModel(t *testing.T) {
	// Arrange.
	f := &Fleet{deps: FleetDeps{DefaultModel: "opus"}}

	// Act.
	got := f.modelOrDefault("")

	// Assert.
	if got != "opus" {
		t.Fatalf("modelOrDefault(\"\") = %q, want the daemon's default", got)
	}
}

// TestBringUpRelaysTheShimsConversationOwnedRefusal covers the workspace
// kernel lock's whole purpose from the daemon's side: another shim already
// holds this conversation, and the shim says so instead of starting a second
// vendor process on it.
func TestBringUpRelaysTheShimsConversationOwnedRefusal(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.response = &shimv1.StartSessionResponse{
		Result: &shimv1.StartSessionResponse_Failure{Failure: &shimv1.StartSessionFailure{
			Detail: "another shim holds this workspace's conversation",
			Cause: &shimv1.StartSessionFailure_ConversationOwned{
				ConversationOwned: &shimv1.StartSessionConversationOwned{},
			},
		}},
	}

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	asRefusal(t, err, ArmConversationOwned)
}

// TestAGateParkedBringUpStatesTheLinkItself pins the link restatement: no
// watcher opens on the gate-parked path, so without this the surfaces keep
// drawing the DEAD link of the shim that died before this one — which outranks
// the gate in the footer's status tree and hides what the user must answer.
func TestAGateParkedBringUpStatesTheLinkItself(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.response = coldResponse()

	// Act
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start with a cold refusal: %v", err)
	}

	// Assert
	want := []sessionwatcher.LinkState{
		shimclient.LinkConnected, shimclient.LinkConnected, shimclient.LinkConnected,
	}
	if len(f.links.links) != len(want) {
		t.Fatalf("OnLink calls = %v, want the footer, topbar and roster each told the link is connected", f.links.links)
	}
	for i, got := range f.links.links {
		if got != want[i] {
			t.Fatalf("OnLink call %d = %v, want %v", i, got, want[i])
		}
	}
}

// TestRunningAnswersQuietForAGateParkedSessionWithNoWatcher pins the nil-watcher
// guard: a session parked behind a cold gate keeps its client but never started
// a session, so it has NO watcher. Reading freeness off it must answer "live and
// quiet" rather than dereference nothing — the close verb reads exactly this,
// and a standing gate is ruled not to block a close.
func TestRunningAnswersQuietForAGateParkedSessionWithNoWatcher(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.response = coldResponse()
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start with a cold refusal: %v", err)
	}

	// Act
	running, live := f.fleet.Running(ws.ID)

	// Assert
	if !live {
		t.Fatal("Running() reports no live session for a gate-parked workspace, want the session reported")
	}
	if running.Turn != nil || !running.LiveWork.Empty() {
		t.Fatalf("Running() = %+v, want nothing in flight behind a standing gate", running)
	}
}

// TestHealthAnswersNotServingForAGateParkedSession is the same guard on the
// liveness probe: the session exists and its link truth is unknown, which is
// "not serving", never a crash.
func TestHealthAnswersNotServingForAGateParkedSession(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.response = coldResponse()
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start with a cold refusal: %v", err)
	}

	// Act
	exists, serving := f.fleet.Health(ws.ID)

	// Assert
	if !exists || serving {
		t.Fatalf("Health() = (%v, %v), want the session to exist and not be serving", exists, serving)
	}
}

// TestStopTellsTheViewsTheLinkIsDead pins the edge the roster's `dead` arm
// rests on. Stop closes the watcher BEFORE it kills the process, so the
// client's own LinkDead publish has nobody left to route it: whether the views
// ever heard the death would otherwise depend on the exit landing before the
// close, which is a race the stop itself can settle.
func TestStopTellsTheViewsTheLinkIsDead(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}
	f.links.links = nil

	// Act
	if err := f.fleet.Stop(context.Background(), ws.ID, true); err != nil {
		t.Fatalf("Stop: %v", err)
	}

	// Assert
	want := []sessionwatcher.LinkState{
		shimclient.LinkDead, shimclient.LinkDead, shimclient.LinkDead,
	}
	if len(f.links.links) != len(want) {
		t.Fatalf("OnLink calls = %v, want the footer, topbar and roster each told the link is dead", f.links.links)
	}
	for i, got := range f.links.links {
		if got != want[i] {
			t.Fatalf("OnLink call %d = %v, want %v", i, got, want[i])
		}
	}
}
