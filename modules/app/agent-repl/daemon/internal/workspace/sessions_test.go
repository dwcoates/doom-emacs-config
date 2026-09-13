package workspace

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/account"
	"claude-repld/internal/dlog"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/topbar"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/sessionwatcher"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/shimsocket"
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
	// standDown is the fixture's shared step order, appended to on the shim's
	// own KillSession.
	standDown *[]string
	// stoodDown records that the stand-down latch was armed, and
	// standDownBeforeKill that it was armed BEFORE the KillSession ask.
	stoodDown           bool
	standDownBeforeKill bool
	// standDownRefused is the DETACHED shape: the latch arms nothing, because
	// that process belongs to the successor daemon.
	standDownRefused bool
	// killSessionErr makes the session directive fail, which is what sends the
	// verb down its escalation path.
	killSessionErr error
	// reaped makes the supervised process ALREADY GONE, which is how a test
	// reaches the split between a session row and a live shim.
	reaped bool
	// entered is closed by StartSession on its first call and startHold is
	// what it then waits on, which is how a test holds a start open for as
	// long as it needs to observe something about the caller that is NOT
	// waiting for it. A nil startHold never waits.
	entered   chan struct{}
	startHold chan struct{}
}

func (c *fakeClient) Reaped() (shimclient.ExitInfo, bool) {
	if !c.reaped {
		return shimclient.ExitInfo{}, false
	}
	return shimclient.ExitInfo{PID: c.pid, Signal: "SIGKILL"}, true
}

func (c *fakeClient) StartSession(ctx context.Context, req *shimv1.StartSessionRequest) (*shimv1.StartSessionResponse, error) {
	c.requests = append(c.requests, req)
	if c.entered != nil {
		close(c.entered)
		c.entered = nil
	}
	if c.startHold != nil {
		select {
		case <-c.startHold:
		case <-ctx.Done():
			return nil, ctx.Err()
		}
	}
	if c.startErr != nil {
		return nil, c.startErr
	}
	return c.response, nil
}

// StandDown arms the fake's stand-down latch. It answers false for the
// detached shape, exactly as the real client does.
func (c *fakeClient) StandDown() bool {
	c.stoodDown = true
	return !c.standDownRefused
}

func (c *fakeClient) KillSession(context.Context, *shimv1.KillSessionRequest) (*shimv1.KillSessionResponse, error) {
	c.standDownBeforeKill = c.stoodDown
	if c.standDown != nil {
		*c.standDown = append(*c.standDown, "shim.KillSession")
	}
	if c.killSessionErr != nil {
		return nil, c.killSessionErr
	}
	return &shimv1.KillSessionResponse{
		Result: &shimv1.KillSessionResponse_Success{Success: &shimv1.KillSessionSuccess{}},
	}, nil
}

func (c *fakeClient) Hibernate(context.Context, *shimv1.HibernateRequest) (*shimv1.HibernateResponse, error) {
	return &shimv1.HibernateResponse{}, nil
}

func (c *fakeClient) PID() int { return c.pid }

func (c *fakeClient) Kill(_ context.Context, attr shimclient.KillAttribution) error {
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
	// adoptBlocks makes an adoption wait out the caller's context, which is a
	// shim whose socket is gone under a lock that reads held: shimclient's own
	// dial ladder redials that forever.
	adoptBlocks bool
	// adoptDeadline / adoptHadDeadline record the context the adoption was
	// actually dialed under, so a test can assert the bound rather than wait
	// it out.
	adoptDeadline    time.Time
	adoptHadDeadline bool
	// onSpawn runs at the top of Spawn, so a test can hold a start open while
	// it drives a second one at the same workspace.
	onSpawn func()
}

func (s *fakeSupervisor) Spawn(_ context.Context, spec shimclient.Spec) (shimclient.Client, error) {
	if s.onSpawn != nil {
		s.onSpawn()
	}
	if s.spawnErr != nil {
		return nil, s.spawnErr
	}
	s.spawns = append(s.spawns, spec)
	return s.client, nil
}

func (s *fakeSupervisor) Adopt(ctx context.Context, _ ids.WorkspaceID, _ string, uds string) (shimclient.Client, error) {
	s.adoptDeadline, s.adoptHadDeadline = ctx.Deadline()
	if s.adoptBlocks {
		<-ctx.Done()
		return nil, ctx.Err()
	}
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
	// standDown is the fixture's shared step order, appended to when the
	// daemon declares the session ending.
	standDown *[]string
}

func (w *fakeWatcher) SessionEnding(string) {
	if w.standDown != nil {
		*w.standDown = append(*w.standDown, "watcher.SessionEnding")
	}
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
	// topbarGates is every cold-gate state the STRIP was handed, in order. A
	// cold-gated workspace starts no session, so the topbar's own state is
	// the only thing standing between the reader and a blank strip.
	topbarGates []topbar.ColdGate
	log         *fakeSurfaces
	watcher     *fakeWatcher
	links       *recordingLinkSink
	probeState  sessionlock.State
	probeErr    error
	// socketState and socketErr script the shim-socket listener probe, which
	// decides adopt-versus-spawn beside the lock.
	socketState shimsocket.State
	socketErr   error
	// socketDir relocates the fixture's socket paths onto a real directory,
	// which is what a scenario about the shim's socket GENERATION needs: the
	// `<base>.nN.sock` candidates are found by reading that directory.
	socketDir string
	// socketStates overrides socketState per path, so one scenario can say
	// "the base is gone and generation 1 is live" — the shape a relaunched
	// shim leaves behind for the next daemon.
	socketStates map[string]shimsocket.State
	// adoptBound is the fixture's adoption bound, generous by default so no
	// ordinary scenario can trip it; a scenario about the give-up shortens it.
	adoptBound time.Duration
	// standDown is the ORDER the stand-down's steps happened in, shared by the
	// fake watcher and the fake client, because the ordering is the guarantee.
	standDown *[]string
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
	return newFleetFixtureBoundedAt(t, time.Minute)
}

// newFleetFixtureBoundedAt is newFleetFixture with the adoption bound stated,
// for the scenarios that are ABOUT the bound. Every other scenario takes the
// generous default so none of them can trip it.
func newFleetFixtureBoundedAt(t *testing.T, adoptBound time.Duration) *fleetFixture {
	t.Helper()
	order := &[]string{}
	f := &fleetFixture{
		standDown:  order,
		db:         newFakeDB(),
		accounts:   &fakeAccounts{configDir: "/config", transcript: account.Transcript{Path: "/transcripts/vendor-1.jsonl", ConfigDir: "/config"}},
		client:     &fakeClient{response: startedResponse("vendor-1"), pid: 4242, standDown: order},
		feed:       &fakeFeed{},
		footer:     newFakeFooter(),
		log:        newFakeSurfaces(),
		watcher:    &fakeWatcher{standDown: order},
		links:      &recordingLinkSink{},
		probeState: sessionlock.StateFree,
		adoptBound: adoptBound,

		socketState: shimsocket.StateAbsent,
	}
	f.supervisor = &fakeSupervisor{client: f.client}

	fleet, err := NewFleet(FleetDeps{
		DB: f.db, Accounts: f.accounts, Supervisor: f.supervisor,
		Feed: f.feed, Footer: f.footer, Topbar: stubTopbar{coldGates: &f.topbarGates}, Log: f.log,
		Sinks: sessionwatcher.Sinks{
			Footer:  footerLinkSink{rec: f.links},
			Topbar:  topbarLinkSink{rec: f.links},
			Sidebar: sidebarLinkSink{rec: f.links},
		},
		SocketPath: func(ws ids.WorkspaceID) string {
			if f.socketDir != "" {
				return filepath.Join(f.socketDir, string(ws)+".sock")
			}
			return "/sock/" + string(ws) + ".sock"
		},
		LockDir: t.TempDir(),
		Probe:   func(string, string) (sessionlock.State, error) { return f.probeState, f.probeErr },
		SocketProbe: func(path string) (shimsocket.State, error) {
			if state, ok := f.socketStates[path]; ok {
				return state, nil
			}
			return f.socketState, f.socketErr
		},
		StartWatcher: func(context.Context, ids.WorkspaceID, shimclient.Client, sessionwatcher.Session, sessionwatcher.Sinks, dlog.Logger) (sessionwatcher.Watcher, error) {
			return f.watcher, nil
		},
		Now:        func() time.Time { return fixedNow },
		AdoptBound: f.adoptBound,
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

func TestClassifySourceTable(t *testing.T) {
	deleted := &wsm.SessionTerminal{Kind: "deleted", Detail: "the user deleted it"}
	killed := &wsm.SessionTerminal{Kind: "killed", Detail: "KillWorkspace"}
	tests := []struct {
		name         string
		session      wsm.Session
		exists       bool
		noTranscript bool
		wantFresh    bool
		wantID       string
		wantErr      bool
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
			name:    "a recorded conversation whose transcript is found resumes",
			session: wsm.Session{VendorSessionID: "vendor-1"},
			exists:  true,
			wantID:  "vendor-1",
		},
		{
			name:         "a recorded conversation with NO transcript comes up fresh",
			session:      wsm.Session{VendorSessionID: "vendor-1"},
			exists:       true,
			noTranscript: true,
			wantFresh:    true,
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
			// Arrange.
			f := newFleetFixture(t)
			if tt.noTranscript {
				f.accounts.transcriptErr = errors.New("no such file")
			}

			// Act.
			got, err := f.fleet.classifySource(context.Background(), f.log.logger, "w1", "/tree/w1", tt.session, tt.exists)

			// Assert.
			if tt.wantErr {
				if err == nil {
					t.Fatalf("classifySource() = %+v, want a refusal", got)
				}
				return
			}
			if err != nil {
				t.Fatalf("classifySource: %v", err)
			}
			if got.Fresh != tt.wantFresh || got.VendorSessionID != tt.wantID {
				t.Fatalf("classifySource() = %+v, want fresh=%v id=%q", got, tt.wantFresh, tt.wantID)
			}
		})
	}
}

func TestClassifySourceOpensTheAbandonedConversationFaultOnce(t *testing.T) {
	// Arrange: the same recorded conversation classified twice must leave ONE
	// record of what was abandoned, naming the old vendor session id. The
	// workspace has TAKEN A TURN, which is what makes the vanished transcript
	// a real abandonment rather than a bounce before the first turn.
	f := newFleetFixture(t)
	f.accounts.transcriptErr = errors.New("no such file")
	f.db.putTurns = append(f.db.putTurns, wsm.Turn{ID: "t1", Workspace: "w1"})
	session := wsm.Session{Workspace: "w1", VendorSessionID: "vendor-old"}

	// Act.
	for range 2 {
		if _, err := f.fleet.classifySource(context.Background(), f.log.logger, "w1", "/tree/w1", session, true); err != nil {
			t.Fatalf("classifySource: %v", err)
		}
	}

	// Assert.
	var opened []wsm.Fault
	for _, fault := range f.db.dbFaults {
		if fault.Kind == health.KindConversationAbandoned {
			opened = append(opened, fault)
		}
	}
	if len(opened) != 1 {
		t.Fatalf("abandoned-conversation faults = %d, want exactly one", len(opened))
	}
	if got := opened[0].Evidence["vendor_session_id"]; got != "vendor-old" {
		t.Fatalf("fault evidence vendor_session_id = %q, want the abandoned id", got)
	}
}

// abandonedFaults is the conversation-abandoned rows the fake state client
// holds, which is the record a user gets that history was left behind.
func abandonedFaults(db *fakeDB) []wsm.Fault {
	var opened []wsm.Fault
	for _, fault := range db.dbFaults {
		if fault.Kind == health.KindConversationAbandoned {
			opened = append(opened, fault)
		}
	}
	return opened
}

// recordedAt reports whether the captured log holds a record at this level for
// this operation carrying this message.
func recordedAt(f *fleetFixture, level, operation, message string) bool {
	for _, r := range f.log.logger.Records() {
		if r.Level == level && r.Operation == operation && r.Message == message {
			return true
		}
	}
	return false
}

const (
	abandonedMessage   = "the recorded conversation has no transcript on disk; the session comes up FRESH"
	neverEngagedNotice = "the recorded conversation never took a turn and wrote no transcript; the session comes up FRESH"
)

func TestClassifySourceIsQuietWhenTheConversationNeverTookATurn(t *testing.T) {
	// Arrange: a vendor id minted at spawn and bounced before its first turn.
	// No turn was ever recorded, so no transcript was ever written and nothing
	// is lost by coming up fresh.
	f := newFleetFixture(t)
	f.accounts.transcriptErr = errors.New("no such file")
	session := wsm.Session{Workspace: "w1", VendorSessionID: "vendor-never-turned"}

	// Act.
	got, err := f.fleet.classifySource(context.Background(), f.log.logger, "w1", "/tree/w1", session, true)

	// Assert.
	if err != nil {
		t.Fatalf("classifySource: %v", err)
	}
	if !got.Fresh {
		t.Fatalf("classifySource() = %+v, want a fresh source", got)
	}
	if recordedAt(f, "warn", opBringUp, abandonedMessage) {
		t.Fatalf("records = %+v, want no abandonment warning for a conversation that never took a turn", f.log.logger.Records())
	}
	if !recordedAt(f, "info", opBringUp, neverEngagedNotice) {
		t.Fatalf("records = %+v, want the ordinary never-engaged notice at info", f.log.logger.Records())
	}
}

func TestClassifySourceOpensNoFaultWhenTheConversationNeverTookATurn(t *testing.T) {
	// Arrange: as above. The fault is the user's only record of lost history,
	// so a conversation that had none must not open one.
	f := newFleetFixture(t)
	f.accounts.transcriptErr = errors.New("no such file")
	session := wsm.Session{Workspace: "w1", VendorSessionID: "vendor-never-turned"}

	// Act.
	if _, err := f.fleet.classifySource(context.Background(), f.log.logger, "w1", "/tree/w1", session, true); err != nil {
		t.Fatalf("classifySource: %v", err)
	}

	// Assert.
	if opened := abandonedFaults(f.db); len(opened) != 0 {
		t.Fatalf("abandoned-conversation faults = %+v, want none for a conversation that never took a turn", opened)
	}
}

func TestClassifySourceWarnsWhenAnEngagedConversationsTranscriptIsGone(t *testing.T) {
	// Arrange: the workspace took a turn, so a vanished transcript is real
	// history abandoned and must stay exactly as loud as it has always been.
	f := newFleetFixture(t)
	f.accounts.transcriptErr = errors.New("no such file")
	f.db.putTurns = append(f.db.putTurns, wsm.Turn{ID: "t1", Workspace: "w1"})
	session := wsm.Session{Workspace: "w1", VendorSessionID: "vendor-old"}

	// Act.
	if _, err := f.fleet.classifySource(context.Background(), f.log.logger, "w1", "/tree/w1", session, true); err != nil {
		t.Fatalf("classifySource: %v", err)
	}

	// Assert.
	if !recordedAt(f, "warn", opBringUp, abandonedMessage) {
		t.Fatalf("records = %+v, want the abandonment warning", f.log.logger.Records())
	}
}

func TestClassifySourceStaysLoudWhenTheTurnsExistenceReadFails(t *testing.T) {
	// Arrange: the state client cannot answer whether the workspace was ever
	// engaged. A read that could not tell is never read as proof of nothing
	// lost, so the abandonment stays loud.
	f := newFleetFixture(t)
	f.accounts.transcriptErr = errors.New("no such file")
	f.db.hasTurnsErr = errFake
	session := wsm.Session{Workspace: "w1", VendorSessionID: "vendor-old"}

	// Act.
	if _, err := f.fleet.classifySource(context.Background(), f.log.logger, "w1", "/tree/w1", session, true); err != nil {
		t.Fatalf("classifySource: %v", err)
	}

	// Assert.
	if opened := abandonedFaults(f.db); len(opened) != 1 {
		t.Fatalf("abandoned-conversation faults = %+v, want exactly one when the engagement read failed", opened)
	}
}

func TestClassifySourceReportsAFailedTurnsExistenceRead(t *testing.T) {
	// Arrange: a state client that cannot answer an existence query is a fault
	// of its own and is never swallowed by the branch that recovers from it.
	f := newFleetFixture(t)
	f.accounts.transcriptErr = errors.New("no such file")
	f.db.hasTurnsErr = errFake
	session := wsm.Session{Workspace: "w1", VendorSessionID: "vendor-old"}

	// Act.
	if _, err := f.fleet.classifySource(context.Background(), f.log.logger, "w1", "/tree/w1", session, true); err != nil {
		t.Fatalf("classifySource: %v", err)
	}

	// Assert.
	if !recordedAt(f, "error", opBringUp, "could not tell whether the workspace ever took a turn; the abandoned conversation stays loud") {
		t.Fatalf("records = %+v, want the failed engagement read reported", f.log.logger.Records())
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

func TestStartComesUpFreshWhenTheRecordedTranscriptIsMissing(t *testing.T) {
	// Arrange: a session that pre-minted a vendor id but never took a turn has
	// no transcript, and refusing it left the workspace with no session at all.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}
	f.accounts.transcriptErr = errors.New("no such file")

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if err != nil {
		t.Fatalf("Start = %v, want a fresh session rather than a refusal", err)
	}
	if got := f.client.requests[0].GetFresh(); got == nil {
		t.Fatalf("StartSession source = %+v, want the fresh arm", f.client.requests[0].GetSource())
	}
}

func TestStartDoesNotDrawTranscriptMissingForANeverTurnedSession(t *testing.T) {
	// Arrange: the retired refusal. The classifier answers fresh instead.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}
	f.accounts.transcriptErr = errors.New("no such file")

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	var refusal *Refusal
	if errors.As(err, &refusal) && refusal.Arm == ArmTranscriptMissing {
		t.Fatalf("Start = %v, want no transcript_missing refusal", err)
	}
}

func TestStartFreshSpawnsWithNoTranscriptOnDisk(t *testing.T) {
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
	f.socketState = shimsocket.StateLive

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

func TestStartStatesTheColdGateToTheTopbar(t *testing.T) {
	// Arrange: a cold refusal starts NO session, so the topbar's session facts
	// never arrive and its own gate state is the only thing that keeps the
	// strip from staying blank for as long as the gate stands.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}
	f.client.response = coldResponse()

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	want := []topbar.ColdGate{{Standing: true, ContextTokens: 120_000}}
	if len(f.topbarGates) != len(want) || f.topbarGates[0] != want[0] {
		t.Fatalf("topbar cold gates = %v, want %v", f.topbarGates, want)
	}
}

// TestColdGatedBringUpIsNotServing is the sweep's selection read end-to-end
// through the production bring-up: a cold refusal keeps the client installed so
// the gate's answer can re-open through it, and for ten hours that installed
// client made the workspace look hibernatable -- one Hibernate directive and
// one `no_session` WARN every five minutes for a session that never started.
func TestColdGatedBringUpIsNotServing(t *testing.T) {
	tests := []struct {
		name     string
		response *shimv1.StartSessionResponse
		want     bool
	}{
		{
			name:     "a bring-up the shim answered cold",
			response: coldResponse(),
			want:     false,
		},
		{
			name:     "a bring-up whose session started",
			response: startedResponse("vendor-1"),
			want:     true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := f.workspace("w1")
			f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}
			f.client.response = tt.response

			// Act.
			if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
				t.Fatalf("Start: %v", err)
			}

			// Assert.
			if got := f.fleet.Serving(ws.ID); got != tt.want {
				t.Fatalf("Serving(%q) = %v, want %v", ws.ID, got, tt.want)
			}
		})
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
	r := asRefusal(t, err, ArmVendorStartFailed)
	if r.Fields["detail"] != "the vendor binary is missing" {
		t.Fatalf("vendor_start_failed detail field = %v, want the shim's own account", r.Fields["detail"])
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

// TestFreshModelNamesTheRecordedModel pins that the user's choice is what a
// fresh session names.
func TestFreshModelNamesTheRecordedModel(t *testing.T) {
	// Arrange / Act.
	got := freshModel("sonnet")

	// Assert.
	if got.GetName() != "sonnet" {
		t.Fatalf("freshModel(\"sonnet\") = %v, want the recorded model", got)
	}
}

// TestFreshModelLeavesTheModelUnsetWhenTheCreateNamedNone is the landing-7
// contract: StartSessionFresh.model is optional and UNSET means the SDK's own
// default, so the daemon substitutes nothing of its own.
func TestFreshModelLeavesTheModelUnsetWhenTheCreateNamedNone(t *testing.T) {
	// Arrange / Act.
	got := freshModel("")

	// Assert.
	if got != nil {
		t.Fatalf("freshModel(\"\") = %v, want an unset model", got)
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

// TestCloseWatchersEndsEveryLiveSessionsWatcher covers the daemon's teardown
// order: the watchers are closed (and their in-flight sink work joined) before
// the state client their sinks read is closed.
func TestCloseWatchersEndsEveryLiveSessionsWatcher(t *testing.T) {
	// Arrange: a live session with a watcher.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Act.
	f.fleet.CloseWatchers()

	// Assert.
	if !f.watcher.closed {
		t.Fatal("the live session's watcher was not closed by the fleet's teardown")
	}
}

// TestCloseWatchersOnAFleetWithNoSessionsDoesNothing is the other edge: a
// daemon that never brought a session up tears down cleanly.
func TestCloseWatchersOnAFleetWithNoSessionsDoesNothing(t *testing.T) {
	// Arrange, Act.
	f := newFleetFixture(t)
	f.fleet.CloseWatchers()

	// Assert.
	if f.watcher.closed {
		t.Fatal("a watcher was closed on a fleet that has no live session")
	}
}

// TestStartPortsTheTranscriptWhenTheAccountRoutingChanged pins daemon.md 10a:
// the config dir is decided at every start, and a resume whose recorded root
// is no longer the routed one carries its transcript across first.
func TestStartPortsTheTranscriptWhenTheAccountRoutingChanged(t *testing.T) {
	// Arrange: the session was filed under /old; this boot routes to /config.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.accounts.transcript = account.Transcript{Path: "/old/projects/w1/vendor-1.jsonl", ConfigDir: "/old"}
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1", ConfigDir: "/old"}

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if len(f.accounts.moved) != 1 || f.accounts.moved[0].ToConfigDir != "/config" {
		t.Fatalf("moved transcripts = %+v, want one into the routed root /config", f.accounts.moved)
	}
}

// TestStartSpawnsUnderTheRoutedRootAfterAnAccountSwitch pins the other half:
// the shim is spawned under the newly routed root, not the recorded one.
func TestStartSpawnsUnderTheRoutedRootAfterAnAccountSwitch(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.accounts.transcript = account.Transcript{Path: "/old/projects/w1/vendor-1.jsonl", ConfigDir: "/old"}
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1", ConfigDir: "/old"}

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if got := f.supervisor.spawns[0].ConfigDir; got != "/config" {
		t.Fatalf("spawn ConfigDir = %q, want the routed root /config", got)
	}
}

// TestStartPortsNothingForAFreshSession pins that a fresh start has no
// conversation to carry: the new root is simply where this one is filed.
func TestStartPortsNothingForAFreshSession(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, ConfigDir: "/old"}

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if len(f.accounts.moved) != 0 {
		t.Fatalf("moved transcripts = %+v, want none for a fresh start", f.accounts.moved)
	}
}

// TestStartPortsNothingWhenTheRoutingIsUnchanged pins that the ordinary start
// -- the recorded root and the routed one agreeing -- touches no transcript.
func TestStartPortsNothingWhenTheRoutingIsUnchanged(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1", ConfigDir: "/config"}

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if len(f.accounts.moved) != 0 {
		t.Fatalf("moved transcripts = %+v, want none when the routing is unchanged", f.accounts.moved)
	}
}

// TestShimAnswersFalseForASessionWhoseProcessIsGone covers the liveness split:
// the map entry outlives a shim killed out from under the daemon, and reading
// presence alone would drive a verb over a dead connection.
func TestShimAnswersFalseForASessionWhoseProcessIsGone(t *testing.T) {
	// Arrange: a live session whose process has since been reaped.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}
	f.supervisor.client.reaped = true

	// Act.
	_, ok := f.fleet.Shim(ws.ID)

	// Assert.
	if ok {
		t.Fatal("Shim() answered a surface for a session whose process is gone")
	}
}

// TestShimAnswersTheSurfaceForALiveSession is the other side of the same split,
// so a liveness read that refused everything would be caught here.
func TestShimAnswersTheSurfaceForALiveSession(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Act.
	_, ok := f.fleet.Shim(ws.ID)

	// Assert.
	if !ok {
		t.Fatal("Shim() refused a live session")
	}
}

// TestFleetProbeWorkspaceLockRecordsAnOrdinaryProbe pins that the fleet's
// production probe lands a debug record for a lock it could read, so a
// spawn-versus-adopt decision is reconstructable from the log.
func TestFleetProbeWorkspaceLockRecordsAnOrdinaryProbe(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()

	// Act.
	if _, err := probeWorkspaceLock(log)(t.TempDir(), t.TempDir()); err != nil {
		t.Fatalf("probe() error = %v", err)
	}

	// Assert.
	records := log.Records()
	if len(records) != 1 || records[0].Level != "debug" ||
		records[0].Operation != "daemon.sessionlock.probe" {
		t.Fatalf("records = %+v, want one debug daemon.sessionlock.probe record", records)
	}
}

// TestFleetProbeWorkspaceLockRecordsAnUnderivablePath pins that a lock path the
// fleet cannot derive is recorded rather than returned silently.
func TestFleetProbeWorkspaceLockRecordsAnUnderivablePath(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()

	// Act.
	state, err := probeWorkspaceLock(log)("", "")

	// Assert.
	if err == nil || state != sessionlock.StateUnknown {
		t.Fatalf("probe() = %v, %v, want StateUnknown and an error", state, err)
	}
	records := log.Records()
	if len(records) != 1 || records[0].Level != "error" ||
		records[0].Operation != "daemon.workspace.probe_workspace_lock" {
		t.Fatalf("records = %+v, want one error daemon.workspace.probe_workspace_lock record", records)
	}
}

// alreadyStartedResponse is the refusal a shim answers a SECOND StartSession
// with: one shim serves exactly one session.
func alreadyStartedResponse() *shimv1.StartSessionResponse {
	return &shimv1.StartSessionResponse{
		Result: &shimv1.StartSessionResponse_Failure{Failure: &shimv1.StartSessionFailure{
			Cause:  &shimv1.StartSessionFailure_AlreadyStarted{AlreadyStarted: &shimv1.StartSessionAlreadyStarted{}},
			Detail: "this shim already started its session; one shim serves exactly one",
		}},
	}
}

func TestStartSendsNoStartSessionToAnAdoptedShim(t *testing.T) {
	// Arrange: a held lock selects the adopt path, and the surviving shim on
	// the other side of it already serves its one session.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}
	f.probeState = sessionlock.StateHeld
	f.socketState = shimsocket.StateLive
	f.client.response = alreadyStartedResponse()

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if err != nil {
		t.Fatalf("Start: %v", err)
	}
	if len(f.client.requests) != 0 {
		t.Fatalf("StartSession requests = %d, want none on an adopted shim", len(f.client.requests))
	}
}

func TestStartRemembersTheAdoptedShimAsLive(t *testing.T) {
	// Arrange: mounting a parked workspace onto a surviving shim is a mount,
	// so the session it attaches to is live afterwards.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}
	f.probeState = sessionlock.StateHeld
	f.socketState = shimsocket.StateLive

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if !f.fleet.Live(ws.ID) {
		t.Fatal("Live() = false after adopting a surviving shim, want the mounted session live")
	}
}

func TestStartOpensTheAdoptedSessionsWatches(t *testing.T) {
	// Arrange: the adopted session's facts arrive on the watch's landing-7
	// re-announcement, so a watch that never opens leaves the daemon blind.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}
	f.probeState = sessionlock.StateHeld
	f.socketState = shimsocket.StateLive

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	exists, serving := f.fleet.Health(ws.ID)
	if !exists || !serving {
		t.Fatalf("Health() = %v, %v; want an adopted session whose watch is serving", exists, serving)
	}
}

func TestStartRefusesAnAlreadyStartedShimUnderItsOwnArm(t *testing.T) {
	// Arrange: a SPAWNED shim that answers already_started is a named state,
	// never an untyped internal on a contract path.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.response = alreadyStartedResponse()

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if err == nil {
		t.Fatal("Start() = nil error, want the already-started refusal surfaced")
	}
	asRefusal(t, err, ArmAlreadyStarted)
}

func TestStartRefusesAnUnsetStartFailureCauseUnderItsOwnArm(t *testing.T) {
	// Arrange: an unset cause oneof is illegal on the wire; it is surfaced
	// rather than guessed at.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.response = &shimv1.StartSessionResponse{
		Result: &shimv1.StartSessionResponse_Failure{Failure: &shimv1.StartSessionFailure{Detail: "no cause"}},
	}

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if err == nil {
		t.Fatal("Start() = nil error, want the unset cause surfaced")
	}
	asRefusal(t, err, ArmStartSessionUnspecified)
}

// ---------------------------------------------------------------------------
// ResumeCold: the remediated re-open an answered cold gate spends.
//
// The gap these pin is what left TestColdGate red end to end: AnswerColdGate
// called the shim directly, so the shim re-opened the session perfectly well
// and the DAEMON installed no watcher, recorded no facts and never republished
// the host view — and the very next SubmitPrompt was refused `no_session`.
// ---------------------------------------------------------------------------

// parkedGate arranges a workspace parked behind a standing cold gate and leaves
// the shim ready to answer the remediated resume with a started session.
func parkedGate(t *testing.T, f *fleetFixture, ws wsm.Workspace) {
	t.Helper()
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1", HostSessionID: "host-1"}
	f.client.response = coldResponse()
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start with a cold refusal: %v", err)
	}
	f.client.response = startedResponse("vendor-2")
}

// payRemediation is the simplest answered choice: the read is paid for.
func payRemediation() *conversationv1.SessionColdRemediation {
	return &conversationv1.SessionColdRemediation{
		Remediation: &conversationv1.SessionColdRemediation_Pay{Pay: &conversationv1.SessionColdPay{}},
	}
}

func TestResumeColdCarriesTheRemediationOnTheResume(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	parkedGate(t, f, ws)

	// Act.
	if err := f.fleet.ResumeCold(context.Background(), ws.ID,
		ColdResume{VendorSessionID: "vendor-1", Remediation: payRemediation()}); err != nil {
		t.Fatalf("ResumeCold: %v", err)
	}

	// Assert.
	if len(f.client.requests) != 2 {
		t.Fatalf("StartSession calls = %d, want the refused bring-up and the remediated re-open", len(f.client.requests))
	}
	resume := f.client.requests[1].GetResume()
	if resume.GetVendorSessionId() != "vendor-1" || resume.GetColdRemediation().GetPay() == nil {
		t.Fatalf("re-open resume = %v, want vendor-1 carrying the pay remediation", resume)
	}
}

func TestResumeColdInstallsTheSessionWatcher(t *testing.T) {
	// Arrange: a parked session has no watcher, which is what refuses the next
	// prompt `no_session`.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	parkedGate(t, f, ws)
	if _, serving := f.fleet.Health(ws.ID); serving {
		t.Fatal("the parked session already reports serving, so this test could not tell the watcher apart")
	}

	// Act.
	if err := f.fleet.ResumeCold(context.Background(), ws.ID,
		ColdResume{VendorSessionID: "vendor-1", Remediation: payRemediation()}); err != nil {
		t.Fatalf("ResumeCold: %v", err)
	}

	// Assert.
	exists, serving := f.fleet.Health(ws.ID)
	if !exists || !serving {
		t.Fatalf("Health() = (%v, %v), want the re-opened session watched and serving", exists, serving)
	}
}

func TestResumeColdRecordsTheReopenedSessionFacts(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	parkedGate(t, f, ws)

	// Act.
	if err := f.fleet.ResumeCold(context.Background(), ws.ID,
		ColdResume{VendorSessionID: "vendor-1", Remediation: payRemediation()}); err != nil {
		t.Fatalf("ResumeCold: %v", err)
	}

	// Assert.
	recorded := f.db.sessions[ws.ID]
	if recorded.VendorSessionID != "vendor-2" || recorded.Model != "opus" {
		t.Fatalf("recorded session = %+v, want the conversation and model the re-open reported", recorded)
	}
}

func TestResumeColdKeepsTheParkedHostIdentity(t *testing.T) {
	// Arrange: the remediated resume is the SAME session, so it must not mint a
	// second host identity for a conversation that never ended.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	parkedGate(t, f, ws)

	// Act.
	if err := f.fleet.ResumeCold(context.Background(), ws.ID,
		ColdResume{VendorSessionID: "vendor-1", Remediation: payRemediation()}); err != nil {
		t.Fatalf("ResumeCold: %v", err)
	}

	// Assert.
	if got := f.db.sessions[ws.ID].HostSessionID; got != "host-1" {
		t.Fatalf("host session id = %q, want the parked session's own %q", got, "host-1")
	}
}

func TestResumeColdRefusesAWorkspaceWithNoLiveSession(t *testing.T) {
	// Arrange: no bring-up ever ran, so nothing is parked.
	f := newFleetFixture(t)
	ws := f.workspace("w1")

	// Act.
	err := f.fleet.ResumeCold(context.Background(), ws.ID,
		ColdResume{VendorSessionID: "vendor-1", Remediation: payRemediation()})

	// Assert.
	asRefusal(t, err, ArmNoSession)
}

func TestResumeColdRefusesAReapedClient(t *testing.T) {
	// Arrange: the map entry outlives the process, and re-opening over a dead
	// connection answers a raw transport error where the contract spells
	// no_session.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	parkedGate(t, f, ws)
	f.client.reaped = true

	// Act.
	err := f.fleet.ResumeCold(context.Background(), ws.ID,
		ColdResume{VendorSessionID: "vendor-1", Remediation: payRemediation()})

	// Assert.
	asRefusal(t, err, ArmNoSession)
}

func TestResumeColdRefusesWhenTheShimRefusesTheRemediatedResumeAsColdAgain(t *testing.T) {
	// Arrange: the shim states the cost a second time, so the session is parked
	// once more rather than up.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	parkedGate(t, f, ws)
	f.client.response = coldResponse()

	// Act.
	err := f.fleet.ResumeCold(context.Background(), ws.ID,
		ColdResume{VendorSessionID: "vendor-1", Remediation: payRemediation()})

	// Assert.
	asRefusal(t, err, ArmNoSession)
}

func TestResumeColdSurfacesAShimRefusal(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	parkedGate(t, f, ws)
	f.client.startErr = errors.New("the link is gone")

	// Act.
	err := f.fleet.ResumeCold(context.Background(), ws.ID,
		ColdResume{VendorSessionID: "vendor-1", Remediation: payRemediation()})

	// Assert.
	if err == nil {
		t.Fatal("ResumeCold() = nil error, want the failed re-open surfaced")
	}
}

// liveHostSessionID reads the identity the fleet is operating a workspace's
// session under -- the same value its shim was spawned with.
func liveHostSessionID(t *testing.T, fleet *Fleet, ws ids.WorkspaceID) string {
	t.Helper()
	fleet.mu.RLock()
	defer fleet.mu.RUnlock()
	session, ok := fleet.sessions[ws]
	if !ok {
		t.Fatalf("the fleet has no live session for %q", ws)
	}
	return session.hostSessionID
}

// sessionStamps returns the agent_repl_session_id of every captured record
// that carries one.
func sessionStamps(records []dlog.Record) []string {
	var out []string
	for _, r := range records {
		if id, ok := r.Context[dlog.KeyAgentReplSessionID].(string); ok {
			out = append(out, id)
		}
	}
	return out
}

func TestStartStampsTheSessionIdentityOnTheWorkspaceRecords(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert: the bring-up's own records carry the identity the shim was
	// spawned with, so the two runtimes' records join on it.
	want := liveHostSessionID(t, f.fleet, ws.ID)
	stamps := sessionStamps(f.log.logger.Records())
	if len(stamps) == 0 {
		t.Fatalf("no record carried %s; records = %+v", dlog.KeyAgentReplSessionID, f.log.logger.Records())
	}
	for _, got := range stamps {
		if got != want {
			t.Fatalf("%s = %q, want the session's host identity %q", dlog.KeyAgentReplSessionID, got, want)
		}
	}
}

func TestStartStampsNothingBeforeTheIdentityIsDecided(t *testing.T) {
	// Arrange: a refused start never reaches the mint, so no record may claim
	// a session identity.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{
		Workspace: ws.ID, VendorSessionID: "vendor-1",
		Terminal: &wsm.SessionTerminal{Kind: "deleted", Detail: "/clear"},
	}

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err == nil {
		t.Fatal("Start() = nil error, want the deleted session refused")
	}

	// Assert.
	if stamps := sessionStamps(f.log.logger.Records()); len(stamps) != 0 {
		t.Fatalf("%s stamped %v on a start that never minted one", dlog.KeyAgentReplSessionID, stamps)
	}
}

func TestStartRestampsTheRotatedIdentityOnAFreshRestart(t *testing.T) {
	// Arrange: a fresh start after a stop mints a new identity, and the
	// records of the second start must carry only that one.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("first Start: %v", err)
	}
	first := liveHostSessionID(t, f.fleet, ws.ID)
	if err := f.fleet.Stop(context.Background(), ws.ID, true); err != nil {
		t.Fatalf("Stop: %v", err)
	}
	// The recorded conversation is gone, so the second start is FRESH and
	// mints a second identity rather than keeping the first.
	delete(f.db.sessions, ws.ID)
	before := len(f.log.logger.Records())

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("second Start: %v", err)
	}

	// Assert.
	second := liveHostSessionID(t, f.fleet, ws.ID)
	if second == first {
		t.Fatalf("the restart kept host session id %q; the fixture must rotate it", first)
	}
	for _, got := range sessionStamps(f.log.logger.Records()[before:]) {
		if got != second {
			t.Fatalf("%s = %q after the restart, want the rotated identity %q", dlog.KeyAgentReplSessionID, got, second)
		}
	}
}

// StandDownEverySpawn is the supervisor's own sweep of processes it started
// and still owns. These fakes spawn no process, so there is never one to
// sweep.
func (s *fakeSupervisor) StandDownEverySpawn(context.Context, string) error { return nil }

// TestStopTellsTheViewsTheLinkIsDeadEvenWhenTheKillFails covers the other path
// out of Stop. A kill that reports a failure -- and Client.Kill can now report
// one, because its caller's context bounds its waits -- has still sent
// everything it is going to send, and the session left this fleet's map before
// it ran. Views left showing a live link for a session nothing serves is the
// worse answer than an error with the views correct.
func TestStopTellsTheViewsTheLinkIsDeadEvenWhenTheKillFails(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}
	f.links.links = nil
	f.client.killErr = errors.New("the exit decode did not land inside the caller's bound")

	// Act
	err := f.fleet.Stop(context.Background(), ws.ID, false)

	// Assert
	if err == nil {
		t.Fatal("Stop() error = nil, want the kill's failure surfaced")
	}
	want := []sessionwatcher.LinkState{
		shimclient.LinkDead, shimclient.LinkDead, shimclient.LinkDead,
	}
	if len(f.links.links) != len(want) {
		t.Fatalf("OnLink calls = %v, want the footer, topbar and roster each told the link is dead", f.links.links)
	}
}

// TestStartRevivesAWorkspaceWhoseShimWasReaped covers the other half of the
// same defect: the dead shim's row made the bring-up answer "already live", so
// the revival a held prompt waits on brought nothing up at all.
func TestStartRevivesAWorkspaceWhoseShimWasReaped(t *testing.T) {
	// Arrange: a session came up, then its shim was killed out from under the
	// daemon and reaped.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}
	f.client.reaped = true
	fresh := &fakeClient{response: startedResponse("vendor-2"), pid: 5151, standDown: f.standDown}
	f.supervisor.client = fresh

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start after the shim died: %v", err)
	}

	// Assert.
	if len(f.supervisor.spawns) != 2 {
		t.Fatalf("spawns = %d, want a second shim spawned for the revival", len(f.supervisor.spawns))
	}
	client, live := f.fleet.Client(ws.ID)
	if !live {
		t.Fatal("the revived workspace has no live client; every prompt would answer no_session")
	}
	if client != shimclient.Client(fresh) {
		t.Fatalf("Client() = %v, want the freshly spawned shim", client)
	}
	if !f.watcher.closed {
		t.Fatal("the dead session's watches were left open by the revival")
	}
}

// TestStartDialsAnAdoptionUnderTheAdoptionBound pins that the adoption is
// dialed under a DEADLINE and never under the caller's bare context.
// shimclient's dial ladder has no attempt limit, and an adopted client
// concludes death only when the socket is gone AND the workspace lock reads
// free, so a lock that reads held for a shim whose socket path is gone is
// redialed forever: the caller's own bound is the only thing that ends it.
func TestStartDialsAnAdoptionUnderTheAdoptionBound(t *testing.T) {
	// Arrange: a held lock selects the adopt path.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}
	f.probeState = sessionlock.StateHeld
	f.socketState = shimsocket.StateLive

	// Act: the caller's context carries no deadline of its own.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if !f.supervisor.adoptHadDeadline {
		t.Fatal("the adoption was dialed under a context with no deadline, want the adoption bound")
	}
}

// TestStartBoundsAnAdoptionAtTheStatedBound pins that the deadline the
// adoption is dialed under is the fleet's stated bound and not something
// longer.
func TestStartBoundsAnAdoptionAtTheStatedBound(t *testing.T) {
	// Arrange.
	f := newFleetFixtureBoundedAt(t, 30*time.Second)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}
	f.probeState = sessionlock.StateHeld
	f.socketState = shimsocket.StateLive

	// Act.
	before := time.Now()
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}
	after := time.Now()

	// Assert: the deadline is the bound measured from an instant inside the
	// call, so it cannot fall outside [before+bound, after+bound] — an exact
	// window rather than a tolerance.
	if f.supervisor.adoptDeadline.Before(before.Add(f.adoptBound)) ||
		f.supervisor.adoptDeadline.After(after.Add(f.adoptBound)) {
		t.Fatalf("the adoption's deadline was %v, want the stated bound %v measured from inside the call",
			f.supervisor.adoptDeadline, f.adoptBound)
	}
}

// TestStartNamesAnAdoptionThatSpentItsWholeBound pins the give-up's own
// sentence. Before the bound, this scenario was a caller that never returned
// and a workspace log that said nothing after "adopting a running shim".
func TestStartNamesAnAdoptionThatSpentItsWholeBound(t *testing.T) {
	// Arrange: a shim whose socket never answers, under a bound short enough
	// to observe.
	f := newFleetFixtureBoundedAt(t, time.Millisecond)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}
	f.probeState = sessionlock.StateHeld
	f.socketState = shimsocket.StateLive
	f.supervisor.adoptBlocks = true

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if err == nil {
		t.Fatal("Start = nil error, want the adoption's give-up")
	}
	if !strings.Contains(err.Error(), "did not answer within") {
		t.Fatalf("Start error = %q, want it to name the bound it spent", err)
	}
}

// TestStartReportsACancelledAdoptionAsItsOwnCause pins that a caller that went
// away is NOT reported as an overrun: the two send a reader to different
// places, and only the caller's context tells them apart.
func TestStartReportsACancelledAdoptionAsItsOwnCause(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}
	f.probeState = sessionlock.StateHeld
	f.socketState = shimsocket.StateLive
	f.supervisor.adoptBlocks = true
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	err := f.fleet.Start(ctx, ws.ID)

	// Assert.
	if err == nil {
		t.Fatal("Start = nil error, want the cancellation")
	}
	if strings.Contains(err.Error(), "did not answer within") {
		t.Fatalf("Start error = %q, want the cancellation rather than an overrun", err)
	}
}

// TestTheColdGateIsRecordedAtInfo pins the gate's LEVEL. A cold refusal is the
// designed answer to resuming a large context -- the cost is published to the
// footer and the feed and the user chooses pay, clear or compact -- so it
// carries no defect and must not stand in a log the owner reads for defects.
func TestTheColdGateIsRecordedAtInfo(t *testing.T) {
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
	var found bool
	for _, record := range f.log.logger.Records() {
		if record.Message != "the session is parked behind a cold gate" {
			continue
		}
		found = true
		if record.Level != "info" {
			t.Fatalf("the cold-gate record is %q, want info", record.Level)
		}
	}
	if !found {
		t.Fatal("no cold-gate record was written")
	}
}

// TestTwoStartsOfOneWorkspaceSpawnOneShim pins the start gate.
//
// The liveness check cannot serialize starts: a session is remembered only
// after its shim is up, so two starts that overlap both read "not live". The
// second one then reached the workspace's socket, found the first one's shim
// listening behind a lock its StartSession had not taken yet, and attached to
// it as an INERT SURVIVOR — a warning about a race rather than a session. It
// stopped being hypothetical when the boot's bring-up moved beside the accept
// loop: a relaunch has Emacs announcing its workspaces while the boot is still
// starting their sessions.
func TestTwoStartsOfOneWorkspaceSpawnOneShim(t *testing.T) {
	// Arrange: the first start is held open inside its spawn.
	f := newFleetFixture(t)
	ws := f.workspace("w-race")
	spawning := make(chan struct{})
	release := make(chan struct{})
	var once sync.Once
	f.supervisor.onSpawn = func() {
		// ONCE, so a SECOND spawn — the defect this test is about — reports
		// itself as the count below rather than as a panic on a closed
		// channel.
		once.Do(func() { close(spawning) })
		<-release
	}
	first := make(chan error, 1)
	go func() { first <- f.fleet.Start(context.Background(), ws.ID) }()
	<-spawning

	// Act: a second start of the SAME workspace, while the first is still
	// inside its spawn.
	second := make(chan error, 1)
	entered := make(chan struct{})
	go func() {
		close(entered)
		second <- f.fleet.Start(context.Background(), ws.ID)
	}()
	<-entered
	close(release)

	// Assert: both callers got a session, and only one shim was spawned.
	if err := <-first; err != nil {
		t.Fatalf("the first Start = %v, want the session started", err)
	}
	if err := <-second; err != nil {
		t.Fatalf("the second Start = %v, want the session the first one brought up", err)
	}
	if len(f.supervisor.spawns) != 1 {
		t.Fatalf("spawns = %d, want exactly one shim for one workspace", len(f.supervisor.spawns))
	}
}

// ---- a start nobody is waiting for ----

// TestStartDetachedDoesNotHoldItsCaller is the contract RegisterWorkspace
// depends on: the caller hands the fleet a workspace to bring up and gets its
// goroutine back, whatever the start is doing.
//
// MEASURED, realtest run 2026-09-13T16:20:34: the register's revival ran
// inline, `Fleet.Start` took the workspace's start gate behind the boot's own
// bring-up, and the shim never answered that bring-up's StartSession. Emacs
// timed the register out at its 10s bound on three consecutive daemon
// generations for a roster row the daemon had already written.
func TestStartDetachedDoesNotHoldItsCaller(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("ws-detached")
	entered := make(chan struct{})
	release := make(chan struct{})
	f.client.entered = entered
	f.client.startHold = release
	settled := make(chan error, 1)

	// Act.
	f.fleet.StartDetached(ws.ID, func(err error) { settled <- err })
	<-entered

	// Assert.
	select {
	case err := <-settled:
		t.Fatalf("the detached start settled (%v) while its StartSession was still in flight", err)
	default:
	}
	close(release)
	if err := <-settled; err != nil {
		t.Fatalf("the detached start = %v, want the session up", err)
	}
}

// TestDrainStartsEndsAnInFlightStartAndJoinsIt covers the exit. A start nobody
// waits for still reads and writes the state client, so the teardown ends it
// and joins it BEFORE that client closes — the same rule the prompt queue's
// background work is drained under.
func TestDrainStartsEndsAnInFlightStartAndJoinsIt(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("ws-drained")
	entered := make(chan struct{})
	f.client.entered = entered
	f.client.startHold = make(chan struct{}) // never closed: only the drain ends it.
	settled := make(chan error, 1)
	f.fleet.StartDetached(ws.ID, func(err error) { settled <- err })
	<-entered

	// Act.
	left := f.fleet.DrainStarts(time.Minute)

	// Assert.
	if !left {
		t.Fatal("DrainStarts = false, want the in-flight start ended and joined")
	}
	if err := <-settled; !errors.Is(err, context.Canceled) {
		t.Fatalf("the drained start = %v, want a cancellation", err)
	}
}

// TestDrainStartsReportsAStartThatOutlivesItsBound is the other half: the
// drain answers false rather than waiting forever, so the exit can say loudly
// that it is tearing down under a start instead of hanging.
func TestDrainStartsReportsAStartThatOutlivesItsBound(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("ws-stuck")
	entered := make(chan struct{})
	stuck := make(chan struct{})
	t.Cleanup(func() { close(stuck) })
	f.client.entered = entered
	// The START finishes; what outlives the drain is the SETTLEMENT, which
	// blocks on `stuck` and never looks at a context at all.
	f.fleet.StartDetached(ws.ID, func(error) { <-stuck })
	<-entered

	// Act.
	left := f.fleet.DrainStarts(10 * time.Millisecond)

	// Assert.
	if left {
		t.Fatal("DrainStarts = true, want false for a start still running at the bound")
	}
}

// TestABringUpStandDownIsNotAFailedStartSession is the fleet's half of the
// same distinction the shim client draws. A StartSession that came back
// because THIS DAEMON killed the shim it was asking is the teardown arriving,
// not a session that would not come up, and the bring-up says so.
//
// MEASURED, realtest run 2026-09-13T16:20:34: `daemon.workspace.bring_up: the
// StartSession call failed` at ERROR on three consecutive daemon generations,
// each one milliseconds after the same process's own
// `daemon.shimclient.standdown` for the same shim.
func TestABringUpStandDownIsNotAFailedStartSession(t *testing.T) {
	tests := []struct {
		name      string
		startErr  error
		wantLevel string
	}{
		{
			name:      "the daemon stood the shim down under the start",
			startErr:  fmt.Errorf("%w: unavailable: unexpected EOF", shimclient.ErrStandDownOrdered),
			wantLevel: "info",
		},
		{
			name:      "the shim link broke on its own",
			startErr:  errors.New("unavailable: unexpected EOF"),
			wantLevel: "error",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := f.workspace("ws-stood-down")
			f.client.startErr = tt.startErr

			// Act.
			err := f.fleet.Start(context.Background(), ws.ID)

			// Assert.
			if err == nil {
				t.Fatal("Start answered success though StartSession failed")
			}
			var level string
			for _, r := range f.log.logger.Records() {
				if r.Operation == opBringUp && strings.Contains(r.Message, "StartSession call") {
					level = r.Level
				}
			}
			if level != tt.wantLevel {
				t.Fatalf("the StartSession failure is recorded at %q, want %q: %+v", level, tt.wantLevel, f.log.logger.Records())
			}
		})
	}
}

// ---- a start that never answers ----

// TestAStartSessionThatNeverAnswersEndsAtItsBound pins the bound itself. A
// shim that accepts the start and goes quiet used to hold the call, and the
// workspace's start gate with it, until the process died.
//
// MEASURED, realtest run 2026-09-13T16:20:34: workspace 2b81f45a724642ef's
// shim logged `shim.convert.hooks: a hook blocked the gated action` on
// `SessionStart:resume` and never answered. Three daemon generations sat in
// that call for 35s, 3m30s and 8m30s, each ended only by the NEXT deploy's
// SIGTERM, with nothing in the daemon's log saying what it was waiting on.
func TestAStartSessionThatNeverAnswersEndsAtItsBound(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("ws-quiet-shim")
	f.fleet.startBound = 10 * time.Millisecond
	f.client.startHold = make(chan struct{}) // never closed: only the bound ends it.

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if err == nil {
		t.Fatal("Start answered success though the shim never answered StartSession")
	}
	if !strings.Contains(err.Error(), "did not answer StartSession") {
		t.Fatalf("Start = %v, want an error naming the unanswered StartSession", err)
	}
}

// TestAnUnansweredStartNamesWhatItWasWaitingOn is the log half. "context
// deadline exceeded" says nothing about which step spent the bound, and this
// step is the difference between a shim that is not there and a shim that took
// the request and went quiet — two states with two different remediations.
func TestAnUnansweredStartNamesWhatItWasWaitingOn(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("ws-quiet-shim")
	f.fleet.startBound = 10 * time.Millisecond
	f.client.startHold = make(chan struct{})

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	var found bool
	for _, r := range f.log.logger.Records() {
		if r.Operation == opBringUp && strings.Contains(r.Message, "did not answer inside its bound") {
			found = true
			if r.Level != "error" {
				t.Fatalf("the unanswered start is recorded at %q, want error", r.Level)
			}
		}
	}
	if !found {
		t.Fatalf("no record names the unanswered start: %+v", f.log.logger.Records())
	}
}

// TestTheStartBoundDoesNotSpeakForTheCallersOwnCancellation is the edge that
// keeps the bound honest: when it is the CALLER that went away, the shim was
// never given its bound to answer in, and saying it went quiet would blame the
// shim for the daemon's own exit.
func TestTheStartBoundDoesNotSpeakForTheCallersOwnCancellation(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("ws-caller-left")
	f.fleet.startBound = time.Minute
	entered := make(chan struct{})
	f.client.entered = entered
	f.client.startHold = make(chan struct{})
	ctx, cancel := context.WithCancel(context.Background())
	settled := make(chan error, 1)
	go func() { settled <- f.fleet.Start(ctx, ws.ID) }()
	<-entered

	// Act.
	cancel()

	// Assert.
	if err := <-settled; err == nil {
		t.Fatal("Start answered success though its caller was cancelled")
	}
	for _, r := range f.log.logger.Records() {
		if strings.Contains(r.Message, "did not answer inside its bound") {
			t.Fatalf("a cancelled caller was reported as a shim that went quiet: %+v", r)
		}
	}
}
