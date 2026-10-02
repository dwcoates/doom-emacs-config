package workspace

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"google.golang.org/protobuf/proto"

	"claude-repld/internal/account"
	"claude-repld/internal/dlog"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/sidebar"
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

	// efforts is every SetSessionEffort asked; effortResponse and effortErr
	// script the answer (a nil response confirms the level asked for).
	efforts        []*shimv1.SetSessionEffortRequest
	effortResponse *shimv1.SetSessionEffortResponse
	effortErr      error

	// detached counts Detach calls: the link let go, the process left running.
	detached int

	requests []*shimv1.StartSessionRequest
	// responses, when set, are answered one per StartSession, in order, before
	// `response` answers every call after them.
	responses []*shimv1.StartSessionResponse
	response  *shimv1.StartSessionResponse
	startErr  error
	pid       int
	kills     []shimclient.KillAttribution
	killErr   error
	// standDown is the fixture's shared step order, appended to on the shim's
	// own KillSession.
	standDown *[]string
	// stoodDown records that the stand-down latch was armed, and
	// standDownBeforeKill that it was armed BEFORE the KillSession ask.
	stoodDown           bool
	standDownBeforeKill bool
	// standDownBeforeStop is the same question for the PROCESS stop, which is
	// where a failed start's shim is ended: the latch has to be armed before
	// the process goes, or the departure reads as a death nobody ordered.
	standDownBeforeStop bool
	// standDownRefused is the DETACHED shape: the latch arms nothing, because
	// that process belongs to the successor daemon.
	standDownRefused bool
	// killSessionErr makes the session directive fail, which is what sends the
	// verb down its escalation path.
	killSessionErr error
	// sessionFrames are the WatchSession frames this shim pushes WHILE its
	// StartSession runs, and sessionStream is the channel the opened watch
	// reads them from. watchSessionErr refuses the watch outright.
	sessionFrames   []*shimv1.WatchSessionResponse
	sessionStream   chan *shimv1.WatchSessionResponse
	watchSessionErr error
	// reaped makes the supervised process ALREADY GONE, which is how a test
	// reaches the split between a session row and a live shim; reapedAt is
	// when its death was concluded.
	reaped   bool
	reapedAt time.Time
	// entered is closed by StartSession on its first call and startHold is
	// what it then waits on, which is how a test holds a start open for as
	// long as it needs to observe something about the caller that is NOT
	// waiting for it. A nil startHold never waits.
	entered   chan struct{}
	startHold chan struct{}
	// onKill is the supervisor's deregistration, armed by the fake spawn: the
	// real client releases the supervisor's hold on its exit decode.
	onKill func()
	// killTurns records every KillTurn request, and killTurnErr fails it.
	killTurns   []*shimv1.KillTurnRequest
	killTurnErr error
	// startTurns records every StartTurn request; each is accepted.
	startTurns []*shimv1.StartTurnRequest
	// historyReads counts ReadHistory calls; history is the answer, a floor
	// page of nothing when unset.
	historyReads int
	history      *shimv1.ReadHistoryResponse
}

func (c *fakeClient) StartTurn(_ context.Context, req *shimv1.StartTurnRequest) (*shimv1.StartTurnResponse, error) {
	c.startTurns = append(c.startTurns, req)
	return &shimv1.StartTurnResponse{Result: &shimv1.StartTurnResponse_Success{Success: &shimv1.StartTurnSuccess{
		Prompt: &conversationv1.AgentPrompt{Id: req.GetTurn(), Agent: &conversationv1.AgentId{Value: "main-agent"}},
		Page:   &conversationv1.HistoryPage{},
	}}}, nil
}

func (c *fakeClient) KillTurn(_ context.Context, req *shimv1.KillTurnRequest) (*shimv1.KillTurnResponse, error) {
	c.killTurns = append(c.killTurns, req)
	if c.killTurnErr != nil {
		return nil, c.killTurnErr
	}
	return &shimv1.KillTurnResponse{Result: &shimv1.KillTurnResponse_Success{Success: &shimv1.KillTurnSuccess{}}}, nil
}

func (c *fakeClient) Reaped() (shimclient.ExitInfo, bool) {
	if !c.reaped {
		return shimclient.ExitInfo{}, false
	}
	return shimclient.ExitInfo{PID: c.pid, Signal: "SIGKILL", At: c.reapedAt}, true
}

func (c *fakeClient) StartSession(ctx context.Context, req *shimv1.StartSessionRequest) (*shimv1.StartSessionResponse, error) {
	c.requests = append(c.requests, req)
	// THE SHIM PUSHES WHILE StartSession RUNS, which is the whole reason the
	// phases need a watch opened before the call. The fake does the same: the
	// sends are UNBUFFERED, so this returns only once the relay has taken
	// every frame, and no test has to wait for one.
	for _, frame := range c.sessionFrames {
		select {
		case c.sessionStream <- frame:
		case <-ctx.Done():
			return nil, ctx.Err()
		}
	}
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
	if len(c.responses) > 0 {
		next := c.responses[0]
		c.responses = c.responses[1:]
		return next, nil
	}
	return c.response, nil
}

// WatchSession opens the fake's session frame stream.
func (c *fakeClient) WatchSession(ctx context.Context) (shimclient.Stream[*shimv1.WatchSessionResponse], error) {
	if c.watchSessionErr != nil {
		return nil, c.watchSessionErr
	}
	if c.sessionStream == nil {
		c.sessionStream = make(chan *shimv1.WatchSessionResponse)
	}
	return &fakeSessionStream{frames: c.sessionStream, ctx: ctx, closed: make(chan struct{})}, nil
}

// fakeSessionStream is one opened WatchSession: it blocks for the next frame
// and ends on the request's context or on Close, exactly as the real one does.
type fakeSessionStream struct {
	frames <-chan *shimv1.WatchSessionResponse
	ctx    context.Context
	closed chan struct{}
	once   sync.Once
}

func (s *fakeSessionStream) Recv() (*shimv1.WatchSessionResponse, error) {
	select {
	case frame := <-s.frames:
		return frame, nil
	case <-s.ctx.Done():
		return nil, s.ctx.Err()
	case <-s.closed:
		return nil, io.EOF
	}
}

func (s *fakeSessionStream) Close() { s.once.Do(func() { close(s.closed) }) }

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

// SetSessionEffort records the ask and answers the scripted outcome; with none
// scripted it confirms the level asked for.
func (c *fakeClient) SetSessionEffort(_ context.Context, req *shimv1.SetSessionEffortRequest) (*shimv1.SetSessionEffortResponse, error) {
	c.efforts = append(c.efforts, req)
	if c.effortErr != nil || c.effortResponse != nil {
		return c.effortResponse, c.effortErr
	}
	return &shimv1.SetSessionEffortResponse{Result: &shimv1.SetSessionEffortResponse_Success{
		Success: &shimv1.SetSessionEffortSuccess{
			EffortChanged: &conversationv1.SessionEffortChanged{EffectiveEffort: req.GetEffort()},
		},
	}}, nil
}

func (c *fakeClient) Kill(_ context.Context, attr shimclient.KillAttribution) error {
	if c.killErr != nil {
		return c.killErr
	}
	c.kills = append(c.kills, attr)
	c.standDownBeforeStop = c.stoodDown
	// A KILL THAT RETURNS HAS WITNESSED THE REAP, exactly as the real client's
	// does: its exit is concluded before the call answers.
	c.reaped = true
	if c.reapedAt.IsZero() {
		c.reapedAt = fixedNow
	}
	// THE SUPERVISOR LETS GO WHEN THE PROCESS DOES, exactly as the real
	// client's exit decode releases its hold: a spawn registry that still
	// named a killed process would answer "ours" for a shim that is gone.
	if c.onKill != nil {
		c.onKill()
	}
	return nil
}

// fakeSupervisor records which bring-up path the lock probe selected.
type fakeSupervisor struct {
	// mu guards standingDown, which a test may latch from another goroutine.
	mu sync.Mutex
	// standingDown is the supervisor's stand-down latch.
	standingDown bool

	client *fakeClient
	spawns []shimclient.Spec
	adopts []string
	// spawned is the live spawn registry the real supervisor keeps: every
	// process it started and still owns, by workspace, entered at the spawn
	// and left at the kill.
	spawned  map[ids.WorkspaceID]int
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
	// spawnAttempts counts every ASK, including the ones that answer an
	// error. `spawns` records only what came up, so a guard that is supposed
	// to prevent the ask cannot be tested against it.
	spawnAttempts int
}

func (s *fakeSupervisor) Spawn(_ context.Context, spec shimclient.Spec) (shimclient.Client, error) {
	s.spawnAttempts++
	if s.onSpawn != nil {
		s.onSpawn()
	}
	// THE LATCH IS THE REAL SUPERVISOR'S BACKSTOP, so the fake carries it too:
	// a fixture that spawned happily while standing down would let a guard
	// that never reads the latch pass every test about reading it.
	if s.StandingDown() {
		return nil, shimclient.ErrStandingDown
	}
	if s.spawnErr != nil {
		return nil, s.spawnErr
	}
	s.spawns = append(s.spawns, spec)
	if s.spawned == nil {
		s.spawned = make(map[ids.WorkspaceID]int)
	}
	s.spawned[spec.WorkspaceID] = s.client.pid
	// THE FORK'S PID REACHES THE CALLER BEFORE ANYTHING BLOCKS, exactly as the
	// real supervisor hands it over between cmd.Start and the bring-up. A fake
	// that skipped it would let the durable-pid invariant pass every test
	// about it without ever being written.
	if spec.Spawned != nil {
		spec.Spawned(s.client.pid)
	}
	// THE TEST'S OWN HOOK SURVIVES the supervisor's deregistration: a scenario
	// that watches the socket go when the process does arms it before the
	// spawn, and losing it here would make the socket outlive the shim in
	// every one of them.
	arranged := s.client.onKill
	s.client.onKill = func() {
		delete(s.spawned, spec.WorkspaceID)
		if arranged != nil {
			arranged()
		}
	}
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
	// turn is what TurnInFlight answers, nil for an idle session.
	turn *ids.TurnID
	// standDown is the fixture's shared step order, appended to when the
	// daemon declares the session ending.
	standDown *[]string
	// pointers is what Pointers answers.
	pointers sessionwatcher.Pointers
	// onClose, when set, runs as Close begins, so a test observes what the
	// close met.
	onClose func()
	// closeErr is what Close answers.
	closeErr error
	// factsErr is what AwaitSessionFacts answers.
	factsErr error
}

// Pointers answers the pointers the fixture states; a fixture that states none
// is a watcher that was served nothing.
func (w *fakeWatcher) Pointers() sessionwatcher.Pointers { return w.pointers }

// MainKnownThrough answers the main watch's pointer the fixture states.
func (w *fakeWatcher) MainKnownThrough() *conversationv1.HistoryPointer { return w.pointers.Main }

// NoteHistoryLoaded takes nothing up: no fixture here loads history.
func (w *fakeWatcher) NoteHistoryLoaded(*conversationv1.AgentId, *conversationv1.HistoryPage, bool) {}

func (w *fakeWatcher) SessionEnding(string) {
	if w.standDown != nil {
		*w.standDown = append(*w.standDown, "watcher.SessionEnding")
	}
}

func (w *fakeWatcher) Close() error {
	if w.onClose != nil {
		w.onClose()
	}
	if w.closeErr != nil {
		return w.closeErr
	}
	w.closed = true
	return nil
}

func (w *fakeWatcher) Connected() bool { return true }

// AwaitSessionFacts answers the facts wait the fixture states: at once, with
// factsErr.
func (w *fakeWatcher) AwaitSessionFacts(context.Context) error { return w.factsErr }

func (w *fakeWatcher) TurnInFlight() *ids.TurnID { return w.turn }

func (w *fakeWatcher) OnTurnOpening(ids.WorkspaceID, ids.TurnID) {}

func (w *fakeWatcher) OnTurnJoining(ids.WorkspaceID, ids.TurnID) bool { return false }

func (w *fakeWatcher) OnTurnOpenFailed(ids.WorkspaceID, ids.TurnID) {}

func (w *fakeWatcher) SetMainAgent(*conversationv1.AgentId) {}

func (w *fakeWatcher) OnTurnOpened(ids.WorkspaceID, *conversationv1.AgentPrompt, *conversationv1.HistoryPage) {
}

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

// fixtureInstance is the daemon instance every fixture fleet serves as.
const fixtureInstance = ids.InstanceID("fleet-fixture-instance")

// fleetFixture is one arranged Fleet plus the fakes behind it.
// bringUpEdge is one BringUps call the fleet made.
type bringUpEdge struct {
	ws       ids.WorkspaceID
	underWay bool
}

// rejectedVendorStart is a vendor-start refusal the shim labeled REJECTED.
func rejectedVendorStart() *shimv1.StartSessionVendorStartFailed {
	return &shimv1.StartSessionVendorStartFailed{
		Retry: &shimv1.StartSessionVendorStartFailed_Rejected{Rejected: &shimv1.StartSessionVendorStartRejected{}}}
}

// retryableVendorStart is a vendor-start refusal the shim labeled RETRYABLE
// and blamed on the vendor.
func retryableVendorStart() *shimv1.StartSessionVendorStartFailed {
	return &shimv1.StartSessionVendorStartFailed{
		Retry: &shimv1.StartSessionVendorStartFailed_Retryable{Retryable: &shimv1.StartSessionVendorStartRetryable{
			Cause: &shimv1.StartSessionVendorStartRetryable_Vendor{Vendor: &shimv1.StartSessionVendorStartVendor{}}}}}
}

// offlineVendorStart is a RETRYABLE vendor-start refusal the shim blamed on
// this machine not reaching the network.
func offlineVendorStart() *shimv1.StartSessionVendorStartFailed {
	return &shimv1.StartSessionVendorStartFailed{
		Retry: &shimv1.StartSessionVendorStartFailed_Retryable{Retryable: &shimv1.StartSessionVendorStartRetryable{
			Cause: &shimv1.StartSessionVendorStartRetryable_Network{Network: &shimv1.StartSessionVendorStartNetwork{}}}}}
}

// vendorRefusal is a StartSession answer refusing the start with this vendor
// label and detail.
func vendorRefusal(label *shimv1.StartSessionVendorStartFailed, detail string) *shimv1.StartSessionResponse {
	return &shimv1.StartSessionResponse{
		Result: &shimv1.StartSessionResponse_Failure{Failure: &shimv1.StartSessionFailure{
			Cause:  &shimv1.StartSessionFailure_VendorStartFailed{VendorStartFailed: label},
			Detail: detail,
		}},
	}
}

type fleetFixture struct {
	// now is the fleet's clock. It moves only when the vendor-start run waits
	// (retryAfter), so the retry window is crossed without any real wait.
	now time.Time
	// retryWaits is every wait the vendor-start run asked for, in order.
	retryWaits []time.Duration
	// retryAfter, when set, answers the run's wait instead of the default,
	// which advances `now` by the wait and fires at once.
	retryAfter func(d time.Duration) <-chan time.Time
	// vendorStarts is every vendor-start state the roster was told, in order.
	vendorStarts []sidebar.VendorStart
	// sessionsUp is every workspace the session-up hook was told about.
	sessionsUp []ids.WorkspaceID
	// bringUps records every BringUps edge, in order.
	bringUps []bringUpEdge
	// bundle is the installed shim bundle every spawn holds.
	bundle     *fakeBundle
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
	// picked is the effort level the topbar holds as picked, and
	// topbarWarnings every warning line the fleet raised on the strip.
	picked         conversationv1.AgentEffortLevel
	topbarWarnings []string
	log            *fakeSurfaces
	watcher        *fakeWatcher
	links          *recordingLinkSink
	probeState     sessionlock.State
	probeErr       error
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
	// onSocketProbe runs on every socket probe, before it answers, so a
	// scenario about a socket that CHANGES -- a shim binding it at the spawn,
	// or letting it go a moment after the stop -- can drive the change from
	// the probe itself rather than from a clock.
	onSocketProbe func(path string)
	// adoptBound is the fixture's adoption bound, generous by default so no
	// ordinary scenario can trip it; a scenario about the give-up shortens it.
	adoptBound time.Duration
	// standDown is the ORDER the stand-down's steps happened in, shared by the
	// fake watcher and the fake client, because the ordering is the guarantee.
	standDown *[]string
	// shimAlive scripts the kernel's answer about a RECORDED spawn's pid,
	// which is what tells a shim that is still starting from no shim at all.
	// Nil means every recorded pid is dead, which is what every scenario that
	// is not about the starting window wants.
	shimAlive func(pid int) bool
	// openings is every Opening a watcher was started with, in order.
	openings []sessionwatcher.Opening
	// openAtAttach is every watcher start's Session.OpenAtAttach, in order.
	openAtAttach [][]sessionwatcher.OpenTurn
	// watchErr, when set, is what starting a watcher answers.
	watchErr error
}

// fleetStepClock is a Clock that never sleeps: After fires at once and ADVANCES
// the clock, so a bounded poll pays its real passes in no wall time.
type fleetStepClock struct {
	mu  sync.Mutex
	now time.Time
}

func (c *fleetStepClock) Now() time.Time {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.now
}

func (c *fleetStepClock) After(d time.Duration) <-chan time.Time {
	c.mu.Lock()
	c.now = c.now.Add(d)
	fired := c.now
	c.mu.Unlock()
	ch := make(chan time.Time, 1)
	ch <- fired
	return ch
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
		now:        fixedNow,
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
	f.bundle = &fakeBundle{build: "installed-build"}

	fleet, err := NewFleet(FleetDeps{
		DB: f.db, Instance: fixtureInstance, Accounts: f.accounts, Supervisor: f.supervisor, ShimBundle: f.bundle,
		Feed: f.feed, Footer: f.footer, Topbar: stubTopbar{coldGates: &f.topbarGates, picked: &f.picked, warnings: &f.topbarWarnings}, Log: f.log,
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
		BringUps: func(ws ids.WorkspaceID, underWay bool) {
			f.bringUps = append(f.bringUps, bringUpEdge{ws, underWay})
		},
		VendorStarts: func(_ ids.WorkspaceID, state sidebar.VendorStart) {
			f.vendorStarts = append(f.vendorStarts, state)
		},
		SessionsUp: func(ws ids.WorkspaceID) { f.sessionsUp = append(f.sessionsUp, ws) },
		Probe:      func(string, string) (sessionlock.State, error) { return f.probeState, f.probeErr },
		SocketProbe: func(path string) (shimsocket.State, error) {
			if f.onSocketProbe != nil {
				f.onSocketProbe(path)
			}
			if state, ok := f.socketStates[path]; ok {
				return state, nil
			}
			return f.socketState, f.socketErr
		},
		StartWatcher: func(_ context.Context, _ ids.WorkspaceID, _ shimclient.Client, session sessionwatcher.Session, _ sessionwatcher.Sinks, _ dlog.Logger) (sessionwatcher.Watcher, error) {
			f.openings = append(f.openings, session.Opening)
			f.openAtAttach = append(f.openAtAttach, session.OpenAtAttach)
			if f.watchErr != nil {
				return nil, f.watchErr
			}
			return f.watcher, nil
		},
		Now:        func() time.Time { return f.now },
		AdoptBound: f.adoptBound,
		Clock:      &fleetStepClock{now: fixedNow},
		RetryAfter: func(d time.Duration) <-chan time.Time {
			f.retryWaits = append(f.retryWaits, d)
			if f.retryAfter != nil {
				return f.retryAfter(d)
			}
			f.now = f.now.Add(d)
			fired := make(chan time.Time, 1)
			fired <- f.now
			return fired
		},
		ShimAlive: func(pid int) bool { return f.shimAlive != nil && f.shimAlive(pid) },
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
		{name: "no instance id", deps: FleetDeps{DB: newFakeDB()}},
		{name: "no account resolver", deps: FleetDeps{DB: newFakeDB(), Instance: fixtureInstance}},
		{name: "no supervisor", deps: FleetDeps{DB: newFakeDB(), Instance: fixtureInstance, Accounts: &fakeAccounts{}}},
		{
			name: "no socket path",
			deps: FleetDeps{DB: newFakeDB(), Instance: fixtureInstance, Accounts: &fakeAccounts{}, Supervisor: &fakeSupervisor{}},
		},
		{
			name: "no shim bundle",
			deps: FleetDeps{
				DB: newFakeDB(), Instance: fixtureInstance, Accounts: &fakeAccounts{}, Supervisor: &fakeSupervisor{},
				SocketPath: func(ids.WorkspaceID) string { return "" },
			},
		},
		{
			name: "no log surfaces",
			deps: FleetDeps{
				DB: newFakeDB(), Instance: fixtureInstance, Accounts: &fakeAccounts{}, Supervisor: &fakeSupervisor{},
				SocketPath: func(ids.WorkspaceID) string { return "" }, ShimBundle: &fakeBundle{build: "b"},
			},
		},
		{
			name: "no lock directory",
			deps: FleetDeps{
				DB: newFakeDB(), Instance: fixtureInstance, Accounts: &fakeAccounts{}, Supervisor: &fakeSupervisor{},
				SocketPath: func(ids.WorkspaceID) string { return "" }, ShimBundle: &fakeBundle{build: "b"},
				Log: dlog.NewTestSurfaces(),
			},
		},
		{
			name: "a relative lock directory",
			deps: FleetDeps{
				DB: newFakeDB(), Instance: fixtureInstance, Accounts: &fakeAccounts{}, Supervisor: &fakeSupervisor{},
				SocketPath: func(ids.WorkspaceID) string { return "" }, ShimBundle: &fakeBundle{build: "b"},
				Log: dlog.NewTestSurfaces(), LockDir: "~/.cache/agent-repl/run",
			},
		},
		{
			name: "no bring-up marker",
			deps: FleetDeps{
				DB: newFakeDB(), Instance: fixtureInstance, Accounts: &fakeAccounts{}, Supervisor: &fakeSupervisor{},
				SocketPath: func(ids.WorkspaceID) string { return "" }, ShimBundle: &fakeBundle{build: "b"},
				Log: dlog.NewTestSurfaces(), LockDir: "/run",
			},
		},
		{
			name: "no vendor-start marker",
			deps: FleetDeps{
				DB: newFakeDB(), Instance: fixtureInstance, Accounts: &fakeAccounts{}, Supervisor: &fakeSupervisor{},
				SocketPath: func(ids.WorkspaceID) string { return "" }, ShimBundle: &fakeBundle{build: "b"},
				Log: dlog.NewTestSurfaces(), LockDir: "/run", BringUps: func(ids.WorkspaceID, bool) {},
				SessionsUp: func(ids.WorkspaceID) {},
			},
		},
		{
			name: "no session-up hook",
			deps: FleetDeps{
				DB: newFakeDB(), Instance: fixtureInstance, Accounts: &fakeAccounts{}, Supervisor: &fakeSupervisor{},
				SocketPath: func(ids.WorkspaceID) string { return "" }, ShimBundle: &fakeBundle{build: "b"},
				Log: dlog.NewTestSurfaces(), LockDir: "/run", BringUps: func(ids.WorkspaceID, bool) {},
				VendorStarts: func(ids.WorkspaceID, sidebar.VendorStart) {},
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
				loseRecordedTranscript(f)
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
	loseRecordedTranscript(f)
	engage(f)
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

func TestClassifySourceAdoptsIdleTranscriptWhenNoRecord(t *testing.T) {
	// Arrange: no session record, and an on-disk transcript last touched well
	// outside the idle window — the interactive-CLI conversation to continue.
	f := newFleetFixture(t)
	f.accounts.newest = account.AdoptableTranscript{
		Transcript:      account.Transcript{Path: "/config/projects/enc/adopt-me.jsonl", ConfigDir: "/config"},
		VendorSessionID: "adopt-me",
		ModTime:         fixedNow.Add(-2 * TranscriptAdoptionIdleWindow),
		LastRecordAt:    fixedNow.Add(-time.Hour),
	}

	// Act.
	got, err := f.fleet.classifySource(context.Background(), f.log.logger, "w1", "/tree/w1", wsm.Session{}, false)

	// Assert.
	if err != nil {
		t.Fatalf("classifySource: %v", err)
	}
	if got.Fresh || got.VendorSessionID != "adopt-me" {
		t.Fatalf("classifySource() = %+v, want a resume of adopt-me", got)
	}
}

func TestClassifySourceComesUpFreshWhenNewestTranscriptTooFresh(t *testing.T) {
	// Arrange: no session record, but the newest transcript was modified inside
	// the idle window — another writer may still hold it, so the idle guard
	// declines it and the session comes up fresh.
	f := newFleetFixture(t)
	f.accounts.newest = account.AdoptableTranscript{
		Transcript:      account.Transcript{Path: "/config/projects/enc/live.jsonl", ConfigDir: "/config"},
		VendorSessionID: "live",
		ModTime:         fixedNow.Add(-time.Second),
	}

	// Act.
	got, err := f.fleet.classifySource(context.Background(), f.log.logger, "w1", "/tree/w1", wsm.Session{}, false)

	// Assert.
	if err != nil {
		t.Fatalf("classifySource: %v", err)
	}
	if !got.Fresh || got.VendorSessionID != "" {
		t.Fatalf("classifySource() = %+v, want a fresh start (idle guard)", got)
	}
}

func TestClassifySourceComesUpFreshWhenNoTranscriptToAdopt(t *testing.T) {
	// Arrange: no session record and no transcript on disk at all.
	f := newFleetFixture(t)

	// Act.
	got, err := f.fleet.classifySource(context.Background(), f.log.logger, "w1", "/tree/w1", wsm.Session{}, false)

	// Assert.
	if err != nil {
		t.Fatalf("classifySource: %v", err)
	}
	if !got.Fresh {
		t.Fatalf("classifySource() = %+v, want a fresh start", got)
	}
	if !f.accounts.newestProbed {
		t.Fatalf("the no-record branch never probed for a transcript to adopt")
	}
}

func TestClassifySourceComesUpFreshWhenAdoptionProbeFails(t *testing.T) {
	// Arrange: no session record, and the probe itself errors — a probe failure
	// must fall back to fresh with a log, never crash or mis-route.
	f := newFleetFixture(t)
	f.accounts.newestErr = errors.New("readdir blew up")

	// Act.
	got, err := f.fleet.classifySource(context.Background(), f.log.logger, "w1", "/tree/w1", wsm.Session{}, false)

	// Assert.
	if err != nil {
		t.Fatalf("classifySource: %v", err)
	}
	if !got.Fresh {
		t.Fatalf("classifySource() = %+v, want a fresh start on probe failure", got)
	}
	if !recordedAt(f, dlog.LevelWarn, opBringUp, "could not probe for a transcript to adopt; the session comes up FRESH") {
		t.Fatalf("a failed adoption probe was not logged loudly")
	}
}

func TestClassifySourceWithRecordNeverProbesForAdoption(t *testing.T) {
	// Arrange: a workspace that DOES have a session record resumes its own
	// conversation exactly as before and never probes for a transcript to adopt.
	f := newFleetFixture(t)
	session := wsm.Session{VendorSessionID: "vendor-1"}

	// Act.
	got, err := f.fleet.classifySource(context.Background(), f.log.logger, "w1", "/tree/w1", session, true)

	// Assert.
	if err != nil {
		t.Fatalf("classifySource: %v", err)
	}
	if got.Fresh || got.VendorSessionID != "vendor-1" {
		t.Fatalf("classifySource() = %+v, want a resume of vendor-1", got)
	}
	if f.accounts.newestProbed {
		t.Fatalf("a recorded session must not probe for a transcript to adopt")
	}
}

func TestClassifySourceEmptyVendorIdRecordNeverProbesForAdoption(t *testing.T) {
	// Arrange: a record exists but names no conversation. Owner ruling: adoption
	// is confined to the NO-RECORD branch, so a record with an empty vendor id
	// still starts fresh WITHOUT probing.
	f := newFleetFixture(t)

	// Act.
	got, err := f.fleet.classifySource(context.Background(), f.log.logger, "w1", "/tree/w1", wsm.Session{}, true)

	// Assert.
	if err != nil {
		t.Fatalf("classifySource: %v", err)
	}
	if !got.Fresh {
		t.Fatalf("classifySource() = %+v, want a fresh start", got)
	}
	if f.accounts.newestProbed {
		t.Fatalf("a record with an empty vendor id must not probe for a transcript to adopt")
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
	loseRecordedTranscript(f)
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
	loseRecordedTranscript(f)
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
	loseRecordedTranscript(f)
	engage(f)
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
	loseRecordedTranscript(f)
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
	loseRecordedTranscript(f)
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

// THE REBIND MARKER IS WHAT MOVES THE SHIM'S BOOK, so which starts carry it is
// the whole behavior: a bind's start must, and every ordinary bring-up must
// not — a marked restart would let a rotated resume handle become the book and
// orphan everything recorded under the old name.
func TestStartMarksTheResumeARebindOnlyWhenTheCallerIsABind(t *testing.T) {
	tests := []struct {
		name       string
		start      func(*fleetFixture, ids.WorkspaceID) error
		wantRebind bool
	}{
		{
			name:       "an ordinary bring-up is a plain resume",
			start:      func(f *fleetFixture, ws ids.WorkspaceID) error { return f.fleet.Start(context.Background(), ws) },
			wantRebind: false,
		},
		{
			name:       "the start a bind runs REBINDS the workspace's book",
			start:      func(f *fleetFixture, ws ids.WorkspaceID) error { return f.fleet.StartRebound(context.Background(), ws) },
			wantRebind: true,
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := f.workspace("w1")
			f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}

			// Act.
			if err := test.start(f, ws.ID); err != nil {
				t.Fatalf("start: %v", err)
			}

			// Assert.
			resume := f.client.requests[0].GetResume()
			if resume == nil {
				t.Fatalf("StartSession request = %v, want a resume", f.client.requests[0])
			}
			if got := resume.GetRebind() != nil; got != test.wantRebind {
				t.Fatalf("resume.rebind present = %v, want %v", got, test.wantRebind)
			}
		})
	}
}

func TestResumeColdDoesNotMarkTheReOpenARebind(t *testing.T) {
	// Arrange: a workspace parked behind a standing cold gate. An answered gate
	// is the SAME conversation being paid for, not a different one being
	// chosen, so its re-open leaves the workspace's book exactly where it is.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	parkedGate(t, f, ws)

	// Act.
	if err := f.fleet.ResumeCold(context.Background(), ws.ID,
		ColdResume{VendorSessionID: "vendor-1", Remediation: payRemediation()}); err != nil {
		t.Fatalf("ResumeCold: %v", err)
	}

	// Assert.
	if rebind := f.client.requests[1].GetResume().GetRebind(); rebind != nil {
		t.Fatalf("re-open resume.rebind = %v, want the book left where it was", rebind)
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
	loseRecordedTranscript(f)

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
	loseRecordedTranscript(f)

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
	loseRecordedTranscript(f)

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

// fakeBundle is the installed shim bundle a spawn holds.
type fakeBundle struct {
	mu       sync.Mutex
	build    string
	err      error
	held     int
	released int
}

func (b *fakeBundle) Hold() (string, func(), error) {
	b.mu.Lock()
	defer b.mu.Unlock()
	if b.err != nil {
		return "", func() {}, b.err
	}
	b.held++
	return b.build, func() {
		b.mu.Lock()
		defer b.mu.Unlock()
		b.released++
	}, nil
}

// holding reports whether a hold is outstanding right now.
func (b *fakeBundle) holding() bool {
	b.mu.Lock()
	defer b.mu.Unlock()
	return b.held > b.released
}

func TestStartStampsTheSpawnWithTheBundleItHolds(t *testing.T) {
	// Arrange: the spawn is observed while it runs.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	heldDuringSpawn := false
	f.supervisor.onSpawn = func() { heldDuringSpawn = f.bundle.holding() }

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if got := f.supervisor.spawns[0].ShimBuildSHA; got != "installed-build" {
		t.Fatalf("spawn build = %q, want the bundle's own", got)
	}
	if !heldDuringSpawn || f.bundle.holding() {
		t.Fatalf("held during the spawn = %v, still held after = %v; want held across the spawn and released after", heldDuringSpawn, f.bundle.holding())
	}
}

func TestStartRefusesToSpawnAnUnresolvableBundle(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.bundle.err = errors.New("main.js does not exist and SHIM_BUILD_SHA is unset")

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if err == nil {
		t.Fatal("Start() = nil error, want the unresolvable bundle refused")
	}
	if f.supervisor.spawnAttempts != 0 {
		t.Fatalf("spawn attempts = %d, want none for a bundle with no build", f.supervisor.spawnAttempts)
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
	if !ok || gate.VendorSessionID != "vendor-1" || gate.Compact == nil || len(gate.Compact.Models) != 1 || len(gate.Compact.Scopes) != 3 {
		t.Fatalf("served cold gate = (%+v, %v), want the menu the row offered", gate, ok)
	}
}

func TestStartServesNoCompactMenuForAWorkAccountColdGate(t *testing.T) {
	// Arrange: the routed config dir IS the work (multi-repo) account.
	f := newFleetFixture(t)
	f.accounts.multiRepoDir = "/config"
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}
	f.client.response = coldResponse()

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	gate, ok := f.fleet.ColdGate(ws.ID)
	if !ok || gate.Compact != nil {
		t.Fatalf("served cold gate = (%+v, %v), want a standing gate with no compact menu", gate, ok)
	}
	if len(f.feed.synthesized) != 1 || f.feed.synthesized[0].GetColdGate().GetStanding().Compact != nil {
		t.Fatalf("synthesized rows = %v, want one standing gate row with no compact menu", f.feed.synthesized)
	}
}

func TestStartDrawsTheCompactMenuForAPersonalAccountColdGate(t *testing.T) {
	// Arrange: no work account, so the routed config dir is personal.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}
	f.client.response = coldResponse()

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if len(f.feed.synthesized) != 1 || f.feed.synthesized[0].GetColdGate().GetStanding().Compact == nil {
		t.Fatalf("synthesized rows = %v, want one standing gate row drawing the compact menu", f.feed.synthesized)
	}
}

func TestStartSurfacesANonColdStartFailure(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.response = &shimv1.StartSessionResponse{
		Result: &shimv1.StartSessionResponse_Failure{Failure: &shimv1.StartSessionFailure{
			Cause:  &shimv1.StartSessionFailure_VendorStartFailed{VendorStartFailed: rejectedVendorStart()},
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

// A REFUSED START IS A FAULT, NOT ONLY A LOG LINE. Grounded 2026-09-13: one
// workspace's StartSession was refused on every boot, `daemon.boot.bring_up`
// logged and counted it, and every surface a user reads drew the workspace as
// merely idle.
func TestStartFilesAFaultWhenTheShimRefusesTheStart(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.response = &shimv1.StartSessionResponse{
		Result: &shimv1.StartSessionResponse_Failure{Failure: &shimv1.StartSessionFailure{
			Cause:  &shimv1.StartSessionFailure_UnknownSession{UnknownSession: &shimv1.StartSessionUnknownSession{}},
			Detail: "no transcript on disk",
		}},
	}

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if got := faultKinds(f.db.dbFaults); len(got) != 1 || got[0] != health.KindResumeFailed {
		t.Fatalf("faults = %v, want exactly one %s", got, health.KindResumeFailed)
	}
}

func TestTheRefusedStartFaultCarriesTheShimsOwnAccount(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.response = &shimv1.StartSessionResponse{
		Result: &shimv1.StartSessionResponse_Failure{Failure: &shimv1.StartSessionFailure{
			Cause:  &shimv1.StartSessionFailure_UnknownSession{UnknownSession: &shimv1.StartSessionUnknownSession{}},
			Detail: "no transcript on disk",
		}},
	}

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert. The cause evidence is what the typed `resume_failed` arm renders.
	if len(f.db.dbFaults) != 1 || !strings.Contains(f.db.dbFaults[0].Evidence["cause"], "no transcript on disk") {
		t.Fatalf("fault evidence = %+v, want the shim's own detail", f.db.dbFaults)
	}
}

func TestStartFilesAFaultWhenTheStartSessionCallItselfFails(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.startErr = errors.New("the shim hung up")

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if got := faultKinds(f.db.dbFaults); len(got) != 1 || got[0] != health.KindResumeFailed {
		t.Fatalf("faults = %v, want exactly one %s", got, health.KindResumeFailed)
	}
}

func TestAColdGateFilesNoRefusedStartFault(t *testing.T) {
	// A DESIGNED PRODUCT STATE IS NOT A FAULT: the gate is the user's to
	// answer, and a fault beside it would report a broken workspace.
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}
	f.client.response = coldResponse()

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if got := faultKinds(f.db.dbFaults); len(got) != 0 {
		t.Fatalf("faults = %v, want none behind a cold gate", got)
	}
}

func TestAStandDownFilesNoRefusedStartFault(t *testing.T) {
	// A teardown this daemon ordered is not a session that would not start.
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.startErr = fmt.Errorf("start: %w", shimclient.ErrStandDownOrdered)

	// Act.
	_ = f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if got := faultKinds(f.db.dbFaults); len(got) != 0 {
		t.Fatalf("faults = %v, want none under a stand-down this daemon ordered", got)
	}
}

func TestAStartedSessionRetractsTheRefusedStartFault(t *testing.T) {
	// Arrange. An earlier boot's refusal is standing when this one succeeds.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	workspace := ws.ID
	if _, err := f.db.OpenFault(context.Background(), wsm.Fault{
		Workspace: &workspace,
		Kind:      health.KindResumeFailed,
		Detail:    "the shim refused to start this workspace's session",
	}); err != nil {
		t.Fatalf("OpenFault: %v", err)
	}

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("second Start: %v", err)
	}

	// Assert.
	if got := faultKinds(f.db.dbFaults); len(got) != 0 {
		t.Fatalf("faults = %v, want the refused-start fault retracted by a started session", got)
	}
}

// EVERY STANDING FAULT WHOSE LIFETIME ENDS AT A HEALTHY ATTACH OR A STARTED
// SESSION IS CLOSED BY A START (health/lifetime.go), not only a hand-listed
// few: `bounce_unknown` stood on a strip for over 30 minutes on 2026-09-27
// because no closer listed it.
func TestAStartClosesEveryFaultWhoseLifetimeEndsThere(t *testing.T) {
	tests := []struct {
		name  string
		kind  string
		stand bool
	}{
		{"a dead shim ends at the healthy attach", health.KindShimDied, false},
		{"a severed link ends at the healthy attach", health.KindLinkSevered, false},
		{"a start that failed ends at the healthy attach", health.KindShimStartFailed, false},
		{"a refused watch open ends at the healthy attach", health.KindWatchOpenRefused, false},
		{"an undetermined bounce ends at the healthy attach", health.KindBounceUnknown, false},
		{"an expired adoption window ends at the healthy attach", health.KindAdoptionWindowExpired, false},
		{"a failed cold-gate re-open ends at the started session", health.KindColdGateReopenFailed, false},
		{"a legacy relaunch refusal ends at the started session", health.KindRelaunchResumeFailed, false},
		{"an abandoned conversation waits for the next turn", health.KindConversationAbandoned, true},
		{"an unresolved final answer waits for the next turn", health.KindFinalAnswerUnresolved, true},
		{"a shim-reported fault waits for the next verdict", health.KindShimReported, true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := f.workspace("w1")
			workspace := ws.ID
			if _, err := f.db.OpenFault(context.Background(), wsm.Fault{Workspace: &workspace, Kind: tt.kind}); err != nil {
				t.Fatalf("OpenFault: %v", err)
			}

			// Act.
			if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
				t.Fatalf("Start: %v", err)
			}

			// Assert.
			got := faultKinds(f.db.dbFaults)
			if stands := len(got) == 1 && got[0] == tt.kind; stands != tt.stand {
				t.Fatalf("faults after the start = %v, want %s standing = %v", got, tt.kind, tt.stand)
			}
		})
	}
}

// A REFUSED START IS STILL A HEALTHY ATTACH, and only the attach's faults
// close: the session-start ones wait for a session the shim serves.
func TestARefusedStartClosesOnlyTheHealthyAttachsFaults(t *testing.T) {
	tests := []struct {
		name  string
		kind  string
		stand bool
	}{
		{"an undetermined bounce ends at the healthy attach", health.KindBounceUnknown, false},
		{"a failed cold-gate re-open waits for a started session", health.KindColdGateReopenFailed, true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := f.workspace("w1")
			workspace := ws.ID
			if _, err := f.db.OpenFault(context.Background(), wsm.Fault{Workspace: &workspace, Kind: tt.kind}); err != nil {
				t.Fatalf("OpenFault: %v", err)
			}
			f.client.response = &shimv1.StartSessionResponse{
				Result: &shimv1.StartSessionResponse_Failure{Failure: &shimv1.StartSessionFailure{
					Cause:  &shimv1.StartSessionFailure_VendorStartFailed{VendorStartFailed: rejectedVendorStart()},
					Detail: "the vendor binary is missing",
				}},
			}

			// Act.
			_ = f.fleet.Start(context.Background(), ws.ID)

			// Assert.
			stands := false
			for _, kind := range faultKinds(f.db.dbFaults) {
				stands = stands || kind == tt.kind
			}
			if stands != tt.stand {
				t.Fatalf("faults after the refused start = %v, want %s standing = %v", faultKinds(f.db.dbFaults), tt.kind, tt.stand)
			}
		})
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

func TestStopArmsTheStandDownBeforeItClosesTheWatcher(t *testing.T) {
	// Arrange: the close reads the latch to tell the bounce registry the
	// shim's departure was ordered.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}
	armedAtClose := false
	f.watcher.onClose = func() { armedAtClose = f.client.stoodDown }

	// Act.
	if err := f.fleet.Stop(context.Background(), ws.ID, false); err != nil {
		t.Fatalf("Stop: %v", err)
	}

	// Assert.
	if !armedAtClose {
		t.Fatal("the watcher closed before the stop armed the stand-down; its departure reads as unordered")
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

// `default` LEFT THE TABLE by the owner's 2026-09-14 ruling: permissionMode no
// longer produces that arm for anyone, so there is nothing for it to round
// trip through. Its replacement guarantee is the upgrade test below.
func TestPermissionModeRoundTripsEveryArm(t *testing.T) {
	tests := []string{"acceptEdits", "bypassPermissions", "plan", "dontAsk", "auto"}
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

func TestPermissionModeOfAnUnknownNameIsAuto(t *testing.T) {
	// Arrange: an unknown name resolves to auto, which KEEPS the gate.
	// Act.
	got := permissionMode("something-nobody-implemented")

	// Assert.
	if got.GetAuto() == nil {
		t.Fatalf("permissionMode(unknown) = %v, want auto", got)
	}
}

func TestPermissionModeOfAnUnknownNameNeverDropsTheGate(t *testing.T) {
	// Arrange: the standing guarantee the auto ruling must not weaken.
	// Act.
	got := permissionMode("something-nobody-implemented")

	// Assert.
	if got.GetBypass() != nil || got.GetDontAsk() != nil {
		t.Fatalf("permissionMode(unknown) = %v, want a mode that keeps the gate", got)
	}
}

func TestPermissionModeOfAnEmptyNameIsAuto(t *testing.T) {
	// Arrange: a row that never recorded a mode. Act.
	got := permissionMode("")

	// Assert.
	if got.GetAuto() == nil {
		t.Fatalf("permissionMode(\"\") = %v, want auto", got)
	}
}

// A SESSION ROW STORED BEFORE THE RULING carries "default"; the next start
// asks the shim for auto instead, and recordFacts then rewrites the row from
// what the shim reports.
func TestAStoredDefaultIsUpgradedToAutoAtTheNextStart(t *testing.T) {
	// Arrange. Act.
	got := permissionMode("default")

	// Assert.
	if got.GetAuto() == nil {
		t.Fatalf("permissionMode(\"default\") = %v, want auto", got)
	}
}

func TestStartFreshOfAStoredDefaultAsksTheShimForAuto(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, PermissionMode: "default"}

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	fresh := f.client.requests[0].GetFresh()
	if fresh == nil || fresh.GetPermissionMode().GetAuto() == nil {
		t.Fatalf("StartSession request = %v, want the stored default started as auto", f.client.requests[0])
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

// TestFreshModelDefaultsToOpusWhenTheCreateNamedNone is the owner ruling of
// 2026-09-14: a session the user never modeled runs under opus, not the
// vendor's own pick (which the CLI reports as the <synthetic> marker).
func TestFreshModelDefaultsToOpusWhenTheCreateNamedNone(t *testing.T) {
	// Arrange / Act.
	got := freshModel("")

	// Assert.
	if got.GetName() != DefaultLaunchModel {
		t.Fatalf("freshModel(\"\") = %v, want the %q default", got, DefaultLaunchModel)
	}
}

// TestStartFreshOfAnUnmodeledSessionAsksTheShimForOpus is the same ruling from
// the daemon's side: the launch request names opus when the row modeled none.
func TestStartFreshOfAnUnmodeledSessionAsksTheShimForOpus(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID}

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	fresh := f.client.requests[0].GetFresh()
	if fresh == nil || fresh.GetModel().GetName() != DefaultLaunchModel {
		t.Fatalf("StartSession request = %v, want an unmodeled start launched under opus", f.client.requests[0])
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

// TestBringUpRelaysTheShimsLockHolderUnavailableRefusal covers a shim whose own
// kernel-lock holder failed: nobody owns the conversation, so the relay must
// name the broken helper and how it broke rather than an ownership conflict.
func TestBringUpRelaysTheShimsLockHolderUnavailableRefusal(t *testing.T) {
	tests := []struct {
		name  string
		check func(t *testing.T, refusal *Refusal)
	}{
		{name: "the refusal rides the lock_holder_unavailable arm", check: func(t *testing.T, refusal *Refusal) {
			if refusal.Arm != ArmLockHolderUnavailable {
				t.Fatalf("arm = %q, want %q", refusal.Arm, ArmLockHolderUnavailable)
			}
		}},
		{name: "the arm carries the shim's failure whole", check: func(t *testing.T, refusal *Refusal) {
			got, _ := refusal.Fields["failure"].(*conversationv1.LockHolderFailure)
			if !proto.Equal(got, exitedHolder()) {
				t.Fatalf("failure = %v, want the shim's own LockHolderFailure", refusal.Fields["failure"])
			}
		}},
		{name: "the reason says how the helper failed", check: func(t *testing.T, refusal *Refusal) {
			if !strings.Contains(refusal.Reason, "/bin/shim-lock exited with code 1 before taking the lock (EACCES)") {
				t.Fatalf("reason = %q, want the helper's exit stated", refusal.Reason)
			}
		}},
		{name: "the reason says nobody owns the conversation", check: func(t *testing.T, refusal *Refusal) {
			if !strings.Contains(refusal.Reason, "nobody owns the conversation") {
				t.Fatalf("reason = %q, want the ownership denied", refusal.Reason)
			}
		}},
		{name: "the reason never claims another shim owns the conversation", check: func(t *testing.T, refusal *Refusal) {
			if strings.Contains(refusal.Reason, "another shim") {
				t.Fatalf("reason = %q, want no ownership claim", refusal.Reason)
			}
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := f.workspace("w1")
			f.client.response = &shimv1.StartSessionResponse{
				Result: &shimv1.StartSessionResponse_Failure{Failure: &shimv1.StartSessionFailure{
					Detail: "this shim's lock helper failed",
					Cause: &shimv1.StartSessionFailure_LockHolderUnavailable{
						LockHolderUnavailable: &shimv1.StartSessionLockHolderUnavailable{Failure: exitedHolder()},
					},
				}},
			}

			// Act.
			err := f.fleet.Start(context.Background(), ws.ID)

			// Assert.
			refusal, ok := AsRefusal(err)
			if !ok {
				t.Fatalf("error = %v, want a refusal", err)
			}
			tt.check(t, refusal)
		})
	}
}

// exitedHolder is a lock holder that exited 1 before taking the lock.
func exitedHolder() *conversationv1.LockHolderFailure {
	return &conversationv1.LockHolderFailure{
		Binary: "/bin/shim-lock",
		How:    &conversationv1.LockHolderFailure_Exited{Exited: &conversationv1.LockHolderExited{Code: 1, Stderr: "EACCES"}},
	}
}

// TestBringUpRecordsALockHolderFailureStatingNoHowAtError covers the contract
// breach: the refusal is still relayed, and the breach is recorded loudly.
func TestBringUpRecordsALockHolderFailureStatingNoHowAtError(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.client.response = &shimv1.StartSessionResponse{
		Result: &shimv1.StartSessionResponse_Failure{Failure: &shimv1.StartSessionFailure{
			Cause: &shimv1.StartSessionFailure_LockHolderUnavailable{
				LockHolderUnavailable: &shimv1.StartSessionLockHolderUnavailable{
					Failure: &conversationv1.LockHolderFailure{Binary: "/bin/shim-lock"},
				},
			},
		}},
	}

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	asRefusal(t, err, ArmLockHolderUnavailable)
	if !recordedAt(f, "error", opBringUp, "the shim's lock_holder_unavailable refusal states no how; relayed as given") {
		t.Fatalf("records = %+v, want the breach recorded at error", f.log.logger.Records())
	}
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

// THE COLD-GATE ANSWER'S RE-OPEN IS THE SECOND PLACE A REFUSED START HID.
// Grounded 2026-09-13: the owner answered a gate with `clear`, the re-open was
// refused, `daemon.workspace.answer_cold_gate` logged "the re-open with the
// remediation failed" -- and nothing else said so anywhere.
func TestResumeColdFilesAFaultWhenTheReopenIsRefused(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	parkedGate(t, f, ws)
	f.client.startErr = errors.New("the link is gone")

	// Act.
	_ = f.fleet.ResumeCold(context.Background(), ws.ID,
		ColdResume{VendorSessionID: "vendor-1", Remediation: payRemediation()})

	// Assert.
	if got := faultKinds(f.db.dbFaults); len(got) != 1 || got[0] != health.KindResumeFailed {
		t.Fatalf("faults = %v, want exactly one %s", got, health.KindResumeFailed)
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

// BeginStandDown latches the fake supervisor's stand-down, answering whether
// this call was the one that latched it.
func (s *fakeSupervisor) BeginStandDown() bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.standingDown {
		return false
	}
	s.standingDown = true
	return true
}

// SpawnedFor answers the fake supervisor's live spawn registry, which is what
// the bring-up's adoption guard reads: a shim this daemon spawned and still
// owns is never adopted a second time.
func (s *fakeSupervisor) SpawnedFor(ws ids.WorkspaceID) (int, bool) {
	pid, ok := s.spawned[ws]
	return pid, ok
}

// StandingDown answers the fake supervisor's latch.
func (s *fakeSupervisor) StandingDown() bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.standingDown
}

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

// ---------------------------------------------------------------------------
// The compaction phases a remediated re-open relays.
//
// The gate's `compact` remediation runs INSIDE StartSession, so there is no
// session watcher to carry its frames: the re-open opens a watch of its own
// for the duration. Owner's report, 2026-09-14 — the compaction ran with the
// daemon writing one record at the very end and nothing anywhere while it went.
// ---------------------------------------------------------------------------

// compactionFrame is one relayed phase, as the shim's stream carries it.
func compactionFrame(
	phase conversationv1.SessionCompactionPhase,
	before, after uint64,
) *shimv1.WatchSessionResponse {
	return &shimv1.WatchSessionResponse{
		Frame: &shimv1.WatchSessionResponse_Update{
			Update: &conversationv1.SessionUpdate{
				Update: &conversationv1.SessionUpdate_CompactionProgress{
					CompactionProgress: &conversationv1.SessionCompactionProgress{
						Phase: phase, TokensBefore: before, TokensAfter: after,
					},
				},
			},
		},
	}
}

func TestResumeColdRelaysEveryCompactionPhase(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	parkedGate(t, f, ws)
	f.client.sessionFrames = []*shimv1.WatchSessionResponse{
		compactionFrame(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_SUMMARIZING, 101_600, 0),
		compactionFrame(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_SUMMARIZED, 101_600, 12_400),
		compactionFrame(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_STARTED, 101_600, 12_400),
	}
	var seen []conversationv1.SessionCompactionPhase

	// Act.
	if err := f.fleet.ResumeCold(context.Background(), ws.ID, ColdResume{
		VendorSessionID: "vendor-1",
		Remediation:     payRemediation(),
		OnPhase: func(p *conversationv1.SessionCompactionProgress) {
			seen = append(seen, p.GetPhase())
		},
	}); err != nil {
		t.Fatalf("ResumeCold: %v", err)
	}

	// Assert.
	if len(seen) != 3 {
		t.Fatalf("relayed phases = %v, want all three", seen)
	}
}

func TestResumeColdRelaysThePhasesInOrder(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	parkedGate(t, f, ws)
	f.client.sessionFrames = []*shimv1.WatchSessionResponse{
		compactionFrame(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_SUMMARIZING, 101_600, 0),
		compactionFrame(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_STARTED, 101_600, 12_400),
	}
	var seen []conversationv1.SessionCompactionPhase

	// Act.
	if err := f.fleet.ResumeCold(context.Background(), ws.ID, ColdResume{
		VendorSessionID: "vendor-1",
		Remediation:     payRemediation(),
		OnPhase: func(p *conversationv1.SessionCompactionProgress) {
			seen = append(seen, p.GetPhase())
		},
	}); err != nil {
		t.Fatalf("ResumeCold: %v", err)
	}

	// Assert.
	want := []conversationv1.SessionCompactionPhase{
		conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_SUMMARIZING,
		conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_STARTED,
	}
	if len(seen) != len(want) || seen[0] != want[0] || seen[1] != want[1] {
		t.Fatalf("relayed phases = %v, want %v in order", seen, want)
	}
}

func TestResumeColdRelaysTheFiguresTheShimStated(t *testing.T) {
	// Arrange.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	parkedGate(t, f, ws)
	f.client.sessionFrames = []*shimv1.WatchSessionResponse{
		compactionFrame(conversationv1.SessionCompactionPhase_SESSION_COMPACTION_PHASE_STARTED, 101_600, 12_400),
	}
	var last *conversationv1.SessionCompactionProgress

	// Act.
	if err := f.fleet.ResumeCold(context.Background(), ws.ID, ColdResume{
		VendorSessionID: "vendor-1",
		Remediation:     payRemediation(),
		OnPhase:         func(p *conversationv1.SessionCompactionProgress) { last = p },
	}); err != nil {
		t.Fatalf("ResumeCold: %v", err)
	}

	// Assert.
	if last.GetTokensBefore() != 101_600 || last.GetTokensAfter() != 12_400 {
		t.Fatalf("figures = (%d, %d), want the shim's own", last.GetTokensBefore(), last.GetTokensAfter())
	}
}

func TestResumeColdStillReopensWhenTheWatchWillNotOpen(t *testing.T) {
	// Arrange. The narration is additive; refusing to bring a session up
	// because its commentary was unavailable would be the worse failure.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	parkedGate(t, f, ws)
	f.client.watchSessionErr = errors.New("the stream would not open")

	// Act.
	err := f.fleet.ResumeCold(context.Background(), ws.ID, ColdResume{
		VendorSessionID: "vendor-1",
		Remediation:     payRemediation(),
		OnPhase:         func(*conversationv1.SessionCompactionProgress) {},
	})

	// Assert.
	if err != nil {
		t.Fatalf("ResumeCold: %v, want the re-open to stand without its phases", err)
	}
}

func TestResumeColdOpensNoWatchWhenNobodyIsListening(t *testing.T) {
	// Arrange. A nil OnPhase must not cost a stream.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	parkedGate(t, f, ws)
	f.client.watchSessionErr = errors.New("WatchSession must not be called")

	// Act.
	err := f.fleet.ResumeCold(context.Background(), ws.ID,
		ColdResume{VendorSessionID: "vendor-1", Remediation: payRemediation()})

	// Assert.
	if err != nil {
		t.Fatalf("ResumeCold: %v", err)
	}
}

func TestFleetWorkspacesAnswersTheHeldSessions(t *testing.T) {
	cases := []struct {
		name  string
		start bool
		stop  bool
		want  []ids.WorkspaceID
	}{
		{name: "no session is held", want: []ids.WorkspaceID{}},
		{name: "a started session is held", start: true, want: []ids.WorkspaceID{"w1"}},
		{name: "a stopped session is not held", start: true, stop: true, want: []ids.WorkspaceID{}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := f.workspace("w1")
			if tc.start {
				if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
					t.Fatalf("Start: %v", err)
				}
			}
			if tc.stop {
				if err := f.fleet.Stop(context.Background(), ws.ID, true); err != nil {
					t.Fatalf("Stop: %v", err)
				}
			}

			// Act.
			got := f.fleet.Workspaces()

			// Assert.
			if !slices.Equal(got, tc.want) {
				t.Fatalf("Workspaces() = %v, want %v", got, tc.want)
			}
		})
	}
}

// ---- THE LIVE-SHIM INVARIANT ----------------------------------------------
//
// A workspace whose shim is live in the fleet carries no terminal session
// record. See retireTerminalRecord for what a stale one cost.

// killedRecord arranges a workspace whose session record reads killed, the
// shape a KillWorkspace leaves behind and nothing ever retired.
func killedRecord(t *testing.T, f *fleetFixture, ws wsm.Workspace, kind string) {
	t.Helper()
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, HostSessionID: "host-1", VendorSessionID: "vendor-1"}
	if err := f.db.SetSessionTerminal(context.Background(), ws.ID, wsm.SessionTerminal{
		Kind: kind, Detail: "KillWorkspace", At: fixedNow,
	}); err != nil {
		t.Fatalf("SetSessionTerminal: %v", err)
	}
}

func TestAGateParkedBringUpRetiresAKilledSessionRecord(t *testing.T) {
	// Arrange: the shim comes up and parks at its cold gate, so NO session
	// facts are recorded — which is exactly how a killed record outlived the
	// shim that replaced it.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	killedRecord(t, f, ws, "killed")
	f.client.response = coldResponse()

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if terminal := f.db.sessions[ws.ID].Terminal; terminal != nil {
		t.Fatalf("terminal = %+v for a live gate-parked shim, want it retired", terminal)
	}
}

func TestAStartedSessionRetiresAKilledSessionRecord(t *testing.T) {
	// Arrange: the guard against the record's only other retirement — the
	// facts a successful start records — ever ceasing to clear it.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	killedRecord(t, f, ws, "killed")

	// Act.
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert.
	if terminal := f.db.sessions[ws.ID].Terminal; terminal != nil {
		t.Fatalf("terminal = %+v for a started session, want it retired", terminal)
	}
}

func TestABringUpDoesNotResurrectADeletedSession(t *testing.T) {
	// Arrange: a forgotten workspace's cause of death is final, and no client
	// comes up to argue with it — the bring-up refuses the deleted session
	// before it spawns, so the retirement is never reached at all.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	killedRecord(t, f, ws, "deleted")

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if err == nil {
		t.Fatal("Start = nil for a deleted session, want the refusal")
	}
	terminal := f.db.sessions[ws.ID].Terminal
	if terminal == nil || terminal.Kind != "deleted" {
		t.Fatalf("terminal = %+v after a refused bring-up, want the deletion kept", terminal)
	}
}

func TestABringUpSurfacesAFailedTerminalRetirement(t *testing.T) {
	// Arrange: the store refuses the retirement, which is a record that will
	// go on calling a serving session dead.
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	killedRecord(t, f, ws, "killed")
	f.db.clearTerminalErr = errors.New("the store is unreadable")

	// Act.
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "the store is unreadable") {
		t.Fatalf("Start = %v, want the failed retirement surfaced", err)
	}
}

func (c *fakeClient) Detach() { c.detached++ }

// ---- an answered cold gate: taken once, stood again on failure ----

func TestTakeColdGateSpendsOnlyTheGateItNames(t *testing.T) {
	tests := []struct {
		name      string
		standing  *ServedColdGate
		take      string
		wantTaken bool
		wantLeft  bool
	}{
		{"the standing gate for the named conversation is taken", &ServedColdGate{VendorSessionID: "vendor-1"}, "vendor-1", true, false},
		{"a gate standing for another conversation is left", &ServedColdGate{VendorSessionID: "vendor-2"}, "vendor-1", false, true},
		{"nothing standing is nothing taken", nil, "vendor-1", false, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := f.workspace("ws-take")
			if tt.standing != nil {
				f.fleet.coldGates[ws.ID] = coldGate{served: *tt.standing}
			}

			// Act.
			taken := f.fleet.TakeColdGate(ws.ID, tt.take)

			// Assert.
			_, left := f.fleet.ColdGate(ws.ID)
			if taken != tt.wantTaken || left != tt.wantLeft {
				t.Fatalf("taken = %t, left standing = %t; want %t, %t", taken, left, tt.wantTaken, tt.wantLeft)
			}
		})
	}
}

func TestASecondTakeOfOneColdGateFindsNothing(t *testing.T) {
	// Arrange: two answers racing for one gate.
	f := newFleetFixture(t)
	ws := f.workspace("ws-race")
	f.fleet.coldGates[ws.ID] = coldGate{served: ServedColdGate{VendorSessionID: "vendor-1"}}

	// Act.
	first := f.fleet.TakeColdGate(ws.ID, "vendor-1")
	second := f.fleet.TakeColdGate(ws.ID, "vendor-1")

	// Assert.
	if !first || second {
		t.Fatalf("takes = %t, %t; want exactly the first to spend the gate", first, second)
	}
}

func TestReraiseColdGateStandsTheGateFromItsKeptFacts(t *testing.T) {
	// Arrange: the gate was raised, answered, and spent.
	f := newFleetFixture(t)
	ws := f.workspace("ws-reraise")
	f.fleet.raiseColdGate(ws.ID, "vendor-1", coldResponse().GetFailure().GetCold(), "/config")
	f.fleet.TakeColdGate(ws.ID, "vendor-1")
	f.feed.synthesized = nil

	// Act.
	raised := f.fleet.ReraiseColdGate(ws.ID, "vendor-1")

	// Assert.
	if _, standing := f.fleet.ColdGate(ws.ID); !raised || !standing {
		t.Fatalf("raised = %t, standing = %t; want the gate stood again", raised, standing)
	}
	want := coldGateRowID(ws.ID, "vendor-1").GetValue()
	if len(f.feed.synthesized) != 1 || f.feed.synthesized[0].GetId().GetValue() != want ||
		f.feed.synthesized[0].GetColdGate().GetStanding() == nil {
		t.Fatalf("synthesized rows = %v, want the standing gate row %q", f.feed.synthesized, want)
	}
}

func TestReraiseColdGateKeepsTheWorkAccountsMenuWithheld(t *testing.T) {
	// Arrange: a work-account gate was raised, answered, and spent.
	f := newFleetFixture(t)
	f.accounts.multiRepoDir = "/work"
	ws := f.workspace("ws-reraise-work")
	f.fleet.raiseColdGate(ws.ID, "vendor-1", coldResponse().GetFailure().GetCold(), "/work")
	f.fleet.TakeColdGate(ws.ID, "vendor-1")

	// Act.
	raised := f.fleet.ReraiseColdGate(ws.ID, "vendor-1")

	// Assert.
	gate, standing := f.fleet.ColdGate(ws.ID)
	if !raised || !standing || gate.Compact != nil {
		t.Fatalf("raised = %t, gate = (%+v, %t); want the gate stood again with no compact menu", raised, gate, standing)
	}
}

func TestReraiseColdGateAnswersFalseWithoutItsFacts(t *testing.T) {
	// Arrange: no gate was ever raised, so no cold facts were kept.
	f := newFleetFixture(t)
	ws := f.workspace("ws-no-facts")

	// Act.
	raised := f.fleet.ReraiseColdGate(ws.ID, "vendor-1")

	// Assert.
	if raised || len(f.feed.synthesized) != 0 {
		t.Fatalf("raised = %t, rows = %v; want nothing stood without cold facts", raised, f.feed.synthesized)
	}
}

// TestDetachedFleetWorkSharesOneDrain holds every kind of detached session work
// to the one runner: whichever verb started it, DrainStarts ends and joins it,
// so a hand-rolled goroutine that escaped the drain fails here.
func TestDetachedFleetWorkSharesOneDrain(t *testing.T) {
	tests := []struct {
		name  string
		start func(f *fleetFixture, ws ids.WorkspaceID, settled chan<- error)
	}{
		{"a detached start", func(f *fleetFixture, ws ids.WorkspaceID, settled chan<- error) {
			f.fleet.StartDetached(ws, func(err error) { settled <- err })
		}},
		{"a detached cold re-open", func(f *fleetFixture, ws ids.WorkspaceID, settled chan<- error) {
			f.fleet.remember(ws, &live{client: f.client})
			f.fleet.ResumeColdDetached(ws, ColdResume{VendorSessionID: "vendor-1", Remediation: &conversationv1.SessionColdRemediation{
				Remediation: &conversationv1.SessionColdRemediation_Pay{Pay: &conversationv1.SessionColdPay{}},
			}}, func(_ context.Context, err error) { settled <- err })
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := f.workspace(ids.WorkspaceID("ws-drain-" + strings.ReplaceAll(tt.name, " ", "-")))
			entered := make(chan struct{})
			f.client.entered = entered
			f.client.startHold = make(chan struct{}) // never closed: only the drain ends it.
			settled := make(chan error, 1)
			tt.start(f, ws.ID, settled)
			<-entered

			// Act.
			left := f.fleet.DrainStarts(time.Minute)

			// Assert.
			if !left {
				t.Fatal("DrainStarts = false, want the in-flight work ended and joined")
			}
			if err := <-settled; !errors.Is(err, context.Canceled) {
				t.Fatalf("the drained work = %v, want a cancellation", err)
			}
		})
	}
}

func TestATakenColdGateStillRefusesPromptsByItsName(t *testing.T) {
	// Arrange: the answer is taken and its remediation has not settled.
	f := newFleetFixture(t)
	ws := f.workspace("ws-answering")
	f.fleet.raiseColdGate(ws.ID, "vendor-1", coldResponse().GetFailure().GetCold(), "/config")
	f.fleet.TakeColdGate(ws.ID, "vendor-1")

	// Act.
	_, menu := f.fleet.ColdGate(ws.ID)
	detail, refused := f.fleet.ColdGateDetail(ws.ID)
	_, carried := f.fleet.ColdGateStanding(ws.ID)

	// Assert.
	if menu || !refused || detail == "" || !carried {
		t.Fatalf("menu offered = %t, prompts refused = %t (%q), carried by a handover = %t; want no menu, a refusal, and a carry",
			menu, refused, detail, carried)
	}
}

func TestEndColdGateRetiresOnlyTheTakenGateItNames(t *testing.T) {
	tests := []struct {
		name      string
		take      bool
		end       string
		wantGated bool
	}{
		{"the taken gate for the named conversation is retired", true, "vendor-1", false},
		{"a gate still standing is left", false, "vendor-1", true},
		{"a taken gate for another conversation is left", true, "vendor-2", true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFleetFixture(t)
			ws := f.workspace("ws-end")
			f.fleet.raiseColdGate(ws.ID, "vendor-1", coldResponse().GetFailure().GetCold(), "/config")
			if tt.take {
				f.fleet.TakeColdGate(ws.ID, "vendor-1")
			}

			// Act.
			f.fleet.EndColdGate(ws.ID, tt.end)

			// Assert.
			if _, gated := f.fleet.ColdGateDetail(ws.ID); gated != tt.wantGated {
				t.Fatalf("prompts refused by the gate = %t, want %t", gated, tt.wantGated)
			}
		})
	}
}

// TestMarkUnservedTellsTheViewsTheLinkIsDead pins the boot's answer for an
// undetermined workspace: the footer, topbar and roster each hear a dead link,
// so the roster row resolves `unavailable` rather than `pending` forever.
func TestMarkUnservedTellsTheViewsTheLinkIsDead(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.links.links = nil

	// Act
	f.fleet.MarkUnserved(ws.ID)

	// Assert
	want := []sessionwatcher.LinkState{
		shimclient.LinkDead, shimclient.LinkDead, shimclient.LinkDead,
	}
	if fmt.Sprint(f.links.links) != fmt.Sprint(want) {
		t.Fatalf("OnLink calls = %v, want the footer, topbar and roster each told the link is dead", f.links.links)
	}
}

// TestStartRaisesTheBringUpForItsOwnDuration pins that the roster holds a
// starting workspace's row `pending` exactly while the start runs.
func TestStartRaisesTheBringUpForItsOwnDuration(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)
	ws := f.workspace("w1")

	// Act
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert
	want := []bringUpEdge{{ws.ID, true}, {ws.ID, false}}
	if fmt.Sprint(f.bringUps) != fmt.Sprint(want) {
		t.Fatalf("BringUps edges = %v, want %v", f.bringUps, want)
	}
}

// TestStartOfALiveSessionRaisesNoBringUp pins that a start answered by the
// session already up brings nothing up, so the row is never held for it.
func TestStartOfALiveSessionRaisesNoBringUp(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}
	f.bringUps = nil

	// Act
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("second Start: %v", err)
	}

	// Assert
	if len(f.bringUps) != 0 {
		t.Fatalf("BringUps edges = %v, want none for a session already live", f.bringUps)
	}
}

// TestNewestAdoptableDecidesTheDirectorysNewestTranscript covers every arm of
// the one adoption decision: adoptable, too fresh, none, and a failed probe.
func TestNewestAdoptableDecidesTheDirectorysNewestTranscript(t *testing.T) {
	tests := []struct {
		name         string
		modTime      time.Time
		writerGoneAt time.Time
		noneOnDir    bool
		probeErr     error
		wantOK       bool
		wantLevel    string
		wantMsg      string
	}{
		{name: "an idle transcript is adoptable", modTime: fixedNow.Add(-2 * TranscriptAdoptionIdleWindow), wantOK: true},
		{
			name: "a fresh transcript last written before this daemon's shim was gone is adoptable", modTime: fixedNow.Add(-2 * time.Second),
			writerGoneAt: fixedNow.Add(-time.Second), wantOK: true,
			wantLevel: dlog.LevelInfo, wantMsg: "the newest transcript was last written before this daemon's own shim for it was gone; the idle guard is waived",
		},
		{
			name: "a fresh transcript written after this daemon's shim was gone is refused", modTime: fixedNow.Add(-time.Second),
			writerGoneAt: fixedNow.Add(-2 * time.Second),
			wantLevel:    dlog.LevelInfo, wantMsg: "the newest transcript was modified too recently to adopt safely; the session comes up FRESH",
		},
		{
			name: "a transcript inside the idle window is refused", modTime: fixedNow.Add(-time.Second),
			wantLevel: dlog.LevelInfo, wantMsg: "the newest transcript was modified too recently to adopt safely; the session comes up FRESH",
		},
		{
			name: "no transcript is refused quietly", noneOnDir: true,
			wantLevel: dlog.LevelDebug, wantMsg: "no on-disk transcript to adopt; the session comes up FRESH",
		},
		{
			name: "a failed probe is refused loudly", probeErr: errors.New("readdir blew up"),
			wantLevel: dlog.LevelWarn, wantMsg: "could not probe for a transcript to adopt; the session comes up FRESH",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFleetFixture(t)
			if !tt.noneOnDir && tt.probeErr == nil {
				f.accounts.newest = account.AdoptableTranscript{VendorSessionID: "newest", ModTime: tt.modTime}
			}
			f.accounts.newestErr = tt.probeErr

			// Act
			got, ok := f.fleet.newestAdoptable(context.Background(), f.log.logger, "/tree/w1", tt.writerGoneAt)

			// Assert
			if ok != tt.wantOK || (ok && got.VendorSessionID != "newest") {
				t.Fatalf("newestAdoptable = (%+v, %v), want ok=%v", got, ok, tt.wantOK)
			}
			if tt.wantMsg != "" && !recordedAt(f, tt.wantLevel, opBringUp, tt.wantMsg) {
				t.Fatalf("the refusal was not recorded at %s: %q", tt.wantLevel, tt.wantMsg)
			}
		})
	}
}

// loseRecordedTranscript makes every recorded conversation's transcript
// missing from disk.
func loseRecordedTranscript(f *fleetFixture) {
	f.accounts.transcriptErr = errors.New("no such file")
}

// engage records a turn of w1, so the workspace has a conversation to lose.
func engage(f *fleetFixture) {
	f.db.putTurns = append(f.db.putTurns, wsm.Turn{ID: "t1", Workspace: "w1"})
}

// restoreMessage is the record of a restored directory transcript.
const restoreMessage = "the recorded conversation has no transcript on disk; restoring the directory's newest transcript"

// engagedWithLostTranscript arranges an engaged workspace whose recorded
// conversation has no transcript, and whose directory's newest transcript is
// "newest", last written at modTime.
func engagedWithLostTranscript(t *testing.T, modTime time.Time) (*fleetFixture, wsm.Session) {
	t.Helper()
	f := newFleetFixture(t)
	loseRecordedTranscript(f)
	engage(f)
	f.accounts.newest = account.AdoptableTranscript{VendorSessionID: "newest", ModTime: modTime}
	return f, wsm.Session{Workspace: "w1", VendorSessionID: "vendor-stale"}
}

func TestClassifySourceRestoresTheDirectorysNewestTranscript(t *testing.T) {
	// Arrange
	f, session := engagedWithLostTranscript(t, fixedNow.Add(-2*TranscriptAdoptionIdleWindow))

	// Act
	got, err := f.fleet.classifySource(context.Background(), f.log.logger, "w1", "/tree/w1", session, true)

	// Assert
	if err != nil {
		t.Fatalf("classifySource: %v", err)
	}
	if got.Fresh || got.VendorSessionID != "newest" {
		t.Fatalf("classifySource() = %+v, want a resume of the directory's newest transcript", got)
	}
	var restored *dlog.Record
	for _, r := range f.log.logger.Records() {
		if r.Level == dlog.LevelInfo && r.Message == restoreMessage {
			restored = &r
		}
	}
	if restored == nil || restored.Context["recorded_vendor_session_id"] != "vendor-stale" || restored.Context["adopted_vendor_session_id"] != "newest" {
		t.Fatalf("restore record = %+v, want both ids at info", restored)
	}
	if opened := abandonedFaults(f.db); len(opened) != 0 {
		t.Fatalf("abandoned faults = %+v, want none for a restored conversation", opened)
	}
}

func TestClassifySourceRecordsTheRestoredConversationAsTheResumeHandle(t *testing.T) {
	// Arrange
	f, session := engagedWithLostTranscript(t, fixedNow.Add(-2*TranscriptAdoptionIdleWindow))

	// Act
	if _, err := f.fleet.classifySource(context.Background(), f.log.logger, "w1", "/tree/w1", session, true); err != nil {
		t.Fatalf("classifySource: %v", err)
	}

	// Assert
	if got := f.db.sessions["w1"].VendorSessionID; got != "newest" {
		t.Fatalf("the recorded resume handle = %q, want the restored conversation", got)
	}
}

func TestClassifySourceStillRestoresWhenTheResumeHandleCannotBeRecorded(t *testing.T) {
	// Arrange
	f, session := engagedWithLostTranscript(t, fixedNow.Add(-2*TranscriptAdoptionIdleWindow))
	f.db.setVendorErr = errFake

	// Act
	got, err := f.fleet.classifySource(context.Background(), f.log.logger, "w1", "/tree/w1", session, true)

	// Assert
	if err != nil || got.VendorSessionID != "newest" {
		t.Fatalf("classifySource() = (%+v, %v), want the restored conversation resumed", got, err)
	}
	if !recordedAt(f, dlog.LevelError, opBringUp, "could not record the adopted conversation as the session's resume handle; the session still resumes it") {
		t.Fatalf("records = %+v, want the failed record at error", f.log.logger.Records())
	}
}

func TestClassifySourceRestoresAJustWrittenTranscriptThisDaemonsReapAccountsFor(t *testing.T) {
	// Arrange: the transcript was written a second ago, by the shim this
	// daemon reaped half a second later.
	f, session := engagedWithLostTranscript(t, fixedNow.Add(-time.Second))
	f.fleet.noteReap("w1", &fakeClient{reaped: true, reapedAt: fixedNow.Add(-500 * time.Millisecond)})

	// Act
	got, err := f.fleet.classifySource(context.Background(), f.log.logger, "w1", "/tree/w1", session, true)

	// Assert
	if err != nil || got.VendorSessionID != "newest" {
		t.Fatalf("classifySource() = (%+v, %v), want the just-written transcript restored", got, err)
	}
}

func TestClassifySourceAbandonsLoudlyWhenAnUnaccountedWriterMayHoldTheTranscript(t *testing.T) {
	// Arrange: written a second ago, and this daemon reaped no shim for it.
	f, session := engagedWithLostTranscript(t, fixedNow.Add(-time.Second))

	// Act
	got, err := f.fleet.classifySource(context.Background(), f.log.logger, "w1", "/tree/w1", session, true)

	// Assert
	if err != nil || !got.Fresh {
		t.Fatalf("classifySource() = (%+v, %v), want a fresh start", got, err)
	}
	if !recordedAt(f, dlog.LevelInfo, opBringUp, "the newest transcript was modified too recently to adopt safely; the session comes up FRESH") {
		t.Fatalf("records = %+v, want the idle guard's notice", f.log.logger.Records())
	}
	if !recordedAt(f, dlog.LevelWarn, opBringUp, abandonedMessage) || len(abandonedFaults(f.db)) != 1 {
		t.Fatalf("records = %+v, want the abandonment warning and its fault", f.log.logger.Records())
	}
}

func TestClassifySourceAbandonsLoudlyWhenTheDirectoryHasNoTranscript(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)
	loseRecordedTranscript(f)
	engage(f)
	session := wsm.Session{Workspace: "w1", VendorSessionID: "vendor-stale"}

	// Act
	got, err := f.fleet.classifySource(context.Background(), f.log.logger, "w1", "/tree/w1", session, true)

	// Assert
	if err != nil || !got.Fresh {
		t.Fatalf("classifySource() = (%+v, %v), want a fresh start", got, err)
	}
	if !recordedAt(f, dlog.LevelWarn, opBringUp, abandonedMessage) || len(abandonedFaults(f.db)) != 1 {
		t.Fatalf("records = %+v, want the abandonment warning and its fault", f.log.logger.Records())
	}
}

func TestEveryAdoptingBranchAsksTheOneAdoptionDecision(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(*fleetFixture) (wsm.Session, bool)
	}{
		{"no session record", func(*fleetFixture) (wsm.Session, bool) { return wsm.Session{}, false }},
		{"a recorded conversation with no transcript", func(f *fleetFixture) (wsm.Session, bool) {
			loseRecordedTranscript(f)
			engage(f)
			return wsm.Session{Workspace: "w1", VendorSessionID: "vendor-stale"}, true
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFleetFixture(t)
			session, exists := tt.arrange(f)

			// Act
			if _, err := f.fleet.classifySource(context.Background(), f.log.logger, "w1", "/tree/w1", session, exists); err != nil {
				t.Fatalf("classifySource: %v", err)
			}

			// Assert
			if !f.accounts.newestProbed {
				t.Fatal("the branch decided without the one adoption decision")
			}
		})
	}
}

func TestTheFleetRecordsEveryReapItWitnesses(t *testing.T) {
	tests := []struct {
		name string
		act  func(*testing.T, *fleetFixture, ids.WorkspaceID)
	}{
		{"a stop", func(t *testing.T, f *fleetFixture, ws ids.WorkspaceID) {
			if err := f.fleet.Stop(context.Background(), ws, true); err != nil {
				t.Fatalf("Stop: %v", err)
			}
		}},
		{"a shim found dead at the next bring-up", func(_ *testing.T, f *fleetFixture, ws ids.WorkspaceID) {
			f.client.reaped = true
			f.fleet.retireReaped(ws)
		}},
		{"an install over a reaped shim", func(t *testing.T, f *fleetFixture, ws ids.WorkspaceID) {
			f.client.reaped = true
			if err := f.fleet.Install(context.Background(), ws, &fakeClient{pid: 4242}); err != nil {
				t.Fatalf("Install: %v", err)
			}
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: a live session whose shim's death is concluded at a
			// known instant.
			f := newFleetFixture(t)
			ws := f.workspace("w1")
			if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
				t.Fatalf("Start: %v", err)
			}
			f.client.reapedAt = fixedNow.Add(-time.Second)

			// Act
			tt.act(t, f, ws.ID)

			// Assert
			if got := f.fleet.writerGoneAt(ws.ID); !got.Equal(fixedNow.Add(-time.Second)) {
				t.Fatalf("writerGoneAt = %v, want the reap's instant", got)
			}
		})
	}
}

func TestTheFleetRecordsNoReapForAShimStillRunning(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)

	// Act
	f.fleet.noteReap("w1", &fakeClient{})

	// Assert
	if got := f.fleet.writerGoneAt("w1"); !got.IsZero() {
		t.Fatalf("writerGoneAt = %v, want none for a shim not yet reaped", got)
	}
}

func TestAResumeCarriesTheWorkspacesRolledBackTurns(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}
	f.db.rolledBack = map[ids.WorkspaceID][]ids.TurnID{ws.ID: {"t2", "t3"}}

	// Act
	if err := f.fleet.Start(context.Background(), ws.ID); err != nil {
		t.Fatalf("Start: %v", err)
	}

	// Assert
	var got []string
	for _, turn := range f.client.requests[0].GetResume().GetRolledBackTurns() {
		got = append(got, turn.GetValue())
	}
	if !slices.Equal(got, []string{"t2", "t3"}) {
		t.Fatalf("resume.rolled_back_turns = %v, want [t2 t3]", got)
	}
}

func TestAResumeIsRefusedWhenTheRolledBackTurnsCannotBeRead(t *testing.T) {
	// Arrange
	f := newFleetFixture(t)
	ws := f.workspace("w1")
	f.db.sessions[ws.ID] = wsm.Session{Workspace: ws.ID, VendorSessionID: "vendor-1"}
	f.db.rolledBackErr = errors.New("database is locked")

	// Act
	err := f.fleet.Start(context.Background(), ws.ID)

	// Assert
	if err == nil {
		t.Fatal("Start resumed without knowing the rolled-back turns")
	}
	if len(f.client.requests) != 0 {
		t.Fatalf("StartSession was sent %d times, want none", len(f.client.requests))
	}
	if !recordedAt(f, dlog.LevelError, "daemon.workspace.start_session", "the rolled-back turns could not be read; the session was not resumed") {
		t.Fatalf("records = %+v, want the failed read at ERROR", f.log.logger.Records())
	}
}
