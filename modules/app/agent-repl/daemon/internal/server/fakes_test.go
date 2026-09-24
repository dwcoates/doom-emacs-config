package server

import (
	"context"
	"crypto/tls"
	"errors"
	"fmt"
	"net"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"sync"
	"testing"
	"time"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/deploy"
	"claude-repld/internal/dlog"
	"claude-repld/internal/drain"
	"claude-repld/internal/feedid"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/login"
	"claude-repld/internal/merge"
	"claude-repld/internal/prompthandler"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/publish"
	"claude-repld/internal/resolve/feed"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/resolve/holds"
	"claude-repld/internal/resolve/sidebar"
	"claude-repld/internal/resolve/topbar"
	"claude-repld/internal/rollout"
	"claude-repld/internal/workspace"
	"claude-repld/internal/wsm"
)

// The test doubles. Every fake EMBEDS its interface, so a method the test never
// wires panics loudly the moment a handler reaches for it rather than answering
// a zero value that would make a broken handler look correct.

// testWorkspaceID is the workspace every fixture registers.
const testWorkspaceID = ids.WorkspaceID("ws-1")

// testWorkspaceDir is that workspace's registry directory.
const testWorkspaceDir = "/tmp/agent-repl-test-workspace"

// fakeLogger records nothing and never fails; the log's CONTENT is asserted by
// dlog's own tests, not here.
type fakeLogger struct{}

func (fakeLogger) Debug(string, string, dlog.Context) {}
func (fakeLogger) Info(string, string, dlog.Context)  {}
func (fakeLogger) Warn(string, string, dlog.Context)  {}
func (fakeLogger) Error(string, string, dlog.Context) {}
func (f fakeLogger) With(dlog.Context) dlog.Logger    { return f }

// fakeSurfaces answers one logger for everything and records client records.
type fakeSurfaces struct {
	dlog.Surfaces
	clientRecords []dlog.ClientRecord
	workspaceErr  error
	workspace     dlog.Logger
	// global, when set, is the global sink, so a test can assert on what the
	// daemon-wide logger recorded rather than discarding it.
	global dlog.Logger
}

func (f *fakeSurfaces) Global() dlog.Logger {
	if f.global != nil {
		return f.global
	}
	return fakeLogger{}
}

func (f *fakeSurfaces) Workspace(string) (dlog.Logger, error) {
	if f.workspaceErr != nil {
		return nil, f.workspaceErr
	}
	if f.workspace != nil {
		return f.workspace, nil
	}
	return fakeLogger{}, nil
}

// WorkspaceOrCentral implements dlog.Surfaces: the workspace's logger when it
// resolves, and the global one — naming the workspace — when it does not.
func (f *fakeSurfaces) WorkspaceOrCentral(dir string) dlog.Logger {
	log, err := f.Workspace(dir)
	if err != nil {
		return f.Global().With(dlog.Context{dlog.KeyUnroutableWorkspace: dir})
	}
	return log
}

func (f *fakeSurfaces) ClientLog(_ string, rec dlog.ClientRecord) error {
	f.clientRecords = append(f.clientRecords, rec)
	return nil
}

// fakeDB answers the registry lookups the handlers make.
type fakeDB struct {
	wsm.DB
	workspaces   map[ids.WorkspaceID]wsm.Workspace
	repositories []wsm.Repository
	drainPut     []wsm.DrainSchedule
	// feedScalePut records every persisted feed text zoom; feedScalePutErr
	// fails the write; feedScaleRead is what FeedTextScale answers (0 means the
	// default).
	feedScalePut    []float64
	feedScalePutErr error
	feedScaleRead   float64
	// sessions are the durable session records, by workspace.
	sessions map[ids.WorkspaceID]wsm.Session
	// sessionErr fails every session read.
	sessionErr error
	// leases are the occupancy leases in force, by workspace.
	leases map[ids.WorkspaceID]wsm.Lease
	// leaseErr fails every lease read.
	leaseErr error
	// workspaceErr fails every registry read, standing in for the closed
	// state client an exiting daemon leaves behind.
	workspaceErr error
	// workspaceEntered, when set, is closed as the first registry read
	// begins, and that read then waits for workspaceRelease: the seam a test
	// holds a read in flight on.
	workspaceEntered chan struct{}
	workspaceRelease chan struct{}
}

func (f *fakeDB) Session(_ context.Context, id ids.WorkspaceID) (wsm.Session, bool, error) {
	if f.sessionErr != nil {
		return wsm.Session{}, false, f.sessionErr
	}
	session, ok := f.sessions[id]
	return session, ok, nil
}

func (f *fakeDB) Lease(_ context.Context, id ids.WorkspaceID) (wsm.Lease, bool, error) {
	if f.leaseErr != nil {
		return wsm.Lease{}, false, f.leaseErr
	}
	lease, ok := f.leases[id]
	return lease, ok, nil
}

func (f *fakeDB) Workspace(_ context.Context, id ids.WorkspaceID) (wsm.Workspace, error) {
	if f.workspaceEntered != nil {
		close(f.workspaceEntered)
		f.workspaceEntered = nil
		<-f.workspaceRelease
	}
	if f.workspaceErr != nil {
		return wsm.Workspace{}, f.workspaceErr
	}
	record, ok := f.workspaces[id]
	if !ok {
		return wsm.Workspace{}, wsm.ErrNotFound
	}
	return record, nil
}

func (f *fakeDB) ListRepositories(context.Context) ([]wsm.Repository, error) {
	return f.repositories, nil
}

func (f *fakeDB) PutDrainSchedule(_ context.Context, s wsm.DrainSchedule) error {
	f.drainPut = append(f.drainPut, s)
	return nil
}

// PutFeedTextScale records each persisted feed text zoom, or fails when
// feedScalePutErr is set (the persistence-failure path).
func (f *fakeDB) PutFeedTextScale(_ context.Context, scale float64) error {
	if f.feedScalePutErr != nil {
		return f.feedScalePutErr
	}
	f.feedScalePut = append(f.feedScalePut, scale)
	return nil
}

// FeedTextScale answers the seeded scale, defaulting to 1.0 like the real
// store's absent-row case.
func (f *fakeDB) FeedTextScale(context.Context) (float64, error) {
	if f.feedScaleRead == 0 {
		return wsm.DefaultFeedTextScale, nil
	}
	return f.feedScaleRead, nil
}

// fakeOwnership answers a fixed serving standing.
type fakeOwnership struct {
	standing workspace.Standing
	err      error
}

func (f *fakeOwnership) Standing(context.Context, ids.WorkspaceID) (workspace.Standing, error) {
	return f.standing, f.err
}

// fakeVerbs answers whatever the test wired, and records the calls made.
type fakeVerbs struct {
	workspace.Verbs

	interruptOutcome workspace.InterruptOutcome
	interruptErr     error
	interruptTarget  workspace.InterruptTarget

	createTask    wsm.Task
	createTaskErr error

	selectErr error
	// selectEntered, when set, is closed as Select begins; Select then waits
	// for its caller to leave, answers that, and closes selectLeft.
	selectEntered chan struct{}
	selectLeft    chan struct{}

	// markViewed records every workspace MarkWorkspaceViewed resolved onto the
	// verb, so the handler's own resolution is what a test asserts.
	markViewed    []ids.WorkspaceID
	markViewedErr error
	closeErr      error
	openErr       error
	forgetErr     error

	// openStages are replayed into whatever reporter the rpc armed, and
	// openProgress is the reporter itself so a test can assert its absence.
	openStages   []workspace.OpenStage
	openProgress workspace.OpenProgress

	// listTranscripts is what ListTranscripts answers, and
	// listTranscriptsErr the refusal it answers instead.
	listTranscripts    []*agentreplv1.WorkspaceTranscript
	listTranscriptsErr error
	// bindStages are replayed into whatever reporter the bind rpc armed;
	// bindProgress is the reporter itself so a test can assert its absence,
	// bindID the conversation the server resolved onto the verb, and bindErr
	// the refusal the verb answers with.
	bindStages   []workspace.BindStage
	bindProgress workspace.BindProgress
	bindID       string
	bindErr      error

	setModel    string
	setModelErr error

	answerPermissionErr  error
	answerQuestionErr    error
	setPermissionModeErr error
	// selectAccountDir is the root the last SelectAccount named; the answer
	// and the failure the fixture hands back sit beside it.
	selectAccountDir      string
	selectAccountLoggedIn bool
	selectAccountErr      error
	assignTaskErr         error

	// createRec is the workspace a Create returns on success; createErr, when
	// set, is the failure it returns instead.
	createRec wsm.Workspace
	createErr error
	// createStages are the stages Create reports through spec.Progress, in
	// order, before it returns — so a test can drive the server's progress
	// relay without the real naming/worktree steps.
	createStages []workspace.CreateStage
	// createEntered, when set, is closed once Create has been called and has
	// reported its stages; createRelease, when set, blocks Create until the
	// test closes it. Together they let a test cancel the accepting request
	// while Create is mid-flight and prove the work is detached from it.
	createEntered chan struct{}
	createRelease chan struct{}
}

// Create records the spec, reports its scripted stages, optionally blocks until
// released, then returns success or the scripted failure. A cancelled context
// at the point of return is itself returned as the failure, so a test proves
// detachment by asserting Create still SUCCEEDED after the request was
// cancelled.
func (f *fakeVerbs) Create(ctx context.Context, spec workspace.CreateSpec) (wsm.Workspace, error) {
	for _, stage := range f.createStages {
		if spec.Progress != nil {
			spec.Progress.Stage(stage)
		}
	}
	if f.createEntered != nil {
		close(f.createEntered)
	}
	if f.createRelease != nil {
		<-f.createRelease
	}
	if err := ctx.Err(); err != nil {
		return wsm.Workspace{}, err
	}
	if f.createErr != nil {
		return wsm.Workspace{}, f.createErr
	}
	return f.createRec, nil
}

func (f *fakeVerbs) SetModel(_ context.Context, _ ids.WorkspaceID, model string) error {
	f.setModel = model
	return f.setModelErr
}

// openStages records every stage the server's reporter relayed into the verb,
// so a test can assert the rpc armed a reporter (or deliberately did not).
func (f *fakeVerbs) Open(_ context.Context, _ ids.WorkspaceID, progress workspace.OpenProgress) error {
	f.openProgress = progress
	for _, stage := range f.openStages {
		if progress != nil {
			progress.Stage(stage)
		}
	}
	return f.openErr
}

func (f *fakeVerbs) ListTranscripts(context.Context, ids.WorkspaceID) ([]*agentreplv1.WorkspaceTranscript, error) {
	if f.listTranscriptsErr != nil {
		return nil, f.listTranscriptsErr
	}
	return f.listTranscripts, nil
}

// BindSession records the conversation the server resolved onto it and replays
// its scripted stages, so a test can drive the server's progress relay without
// a session fleet.
func (f *fakeVerbs) BindSession(_ context.Context, _ ids.WorkspaceID, vendorSessionID string, progress workspace.BindProgress) error {
	f.bindID = vendorSessionID
	f.bindProgress = progress
	for _, stage := range f.bindStages {
		if progress != nil {
			progress.Stage(stage)
		}
	}
	return f.bindErr
}

func (f *fakeVerbs) SetPermissionMode(context.Context, ids.WorkspaceID, string) error {
	return f.setPermissionModeErr
}

func (f *fakeVerbs) SelectAccount(_ context.Context, _ ids.WorkspaceID, configDir string) (bool, error) {
	f.selectAccountDir = configDir
	return f.selectAccountLoggedIn, f.selectAccountErr
}

func (f *fakeVerbs) AssignTask(context.Context, ids.WorkspaceID, *ids.TaskID) error {
	return f.assignTaskErr
}

func (f *fakeVerbs) Interrupt(_ context.Context, _ ids.WorkspaceID, target workspace.InterruptTarget, _ bool) (workspace.InterruptOutcome, error) {
	f.interruptTarget = target
	return f.interruptOutcome, f.interruptErr
}

func (f *fakeVerbs) CreateTask(context.Context, string) (wsm.Task, error) {
	return f.createTask, f.createTaskErr
}

func (f *fakeVerbs) Select(ctx context.Context, _ ids.WorkspaceID) error {
	if f.selectEntered != nil {
		close(f.selectEntered)
		<-ctx.Done()
		defer close(f.selectLeft)
		return fmt.Errorf("select: revive: %w", ctx.Err())
	}
	return f.selectErr
}

func (f *fakeVerbs) MarkViewed(_ context.Context, ws ids.WorkspaceID) error {
	f.markViewed = append(f.markViewed, ws)
	return f.markViewedErr
}

func (f *fakeVerbs) Close(context.Context, ids.WorkspaceID) error { return f.closeErr }

func (f *fakeVerbs) Forget(context.Context, ids.WorkspaceID) error { return f.forgetErr }

func (f *fakeVerbs) AnswerPermission(context.Context, ids.WorkspaceID, *conversationv1.AgentAnswer) error {
	return f.answerPermissionErr
}

func (f *fakeVerbs) AnswerQuestion(context.Context, ids.WorkspaceID, *conversationv1.AgentAnswer) error {
	return f.answerQuestionErr
}

// fakePrompts answers one submission outcome and records the said it was
// handed, so a reply-to-a-past-response test can assert the daemon prepended
// the referenced response before delivery.
type fakePrompts struct {
	prompthandler.Handler
	outcome  prompthandler.Outcome
	err      error
	lastSaid *conversationv1.UserSaid
}

func (f *fakePrompts) Submit(_ context.Context, _ ids.WorkspaceID, said *conversationv1.UserSaid, _ string, _ conversationv1.PromptOrigin, _ *feedid.Ref) (prompthandler.Outcome, error) {
	f.lastSaid = said
	return f.outcome, f.err
}

// fakeQueue answers the three held-prompt verbs and the edit's steps.
type fakeQueue struct {
	promptqueue.Queue
	releaseErr error
	dropErr    error
	acceptErr  error
	released   []ids.TurnID

	// beginErr, commitErr and cancelErr answer the edit's three steps.
	beginErr  error
	commitErr error
	cancelErr error
	// editorLive is what the begin's editor probe answered.
	editorLive []bool
	// committed is every commit's new content, in order.
	committed []*conversationv1.UserSaid
	// cancelled is every cancelled turn, in order.
	cancelled []ids.TurnID
	// editorGone is every workspace whose editor left, in order; the host
	// stream's own goroutine writes it, so goneMu guards it.
	goneMu     sync.Mutex
	editorGone []ids.WorkspaceID
	// edit is the standing edit Editing answers, nil when none stands.
	edit *promptqueue.Edit
}

func (f *fakeQueue) BeginEdit(_ context.Context, _ ids.WorkspaceID, _ ids.TurnID, editor promptqueue.EditorProbe) error {
	f.editorLive = append(f.editorLive, editor())
	return f.beginErr
}

func (f *fakeQueue) CommitEdit(_ context.Context, _ ids.WorkspaceID, _ ids.TurnID, said *conversationv1.UserSaid) error {
	f.committed = append(f.committed, said)
	return f.commitErr
}

func (f *fakeQueue) CancelEdit(_ context.Context, _ ids.WorkspaceID, turn ids.TurnID) error {
	f.cancelled = append(f.cancelled, turn)
	return f.cancelErr
}

func (f *fakeQueue) EditorGone(ws ids.WorkspaceID) {
	f.goneMu.Lock()
	defer f.goneMu.Unlock()
	f.editorGone = append(f.editorGone, ws)
}

// editorsGone answers every workspace whose editor left, in order.
func (f *fakeQueue) editorsGone() []ids.WorkspaceID {
	f.goneMu.Lock()
	defer f.goneMu.Unlock()
	return append([]ids.WorkspaceID(nil), f.editorGone...)
}

func (f *fakeQueue) Editing(ids.WorkspaceID) (promptqueue.Edit, bool) {
	if f.edit == nil {
		return promptqueue.Edit{}, false
	}
	return *f.edit, true
}

func (f *fakeQueue) Release(_ context.Context, _ ids.WorkspaceID, turn ids.TurnID) error {
	f.released = append(f.released, turn)
	return f.releaseErr
}

func (f *fakeQueue) Drop(context.Context, ids.WorkspaceID, ids.TurnID) error { return f.dropErr }

func (f *fakeQueue) Accept(context.Context, ids.WorkspaceID, ids.TurnID) error { return f.acceptErr }

// fakeMerge answers the orchestrator's four server-facing verbs.
type fakeMerge struct {
	merge.Orchestrator
	enqueueErr       error
	pauseErr         error
	pauseScope       *merge.RepositoryScope
	answerDequeueErr error
	evictErr         error
}

func (f *fakeMerge) Enqueue(context.Context, ids.WorkspaceID) error { return f.enqueueErr }

func (f *fakeMerge) AnswerDequeue(context.Context, ids.WorkspaceID, bool) error {
	return f.answerDequeueErr
}

func (f *fakeMerge) Evict(context.Context, ids.WorkspaceID) error { return f.evictErr }

func (f *fakeMerge) Pause(_ context.Context, scope *merge.RepositoryScope) error {
	f.pauseScope = scope
	return f.pauseErr
}

// fakeDrain answers the schedule verbs and records what it was handed.
type fakeDrain struct {
	drain.Controller
	scheduled []wsm.DrainSchedule
	cancelErr error
	nowReason *agentreplv1.DrainReason
}

func (f *fakeDrain) Schedule(_ context.Context, s wsm.DrainSchedule) error {
	f.scheduled = append(f.scheduled, s)
	return nil
}

func (f *fakeDrain) Cancel(context.Context) error { return f.cancelErr }

func (f *fakeDrain) ShutdownNow(_ context.Context, reason *agentreplv1.DrainReason) error {
	f.nowReason = reason
	return nil
}

// fakeRollout answers the two adoption calls.
type fakeRollout struct {
	rollout.Controller
	adoptHostErr error
	adoptWebErr  error
}

// fakeDeployer answers the Deploy rpc with a scripted result.
type fakeDeployer struct {
	mu     sync.Mutex
	forced []bool
	result deploy.Result
	err    error
}

func (f *fakeDeployer) Deploy(_ context.Context, force bool) (deploy.Result, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.forced = append(f.forced, force)
	return f.result, f.err
}

func (f *fakeRollout) AdoptHost(context.Context, ids.WorkspaceID) error { return f.adoptHostErr }

func (f *fakeRollout) AdoptWeb(context.Context, ids.WorkspaceID) error { return f.adoptWebErr }

// fakeHealth answers the two health reports.
type fakeHealth struct {
	health.Reporter
	daemon  *agentreplv1.DaemonHealthResponse
	session *agentreplv1.SessionHealthResponse
	// faults are the open faults every scope answers with.
	faults []wsm.Fault
	// faultsErr fails every open-fault read.
	faultsErr error
}

func (f *fakeHealth) OpenFaults(context.Context, wsm.FaultScope) ([]wsm.Fault, error) {
	if f.faultsErr != nil {
		return nil, f.faultsErr
	}
	return f.faults, nil
}

// fakeSessionFacts is a SessionFacts. It answers per workspace, so a test
// arranges a live session by putting facts in and an absent one by leaving
// them out.
type fakeSessionFacts struct {
	facts map[ids.WorkspaceID]HostFacts
}

func (f *fakeSessionFacts) HostSessionFacts(ws ids.WorkspaceID) (HostFacts, bool) {
	got, ok := f.facts[ws]
	return got, ok
}

func (f *fakeHealth) Daemon(context.Context) (*agentreplv1.DaemonHealthResponse, error) {
	return f.daemon, nil
}

func (f *fakeHealth) Session(context.Context, ids.WorkspaceID) (*agentreplv1.SessionHealthResponse, error) {
	return f.session, nil
}

// fakeLogin answers the pty verbs and drives the terminal stream by hand.
type fakeLogin struct {
	login.Manager
	configDir   string
	openErr     error
	watchFrames chan login.Output
	watchErr    error
	sendErr     error
	closeErr    error
}

func (f *fakeLogin) Open(context.Context, ids.WorkspaceID) (string, error) {
	return f.configDir, f.openErr
}

func (f *fakeLogin) Watch(context.Context, ids.WorkspaceID) (<-chan login.Output, error) {
	if f.watchErr != nil {
		return nil, f.watchErr
	}
	return f.watchFrames, nil
}

func (f *fakeLogin) SendKeystrokes(context.Context, ids.WorkspaceID, []byte) error {
	return f.sendErr
}

func (f *fakeLogin) SendResize(context.Context, ids.WorkspaceID, login.Resize) error {
	return f.sendErr
}

func (f *fakeLogin) Close(context.Context, ids.WorkspaceID) error { return f.closeErr }

// fakeFeed answers the page and tail seams.
type fakeFeed struct {
	feed.Resolver
	page      *frontendv1.FeedPage
	token     *agentreplv1.FeedWatchToken
	openErr   error
	nextPage  *frontendv1.FeedPage
	nextErr   error
	tail      *fakeTail
	tailErr   error
	lastFeed  feedid.Feed
	lastRead  feed.ReaderID
	openCalls int
	// finals is the ordered selectable final-response set SelectResponse walks;
	// markdown is each selectable row's copied markdown, keyed by FeedId value,
	// for the reply-prefix path.
	finals   []*frontendv1.FeedId
	markdown map[string]string
}

func (f *fakeFeed) FinalResponses(ids.WorkspaceID) []*frontendv1.FeedId {
	return f.finals
}

func (f *fakeFeed) ResponseMarkdown(_ ids.WorkspaceID, id *frontendv1.FeedId) (string, bool) {
	md, ok := f.markdown[id.GetValue()]
	return md, ok
}

func (f *fakeFeed) OpenPage(_ context.Context, _ ids.WorkspaceID, target feedid.Feed, reader feed.ReaderID) (*frontendv1.FeedPage, *agentreplv1.FeedWatchToken, error) {
	f.openCalls++
	f.lastFeed = target
	f.lastRead = reader
	return f.page, f.token, f.openErr
}

func (f *fakeFeed) NextPage(context.Context, ids.WorkspaceID, feedid.Feed, feed.ReaderID) (*frontendv1.FeedPage, error) {
	return f.nextPage, f.nextErr
}

func (f *fakeFeed) Tail(context.Context, ids.WorkspaceID, feedid.Feed, *agentreplv1.FeedWatchToken) (feed.Tail, error) {
	if f.tailErr != nil {
		return nil, f.tailErr
	}
	return f.tail, nil
}

// fakeTail hands the handler a row channel the test drives.
type fakeTail struct {
	rows  chan *frontendv1.FeedRow
	token *agentreplv1.FeedWatchToken
}

func (t *fakeTail) Rows(context.Context) <-chan *frontendv1.FeedRow { return t.rows }

func (t *fakeTail) Token() *agentreplv1.FeedWatchToken { return t.token }

// fakeFooter, fakeTopbar, fakeSidebar and fakeHolds each own one topic per
// workspace, which is exactly the publisher seam the handlers subscribe to.
type fakeFooter struct {
	footer.Resolver
	topic publish.Topic[*frontendv1.FooterView]

	edges participantRecorder
}

func (f *fakeFooter) Topic(ids.WorkspaceID) *publish.Topic[*frontendv1.FooterView] {
	return &f.topic
}

// SetParticipants records the stream-liveness edges the server states, which
// is the other two hops of connectivity truth.
func (f *fakeFooter) SetParticipants(ws ids.WorkspaceID, host, web bool) {
	f.edges.record(participantEdge{WS: ws, Host: host, Web: web})
}

// Participants is the recorded edges, in order.
func (f *fakeFooter) Participants() []participantEdge { return f.edges.all() }

// AwaitEdge blocks for the next participant edge, so a test synchronizes on
// the handler's own publication rather than on a delay.
func (f *fakeFooter) AwaitEdge(t *testing.T) participantEdge { return f.edges.await(t) }

// participantEdge is one stated host/web stream liveness for a workspace.
type participantEdge struct {
	WS   ids.WorkspaceID
	Host bool
	Web  bool
}

// participantRecorder records the edges and hands each one to a waiter, which
// is what lets a test synchronize on an asynchronous close edge without a
// delay. The channel is generously buffered: a dropped edge would be a silent
// hang, so an overflow fails loudly instead.
type participantRecorder struct {
	mu   sync.Mutex
	all_ []participantEdge
	ch   chan participantEdge
}

func (p *participantRecorder) record(e participantEdge) {
	p.mu.Lock()
	if p.ch == nil {
		p.ch = make(chan participantEdge, 64)
	}
	p.all_ = append(p.all_, e)
	ch := p.ch
	p.mu.Unlock()
	select {
	case ch <- e:
	default:
		panic("the participant edge channel overflowed; the recorder lost an edge")
	}
}

func (p *participantRecorder) all() []participantEdge {
	p.mu.Lock()
	defer p.mu.Unlock()
	return append([]participantEdge(nil), p.all_...)
}

func (p *participantRecorder) await(t *testing.T) participantEdge {
	t.Helper()
	p.mu.Lock()
	if p.ch == nil {
		p.ch = make(chan participantEdge, 64)
	}
	ch := p.ch
	p.mu.Unlock()
	select {
	case e := <-ch:
		return e
	case <-time.After(10 * time.Second):
		t.Fatal("no participant edge arrived")
		return participantEdge{}
	}
}

type fakeTopbar struct {
	topbar.Resolver
	topic publish.Topic[*frontendv1.TopbarView]

	edges participantRecorder
}

func (f *fakeTopbar) Topic(ids.WorkspaceID) *publish.Topic[*frontendv1.TopbarView] {
	return &f.topic
}

// SetParticipants records the stream-liveness edges the server states.
func (f *fakeTopbar) SetParticipants(ws ids.WorkspaceID, host, web bool) {
	f.edges.record(participantEdge{WS: ws, Host: host, Web: web})
}

// Participants is the recorded edges, in order.
func (f *fakeTopbar) Participants() []participantEdge { return f.edges.all() }

type fakeSidebar struct {
	sidebar.Resolver
	topic publish.Topic[*frontendv1.WorkspaceRoster]
}

func (f *fakeSidebar) Topic() *publish.Topic[*frontendv1.WorkspaceRoster] { return &f.topic }

type fakeHolds struct {
	holds.Resolver
	topic publish.Topic[*frontendv1.DaemonHoldTray]
}

func (f *fakeHolds) Topic(ids.WorkspaceID) *publish.Topic[*frontendv1.DaemonHoldTray] {
	return &f.topic
}

// harness is one running surface over real Connect handlers.
type harness struct {
	t          *testing.T
	Server     Server
	HTTP       *httptest.Server
	Client     agentreplv1connect.AgentReplClient
	DB         *fakeDB
	Ownership  *fakeOwnership
	Verbs      *fakeVerbs
	Prompts    *fakePrompts
	Queue      *fakeQueue
	Merge      *fakeMerge
	Drain      *fakeDrain
	Rollout    *fakeRollout
	Deployer   *fakeDeployer
	Health     *fakeHealth
	Facts      *fakeSessionFacts
	Login      *fakeLogin
	Feed       *fakeFeed
	Footer     *fakeFooter
	Topbar     *fakeTopbar
	Sidebar    *fakeSidebar
	Holds      *fakeHolds
	Surfaces   *fakeSurfaces
	WebappDist string
}

// option customizes a harness before it is built.
type option func(*Deps)

// newHarness builds the surface, serves it over httptest with h2c, and dials it
// with a real Connect client. Every test that exercises a handler goes through
// the real transport, so the codecs and the transport are covered by every one.
func newHarness(t *testing.T, opts ...option) *harness {
	t.Helper()
	t.Setenv("AGENT_REPL_FORBID_VENDOR_CALLS", "1")

	dist := t.TempDir()
	if err := os.WriteFile(filepath.Join(dist, entryPoint), []byte("<html>first</html>"), 0o644); err != nil {
		t.Fatalf("write the entry point: %v", err)
	}

	h := &harness{
		t: t,
		DB: &fakeDB{workspaces: map[ids.WorkspaceID]wsm.Workspace{
			testWorkspaceID: {ID: testWorkspaceID, Dir: testWorkspaceDir, Name: "test"},
		}},
		Ownership:  &fakeOwnership{standing: workspace.StandingOwned},
		Verbs:      &fakeVerbs{},
		Prompts:    &fakePrompts{},
		Queue:      &fakeQueue{},
		Merge:      &fakeMerge{},
		Drain:      &fakeDrain{},
		Rollout:    &fakeRollout{},
		Deployer:   &fakeDeployer{},
		Health:     &fakeHealth{},
		Facts:      &fakeSessionFacts{facts: map[ids.WorkspaceID]HostFacts{}},
		Login:      &fakeLogin{},
		Feed:       &fakeFeed{},
		Footer:     &fakeFooter{},
		Topbar:     &fakeTopbar{},
		Sidebar:    &fakeSidebar{},
		Holds:      &fakeHolds{},
		Surfaces:   &fakeSurfaces{},
		WebappDist: dist,
	}

	deps := Deps{
		DB:               h.DB,
		Prompts:          h.Prompts,
		Queue:            h.Queue,
		Verbs:            h.Verbs,
		Merge:            h.Merge,
		Drain:            h.Drain,
		Rollout:          h.Rollout,
		Deploy:           h.Deployer,
		Health:           h.Health,
		SessionFacts:     h.Facts,
		Login:            h.Login,
		Ownership:        h.Ownership,
		SuccessorAddress: func() string { return "127.0.0.1:9999" },
		Feed:             h.Feed,
		Footer:           h.Footer,
		Topbar:           h.Topbar,
		Sidebar:          h.Sidebar,
		Holds:            h.Holds,
		WebappDist:       dist,
		ImageOrigin:      http.NotFoundHandler(),
		Log:              h.Surfaces,
	}
	for _, apply := range opts {
		apply(&deps)
	}

	surface, err := New(deps)
	if err != nil {
		t.Fatalf("build the surface: %v", err)
	}
	h.Server = surface
	h.HTTP = httptest.NewServer(H2C(surface, h.Surfaces.Global()))
	t.Cleanup(func() {
		h.HTTP.Close()
		_ = surface.Close()
	})
	h.Client = agentreplv1connect.NewAgentReplClient(h.HTTP.Client(), h.HTTP.URL)
	return h
}

// h2cClient dials the surface over CLEARTEXT HTTP/2, which is the other half of
// the one-listener contract: one origin serving HTTP/1.1 and h2c alike.
func h2cClient() *http.Client {
	return &http.Client{Transport: &http2.Transport{
		AllowHTTP: true,
		DialTLSContext: func(ctx context.Context, network, addr string, _ *tls.Config) (net.Conn, error) {
			return (&net.Dialer{}).DialContext(ctx, network, addr)
		},
	}}
}

// ref is the workspace ref a test echoes back.
func ref() *workspacev1.WorkspaceRef {
	return &workspacev1.WorkspaceRef{Id: string(testWorkspaceID), Dir: testWorkspaceDir}
}

// connectCode answers a Connect error's code, failing the test when err is not
// a Connect error at all.
func connectCode(t *testing.T, err error) connect.Code {
	t.Helper()
	var cerr *connect.Error
	if !errors.As(err, &cerr) {
		t.Fatalf("expected a Connect error, got %v", err)
	}
	return cerr.Code()
}

// logRecord is one record a recordingLogger captured.
type logRecord struct {
	Level     string
	Operation string
	Message   string
	Context   dlog.Context
}

// recordingLogger captures every record so a test can assert BOTH the shape of
// what was logged and that nothing was logged at a level it forbids.
type recordingLogger struct {
	records []logRecord
}

func (l *recordingLogger) Debug(op, msg string, c dlog.Context) { l.record("DEBUG", op, msg, c) }
func (l *recordingLogger) Info(op, msg string, c dlog.Context)  { l.record("INFO", op, msg, c) }
func (l *recordingLogger) Warn(op, msg string, c dlog.Context)  { l.record("WARN", op, msg, c) }
func (l *recordingLogger) Error(op, msg string, c dlog.Context) { l.record("ERROR", op, msg, c) }
func (l *recordingLogger) With(dlog.Context) dlog.Logger        { return l }

func (l *recordingLogger) record(level, op, msg string, c dlog.Context) {
	l.records = append(l.records, logRecord{Level: level, Operation: op, Message: msg, Context: c})
}

// at answers the records captured at one level.
func (l *recordingLogger) at(level string) []logRecord {
	var out []logRecord
	for _, rec := range l.records {
		if rec.Level == level {
			out = append(out, rec)
		}
	}
	return out
}

// receiveHostEvent reads the host stream until an EVENT arm arrives, skipping
// the `host` state pushes.
//
// Every fresh subscription now opens with the workspace's host STATE (the
// stream's whole point), so a test asserting on one of the four event arms has
// to read past it rather than assume the first frame is its own.
// receiveWebEvent reads past the web stream's opening `session_identity`
// state push to the next EVENT, exactly as receiveHostEvent reads past the
// host stream's state.
func receiveWebEvent(
	t *testing.T,
	stream *connect.ServerStreamForClient[agentreplv1.WatchWebWorkspaceResponse],
) *agentreplv1.WatchWebWorkspaceResponse {
	t.Helper()
	for stream.Receive() {
		if stream.Msg().GetSessionIdentity() != nil {
			continue
		}
		return stream.Msg()
	}
	t.Fatalf("the web stream ended before an event arrived: %v", stream.Err())
	return nil
}

func receiveHostEvent(
	t *testing.T,
	stream *connect.ServerStreamForClient[agentreplv1.WatchHostWorkspaceResponse],
) *agentreplv1.WatchHostWorkspaceResponse {
	t.Helper()
	for stream.Receive() {
		if stream.Msg().GetHost() != nil {
			continue
		}
		return stream.Msg()
	}
	t.Fatalf("the host stream ended before an event arrived: %v", stream.Err())
	return nil
}
