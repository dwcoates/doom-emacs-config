package server

import (
	"context"
	"crypto/tls"
	"errors"
	"net"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"testing"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

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
}

func (f *fakeSurfaces) Global() dlog.Logger { return fakeLogger{} }

func (f *fakeSurfaces) Workspace(string) (dlog.Logger, error) {
	if f.workspaceErr != nil {
		return nil, f.workspaceErr
	}
	return fakeLogger{}, nil
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
}

func (f *fakeDB) Workspace(_ context.Context, id ids.WorkspaceID) (wsm.Workspace, error) {
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
	closeErr  error

	answerPermissionErr  error
	answerQuestionErr    error
	answerColdGateErr    error
	setPermissionModeErr error
	assignTaskErr        error
}

func (f *fakeVerbs) SetPermissionMode(context.Context, ids.WorkspaceID, string) error {
	return f.setPermissionModeErr
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

func (f *fakeVerbs) Select(context.Context, ids.WorkspaceID) error { return f.selectErr }

func (f *fakeVerbs) Close(context.Context, ids.WorkspaceID) error { return f.closeErr }

func (f *fakeVerbs) AnswerPermission(context.Context, ids.WorkspaceID, *conversationv1.AgentAnswer) error {
	return f.answerPermissionErr
}

func (f *fakeVerbs) AnswerQuestion(context.Context, ids.WorkspaceID, *conversationv1.AgentAnswer) error {
	return f.answerQuestionErr
}

// fakePrompts answers one submission outcome.
type fakePrompts struct {
	prompthandler.Handler
	outcome prompthandler.Outcome
	err     error
}

func (f *fakePrompts) Submit(context.Context, ids.WorkspaceID, *conversationv1.UserSaid, string, conversationv1.PromptOrigin, *feedid.Ref) (prompthandler.Outcome, error) {
	return f.outcome, f.err
}

// fakeQueue answers the three held-prompt verbs.
type fakeQueue struct {
	promptqueue.Queue
	releaseErr error
	dropErr    error
	acceptErr  error
	released   []ids.TurnID
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

func (f *fakeRollout) AdoptHost(context.Context, ids.WorkspaceID) error { return f.adoptHostErr }

func (f *fakeRollout) AdoptWeb(context.Context, ids.WorkspaceID) error { return f.adoptWebErr }

// fakeHealth answers the two health reports.
type fakeHealth struct {
	health.Reporter
	daemon  *agentreplv1.DaemonHealthResponse
	session *agentreplv1.SessionHealthResponse
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
}

func (f *fakeFooter) Topic(ids.WorkspaceID) *publish.Topic[*frontendv1.FooterView] {
	return &f.topic
}

type fakeTopbar struct {
	topbar.Resolver
	topic publish.Topic[*frontendv1.TopbarView]
}

func (f *fakeTopbar) Topic(ids.WorkspaceID) *publish.Topic[*frontendv1.TopbarView] {
	return &f.topic
}

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
	Health     *fakeHealth
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
		Health:     &fakeHealth{},
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
		Health:           h.Health,
		Login:            h.Login,
		Ownership:        h.Ownership,
		SuccessorAddress: func() string { return "127.0.0.1:9999" },
		Feed:             h.Feed,
		Footer:           h.Footer,
		Topbar:           h.Topbar,
		Sidebar:          h.Sidebar,
		Holds:            h.Holds,
		WebappDist:       dist,
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
	h.HTTP = httptest.NewServer(H2C(surface))
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
