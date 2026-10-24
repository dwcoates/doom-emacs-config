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
	"slices"
	"strings"
	"sync"
	"sync/atomic"
	"testing"
	"time"

	"connectrpc.com/connect"
	"golang.org/x/net/http2"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	shimv1 "agentrepl/proto/shim/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/classifierupdate"
	"agent-repld/internal/commandcatalog"
	"agent-repld/internal/creategate"
	"agent-repld/internal/deploy"
	"agent-repld/internal/desktopnotify"
	"agent-repld/internal/dlog"
	"agent-repld/internal/drain"
	"agent-repld/internal/feedid"
	"agent-repld/internal/health"
	"agent-repld/internal/ids"
	"agent-repld/internal/login"
	"agent-repld/internal/merge"
	"agent-repld/internal/opentabs"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/publish"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/resolve/topbar"
	"agent-repld/internal/restarttiming"
	"agent-repld/internal/restarttiming/restarttimingtest"
	"agent-repld/internal/rollout"
	"agent-repld/internal/runtimemetrics"
	"agent-repld/internal/subagentmaker"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
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
	// centralRecords are the records filed with CentralClientLog (the webhost's).
	centralRecords []dlog.ClientRecord
	workspaceErr   error
	workspace      dlog.Logger
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

func (f *fakeSurfaces) CentralClientLog(rec dlog.ClientRecord) error {
	f.centralRecords = append(f.centralRecords, rec)
	return nil
}

// fakeDB answers the registry lookups the handlers make.
type fakeDB struct {
	wsm.DB
	// subscribed records every workspace subscription; replacesSub,
	// subscribeErr, endsSub and unsubscribeErr answer them.
	subscribed     []wsm.WorkspaceSubscription
	replacesSub    bool
	subscribeErr   error
	unsubscribed   [][2]ids.WorkspaceID
	endsSub        bool
	unsubscribeErr error
	workspaces     map[ids.WorkspaceID]wsm.Workspace
	repositories   []wsm.Repository
	// listRepositoriesErr fails the repository read, which the slice cannot.
	listRepositoriesErr error
	drainPut            []wsm.DrainSchedule
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
	// workspaceErrs fails Workspace for the one workspace each names.
	workspaceErrs map[ids.WorkspaceID]error
	// turnStarted answers Turn's start by turn; turnStartedErr fails every
	// read instead (wsm.ErrNotFound for a turn the workspace never recorded).
	turnStarted    map[ids.TurnID]time.Time
	turnStartedErr error
	// openTurns is what OpenTurns answers; openTurnsErr fails the read.
	openTurns    []wsm.Turn
	openTurnsErr error
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
	if err := f.workspaceErrs[id]; err != nil {
		return wsm.Workspace{}, err
	}
	record, ok := f.workspaces[id]
	if !ok {
		return wsm.Workspace{}, wsm.ErrNotFound
	}
	return record, nil
}

func (f *fakeDB) ListRepositories(context.Context) ([]wsm.Repository, error) {
	if f.listRepositoriesErr != nil {
		return nil, f.listRepositoriesErr
	}
	return f.repositories, nil
}

// Turn answers the turn with its seeded start time, or turnStartedErr when
// set (e.g. wsm.ErrNotFound for a turn the workspace never recorded).
func (f *fakeDB) Turn(_ context.Context, ws ids.WorkspaceID, turn ids.TurnID) (wsm.Turn, error) {
	if f.turnStartedErr != nil {
		return wsm.Turn{}, f.turnStartedErr
	}
	return wsm.Turn{ID: turn, Workspace: ws, StartedAt: f.turnStarted[turn]}, nil
}

// OpenTurns answers the seeded open turns, or openTurnsErr when set.
func (f *fakeDB) OpenTurns(context.Context, ids.WorkspaceID) ([]wsm.Turn, error) {
	return f.openTurns, f.openTurnsErr
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

// retargetCall is one Retarget the operation received.
type retargetCall struct {
	ws     ids.WorkspaceID
	parent string
}

// fakeRetarget answers err and records each retarget.
type fakeRetarget struct {
	err   error
	calls []retargetCall
}

func (f *fakeRetarget) Retarget(_ context.Context, ws ids.WorkspaceID, parent *ids.WorkspaceID) error {
	call := retargetCall{ws: ws}
	if parent != nil {
		call.parent = string(*parent)
	}
	f.calls = append(f.calls, call)
	return f.err
}

// AnswerColdGate records the request the handler handed it.
func (v *fakeVerbs) AnswerColdGate(_ context.Context, _ ids.WorkspaceID, answer *agentreplv1.AnswerColdGateRequest) error {
	v.coldGateAnswers = append(v.coldGateAnswers, answer)
	return nil
}

// fakeVerbs answers whatever the test wired, and records the calls made.
type fakeVerbs struct {
	workspace.Verbs
	// api answers the agent API's verbs.
	api agentAPI

	// coldGateAnswers records every AnswerColdGate request the handler handed
	// the verb, in order.
	coldGateAnswers []*agentreplv1.AnswerColdGateRequest

	// createSpecs records every Create spec, in order.
	createSpecs []workspace.CreateSpec
	// swaps records every Swap pair; swapErr is what it answers.
	swaps   [][2]ids.WorkspaceID
	swapErr error
	// moves records every Move pair; moveErr is what it answers.
	moves   [][2]ids.WorkspaceID
	moveErr error

	// editorOpens records every relayed open, a workspace file's path or a
	// daemon file's.
	editorOpens []string
	// feedLinkUnresolved and feedLinkErr are what OpenFeedLink answers.
	feedLinkUnresolved *workspace.UnresolvedLink
	feedLinkErr        error
	// feedLinkReport is each OpenFeedLink call's report flag, in order.
	feedLinkReport []bool

	interruptOutcome workspace.InterruptOutcome
	interruptErr     error
	interruptTarget  workspace.InterruptTarget

	createTask    wsm.Task
	createTaskErr error
	// taskChanges records every UpdateTask change the handler resolved.
	taskChanges []wsm.TaskChange

	selectErr error
	// selectEntered, when set, is closed as Select begins; Select then waits
	// for its caller to leave, answers that, and closes selectLeft.
	selectEntered chan struct{}
	selectLeft    chan struct{}

	// markViewed records every workspace MarkWorkspaceViewed resolved onto the
	// verb, so the handler's own resolution is what a test asserts.
	markViewed    []ids.WorkspaceID
	markViewedErr error
	// hiddenSets records every SetWorkspaceHidden the verb received;
	// setHiddenErr is what it answers.
	hiddenSets   []hiddenSet
	setHiddenErr error
	// principalLinks records every link SetPrincipalLink received, "" for a
	// clear; setPrincipalLinkErr is what it answers.
	principalLinks      []string
	setPrincipalLinkErr error
	// opens and selects count every Open and Select the handlers made: the
	// two verbs that bring a workspace up.
	opens, selects atomic.Int32
	// lastSelectHistory is the workspace.SelectionHistory the newest Select
	// was asked for.
	lastSelectHistory atomic.Int32
	// registeredPaths records every RegisterRepository path the verb received.
	registeredPaths []string
	// arrangements records every ArrangeSidebar verb call, spelled
	// "<method> <args>", and arrangeErr is what each answers instead.
	arrangements []string
	arrangeErr   error
	// arrangement is what SidebarArrangement answers, arrangementErr its
	// failure instead.
	arrangement    []sidebar.ArrangedRepository
	arrangementErr error

	// folds records every FoldRepository the handler resolved onto the verb,
	// and foldErr is what the verb answers instead.
	folds   []repositoryFold
	foldErr error
	// sectionFolds records every sidebar view change
	// the handler resolved onto the verbs, and sectionFoldErr is what each of
	// those verbs answers instead.
	sectionFolds   []sectionFold
	sectionFoldErr error
	closeErr       error
	// closedBy is every cause Close was asked with, in order.
	closedBy  []wsm.CloseCause
	openErr   error
	forgetErr error

	// openStages are replayed into whatever reporter the rpc armed, and
	// openProgress is the reporter itself so a test can assert its absence.
	openStages   []workspace.OpenStage
	openProgress workspace.OpenProgress
	// revertOpens counts every OpenToRevertMerge the handler made, and
	// revertErr is what each answers.
	revertOpens atomic.Int32
	revertErr   error

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

	setEffort    conversationv1.AgentEffortLevel
	setEffortErr error

	// retries counts every RetryVendorError, answered with retryResponse and
	// retryErr.
	retries       int
	retryResponse *shimv1.RetryVendorErrorNowResponse
	retryErr      error

	answerPermissionErr error
	answerQuestionErr   error
	// draftEdits is every UpdateQuestionDraft call's ask and edit, in order;
	// draftErr is what it answers.
	draftEdits           []fakeServerDraftEdit
	draftErr             error
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

	// beginErr refuses a kill's or a nuke's fast half; teardownErr is what
	// its teardown answers. teardownRan, when set, receives the error of the
	// context each teardown ran under, read while it runs, after
	// teardownRelease (when set) is closed.
	beginErr        error
	teardownErr     error
	teardownRan     chan error
	teardownRelease chan struct{}

	// rollBackReq records every RollBack request, whole, in order;
	// rollBackResult and rollBackErr answer it.
	rollBackReq    []workspace.RollbackRequest
	rollBackResult workspace.RollbackResult
	rollBackErr    error
}

// RollBack records the request and answers the scripted result or error.
func (f *fakeVerbs) RollBack(_ context.Context, _ ids.WorkspaceID, req workspace.RollbackRequest) (workspace.RollbackResult, error) {
	f.rollBackReq = append(f.rollBackReq, req)
	return f.rollBackResult, f.rollBackErr
}

// BeginKill answers the scripted fast half and a teardown that reports the
// context it ran under.
func (f *fakeVerbs) BeginKill(context.Context, ids.WorkspaceID) (workspace.Teardown, error) {
	return f.begin()
}

// BeginNuke is BeginKill for a nuke.
func (f *fakeVerbs) BeginNuke(context.Context, ids.WorkspaceID) (workspace.Teardown, error) {
	return f.begin()
}

func (f *fakeVerbs) begin() (workspace.Teardown, error) {
	if f.beginErr != nil {
		return nil, f.beginErr
	}
	return func(ctx context.Context) error {
		if f.teardownRelease != nil {
			<-f.teardownRelease
		}
		if f.teardownRan != nil {
			f.teardownRan <- ctx.Err()
		}
		return f.teardownErr
	}, nil
}

// Create records the spec, reports its scripted stages, optionally blocks until
// released, then returns success or the scripted failure. A cancelled context
// at the point of return is itself returned as the failure, so a test proves
// detachment by asserting Create still SUCCEEDED after the request was
// cancelled.
func (f *fakeVerbs) Create(ctx context.Context, spec workspace.CreateSpec) (wsm.Workspace, error) {
	f.createSpecs = append(f.createSpecs, spec)
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

func (f *fakeVerbs) SetEffort(_ context.Context, _ ids.WorkspaceID, level conversationv1.AgentEffortLevel) error {
	f.setEffort = level
	return f.setEffortErr
}

func (f *fakeVerbs) RetryVendorError(_ context.Context, _ ids.WorkspaceID) (*shimv1.RetryVendorErrorNowResponse, error) {
	f.retries++
	return f.retryResponse, f.retryErr
}

func (f *fakeVerbs) SetModel(_ context.Context, _ ids.WorkspaceID, model string) error {
	f.setModel = model
	return f.setModelErr
}

// openStages records every stage the server's reporter relayed into the verb,
// so a test can assert the rpc armed a reporter (or deliberately did not).
func (f *fakeVerbs) Open(_ context.Context, _ ids.WorkspaceID, progress workspace.OpenProgress) error {
	f.opens.Add(1)
	f.openProgress = progress
	for _, stage := range f.openStages {
		if progress != nil {
			progress.Stage(stage)
		}
	}
	return f.openErr
}

// OpenToRevertMerge counts the revert-merge opens the rpc routed here.
func (f *fakeVerbs) OpenToRevertMerge(context.Context, ids.WorkspaceID, workspace.OpenProgress) error {
	f.revertOpens.Add(1)
	return f.revertErr
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

func (f *fakeVerbs) Select(ctx context.Context, _ ids.WorkspaceID, history workspace.SelectionHistory) error {
	f.selects.Add(1)
	f.lastSelectHistory.Store(int32(history))
	if f.selectEntered != nil {
		close(f.selectEntered)
		<-ctx.Done()
		defer close(f.selectLeft)
		return fmt.Errorf("select: revive: %w", ctx.Err())
	}
	return f.selectErr
}

// hiddenSet is one SetHidden the verb received.
type hiddenSet struct {
	ws     ids.WorkspaceID
	hidden bool
}

func (f *fakeVerbs) SetHidden(_ context.Context, ws ids.WorkspaceID, hidden bool) error {
	f.hiddenSets = append(f.hiddenSets, hiddenSet{ws: ws, hidden: hidden})
	return f.setHiddenErr
}

func (f *fakeVerbs) SetPrincipalLink(_ context.Context, _ ids.WorkspaceID, link *string) error {
	url := ""
	if link != nil {
		url = *link
	}
	f.principalLinks = append(f.principalLinks, url)
	return f.setPrincipalLinkErr
}

func (f *fakeVerbs) RegisterRepository(_ context.Context, path string) (workspace.RegisteredRepository, error) {
	f.registeredPaths = append(f.registeredPaths, path)
	return workspace.RegisteredRepository{}, nil
}

func (f *fakeVerbs) UpdateTask(_ context.Context, _ ids.TaskID, change wsm.TaskChange) error {
	f.taskChanges = append(f.taskChanges, change)
	return nil
}

func (f *fakeVerbs) MarkViewed(_ context.Context, ws ids.WorkspaceID) error {
	f.markViewed = append(f.markViewed, ws)
	return f.markViewedErr
}

// agentAPI is what the fake verbs answer, and record, for the agent API's
// verbs.
type agentAPI struct {
	status       *agentreplv1.WorkspaceStatus
	statusErr    error
	selfCloses   []ids.WorkspaceID
	closesLater  bool
	closeSelfErr error
	oneShots     []fakeOneShot
	directive    string
	oneShotErr   error
	awaits       []fakeAwait
	awaitErr     error
}

// fakeOneShot is one SetOneShot the fake verbs were asked for.
type fakeOneShot struct {
	ws      ids.WorkspaceID
	asker   *ids.WorkspaceID
	oneShot bool
}

// fakeAwait is one SetCorrespondence the fake verbs were asked for.
type fakeAwait struct {
	ws      ids.WorkspaceID
	asker   *ids.WorkspaceID
	awaited *ids.WorkspaceID
}

func (f *fakeVerbs) Status(context.Context, ids.WorkspaceID) (*agentreplv1.WorkspaceStatus, error) {
	return f.api.status, f.api.statusErr
}

func (f *fakeVerbs) CloseSelf(_ context.Context, ws ids.WorkspaceID) (bool, error) {
	f.api.selfCloses = append(f.api.selfCloses, ws)
	return f.api.closesLater, f.api.closeSelfErr
}

func (f *fakeVerbs) SetOneShot(_ context.Context, ws ids.WorkspaceID, asker *ids.WorkspaceID, oneShot bool) (string, error) {
	f.api.oneShots = append(f.api.oneShots, fakeOneShot{ws: ws, asker: asker, oneShot: oneShot})
	if oneShot {
		return f.api.directive, f.api.oneShotErr
	}
	return "", f.api.oneShotErr
}

func (f *fakeVerbs) SetCorrespondence(_ context.Context, ws ids.WorkspaceID, asker, awaited *ids.WorkspaceID) error {
	f.api.awaits = append(f.api.awaits, fakeAwait{ws: ws, asker: asker, awaited: awaited})
	return f.api.awaitErr
}

func (f *fakeVerbs) Close(_ context.Context, _ ids.WorkspaceID, by wsm.CloseCause) error {
	f.closedBy = append(f.closedBy, by)
	return f.closeErr
}

func (f *fakeVerbs) Forget(context.Context, ids.WorkspaceID) error { return f.forgetErr }

func (f *fakeVerbs) AnswerPermission(context.Context, ids.WorkspaceID, *conversationv1.AgentAnswer) error {
	return f.answerPermissionErr
}

func (f *fakeVerbs) AnswerQuestion(context.Context, ids.WorkspaceID, *conversationv1.AgentAnswer) error {
	return f.answerQuestionErr
}

// fakeServerDraftEdit is one UpdateQuestionDraft call.
type fakeServerDraftEdit struct {
	ask  string
	edit feed.QuestionEdit
}

func (f *fakeVerbs) UpdateQuestionDraft(_ context.Context, _ ids.WorkspaceID, ask string, edit feed.QuestionEdit) error {
	f.draftEdits = append(f.draftEdits, fakeServerDraftEdit{ask: ask, edit: edit})
	return f.draftErr
}

// fakePrompts answers one submission outcome and records the said it was
// handed, so a reply-to-a-past-response test can assert the daemon prepended
// the referenced response before delivery.
type fakePrompts struct {
	prompthandler.Handler
	outcome  prompthandler.Outcome
	err      error
	lastSaid *conversationv1.UserSaid
	// lastDelivery is the delivery the last submission was handed with.
	lastDelivery wsm.Delivery
	// lastOrigin is the origin the last submission was handed with.
	lastOrigin conversationv1.PromptOrigin
	// lastSentBy is the sender the last submission was handed with.
	lastSentBy *ids.WorkspaceID
	// submits counts the submissions.
	submits int
}

func (f *fakePrompts) Submit(_ context.Context, _ ids.WorkspaceID, said *conversationv1.UserSaid, _ string, origin conversationv1.PromptOrigin, sentBy *ids.WorkspaceID, delivery wsm.Delivery, _ *feedid.Ref) (prompthandler.Outcome, error) {
	f.lastSaid = said
	f.lastSentBy = sentBy
	f.lastDelivery = delivery
	f.lastOrigin = origin
	f.submits++
	return f.outcome, f.err
}

// fakeQueue answers the three held-prompt verbs and the edit's steps.
type fakeQueue struct {
	promptqueue.Queue
	releaseErr error
	dropErr    error
	released   []ids.TurnID
	dropped    []ids.TurnID

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
	// foldErr answers a fold; folds records every fold's two turns, in order.
	foldErr error
	folds   [][2]ids.TurnID

	// heldSince is what HeldSince answers; heldSinceErr fails the read.
	// heldSinceCalls records every `since` HeldSince was asked with, in
	// order, so a test can assert the far-future sentinel for a turn the DB
	// never recorded.
	heldSince      []ids.TurnID
	heldSinceErr   error
	heldSinceCalls []time.Time

	// rollBackErr is RollBack's scripted refusal, answered WITHOUT calling
	// perform — mirroring the real queue's ErrHoldsChanged contract, where a
	// queue that changed since planning refuses before anything runs.
	// rollBackCalls records every call's since and drop, in order.
	rollBackErr   error
	rollBackCalls []rollBackCall
}

// rollBackCall is one fakeQueue.RollBack call's since and drop.
type rollBackCall struct {
	Since time.Time
	Drop  []ids.TurnID
}

func (f *fakeQueue) Fold(_ context.Context, _ ids.WorkspaceID, turn, above ids.TurnID) error {
	f.folds = append(f.folds, [2]ids.TurnID{turn, above})
	return f.foldErr
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

func (f *fakeQueue) Drop(_ context.Context, _ ids.WorkspaceID, turn ids.TurnID) error {
	f.dropped = append(f.dropped, turn)
	return f.dropErr
}

// HeldSince records the asked `since` and answers the seeded held turns.
func (f *fakeQueue) HeldSince(_ context.Context, _ ids.WorkspaceID, since time.Time) ([]ids.TurnID, error) {
	f.heldSinceCalls = append(f.heldSinceCalls, since)
	if f.heldSinceErr != nil {
		return nil, f.heldSinceErr
	}
	return f.heldSince, nil
}

// RollBack records the call and, absent a scripted refusal, invokes perform
// exactly as the real queue does when nothing refuses the rollback.
func (f *fakeQueue) RollBack(ctx context.Context, _ ids.WorkspaceID, since time.Time, drop []ids.TurnID, perform func(context.Context) error) error {
	f.rollBackCalls = append(f.rollBackCalls, rollBackCall{Since: since, Drop: drop})
	if f.rollBackErr != nil {
		return f.rollBackErr
	}
	return perform(ctx)
}

// fakeMerge answers the orchestrator's four server-facing verbs.
type fakeMerge struct {
	merge.Orchestrator
	enqueueErr          error
	pauseErr            error
	pauseScope          *merge.RepositoryScope
	answerQueueOfferErr error
	// queueOfferAnswers is every AnswerQueueOffer answer, in order.
	queueOfferAnswers []merge.QueueOfferAnswer
	evictErr          error
	holdErr           error
	releaseErr        error
	// holds and releases count the deploy-hold verbs, and holdScopes every
	// repository they named; release is what a release answers.
	holds, releases int
	holdScopes      []merge.RepositoryScope
	release         merge.DeployRelease
	// rollOuts records every RollOutLanding ledger; rollOutRegistered and
	// rollOutErr are its answer.
	rollOuts          []ids.LeaseID
	rollOutRegistered bool
	rollOutErr        error
	disableRacingErr  error
	enableRacingErr   error
	// disables and enables count the racing-switch verbs.
	disables, enables int
	moveErr           error
	// evicted records every workspace Evict was asked to take off the queue.
	evicted []ids.WorkspaceID
	// moves records every move Move was asked for.
	moves []fakeMove
	// enqueuedBy records who each enqueue said asked for the merge, and
	// requests every request whole.
	enqueuedBy []merge.Requester
	requests   []merge.Request
	// logPaths answers TestLogPath by token; logErr refuses every one.
	logPaths map[string]string
	logErr   error
	// reveals records every RevealMergeWorkspace; revealErr fails it.
	reveals   []ids.WorkspaceID
	revealErr error
	// tabInterrupts records every InterruptTab's tab; tabRunning and tabErr
	// answer it.
	tabInterrupts []feedid.Ref
	tabRunning    bool
	tabErr        error
	// subscriptions records every Subscribe, unsubscriptions every
	// Unsubscribe; subscribeErr and unsubscribeErr answer them.
	subscriptions   []fakeSubscription
	unsubscriptions []fakeSubscription
	subscribeErr    error
	unsubscribeErr  error
	// replaced is what Subscribe answers; statuses and statusErr answer
	// Status, whose scopes are recorded in statusScopes.
	replaced     bool
	statuses     []merge.QueueStatus
	statusErr    error
	statusScopes []*merge.RepositoryScope
}

func (f *fakeMerge) Status(_ context.Context, scope *merge.RepositoryScope) ([]merge.QueueStatus, error) {
	f.statusScopes = append(f.statusScopes, scope)
	return f.statuses, f.statusErr
}

// fakeSubscription is one Subscribe or Unsubscribe the fake was asked for.
type fakeSubscription struct {
	ws     ids.WorkspaceID
	scope  *merge.RepositoryScope
	detail wsm.MergeQueueDetail
}

func (f *fakeMerge) Subscribe(_ context.Context, ws ids.WorkspaceID, scope *merge.RepositoryScope, detail wsm.MergeQueueDetail) (bool, error) {
	f.subscriptions = append(f.subscriptions, fakeSubscription{ws: ws, scope: scope, detail: detail})
	return f.replaced, f.subscribeErr
}

func (f *fakeMerge) Unsubscribe(_ context.Context, ws ids.WorkspaceID, scope *merge.RepositoryScope) error {
	f.unsubscriptions = append(f.unsubscriptions, fakeSubscription{ws: ws, scope: scope})
	return f.unsubscribeErr
}

func (f *fakeMerge) RevealMergeWorkspace(_ context.Context, ws ids.WorkspaceID, _ string) error {
	f.reveals = append(f.reveals, ws)
	return f.revealErr
}

func (f *fakeMerge) InterruptTab(_ context.Context, _ ids.WorkspaceID, tab feedid.Ref) (bool, error) {
	f.tabInterrupts = append(f.tabInterrupts, tab)
	return f.tabRunning, f.tabErr
}

func (f *fakeMerge) Enqueue(_ context.Context, req merge.Request) (merge.Enqueued, error) {
	f.enqueuedBy = append(f.enqueuedBy, req.By)
	f.requests = append(f.requests, req)
	if f.enqueueErr != nil {
		return merge.Enqueued{}, f.enqueueErr
	}
	if req.Detached != nil {
		return merge.Enqueued{MergeBranch: "agent/" + req.Detached.Name}, nil
	}
	return merge.Enqueued{}, nil
}

func (v *fakeVerbs) OpenInEditor(_ context.Context, _ ids.WorkspaceID, path string, _ *uint32) error {
	v.editorOpens = append(v.editorOpens, "file:"+path)
	return nil
}

// OpenFeedLink records the link and answers the seeded unresolved question
// and error.
func (v *fakeVerbs) OpenFeedLink(_ context.Context, _ ids.WorkspaceID, href string, report bool) (*workspace.UnresolvedLink, error) {
	v.editorOpens = append(v.editorOpens, "link:"+href)
	v.feedLinkReport = append(v.feedLinkReport, report)
	return v.feedLinkUnresolved, v.feedLinkErr
}

func (v *fakeVerbs) OpenCommitLogInEditor(_ context.Context, _ ids.WorkspaceID, target *agentreplv1.OpenInEditorCommitLog) error {
	v.editorOpens = append(v.editorOpens, "commit:"+target.GetRepositoryDir()+"@"+target.GetBranch()+"#"+target.GetCommit())
	return nil
}

func (v *fakeVerbs) OpenDaemonFileInEditor(_ context.Context, _ ids.WorkspaceID, path string) error {
	v.editorOpens = append(v.editorOpens, "daemon:"+path)
	return nil
}

func (f *fakeMerge) TestLogPath(_ context.Context, _ ids.WorkspaceID, token string) (string, error) {
	if f.logErr != nil {
		return "", f.logErr
	}
	return f.logPaths[token], nil
}

func (f *fakeMerge) AnswerQueueOffer(_ context.Context, ws ids.WorkspaceID, answer merge.QueueOfferAnswer) error {
	f.queueOfferAnswers = append(f.queueOfferAnswers, answer)
	return f.answerQueueOfferErr
}

func (f *fakeMerge) Evict(_ context.Context, ws ids.WorkspaceID) error {
	f.evicted = append(f.evicted, ws)
	return f.evictErr
}

// fakeMove is one move fakeMerge was asked for.
type fakeMove struct {
	ws   ids.WorkspaceID
	move wsm.MergeMove
}

func (f *fakeMerge) Move(_ context.Context, ws ids.WorkspaceID, move wsm.MergeMove) (bool, error) {
	f.moves = append(f.moves, fakeMove{ws: ws, move: move})
	return f.moveErr == nil, f.moveErr
}

func (f *fakeMerge) HoldDeploys(_ context.Context, scope merge.RepositoryScope) error {
	f.holds++
	f.holdScopes = append(f.holdScopes, scope)
	return f.holdErr
}

func (f *fakeMerge) ReleaseDeploys(_ context.Context, scope merge.RepositoryScope) (merge.DeployRelease, error) {
	f.releases++
	f.holdScopes = append(f.holdScopes, scope)
	if f.releaseErr != nil {
		return merge.DeployRelease{}, f.releaseErr
	}
	return f.release, nil
}

func (f *fakeMerge) RollOutLanding(_ context.Context, ledger ids.LeaseID) (bool, error) {
	f.rollOuts = append(f.rollOuts, ledger)
	return f.rollOutRegistered, f.rollOutErr
}

func (f *fakeMerge) DisableRacing(context.Context) error {
	f.disables++
	return f.disableRacingErr
}

func (f *fakeMerge) EnableRacing(context.Context) error {
	f.enables++
	return f.enableRacingErr
}

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

// fakeMakers answers which workspace spawned a subagent: the workspace
// seeded for its id, subagentmaker.ErrNotFound for any other, or err.
type fakeMakers struct {
	spawners map[string]wsm.Workspace
	err      error
}

func (f *fakeMakers) Find(_ context.Context, _ wsm.RepoID, agentID string) (wsm.Workspace, error) {
	if f.err != nil {
		return wsm.Workspace{}, f.err
	}
	if ws, ok := f.spawners[agentID]; ok {
		return ws, nil
	}
	return wsm.Workspace{}, subagentmaker.ErrNotFound
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
	// daemonErr, when set, fails every daemon report.
	daemonErr error
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
	gates map[ids.WorkspaceID]HostGateKind
}

func (f *fakeSessionFacts) StandingGate(ws ids.WorkspaceID) (HostGateKind, bool) {
	got, ok := f.gates[ws]
	return got, ok
}

func (f *fakeSessionFacts) HostSessionFacts(ws ids.WorkspaceID) (HostFacts, bool) {
	got, ok := f.facts[ws]
	return got, ok
}

func (f *fakeHealth) Daemon(context.Context) (*agentreplv1.DaemonHealthResponse, error) {
	if f.daemonErr != nil {
		return nil, f.daemonErr
	}
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
	// targets is the response row each label targets (ComposerTargets),
	// keyed by label; a missing label targets nothing. targetReplies records
	// the reply each ComposerTargets call was asked for, in order.
	targets       map[string]string
	targetReplies []string
	// neighbors is each label's neighbor (EntryNeighbor), keyed by the
	// label, a NUL, and "next" or "previous".
	neighbors map[string]string
	// labelMoves is what WatchLabelChanges announces (moveLabels).
	labelMoves publish.Events[ids.WorkspaceID]
	// entries is what each prompt row's entry says, keyed by row id, a NUL,
	// and the entry's label: what PromptEntry answers.
	entries map[string]string
	// sent are the sent rows SentPrompt drew, in order.
	sentMu sync.Mutex
	sent   []sentRow
	// hostAsks is each workspace's standing ask HostAsk answers, and
	// askMoves the stream WatchHostAsks hands the server.
	hostAsksMu sync.Mutex
	hostAsks   map[ids.WorkspaceID]*agentreplv1.HostAsk
	askMoves   publish.Events[ids.WorkspaceID]
	page       *frontendv1.FeedPage
	token      *agentreplv1.FeedWatchToken
	openErr    error
	// held is the held section OpenPage answers with.
	held      *frontendv1.FeedHeld
	olderPage *frontendv1.FeedPage
	olderErr  error
	tail      *fakeTail
	tailErr   error
	lastFeed  feedid.Feed
	openCalls int
	// olderKeys are the oldest held rows' keys OlderPage was asked above, in
	// order.
	olderKeys []string
	// finals is the ordered selectable final-response set and prompts the
	// ordered selectable rollback-prompt set, which TurnBoundaries merges for
	// SelectFeedRow's turn-boundary step; markdown is each selectable row's copied
	// markdown, keyed by FeedId value, for the reply-prefix path.
	finals   []*frontendv1.FeedId
	prompts  []*frontendv1.FeedId
	markdown map[string]string
	// unreadable names rows SelectableMarkdown misses even though they are
	// finals or rollback prompts: the resolver defect a selection must refuse.
	unreadable map[string]bool
	// promptRows names selectable rows that are prompts but not rollback
	// prompts (an agent prompt, a user prompt a rollback cannot reach).
	promptRows map[string]bool
	// unselectable names bubbles the reader cannot select (a thinking bubble,
	// an interim response) by row, each with its text: BubbleText answers them
	// and SelectableText misses them, as the resolver does.
	unselectable map[string]string
	// mergeHeads names the merge bubble heads SetMergeFold accepts, and
	// mergeFolds records each fold it was told, keyed by row.
	mergeHeads map[string]bool
	mergeFolds map[string]bool

	// rollbackTarget and rollbackTargetOK are what RollbackTarget answers —
	// one scripted answer suffices, since no rollback test asks it about more
	// than one row at a time. rollbackTargetRows records every row it was
	// asked about, in order, so a test can assert WHICH row a plan targeted.
	rollbackTarget     feed.RollbackTarget
	rollbackTargetOK   bool
	rollbackTargetRows []*frontendv1.FeedId
	// rollbackChoices is what RollbackChoices answers.
	rollbackChoices []feed.RollbackChoice
	// promptFollowed is what PromptFollowed answers; promptFollowedRows
	// records every row it was asked about.
	promptFollowed     bool
	promptFollowedRows []*frontendv1.FeedId
	// liveDetached is what LiveDetachedIn answers; liveDetachedCalls counts
	// how many times it was called, so a test can assert it was never
	// computed when files are kept.
	liveDetached      int
	liveDetachedCalls int
	// rollBackTurnsErr is RollBackTurns' scripted error; rollBackTurnsCalls
	// records every turns slice handed to it, in order.
	rollBackTurnsErr   error
	rollBackTurnsCalls [][]ids.TurnID

	// olderFinals and olderPrompts are what each LoadOlder call loads, in
	// order: the rows it prepends to finals and prompts. loadOlderErr fails
	// every call; loadOlderCalls counts them.
	olderFinals    [][]*frontendv1.FeedId
	olderPrompts   [][]*frontendv1.FeedId
	loadOlderErr   error
	loadOlderCalls int

	// throughPages are the pages LoadThrough hands over, then answers
	// throughErr or the target; throughOldest is the oldest held row's key it
	// was asked to walk above, nil when the reader named none.
	throughPages  []*frontendv1.FeedPage
	throughErr    error
	throughOldest *string
}

// LoadOlder loads the next scripted older page, false when none is left.
func (f *fakeFeed) HostAsk(ws ids.WorkspaceID) *agentreplv1.HostAsk {
	f.hostAsksMu.Lock()
	defer f.hostAsksMu.Unlock()
	return f.hostAsks[ws]
}

func (f *fakeFeed) WatchHostAsks(ctx context.Context) <-chan ids.WorkspaceID {
	return f.askMoves.Subscribe(ctx)
}

// setHostAsk stands ASK (nil: none) for WS and announces the move, as the
// feed resolver does on a card's draw.
func (f *fakeFeed) setHostAsk(ws ids.WorkspaceID, ask *agentreplv1.HostAsk) {
	f.hostAsksMu.Lock()
	if f.hostAsks == nil {
		f.hostAsks = map[ids.WorkspaceID]*agentreplv1.HostAsk{}
	}
	f.hostAsks[ws] = ask
	f.hostAsksMu.Unlock()
	f.askMoves.Emit(ws)
}

func (f *fakeFeed) LoadOlder(context.Context, ids.WorkspaceID) (bool, error) {
	f.loadOlderCalls++
	if f.loadOlderErr != nil {
		return false, f.loadOlderErr
	}
	if len(f.olderFinals) == 0 && len(f.olderPrompts) == 0 {
		return false, nil
	}
	if len(f.olderFinals) > 0 {
		f.finals = append(append([]*frontendv1.FeedId{}, f.olderFinals[0]...), f.finals...)
		f.olderFinals = f.olderFinals[1:]
	}
	if len(f.olderPrompts) > 0 {
		f.prompts = append(append([]*frontendv1.FeedId{}, f.olderPrompts[0]...), f.prompts...)
		f.olderPrompts = f.olderPrompts[1:]
	}
	return true, nil
}

// LoadThrough hands over the scripted pages, then answers the scripted outcome.
func (f *fakeFeed) LoadThrough(_ context.Context, _ ids.WorkspaceID, oldestHeld *string, target *frontendv1.FeedId, emit func(*frontendv1.FeedPage) error) (*frontendv1.FeedId, error) {
	f.throughOldest = oldestHeld
	for _, page := range f.throughPages {
		if err := emit(page); err != nil {
			return nil, err
		}
	}
	if f.throughErr != nil {
		return nil, f.throughErr
	}
	return target, nil
}

func (f *fakeFeed) FinalResponses(ids.WorkspaceID) []*frontendv1.FeedId {
	return f.finals
}

func (f *fakeFeed) RollbackPrompts(ids.WorkspaceID) []*frontendv1.FeedId {
	return f.prompts
}

// TurnBoundaries answers prompts and finals as one walk. The fake's feed order
// is the rows' FeedId value order, so a test spells the interleaving it wants
// in its row names.
func (f *fakeFeed) TurnBoundaries(ids.WorkspaceID) []feed.TurnBoundary {
	out := make([]feed.TurnBoundary, 0, len(f.prompts)+len(f.finals))
	for _, row := range f.prompts {
		out = append(out, feed.TurnBoundary{Row: row, Kind: feed.TurnBoundaryPrompt})
	}
	for _, row := range f.finals {
		out = append(out, feed.TurnBoundary{Row: row, Kind: feed.TurnBoundaryResponse})
	}
	slices.SortStableFunc(out, func(a, b feed.TurnBoundary) int {
		return strings.Compare(a.Row.GetValue(), b.Row.GetValue())
	})
	return out
}

func (f *fakeFeed) SetMergeFold(_ ids.WorkspaceID, id *frontendv1.FeedId, folded bool) bool {
	if !f.mergeHeads[id.GetValue()] {
		return false
	}
	if f.mergeFolds == nil {
		f.mergeFolds = map[string]bool{}
	}
	f.mergeFolds[id.GetValue()] = folded
	return true
}

// SelectableText answers a row's scripted text. Like the resolver, it
// treats every final response and rollback prompt as selectable (they are
// landed root-feed rows by construction), answering "text of <row>" for one
// with no scripted text; unreadable breaks that guarantee for a row, so a test
// can drive the defect path. An unselectable row is never selectable.
// PromptEntry answers the entry text a test filed under the row and label.
func (f *fakeFeed) ComposerTargets(_ ids.WorkspaceID, reply *frontendv1.FeedId, labels []string) []*frontendv1.FeedId {
	f.sentMu.Lock()
	f.targetReplies = append(f.targetReplies, reply.GetValue())
	f.sentMu.Unlock()
	out := make([]*frontendv1.FeedId, len(labels))
	for i, label := range labels {
		if row, ok := f.targets[label]; ok {
			out[i] = &frontendv1.FeedId{Value: row}
		}
	}
	return out
}

func (f *fakeFeed) EntryNeighbor(_ ids.WorkspaceID, _ *frontendv1.FeedId, label string, next bool) (string, bool) {
	way := "previous"
	if next {
		way = "next"
	}
	neighbor, ok := f.neighbors[label+"\x00"+way]
	return neighbor, ok
}

func (f *fakeFeed) WatchLabelChanges(ctx context.Context) <-chan ids.WorkspaceID {
	return f.labelMoves.Subscribe(ctx)
}

func (f *fakeFeed) PromptEntry(_ ids.WorkspaceID, id *frontendv1.FeedId, label string) (string, bool) {
	text, ok := f.entries[id.GetValue()+"\x00"+label]
	return text, ok
}

func (f *fakeFeed) SelectableText(_ ids.WorkspaceID, id *frontendv1.FeedId) (feed.RowText, bool) {
	prompt := indexOfFeedID(f.prompts, id) >= 0 || f.promptRows[id.GetValue()]
	if f.unreadable[id.GetValue()] {
		return feed.RowText{}, false
	}
	if md, ok := f.markdown[id.GetValue()]; ok {
		return feed.RowText{Markdown: md, Prompt: prompt}, true
	}
	if indexOfFeedID(f.finals, id) >= 0 || prompt {
		return feed.RowText{Markdown: "text of " + id.GetValue(), Prompt: prompt}, true
	}
	return feed.RowText{}, false
}

// BubbleText answers an unselectable row's text, and otherwise what
// SelectableText does: every selectable row is a bubble with text.
func (f *fakeFeed) BubbleText(ws ids.WorkspaceID, id *frontendv1.FeedId) (feed.RowText, bool) {
	if md, ok := f.unselectable[id.GetValue()]; ok {
		return feed.RowText{Markdown: md}, true
	}
	return f.SelectableText(ws, id)
}

// RollbackTarget records the row it was asked about and answers the seeded
// target and ok.
func (f *fakeFeed) RollbackTarget(_ ids.WorkspaceID, row *frontendv1.FeedId) (feed.RollbackTarget, bool) {
	f.rollbackTargetRows = append(f.rollbackTargetRows, row)
	return f.rollbackTarget, f.rollbackTargetOK
}

// RollbackChoices answers the seeded choices.
func (f *fakeFeed) RollbackChoices(ids.WorkspaceID) []feed.RollbackChoice {
	return f.rollbackChoices
}

// PromptFollowed records the row it was asked about and answers the seeded
// verdict.
func (f *fakeFeed) PromptFollowed(_ ids.WorkspaceID, row *frontendv1.FeedId) bool {
	f.promptFollowedRows = append(f.promptFollowedRows, row)
	return f.promptFollowed
}

// LiveDetachedIn records that it was called and answers the seeded count.
func (f *fakeFeed) LiveDetachedIn(ids.WorkspaceID, []ids.TurnID) int {
	f.liveDetachedCalls++
	return f.liveDetached
}

// RollBackTurns records the turns it was handed and answers the seeded error.
func (f *fakeFeed) RollBackTurns(_ ids.WorkspaceID, turns []ids.TurnID) error {
	f.rollBackTurnsCalls = append(f.rollBackTurnsCalls, turns)
	return f.rollBackTurnsErr
}

func (f *fakeFeed) OpenPage(_ context.Context, _ ids.WorkspaceID, target feedid.Feed) (*frontendv1.FeedPage, *agentreplv1.FeedWatchToken, *frontendv1.FeedHeld, error) {
	f.openCalls++
	f.lastFeed = target
	return f.page, f.token, f.held, f.openErr
}

func (f *fakeFeed) OlderPage(_ context.Context, _ ids.WorkspaceID, target feedid.Feed, oldestHeld string) (*frontendv1.FeedPage, error) {
	f.lastFeed = target
	f.olderKeys = append(f.olderKeys, oldestHeld)
	return f.olderPage, f.olderErr
}

func (f *fakeFeed) Tail(context.Context, ids.WorkspaceID, feedid.Feed, *agentreplv1.FeedWatchToken) (feed.Tail, error) {
	if f.tailErr != nil {
		return nil, f.tailErr
	}
	return f.tail, nil
}

// fakeTail hands the handler a publication channel the test drives.
type fakeTail struct {
	pubs  chan feed.Publication
	token *agentreplv1.FeedWatchToken
}

func (t *fakeTail) Publications(context.Context) <-chan feed.Publication { return t.pubs }

func (t *fakeTail) Token() *agentreplv1.FeedWatchToken { return t.token }

// fakeFooter, fakeTopbar and fakeSidebar each own one topic per
// workspace, which is exactly the publisher seam the handlers subscribe to.
type fakeFooter struct {
	footer.Resolver
	topic publish.Topic[*frontendv1.FooterView]

	// faults are the faults the server opened on the footer.
	faultsMu sync.Mutex
	faults   []footer.Fault

	edges participantRecorder
}

// OpenFault records a fault the server opened.
func (f *fakeFooter) OpenFault(_ ids.WorkspaceID, fault footer.Fault) {
	f.faultsMu.Lock()
	defer f.faultsMu.Unlock()
	f.faults = append(f.faults, fault)
}

// openedFaults answers the faults opened so far.
func (f *fakeFooter) openedFaults() []footer.Fault {
	f.faultsMu.Lock()
	defer f.faultsMu.Unlock()
	return append([]footer.Fault(nil), f.faults...)
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

// AwaitEdge blocks for the next participant edge the topbar was told.
func (f *fakeTopbar) AwaitEdge(t *testing.T) participantEdge { return f.edges.await(t) }

type fakeSidebar struct {
	sidebar.Resolver
	topic publish.Topic[*frontendv1.WorkspaceRoster]
	// revision is what Revision answers.
	revision uint64
}

func (f *fakeSidebar) Topic() *publish.Topic[*frontendv1.WorkspaceRoster] { return &f.topic }

func (f *fakeSidebar) Revision() uint64 { return f.revision }

// harness is one running surface over real Connect handlers.
type harness struct {
	t *testing.T
	// Deps is the COMPLETE dependency set the surface was built from, so a
	// test of one missing dependency strips exactly that one (withoutDep).
	Deps      Deps
	Server    Server
	HTTP      *httptest.Server
	Client    agentreplv1connect.AgentReplClient
	DB        *fakeDB
	Ownership *fakeOwnership
	Verbs     *fakeVerbs
	Creates   *creategate.Gate
	Retarget  *fakeRetarget
	Prompts   *fakePrompts
	Queue     *fakeQueue
	Merge     *fakeMerge
	Makers    *fakeMakers
	Drain     *fakeDrain
	Rollout   *fakeRollout
	Deployer  *fakeDeployer
	Health    *fakeHealth
	Facts     *fakeSessionFacts
	Login     *fakeLogin
	Feed      *fakeFeed
	Images    *fakeImageSources
	Footer    *fakeFooter
	Topbar    *fakeTopbar
	Sidebar   *fakeSidebar
	// LoudFaults is the standing loud faults every Emacs stream is told.
	LoudFaults publish.Topic[*agentreplv1.DaemonFaultsStanding]
	// Focus is Emacs's desktop focus, attached by an Emacs WatchDaemon stream.
	Focus *desktopnotify.Focus
	// Emacs records every Emacs pid a stream attached and every release.
	Emacs *fakeEmacsMeasure
	// Banners is the desktop banners a ReportEditorFocus gain claims.
	Banners *fakeBanners
	// PersistentWifi is the persistent-wifi controller the rpc delegates to.
	PersistentWifi *fakePersistentWifi
	// HiddenKeepalives is the cache keeper SendHiddenKeepalives delegates to.
	HiddenKeepalives *fakeHiddenKeepalives
	// NewsDigest is the news digest the two rpcs delegate to.
	NewsDigest *fakeNewsDigest
	// ClassifierPrompt is the routing brief's updater the rpc delegates to.
	ClassifierPrompt *fakeClassifierPrompt
	EditorInstances  *fakeEditorInstances
	// Opens is the REAL OpenTabs coordinator over the fakes in OpenWorld.
	Opens *opentabs.Coordinator
	// StallBeat and Pages record what the server hands its two web-process
	// detectors.
	StallBeat   *fakeStallBeat
	Pages       *fakeSupersededPages
	OpenWorld   *fakeOpenWorld
	ChessBoards *fakeChessSquares
	// Latencies records what ReportLatencies accepts.
	Latencies *fakeLatencies
	RPCs      *fakeRPCs
	// Reconnects times the streams a test expects back.
	Reconnects *restarttiming.Reconnects
	// ReconnectRecords keeps what Reconnects timed.
	ReconnectRecords *restarttimingtest.Recorder
	// SlashCommands is the real catalog table, which a test feeds directly.
	SlashCommands *commandcatalog.Catalogs
	Surfaces      *fakeSurfaces
	Tests         *fakeTests
	WebappDist    string
}

// fakeBanners answers one scripted claim and records every claim and forget.
type fakeBanners struct {
	mu      sync.Mutex
	claim   desktopnotify.Claim
	claims  int
	forgets []string
}

func (b *fakeBanners) Claim() desktopnotify.Claim {
	b.mu.Lock()
	defer b.mu.Unlock()
	b.claims++
	return b.claim
}

func (b *fakeBanners) claimCount() int {
	b.mu.Lock()
	defer b.mu.Unlock()
	return b.claims
}

func (b *fakeBanners) Forget(reason string) {
	b.mu.Lock()
	defer b.mu.Unlock()
	b.forgets = append(b.forgets, reason)
}

func (b *fakeBanners) forgotten() []string {
	b.mu.Lock()
	defer b.mu.Unlock()
	return append([]string(nil), b.forgets...)
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
		Creates:    creategate.New(),
		Retarget:   &fakeRetarget{},
		Prompts:    &fakePrompts{},
		Queue:      &fakeQueue{},
		Merge:      &fakeMerge{},
		Makers:     &fakeMakers{},
		Drain:      &fakeDrain{},
		Rollout:    &fakeRollout{},
		Deployer:   &fakeDeployer{},
		Health:     &fakeHealth{},
		Facts:      &fakeSessionFacts{facts: map[ids.WorkspaceID]HostFacts{}},
		Login:      &fakeLogin{},
		Feed:       &fakeFeed{},
		Images:     &fakeImageSources{},
		Footer:     &fakeFooter{},
		Topbar:     &fakeTopbar{},
		Sidebar:    &fakeSidebar{},
		Surfaces:   &fakeSurfaces{},
		Focus:      desktopnotify.NewFocus(dlog.NewTestLogger()),
		Emacs:      &fakeEmacsMeasure{},
		Banners:    &fakeBanners{},
		WebappDist: dist,

		PersistentWifi:   &fakePersistentWifi{},
		HiddenKeepalives: &fakeHiddenKeepalives{},
		Latencies:        &fakeLatencies{enabled: true},
		RPCs:             &fakeRPCs{},
		ReconnectRecords: &restarttimingtest.Recorder{},
		NewsDigest:       &fakeNewsDigest{},
		ClassifierPrompt: &fakeClassifierPrompt{},
		EditorInstances:  &fakeEditorInstances{},
		OpenWorld:        newFakeOpenWorld(),
		ChessBoards:      &fakeChessSquares{},
		SlashCommands:    commandcatalog.New(),
		StallBeat:        &fakeStallBeat{},
		Pages:            &fakeSupersededPages{},
	}

	opens, err := opentabs.New(h.OpenWorld.deps())
	if err != nil {
		t.Fatalf("opentabs.New: %v", err)
	}
	h.Opens = opens

	h.Tests = &fakeTests{}
	deps := Deps{
		Tests:             h.Tests,
		SubagentMakers:    h.Makers,
		DB:                h.DB,
		Prompts:           h.Prompts,
		Queue:             h.Queue,
		Verbs:             h.Verbs,
		Retarget:          h.Retarget,
		Merge:             h.Merge,
		Drain:             h.Drain,
		Rollout:           h.Rollout,
		Deploy:            h.Deployer,
		Health:            h.Health,
		SessionFacts:      h.Facts,
		SlashCommands:     h.SlashCommands,
		Login:             h.Login,
		Ownership:         h.Ownership,
		SuccessorAddress:  func() string { return "127.0.0.1:9999" },
		Creates:           h.Creates,
		Feed:              h.Feed,
		Images:            h.Images,
		Footer:            h.Footer,
		Topbar:            h.Topbar,
		Sidebar:           h.Sidebar,
		LoudFaults:        &h.LoudFaults,
		Focus:             h.Focus,
		Emacs:             h.Emacs,
		Banners:           h.Banners,
		PersistentWifi:    h.PersistentWifi,
		HiddenKeepalives:  h.HiddenKeepalives,
		NewsDigest:        h.NewsDigest,
		ClassifierPrompt:  h.ClassifierPrompt,
		EditorInstances:   h.EditorInstances,
		Opens:             h.Opens,
		StallBeat:         h.StallBeat,
		Pages:             h.Pages,
		WebappDist:        dist,
		ImageOrigin:       http.NotFoundHandler(),
		ChessBoards:       h.ChessBoards,
		Latencies:         h.Latencies,
		RPCs:              h.RPCs,
		Reconnects:        h.reconnects(),
		ChessWidgetBundle: http.NotFoundHandler(),
		Log:               h.Surfaces,
	}
	for _, apply := range opts {
		apply(&deps)
	}

	h.Deps = deps
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
	// mu guards records: a stream handler records on its own goroutine.
	mu      sync.Mutex
	records []logRecord
}

func (l *recordingLogger) Debug(op, msg string, c dlog.Context) { l.record("DEBUG", op, msg, c) }
func (l *recordingLogger) Info(op, msg string, c dlog.Context)  { l.record("INFO", op, msg, c) }
func (l *recordingLogger) Warn(op, msg string, c dlog.Context)  { l.record("WARN", op, msg, c) }
func (l *recordingLogger) Error(op, msg string, c dlog.Context) { l.record("ERROR", op, msg, c) }
func (l *recordingLogger) With(dlog.Context) dlog.Logger        { return l }

func (l *recordingLogger) record(level, op, msg string, c dlog.Context) {
	l.mu.Lock()
	defer l.mu.Unlock()
	l.records = append(l.records, logRecord{Level: level, Operation: op, Message: msg, Context: c})
}

// at answers the records captured at one level.
func (l *recordingLogger) at(level string) []logRecord {
	l.mu.Lock()
	defer l.mu.Unlock()
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
	return receivePush(t, stream, "an event", func(r *agentreplv1.WatchWebWorkspaceResponse) *agentreplv1.WatchWebWorkspaceResponse {
		if r.GetSessionIdentity() != nil {
			return nil
		}
		return r
	})
}

func receiveHostEvent(
	t *testing.T,
	stream *connect.ServerStreamForClient[agentreplv1.WatchHostWorkspaceResponse],
) *agentreplv1.WatchHostWorkspaceResponse {
	t.Helper()
	return receivePush(t, stream, "an event", func(r *agentreplv1.WatchHostWorkspaceResponse) *agentreplv1.WatchHostWorkspaceResponse {
		if r.GetHost() != nil {
			return nil
		}
		return r
	})
}

// repositoryFold is one FoldRepository the verb received.
type repositoryFold struct {
	repo   ids.RepoID
	folded bool
}

func (v *fakeVerbs) Move(_ context.Context, ws, target ids.WorkspaceID) error {
	v.moves = append(v.moves, [2]ids.WorkspaceID{ws, target})
	return v.moveErr
}

func (v *fakeVerbs) Swap(_ context.Context, ws, target ids.WorkspaceID) error {
	v.swaps = append(v.swaps, [2]ids.WorkspaceID{ws, target})
	return v.swapErr
}

// arrange records one arrangement verb call and answers arrangeErr.
func (v *fakeVerbs) arrange(call string) error {
	v.arrangements = append(v.arrangements, call)
	return v.arrangeErr
}

func (v *fakeVerbs) CreateSubsection(_ context.Context, repo ids.RepoID, name string) (wsm.Subsection, error) {
	return wsm.Subsection{ID: "sub-new", Repo: repo, Name: name, Place: 1}, v.arrange("CreateSubsection " + string(repo) + " " + name)
}

func (v *fakeVerbs) RenameSubsection(_ context.Context, id ids.SubsectionID, name string) error {
	return v.arrange("RenameSubsection " + string(id) + " " + name)
}

func (v *fakeVerbs) DeleteSubsection(_ context.Context, id ids.SubsectionID) error {
	return v.arrange("DeleteSubsection " + string(id))
}

func (v *fakeVerbs) MoveSubsection(_ context.Context, id ids.SubsectionID, to wsm.Anchor) error {
	return v.arrange("MoveSubsection " + string(id) + " " + wsm.AnchorField(to))
}

func (v *fakeVerbs) MoveRepository(_ context.Context, id ids.RepoID, before *ids.RepoID) error {
	return v.arrange("MoveRepository " + string(id) + " " + ids.Optional(before))
}

func (v *fakeVerbs) PlaceWorkspaces(_ context.Context, ws []ids.WorkspaceID, into *ids.SubsectionID, at wsm.Anchor) error {
	return v.arrange(fmt.Sprintf("PlaceWorkspaces %v %s %s", ws, ids.Optional(into), wsm.AnchorField(at)))
}

func (v *fakeVerbs) SidebarArrangement(context.Context) ([]sidebar.ArrangedRepository, error) {
	return v.arrangement, v.arrangementErr
}

func (v *fakeVerbs) FoldRepository(_ context.Context, repo ids.RepoID, folded bool) error {
	v.folds = append(v.folds, repositoryFold{repo: repo, folded: folded})
	return v.foldErr
}

// sectionFold is one FoldMergedSection (merged), FoldHiddenSection (hidden)
// or FoldOpenSection (open) the verb received.
type sectionFold struct {
	merged bool
	hidden bool
	open   bool
	folded bool
}

func (v *fakeVerbs) FoldMergedSection(_ context.Context, folded bool) error {
	v.sectionFolds = append(v.sectionFolds, sectionFold{merged: true, folded: folded})
	return v.sectionFoldErr
}

func (v *fakeVerbs) FoldHiddenSection(_ context.Context, folded bool) error {
	v.sectionFolds = append(v.sectionFolds, sectionFold{hidden: true, folded: folded})
	return v.sectionFoldErr
}

func (v *fakeVerbs) FoldOpenSection(_ context.Context, folded bool) error {
	v.sectionFolds = append(v.sectionFolds, sectionFold{open: true, folded: folded})
	return v.sectionFoldErr
}

// fakeHiddenKeepalives counts every call and answers a scripted response.
type fakeHiddenKeepalives struct {
	calls    int
	response *agentreplv1.SendHiddenKeepalivesResponse
}

func (f *fakeHiddenKeepalives) SendHiddenKeepalives(context.Context) *agentreplv1.SendHiddenKeepalivesResponse {
	f.calls++
	return f.response
}

// fakePersistentWifi records every action and answers a scripted response.
type fakePersistentWifi struct {
	mu       sync.Mutex
	topic    publish.Topic[*agentreplv1.PersistentWifiState]
	requests []*agentreplv1.UpdatePersistentWifiModeRequest
	response *agentreplv1.UpdatePersistentWifiModeResponse
}

func (f *fakePersistentWifi) Update(_ context.Context, req *agentreplv1.UpdatePersistentWifiModeRequest) *agentreplv1.UpdatePersistentWifiModeResponse {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.requests = append(f.requests, req)
	return f.response
}

func (f *fakePersistentWifi) Topic() *publish.Topic[*agentreplv1.PersistentWifiState] {
	return &f.topic
}

// fakeNewsDigest records every dismiss and refresh and answers scripted
// responses or failures.
type fakeNewsDigest struct {
	mu           sync.Mutex
	topic        publish.Topic[*agentreplv1.NewsDigestStanding]
	dismissals   []*agentreplv1.DismissNewsDigestRequest
	refreshes    int
	dismiss      *agentreplv1.DismissNewsDigestResponse
	dismissErr   error
	refresh      *agentreplv1.RefreshNewsDigestResponse
	refreshErr   error
	redisplays   int
	redisplayErr error
}

func (f *fakeNewsDigest) Dismiss(_ context.Context, req *agentreplv1.DismissNewsDigestRequest) (*agentreplv1.DismissNewsDigestResponse, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.dismissals = append(f.dismissals, req)
	return f.dismiss, f.dismissErr
}

func (f *fakeNewsDigest) Refresh(context.Context) (*agentreplv1.RefreshNewsDigestResponse, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.refreshes++
	return f.refresh, f.refreshErr
}

func (f *fakeNewsDigest) Redisplay(context.Context) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.redisplays++
	return f.redisplayErr
}

// fakeClassifierPrompt records every update and answers a scripted result or
// failure.
type fakeClassifierPrompt struct {
	mu      sync.Mutex
	updates []classifierupdate.Request
	result  classifierupdate.Result
	err     error
}

func (f *fakeClassifierPrompt) Update(_ context.Context, req classifierupdate.Request) (classifierupdate.Result, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.updates = append(f.updates, req)
	return f.result, f.err
}

// fakeOpenWorld is the OpenTabs coordinator's collaborators: which
// workspaces are live, the bring-ups it asked for, and the roster's opening
// edges.
type fakeOpenWorld struct {
	mu       sync.Mutex
	live     map[ids.WorkspaceID]bool
	bringUps chan []ids.WorkspaceID
	opening  []string
}

func newFakeOpenWorld() *fakeOpenWorld {
	return &fakeOpenWorld{live: map[ids.WorkspaceID]bool{}, bringUps: make(chan []ids.WorkspaceID, 8)}
}

func (f *fakeOpenWorld) deps() opentabs.Deps {
	return opentabs.Deps{
		Live: func(ws ids.WorkspaceID) bool {
			f.mu.Lock()
			defer f.mu.Unlock()
			return f.live[ws]
		},
		BringUp: func(pending []ids.WorkspaceID, _ func(ids.WorkspaceID, error)) { f.bringUps <- pending },
		Opening: func(ws ids.WorkspaceID, opening bool) {
			f.mu.Lock()
			defer f.mu.Unlock()
			f.opening = append(f.opening, fmt.Sprintf("%s:%v", ws, opening))
		},
		// The conversation reads at once, nothing is owed, and the roster
		// answers a bare row naming the workspace: the server's own tests are
		// about the stream, not the open's work (internal/opentabs covers it).
		ReadConversation: func(context.Context, ids.WorkspaceID) error { return nil },
		Owed:             func(context.Context, ids.WorkspaceID) (bool, error) { return false, nil },
		Row: func(ws ids.WorkspaceID) (*frontendv1.RosterRow, uint64, bool) {
			return &frontendv1.RosterRow{Workspace: &frontendv1.RosterRowWorkspace{
				Workspace: &workspacev1.WorkspaceRef{Id: string(ws)}}}, 1, true
		},
		Now: time.Now,
		Log: dlog.NewTestLogger(),
	}
}

func (f *fakeOpenWorld) setLive(ws ids.WorkspaceID) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.live[ws] = true
}

// fakeEditorInstances answers a scripted verdict on every Emacs instance and
// records what it was told.
type fakeEditorInstances struct {
	mu    sync.Mutex
	isNew bool
	err   error
	seen  []string
}

func (f *fakeEditorInstances) Connected(_ context.Context, instance string) (bool, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.seen = append(f.seen, instance)
	return f.isNew, f.err
}

func (f *fakeNewsDigest) Topic() *publish.Topic[*agentreplv1.NewsDigestStanding] {
	return &f.topic
}

// withoutDep answers the harness's complete Deps with ONE dependency stripped,
// so New's refusal of that dependency is the only refusal it can make,
// whatever order New checks its dependencies in.
func (h *harness) withoutDep(strip func(*Deps)) Deps {
	deps := h.Deps
	strip(&deps)
	return deps
}

// fakeStallBeat records every stall the server observes.
type fakeStallBeat struct {
	mu       sync.Mutex
	observed []observedStall
}

type observedStall struct {
	at        time.Time
	blockedMS int64
}

func (f *fakeStallBeat) Observe(at time.Time, blockedMS int64) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.observed = append(f.observed, observedStall{at: at, blockedMS: blockedMS})
}

func (f *fakeStallBeat) stalls() []observedStall {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]observedStall(nil), f.observed...)
}

// fakeSupersededPages records every page the server reports serving a workspace.
type fakeSupersededPages struct {
	mu     sync.Mutex
	served []servedPage
}

type servedPage struct {
	page string
	ws   ids.WorkspaceID
}

func (f *fakeSupersededPages) Served(page string, ws ids.WorkspaceID) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.served = append(f.served, servedPage{page: page, ws: ws})
}

func (f *fakeSupersededPages) pages() []servedPage {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]servedPage(nil), f.served...)
}

// observedLatency is one latency ReportLatencies recorded.
type observedLatency struct {
	workspace ids.WorkspaceID
	kind      string
	seconds   float64
}

// fakeLatencies is a metrics surface that records what it is given.
type fakeLatencies struct {
	mu       sync.Mutex
	enabled  bool
	observed []observedLatency
	restarts []restarttiming.Observation
	editor   []observedLatency
}

func (f *fakeLatencies) Enabled() bool { return f.enabled }

func (f *fakeLatencies) ObserveEditorLatency(kind runtimemetrics.LatencyKind, seconds float64) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.editor = append(f.editor, observedLatency{kind: kind.Name, seconds: seconds})
}

// editorLatencies answers every editor-wide latency recorded.
func (f *fakeLatencies) editorLatencies() []observedLatency {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]observedLatency(nil), f.editor...)
}

func (f *fakeLatencies) ObserveRestart(o restarttiming.Observation) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.restarts = append(f.restarts, o)
}

func (f *fakeLatencies) ObserveLatency(ws ids.WorkspaceID, kind runtimemetrics.LatencyKind, seconds float64) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.observed = append(f.observed, observedLatency{workspace: ws, kind: kind.Name, seconds: seconds})
}

// fakeRPCs records every rpc timing, as rpc:outcome.
type fakeRPCs struct {
	mu       sync.Mutex
	observed []string
}

func (f *fakeRPCs) ObserveRPC(rpc, outcome string, took time.Duration) {
	if took < 0 {
		panic("a negative rpc duration")
	}
	f.mu.Lock()
	defer f.mu.Unlock()
	f.observed = append(f.observed, rpc+":"+outcome)
}

func (f *fakeRPCs) all() []string {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]string(nil), f.observed...)
}

func (f *fakeLatencies) all() []observedLatency {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]observedLatency(nil), f.observed...)
}

// startups answers every restart recorded, as phase/cause/seconds.
func (f *fakeLatencies) startups() []string {
	f.mu.Lock()
	defer f.mu.Unlock()
	var out []string
	for _, o := range f.restarts {
		out = append(out, fmt.Sprintf("%s:%s/%s/%v", o.Restart(), o.Phase(), o.Cause(), o.Seconds()))
	}
	return out
}

func (f *fakeDB) SubscribeWorkspace(_ context.Context, sub wsm.WorkspaceSubscription) (bool, error) {
	if f.subscribeErr != nil {
		return false, f.subscribeErr
	}
	f.subscribed = append(f.subscribed, sub)
	return f.replacesSub, nil
}

func (f *fakeDB) UnsubscribeWorkspace(_ context.Context, subscriber, followed ids.WorkspaceID) (bool, error) {
	f.unsubscribed = append(f.unsubscribed, [2]ids.WorkspaceID{subscriber, followed})
	return f.endsSub, f.unsubscribeErr
}

// FoldSubsection records the subsection fold as an arrangement call and
// answers arrangeErr.
func (v *fakeVerbs) FoldSubsection(_ context.Context, sub ids.SubsectionID, folded bool) error {
	return v.arrange(fmt.Sprintf("FoldSubsection %s %v", sub, folded))
}

// reconnects answers the harness's reconnect tracker, building it on first
// use over ReconnectRecords and the wall clock.
func (h *harness) reconnects() *restarttiming.Reconnects {
	if h.Reconnects == nil {
		h.Reconnects = restarttiming.NewReconnects(h.ReconnectRecords, time.Now)
	}
	return h.Reconnects
}

// sentRow is one SentPrompt the server asked the feed for.
type sentRow struct {
	sender    ids.WorkspaceID
	recipient string
	key       string
	fate      feed.SentFate
}

func (f *fakeFeed) SentPrompt(sender ids.WorkspaceID, recipient, key string, _ *conversationv1.UserSaid, fate feed.SentFate) {
	f.sentMu.Lock()
	defer f.sentMu.Unlock()
	f.sent = append(f.sent, sentRow{sender: sender, recipient: recipient, key: key, fate: fate})
}

// sentRows answers the sent rows drawn so far.
func (f *fakeFeed) sentRows() []sentRow {
	f.sentMu.Lock()
	defer f.sentMu.Unlock()
	return append([]sentRow(nil), f.sent...)
}

// fakeEmacsMeasure records the Emacs pids streams attached, and their releases,
// in order, as "attach:<pid>" and "release:<pid>".
type fakeEmacsMeasure struct {
	mu     sync.Mutex
	events []string
	// released is signalled once per release, so a test awaits a stream's end.
	released chan struct{}
}

func (f *fakeEmacsMeasure) AttachEmacs(pid int) func() {
	f.mu.Lock()
	f.events = append(f.events, fmt.Sprintf("attach:%d", pid))
	f.mu.Unlock()
	return func() {
		f.mu.Lock()
		f.events = append(f.events, fmt.Sprintf("release:%d", pid))
		ch := f.released
		f.mu.Unlock()
		if ch != nil {
			ch <- struct{}{}
		}
	}
}

func (f *fakeEmacsMeasure) all() []string {
	f.mu.Lock()
	defer f.mu.Unlock()
	return append([]string(nil), f.events...)
}

// fakeImageSources is feed.ImageSources answering what a test scripts: a
// composer image resolves to `resolved` unless `composerErr` is set, and a
// drawn image is accepted unless `drawnErr` is set.
type fakeImageSources struct {
	resolved    *frontendv1.FeedImageBlock
	composerErr error
	drawnErr    error
}

func (f *fakeImageSources) Composer(*conversationv1.ImageBlock) (*frontendv1.FeedImageBlock, error) {
	if f.composerErr != nil {
		return nil, f.composerErr
	}
	return f.resolved, nil
}

func (f *fakeImageSources) Drawn(*frontendv1.FeedImageBlock) error {
	return f.drawnErr
}

func (f *fakeQueue) Supersede(context.Context, ids.WorkspaceID, ids.TurnID) (bool, error) {
	return false, nil
}
