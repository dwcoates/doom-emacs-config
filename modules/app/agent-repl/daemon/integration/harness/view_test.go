package harness

import (
	"context"
	"encoding/json"
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"
)

// viewPID is the pid every record of these tests' fake daemon carries.
const viewPID = 4242

// newOwnerForViews is a bare owning Daemon over a state root whose run log
// the test writes, carrying viewPID as its process.
func newOwnerForViews(t *testing.T, owner testing.TB) *Daemon {
	t.Helper()
	state := t.TempDir()
	if err := os.MkdirAll(filepath.Join(state, "logs"), 0o755); err != nil {
		t.Fatalf("mkdir logs: %v", err)
	}
	process, err := os.FindProcess(viewPID)
	if err != nil {
		t.Fatalf("FindProcess: %v", err)
	}
	return &Daemon{
		t: owner, ctx: context.Background(), StateDir: state, waitBound: DefaultTimeout, Git: World(owner),
		expected: map[string]bool{}, required: map[string]bool{},
		daemonProc: newDaemonProc(&exec.Cmd{Process: process}),
	}
}

// appendRecords writes records into the owner's run log as the daemon would.
func appendRecords(t *testing.T, d *Daemon, records ...LogRecord) {
	t.Helper()
	f, err := os.OpenFile(d.RunLogPath(), os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0o644)
	if err != nil {
		t.Fatalf("open run log: %v", err)
	}
	defer f.Close()
	for _, r := range records {
		r.PID = viewPID
		if r.Timestamp == "" {
			r.Timestamp = time.Now().UTC().Format(time.RFC3339Nano)
		}
		line, err := json.Marshal(r)
		if err != nil {
			t.Fatalf("marshal record: %v", err)
		}
		if _, err := f.Write(append(line, '\n')); err != nil {
			t.Fatalf("write record: %v", err)
		}
	}
}

func warnFor(dir, op string) LogRecord {
	return LogRecord{Level: "warn", Operation: op, Message: "m", WorkspaceDir: dir}
}

func TestAViewSweepsOnlyTheWorkspacesItClaimed(t *testing.T) {
	// Arrange
	owner := newOwnerForViews(t, t)
	viewTB := NewRecordingTB(t, "TestMine")
	view := owner.View(viewTB)
	mine, theirs := t.TempDir(), t.TempDir()
	view.Claim(mine)
	appendRecords(t, owner, warnFor(mine, "daemon.mine"), warnFor(theirs, "daemon.theirs"), warnFor("", "daemon.unscoped"))

	// Act
	got := operations(view.UnexpectedWarnings())

	// Assert
	if len(got) != 1 || got[0] != "daemon.mine" {
		t.Fatalf("view.UnexpectedWarnings = %v, want only daemon.mine", got)
	}
}

func TestTheOwnerSweepsEveryRecordNoViewClaimed(t *testing.T) {
	// Arrange
	owner := newOwnerForViews(t, t)
	view := owner.View(NewRecordingTB(t, "TestMine"))
	mine, theirs := t.TempDir(), t.TempDir()
	view.Claim(mine)
	appendRecords(t, owner, warnFor(mine, "daemon.mine"), warnFor(theirs, "daemon.theirs"), warnFor("", "daemon.unscoped"))

	// Act
	got := strings.Join(operations(owner.UnexpectedWarnings()), ",")

	// Assert
	if got != "daemon.theirs,daemon.unscoped" {
		t.Fatalf("owner.UnexpectedWarnings = %s, want daemon.theirs,daemon.unscoped", got)
	}
}

func TestAViewsDeclarationLicensesNoOtherViewsRecord(t *testing.T) {
	// Arrange: two views, each claiming one workspace that logs the same op;
	// only the first declares it.
	owner := newOwnerForViews(t, t)
	declaring := owner.View(NewRecordingTB(t, "TestDeclaring"))
	silent := owner.View(NewRecordingTB(t, "TestSilent"))
	a, b := t.TempDir(), t.TempDir()
	declaring.Claim(a)
	silent.Claim(b)
	declaring.ExpectWarnings("daemon.shared_op")
	appendRecords(t, owner, warnFor(a, "daemon.shared_op"), warnFor(b, "daemon.shared_op"))

	// Act
	gotDeclaring := declaring.UnexpectedWarnings()
	gotSilent := silent.UnexpectedWarnings()

	// Assert
	if len(gotDeclaring) != 0 {
		t.Errorf("the declaring view flagged %v, want nothing", operations(gotDeclaring))
	}
	if len(gotSilent) != 1 || gotSilent[0].WorkspaceDir != b {
		t.Errorf("the silent view flagged %v, want its own daemon.shared_op", gotSilent)
	}
}

func TestExpectWarningsForLicensesThatWorkspacesRecordsOnly(t *testing.T) {
	// Arrange: an owner (not a view) with two workspaces, one declared for.
	owner := newOwnerForViews(t, t)
	a, b := t.TempDir(), t.TempDir()
	owner.ExpectWarningsFor(&workspacev1.WorkspaceRef{Dir: a}, "daemon.op")
	appendRecords(t, owner, warnFor(a, "daemon.op"), warnFor(b, "daemon.op"))

	// Act
	got := owner.UnexpectedWarnings()

	// Assert
	if len(got) != 1 || got[0].WorkspaceDir != b {
		t.Fatalf("UnexpectedWarnings = %v, want only workspace %s's daemon.op", got, b)
	}
}

func TestAClaimMatchesRecordsThatNameTheResolvedDirectory(t *testing.T) {
	// Arrange: the view claims through a symlink; the daemon writes the
	// resolved path.
	owner := newOwnerForViews(t, t)
	view := owner.View(NewRecordingTB(t, "TestMine"))
	real := t.TempDir()
	link := filepath.Join(t.TempDir(), "link")
	if err := os.Symlink(real, link); err != nil {
		t.Fatalf("symlink: %v", err)
	}
	resolved, err := filepath.EvalSymlinks(real)
	if err != nil {
		t.Fatalf("EvalSymlinks: %v", err)
	}
	view.Claim(link)
	appendRecords(t, owner, warnFor(resolved, "daemon.mine"))

	// Act
	got := view.UnexpectedWarnings()

	// Assert
	if len(got) != 1 {
		t.Fatalf("view.UnexpectedWarnings = %v, want the record naming the resolved dir", got)
	}
}

func TestAViewRequiresOnlyWhatItsOwnWorkspacesProduced(t *testing.T) {
	// Arrange: the required op is produced, but by a workspace the view does
	// not own.
	owner := newOwnerForViews(t, t)
	view := owner.View(NewRecordingTB(t, "TestMine"))
	mine, theirs := t.TempDir(), t.TempDir()
	view.Claim(mine)
	view.RequireWarnings("daemon.op")
	appendRecords(t, owner, warnFor(theirs, "daemon.op"))

	// Act
	got := view.UnproducedRequiredWarnings()

	// Assert
	if len(got) != 1 || got[0] != "daemon.op" {
		t.Fatalf("UnproducedRequiredWarnings = %v, want daemon.op still unproduced", got)
	}
}

func TestARequiredWarningCountsAsUnproducedUntilItIsProduced(t *testing.T) {
	tests := []struct {
		name  string
		level string
		want  []string
	}{
		{name: "an error record produces it", level: "error"},
		{name: "a warn record produces it", level: "warn"},
		{name: "an info record does not", level: "info", want: []string{"daemon.op"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			owner := newOwnerForViews(t, t)
			view := owner.View(NewRecordingTB(t, "TestMine"))
			mine := t.TempDir()
			view.Claim(mine)
			view.RequireWarnings("daemon.op")
			before := view.UnproducedRequiredWarnings()
			record := warnFor(mine, "daemon.op")
			record.Level = tt.level

			// Act
			appendRecords(t, owner, record)
			got := view.UnproducedRequiredWarnings()

			// Assert
			if !reflect.DeepEqual(before, []string{"daemon.op"}) {
				t.Fatalf("UnproducedRequiredWarnings before any record = %v, want [daemon.op]", before)
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("UnproducedRequiredWarnings after a %s record = %v, want %v", tt.level, got, tt.want)
			}
		})
	}
}

func TestAViewRefusesWhatOnlyAnOwnerMayDo(t *testing.T) {
	tests := []struct {
		name string
		act  func(view *Daemon, viewTB *RecordingTB)
		want string
	}{
		{"a view of a view", func(view *Daemon, viewTB *RecordingTB) { view.View(viewTB) }, "View of a view"},
		{"stopping the process", func(view *Daemon, _ *RecordingTB) { view.Stop() }, "Stop on a view"},
		{"killing the process", func(view *Daemon, _ *RecordingTB) { view.Kill() }, "Kill on a view"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			owner := newOwnerForViews(t, t)
			viewTB := NewRecordingTB(t, "TestMine")
			view := owner.View(viewTB)

			// Act
			completed := Within(func() { tt.act(view, viewTB) })

			// Assert
			if completed || !strings.Contains(viewTB.Failures(), tt.want) {
				t.Fatalf("completed=%v failures=%q, want a fatal naming %q", completed, viewTB.Failures(), tt.want)
			}
		})
	}
}

func TestClaimingOnTheOwnerIsRefused(t *testing.T) {
	// Arrange
	ownerTB := NewRecordingTB(t, "TestOwner")
	owner := newOwnerForViews(t, ownerTB)
	dir := t.TempDir()

	// Act
	completed := Within(func() { owner.Claim(dir) })

	// Assert
	if completed || !strings.Contains(ownerTB.Failures(), "only a view claims") {
		t.Fatalf("completed=%v failures=%q, want the owner's claim refused", completed, ownerTB.Failures())
	}
}

func TestASecondTestClaimingAClaimedWorkspaceIsRefused(t *testing.T) {
	// Arrange
	owner := newOwnerForViews(t, t)
	first := owner.View(NewRecordingTB(t, "TestFirst"))
	secondTB := NewRecordingTB(t, "TestSecond")
	second := owner.View(secondTB)
	dir := t.TempDir()
	first.Claim(dir)

	// Act
	completed := Within(func() { second.Claim(dir) })

	// Assert
	if completed || !strings.Contains(secondTB.Failures(), "already claimed by TestFirst") {
		t.Fatalf("completed=%v failures=%q, want the second claim refused naming TestFirst", completed, secondTB.Failures())
	}
}

func TestTheViewsSweepFailsItsOwnTestAtItsEnd(t *testing.T) {
	// Arrange
	owner := newOwnerForViews(t, t)
	viewTB := NewRecordingTB(t, "TestMine")
	view := owner.View(viewTB)
	dir := t.TempDir()
	view.Claim(dir)
	appendRecords(t, owner, warnFor(dir, "daemon.undeclared"))

	// Act
	viewTB.RunCleanups()

	// Assert
	if !strings.Contains(viewTB.Failures(), "daemon.undeclared") {
		t.Fatalf("view test failures = %q, want the undeclared warning", viewTB.Failures())
	}
}

func TestTheOwnerNamesTheViewWhoseWorkspaceLoggedAfterItsSweep(t *testing.T) {
	// Arrange: the view's test ends cleanly, then its workspace logs an
	// undeclared warning.
	ownerTB := NewRecordingTB(t, "TestOwner")
	owner := newOwnerForViews(t, ownerTB)
	viewTB := NewRecordingTB(t, "TestMine")
	view := owner.View(viewTB)
	dir := t.TempDir()
	view.Claim(dir)
	viewTB.RunCleanups()
	later := time.Now().Add(time.Second).UTC().Format(time.RFC3339Nano)
	late := warnFor(dir, "daemon.late")
	late.Timestamp = later

	// Act
	appendRecords(t, owner, late)
	owner.assertNoUnexpectedWarnings()

	// Assert
	if viewTB.Failed() {
		t.Errorf("the view's own test failed: %q, want it clean", viewTB.Failures())
	}
	if !strings.Contains(ownerTB.Failures(), "TestMine: warn daemon.late") {
		t.Errorf("owner failures = %q, want the late record named by TestMine", ownerTB.Failures())
	}
}

func TestTheOwnerLeavesARecordTheViewAlreadySweptToTheView(t *testing.T) {
	// Arrange: the record predates the view's sweep, which reported it.
	ownerTB := NewRecordingTB(t, "TestOwner")
	owner := newOwnerForViews(t, ownerTB)
	viewTB := NewRecordingTB(t, "TestMine")
	view := owner.View(viewTB)
	dir := t.TempDir()
	view.Claim(dir)
	appendRecords(t, owner, warnFor(dir, "daemon.early"))
	viewTB.RunCleanups()

	// Act
	owner.assertNoUnexpectedWarnings()

	// Assert
	if !strings.Contains(viewTB.Failures(), "daemon.early") {
		t.Errorf("view failures = %q, want daemon.early reported by the view", viewTB.Failures())
	}
	if ownerTB.Failed() {
		t.Errorf("owner failures = %q, want none: the view already answered for the record", ownerTB.Failures())
	}
}

func TestALateRecordTheViewDeclaredIsNotReported(t *testing.T) {
	// Arrange
	ownerTB := NewRecordingTB(t, "TestOwner")
	owner := newOwnerForViews(t, ownerTB)
	viewTB := NewRecordingTB(t, "TestMine")
	view := owner.View(viewTB)
	dir := t.TempDir()
	view.Claim(dir)
	view.ExpectWarnings("daemon.late")
	viewTB.RunCleanups()
	late := warnFor(dir, "daemon.late")
	late.Timestamp = time.Now().Add(time.Second).UTC().Format(time.RFC3339Nano)
	appendRecords(t, owner, late)

	// Act
	owner.assertNoUnexpectedWarnings()

	// Assert
	if ownerTB.Failed() {
		t.Fatalf("owner failures = %q, want none: the view declared daemon.late", ownerTB.Failures())
	}
}

// registeringClient answers RegisterWorkspace with the given ref and
// SetWorkspaceSummary with a success, as the daemon would, recording every
// summary set into summaries when it is non-nil; every other call is
// unexpected.
type registeringClient struct {
	agentreplv1connect.AgentReplClient
	ref       *workspacev1.WorkspaceRef
	summaries *[]*agentreplv1.SetWorkspaceSummaryRequest
}

func (c registeringClient) SetWorkspaceSummary(_ context.Context, req *connect.Request[agentreplv1.SetWorkspaceSummaryRequest]) (*connect.Response[agentreplv1.SetWorkspaceSummaryResponse], error) {
	if c.summaries != nil {
		*c.summaries = append(*c.summaries, req.Msg)
	}
	return connect.NewResponse(&agentreplv1.SetWorkspaceSummaryResponse{
		Result: &agentreplv1.SetWorkspaceSummaryResponse_Success{Success: &agentreplv1.SetWorkspaceSummarySuccess{}},
	}), nil
}

func (c registeringClient) RegisterWorkspace(context.Context, *connect.Request[agentreplv1.RegisterWorkspaceRequest]) (*connect.Response[agentreplv1.RegisterWorkspaceResponse], error) {
	return connect.NewResponse(&agentreplv1.RegisterWorkspaceResponse{
		Result: &agentreplv1.RegisterWorkspaceResponse_Success{Success: &agentreplv1.RegisterWorkspaceSuccess{Workspace: c.ref}},
	}), nil
}

func TestRegisterThroughAViewClaimsTheWorkspace(t *testing.T) {
	// Arrange
	owner := newOwnerForViews(t, t)
	dir := t.TempDir()
	owner.client = registeringClient{ref: &workspacev1.WorkspaceRef{Id: "w1", Dir: dir}}
	view := owner.View(NewRecordingTB(t, "TestMine"))
	appendRecords(t, owner, warnFor(dir, "daemon.mine"))

	// Act
	Register(t, view, dir)

	// Assert
	if got := view.UnexpectedWarnings(); len(got) != 1 {
		t.Errorf("view.UnexpectedWarnings = %v, want the registered workspace's record", got)
	}
	if got := owner.UnexpectedWarnings(); len(got) != 0 {
		t.Errorf("owner.UnexpectedWarnings = %v, want none: the view claimed the workspace", got)
	}
}

func TestRegisterThroughTheOwnerClaimsNothing(t *testing.T) {
	// Arrange
	owner := newOwnerForViews(t, t)
	dir := t.TempDir()
	owner.client = registeringClient{ref: &workspacev1.WorkspaceRef{Id: "w1", Dir: dir}}
	appendRecords(t, owner, warnFor(dir, "daemon.mine"))

	// Act
	Register(t, owner, dir)

	// Assert
	if got := owner.UnexpectedWarnings(); len(got) != 1 {
		t.Fatalf("owner.UnexpectedWarnings = %v, want the record: nothing claimed it", got)
	}
}

func TestRegisterStatesTheSeededSummary(t *testing.T) {
	tests := []struct {
		name     string
		register func(testing.TB, *Daemon, string) *workspacev1.WorkspaceRef
		want     []string
	}{
		{name: "Register", register: Register, want: []string{SeededLongSummary + " | " + SeededShortSummary}},
		{name: "RegisterOwingSummary", register: RegisterOwingSummary, want: nil},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			owner := newOwnerForViews(t, t)
			dir := t.TempDir()
			var set []*agentreplv1.SetWorkspaceSummaryRequest
			owner.client = registeringClient{ref: &workspacev1.WorkspaceRef{Id: "w1", Dir: dir}, summaries: &set}

			// Act
			tt.register(t, owner, dir)

			// Assert
			var got []string
			for _, req := range set {
				if req.GetWorkspace().GetId() != "w1" {
					t.Fatalf("SetWorkspaceSummary named workspace %q, want the registered w1", req.GetWorkspace().GetId())
				}
				got = append(got, req.GetLongSummary()+" | "+req.GetShortSummary())
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Fatalf("summaries set = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestALateRecordWithAnUnparseableTimestampFailsTheOwner(t *testing.T) {
	// Arrange
	ownerTB := NewRecordingTB(t, "TestOwner")
	owner := newOwnerForViews(t, ownerTB)
	viewTB := NewRecordingTB(t, "TestMine")
	view := owner.View(viewTB)
	dir := t.TempDir()
	view.Claim(dir)
	viewTB.RunCleanups()
	broken := warnFor(dir, "daemon.late")
	broken.Timestamp = "not a time"
	appendRecords(t, owner, broken)

	// Act
	owner.assertNoUnexpectedWarnings()

	// Assert
	if !strings.Contains(ownerTB.Failures(), `unparseable timestamp "not a time"`) {
		t.Fatalf("owner failures = %q, want the unparseable timestamp named", ownerTB.Failures())
	}
}

func TestClaimingADirectoryThatDoesNotExistIsRefused(t *testing.T) {
	// Arrange
	owner := newOwnerForViews(t, t)
	viewTB := NewRecordingTB(t, "TestMine")
	view := owner.View(viewTB)
	missing := filepath.Join(t.TempDir(), "gone")

	// Act
	completed := Within(func() { view.Claim(missing) })

	// Assert
	if completed || !strings.Contains(viewTB.Failures(), "resolve workspace dir "+missing) {
		t.Fatalf("completed=%v failures=%q, want the missing dir refused", completed, viewTB.Failures())
	}
}

func TestARepositoryAViewsTestMintsIsInTheSharedDaemonsGitWorld(t *testing.T) {
	// Arrange
	owner := newOwnerForViews(t, t)
	viewTB := NewRecordingTB(t, "TestMine")
	owner.View(viewTB)

	// Act
	repo := NewRepo(viewTB)

	// Assert
	if owner.Git.read().Repo(repo.Dir) == nil {
		t.Fatalf("the shared daemon's git world has no row for %s, want the view's repository there", repo.Dir)
	}
}

func TestAViewOverATestThatAlreadyHasItsOwnGitWorldIsRefused(t *testing.T) {
	// Arrange
	owner := newOwnerForViews(t, t)
	viewTB := NewRecordingTB(t, "TestMine")
	World(viewTB)

	// Act
	completed := Within(func() { owner.View(viewTB) })

	// Assert
	if completed || !strings.Contains(viewTB.Failures(), "already has a git world of its own") {
		t.Fatalf("completed=%v failures=%q, want the view refused", completed, viewTB.Failures())
	}
}

func TestAViewOfADaemonWithNoGitWorldIsRefused(t *testing.T) {
	// Arrange
	owner := newOwnerForViews(t, t)
	owner.Git = nil
	viewTB := NewRecordingTB(t, "TestMine")

	// Act
	completed := Within(func() { owner.View(viewTB) })

	// Assert
	if completed || !strings.Contains(viewTB.Failures(), "views a daemon with no git world") {
		t.Fatalf("completed=%v failures=%q, want the view refused", completed, viewTB.Failures())
	}
}

func TestAViewsStreamsSettleAgainstTheView(t *testing.T) {
	// Arrange
	owner := newOwnerForViews(t, t)

	// Act
	v := owner.View(t)

	// Assert: a stream the view opens finds the view to settle against
	// (Stream.settle), as an owner's finds its owner.
	if got, _ := v.ctx.Value(daemonKey{}).(*Daemon); got != v {
		t.Fatalf("the view's context carries %p, want the view %p", got, v)
	}
}

// retiringClient is registeringClient that also answers the retirement's
// calls from scripts, recording each call as "<verb> <id>".
type retiringClient struct {
	registeringClient
	refs    map[string]*workspacev1.WorkspaceRef
	calls   *[]string
	kill    func(id string) (*agentreplv1.KillWorkspaceResponse, error)
	forgets map[string][]*agentreplv1.ForgetWorkspaceResponse
	held    []string
	// found answers FindWorkspaces.
	found []*agentreplv1.FoundWorkspace
}

func (c retiringClient) FindWorkspaces(context.Context, *connect.Request[agentreplv1.FindWorkspacesRequest]) (*connect.Response[agentreplv1.FindWorkspacesResponse], error) {
	*c.calls = append(*c.calls, "find")
	return connect.NewResponse(&agentreplv1.FindWorkspacesResponse{Result: &agentreplv1.FindWorkspacesResponse_Success{Success: &agentreplv1.FindWorkspacesSuccess{
		Extent: &agentreplv1.FindWorkspacesSuccess_Complete{Complete: &agentreplv1.FindWorkspacesSuccessComplete{Workspaces: c.found}}}}}), nil
}

func (c retiringClient) RegisterWorkspace(_ context.Context, req *connect.Request[agentreplv1.RegisterWorkspaceRequest]) (*connect.Response[agentreplv1.RegisterWorkspaceResponse], error) {
	return connect.NewResponse(&agentreplv1.RegisterWorkspaceResponse{
		Result: &agentreplv1.RegisterWorkspaceResponse_Success{Success: &agentreplv1.RegisterWorkspaceSuccess{Workspace: c.refs[req.Msg.GetDir()]}},
	}), nil
}

func (c retiringClient) KillWorkspace(_ context.Context, req *connect.Request[agentreplv1.KillWorkspaceRequest]) (*connect.Response[agentreplv1.KillWorkspaceResponse], error) {
	id := req.Msg.GetWorkspace().GetId()
	*c.calls = append(*c.calls, "kill "+id)
	if c.kill != nil {
		resp, err := c.kill(id)
		if err != nil {
			return nil, err
		}
		return connect.NewResponse(resp), nil
	}
	return connect.NewResponse(&agentreplv1.KillWorkspaceResponse{Result: &agentreplv1.KillWorkspaceResponse_Success{Success: &agentreplv1.KillWorkspaceSuccess{}}}), nil
}

func (c retiringClient) ForgetWorkspace(_ context.Context, req *connect.Request[agentreplv1.ForgetWorkspaceRequest]) (*connect.Response[agentreplv1.ForgetWorkspaceResponse], error) {
	id := req.Msg.GetWorkspace().GetId()
	*c.calls = append(*c.calls, "forget "+id)
	if script := c.forgets[id]; len(script) > 0 {
		c.forgets[id] = script[1:]
		return connect.NewResponse(script[0]), nil
	}
	return connect.NewResponse(&agentreplv1.ForgetWorkspaceResponse{Result: &agentreplv1.ForgetWorkspaceResponse_Success{Success: &agentreplv1.ForgetWorkspaceSuccess{}}}), nil
}

func (c retiringClient) OpenFeed(_ context.Context, req *connect.Request[agentreplv1.OpenFeedRequest]) (*connect.Response[agentreplv1.OpenFeedResponse], error) {
	*c.calls = append(*c.calls, "open_feed "+req.Msg.GetWorkspace().GetId())
	held := &frontendv1.FeedHeld{}
	for _, turn := range c.held {
		held.Items = append(held.Items, &frontendv1.FeedHeldItem{Item: &frontendv1.FeedHeldItem_Prompt{Prompt: &frontendv1.HeldPrompt{Turn: &conversationv1.TurnId{Value: turn}}}})
	}
	return connect.NewResponse(&agentreplv1.OpenFeedResponse{Result: &agentreplv1.OpenFeedResponse_Success{Success: &agentreplv1.OpenFeedSuccess{Held: held}}}), nil
}

func (c retiringClient) UpdateHeldPrompt(_ context.Context, req *connect.Request[agentreplv1.UpdateHeldPromptRequest]) (*connect.Response[agentreplv1.UpdateHeldPromptResponse], error) {
	*c.calls = append(*c.calls, "drop "+req.Msg.GetTurn().GetValue())
	return connect.NewResponse(&agentreplv1.UpdateHeldPromptResponse{Result: &agentreplv1.UpdateHeldPromptResponse_Success{Success: &agentreplv1.UpdateHeldPromptSuccess{}}}), nil
}

func forgetError(e *agentreplv1.ForgetWorkspaceError) *agentreplv1.ForgetWorkspaceResponse {
	return &agentreplv1.ForgetWorkspaceResponse{Result: &agentreplv1.ForgetWorkspaceResponse_Error{Error: e}}
}

func TestAViewRetiresItsWorkspacesWhenItsTestEnds(t *testing.T) {
	cases := []struct {
		name      string
		kill      func(id string) (*agentreplv1.KillWorkspaceResponse, error)
		forgets   map[string][]*agentreplv1.ForgetWorkspaceResponse
		held      []string
		found     func(d1, d2 string) []*agentreplv1.FoundWorkspace
		wantCalls []string
		wantFail  string
	}{
		{
			name:      "every workspace is killed and forgotten, latest first",
			wantCalls: []string{"find", "kill w2", "forget w2", "kill w1", "forget w1"},
		},
		{
			name: "a workspace the test already removed is passed over",
			kill: func(id string) (*agentreplv1.KillWorkspaceResponse, error) {
				return &agentreplv1.KillWorkspaceResponse{Result: &agentreplv1.KillWorkspaceResponse_Error{Error: &agentreplv1.KillWorkspaceError{
					Cause: &agentreplv1.KillWorkspaceError_UnknownWorkspace{UnknownWorkspace: &agentreplv1.KillWorkspaceUnknownWorkspace{}}}}}, nil
			},
			wantCalls: []string{"find", "kill w2", "kill w1"},
		},
		{
			name: "a forget its held prompts block drops them and forgets again",
			forgets: map[string][]*agentreplv1.ForgetWorkspaceResponse{"w2": {forgetError(&agentreplv1.ForgetWorkspaceError{
				Cause: &agentreplv1.ForgetWorkspaceError_Blocked{Blocked: &agentreplv1.ForgetWorkspaceBlocked{HeldPrompts: 1}}})}},
			held:      []string{"t1"},
			wantCalls: []string{"find", "kill w2", "forget w2", "open_feed w2", "drop t1", "forget w2", "kill w1", "forget w1"},
		},
		{
			name: "a main worktree's repository mates are retired before it is forgotten",
			forgets: map[string][]*agentreplv1.ForgetWorkspaceResponse{"w1": {forgetError(&agentreplv1.ForgetWorkspaceError{
				Cause: &agentreplv1.ForgetWorkspaceError_MainWorktree{MainWorktree: &agentreplv1.ForgetWorkspaceMainWorktree{OtherWorkspaces: 1}}})}},
			found: func(d1, d2 string) []*agentreplv1.FoundWorkspace {
				return []*agentreplv1.FoundWorkspace{
					{Workspace: &workspacev1.WorkspaceRef{Id: "w1", Dir: d1}, Repository: &workspacev1.RepositoryRef{Dir: d1}},
					{Workspace: &workspacev1.WorkspaceRef{Id: "m1"}, Repository: &workspacev1.RepositoryRef{Dir: d1}},
					{Workspace: &workspacev1.WorkspaceRef{Id: "x1"}, Repository: &workspacev1.RepositoryRef{Dir: d2 + "-elsewhere"}},
				}
			},
			wantCalls: []string{"find", "kill w2", "forget w2", "kill w1", "forget w1", "find", "kill m1", "forget m1", "forget w1"},
		},
		{
			name: "a workspace's closed children are retired before it is forgotten",
			forgets: map[string][]*agentreplv1.ForgetWorkspaceResponse{"w2": {forgetError(&agentreplv1.ForgetWorkspaceError{
				Cause: &agentreplv1.ForgetWorkspaceError_HasChildren{HasChildren: &agentreplv1.ForgetWorkspaceHasChildren{Children: []string{"c1"}}}})}},
			wantCalls: []string{"find", "kill w2", "forget w2", "kill c1", "forget c1", "forget w2", "kill w1", "forget w1"},
		},
		{
			name: "a workspace's open children are retired before it",
			kill: func() func(id string) (*agentreplv1.KillWorkspaceResponse, error) {
				refused := false
				return func(id string) (*agentreplv1.KillWorkspaceResponse, error) {
					if id == "w2" && !refused {
						refused = true
						return &agentreplv1.KillWorkspaceResponse{Result: &agentreplv1.KillWorkspaceResponse_Error{Error: &agentreplv1.KillWorkspaceError{
							Cause: &agentreplv1.KillWorkspaceError_HasOpenChildren{HasOpenChildren: &agentreplv1.WorkspaceHasOpenChildren{
								Children: []*workspacev1.WorkspaceRef{{Id: "c1"}}}}}}}, nil
					}
					return &agentreplv1.KillWorkspaceResponse{Result: &agentreplv1.KillWorkspaceResponse_Success{Success: &agentreplv1.KillWorkspaceSuccess{}}}, nil
				}
			}(),
			wantCalls: []string{"find", "kill w2", "kill c1", "forget c1", "kill w2", "forget w2", "kill w1", "forget w1"},
		},
		{
			name: "a refused kill fails the test and forgets nothing",
			kill: func(id string) (*agentreplv1.KillWorkspaceResponse, error) {
				return &agentreplv1.KillWorkspaceResponse{Result: &agentreplv1.KillWorkspaceResponse_Error{Error: &agentreplv1.KillWorkspaceError{
					Cause: &agentreplv1.KillWorkspaceError_TransferringAway{TransferringAway: &agentreplv1.KillWorkspaceTransferringAway{}}}}}, nil
			},
			wantCalls: []string{"find", "kill w2", "kill w1"},
			wantFail:  "KillWorkspace = error:{transferring_away",
		},
		{
			name: "a kill that cannot be asked fails the test",
			kill: func(string) (*agentreplv1.KillWorkspaceResponse, error) {
				return nil, errors.New("connection refused")
			},
			wantCalls: []string{"find", "kill w2", "kill w1"},
			wantFail:  "KillWorkspace = error connection refused",
		},
		{
			name: "a refused forget fails the test",
			forgets: map[string][]*agentreplv1.ForgetWorkspaceResponse{"w1": {forgetError(&agentreplv1.ForgetWorkspaceError{
				Cause: &agentreplv1.ForgetWorkspaceError_NotClosed{NotClosed: &agentreplv1.ForgetWorkspaceNotClosed{}}})}},
			wantCalls: []string{"find", "kill w2", "forget w2", "kill w1", "forget w1"},
			wantFail:  "ForgetWorkspace = error:{not_closed",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a view registers two workspaces.
			owner := newOwnerForViews(t, t)
			d1, d2 := t.TempDir(), t.TempDir()
			var calls []string
			owner.client = retiringClient{
				refs:  map[string]*workspacev1.WorkspaceRef{d1: {Id: "w1", Dir: d1}, d2: {Id: "w2", Dir: d2}},
				calls: &calls, kill: tc.kill, forgets: tc.forgets, held: tc.held,
			}
			if tc.found != nil {
				client := owner.client.(retiringClient)
				client.found = tc.found(d1, d2)
				owner.client = client
			}
			tb := NewRecordingTB(t, "TestRetires")
			view := owner.View(tb)
			Register(t, view, d1)
			Register(t, view, d2)

			// Act: the view's test ends.
			tb.RunCleanups()

			// Assert
			if !reflect.DeepEqual(calls, tc.wantCalls) {
				t.Fatalf("calls = %v, want %v", calls, tc.wantCalls)
			}
			switch failures := tb.Failures(); {
			case tc.wantFail == "" && failures != "":
				t.Fatalf("failures = %q, want none", failures)
			case tc.wantFail != "" && !strings.Contains(failures, tc.wantFail):
				t.Fatalf("failures = %q, want one saying %q", failures, tc.wantFail)
			}
		})
	}
}

// barrierClient is retiringClient that answers the barrier, recording each
// request's workspaces.
type barrierClient struct {
	retiringClient
	asked *[][]string
}

func (c barrierClient) AwaitIdle(_ context.Context, req *connect.Request[agentreplv1.AwaitIdleRequest]) (*connect.Response[agentreplv1.AwaitIdleResponse], error) {
	var ids []string
	for _, ws := range req.Msg.GetWorkspaces() {
		ids = append(ids, ws.GetId())
	}
	*c.asked = append(*c.asked, ids)
	return connect.NewResponse(&agentreplv1.AwaitIdleResponse{Result: &agentreplv1.AwaitIdleResponse_Success{Success: &agentreplv1.AwaitIdleSuccess{}}}), nil
}

func TestTheBarrierAsksAboutAViewsOwnWorkspacesOnly(t *testing.T) {
	// Arrange: a view registered two workspaces.
	owner := newOwnerForViews(t, t)
	d1, d2 := t.TempDir(), t.TempDir()
	var calls []string
	var asked [][]string
	owner.client = barrierClient{retiringClient: retiringClient{
		refs:  map[string]*workspacev1.WorkspaceRef{d1: {Id: "w1", Dir: d1}, d2: {Id: "w2", Dir: d2}},
		calls: &calls,
	}, asked: &asked}
	view := owner.View(NewRecordingTB(t, "TestBarrier"))
	Register(t, view, d1)
	Register(t, view, d2)

	// Act
	view.AwaitIdle()
	owner.AwaitIdle()

	// Assert: the view names its slice; the owner names nothing, so it waits
	// for everything.
	want := [][]string{{"w1", "w2"}, nil}
	if !reflect.DeepEqual(asked, want) {
		t.Fatalf("barrier scopes = %v, want %v", asked, want)
	}
}

func TestMainWorktreesBesideTheSlice(t *testing.T) {
	// Every directory is a plain path no symlink resolves, so canonicalDir
	// answers it as given.
	found := func(id, dir, repo string) *agentreplv1.FoundWorkspace {
		return &agentreplv1.FoundWorkspace{
			Workspace:  &workspacev1.WorkspaceRef{Id: id, Dir: dir},
			Repository: &workspacev1.RepositoryRef{Dir: repo},
		}
	}
	ref := func(id string) *workspacev1.WorkspaceRef { return &workspacev1.WorkspaceRef{Id: id} }
	cases := []struct {
		name  string
		found []*agentreplv1.FoundWorkspace
		slice []*workspacev1.WorkspaceRef
		want  []string
	}{
		{
			name:  "a linked worktree's main worktree is taken",
			found: []*agentreplv1.FoundWorkspace{found("main", "/r/a", "/r/a"), found("linked", "/r/a-one", "/r/a")},
			slice: []*workspacev1.WorkspaceRef{ref("linked")},
			want:  []string{"main"},
		},
		{
			name:  "a main worktree in the slice is not taken twice",
			found: []*agentreplv1.FoundWorkspace{found("main", "/r/a", "/r/a"), found("linked", "/r/a-one", "/r/a")},
			slice: []*workspacev1.WorkspaceRef{ref("main"), ref("linked")},
			want:  nil,
		},
		{
			name:  "another repository's main worktree is left alone",
			found: []*agentreplv1.FoundWorkspace{found("other", "/r/b", "/r/b"), found("linked", "/r/a-one", "/r/a")},
			slice: []*workspacev1.WorkspaceRef{ref("linked")},
			want:  nil,
		},
		{
			name:  "a linked worktree beside the slice's is no main worktree",
			found: []*agentreplv1.FoundWorkspace{found("linked", "/r/a-one", "/r/a"), found("created", "/r/a-two", "/r/a")},
			slice: []*workspacev1.WorkspaceRef{ref("linked")},
			want:  nil,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := mainWorktreesBeside(tc.found, tc.slice)

			// Assert.
			var ids []string
			for _, ws := range got {
				ids = append(ids, ws.GetId())
			}
			if !reflect.DeepEqual(ids, tc.want) {
				t.Fatalf("mainWorktreesBeside = %v, want %v", ids, tc.want)
			}
		})
	}
}

// findingClient answers FindWorkspaces with resp or err; every other call is
// unexpected.
type findingClient struct {
	agentreplv1connect.AgentReplClient
	resp *agentreplv1.FindWorkspacesResponse
	err  error
}

func (c findingClient) FindWorkspaces(context.Context, *connect.Request[agentreplv1.FindWorkspacesRequest]) (*connect.Response[agentreplv1.FindWorkspacesResponse], error) {
	if c.err != nil {
		return nil, c.err
	}
	return connect.NewResponse(c.resp), nil
}

func TestEveryWorkspaceTakesOnlyACompleteAnswer(t *testing.T) {
	one := &agentreplv1.FoundWorkspace{Workspace: &workspacev1.WorkspaceRef{Id: "w1"}}
	success := func(s *agentreplv1.FindWorkspacesSuccess) *agentreplv1.FindWorkspacesResponse {
		return &agentreplv1.FindWorkspacesResponse{Result: &agentreplv1.FindWorkspacesResponse_Success{Success: s}}
	}
	cases := []struct {
		name       string
		client     findingClient
		wantIDs    []string
		wantFailed bool
	}{
		{
			name: "a complete answer is every workspace",
			client: findingClient{resp: success(&agentreplv1.FindWorkspacesSuccess{Extent: &agentreplv1.FindWorkspacesSuccess_Complete{
				Complete: &agentreplv1.FindWorkspacesSuccessComplete{Workspaces: []*agentreplv1.FoundWorkspace{one}}}})},
			wantIDs: []string{"w1"},
		},
		{
			name: "a partial answer fails the retirement",
			client: findingClient{resp: success(&agentreplv1.FindWorkspacesSuccess{Extent: &agentreplv1.FindWorkspacesSuccess_Partial{
				Partial: &agentreplv1.FindWorkspacesSuccessPartial{}}})},
			wantFailed: true,
		},
		{
			name:       "a transport error fails the retirement",
			client:     findingClient{err: errors.New("connection refused")},
			wantFailed: true,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			rec := NewRecordingTB(t, "TestRetiring")
			owner := newOwnerForViews(t, rec)
			owner.client = tc.client

			// Act.
			got := owner.everyWorkspace("retire the slice")

			// Assert.
			var ids []string
			for _, w := range got {
				ids = append(ids, w.GetWorkspace().GetId())
			}
			if !reflect.DeepEqual(ids, tc.wantIDs) {
				t.Fatalf("everyWorkspace = %v, want %v", ids, tc.wantIDs)
			}
			if rec.Failed() != tc.wantFailed {
				t.Fatalf("failed = %v (%q), want %v", rec.Failed(), rec.Failures(), tc.wantFailed)
			}
			if tc.wantFailed && !strings.Contains(rec.Failures(), "retire the slice from the shared daemon: FindWorkspaces") {
				t.Fatalf("failure = %q, want it to name the retirement step", rec.Failures())
			}
		})
	}
}

func TestRecordKeyNamesTheWorkspaceByDirectoryThenID(t *testing.T) {
	cases := []struct {
		name   string
		record LogRecord
		want   string
	}{
		{name: "a directory wins", record: LogRecord{WorkspaceDir: "/r/a", WorkspaceID: "w1"}, want: "/r/a"},
		{name: "an id alone is keyed as an id", record: LogRecord{WorkspaceID: "w1"}, want: "id:w1"},
		{name: "neither is no workspace's", record: LogRecord{}, want: ""},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := recordKey(tc.record)

			// Assert
			if got != tc.want {
				t.Fatalf("recordKey = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestRegisterThroughAViewClaimsTheWorkspacesIDRecords(t *testing.T) {
	cases := []struct {
		name     string
		record   LogRecord
		viewOwns bool
	}{
		{name: "a record naming the registered id is the view's", record: LogRecord{Level: "warn", Operation: "daemon.banner", Message: "m", WorkspaceID: "w1"}, viewOwns: true},
		{name: "a record naming another id is not", record: LogRecord{Level: "warn", Operation: "daemon.banner", Message: "m", WorkspaceID: "w2"}, viewOwns: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			owner := newOwnerForViews(t, t)
			owner.client = registeringClient{ref: &workspacev1.WorkspaceRef{Id: "w1", Dir: t.TempDir()}}
			view := owner.View(NewRecordingTB(t, "TestMine"))
			Register(t, view, owner.client.(registeringClient).ref.GetDir())
			appendRecords(t, owner, tc.record)

			// Act
			owns := view.Owns(tc.record)

			// Assert
			if owns != tc.viewOwns || !owner.Owns(tc.record) {
				t.Fatalf("view.Owns = %v, owner.Owns = %v; want %v and true", owns, owner.Owns(tc.record), tc.viewOwns)
			}
			viewSwept, ownerSwept := len(view.UnexpectedWarnings()), len(owner.UnexpectedWarnings())
			if wantView := map[bool]int{true: 1, false: 0}[tc.viewOwns]; viewSwept != wantView || ownerSwept != 1-wantView {
				t.Fatalf("swept by view %d, by owner %d; want the record swept once, by its owner", viewSwept, ownerSwept)
			}
			if got := len(view.OwnRunLog()); got != map[bool]int{true: 1, false: 0}[tc.viewOwns] {
				t.Fatalf("view.OwnRunLog holds %d records, want the view's own only", got)
			}
		})
	}
}
