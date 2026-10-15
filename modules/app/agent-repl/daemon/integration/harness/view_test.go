package harness

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"sync"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"
)

// viewPID is the pid every record of these tests' fake daemon carries.
const viewPID = 4242

// recordingTB is a test the code under test may fail: it records Errorf and
// Fatalf instead of failing the real test, ends the calling goroutine on a
// Fatal as the testing package does, and keeps its cleanups for the test to
// run when it chooses. Everything else is the real test's.
type recordingTB struct {
	testing.TB
	name string

	mu       sync.Mutex
	errors   []string
	fatal    bool
	cleanups []func()
}

func newRecordingTB(t *testing.T, name string) *recordingTB {
	return &recordingTB{TB: t, name: name}
}

func (r *recordingTB) Name() string { return r.name }
func (r *recordingTB) Helper()      {}
func (r *recordingTB) Cleanup(f func()) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.cleanups = append(r.cleanups, f)
}
func (r *recordingTB) Errorf(format string, args ...any) {
	r.mu.Lock()
	defer r.mu.Unlock()
	r.errors = append(r.errors, fmt.Sprintf(format, args...))
}
func (r *recordingTB) Fatalf(format string, args ...any) {
	r.Errorf(format, args...)
	r.mu.Lock()
	r.fatal = true
	r.mu.Unlock()
	runtime.Goexit()
}
func (r *recordingTB) Failed() bool {
	r.mu.Lock()
	defer r.mu.Unlock()
	return len(r.errors) > 0
}

// runCleanups runs the recorded cleanups last-registered-first, as the testing
// package would at the test's end.
func (r *recordingTB) runCleanups() {
	r.mu.Lock()
	cleanups := r.cleanups
	r.cleanups = nil
	r.mu.Unlock()
	for i := len(cleanups) - 1; i >= 0; i-- {
		cleanups[i]()
	}
}

func (r *recordingTB) failures() string {
	r.mu.Lock()
	defer r.mu.Unlock()
	return strings.Join(r.errors, "\n")
}

// within runs act on its own goroutine, so a Fatalf ends act and not the test,
// and answers whether act ran to its end.
func within(act func()) (completed bool) {
	done := make(chan bool, 1)
	go func() {
		finished := false
		defer func() { done <- finished }()
		act()
		finished = true
	}()
	return <-done
}

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
		t: owner, ctx: context.Background(), StateDir: state, waitBound: DefaultTimeout,
		expected: map[string]bool{}, required: map[string]bool{},
		daemonProc: &daemonProc{cmd: &exec.Cmd{Process: process}},
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
	viewTB := newRecordingTB(t, "TestMine")
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
	view := owner.View(newRecordingTB(t, "TestMine"))
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
	declaring := owner.View(newRecordingTB(t, "TestDeclaring"))
	silent := owner.View(newRecordingTB(t, "TestSilent"))
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
	view := owner.View(newRecordingTB(t, "TestMine"))
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
	view := owner.View(newRecordingTB(t, "TestMine"))
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

func TestAViewRefusesWhatOnlyAnOwnerMayDo(t *testing.T) {
	tests := []struct {
		name string
		act  func(view *Daemon, viewTB *recordingTB)
		want string
	}{
		{"a view of a view", func(view *Daemon, viewTB *recordingTB) { view.View(viewTB) }, "View of a view"},
		{"stopping the process", func(view *Daemon, _ *recordingTB) { view.Stop() }, "Stop on a view"},
		{"killing the process", func(view *Daemon, _ *recordingTB) { view.Kill() }, "Kill on a view"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			owner := newOwnerForViews(t, t)
			viewTB := newRecordingTB(t, "TestMine")
			view := owner.View(viewTB)

			// Act
			completed := within(func() { tt.act(view, viewTB) })

			// Assert
			if completed || !strings.Contains(viewTB.failures(), tt.want) {
				t.Fatalf("completed=%v failures=%q, want a fatal naming %q", completed, viewTB.failures(), tt.want)
			}
		})
	}
}

func TestClaimingOnTheOwnerIsRefused(t *testing.T) {
	// Arrange
	ownerTB := newRecordingTB(t, "TestOwner")
	owner := newOwnerForViews(t, ownerTB)
	dir := t.TempDir()

	// Act
	completed := within(func() { owner.Claim(dir) })

	// Assert
	if completed || !strings.Contains(ownerTB.failures(), "only a view claims") {
		t.Fatalf("completed=%v failures=%q, want the owner's claim refused", completed, ownerTB.failures())
	}
}

func TestASecondTestClaimingAClaimedWorkspaceIsRefused(t *testing.T) {
	// Arrange
	owner := newOwnerForViews(t, t)
	first := owner.View(newRecordingTB(t, "TestFirst"))
	secondTB := newRecordingTB(t, "TestSecond")
	second := owner.View(secondTB)
	dir := t.TempDir()
	first.Claim(dir)

	// Act
	completed := within(func() { second.Claim(dir) })

	// Assert
	if completed || !strings.Contains(secondTB.failures(), "already claimed by TestFirst") {
		t.Fatalf("completed=%v failures=%q, want the second claim refused naming TestFirst", completed, secondTB.failures())
	}
}

func TestTheViewsSweepFailsItsOwnTestAtItsEnd(t *testing.T) {
	// Arrange
	owner := newOwnerForViews(t, t)
	viewTB := newRecordingTB(t, "TestMine")
	view := owner.View(viewTB)
	dir := t.TempDir()
	view.Claim(dir)
	appendRecords(t, owner, warnFor(dir, "daemon.undeclared"))

	// Act
	viewTB.runCleanups()

	// Assert
	if !strings.Contains(viewTB.failures(), "daemon.undeclared") {
		t.Fatalf("view test failures = %q, want the undeclared warning", viewTB.failures())
	}
}

func TestTheOwnerNamesTheViewWhoseWorkspaceLoggedAfterItsSweep(t *testing.T) {
	// Arrange: the view's test ends cleanly, then its workspace logs an
	// undeclared warning.
	ownerTB := newRecordingTB(t, "TestOwner")
	owner := newOwnerForViews(t, ownerTB)
	viewTB := newRecordingTB(t, "TestMine")
	view := owner.View(viewTB)
	dir := t.TempDir()
	view.Claim(dir)
	viewTB.runCleanups()
	later := time.Now().Add(time.Second).UTC().Format(time.RFC3339Nano)
	late := warnFor(dir, "daemon.late")
	late.Timestamp = later

	// Act
	appendRecords(t, owner, late)
	owner.assertNoUnexpectedWarnings()

	// Assert
	if viewTB.Failed() {
		t.Errorf("the view's own test failed: %q, want it clean", viewTB.failures())
	}
	if !strings.Contains(ownerTB.failures(), "TestMine: warn daemon.late") {
		t.Errorf("owner failures = %q, want the late record named by TestMine", ownerTB.failures())
	}
}

func TestTheOwnerLeavesARecordTheViewAlreadySweptToTheView(t *testing.T) {
	// Arrange: the record predates the view's sweep, which reported it.
	ownerTB := newRecordingTB(t, "TestOwner")
	owner := newOwnerForViews(t, ownerTB)
	viewTB := newRecordingTB(t, "TestMine")
	view := owner.View(viewTB)
	dir := t.TempDir()
	view.Claim(dir)
	appendRecords(t, owner, warnFor(dir, "daemon.early"))
	viewTB.runCleanups()

	// Act
	owner.assertNoUnexpectedWarnings()

	// Assert
	if !strings.Contains(viewTB.failures(), "daemon.early") {
		t.Errorf("view failures = %q, want daemon.early reported by the view", viewTB.failures())
	}
	if ownerTB.Failed() {
		t.Errorf("owner failures = %q, want none: the view already answered for the record", ownerTB.failures())
	}
}

func TestALateRecordTheViewDeclaredIsNotReported(t *testing.T) {
	// Arrange
	ownerTB := newRecordingTB(t, "TestOwner")
	owner := newOwnerForViews(t, ownerTB)
	viewTB := newRecordingTB(t, "TestMine")
	view := owner.View(viewTB)
	dir := t.TempDir()
	view.Claim(dir)
	view.ExpectWarnings("daemon.late")
	viewTB.runCleanups()
	late := warnFor(dir, "daemon.late")
	late.Timestamp = time.Now().Add(time.Second).UTC().Format(time.RFC3339Nano)
	appendRecords(t, owner, late)

	// Act
	owner.assertNoUnexpectedWarnings()

	// Assert
	if ownerTB.Failed() {
		t.Fatalf("owner failures = %q, want none: the view declared daemon.late", ownerTB.failures())
	}
}

// registeringClient answers RegisterWorkspace with the given ref, as the
// daemon would; every other call is unexpected.
type registeringClient struct {
	agentreplv1connect.AgentReplClient
	ref *workspacev1.WorkspaceRef
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
	view := owner.View(newRecordingTB(t, "TestMine"))
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

func TestALateRecordWithAnUnparseableTimestampFailsTheOwner(t *testing.T) {
	// Arrange
	ownerTB := newRecordingTB(t, "TestOwner")
	owner := newOwnerForViews(t, ownerTB)
	viewTB := newRecordingTB(t, "TestMine")
	view := owner.View(viewTB)
	dir := t.TempDir()
	view.Claim(dir)
	viewTB.runCleanups()
	broken := warnFor(dir, "daemon.late")
	broken.Timestamp = "not a time"
	appendRecords(t, owner, broken)

	// Act
	owner.assertNoUnexpectedWarnings()

	// Assert
	if !strings.Contains(ownerTB.failures(), `unparseable timestamp "not a time"`) {
		t.Fatalf("owner failures = %q, want the unparseable timestamp named", ownerTB.failures())
	}
}

func TestClaimingADirectoryThatDoesNotExistIsRefused(t *testing.T) {
	// Arrange
	owner := newOwnerForViews(t, t)
	viewTB := newRecordingTB(t, "TestMine")
	view := owner.View(viewTB)
	missing := filepath.Join(t.TempDir(), "gone")

	// Act
	completed := within(func() { view.Claim(missing) })

	// Assert
	if completed || !strings.Contains(viewTB.failures(), "resolve workspace dir "+missing) {
		t.Fatalf("completed=%v failures=%q, want the missing dir refused", completed, viewTB.failures())
	}
}
