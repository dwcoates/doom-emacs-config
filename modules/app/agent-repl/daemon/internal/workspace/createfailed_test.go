package workspace

import (
	"context"
	"errors"
	"fmt"
	"path/filepath"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/bounce"
	"agent-repld/internal/bringup"
	"agent-repld/internal/dlog"
	"agent-repld/internal/health"
	"agent-repld/internal/ids"
	"agent-repld/internal/prompts"
	"agent-repld/internal/rollout"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/wsm"
)

// mainWorkspaceID is the main-worktree workspace a failed create reports to.
const mainWorkspaceID = ids.WorkspaceID("main")

// createFailedMessage is the record a delivered report ends on.
const createFailedMessage = "asked the repository's main-worktree workspace why a create failed"

// failingCreate arranges a create in a registered repository whose main
// worktree is a registered workspace, and whose `git worktree add` fails. It
// answers the spec to create with.
func failingCreate(t *testing.T, f *fixture, closed bool) CreateSpec {
	t.Helper()
	spec := standardSpec(t, f)
	f.git.mainWorktree = spec.RepoDir
	f.git.createErr = errors.New("fatal: a branch named 'ABC/fix' already exists")
	f.db.with(wsm.Workspace{ID: mainWorkspaceID, Dir: spec.RepoDir, Repo: "repo-main", Name: "main", Closed: closed})
	f.briefs[BriefCreateFailed] = prompts.Prompt{
		Name:         BriefCreateFailed,
		Body:         "in {{repository}} asked {{request}} failed {{cause}} see {{agent_repl_dir}}",
		Placeholders: []string{"repository", "request", "cause", "agent_repl_dir"},
	}
	return spec
}

// submittedText answers the text of a queue submission.
func submittedText(sub interface {
	GetContent() *conversationv1.UserContent
}) string {
	return sub.GetContent().GetBlocks()[0].GetText().GetText()
}

func TestAFailedCreateIsReportedToTheMainWorktree(t *testing.T) {
	cases := []struct {
		name string
		// closed arranges the main worktree closed, with no session.
		closed bool
		// wantStarted is whether the report's open starts a session.
		wantStarted bool
	}{
		{name: "a closed main worktree is opened and asked", closed: true, wantStarted: true},
		{name: "a live main worktree is asked as it is", closed: false, wantStarted: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			spec := failingCreate(t, f, tc.closed)
			if !tc.closed {
				f.fleet.live[mainWorkspaceID] = true
			}

			// Act.
			_, err := f.verbs.Create(context.Background(), spec)
			f.verbs.(*verbs).createFailureReports.Wait()

			// Assert.
			if err == nil {
				t.Fatal("Create with a failing worktree add = success, want its failure")
			}
			if started := len(f.fleet.started) == 1 && f.fleet.started[0] == mainWorkspaceID; started != tc.wantStarted {
				t.Fatalf("started sessions = %v, want the main worktree started: %v", f.fleet.started, tc.wantStarted)
			}
			if f.db.closedFlags[mainWorkspaceID] {
				t.Fatal("the main worktree is still closed after the report")
			}
			if len(f.queue.submissions) != 1 {
				t.Fatalf("submissions = %+v, want the one report", f.queue.submissions)
			}
			sub := f.queue.submissions[0]
			if sub.WS != mainWorkspaceID {
				t.Fatalf("report workspace = %q, want %q", sub.WS, mainWorkspaceID)
			}
			if sub.Origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_CREATE_FAILED {
				t.Fatalf("report origin = %v, want WORKSPACE_CREATE_FAILED", sub.Origin)
			}
			if sub.Delivery != wsm.DeliveryDeferred {
				t.Fatalf("report delivery = %v, want deferred", sub.Delivery)
			}
			text := submittedText(sub.Said)
			for _, want := range []string{spec.RepoDir, "already exists", "fix the login bug", fixtureCheckoutRoot} {
				if !strings.Contains(text, want) {
					t.Fatalf("report = %q, want it to carry %q", text, want)
				}
			}
			if record, _ := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opCreateFailedReport}); record.Message != createFailedMessage {
				t.Fatalf("report record = %q, want %q", record.Message, createFailedMessage)
			}
		})
	}
}

func TestASucceededCreateReportsNothing(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	spec := failingCreate(t, f, true)
	f.git.createErr = nil

	// Act.
	if _, err := f.verbs.Create(context.Background(), spec); err != nil {
		t.Fatalf("Create: %v", err)
	}
	f.verbs.(*verbs).createFailureReports.Wait()

	// Assert.
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Operation: opCreateFailedReport}); ok {
		t.Fatalf("records = %+v, want no report record for a create that succeeded", f.log.logger.Records())
	}
}

func TestAMergeWorkspacesFailedCreateIsLeftToTheMergeQueue(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	spec := failingCreate(t, f, true)
	spec.MergeOf = &wsm.MergeOrigin{}
	v := f.verbs.(*verbs)

	// Act.
	v.reportCreateFailure(context.Background(), spec, errors.New("the merge workspace's worktree add failed"))

	// Assert.
	record, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelDebug, Operation: opCreateFailedReport})
	if !ok || !strings.Contains(record.Message, "merge queue") {
		t.Fatalf("records = %+v, want the debug record that the merge queue hands it back", f.log.logger.Records())
	}
}

func TestAHandedOverCreateIsNotReported(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	spec := failingCreate(t, f, true)
	v := f.verbs.(*verbs)

	// Act.
	v.reportCreateFailure(context.Background(), spec, fmt.Errorf("create: start the session: %w", shimclient.ErrStandingDown))

	// Assert.
	record, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelDebug, Operation: opCreateFailedReport})
	if !ok || !strings.Contains(record.Message, "handed over") {
		t.Fatalf("records = %+v, want the debug record that the create was handed over", f.log.logger.Records())
	}
}

func TestCreateHandedOver(t *testing.T) {
	cases := []struct {
		name string
		err  error
		want bool
	}{
		{name: "a transferring_away refusal", err: &Refusal{Arm: ArmTransferringAway}, want: true},
		{name: "a not_yet_adopted refusal", err: &Refusal{Arm: ArmNotYetAdopted}, want: true},
		{name: "a bring-up this daemon did not serve", err: fmt.Errorf("start: %w", bringup.ErrNotServed), want: true},
		{name: "a supervisor standing down", err: fmt.Errorf("start: %w", shimclient.ErrStandingDown), want: true},
		{name: "a workspace not yet adopted", err: fmt.Errorf("own: %w", rollout.ErrNotYetAdopted), want: true},
		{name: "a workspace moved away", err: fmt.Errorf("bounce: %w", bounce.ErrMovedAway), want: true},
		{name: "a daemon exiting", err: fmt.Errorf("git: %w", context.Canceled), want: true},
		{name: "any other refusal", err: &Refusal{Arm: ArmUnknownRepository}, want: false},
		{name: "an internal failure", err: errors.New("fatal: worktree add failed"), want: false},
		{name: "a deadline", err: fmt.Errorf("git: %w", context.DeadlineExceeded), want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the cases.

			// Act.
			got := createHandedOver(tc.err)

			// Assert.
			if got != tc.want {
				t.Fatalf("createHandedOver(%v) = %v, want %v", tc.err, got, tc.want)
			}
		})
	}
}

func TestAFailedCreateWithNowhereToReportIsRecordedAtInfo(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(f *fixture)
		message string
	}{
		{
			name:    "a directory in no git repository",
			arrange: func(f *fixture) { f.git.commonDirErr = errors.New("fatal: not a git repository") },
			message: "the failed create names no git repository; it is reported nowhere but this log",
		},
		{
			name:    "a repository whose main worktree is not registered",
			arrange: func(f *fixture) { f.git.mainWorktree = "/nowhere/registered" },
			message: "the failed create's repository has no registered main-worktree workspace; it is reported nowhere but this log",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			spec := failingCreate(t, f, true)
			tc.arrange(f)
			v := f.verbs.(*verbs)

			// Act.
			v.reportCreateFailure(context.Background(), spec, errors.New("fatal: worktree add failed"))
			v.createFailureReports.Wait()

			// Assert.
			record, _ := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opCreateFailedReport})
			if record.Message != tc.message {
				t.Fatalf("record = %q, want %q", record.Message, tc.message)
			}
			if len(f.queue.submissions) != 0 || len(f.fleet.started) != 0 {
				t.Fatalf("submissions = %+v, started = %v, want nothing opened or asked", f.queue.submissions, f.fleet.started)
			}
		})
	}
}

func TestAFailedCreatesReportThatCannotBeDeliveredIsAnError(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(f *fixture)
		message string
		// wantStarted is whether the report got as far as opening.
		wantStarted bool
	}{
		{
			name:    "the main worktree cannot be read",
			arrange: func(f *fixture) { f.db.workspaceByDirErr = errors.New("sql: database is locked") },
			message: "could not read the main-worktree workspace to report a failed create to",
		},
		{
			name:    "the main worktree's session will not start",
			arrange: func(f *fixture) { f.fleet.startErr = errors.New("the shim did not come up") },
			message: "could not open the main-worktree workspace to report a failed create to",
		},
		{
			name:        "the queue fails the submission",
			arrange:     func(f *fixture) { f.queue.submitErr = errors.New("the queue is closed") },
			message:     "the main-worktree workspace's queue did not accept the failed create's report",
			wantStarted: true,
		},
		{
			name:        "the queue refuses the submission",
			arrange:     func(f *fixture) { f.queue.submitRefusedArm = "workspace_closed" },
			message:     "the main-worktree workspace's queue refused the failed create's report",
			wantStarted: true,
		},
		{
			name:    "the brief is missing",
			arrange: func(f *fixture) { delete(f.briefs, BriefCreateFailed) },
			message: "could not read a daemon brief",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			spec := failingCreate(t, f, true)
			tc.arrange(f)
			v := f.verbs.(*verbs)

			// Act.
			v.reportCreateFailure(context.Background(), spec, errors.New("fatal: worktree add failed"))
			v.createFailureReports.Wait()

			// Assert.
			record, _ := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opCreateFailedReport})
			if record.Message != tc.message {
				t.Fatalf("record = %q, want %q", record.Message, tc.message)
			}
			if record.Context["create_failure"] != "fatal: worktree add failed" {
				t.Fatalf("record create_failure = %v, want the create's failure", record.Context["create_failure"])
			}
			if len(f.queue.submissions) != 0 {
				t.Fatalf("submissions = %+v, want none", f.queue.submissions)
			}
			if started := len(f.fleet.started) > 0; started != tc.wantStarted {
				t.Fatalf("started = %v, want the main worktree started: %v", f.fleet.started, tc.wantStarted)
			}
		})
	}
}

func TestAFailedCreatesMissingBriefRaisesThePromptsFault(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	spec := failingCreate(t, f, true)
	delete(f.briefs, BriefCreateFailed)
	v := f.verbs.(*verbs)

	// Act.
	v.reportCreateFailure(context.Background(), spec, errors.New("fatal: worktree add failed"))
	v.createFailureReports.Wait()

	// Assert.
	if len(f.health.opened) != 1 || f.health.opened[0].Kind != health.KindPromptsDirMissing {
		t.Fatalf("opened faults = %+v, want one prompts_dir_missing fault", f.health.opened)
	}
}

func TestDescribeCreateRequest(t *testing.T) {
	parent := ids.WorkspaceID("ws-parent")
	cases := []struct {
		name string
		spec CreateSpec
		want string
	}{
		{
			name: "every stated fact, one per line",
			spec: CreateSpec{
				Name: "fix-login", OneShot: true, BaseRef: "master", Parent: &parent, ForkFrom: &parent,
				Model: "opus", PermissionMode: "auto", InitialPrompt: "fix the login bug",
			},
			want: strings.Join([]string{
				"- name: `fix-login`",
				"- one-shot: yes",
				"- base ref: `master`",
				"- parent workspace id: `ws-parent`",
				"- forked from workspace id: `ws-parent`",
				"- model: `opus`",
				"- permission mode: `auto`",
				"- initial prompt:",
				"",
				"> fix the login bug",
			}, "\n"),
		},
		{
			name: "a create with no prompt says so",
			spec: CreateSpec{},
			want: "- initial prompt: none",
		},
		{
			name: "a multi-line prompt is quoted line by line",
			spec: CreateSpec{InitialPrompt: "first\n\n```go\nx\n```"},
			want: "- initial prompt:\n\n> first\n>\n> ```go\n> x\n> ```",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the cases.

			// Act.
			got := describeCreateRequest(tc.spec)

			// Assert.
			if got != tc.want {
				t.Fatalf("describeCreateRequest =\n%s\nwant\n%s", got, tc.want)
			}
		})
	}
}

func TestTheShippedCreateFailedBriefSplices(t *testing.T) {
	// Arrange.
	brief, err := prompts.Load(filepath.Join("..", "..", "..", "prompts"), BriefCreateFailed)
	if err != nil {
		t.Fatalf("Load: %v", err)
	}

	// Act.
	text, err := brief.Splice(map[string]string{
		"repository": "/repo", "request": "- initial prompt: none", "cause": "> boom", "agent_repl_dir": "/agent-repl",
	})

	// Assert.
	if err != nil {
		t.Fatalf("Splice: %v", err)
	}
	for _, want := range []string{"/repo", "- initial prompt: none", "> boom", "/agent-repl"} {
		if !strings.Contains(text, want) {
			t.Fatalf("brief = %q, want it to carry %q", text, want)
		}
	}
}
