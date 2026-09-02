//go:build integration

// Package integration exercises a real claude-repld process against fakes of
// every neighbor. Build tag `integration`; run it with
// `go test -tags integration ./integration/...`.
package integration

import (
	"errors"
	"os"
	"strings"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/integration/harness"

	"connectrpc.com/connect"
)

func TestMain(m *testing.M) { os.Exit(harness.Main(m)) }

// fixture is a daemon with one registered, opened workspace whose fake shim is
// ready. Almost every test starts here.
type fixture struct {
	d    *harness.Daemon
	repo *harness.Repo
	ws   *workspacev1.WorkspaceRef
	shim *harness.ShimControl
	t    *testing.T
}

// newDaemon starts a daemon with no workspace.
func newDaemon(t *testing.T, opts harness.Opts) *harness.Daemon {
	t.Helper()
	return harness.StartDaemon(t, opts)
}

// newRegistered starts a daemon and registers a fresh repository's worktree,
// without opening a session.
func newRegistered(t *testing.T, opts harness.Opts) *fixture {
	t.Helper()
	d := harness.StartDaemon(t, opts)
	repo := harness.NewRepo(t)
	ws := harness.Register(t, d, repo.Dir)
	d.WatchWorkspaceLogs(repo.Dir)
	return &fixture{d: d, repo: repo, ws: ws, t: t}
}

// newOpened starts a daemon, registers a repository, opens the workspace and
// waits for the fake shim's control socket.
func newOpened(t *testing.T, opts harness.Opts) *fixture {
	t.Helper()
	f := newRegistered(t, opts)
	f.open()
	return f
}

// open sends OpenWorkspace and attaches to the spawned fake shim.
func (f *fixture) open() {
	f.t.Helper()
	resp, err := f.d.Client().OpenWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenWorkspaceRequest{Workspace: f.ws}))
	if err != nil {
		f.t.Fatalf("OpenWorkspace = error %v, want a success", err)
	}
	if resp.Msg.GetSuccess() == nil {
		f.t.Fatalf("OpenWorkspace = %v, want a success", resp.Msg)
	}
	f.shim = f.d.Shim(f.ws)
}

// selectWorkspace makes this workspace the current one.
func (f *fixture) selectWorkspace() {
	f.t.Helper()
	if _, err := f.d.Client().SelectWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.SelectWorkspaceRequest{Workspace: f.ws})); err != nil {
		f.t.Fatalf("SelectWorkspace = error %v, want a success", err)
	}
}

// submit submits a prompt to the fixture's workspace.
func (f *fixture) submit(text, key string, origin conversationv1.PromptOrigin) *agentreplv1.SubmitPromptResponse {
	f.t.Helper()
	return f.submitRaw(&agentreplv1.SubmitPromptRequest{
		Workspace:      f.ws,
		Said:           said(text),
		IdempotencyKey: key,
		Origin:         origin,
	})
}

// submitRaw sends a SubmitPrompt exactly as given.
func (f *fixture) submitRaw(req *agentreplv1.SubmitPromptRequest) *agentreplv1.SubmitPromptResponse {
	f.t.Helper()
	resp, err := f.d.Client().SubmitPrompt(f.d.Ctx(), connect.NewRequest(req))
	if err != nil {
		f.t.Fatalf("SubmitPrompt(%q) = error %v, want an answer", text(req.GetSaid()), err)
	}
	return resp.Msg
}

// submitExpectingError sends a SubmitPrompt whose refusal is the subject.
func (f *fixture) submitExpectingError(req *agentreplv1.SubmitPromptRequest) error {
	f.t.Helper()
	_, err := f.d.Client().SubmitPrompt(f.d.Ctx(), connect.NewRequest(req))
	return err
}

// openFeed opens a feed and answers its first page and watch token.
func (f *fixture) openFeed(feed *frontendv1.FeedId) (*frontendv1.FeedPage, *agentreplv1.FeedWatchToken) {
	f.t.Helper()
	resp, err := f.d.Client().OpenFeed(f.d.Ctx(), connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: f.ws, Feed: feed}))
	if err != nil {
		f.t.Fatalf("OpenFeed = error %v, want a page and a token", err)
	}
	success := resp.Msg.GetSuccess()
	if success == nil {
		f.t.Fatalf("OpenFeed = %v, want a success", resp.Msg)
	}
	return success.GetPage(), success.GetWatch()
}

// openFeedOnceCarrying re-opens the root feed until its page satisfies the
// predicate, and answers that page and its token. It exists because a fake
// shim push is fire-and-forget: nothing tells a test when the daemon has
// finished routing a frame, and re-taking the open is how a test synchronizes
// on the page without sleeping.
func (f *fixture) openFeedOnceCarrying(what string, pred func(*frontendv1.FeedPage) bool) (*frontendv1.FeedPage, *agentreplv1.FeedWatchToken) {
	f.t.Helper()
	ticker := time.NewTicker(10 * time.Millisecond)
	defer ticker.Stop()
	for {
		page, token := f.openFeed(nil)
		if pred(page) {
			return page, token
		}
		select {
		case <-ticker.C:
		case <-f.d.Ctx().Done():
			f.t.Fatalf("waiting for a feed page carrying %s: %v", what, f.d.Ctx().Err())
			return nil, nil
		}
	}
}

// awaitRowInFeed answers a row of a feed that satisfies the predicate, looking
// FIRST at the page the open serves and only then at the tail.
//
// A sub-feed's rows are usually already history by the time a test opens it --
// a merge's tabs are all pushed before its terminal row exists to open a feed
// on -- and a tail delivers only what arrives after the open.
func (f *fixture) awaitRowInFeed(feed *frontendv1.FeedId, what string, pred func(*frontendv1.FeedRow) bool) *frontendv1.FeedRow {
	f.t.Helper()
	page, token := f.openFeed(feed)
	for _, row := range page.GetSuccess().GetRows() {
		if pred(row) {
			return row
		}
	}
	return awaitRow(f.t, f, f.d.WatchFeed(token), what, pred)
}

// watchRootFeed opens the root feed and tails it.
func (f *fixture) watchRootFeed() *harness.Stream[*frontendv1.FeedRow] {
	f.t.Helper()
	_, token := f.openFeed(nil)
	return f.d.WatchFeed(token)
}

// said builds a one-block user prompt.
func said(s string) *conversationv1.UserSaid {
	return &conversationv1.UserSaid{Content: &conversationv1.UserContent{
		Blocks: []*conversationv1.UserContentBlock{
			{Block: &conversationv1.UserContentBlock_Text{Text: &conversationv1.TextBlock{Text: s}}},
		},
	}}
}

// text reads back the concatenated text of a UserSaid.
func text(u *conversationv1.UserSaid) string {
	out := ""
	for _, b := range u.GetContent().GetBlocks() {
		out += b.GetText().GetText()
	}
	return out
}

// promptText reads back a drawn user-prompt row's text.
func promptText(row *frontendv1.FeedRow) string {
	out := ""
	for _, b := range row.GetUserPrompt().GetSuccess().GetBody().GetBlocks() {
		out += b.GetText().GetText()
	}
	return out
}

// activityFrame wraps one activity as an agent frame addressed to an agent.
func activityFrame(agent string, activity *conversationv1.AgentActivity) *conversationv1.AgentFrame {
	return &conversationv1.AgentFrame{
		AgentId: &conversationv1.AgentId{Value: agent},
		Result: &conversationv1.AgentFrame_Update{Update: &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_Activity{Activity: activity},
		}},
	}
}

// updateFrame wraps any AgentUpdate as a frame.
func updateFrame(agent string, update *conversationv1.AgentUpdate) *conversationv1.AgentFrame {
	return &conversationv1.AgentFrame{
		AgentId: &conversationv1.AgentId{Value: agent},
		Result:  &conversationv1.AgentFrame_Update{Update: update},
	}
}

// successFrame is an agent's terminal success frame.
func successFrame(agent string, answer *conversationv1.AgentActivityId) *conversationv1.AgentFrame {
	return &conversationv1.AgentFrame{
		AgentId: &conversationv1.AgentId{Value: agent},
		Result: &conversationv1.AgentFrame_Success{Success: &conversationv1.AgentSuccess{
			Outcome: &conversationv1.AgentSuccess_Completed{Completed: &conversationv1.AgentCompleted{Answer: answer}},
		}},
	}
}

// interruptedFrame is an agent's terminal interruption frame.
func interruptedFrame(agent string) *conversationv1.AgentFrame {
	return &conversationv1.AgentFrame{
		AgentId: &conversationv1.AgentId{Value: agent},
		Result: &conversationv1.AgentFrame_Success{Success: &conversationv1.AgentSuccess{
			Outcome: &conversationv1.AgentSuccess_Interrupted{Interrupted: &conversationv1.AgentInterrupted{
				Cause: &conversationv1.AgentInterrupted_ByUser{ByUser: &conversationv1.AgentInterruptedByUser{}},
			}},
		}},
	}
}

// failureFrame is an agent's terminal failure frame.
func failureFrame(agent string, failure *conversationv1.AgentFailure) *conversationv1.AgentFrame {
	return &conversationv1.AgentFrame{
		AgentId: &conversationv1.AgentId{Value: agent},
		Result:  &conversationv1.AgentFrame_Failure{Failure: failure},
	}
}

// detachedWorkFrame announces detached work on an agent's stream.
func detachedWorkFrame(agent string, work *conversationv1.AgentDetachedWork) *conversationv1.AgentFrame {
	return &conversationv1.AgentFrame{
		AgentId: &conversationv1.AgentId{Value: agent},
		Result:  &conversationv1.AgentFrame_DetachedWork{DetachedWork: work},
	}
}

// mainAgent is the agent id the fake shim answers StartTurn with.
const mainAgent = "main"

// startedAt builds an activity start stamp.
func startedAt(ms int64) *conversationv1.AgentActivityStartedAt {
	return &conversationv1.AgentActivityStartedAt{AtMs: ms}
}

// settledAt builds an activity settle stamp.
func settledAt(ms int64) *conversationv1.AgentActivitySettledAt {
	return &conversationv1.AgentActivitySettledAt{AtMs: ms}
}

// activityID builds an activity identity.
func activityID(v string) *conversationv1.AgentActivityId {
	return &conversationv1.AgentActivityId{Value: v}
}

// awaitRow reads feed rows until one satisfies the predicate.
func awaitRow(t *testing.T, f *fixture, s *harness.Stream[*frontendv1.FeedRow], what string, pred func(*frontendv1.FeedRow) bool) *frontendv1.FeedRow {
	t.Helper()
	return harness.AwaitView(t, f.d.Ctx(), s, what, pred)
}

// awaitFooter reads footer pushes until one satisfies the predicate.
func awaitFooter(t *testing.T, f *fixture, s *harness.Stream[*frontendv1.FooterView], what string, pred func(*frontendv1.FooterView) bool) *frontendv1.FooterView {
	t.Helper()
	return harness.AwaitView(t, f.d.Ctx(), s, what, pred)
}

// awaitTopbar reads topbar pushes until one satisfies the predicate.
func awaitTopbar(t *testing.T, f *fixture, s *harness.Stream[*frontendv1.TopbarView], what string, pred func(*frontendv1.TopbarView) bool) *frontendv1.TopbarView {
	t.Helper()
	return harness.AwaitView(t, f.d.Ctx(), s, what, pred)
}

// awaitRoster reads roster pushes until one satisfies the predicate.
func awaitRoster(t *testing.T, d *harness.Daemon, s *harness.Stream[*frontendv1.WorkspaceRoster], what string, pred func(*frontendv1.WorkspaceRoster) bool) *frontendv1.WorkspaceRoster {
	t.Helper()
	return harness.AwaitView(t, d.Ctx(), s, what, pred)
}

// rosterRow finds a workspace's row anywhere in the roster, repository
// sections, task sections, recently merged and nested children alike.
func rosterRow(roster *frontendv1.WorkspaceRoster, id string) *frontendv1.RosterRow {
	var walk func(rows []*frontendv1.RosterRow) *frontendv1.RosterRow
	walk = func(rows []*frontendv1.RosterRow) *frontendv1.RosterRow {
		for _, r := range rows {
			if r.GetWorkspace().GetWorkspace().GetId() == id {
				return r
			}
			if found := walk(r.GetChildren()); found != nil {
				return found
			}
		}
		return nil
	}
	for _, s := range roster.GetRepository().GetSections() {
		if found := walk(s.GetRows().GetRows()); found != nil {
			return found
		}
	}
	for _, s := range roster.GetTask().GetSections() {
		if found := walk(s.GetRows().GetRows()); found != nil {
			return found
		}
	}
	return walk(roster.GetRecentlyMerged().GetRows().GetRows())
}

// rosterRepoRow finds a workspace's row only under the repository grouping.
func rosterRepoRow(roster *frontendv1.WorkspaceRoster, id string) *frontendv1.RosterRow {
	for _, s := range roster.GetRepository().GetSections() {
		for _, r := range s.GetRows().GetRows() {
			if r.GetWorkspace().GetWorkspace().GetId() == id {
				return r
			}
			for _, c := range r.GetChildren() {
				if c.GetWorkspace().GetWorkspace().GetId() == id {
					return c
				}
			}
		}
	}
	return nil
}

// rosterTaskRow finds a workspace's row under a named task section.
func rosterTaskRow(roster *frontendv1.WorkspaceRoster, taskID, id string) *frontendv1.RosterRow {
	for _, s := range roster.GetTask().GetSections() {
		if s.GetKey().GetTaskId() != taskID {
			continue
		}
		for _, r := range s.GetRows().GetRows() {
			if r.GetWorkspace().GetWorkspace().GetId() == id {
				return r
			}
		}
	}
	return nil
}

// connectCode reads a Connect error's code, or CodeUnknown for anything else.
func connectCode(err error) connect.Code {
	var cerr *connect.Error
	if errors.As(err, &cerr) {
		return cerr.Code()
	}
	return connect.CodeUnknown
}

// namesIntendedArm reports whether a refusal names the exact unlanded error
// arm the contract will grow, in the contracted
// `intended arm: <Rpc>Error.<arm>: <reason>` spelling.
func namesIntendedArm(err error, arm string) bool {
	if err == nil {
		return false
	}
	msg := err.Error()
	return strings.Contains(msg, intendedArm) && strings.Contains(msg, arm)
}

// intendedArm is the refusal prefix a not-yet-landed error arm answers with.
const intendedArm = "intended arm: "

// containsField reports whether a validation refusal names a field.
func containsField(err error, field string) bool {
	return err != nil && strings.Contains(err.Error(), field)
}

// healthRequest is the DaemonHealth probe used as a liveness check.
func healthRequest() *connect.Request[agentreplv1.DaemonHealthRequest] {
	return connect.NewRequest(&agentreplv1.DaemonHealthRequest{})
}

// worktreeOf adds a named worktree to a repository and answers its directory.
func worktreeOf(t *testing.T, repo *harness.Repo, name string) string {
	t.Helper()
	return repo.AddWorktree(name)
}

// worktreeOfRepo is worktreeOf, named for the merge tests that care that the
// repository is the daemon's own checkout.
func worktreeOfRepo(t *testing.T, repo *harness.Repo, name string) string {
	t.Helper()
	return repo.AddWorktree(name)
}

// writeCommit commits a file inside a worktree of a repository.
func writeCommit(t *testing.T, repo *harness.Repo, worktree, file, content string) string {
	t.Helper()
	return repo.CommitIn(worktree, file, content)
}

// setPriority sets or clears a workspace's priority. It takes the FULL ref the
// daemon minted, because the daemon refuses a ref whose dir disagrees with the
// registry: rebuilding one from the id alone means guessing the dir.
//
// (This helper previously looked the dir up in a package-level cache that
// NOTHING EVER WROTE TO, so every caller fataled with "no registered directory
// recorded for workspace". The cache is gone; harness.Register already answers
// the ref with the dir on it.)
func setPriority(t *testing.T, d *harness.Daemon, ws *workspacev1.WorkspaceRef, p *agentreplv1.WorkspacePriority) {
	t.Helper()
	req := &agentreplv1.SetWorkspacePriorityRequest{Workspace: ws, Priority: p}
	if _, err := d.Client().SetWorkspacePriority(d.Ctx(), connect.NewRequest(req)); err != nil {
		t.Fatalf("SetWorkspacePriority(%s) = error %v, want a success", ws.GetId(), err)
	}
}

// repoRowIDs lists the workspace ids under the repository grouping, in order.
func repoRowIDs(r *frontendv1.WorkspaceRoster) []string {
	var out []string
	for _, s := range r.GetRepository().GetSections() {
		for _, row := range s.GetRows().GetRows() {
			out = append(out, row.GetWorkspace().GetWorkspace().GetId())
		}
	}
	return out
}

// sameOrder reports whether `want` appears in `got` in exactly that order.
func sameOrder(got, want []string) bool {
	if len(got) != len(want) {
		return false
	}
	for i := range want {
		if got[i] != want[i] {
			return false
		}
	}
	return true
}

// taskSection finds a task section by task id.
func taskSection(r *frontendv1.WorkspaceRoster, taskID string) *frontendv1.RosterTaskSection {
	for _, s := range r.GetTask().GetSections() {
		if s.GetKey().GetTaskId() == taskID {
			return s
		}
	}
	return nil
}

// createTask creates a task and answers its ref.
func createTask(t *testing.T, f *fixture, title string) *agentreplv1.TaskRef {
	t.Helper()
	resp, err := f.d.Client().CreateTask(f.d.Ctx(), connect.NewRequest(&agentreplv1.CreateTaskRequest{Title: title}))
	if err != nil {
		t.Fatalf("CreateTask(%q) = error %v, want a task ref", title, err)
	}
	return resp.Msg.GetSuccess().GetTask()
}

// assignTask assigns the fixture's workspace to a task.
func assignTask(t *testing.T, f *fixture, task *agentreplv1.TaskRef) {
	t.Helper()
	if _, err := f.d.Client().AssignWorkspaceTask(f.d.Ctx(), connect.NewRequest(&agentreplv1.AssignWorkspaceTaskRequest{Workspace: f.ws, Task: task})); err != nil {
		t.Fatalf("AssignWorkspaceTask = error %v, want a success", err)
	}
}

// openPermission is a permission ask in its opening state.
func openPermission(id, gated string) *conversationv1.AgentPermission {
	return &conversationv1.AgentPermission{
		Id:        &conversationv1.AgentPermissionId{Value: id},
		GatedCall: activityID(gated),
		Result: &conversationv1.AgentPermission_Start{Start: &conversationv1.AgentPermissionStart{
			Prompt:    &conversationv1.AgentPermissionPrompt{Title: "Run a command", DisplayName: "Bash"},
			StartedAt: startedAt(1_700_000_000_000),
		}},
	}
}

// standingPermission is a permission ask that also offers a standing rule.
func standingPermission(id, gated string) *conversationv1.AgentPermission {
	p := openPermission(id, gated)
	p.GetStart().OfferedStanding = &conversationv1.AgentPermissionStanding{
		Changes: []*conversationv1.AgentPermissionChange{{
			Destination: conversationv1.AgentPermissionDestination_AGENT_PERMISSION_DESTINATION_LOCAL_SETTINGS,
			Change: &conversationv1.AgentPermissionChange_AddRules{AddRules: &conversationv1.AgentPermissionRulesAdded{
				Rules:    []*conversationv1.AgentPermissionRule{{ToolName: "Bash", RuleContent: strPtr("ls:*")}},
				Behavior: conversationv1.AgentPermissionBehavior_AGENT_PERMISSION_BEHAVIOR_ALLOW,
			}},
		}},
	}
	return p
}

// answeredPermission is a permission ask that has been allowed once.
func answeredPermission(id, gated string) *conversationv1.AgentPermission {
	return &conversationv1.AgentPermission{
		Id:        &conversationv1.AgentPermissionId{Value: id},
		GatedCall: activityID(gated),
		Result: &conversationv1.AgentPermission_Success{Success: &conversationv1.AgentPermissionSuccess{
			Decision: &conversationv1.AgentPermissionSuccess_Allowed{Allowed: &conversationv1.AgentPermissionAllowed{
				Scope: &conversationv1.AgentPermissionAllowed_Once{Once: &conversationv1.AgentPermissionAllowedOnce{}},
			}},
		}},
	}
}

// detachedShell announces a detached bash unit.
func detachedShell(work, command string) *conversationv1.AgentDetachedWork {
	return &conversationv1.AgentDetachedWork{
		Work: &conversationv1.DetachedWorkId{Value: work},
		Origin: &conversationv1.AgentDetachedWork_Created{Created: &conversationv1.DetachedWorkCreated{
			WorkCreated: &conversationv1.DetachableWork{Work: &conversationv1.DetachableWork_Bash{Bash: &conversationv1.AgentBash{
				Result: &conversationv1.AgentBash_Start{Start: &conversationv1.AgentBashStart{
					Command:   &conversationv1.AgentBashCommand{Line: command},
					StartedAt: startedAt(1_700_000_000_000),
				}},
			}}},
		}},
	}
}

// detachedSubagent announces a detached subagent unit.
func detachedSubagent(work, agent, label string) *conversationv1.AgentDetachedWork {
	return &conversationv1.AgentDetachedWork{
		Work: &conversationv1.DetachedWorkId{Value: work},
		Origin: &conversationv1.AgentDetachedWork_Created{Created: &conversationv1.DetachedWorkCreated{
			WorkCreated: &conversationv1.DetachableWork{Work: &conversationv1.DetachableWork_Subagent{Subagent: &conversationv1.AgentSubagent{
				Result: &conversationv1.AgentSubagent_Start{Start: &conversationv1.AgentSubagentStart{
					CreatedAgentId: &conversationv1.AgentId{Value: agent},
					Prompt:         &conversationv1.AgentSubagentPrompt{Text: label},
					StartedAt:      startedAt(1_700_000_000_000),
				}},
			}}},
		}},
	}
}

func strPtr(s string) *string { return &s }
