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
	// host and web are the workspace's two client-hop streams, held open for
	// the fixture's life so the workspace reads CONNECTED. They are the
	// fixture's own: a test that wants to observe those streams opens its own.
	host *harness.Stream[*agentreplv1.WatchHostWorkspaceResponse]
	web  *harness.Stream[*agentreplv1.WatchWebWorkspaceResponse]
	// walk is the walk the fixture's last page named (feedPage), so a next
	// continues it.
	walk *frontendv1.FeedWalkId
}

// expectSessionKillRecords declares the WARN and ERROR trail that ENDING A LIVE
// SESSION ON PURPOSE leaves on the workspace's own log sink: the shim's death,
// the severed link, the two standing streams that end without the session
// ending, and — when the shim is hung or already gone — the forced KillSession
// that never answers.
//
// It exists because five tests kill a live session as their ARRANGEMENT, and
// each of them declared a different subset of the same trail while the sweep
// read the workspace sink through a symlink and saw none of it. One statement
// of the trail is what keeps those five from drifting apart again.
func expectSessionKillRecords(d *harness.Daemon) {
	d.ExpectWarnings(
		"daemon.shimclient.exit", "daemon.shimclient.kill_session",
		"daemon.sessionwatcher.link_fault", "daemon.sessionwatcher.watch_session",
		"daemon.sessionwatcher.watch_agent", "daemon.workspace.kill",
		// The adopted-death witness: the monitor can see the stream break
		// before the exit is decoded, so it says "shim link broke; redialing"
		// and then "redial stopped" the moment the death is evidence. Whether
		// it gets there first is a scheduling matter, so the pair is declared
		// rather than raced on.
		"daemon.shimclient.redial", "daemon.sessionwatcher.reopen",
	)
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
	return &fixture{d: d, repo: repo, ws: ws, t: t}
}

// newOpened starts a daemon, registers a repository, opens the workspace and
// waits for the fake shim's control socket.
func newOpened(t *testing.T, opts harness.Opts) *fixture {
	t.Helper()
	f := newRegistered(t, opts)
	f.open()
	// AN OPENED WORKSPACE HAS ALL THREE CONNECTIVITY HOPS UP (daemon.md
	// invariant 11): Emacs holds WatchHostWorkspace and the page holds
	// WatchWebWorkspace, and a workspace missing either is not connected
	// however healthy its shim link is. Holding both here is what makes this
	// fixture the OPENED workspace it claims to be rather than a half-open one
	// whose footer would read disconnected forever.
	f.host = f.d.WatchHost(f.ws)
	f.web = f.d.WatchWeb(f.ws)
	return f
}

// newOpenedWithProfile is newOpened for a fake shim that must be born with a
// profile in force — a condition the daemon can meet on its very first request
// or its very first idle sweep, which a control-socket script filed after the
// workspace is open would be racing.
func newOpenedWithProfile(t *testing.T, opts harness.Opts, profile harness.ShimProfile) *fixture {
	t.Helper()
	f := newRegistered(t, opts)
	f.d.WriteShimProfile(f.repo.Dir, profile)
	f.open()
	f.host = f.d.WatchHost(f.ws)
	f.web = f.d.WatchWeb(f.ws)
	return f
}

// newOpenedWorktree is newOpened for a workspace on a WORKTREE of its
// repository rather than on the repository root. It exists for the tests whose
// subject is destructive git — a workspace at the repository root is one whose
// removal takes the repository with it, which no later git command survives.
func newOpenedWorktree(t *testing.T, opts harness.Opts, name string) *fixture {
	t.Helper()
	d := harness.StartDaemon(t, opts)
	repo := harness.NewRepo(t)
	dir := worktreeOf(t, repo, name)
	ws := harness.Register(t, d, dir)
	f := &fixture{d: d, repo: repo, ws: ws, t: t}
	f.open()
	f.host = f.d.WatchHost(f.ws)
	f.web = f.d.WatchWeb(f.ws)
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

// answeringResponseFrame is the SETTLED response bubble a terminal's answer
// names. A real producer never names an answer it did not emit — the Completed
// arm points at a response block the same stream paid out — so a fake that
// pushes the terminal alone is not a shim, and the daemon is right to call that
// a turn whose answer did not land.
func answeringResponseFrame(agent string, answer *conversationv1.AgentActivityId, prose string) *conversationv1.AgentFrame {
	return activityFrame(agent, &conversationv1.AgentActivity{
		ActivityId: answer,
		Item: &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{
			Result: &conversationv1.AgentResponse_Success{Success: &conversationv1.AgentResponseSuccess{
				Prose: &conversationv1.AgentResponseProse{Markdown: prose},
			}},
		}},
	})
}

// pushConcludedTurn ends a turn the way a producer ends one: the answering
// response block first, then the terminal that names it. Every fixture that
// only needs a turn to END goes through it, so no fixture can leave the daemon
// looking at a conclusion whose answer resolves to nothing.
func pushConcludedTurn(shim *harness.ShimControl, agent, unit string) {
	shim.PushAgentFrame(agent, answeringResponseFrame(agent, activityID(unit), "done"))
	shim.PushAgentFrame(agent, successFrame(agent, activityID(unit)))
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

// awaitLiveWork waits until the workspace's watcher has recorded `want` live
// detached items, which is the daemon-side fact the interrupt verbs read.
func awaitLiveWork(t *testing.T, f *fixture, want int) {
	t.Helper()
	f.d.AwaitLogRecord(harness.WorkspaceLogPath(f.repo.Dir, "daemon"), "the live-work set", func(r harness.LogRecord) bool {
		if r.Operation != "daemon.sessionwatcher.live_work" {
			return false
		}
		total := 0
		for _, key := range []string{"agents", "shells", "monitors"} {
			if n, ok := r.Context[key].(float64); ok {
				total += int(n)
			}
		}
		return total == want
	})
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
	// ONE WAIT'S BOUND, NEVER THE RUN'S -- see harness.Daemon.WaitCtx.
	ctx, cancel := f.d.WaitCtx()
	defer cancel()
	return harness.AwaitView(t, ctx, s, what, pred)
}

// awaitShellHead answers a detached shell's HEAD row on the feed being tailed.
//
// THE SHELL BUBBLE IS A FEED, exactly as a subagent's is (feed.proto,
// FeedRow.shell_head): the HEAD carries the command, the clock and the state
// and rides the PARENT feed, while the spool is a separate BODY row
// (FeedRow.detached_shell) on the shell's own sub-feed, which the head's own
// FeedId addresses. `detached_shell` is therefore NEVER a top-level row, and a
// test that waits for one on the root feed waits until its bound expires.
func awaitShellHead(t *testing.T, f *fixture, s *harness.Stream[*frontendv1.FeedRow], what string) *frontendv1.FeedRow {
	t.Helper()
	return awaitRow(t, f, s, what, func(r *frontendv1.FeedRow) bool { return r.GetShellHead() != nil })
}

// awaitShellSpool answers the spool BODY row on the sub-feed a shell head
// addresses — the row that carries the output, wherever in that sub-feed's
// page or tail it turns up.
func awaitShellSpool(t *testing.T, f *fixture, head *frontendv1.FeedRow, what string, pred func(*frontendv1.FeedShell) bool) *frontendv1.FeedShell {
	t.Helper()
	row := f.awaitRowInFeed(head.GetId(), what, func(r *frontendv1.FeedRow) bool {
		return r.GetDetachedShell() != nil && pred(r.GetDetachedShell().GetShell())
	})
	return row.GetDetachedShell().GetShell()
}

// expectNoShellSpool asserts the shell bubble's sub-feed carries NO spool body
// at all — neither already in its page nor arriving on its tail.
func expectNoShellSpool(t *testing.T, f *fixture, head *frontendv1.FeedRow, what string) {
	t.Helper()
	page, token := f.openFeed(head.GetId())
	for _, r := range page.GetSuccess().GetRows() {
		if r.GetDetachedShell() != nil {
			t.Fatalf("the shell sub-feed's page carries a spool body %v, want none: %s", r.GetDetachedShell(), what)
		}
	}
	harness.ExpectNoPush(t, f.d.WatchFeed(token), harness.ProbeWindow, what)
}

// awaitFooter reads footer pushes until one satisfies the predicate.
func awaitFooter(t *testing.T, f *fixture, s *harness.Stream[*frontendv1.FooterView], what string, pred func(*frontendv1.FooterView) bool) *frontendv1.FooterView {
	t.Helper()
	ctx, cancel := f.d.WaitCtx()
	defer cancel()
	return harness.AwaitView(t, ctx, s, what, pred)
}

// awaitTopbar reads topbar pushes until one satisfies the predicate.
func awaitTopbar(t *testing.T, f *fixture, s *harness.Stream[*frontendv1.TopbarView], what string, pred func(*frontendv1.TopbarView) bool) *frontendv1.TopbarView {
	t.Helper()
	ctx, cancel := f.d.WaitCtx()
	defer cancel()
	return harness.AwaitView(t, ctx, s, what, pred)
}

// awaitRoster reads roster pushes until one satisfies the predicate.
func awaitRoster(t *testing.T, d *harness.Daemon, s *harness.Stream[*frontendv1.WorkspaceRoster], what string, pred func(*frontendv1.WorkspaceRoster) bool) *frontendv1.WorkspaceRoster {
	t.Helper()
	ctx, cancel := d.WaitCtx()
	defer cancel()
	return harness.AwaitView(t, ctx, s, what, pred)
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

// markViewed reports that the user has seen ws, as the editor's dwell does.
func markViewed(t *testing.T, d *harness.Daemon, ws *workspacev1.WorkspaceRef) {
	t.Helper()
	req := &agentreplv1.MarkWorkspaceViewedRequest{Workspace: ws}
	resp, err := d.Client().MarkWorkspaceViewed(d.Ctx(), connect.NewRequest(req))
	if err != nil {
		t.Fatalf("MarkWorkspaceViewed(%s) = error %v, want a success", ws.GetId(), err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("MarkWorkspaceViewed(%s) = %v, want a success", ws.GetId(), resp.Msg)
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
//
// THE MAIN AGENT OWNS IT, as the producer states: detached work is drawn only
// in its owner's feed, at its spawning call's row — so a fixture whose head
// must draw pushes that call first (pushDetachedShell).
func detachedShell(work, command string) *conversationv1.AgentDetachedWork {
	return &conversationv1.AgentDetachedWork{
		Work:  &conversationv1.DetachedWorkId{Value: work},
		Owner: &conversationv1.AgentId{Value: mainAgent},
		Kind:  bashKind(),
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

// movedShell announces that an in-turn shell unit's work LEFT for the
// background. The handle IS the spawning call's own id (ruling, landing 3), so
// one identity addresses the work and the unit it moved out of.
func movedShell(unit string) *conversationv1.AgentDetachedWork {
	return &conversationv1.AgentDetachedWork{
		Work: &conversationv1.DetachedWorkId{Value: unit},
		Kind: bashKind(),
		Origin: &conversationv1.AgentDetachedWork_Detached{Detached: &conversationv1.DetachedWorkDetached{
			DetachedFromId: &conversationv1.AgentActivityId{Value: unit},
			Cause: &conversationv1.DetachedWorkDetached_TimedOut{TimedOut: &conversationv1.DetachedCauseTimedOut{
				TimeoutMs: 120_000,
			}},
		}},
	}
}

// detachedSubagent announces a detached subagent unit.
//
// The main agent spawned it, as the producer states.
func detachedSubagent(work, agent, label string) *conversationv1.AgentDetachedWork {
	return &conversationv1.AgentDetachedWork{
		Work:  &conversationv1.DetachedWorkId{Value: work},
		Owner: &conversationv1.AgentId{Value: mainAgent},
		Kind:  subagentKind(agent),
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

// subagentKind and bashKind are the kinds a producer states on an
// announcement; a subagent's names the agent that is running.
func subagentKind(agent string) *conversationv1.DetachedWorkKind {
	return &conversationv1.DetachedWorkKind{Kind: &conversationv1.DetachedWorkKind_Subagent{
		Subagent: &conversationv1.DetachedWorkKindSubagent{AgentId: &conversationv1.AgentId{Value: agent}},
	}}
}

func bashKind() *conversationv1.DetachedWorkKind {
	return &conversationv1.DetachedWorkKind{Kind: &conversationv1.DetachedWorkKind_Bash{Bash: &conversationv1.DetachedWorkKindBash{}}}
}

// resumedSubagent announces a subagent RESUMED BY SENDMESSAGE: detached from
// the send's own unit, running the agent its original spawn created.
func resumedSubagent(work, sendUnit, agent string) *conversationv1.AgentDetachedWork {
	return &conversationv1.AgentDetachedWork{
		Work:  &conversationv1.DetachedWorkId{Value: work},
		Owner: &conversationv1.AgentId{Value: mainAgent},
		Kind:  subagentKind(agent),
		Origin: &conversationv1.AgentDetachedWork_Detached{Detached: &conversationv1.DetachedWorkDetached{
			DetachedFromId: &conversationv1.AgentActivityId{Value: sendUnit},
			Cause:          &conversationv1.DetachedWorkDetached_Requested{Requested: &conversationv1.DetachedCauseRequested{}},
		}},
	}
}

func strPtr(s string) *string { return &s }

// shellCallFrame is the main agent's Bash call that a detached shell's work
// left: the card its head is drawn in place of.
func shellCallFrame(work, command string) *conversationv1.AgentFrame {
	return activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID(work),
		Item: &conversationv1.AgentActivity_Bash{Bash: &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Start{
			Start: &conversationv1.AgentBashStart{
				Command:   &conversationv1.AgentBashCommand{Line: command},
				StartedAt: startedAt(1_700_000_000_000),
			},
		}}},
	})
}

// pushDetachedShell pushes a main-agent shell the way a producer states one:
// the call that launched it, then the announcement that its work left.
func pushDetachedShell(shim *harness.ShimControl, work, command string) {
	shim.PushAgentFrame(mainAgent, shellCallFrame(work, command))
	shim.PushAgentFrame(mainAgent, detachedWorkFrame(mainAgent, detachedShell(work, command)))
}
