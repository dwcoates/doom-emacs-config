package merge

import (
	"path/filepath"
	"slices"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	repoconfigv1 "agentrepl/proto/repoconfig/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/lifecycleconfig"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/wsm"
)

// finishScript is the finish-stage script hook the tests' configs name.
const finishScript = "bin/open-pr.sh"

// theThread is the Slack message an emoji-ack harness workspace was created
// for.
const theThread = "https://acme.slack.com/archives/C1/p1730000000123456"

// finishHooks makes the harness's repository's config a hooks-only deploy
// whose user_or_workspace finish stage is ENTRIES.
func (h *harness) finishHooks(entries ...*repoconfigv1.HookEntry) {
	h.mu.Lock()
	defer h.mu.Unlock()
	h.config = &repoconfigv1.LifecycleConfig{PreDeploy: &repoconfigv1.DeployPreDeployStage{}, Finish: usersList(entries...)}
}

// finishPrompts answers the submissions the finish stage made, in order.
func (h *harness) finishPrompts() []string {
	h.queue.mu.Lock()
	defer h.queue.mu.Unlock()
	var out []string
	for _, sub := range h.queue.submissions {
		if sub.Origin == conversationv1.PromptOrigin_PROMPT_ORIGIN_FINISH_HOOK {
			out = append(out, sub.Said.GetContent().GetBlocks()[0].GetText().GetText())
		}
	}
	return out
}

func TestAFinishPromptRunsBeforeTheDeployIsPutInLine(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.finishHooks(anyTarget(promptHookOf("open the pull request")))
	h.inFlight = "turn-asking"
	if err := h.request(t, ownBranch, RequestedByAgent); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Act
	h.runWait(t)

	// Assert
	if got := h.finishPrompts(); !slices.Equal(got, []string{"open the pull request"}) {
		t.Fatalf("finish prompts = %v, want the configured one", got)
	}
	if state, _ := h.entryState(); state != wsm.MergeQueued {
		t.Fatalf("queue entry state = %v, want queued once the finish stage succeeded", state)
	}
	if len(h.awaitedTurns) != 2 || h.awaitedTurns[0] != "turn-asking" {
		t.Fatalf("awaited turns = %v, want the requesting turn, then the finish prompt's", h.awaitedTurns)
	}
	if _, found := h.db.finish[theWorkspace]; found {
		t.Fatal("the finish progress stands after the stage succeeded")
	}
}

func TestTheDeployIsNotInLineWhileItsFinishPromptRuns(t *testing.T) {
	// Arrange: the finish prompt's turn is observed while it runs.
	h := newHarness(t)
	h.finishHooks(anyTarget(promptHookOf("open the pull request")))
	if err := h.request(t, ownBranch, RequestedByAgent); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	var stateWhileRunning wsm.MergeQueueState
	h.queue.onSubmit = func(promptqueue.Submission) { stateWhileRunning, _ = h.entryState() }

	// Act
	h.runWait(t)

	// Assert
	if stateWhileRunning != wsm.MergeRequested {
		t.Fatalf("queue entry state while the finish prompt ran = %v, want only requested", stateWhileRunning)
	}
}

func TestAFailedFinishPromptWithdrawsTheDeployAndHandsItBack(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.finishHooks(anyTarget(promptHookOf("open the pull request")))
	h.turnCloses = []wsm.TurnClose{wsm.CloseFailed}
	if err := h.request(t, ownBranch, RequestedByAgent); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Act
	h.runWait(t)

	// Assert
	if _, queued := h.entryState(); queued {
		t.Fatal("the deploy stands in the queue after its finish stage failed")
	}
	sent := h.detached.sent()
	if len(sent) != 1 || sent[0].outcome.Kind != DetachedFailed || !strings.Contains(sent[0].outcome.Summary, "its finish prompt hook did not complete") {
		t.Fatalf("handbacks = %+v, want the failure handed back to the workspace", sent)
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opFinishStage, Message: "withdrew the deploy whose finish stage failed; the workspace is told"}); !ok {
		t.Fatalf("no withdrawal record: %+v", h.logs.Records())
	}
	if _, found := h.db.finish[theWorkspace]; found {
		t.Fatal("the finish progress stands after the deploy was withdrawn")
	}
}

func TestAFailedFinishScriptWithdrawsTheDeployNamingItsLog(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.finishHooks(anyTarget(scriptHookOf(finishScript)))
	h.hooks.exits = map[string]int{filepath.Join(h.targetD, finishScript): 3}
	if err := h.request(t, ownBranch, RequestedByAgent); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Act
	h.runWait(t)

	// Assert
	if _, queued := h.entryState(); queued {
		t.Fatal("the deploy stands in the queue after its finish script failed")
	}
	sent := h.detached.sent()
	if len(sent) != 1 || !strings.Contains(sent[0].outcome.Summary, "its finish script "+finishScript+" failed: it exited 3") ||
		!strings.Contains(sent[0].outcome.Summary, LogsDir(h.stateDir)) {
		t.Fatalf("handbacks = %+v, want the script, its exit and its log named", sent)
	}
	record, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opFinishStage, Message: "a finish script hook failed; the deploy is withdrawn"})
	if !ok || record.Context["script"] != finishScript || record.Context["failure"] != "it exited 3" {
		t.Fatalf("failure record = %+v (found %v), want the script and how it failed", record, ok)
	}
}

func TestAFinishScriptRunsWithTheFinishEnvironment(t *testing.T) {
	// Arrange: an emoji-ack workspace, whose script sees its thread.
	h := newHarness(t)
	h.finishHooks()
	h.mu.Lock()
	h.config.Finish = &repoconfigv1.LifecycleStageHooks{EmojiAck: &repoconfigv1.LifecycleHookList{Hooks: []*repoconfigv1.HookEntry{anyTarget(scriptHookOf(finishScript))}}}
	h.mu.Unlock()
	h.db.setOriginator(theWorkspace, "emoji_ack", theThread)
	if err := h.request(t, ownBranch, RequestedByAgent); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Act
	h.runWait(t)

	// Assert
	runs := h.hooks.ran()
	if len(runs) != 1 || runs[0].dir != h.targetD || !slices.Equal(runs[0].argv, []string{"bash", h.scriptAt(finishScript)}) {
		t.Fatalf("script runs = %+v, want the finish script from the repository root", runs)
	}
	for _, want := range []string{EnvCallingWorkspace + "=" + h.sourceD, lifecycleconfig.EnvHookStage + "=finish", lifecycleconfig.EnvSlackThread + "=" + theThread} {
		if !slices.Contains(runs[0].env, want) {
			t.Errorf("env = %v, want %s", runs[0].env, want)
		}
	}
	if state, _ := h.entryState(); state != wsm.MergeQueued {
		t.Fatalf("queue entry state = %v, want queued", state)
	}
}

func TestAnEmojiAckWorkspaceRunsTheDefaultFinishPromptWithItsThread(t *testing.T) {
	// Arrange: no finish stage configured at all.
	h := newHarness(t)
	h.finishHooks()
	h.mu.Lock()
	h.config.Finish = nil
	h.mu.Unlock()
	h.db.setOriginator(theWorkspace, "emoji_ack", theThread)
	if err := h.request(t, ownBranch, RequestedByAgent); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Act
	h.runWait(t)

	// Assert
	want, _ := lifecycleconfig.ExpandPrompt(lifecycleconfig.DefaultFinishPrompt, theThread)
	if got := h.finishPrompts(); !slices.Equal(got, []string{want}) {
		t.Fatalf("finish prompts = %v, want the default naming the thread", got)
	}
}

func TestAUserOrWorkspaceWorkspaceRunsNoDefaultFinishPrompt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.finishHooks()
	h.mu.Lock()
	h.config.Finish = nil
	h.mu.Unlock()
	if err := h.request(t, ownBranch, RequestedByAgent); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Act
	h.runWait(t)

	// Assert
	if got := h.finishPrompts(); len(got) != 0 {
		t.Fatalf("finish prompts = %v, want none", got)
	}
	if state, _ := h.entryState(); state != wsm.MergeQueued {
		t.Fatalf("queue entry state = %v, want queued", state)
	}
}

func TestAFinishPromptNamingTheThreadOfAWorkspaceWithNoneWithdrawsTheDeploy(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.finishHooks(anyTarget(promptHookOf("reply in {{slack_thread}}")))
	if err := h.request(t, ownBranch, RequestedByAgent); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}

	// Act
	h.runWait(t)

	// Assert
	if got := h.finishPrompts(); len(got) != 0 {
		t.Fatalf("finish prompts = %v, want none sent", got)
	}
	if _, queued := h.entryState(); queued {
		t.Fatal("the deploy stands in the queue")
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opFinishStage}); !ok {
		t.Fatalf("no ERROR record of the unresolvable placeholder: %+v", h.logs.Records())
	}
}

func TestAFinishStageResumesAtItsRecordedHookAndReattachesToItsTurn(t *testing.T) {
	// Arrange: a restart left the stage at its second hook, whose prompt's
	// turn ended while the daemon was down.
	h := newHarness(t)
	h.finishHooks(anyTarget(scriptHookOf(finishScript)), anyTarget(promptHookOf("announce the pull request")))
	if err := h.request(t, ownBranch, RequestedByAgent); err != nil {
		t.Fatalf("Enqueue: %v", err)
	}
	h.db.finish[theWorkspace] = wsm.FinishProgress{HookIndex: 1, Turn: "turn-finish"}
	h.recordClosedTurn(theWorkspace, "turn-finish", wsm.CloseCompleted)

	// Act
	h.runWait(t)

	// Assert
	if runs := h.hooks.ran(); len(runs) != 0 {
		t.Fatalf("script runs = %+v, want none: the script before the recorded hook succeeded", runs)
	}
	if got := h.finishPrompts(); len(got) != 0 {
		t.Fatalf("finish prompts = %v, want none: the recorded turn is reattached to, never sent again", got)
	}
	if state, _ := h.entryState(); state != wsm.MergeQueued {
		t.Fatalf("queue entry state = %v, want queued", state)
	}
}

func TestAUsersFinishWithFinishHooksWaitsForThem(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.finishHooks(anyTarget(promptHookOf("open the pull request")))

	// Act
	enqueue(t, h)

	// Assert
	if state, _ := h.entryState(); state != wsm.MergeRequested {
		t.Fatalf("queue entry state = %v, want requested until the finish stage runs", state)
	}
	h.runWait(t)
	if state, _ := h.entryState(); state != wsm.MergeQueued || len(h.finishPrompts()) != 1 {
		t.Fatalf("after the wait: state %v, finish prompts %v; want queued after one finish prompt", state, h.finishPrompts())
	}
}

func TestAnAsyncMergeRunsNoFinishStage(t *testing.T) {
	// Arrange: the repository's kind has finish hooks, which an async merge --
	// its merge workspace's own merge -- never runs.
	h := newHarness(t)
	h.finishHooks(anyTarget(promptHookOf("open the pull request")))
	h.register(theMergeWorkspace, "reconnect-fix")
	h.db.mu.Lock()
	merge := h.db.workspaces[theMergeWorkspace]
	merge.MergeOf = &wsm.MergeOrigin{Requester: theWorkspace}
	h.db.workspaces[theMergeWorkspace] = merge
	h.db.mu.Unlock()

	// Act
	ft, err := h.o.finishTargetOf(t.Context(), theMergeWorkspace, wsm.MergeSource{Kind: wsm.MergeSourceOwnBranch})

	// Assert
	if err != nil || len(ft.hooks) != 0 {
		t.Fatalf("finishTargetOf = %+v, %v; want no hooks for an async merge", ft, err)
	}
}

// recordClosedTurn records TURN of WS as having ended HOW, as the session
// watcher does when a turn closes.
func (h *harness) recordClosedTurn(ws wsm.WorkspaceID, turn wsm.TurnID, how wsm.TurnClose) {
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	at := h.clock()
	h.db.turns[ws] = append(h.db.turns[ws], wsm.Turn{ID: turn, Workspace: ws, Close: &how, ClosedAt: &at})
}
