//go:build integration

package integration

import (
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/integration/harness"
)

// isMergeHead reports whether a root-feed row is a merge bubble's head.
func isMergeHead(r *frontendv1.FeedRow) bool {
	return r.GetActivity().GetMerge().GetHead() != nil
}

// TestTheMergeBubbleIsPlacedOnlyOnceTheWorkspaceIsFreeAndStaysLast pins the
// owner's invariant (2026-10-08): "while merging, the merge bubble is
// necessarily the last thing in the feed, because things only land outside of
// it after the merge has finished". A merge asked for while a background
// subagent runs waits for the workspace to fall free; a turn the vendor starts
// meanwhile draws its rows BEFORE any bubble exists, the bubble is placed only
// once the subagent settles, and no root row lands after it until the merge
// has landed.
func TestTheMergeBubbleIsPlacedOnlyOnceTheWorkspaceIsFreeAndStaysLast(t *testing.T) {
	t.Parallel()
	// Arrange: a running turn with a live background subagent, the root feed
	// tailed before anything is asked.
	f, d, _, script := mergeCleanRepo(t)
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")
	workspaceLog := harness.WorkspaceLogPath(f.ws.GetDir(), "daemon")
	tail := f.watchRootFeed()
	var seen []*frontendv1.FeedRow
	collect := func(pred func(*frontendv1.FeedRow) bool) func(*frontendv1.FeedRow) bool {
		return func(r *frontendv1.FeedRow) bool {
			seen = append(seen, r)
			return pred(r)
		}
	}
	f.submit("keep going", "k-placement", origin)
	f.shim.ExpectStartTurn()
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, ftSubagentSpawn("toolu-1", "toolu-1", "sweep the tree")))
	f.shim.PushAgentFrame(mainAgent, detachedWorkFrame(mainAgent, movedSubagent("toolu-1")))
	d.AwaitLogRecord(workspaceLog, "the live subagent", func(r harness.LogRecord) bool {
		agents, ok := r.Context["agents"].(float64)
		return r.Operation == "daemon.sessionwatcher.live_work" && ok && agents == 1
	})
	harness.CommitWork(t, f.ws.GetDir())
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: f.ws, Source: harness.OwnBranch(true)})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}
	f.shim.ExpectKillTurn()
	d.AwaitLogRecord(workspaceLog, "the merge waiting for the workspace to fall free", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.merge.await_free" && strings.HasPrefix(r.Message, "the merge waits")
	})

	// Act: the vendor starts a turn of its own while the merge waits, and
	// answers in it; then the subagent settles and the workspace falls free.
	const vendorTurn = "turn-woken-by-the-subagent"
	f.shim.PushUserPrompt(mainAgent, &conversationv1.AgentPrompt{
		Id:     &conversationv1.TurnId{Value: vendorTurn},
		Agent:  &conversationv1.AgentId{Value: mainAgent},
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_VENDOR_STARTED,
	})
	f.shim.PushAgentFrameIn(mainAgent, vendorTurn, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("resp-while-waiting"),
		Item: &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{Result: &conversationv1.AgentResponse_Success{
			Success: &conversationv1.AgentResponseSuccess{Prose: &conversationv1.AgentResponseProse{Markdown: "the sweep is half done"}},
		}}},
	}))
	waiting := awaitRow(t, f, tail, "the woken turn's response", collect(func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetResponse().GetSuccess() != nil
	}))
	f.shim.PushAgentFrameIn(mainAgent, vendorTurn, successFrame(mainAgent, activityID("resp-while-waiting")))
	headDrawnWhileWaiting := false
	for _, r := range seen {
		headDrawnWhileWaiting = headDrawnWhileWaiting || isMergeHead(r)
	}
	f.shim.PushAgentFrame("toolu-1", activityFrame("toolu-1", ftSubagentSettled("sub-unit-9", "toolu-1")))
	awaitRow(t, f, tail, "the merge bubble's landed head", collect(func(r *frontendv1.FeedRow) bool {
		return isMergeHead(r) && r.GetActivity().GetMerge().GetSuccess() != nil
	}))

	// Assert: no bubble while the merge waited; the bubble follows the woken
	// turn's rows; nothing landed after it while the merge ran.
	if headDrawnWhileWaiting {
		t.Fatal("a merge bubble was drawn while the merge waited for the workspace to fall free")
	}
	var head *frontendv1.FeedRow
	for _, r := range seen {
		if isMergeHead(r) {
			head = r
			break
		}
	}
	if head == nil {
		t.Fatal("the merge drew no bubble")
	}
	placed := head.GetOrder().GetKey()
	if waiting.GetOrder().GetKey() >= placed {
		t.Fatalf("the woken turn's row (order %q) sorts after the merge bubble (order %q), want it before", waiting.GetOrder().GetKey(), placed)
	}
	started := false
	for _, r := range seen {
		if r == head {
			started = true
			continue
		}
		if started && !isMergeHead(r) && r.GetOrder().GetKey() > placed {
			t.Fatalf("row %v (order %q) landed after the merge bubble (order %q) while the merge ran", r.GetId(), r.GetOrder().GetKey(), placed)
		}
	}
}
