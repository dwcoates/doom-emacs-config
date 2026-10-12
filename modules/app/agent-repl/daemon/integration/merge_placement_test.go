//go:build integration

package integration

import (
	"strings"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/integration/harness"
)

// isMergeHead reports whether a root-feed row is a merge bubble's head.
func isMergeHead(r *frontendv1.FeedRow) bool {
	return r.GetActivity().GetMerge().GetHead() != nil
}

// TestTheMergeBubbleIsPinnedLastFromItsQueueingAndSettlesWhereItLands pins the
// owner's invariant (2026-10-10): the merge bubble appears the moment the
// merge is queued and stays at the bottom of the feed until the merge
// resolves, then stays where it resolved. A merge asked for while a background
// subagent runs waits for the workspace to fall free; its bubble is drawn
// PINNED while it waits, a turn the vendor starts meanwhile draws its rows
// ABOVE the pinned bubble, and the landed head settles at a key after every
// row drawn before it.
func TestTheMergeBubbleIsPinnedLastFromItsQueueingAndSettlesWhereItLands(t *testing.T) {
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

	// Assert: the bubble was drawn pinned while the merge waited; the woken
	// turn's row sorts above it; the landed head settled after every row drawn
	// before it.
	if !headDrawnWhileWaiting {
		t.Fatal("no merge bubble was drawn while the merge waited for the workspace to fall free")
	}
	var landed *frontendv1.FeedRow
	for _, r := range seen {
		if !isMergeHead(r) {
			continue
		}
		if r.GetActivity().GetMerge().GetSuccess() != nil {
			landed = r
			continue
		}
		if r.GetOrder().GetPinned() == nil {
			t.Fatalf("a live merge head was drawn unpinned (order %v), want it pinned until the merge resolves", r.GetOrder())
		}
		if waiting.GetOrder().GetKey() >= r.GetOrder().GetKey() {
			t.Fatalf("the woken turn's row (order %q) sorts after the pinned bubble (order %q), want it above", waiting.GetOrder().GetKey(), r.GetOrder().GetKey())
		}
	}
	if landed == nil {
		t.Fatal("the merge drew no landed head")
	}
	settled := landed.GetOrder()
	if settled.GetPinned() != nil {
		t.Fatalf("the landed head's order = %v, want it settled (unpinned)", settled)
	}
	for _, r := range seen {
		if r == landed || isMergeHead(r) {
			continue
		}
		if r.GetOrder().GetKey() >= settled.GetKey() {
			t.Fatalf("row %v (order %q) sorts at or after the settled bubble (order %q), want every row drawn before the settle above it", r.GetId(), r.GetOrder().GetKey(), settled.GetKey())
		}
	}
}
