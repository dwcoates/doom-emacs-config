//go:build integration

package integration

import (
	"fmt"
	"sync"
	"testing"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// THE CONCURRENCY CASES. A run recorded for test impact selection runs one
// top-level test at a time per daemon (testrun/internal/impact), so tests no
// longer overlap inside the shared daemon, and the overlap that once
// surfaced real bugs (the feed fan-out, the PutTurn collision, the
// per-workspace in-flight accounting) has to be made INSIDE a test: each case
// here drives several workspaces of one daemon at once, through one view, so
// the daemon serves them concurrently whatever else the process runs.

// workspacesOnOneDaemon registers and opens n workspaces on the canonical
// daemon through one view, each with its own repository and fake shim.
func workspacesOnOneDaemon(t *testing.T, n int) []*fixture {
	t.Helper()
	fixtures := make([]*fixture, n)
	for i := range fixtures {
		f := newCanonicalRegistered(t)
		f.openWatched()
		fixtures[i] = f
	}
	return fixtures
}

// answerMarkdown is a settled response row's prose.
func answerMarkdown(r *frontendv1.FeedRow) string {
	return r.GetActivity().GetResponse().GetSuccess().GetProse().GetProse().GetSource().GetMarkdown()
}

// concludeWith ends the workspace's running turn with an answer of its own.
func concludeWith(f *fixture, unit, markdown string) {
	f.shim.PushAgentFrame(mainAgent, answeringResponseFrame(mainAgent, activityID(unit), markdown))
	f.shim.PushAgentFrame(mainAgent, successFrame(mainAgent, activityID(unit)))
}

func TestSeveralWorkspacesRunTurnsAtOnceAndEachFeedHoldsOnlyItsOwnAnswer(t *testing.T) {
	t.Parallel()
	// Arrange: four workspaces of one daemon, each with a turn running.
	const n = 4
	fs := workspacesOnOneDaemon(t, n)
	for i, f := range fs {
		f.submit(fmt.Sprintf("work %d", i), fmt.Sprintf("k-run-%d", i), origin)
	}
	for _, f := range fs {
		f.shim.ExpectStartTurn()
	}

	// Act: the turns end in the reverse of the order they began, each with an
	// answer only its workspace should ever show.
	answers := make([]string, n)
	for i := n - 1; i >= 0; i-- {
		answers[i] = fmt.Sprintf("answer of workspace %d", i)
		concludeWith(fs[i], fmt.Sprintf("unit-%d", i), answers[i])
	}

	// Assert: every workspace's feed carries its own answer and no other's.
	for i, f := range fs {
		page, _ := f.openFeedOnceCarrying(answers[i], func(p *frontendv1.FeedPage) bool {
			for _, r := range p.GetSuccess().GetRows() {
				if answerMarkdown(r) == answers[i] {
					return true
				}
			}
			return false
		})
		for j, other := range answers {
			if j == i {
				continue
			}
			for _, r := range page.GetSuccess().GetRows() {
				if answerMarkdown(r) == other {
					t.Fatalf("workspace %d's feed holds workspace %d's answer %q", i, j, other)
				}
			}
		}
	}
}

func TestConcurrentSubmitsOfOneKeyToDifferentWorkspacesEachStartTheirOwnTurn(t *testing.T) {
	t.Parallel()
	// Arrange: workspaces that will all be sent the same idempotency key at
	// once. A key is unique to its workspace, and a turn id is unique to the
	// daemon, so no two of these may collide on the turn record.
	const n = 4
	fs := workspacesOnOneDaemon(t, n)
	type outcome struct {
		resp *connect.Response[agentreplv1.SubmitPromptResponse]
		err  error
	}
	outcomes := make([]outcome, n)

	// Act: every submit starts together.
	var ready, done sync.WaitGroup
	start := make(chan struct{})
	ready.Add(n)
	done.Add(n)
	for i, f := range fs {
		go func() {
			defer done.Done()
			ready.Done()
			<-start
			outcomes[i].resp, outcomes[i].err = f.d.Client().SubmitPrompt(f.d.Ctx(), connect.NewRequest(&agentreplv1.SubmitPromptRequest{
				Workspace: f.ws, Said: said("the same words"), IdempotencyKey: "k-shared-by-all", Origin: origin,
			}))
		}()
	}
	ready.Wait()
	close(start)
	done.Wait()

	// Assert: each was accepted as a turn of its own, and each shim was told
	// to start exactly that turn.
	seen := map[string]int{}
	for i, f := range fs {
		if outcomes[i].err != nil {
			t.Fatalf("workspace %d: SubmitPrompt = error %v, want an answer", i, outcomes[i].err)
		}
		turn := outcomes[i].resp.Msg.GetSuccess().GetTurn().GetTurn()
		if turn.GetValue() == "" {
			t.Fatalf("workspace %d: SubmitPrompt = %v, want an accepted turn", i, outcomes[i].resp.Msg)
		}
		if prior, dup := seen[turn.GetValue()]; dup {
			t.Fatalf("workspaces %d and %d were both given turn %q", prior, i, turn.GetValue())
		}
		seen[turn.GetValue()] = i
		if got := f.shim.ExpectStartTurn().GetTurn().GetValue(); got != turn.GetValue() {
			t.Fatalf("workspace %d's shim was told to start %q, want its own turn %q", i, got, turn.GetValue())
		}
	}
}

func TestAWorkspacesHeldPromptWaitsForItsOwnTurnNotAnotherWorkspacesEnd(t *testing.T) {
	t.Parallel()
	// Arrange: two workspaces with a turn running each, and a prompt held
	// behind the first one's.
	fs := workspacesOnOneDaemon(t, 2)
	busy, other := fs[0], fs[1]
	busy.submit("running in the first", "k-busy", origin)
	other.submit("running in the second", "k-other", origin)
	busy.shim.ExpectStartTurn()
	other.shim.ExpectStartTurn()
	held := busy.submit("held behind the first's turn", "k-held", origin).GetSuccess().GetTurn().GetTurn()
	awaitView(t, busy, busy.d.WatchHolds(busy.ws), "the prompt held", func(tray *frontendv1.FeedHeld) bool {
		return promptHeldEntry(tray, held) != nil
	})

	// Act: only the OTHER workspace's turn ends, and its end is seen.
	concludeWith(other, "unit-other", "the second is done")
	other.awaitRowInFeed(nil, "the second workspace's answer", func(r *frontendv1.FeedRow) bool {
		return answerMarkdown(r) == "the second is done"
	})

	// Assert: the first workspace's prompt is held still: a fresh view of its
	// tray, whose first answer is the daemon's current state, holds it.
	awaitView(t, busy, busy.d.WatchHolds(busy.ws), "the prompt still held after the other workspace's turn ended", func(tray *frontendv1.FeedHeld) bool {
		return promptHeldEntry(tray, held) != nil
	})

	// Act: its own turn ends.
	concludeWith(busy, "unit-busy", "the first is done")

	// Assert: now the held prompt is delivered, once.
	start, count := busy.shim.ExpectStartTurnWithCount()
	if start.GetTurn().GetValue() != held.GetValue() || count != 2 {
		t.Fatalf("the first workspace's next StartTurn = %q after %d, want the held prompt %q as its second", start.GetTurn().GetValue(), count, held.GetValue())
	}
}
