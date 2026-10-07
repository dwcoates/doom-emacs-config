// subagents_e2e_test.go — SPEC.md section C, "Subagents — sync / detached /
// nested" (§C #27-30). Drives the real Agent-tool scenarios in
// agent-shim/claude/shim/src/fake/scenarios/subagents.ts through the real
// shim, asserting on the daemon's own Connect API (OpenFeed/WatchFeed) —
// never on hand-written store facts or scripted routing events. Per the
// dispatch note: subagent coverage ranked HIGH in the coverage report
// because the deleted suite hand-wrote subagent routing events and carried a
// stale comment claiming "the fake engine has no subagents" — it does, and
// every test below drives it for real.
//
// Contract grounding (docs/overhaul/daemon.md, "Identity, as the daemon
// lives it" / PROTO-CHANGES.md Landing 3): "AgentId minting rule (proto
// comment on AgentId): main = original vendor session id; subagent = the
// spawning call's tool_use_id." That rule is comment-only on
// conversation.v1.AgentId and stops at the daemon — the frontend wire never
// exposes it. What IS observable from here is its behavioral consequence,
// per frontend/v1/feed.proto's own header ("a subagent bubble IS a feed"):
// a subagent's own activity is attributed to ONE collapsed bubble row on the
// parent feed (FeedTurnActivity.subagent / FeedRow.detached_subagent), and
// its nested content (its own prompt, its own tool calls, its own prose)
// lives ONLY on the sub-feed addressed by that row's own FeedId
// (OpenFeed(feed=<that id>)), never inlined on the parent feed. That
// containment is what #27-29 assert.
package e2e

import (
	"context"
	"strings"
	"testing"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"connectrpc.com/connect"

	"claude-repld/integration/harness"
)

// ---------------------------------------------------------------------------
// Small helpers, local to this file. They read the daemon's feed surface the
// same way a real client would (OpenFeed for a page + watch token, WatchFeed
// to tail it) — no store reads, no hand-authored rows.
// ---------------------------------------------------------------------------

// openFeed opens the workspace's root feed (feed == nil) or a bubble's own
// sub-feed (feed != nil), answering the newest page and the token WatchFeed
// echoes.
func openFeed(t *testing.T, w *World, ws *workspacev1.WorkspaceRef, feed *frontendv1.FeedId) (*frontendv1.FeedPage, *agentreplv1.FeedWatchToken) {
	t.Helper()
	resp, err := w.Client().OpenFeed(w.Ctx(), connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ws, Feed: feed}))
	if err != nil {
		t.Fatalf("OpenFeed(feed=%v): %v", feed, err)
	}
	success := resp.Msg.GetSuccess()
	if success == nil {
		t.Fatalf("OpenFeed(feed=%v) = %v, want success", feed, resp.Msg)
	}
	return success.GetPage(), success.GetWatch()
}

// findRow answers the first row in rows satisfying pred, or nil.
func findRow(rows []*frontendv1.FeedRow, pred func(*frontendv1.FeedRow) bool) *frontendv1.FeedRow {
	for _, row := range rows {
		if pred(row) {
			return row
		}
	}
	return nil
}

// subagentBubble answers the row's FeedSubagent head, whichever placement
// arm carries it (sync: FeedTurnActivity.subagent; detached:
// FeedRow.detached_subagent.subagent) — SPEC.md B / feed.proto: "the same
// drawn component the detached wrapper carries; a bubble is a sub-feed
// either way."
func subagentBubble(row *frontendv1.FeedRow) *frontendv1.FeedSubagent {
	if s := row.GetActivity().GetSubagent(); s != nil {
		return s
	}
	return row.GetDetachedSubagent().GetSubagent()
}

// responseMarkdown answers the prose an activity row's response unit carries
// in whichever result arm is set (arriving, settled, or broken — a fake-SDK
// scenario with no streamed deltas may settle straight to success), and
// whether the row was a response unit at all.
func responseMarkdown(a *frontendv1.FeedTurnActivity) (string, bool) {
	resp := a.GetResponse()
	if resp == nil {
		return "", false
	}
	switch {
	case resp.GetUpdate() != nil:
		return resp.GetUpdate().GetProse().GetMarkdown(), true
	case resp.GetSuccess() != nil:
		return resp.GetSuccess().GetProse().GetMarkdown(), true
	case resp.GetError() != nil:
		return resp.GetError().GetProse().GetMarkdown(), true
	default:
		return "", true
	}
}

// pageHasResponseText reports whether any row in rows is a response unit
// whose prose equals text exactly.
func pageHasResponseText(rows []*frontendv1.FeedRow, text string) bool {
	return findRow(rows, func(row *frontendv1.FeedRow) bool {
		md, ok := responseMarkdown(row.GetActivity())
		return ok && md == text
	}) != nil
}

// awaitResponseText waits (event-driven, bounded by DefaultTimeout) for a
// row carrying exactly this response text to arrive on the already-open
// page+stream, checking the page first (the row may already have landed
// before the watch was opened).
func awaitResponseText(t *testing.T, w *World, page *frontendv1.FeedPage, stream *harness.Stream[*frontendv1.FeedRow], text string) *frontendv1.FeedRow {
	t.Helper()
	if row := findRow(page.GetSuccess().GetRows(), func(row *frontendv1.FeedRow) bool {
		md, ok := responseMarkdown(row.GetActivity())
		return ok && md == text
	}); row != nil {
		return row
	}
	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	return harness.AwaitView(t, ctx, stream, "response text "+text, func(row *frontendv1.FeedRow) bool {
		md, ok := responseMarkdown(row.GetActivity())
		return ok && md == text
	})
}

// awaitSubagentSettled waits for the row identified by id to reach a
// SETTLED state on the given already-open watch stream, checking the
// already-fetched page first (the settle may have already landed before the
// watch was opened, which is the ordinary case for a SYNC subagent that
// finishes inside the same turn).
func awaitSubagentSettled(t *testing.T, w *World, page *frontendv1.FeedPage, stream *harness.Stream[*frontendv1.FeedRow], id *frontendv1.FeedId) *frontendv1.FeedSubagent {
	t.Helper()
	settled := func(row *frontendv1.FeedRow) *frontendv1.FeedSubagent {
		if row.GetId().GetValue() != id.GetValue() {
			return nil
		}
		if b := subagentBubble(row); b != nil && b.GetSettled() != nil {
			return b
		}
		return nil
	}
	for _, row := range page.GetSuccess().GetRows() {
		if b := settled(row); b != nil {
			return b
		}
	}
	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	var found *frontendv1.FeedSubagent
	harness.AwaitView(t, ctx, stream, "subagent "+id.GetValue()+" to settle", func(row *frontendv1.FeedRow) bool {
		if b := settled(row); b != nil {
			found = b
			return true
		}
		return false
	})
	return found
}

// assertNoResponseTextEver drains stream for probe (bounded, a designed
// negative-assertion window per harness.ExpectNoPush's own discipline — this
// suite never sleeps to synchronize, but a claim of ABSENCE necessarily
// waits out a bound rather than an event) failing if any pushed row is a
// response unit whose prose equals text. Pushes unrelated to text (e.g. the
// bubble's own live-progress heartbeat) are drained and ignored — the
// assertion is about this ONE text never reaching the feed the stream is
// open on, not about the feed being silent.
func assertNoResponseTextEver(t *testing.T, stream *harness.Stream[*frontendv1.FeedRow], probe time.Duration, text string) {
	t.Helper()
	timer := time.NewTimer(probe)
	defer timer.Stop()
	for {
		select {
		case row, ok := <-stream.C:
			if !ok {
				return
			}
			if md, isResponse := responseMarkdown(row.GetActivity()); isResponse && md == text {
				t.Fatalf("feed carried response text %q, want it to stay off this feed", text)
			}
		case <-timer.C:
			return
		}
	}
}

// ---------------------------------------------------------------------------
// #27 — SubagentSyncNestedActivity.
// ---------------------------------------------------------------------------

// TestSubagentSyncNestedActivity drives the `subagent` scenario (prompt
// "!subagent"; MANIFEST.md capture golden "subagent-sync-nested-activity",
// 2026-09-01) — a synchronous Agent-tool spawn whose subagent reads a file
// and reports back within the SAME turn. daemon.md "Identity, as the
// daemon lives it" / PROTO-CHANGES.md Landing 3 AgentId minting rule: the
// nested activity is attributed to the subagent, never folded into the
// main conversation. Asserts the behavioral consequence: the root feed
// carries exactly ONE collapsed bubble for the spawn (settled, succeeded),
// and the subagent's own nested activity (its commission, its Read tool
// call, its own response) is reachable ONLY via that bubble's own sub-feed
// — never inlined as a second top-level row on the root feed.
func TestSubagentSyncNestedActivity(t *testing.T) {
	t.Parallel()
	// Arrange.
	w := NewWorld(t, WorldOpts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)

	// Act.
	driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "subagent")

	rootPage, rootToken := openFeed(t, w, ws, nil)
	rootRows := rootPage.GetSuccess().GetRows()

	bubbleRow := findRow(rootRows, func(row *frontendv1.FeedRow) bool { return subagentBubble(row) != nil })
	if bubbleRow == nil {
		t.Fatalf("root feed page %v, want a row carrying a subagent bubble", rootPage)
	}

	// Assert: the collapsed head, on the ROOT feed.
	bubble := subagentBubble(bubbleRow)
	if got := bubble.GetLabel().GetText(); got != "general-purpose" {
		t.Errorf("subagent bubble label = %q, want %q", got, "general-purpose")
	}
	if got := bubble.GetDescription().GetText(); got != "Explore the module" {
		t.Errorf("subagent bubble description = %q, want %q (ctx.args==\"\" default)", got, "Explore the module")
	}
	bubble = awaitSubagentSettled(t, w, rootPage, w.WatchFeed(rootToken), bubbleRow.GetId())
	if bubble.GetSettled().GetOutcome() == nil {
		t.Fatalf("subagent bubble settled = %v, want an outcome", bubble.GetSettled())
	}
	if bubble.GetSettled().GetSucceeded() == nil {
		t.Errorf("subagent bubble settled outcome = %v, want succeeded", bubble.GetSettled())
	}

	// Assert: nothing of the SUBAGENT's own nested activity leaked onto the
	// root feed as a second top-level row (its Read tool call, its own two
	// response lines).
	if pageHasResponseText(rootRows, "Reading the module's conventions.") {
		t.Errorf("root feed carried the subagent's own thinking-adjacent response text; it belongs on the sub-feed only")
	}
	if pageHasResponseText(rootRows, "The module's test command is `npm test`.") {
		t.Errorf("root feed carried the subagent's own final report text; it belongs on the sub-feed only")
	}

	// Assert: the SAME content IS reachable on the bubble's own sub-feed —
	// "a subagent bubble IS a feed" (feed.proto header).
	subPage, _ := openFeed(t, w, ws, bubbleRow.GetId())
	subRows := subPage.GetSuccess().GetRows()
	if !pageHasResponseText(subRows, "Reading the module's conventions.") {
		t.Errorf("sub-feed page %v, want the subagent's own response text", subPage)
	}
	if !pageHasResponseText(subRows, "The module's test command is `npm test`.") {
		t.Errorf("sub-feed page %v, want the subagent's own final report text", subPage)
	}
	// NO user_prompt ROW FOR THE COMMISSION. sidecar.md's R15 (:298-300):
	// "file-plane user prompts are NEVER page lines — classified as unserved
	// vendor_specific{kind \"user_prompt\"}; the shim's AgentPrompt is the
	// one served form (subagent commissions ride
	// AgentSubagentStart.prompt)."
	//
	// THE COMMISSION IS AN agent_prompt ROW ON THIS SUB-FEED. A bubble's body
	// IS its sub-feed (feed.proto, "THE BUBBLE IS A FEED"), and
	// AgentSubagentPrompt.text is "drawn only where there is room for it — a
	// bubble's body, not its head", so the instruction lands here, addressed
	// from the caller, and nowhere else.
	if !pageHasAgentPromptText(subRows, "Read the module's AGENTS.md and report the test command.") {
		t.Errorf("sub-feed page %v, want an agent_prompt row carrying the commission verbatim", subPage)
	}
}

// pageHasAgentPromptText answers whether any row on the page is an
// agent_prompt whose body carries `text` in a text block, under an address
// naming a sender.
func pageHasAgentPromptText(rows []*frontendv1.FeedRow, text string) bool {
	for _, row := range rows {
		prompt := row.GetAgentPrompt()
		if prompt == nil || !strings.HasPrefix(prompt.GetAddress().GetText(), "from ") {
			continue
		}
		for _, block := range prompt.GetBody().GetBlocks() {
			if block.GetText().GetText() == text {
				return true
			}
		}
	}
	return false
}

// ---------------------------------------------------------------------------
// #28 — SubagentDetached.
// ---------------------------------------------------------------------------

// TestSubagentDetached drives the `subagent-detached` scenario (prompt
// "!subagent-detached"; MANIFEST.md capture golden "subagent-detached",
// 2026-09-02) — an Agent tool_use with run_in_background:true. The turn
// concludes with an async-launch ack; the sweep's own completion (its
// final response, its FeedSubagentSucceeded settle) lands AFTER the turn
// has already ended, on the detached work's own feed — daemon.md
// §"Resolvers and push duties": "the merge bubble is the SAME plumbing as
// subagent bubbles" (i.e. a bubble is a sub-feed, sync or detached, one
// component; feed.proto's FeedDetachedSubagent wrapper: "identical to the
// sync arm's; the wrapper carries the placement fact, never a second
// drawing").
func TestSubagentDetached(t *testing.T) {
	t.Parallel()
	// Arrange.
	w := NewWorld(t, WorldOpts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)

	// Act: the main turn ends on the async-launch ack, well before the
	// sweep itself completes.
	driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "subagent-detached")

	rootPage, rootToken := openFeed(t, w, ws, nil)
	bubbleRow := findRow(rootPage.GetSuccess().GetRows(), func(row *frontendv1.FeedRow) bool {
		return row.GetDetachedSubagent() != nil
	})
	if bubbleRow == nil {
		t.Fatalf("root feed page %v, want a detached_subagent row for the launched sweep", rootPage)
	}

	// Assert: the async-launch ack placed the bubble on the ROOT feed as a
	// detached wrapper (not the sync arm), addressed by its own FeedId.
	if bubbleRow.GetActivity().GetSubagent() != nil {
		t.Errorf("row %v carries the SYNC subagent arm, want the detached wrapper", bubbleRow)
	}

	// Act: wait (event-driven — no sleep) for the sweep's own eventual
	// completion, which lands on this row after the turn already ended.
	rootStream := w.WatchFeed(rootToken)
	bubble := awaitSubagentSettled(t, w, rootPage, rootStream, bubbleRow.GetId())

	// Assert: the completion settled the bubble as succeeded...
	if bubble.GetSettled().GetSucceeded() == nil {
		t.Errorf("detached subagent bubble settled = %v, want succeeded", bubble.GetSettled())
	}
	// ...and the completion's OWN content (the sweep's final response) is
	// on the detached work's own sub-feed, not inlined on the root feed.
	if pageHasResponseText(rootPage.GetSuccess().GetRows(), "Sweep finished.") {
		t.Errorf("root feed's own page already carried the sweep's completion text before settling; it should only ever appear on the sub-feed")
	}
	subPage, subToken := openFeed(t, w, ws, bubbleRow.GetId())
	awaitResponseText(t, w, subPage, w.WatchFeed(subToken), "Sweep finished.")
}

// ---------------------------------------------------------------------------
// #29 — SubagentDetachedUtteranceStaysOffTopLevel.
// ---------------------------------------------------------------------------

// TestSubagentDetachedUtteranceStaysOffTopLevel drives
// `!subagent-detached-utterance` (landed addition, subagents.ts) —
// E2E-EVENT-INVENTORY.md remediation item 8. MANIFEST.md marks this
// scenario GROUNDED IN SHAPE (it reuses subagent-detached's launch
// machinery, itself a real capture golden) but the utterance-in-isolation
// placement itself is INVENTED: "no capture records a live subagent's
// mid-flight utterance in isolation ... so the utterance's OWN placement
// (post-turn, no terminal) is invented from the family's established
// pattern rather than a specific recording." This test's own assertion —
// that the router keeps a live subagent's prose off the top-level feed —
// is exactly what that invented placement exists to prove; nothing here
// ever completes the agent (SPEC.md §C #29: "asserts the router keeps it
// out of the top-level feed while the unit is still live").
func TestSubagentDetachedUtteranceStaysOffTopLevel(t *testing.T) {
	t.Parallel()
	// Arrange.
	w := NewWorld(t, WorldOpts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)

	// Act: the main turn ends before the agent's mid-flight utterance is
	// even written — open the root feed (and start tailing it) RIGHT AWAY
	// so the absence check below cannot miss a push that races ahead of it.
	driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "subagent-detached-utterance")
	rootPage, rootToken := openFeed(t, w, ws, nil)
	rootStream := w.WatchFeed(rootToken)

	bubbleRow := findRow(rootPage.GetSuccess().GetRows(), func(row *frontendv1.FeedRow) bool {
		return row.GetDetachedSubagent() != nil
	})
	if bubbleRow == nil {
		t.Fatalf("root feed page %v, want a detached_subagent row for the launched sweep", rootPage)
	}

	// Assert: the bubble stays LIVE — this scenario never completes the
	// agent (no task_notification is ever emitted).
	if bubbleRow.GetDetachedSubagent().GetSubagent().GetLive() == nil {
		t.Errorf("detached subagent bubble = %v, want it LIVE (this scenario never completes it)", bubbleRow.GetDetachedSubagent().GetSubagent())
	}

	// Act: wait for the mid-flight utterance to actually land, on the
	// bubble's OWN sub-feed — the positive half of the assertion, proving
	// the utterance happened at all before checking where it did NOT go.
	const utterance = "Still sweeping; found something interesting."
	subPage, subToken := openFeed(t, w, ws, bubbleRow.GetId())
	awaitResponseText(t, w, subPage, w.WatchFeed(subToken), utterance)

	// Assert: the SAME utterance never reached the root feed — neither in
	// the page already fetched above, nor as any later push observed on
	// the stream that has been tailing since before the utterance landed.
	if pageHasResponseText(rootPage.GetSuccess().GetRows(), utterance) {
		t.Errorf("root feed page already carried the subagent's mid-flight utterance; it must stay off the top-level feed while the unit is live")
	}
	assertNoResponseTextEver(t, rootStream, StoreOutageWindow, utterance)
}

// ---------------------------------------------------------------------------
// #30 — NestedSubagentHistoricalUsage.
// ---------------------------------------------------------------------------

// TestNestedSubagentHistoricalUsage drives `!usage-historical` (landed
// addition, subagents.ts) — E2E-EVENT-INVENTORY.md remediation item 9.
// UNGROUNDED, INVENTED per MANIFEST.md: "no capture in this manifest
// carries a FILE-plane-only historical usage record ... attributed to a
// NESTED (spawnDepth 2) subagent id." The scenario writes ONLY a file — a
// `.meta.json` + one untimed assistant record on a NESTED subagent's own
// transcript, with no corresponding Agent tool_use ever appearing in the
// main transcript (no spawning call exists to attribute a bubble to) and
// no paired stream-plane message_start; the main turn itself emits prose
// only.
//
// OPEN QUESTION (flagged, not resolved here — the contract docs read for
// this test do not say): daemon.md's own correctness ruling states context/
// account usage on the topbar and footer "arrives as WatchSession's pushed
// context_usage arm (the vendor's own answer, NEVER derived from usage
// frames)" — i.e. the daemon does not itself aggregate scattered subagent
// usage records into a client-visible figure. Nothing in daemon.md, shim.md,
// sidecar.md, or PROTO-CHANGES.md names a daemon-visible surface this
// FILE-plane-only record is expected to reach. This test therefore asserts
// only what the contract does commit to: the record is durably captured
// (driveScenarioToCompletion's own cursor-advance wait) without disrupting
// the ordinary turn, and — since no spawning call for this nested agent
// ever appears on the wire — the daemon fabricates NO subagent bubble for
// it. If a future capture or doc grounds a specific wire consequence, this
// test should gain that assertion; it is not invented here.
func TestNestedSubagentHistoricalUsage(t *testing.T) {
	t.Parallel()
	// Arrange.
	w := NewWorld(t, WorldOpts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)

	// Act.
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "usage-historical")

	// Assert: the ordinary turn concluded normally (prose only on the main
	// stream — no failure terminal from the odd nested file).
	row := AwaitTurnEnded(t, w, ws, turn)
	if row.GetTurnEnded().GetConcluded() == nil {
		t.Errorf("turn ended = %v, want a concluded (success) terminal", row.GetTurnEnded())
	}

	// Assert: no subagent bubble was fabricated for the orphaned nested
	// file — nothing ever spawned it on the wire, so nothing should
	// address it as a bubble.
	rootPage, _ := openFeed(t, w, ws, nil)
	if bubble := findRow(rootPage.GetSuccess().GetRows(), func(row *frontendv1.FeedRow) bool {
		return subagentBubble(row) != nil
	}); bubble != nil {
		t.Errorf("root feed page %v, want no subagent bubble (no Agent tool_use ever spawned one in this scenario)", rootPage)
	}

	// Assert: the world is still healthy after ingesting the odd record —
	// pinned explicitly here (in addition to NewWorld's own end-of-test
	// guarantee) at the point most likely to expose a sidecar/store crash
	// on an unusual file shape.
	w.RequireNoUnexpectedExit(t)
}

// ---------------------------------------------------------------------------
// Coverage extension — `!subagent-failed`: a subagent ending in failure.
// ---------------------------------------------------------------------------

// TestSubagentFailed drives "!subagent-failed" (subagents.ts's
// SUBAGENT_FAILED): a detached `Agent` whose vendor `task_updated` carries
// `status: "failed"` and whose `task_notification` reports the same — the
// scenario's own declared arm is AgentSubagentFailure.
//
// The frontend fact under test is feed.proto's FeedSubagentSettled.outcome
// oneof, which spells four DISTINCT terminals — succeeded, failed, cancelled,
// and lost ("We STOPPED BEING ABLE TO SEE IT — not known to have failed; the
// word carries the distinction, so it never draws as a plain failure"). A
// subagent that ended on its OWN failure must reach `failed` and none of the
// other three: drawing it succeeded would hide the failure, cancelled would
// blame a hand that never touched it, and lost would claim ignorance the
// producer does not have. Every arm is therefore checked, not just the wanted
// one — this is the bubble family's terminal-arm discrimination, and the
// positive alone would pass on a resolver that collapsed all four.
//
// Placement is asserted alongside it: the launch is `run_in_background: true`,
// so the row is the DETACHED wrapper on the root feed, exactly as #28's
// successful sibling is (feed.proto's FeedDetachedSubagent: "identical to the
// sync arm's; the wrapper carries the placement fact, never a second
// drawing").
func TestSubagentFailed(t *testing.T) {
	t.Parallel()
	// Arrange.
	w := NewWorld(t, WorldOpts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)

	// Act: the main turn ends on the async-launch ack; the failure itself
	// lands after it.
	driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "subagent-failed")

	rootPage, rootToken := openFeed(t, w, ws, nil)
	bubbleRow := findRow(rootPage.GetSuccess().GetRows(), func(row *frontendv1.FeedRow) bool {
		return row.GetDetachedSubagent() != nil
	})
	if bubbleRow == nil {
		t.Fatalf("root feed page %v, want a detached_subagent row for the launched sweep", rootPage)
	}
	if bubbleRow.GetActivity().GetSubagent() != nil {
		t.Errorf("row %v carries the SYNC subagent arm, want the detached wrapper", bubbleRow)
	}

	// Assert: the head still names the commission, so a failed run is
	// readable rather than anonymous.
	bubble := awaitSubagentSettled(t, w, rootPage, w.WatchFeed(rootToken), bubbleRow.GetId())
	if got := bubble.GetDescription().GetText(); got != "A sweep that will fail" {
		t.Errorf("settled bubble description = %q, want the commission's own text %q", got, "A sweep that will fail")
	}

	// Assert: the terminal is `failed`, and specifically NOT one of the three
	// neighbouring arms.
	settled := bubble.GetSettled()
	if settled.GetFailed() == nil {
		t.Fatalf("settled outcome = %v, want failed (the vendor reported status \"failed\")", settled.GetOutcome())
	}
	if settled.GetSucceeded() != nil {
		t.Errorf("settled outcome also carries succeeded, want failed alone")
	}
	if settled.GetCancelled() != nil {
		t.Errorf("settled outcome = cancelled, want failed: nothing stopped this agent by hand")
	}
	if settled.GetLost() != nil {
		t.Errorf("settled outcome = lost, want failed: the producer reported the failure, so it was never " +
			"out of sight (feed.proto: lost is \"not known to have failed\")")
	}
}

// TestSubagentBubbleIsDrawnOnceAndAddressesItsSubFeed pins the CROSS-PLANE
// consequence of the bubble's row identity carrying the created agent.
//
// One run's frames reach the daemon from TWO producers under one upsert key —
// the shim's live stream and the sidecar's tail of the vendor transcript
// (docs/overhaul/shim-fanout.md, "the CROSS-PLANE rule"). Only
// `AgentSubagentStart` states `created_agent_id`
// (conversation/v1/agent_activity.proto: "the join key the whole flat model
// rests on"), and the bubble's own FeedId IS that agent's sub-feed address —
// so a settled frame folded before the start landed drew a row whose `Sub` was
// empty, `OpenFeed` refused it as `feed_undecodable`, and the start that
// followed minted a SECOND bubble beside it. The plane order is not steerable
// from this suite, so the assertion is on the INVARIANT that order cannot be
// allowed to break: exactly one bubble on the root feed, and it opens.
func TestSubagentBubbleIsDrawnOnceAndAddressesItsSubFeed(t *testing.T) {
	t.Parallel()
	// Arrange.
	w := NewWorld(t, WorldOpts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)

	// Act.
	driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "subagent")
	rootPage, _ := openFeed(t, w, ws, nil)

	// Assert: ONE bubble, however the two planes interleaved.
	var bubbles []*frontendv1.FeedRow
	for _, row := range rootPage.GetSuccess().GetRows() {
		if subagentBubble(row) != nil {
			bubbles = append(bubbles, row)
		}
	}
	if len(bubbles) != 1 {
		t.Fatalf("root feed carries %d subagent bubbles, want exactly 1 (a bubble drawn before its start names no agent and the start then mints a second)", len(bubbles))
	}

	// Assert: that one bubble is addressable — its id names the created agent.
	resp, err := w.Client().OpenFeed(w.Ctx(), connect.NewRequest(&agentreplv1.OpenFeedRequest{
		Workspace: ws, Feed: bubbles[0].GetId(),
	}))
	if err != nil {
		t.Fatalf("OpenFeed(bubble): %v", err)
	}
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("OpenFeed(bubble) = %v, want a success rather than a refusal", resp.Msg)
	}
}

// TestSubagentBubbleFromAReplayIsStillAddressable — PROTO-CHANGES.md Landing
// 12 (AgentSubagentSuccess.created_agent_id).
//
// A HISTORY REPLAY IS A SETTLED-ONLY DELIVERY, and that is not an accident of
// this test's staging: every frame of one spawn shares ONE upsert key
// (ActivityKey(<activity id>)), so the store durably holds the unit's LAST
// frame and nothing else. A cold-booted daemon replaying that workspace is
// therefore handed the spawn's success with no start behind it — the exact
// delivery whose bubble used to be drawn warned (daemon.feed.subagent_without_start)
// and whose OpenFeed then refused, because the row named no created agent.
//
// This drives the SAME real "subagent" scenario as
// TestSubagentBubbleIsDrawnOnceAndAddressesItsSubFeed and then bounces the
// daemon exactly as #47 does (adColdBoot, adoption_e2e_test.go), so the only
// thing under test is what the replay alone can draw.
func TestSubagentBubbleFromAReplayIsStillAddressable(t *testing.T) {
	t.Parallel()
	// Arrange: a real spawn, driven to completion so the store holds it.
	w := NewWorld(t, WorldOpts{})
	w.ExpectWarnings("daemon.rollout.reconcile")
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "subagent")

	// Act: crash the daemon and cold-boot a successor, which knows the spawn
	// only from the store's durable rows.
	successor := adColdBoot(t, w)
	// THE REPLAY IS NOT FINISHED WHEN THE BOOT ANSWERS: see
	// adAwaitReplayedFeedRow (adoption_e2e_test.go) for the sequence that made
	// this the once-in-nine red it was.
	bubble := adAwaitReplayedFeedRow(t, successor, ws,
		"the spawn's bubble drawn from the store's durable rows",
		func(row *frontendv1.FeedRow) bool { return subagentBubble(row) != nil })

	// Assert: the replayed row addresses the created agent's own feed, so an
	// expand resolves rather than refusing as feed_undecodable.
	sub, err := successor.Client().OpenFeed(successor.Ctx(),
		connect.NewRequest(&agentreplv1.OpenFeedRequest{Workspace: ws, Feed: bubble.GetId()}))
	if err != nil {
		t.Fatalf("OpenFeed(replayed bubble) = error %v, want a success", err)
	}
	if sub.Msg.GetSuccess() == nil {
		t.Fatalf("OpenFeed(replayed bubble) = %v, want a success rather than a refusal", sub.Msg)
	}
}

// ---------------------------------------------------------------------------
// !subagent-detached-hold — an interjection ends the turn, never the agent.
// ---------------------------------------------------------------------------

// TestSubagentDetachedSurvivesAnInterjection drives `!subagent-detached-hold`
// (subagents.ts): a detached agent is launched LIVE and the turn that spawned
// it then holds until an interrupt lands. The owner's ruling ("an interrupt
// only necessitates the interrupt of the synchronous TURN, not the detached
// work", 2026-09-23) is carried by the queue's interjection, whose kill is
// unforced, and by the shim declaring `perTaskStopAffordance`.
//
// An explicit-interrupt prompt ("stop") interjects the held turn; the turn
// must end `interrupted` while the agent stays live. LIVENESS IS PROVED BY AN
// ANSWER, not by a window: stopping the agent's bubble afterwards succeeds
// only against a live agent, and the bubble then settles `cancelled`, the arm
// a hand stop draws. An interrupt that had taken the agent down with the turn
// would have settled it already, and the stop would be refused.
func TestSubagentDetachedSurvivesAnInterjection(t *testing.T) {
	t.Parallel()
	// Arrange: the held turn, with its detached agent live beneath it.
	w := NewWorld(t, WorldOpts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	_, rootToken := openFeed(t, w, ws, nil)
	rootStream := w.WatchFeed(rootToken)

	held := SubmitPrompt(t, w, ws, "!subagent-detached-hold")
	ctx, cancel := context.WithTimeout(w.Ctx(), DefaultTimeout)
	defer cancel()
	harness.AwaitView(t, ctx, rootStream, "the held turn's waiting prose", func(row *frontendv1.FeedRow) bool {
		md, ok := responseMarkdown(row.GetActivity())
		return ok && row.GetTurn().GetValue() == held.GetValue() && md == "Waiting beside the sweep…"
	})
	bubbleRow := awaitFeedRow(t, w, ws, "the live detached-subagent bubble", func(row *frontendv1.FeedRow) bool {
		return row.GetDetachedSubagent().GetSubagent().GetLive() != nil
	})

	// Act: interject the held turn with an explicit interrupt.
	SubmitPrompt(t, w, ws, "stop")

	// Assert: the held turn ends interrupted...
	ended := AwaitTurnEnded(t, w, ws, held)
	if ended.GetTurnEnded().GetInterrupted() == nil {
		t.Fatalf("the held turn's terminal = %v, want turn_ended.interrupted", ended.GetTurnEnded())
	}

	// ...and the agent it spawned is still live: its own stop is accepted.
	resp, err := w.Client().Interrupt(w.Ctx(), connect.NewRequest(&agentreplv1.InterruptRequest{
		Workspace: ws,
		Target:    &agentreplv1.InterruptRequest_Detached{Detached: bubbleRow.GetId()},
	}))
	if err != nil {
		t.Fatalf("Interrupt(detached) after the interjection = error %v, want the live agent stopped", err)
	}
	if got := resp.Msg.GetSuccess().GetInterruptedDetached(); got == nil {
		t.Fatalf("Interrupt(detached) after the interjection = %v, want success.interrupted_detached", resp.Msg)
	}
	page, token := openFeed(t, w, ws, nil)
	bubble := awaitSubagentSettled(t, w, page, w.WatchFeed(token), bubbleRow.GetId())
	if bubble.GetSettled().GetCancelled() == nil {
		t.Fatalf("the agent settled %v after its own stop, want cancelled", bubble.GetSettled().GetOutcome())
	}
}

// ---------------------------------------------------------------------------
// !subagent-interleaved — a subagent streaming INTO the main agent's blocks.
// ---------------------------------------------------------------------------

// TestSubagentInterleavedResponsesStayOnTheirOwnFeeds drives
// `!subagent-interleaved` (subagents.ts): a detached agent's two whole
// responses land BETWEEN the deltas of the main agent's open thinking block
// and open text block. A fold that kept one block cursor for every agent would
// re-key the rest of the main block onto the subagent's message, splitting the
// main thinking and answer or pulling the subagent's words onto the root feed.
//
// The scenario's stated arms are asserted through the daemon's feed: the main
// turn draws exactly ONE thinking unit and exactly ONE answer, each whole and
// the answer marked final; the subagent's own two responses reach its bubble's
// sub-feed and never the root feed; and the bubble settles succeeded.
func TestSubagentInterleavedResponsesStayOnTheirOwnFeeds(t *testing.T) {
	t.Parallel()
	// Arrange.
	w := NewWorld(t, WorldOpts{})
	repo := harness.NewRepo(t)
	ws := harness.Register(t, w.Daemon, repo.Dir)
	const (
		thinking   = "Answering while the sweep runs."
		conclusion = "The sweep is running in the background."
		started    = "Sweep started."
		finished   = "Sweep finished."
	)

	// Act: the turn ends; the agent's own completion follows it.
	turn := driveScenarioToCompletion(t, w, ws, w.DefaultConfigDir, "subagent-interleaved")
	firstPage, firstToken := openFeed(t, w, ws, nil)
	bubbleRow := findRow(firstPage.GetSuccess().GetRows(), func(row *frontendv1.FeedRow) bool {
		return row.GetDetachedSubagent() != nil
	})
	if bubbleRow == nil {
		t.Fatalf("root feed page %v, want a detached_subagent row for the launched sweep", firstPage)
	}
	bubble := awaitSubagentSettled(t, w, firstPage, w.WatchFeed(firstToken), bubbleRow.GetId())
	rootPage, _ := openFeed(t, w, ws, nil)

	// Assert: the bubble settled succeeded.
	if bubble.GetSettled().GetSucceeded() == nil {
		t.Errorf("the interleaved agent settled %v, want succeeded", bubble.GetSettled().GetOutcome())
	}

	// Assert: the main turn's prose is ONE thinking unit and ONE final answer.
	var thinkingRows, answerRows []*frontendv1.FeedResponse
	for _, row := range rootPage.GetSuccess().GetRows() {
		resp := row.GetActivity().GetResponse()
		if row.GetTurn().GetValue() != turn.GetValue() || resp == nil {
			continue
		}
		if resp.GetThinking() {
			thinkingRows = append(thinkingRows, resp)
			continue
		}
		answerRows = append(answerRows, resp)
	}
	if len(thinkingRows) != 1 {
		t.Fatalf("the main turn drew %d thinking units, want exactly one: %v", len(thinkingRows), thinkingRows)
	}
	if got := thinkingRows[0].GetSuccess().GetProse().GetMarkdown(); got != thinking {
		t.Errorf("the main thinking unit = %q, want it whole: %q", got, thinking)
	}
	if len(answerRows) != 1 {
		t.Fatalf("the main turn drew %d response units, want exactly one answer: %v", len(answerRows), answerRows)
	}
	if got := answerRows[0].GetSuccess().GetProse().GetMarkdown(); got != conclusion {
		t.Errorf("the main answer = %q, want it whole: %q", got, conclusion)
	}
	if !answerRows[0].GetFinalAnswer() {
		t.Errorf("the main answer is not marked final_answer, want the turn's concluded answer")
	}

	// Assert: the agent's own words are on its sub-feed, and never on the root.
	for _, text := range []string{started, finished} {
		if pageHasResponseText(rootPage.GetSuccess().GetRows(), text) {
			t.Errorf("the root feed carried the subagent's %q, want it on the sub-feed alone", text)
		}
	}
	subPage, subToken := openFeed(t, w, ws, bubbleRow.GetId())
	subStream := w.WatchFeed(subToken)
	awaitResponseText(t, w, subPage, subStream, started)
	awaitResponseText(t, w, subPage, subStream, finished)
}

// ---------------------------------------------------------------------------
// Liveness is the vendor process's level (conversation.v1 SessionLiveWork).
// ---------------------------------------------------------------------------

// TestADetachedSubagentWhoseProcessDiedIsNotLiveAfterAColdBoot is the
// 2026-10-07 regression end to end: a detached subagent the record shows
// running, whose vendor process is gone, must not come back as live work when
// a successor daemon replays the conversation. The subagent of
// `!subagent-detached-utterance` never completes; the daemon is crashed and
// its shim killed, so the successor's bring-up resumes the conversation in a
// NEW vendor process that runs no such agent.
func TestADetachedSubagentWhoseProcessDiedIsNotLiveAfterAColdBoot(t *testing.T) {
	t.Parallel()
	// Arrange.
	first := NewWorld(t, WorldOpts{})
	first.ExpectWarnings("daemon.rollout.reconcile")
	repo := harness.NewRepo(t)
	ws := harness.Register(t, first.Daemon, repo.Dir)
	turn := driveScenarioToCompletion(t, first, ws, first.DefaultConfigDir, "subagent-detached-utterance")
	// THE CRASH WAITS FOR THE TURN'S DURABLE CLOSE, as raiseColdGate's does
	// (coldgate_e2e_test.go): a kill between the feed's turn end and the
	// queue's close leaves the turn open on disk for the successor to close
	// as an orphan, which is not this test's subject.
	first.Daemon.AwaitWorkspaceLogRecord(ws.GetDir(), "the first daemon's durable close of the turn", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.promptqueue.turn_ended" &&
			r.Message == "the turn ended; nothing is waiting to be delivered" &&
			r.Context["turn"] == turn.GetValue()
	})

	// Act: the daemon crashes and its shim -- the process running the agent
	// -- is killed; a successor boots on the same state and account root.
	first.Kill()
	coldGateKillShims(t, first.Daemon)
	opts := first.SuccessorOpts(t)
	opts.ExtraArgs = []string{"--default-config-dir", first.DefaultConfigDir}
	opts.Timeout = AdoptionChainTimeout
	successor := harness.StartDaemon(t, opts)
	successor.ExpectWarnings("daemon.rollout.reconcile")

	// Assert: the replayed bubble is drawn, and not as running...
	bubble := adAwaitReplayedFeedRow(t, successor, ws,
		"the agent's bubble replayed as no longer running",
		func(row *frontendv1.FeedRow) bool {
			sub := subagentBubble(row)
			return sub != nil && sub.GetLive() == nil
		})
	if subagentBubble(bubble).GetLive() != nil {
		t.Fatalf("bubble = %v, want it drawn stopped", bubble)
	}
	// ...and the footer counts no live agent for it.
	host := successor.WatchHost(ws)
	defer host.Close()
	web := successor.WatchWeb(ws)
	defer web.Close()
	footer := successor.WatchFooter(ws)
	defer footer.Close()
	ctx, cancel := context.WithTimeout(successor.Ctx(), DefaultTimeout)
	defer cancel()
	harness.AwaitView(t, ctx, footer, "the footer counting no live agent", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetLiveWork().GetAgents().GetCount() == 0
	})
}
