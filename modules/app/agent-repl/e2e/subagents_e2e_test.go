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
	// AgentSubagentStart.prompt)." The commission's CONTENT is already
	// covered above, on the bubble's own description.
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
