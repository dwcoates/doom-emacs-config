//go:build integration

package integration

import (
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/integration/harness"

	"connectrpc.com/connect"
	"google.golang.org/protobuf/proto"
)

// ==========================================================================
// OpenFeed / WatchFeed / GetFeedPage — the page/tail seam.
// ==========================================================================

func TestWatchFeedTailsExactlyAfterTheOpenedPageWithNoGapOrOverlap(t *testing.T) {
	t.Parallel()
	// Arrange: one row lands before the feed is ever opened.
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-seam", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	f.shim.PushAgentFrame(mainAgent, feedResponseFrames("resp-1", "first")[0])
	f.shim.PushAgentFrame(mainAgent, feedResponseFrames("resp-1", "first")[1])

	// Act: open the root feed (mints the page + token), then push a SECOND
	// row before ever watching the tail.
	//
	// A push is fire-and-forget over the fake shim's control socket, so the
	// open is re-taken until resp-1 has actually landed: the subject is what a
	// page carries versus what the tail then delivers, and racing the open
	// against the routing of the frame tests neither.
	page, token := f.openFeedOnceCarrying("resp-1's settled prose", func(p *frontendv1.FeedPage) bool {
		for _, r := range p.GetSuccess().GetRows() {
			if r.GetActivity().GetResponse().GetSuccess().GetProse().GetProse().GetSource().GetMarkdown() == "first" {
				return true
			}
		}
		return false
	})
	f.shim.PushAgentFrame(mainAgent, feedResponseFrames("resp-2", "second")[0])
	f.shim.PushAgentFrame(mainAgent, feedResponseFrames("resp-2", "second")[1])
	tail := f.d.WatchFeed(token)

	// Assert: the page already carries resp-1's text; the tail's first row
	// is resp-2's, proving no gap (resp-2 was not lost between open and
	// watch) and no overlap (resp-1 is not re-delivered on the tail).
	sawFirst := false
	for _, r := range page.GetSuccess().GetRows() {
		if r.GetActivity().GetResponse().GetSuccess().GetProse().GetProse().GetSource().GetMarkdown() == "first" {
			sawFirst = true
		}
	}
	if !sawFirst {
		t.Fatalf("OpenFeed's page = %v, want it to already carry the row pushed before the open", page)
	}
	// AN OVERLAP IS RESP-1 RE-DELIVERED EXACTLY AS THE PAGE HELD IT. A
	// restatement CARRYING A CHANGE the page did not hold is not one: resp-2
	// landing re-pushes resp-1 as `interim` (resolve/feed/interim.go), and the
	// reveal pace can be stamped on resp-1 just after the open (measured
	// 2026-10-15: `reveal_pace` republished 1ms after the page was served).
	// So every resp-1 row the tail delivers before resp-2's settled row must
	// differ from the page's, and resp-2's settled row must arrive (no gap).
	var held *frontendv1.FeedRow
	for _, r := range page.GetSuccess().GetRows() {
		if r.GetActivity().GetResponse().GetSuccess().GetProse().GetProse().GetSource().GetMarkdown() == "first" {
			held = r
		}
	}
	// awaitRow fails the test if resp-2's settled row never arrives.
	awaitRow(t, f, tail, "resp-2's settled row on the tail", func(r *frontendv1.FeedRow) bool {
		resp := r.GetActivity().GetResponse()
		if resp.GetSuccess().GetProse().GetProse().GetSource().GetMarkdown() == "first" && proto.Equal(r, held) {
			t.Errorf("the tail re-delivered resp-1 exactly as the page held it: %v", r)
		}
		return resp.GetSuccess().GetProse().GetProse().GetSource().GetMarkdown() == "second" && !resp.GetInterim()
	})
}

// TestAFeedTextScaleFrameIsNotARowOnTheFeedTail pins that WatchFeed's THREE
// arms stay distinguishable on the wire: the response carries `row`,
// `selection` and `feed_text_scale` (endpoint_watch_feed.proto), and the zoom
// frame is pushed on EVERY open feed's watch. A reader that takes any frame
// for a row sees a row that does not exist -- which is precisely what every
// "produces no feed row" probe in this suite reported until the harness
// selected row frames rather than picking a (nil) row out of each one.
func TestAFeedTextScaleFrameIsNotARowOnTheFeedTail(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	tail := f.watchRootFeed()

	// Act: nudge the daemon-global zoom, which pushes a FeedTextScale frame to
	// every open feed's watch -- this tail included.
	if _, err := f.d.Client().AdjustFeedTextScale(f.d.Ctx(), connect.NewRequest(&agentreplv1.AdjustFeedTextScaleRequest{
		Direction: agentreplv1.AdjustFeedTextScaleDirection_ADJUST_FEED_TEXT_SCALE_DIRECTION_INCREASE,
	})); err != nil {
		t.Fatalf("AdjustFeedTextScale = error %v, want the scale now in force", err)
	}

	// Assert: the zoom reached the watch, and NO row was drawn by it.
	harness.ExpectNoPush(t, tail, harness.ProbeWindow, "a feed text scale frame is not a feed row")
}

func TestWatchFeedWithAnUnmintedTokenIsRefusedAtTheTransport(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})

	// Act
	stream, err := f.d.Client().WatchFeed(f.d.Ctx(), connect.NewRequest(&agentreplv1.WatchFeedRequest{
		Watch: &agentreplv1.FeedWatchToken{Value: "bogus-unminted-token"},
	}))

	// Assert
	if err == nil {
		if stream.Receive() {
			t.Fatalf("WatchFeed(bogus token) delivered a row %v, want a transport refusal", stream.Msg())
		}
		err = stream.Err()
	}
	if err == nil {
		t.Fatal("WatchFeed(bogus token) = success, want a transport-level refusal")
	}
	// A refused Watch* open is transport-closed BY DESIGN (project lead): it is
	// recorded at INFO under daemon.refusal.transport_closed, never warned as
	// an unlanded arm.
	rec := f.d.AwaitRunLogOperation("daemon.refusal.transport_closed")
	if !strings.EqualFold(rec.Level, "info") {
		t.Fatalf("the transport-closed record = level %q, want INFO", rec.Level)
	}
	if rec.Context["rpc"] != "WatchFeed" || rec.Context["cause"] != "unknown_token" {
		t.Fatalf("the transport-closed record's context = %v, want rpc WatchFeed and cause unknown_token", rec.Context)
	}
	// No ExpectWarnings operation: the refusal must produce no WARN at all.
}

func TestGetFeedPageFirstThenNextWalksOlderPages(t *testing.T) {
	t.Parallel()
	// Arrange: push MORE than one page's worth of rows off harness.FeedPageSize
	// — the daemon's own feed.DefaultPageSize, not a guessed number — so a
	// second page provably exists regardless of what the page size is.
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-walk", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	const n = harness.FeedPageSize + walkPageMargin
	for i := 0; i < n; i++ {
		f.shim.PushAgentFrame(mainAgent, feedRowLabeledResponse(i))
	}
	// Sync: the pushes are asynchronous, so the walk waits for the LAST row to
	// land -- a page taken mid-ingest would be a page over however many rows
	// happened to have arrived.
	awaitRow(t, f, tail, "the last padded row", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetResponse().GetSuccess().GetProse().GetProse().GetSource().GetMarkdown() == "row "+itoa(n-1)
	})

	// Act
	first, err := f.d.Client().GetFeedPage(f.d.Ctx(), connect.NewRequest(&agentreplv1.GetFeedPageRequest{
		Workspace: f.ws,
		Page:      &agentreplv1.GetFeedPageRequest_First{First: &agentreplv1.GetFeedPageFirst{}},
	}))
	if err != nil {
		t.Fatalf("GetFeedPage{first} = error %v, want a page", err)
	}
	if first.Msg.GetSuccess().GetError() != nil {
		t.Fatalf("GetFeedPage{first} = %v, want a clean page over %d rows", first.Msg, n)
	}
	if first.Msg.GetSuccess().GetSuccess().GetHasMore() == nil {
		t.Fatalf("GetFeedPage{first} over %d rows (page size %d) = %v, want has_more set: a second page must exist", n, harness.FeedPageSize, first.Msg)
	}
	next, err := f.d.Client().GetFeedPage(f.d.Ctx(), connect.NewRequest(&agentreplv1.GetFeedPageRequest{
		Workspace: f.ws,
		Page:      olderThanOldestOf(first.Msg.GetSuccess()),
	}))

	// Assert: the older page exists and does not repeat the newest page's rows.
	if err != nil {
		t.Fatalf("GetFeedPage{next} = error %v, want the older page", err)
	}
	if next.Msg.GetSuccess().GetError() != nil {
		t.Fatalf("GetFeedPage{next} = %v, want the older page", next.Msg)
	}
	newest := map[string]bool{}
	for _, r := range first.Msg.GetSuccess().GetSuccess().GetRows() {
		newest[r.GetId().GetValue()] = true
	}
	for _, r := range next.Msg.GetSuccess().GetSuccess().GetRows() {
		if newest[r.GetId().GetValue()] {
			t.Fatalf("GetFeedPage{next} repeated a row %v the first page already served", r.GetId())
		}
	}
}

func TestGetFeedPageWalkOnASubagentBubbleFeedIdPagesTheSubFeedNotTheRoot(t *testing.T) {
	t.Parallel()
	// Arrange: spawn a subagent and push MORE than one page of its own work,
	// plus a distinguishable row that lands only on the ROOT feed.
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-subwalk", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("spawn-walk"),
		Item: &conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{Result: &conversationv1.AgentSubagent_Start{
			Start: &conversationv1.AgentSubagentStart{CreatedAgentId: &conversationv1.AgentId{Value: "sub-walk"}, Prompt: &conversationv1.AgentSubagentPrompt{Text: "explore"}, StartedAt: startedAt(1)},
		}}},
	}))
	bubble := awaitRow(t, f, tail, "the spawn's bubble head", func(r *frontendv1.FeedRow) bool { return r.GetActivity().GetSubagent() != nil })

	const n = harness.FeedPageSize + walkPageMargin
	for i := 0; i < n; i++ {
		f.shim.PushAgentFrame("sub-walk", feedRowLabeledResponseFor("sub-walk", i))
	}
	// Sync: wait for the LAST sub-feed row to land before walking it, proving
	// every row pushed to "sub-walk" is durable by the time the walk begins.
	lastSubMd := "sub-walk row " + itoa(n-1)
	f.awaitRowInFeed(bubble.GetId(), "the subagent's last pushed row", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetResponse().GetSuccess().GetProse().GetProse().GetSource().GetMarkdown() == lastSubMd
	})
	// A row that must NEVER appear on the sub-feed's walk.
	f.shim.PushAgentFrame(mainAgent, feedResponseFrames("root-only", "root row")[0])
	f.shim.PushAgentFrame(mainAgent, feedResponseFrames("root-only", "root row")[1])
	awaitRow(t, f, tail, "the root-only row landing on the root feed", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetResponse().GetSuccess().GetProse().GetProse().GetSource().GetMarkdown() == "root row"
	})

	// Act: walk the SUBAGENT BUBBLE's own FeedId, never the root.
	isRootLeak := func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetResponse().GetSuccess().GetProse().GetProse().GetSource().GetMarkdown() == "root row"
	}
	first, err := f.d.Client().GetFeedPage(f.d.Ctx(), connect.NewRequest(&agentreplv1.GetFeedPageRequest{
		Workspace: f.ws, Feed: bubble.GetId(),
		Page: &agentreplv1.GetFeedPageRequest_First{First: &agentreplv1.GetFeedPageFirst{}},
	}))
	requireRPCAnswered(t, "GetFeedPage{first} on the subagent's FeedId", first, err)
	if first.Msg.GetSuccess().GetError() != nil {
		t.Fatalf("GetFeedPage{first} on the subagent's FeedId = %v, want the sub-feed's newest page", first.Msg)
	}
	if findRow(first.Msg.GetSuccess(), isRootLeak) != nil {
		t.Fatalf("a walk on the subagent's FeedId served the ROOT feed's row: %v", first.Msg)
	}
	if first.Msg.GetSuccess().GetSuccess().GetHasMore() == nil {
		t.Fatalf("GetFeedPage{first} on the subagent's FeedId over %d rows = %v, want has_more set", n, first.Msg)
	}

	// Assert: {next} on the SAME (sub-feed) walk, still never the root's row.
	next, err := f.d.Client().GetFeedPage(f.d.Ctx(), connect.NewRequest(&agentreplv1.GetFeedPageRequest{
		Workspace: f.ws, Feed: bubble.GetId(),
		Page: olderThanOldestOf(first.Msg.GetSuccess()),
	}))
	requireRPCAnswered(t, "GetFeedPage{next} on the subagent's FeedId", next, err)
	if next.Msg.GetSuccess().GetError() != nil {
		t.Fatalf("GetFeedPage{next} on the subagent's FeedId = %v, want the sub-feed's older page", next.Msg)
	}
	if findRow(next.Msg.GetSuccess(), isRootLeak) != nil {
		t.Fatalf("GetFeedPage{next} on the subagent's FeedId served the ROOT feed's row: %v", next.Msg)
	}
	for _, r := range next.Msg.GetSuccess().GetSuccess().GetRows() {
		md := r.GetActivity().GetResponse().GetSuccess().GetProse().GetProse().GetSource().GetMarkdown()
		if md == "" {
			continue
		}
		if !strings.HasPrefix(md, "sub-walk row ") {
			t.Fatalf("the sub-feed's older page carried a foreign row %v, want only sub-walk's own rows", r)
		}
	}
}

func TestGetFeedPageWithAnUndecodableFeedIdAnswersFeedUndecodable(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})

	// Act
	resp, err := f.d.Client().GetFeedPage(f.d.Ctx(), connect.NewRequest(&agentreplv1.GetFeedPageRequest{
		Workspace: f.ws,
		Feed:      &frontendv1.FeedId{Value: "not-a-real-feed-id"},
		Page:      &agentreplv1.GetFeedPageRequest_First{First: &agentreplv1.GetFeedPageFirst{}},
	}))

	// Assert
	if err != nil {
		t.Fatalf("GetFeedPage with a garbage feed id = error %v, want a typed feed_undecodable refusal", err)
	}
	if resp.Msg.GetError().GetFeedUndecodable() == nil {
		t.Fatalf("GetFeedPage with a garbage feed id = %v, want error.feed_undecodable", resp.Msg)
	}
}

func TestGetFeedPageWithAnotherWorkspacesBubbleFeedIdAnswersFeedNotInWorkspace(t *testing.T) {
	t.Parallel()
	// Arrange: a second, independent workspace on the same daemon, with its
	// own subagent bubble.
	f := newOpened(t, harness.Opts{})
	other := secondWorkspaceOn(t, f.d)
	other.submit("go", "k-otherwalk", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	otherTail := other.watchRootFeed()
	other.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("other-spawn"),
		Item: &conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{Result: &conversationv1.AgentSubagent_Start{
			Start: &conversationv1.AgentSubagentStart{CreatedAgentId: &conversationv1.AgentId{Value: "other-sub"}, Prompt: &conversationv1.AgentSubagentPrompt{Text: "x"}, StartedAt: startedAt(1)},
		}}},
	}))
	otherBubble := awaitRow(t, other, otherTail, "the other workspace's bubble", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetSubagent() != nil
	})

	// Act: address the FIRST workspace with the SECOND workspace's FeedId.
	resp, err := f.d.Client().GetFeedPage(f.d.Ctx(), connect.NewRequest(&agentreplv1.GetFeedPageRequest{
		Workspace: f.ws, Feed: otherBubble.GetId(),
		Page: &agentreplv1.GetFeedPageRequest_First{First: &agentreplv1.GetFeedPageFirst{}},
	}))

	// Assert
	if err != nil {
		t.Fatalf("GetFeedPage across workspaces = error %v, want a typed feed_not_in_workspace refusal", err)
	}
	if resp.Msg.GetError().GetFeedNotInWorkspace() == nil {
		t.Fatalf("GetFeedPage with another workspace's bubble FeedId = %v, want error.feed_not_in_workspace", resp.Msg)
	}
}

func TestGetFeedPageAtStartAndAFurtherNextRepeatsTheWholeFeed(t *testing.T) {
	t.Parallel()
	// Arrange: walk all the way to the feed's start.
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-atstart", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	const n = harness.FeedPageSize + walkPageMargin
	for i := 0; i < n; i++ {
		f.shim.PushAgentFrame(mainAgent, feedRowLabeledResponse(i))
	}
	awaitRow(t, f, tail, "the last padded row", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetResponse().GetSuccess().GetProse().GetProse().GetSource().GetMarkdown() == "row "+itoa(n-1)
	})

	first, err := f.d.Client().GetFeedPage(f.d.Ctx(), connect.NewRequest(&agentreplv1.GetFeedPageRequest{
		Workspace: f.ws, Page: &agentreplv1.GetFeedPageRequest_First{First: &agentreplv1.GetFeedPageFirst{}},
	}))
	requireRPCAnswered(t, "GetFeedPage{first}", first, err)
	if first.Msg.GetSuccess().GetError() != nil {
		t.Fatalf("GetFeedPage{first} = %v, want the newest page", first.Msg)
	}
	current := first.Msg
	// The oldest row the walk holds; a page that served no row leaves it.
	held := first.Msg.GetSuccess()
	var oldest *agentreplv1.GetFeedPageResponse
	for current.GetSuccess().GetSuccess().GetAtStart() == nil {
		resp, err := f.d.Client().GetFeedPage(f.d.Ctx(), connect.NewRequest(&agentreplv1.GetFeedPageRequest{
			Workspace: f.ws, Page: olderThanOldestOf(held),
		}))
		if err != nil {
			t.Fatalf("GetFeedPage{next} while walking to the start = error %v", err)
		}
		if resp.Msg.GetSuccess().GetError() != nil {
			t.Fatalf("GetFeedPage{next} while walking to the start = %v, want a clean page", resp.Msg)
		}
		current = resp.Msg
		if len(current.GetSuccess().GetSuccess().GetRows()) > 0 {
			held = current.GetSuccess()
		}
	}
	oldest = current

	// Act: a FURTHER {next} past the page that already set at_start.
	further, err := f.d.Client().GetFeedPage(f.d.Ctx(), connect.NewRequest(&agentreplv1.GetFeedPageRequest{
		Workspace: f.ws, Page: olderThanOldestOf(held),
	}))

	// Assert: per pages.go's stepPage, a walk already at the start (with no
	// history_replay_truncated record) is answered with success and
	// composePage(durable, 0) — the WHOLE feed in one page, still at_start —
	// never a refusal and never an empty page.
	if err != nil {
		t.Fatalf("GetFeedPage{next} past at_start = error %v, want the daemon's composePage(durable, 0) answer", err)
	}
	if further.Msg.GetSuccess().GetError() != nil {
		t.Fatalf("GetFeedPage{next} past at_start = %v, want a success page", further.Msg)
	}
	if further.Msg.GetSuccess().GetSuccess().GetAtStart() == nil {
		t.Fatalf("GetFeedPage{next} past at_start = %v, want at_start still set", further.Msg)
	}
	firstIDs := map[string]bool{}
	for _, r := range first.Msg.GetSuccess().GetSuccess().GetRows() {
		firstIDs[r.GetId().GetValue()] = true
	}
	oldestIDs := map[string]bool{}
	for _, r := range oldest.GetSuccess().GetSuccess().GetRows() {
		oldestIDs[r.GetId().GetValue()] = true
	}
	furtherIDs := map[string]bool{}
	for _, r := range further.Msg.GetSuccess().GetSuccess().GetRows() {
		furtherIDs[r.GetId().GetValue()] = true
	}
	wantTotal := len(firstIDs) + len(oldestIDs)
	if len(furtherIDs) != wantTotal {
		t.Fatalf("GetFeedPage{next} past at_start served %d rows, want %d (every row from both the newest page %d and the oldest page %d, re-served as one page)",
			len(furtherIDs), wantTotal, len(firstIDs), len(oldestIDs))
	}
	for id := range firstIDs {
		if !furtherIDs[id] {
			t.Fatalf("GetFeedPage{next} past at_start dropped a row %q from the original newest page", id)
		}
	}
	for id := range oldestIDs {
		if !furtherIDs[id] {
			t.Fatalf("GetFeedPage{next} past at_start dropped a row %q from the oldest page", id)
		}
	}
}

func TestGetFeedPageFirstAfterNextReservesTheNewestPage(t *testing.T) {
	t.Parallel()
	// Arrange: walk one page older, so the reader's position is no longer the
	// newest page.
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-firstafter", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	const n = harness.FeedPageSize + walkPageMargin
	for i := 0; i < n; i++ {
		f.shim.PushAgentFrame(mainAgent, feedRowLabeledResponse(i))
	}
	awaitRow(t, f, tail, "the last padded row", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetResponse().GetSuccess().GetProse().GetProse().GetSource().GetMarkdown() == "row "+itoa(n-1)
	})

	first, err := f.d.Client().GetFeedPage(f.d.Ctx(), connect.NewRequest(&agentreplv1.GetFeedPageRequest{
		Workspace: f.ws, Page: &agentreplv1.GetFeedPageRequest_First{First: &agentreplv1.GetFeedPageFirst{}},
	}))
	requireRPCAnswered(t, "GetFeedPage{first}", first, err)
	if first.Msg.GetSuccess().GetError() != nil {
		t.Fatalf("GetFeedPage{first} = %v, want the newest page", first.Msg)
	}
	if _, err := f.d.Client().GetFeedPage(f.d.Ctx(), connect.NewRequest(&agentreplv1.GetFeedPageRequest{
		Workspace: f.ws, Page: olderThanOldestOf(first.Msg.GetSuccess()),
	})); err != nil {
		t.Fatalf("GetFeedPage{next} = error %v", err)
	}

	// Act: {first} again, after the walk moved to an older page.
	second, err := f.d.Client().GetFeedPage(f.d.Ctx(), connect.NewRequest(&agentreplv1.GetFeedPageRequest{
		Workspace: f.ws, Page: &agentreplv1.GetFeedPageRequest_First{First: &agentreplv1.GetFeedPageFirst{}},
	}))

	// Assert: {first} re-serves the SAME newest page as the original {first},
	// regardless of the intervening {next}.
	if err != nil {
		t.Fatalf("GetFeedPage{first} after a {next} = error %v, want the newest page again", err)
	}
	if !proto.Equal(second.Msg, first.Msg) {
		t.Fatalf("GetFeedPage{first} after a {next} = %v, want the same newest page as the original {first} = %v", second.Msg, first.Msg)
	}
}

func TestAWalkContinuesOnAnyConnection(t *testing.T) {
	t.Parallel()
	// Arrange: a walk begun on connection A (owner's report, 2026-10-03: a
	// webview spreads its requests over several connections, so an `older`
	// click arrived on a connection the walk was not keyed by and was refused).
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-anyconn", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	const n = harness.FeedPageSize + walkPageMargin
	for i := 0; i < n; i++ {
		f.shim.PushAgentFrame(mainAgent, feedRowLabeledResponse(i))
	}
	awaitRow(t, f, tail, "the last padded row", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetResponse().GetSuccess().GetProse().GetProse().GetSource().GetMarkdown() == "row "+itoa(n-1)
	})
	first, err := f.d.Client().GetFeedPage(f.d.Ctx(), connect.NewRequest(&agentreplv1.GetFeedPageRequest{
		Workspace: f.ws, Page: &agentreplv1.GetFeedPageRequest_First{First: &agentreplv1.GetFeedPageFirst{}},
	}))
	requireRPCAnswered(t, "GetFeedPage{first}", first, err)
	if first.Msg.GetSuccess().GetSuccess().GetHasMore() == nil {
		t.Fatalf("GetFeedPage{first} = %v, want a page with older rows above it", first.Msg)
	}

	// Act: {next} naming that walk, on a second, independent connection.
	resp, err := f.d.Dial().GetFeedPage(f.d.Ctx(), connect.NewRequest(&agentreplv1.GetFeedPageRequest{
		Workspace: f.ws, Page: olderThanOldestOf(first.Msg.GetSuccess()),
	}))

	// Assert: the older page, served on whichever connection asked.
	requireRPCAnswered(t, "GetFeedPage{next} on another connection", resp, err)
	if resp.Msg.GetSuccess() == nil {
		t.Fatalf("GetFeedPage{next} on another connection = %v, want the walk's older page", resp.Msg)
	}
}

func TestTwoOpeningsWalkIndependently(t *testing.T) {
	t.Parallel()
	// Arrange: two readers (two webviews) each open the feed.
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-twowalks", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	const n = harness.FeedPageSize + walkPageMargin
	for i := 0; i < n; i++ {
		f.shim.PushAgentFrame(mainAgent, feedRowLabeledResponse(i))
	}
	awaitRow(t, f, tail, "the last padded row", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetResponse().GetSuccess().GetProse().GetProse().GetSource().GetMarkdown() == "row "+itoa(n-1)
	})
	firstPage := func() *frontendv1.FeedPage {
		resp, err := f.d.Client().GetFeedPage(f.d.Ctx(), connect.NewRequest(&agentreplv1.GetFeedPageRequest{
			Workspace: f.ws, Page: &agentreplv1.GetFeedPageRequest_First{First: &agentreplv1.GetFeedPageFirst{}},
		}))
		requireRPCAnswered(t, "GetFeedPage{first}", resp, err)
		if resp.Msg.GetSuccess() == nil {
			t.Fatalf("GetFeedPage{first} = %v", resp.Msg)
		}
		return resp.Msg.GetSuccess()
	}
	a, b := firstPage(), firstPage()
	nextOf := func(page *frontendv1.FeedPage) *frontendv1.FeedPage {
		resp, err := f.d.Client().GetFeedPage(f.d.Ctx(), connect.NewRequest(&agentreplv1.GetFeedPageRequest{
			Workspace: f.ws, Page: olderThanOldestOf(page),
		}))
		requireRPCAnswered(t, "GetFeedPage{next}", resp, err)
		if resp.Msg.GetSuccess() == nil {
			t.Fatalf("GetFeedPage{next} = %v", resp.Msg)
		}
		return resp.Msg.GetSuccess()
	}

	// Act: A walks one page older, then B does.
	aNext := nextOf(a)
	bNext := nextOf(b)

	// Assert: B's older page was not consumed by A's: both got the same one.
	if !proto.Equal(aNext, bNext) {
		t.Fatalf("B's older page %v differs from A's %v: a walk consumed another's", bNext, aNext)
	}
}

// ==========================================================================
// Tool cards.
// ==========================================================================

func TestReadToolCardDrawsCodeOutputWithPaintSpansAndOmittedForAHeadCut(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-read", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("read-1"),
		Item: &conversationv1.AgentActivity_Read{Read: &conversationv1.AgentRead{Result: &conversationv1.AgentRead_Start{
			Start: &conversationv1.AgentReadStart{Path: &conversationv1.ReadPath{Path: "big.go"}, StartedAt: startedAt(1)},
		}}},
	}))
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("read-1"),
		Item: &conversationv1.AgentActivity_Read{Read: &conversationv1.AgentRead{Result: &conversationv1.AgentRead_Success{
			Success: &conversationv1.AgentReadSuccess{
				Path: &conversationv1.ReadPath{Path: "big.go"},
				Extent: &conversationv1.AgentReadSuccess_Head{Head: &conversationv1.AgentReadHead{
					Contents:   "package main\n",
					TotalLines: 4312,
					Cut:        &conversationv1.AgentReadHead_LineCap{LineCap: &conversationv1.AgentReadCutAtLineCap{}},
				}},
				SettledAt: settledAt(2),
			},
		}}},
	}))

	// Assert
	row := awaitRow(t, f, tail, "the read's settled tool card", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetSimpleToolCall().GetReturned().GetForm() != nil
	})
	code := row.GetActivity().GetSimpleToolCall().GetReturned().GetCode()
	if code == nil {
		t.Fatalf("the read's tool card form = %v, want code output", row.GetActivity().GetSimpleToolCall().GetReturned())
	}
	if len(code.GetSpans()) == 0 {
		t.Fatal("the read's code output carries no paint spans, want at least one")
	}
	if code.GetOmitted() == nil {
		t.Fatal("a head-cut read's code output carries no omitted line, want one composed for the cut")
	}
}

func TestWriteToolCardDrawsDiffLines(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-write", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("write-1"),
		Item: &conversationv1.AgentActivity_Write{Write: &conversationv1.AgentWrite{Result: &conversationv1.AgentWrite_Start{
			Start: &conversationv1.AgentWriteStart{Path: &conversationv1.ReadPath{Path: "new.go"}, StartedAt: startedAt(1)},
		}}},
	}))
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("write-1"),
		Item: &conversationv1.AgentActivity_Write{Write: &conversationv1.AgentWrite{Result: &conversationv1.AgentWrite_Success{
			Success: &conversationv1.AgentWriteSuccess{
				Path:      &conversationv1.ReadPath{Path: "new.go"},
				Outcome:   &conversationv1.AgentWriteSuccess_Created{Created: &conversationv1.AgentWriteCreated{}},
				Patch:     []*conversationv1.FilePatchHunk{{Lines: []string{"package main"}}},
				SettledAt: settledAt(2),
			},
		}}},
	}))

	// Assert
	row := awaitRow(t, f, tail, "the write's settled tool card", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetSimpleToolCall().GetReturned().GetDiff() != nil
	})
	diff := row.GetActivity().GetSimpleToolCall().GetReturned().GetDiff()
	if len(diff.GetLines()) == 0 {
		t.Fatal("the write's diff output carries no lines, want the created file's hunk")
	}
}

func TestDiagnosticsBeforeTheirWriteCardProduceNoWarning(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-diagnostics-before-write", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act: the transcript-plane diagnostics arrive before the stream-plane card.
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("write-diagnostics-first"),
		Item: &conversationv1.AgentActivity_Write{Write: &conversationv1.AgentWrite{Result: &conversationv1.AgentWrite_Diagnostics{
			Diagnostics: &conversationv1.AgentDiagnosticsReport{Files: []*conversationv1.AgentDiagnosticsFile{{
				Path: "new.go",
				Diagnostics: []*conversationv1.AgentDiagnostic{{
					Severity:  conversationv1.AgentDiagnosticSeverity_AGENT_DIAGNOSTIC_SEVERITY_WARNING,
					Message:   "unused result",
					StartLine: 6,
				}},
			}}},
		}}},
	}))
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("write-diagnostics-first"),
		Item: &conversationv1.AgentActivity_Write{Write: &conversationv1.AgentWrite{Result: &conversationv1.AgentWrite_Success{
			Success: &conversationv1.AgentWriteSuccess{
				Path:      &conversationv1.ReadPath{Path: "new.go"},
				Outcome:   &conversationv1.AgentWriteSuccess_Created{Created: &conversationv1.AgentWriteCreated{}},
				Patch:     []*conversationv1.FilePatchHunk{{Lines: []string{"package main"}}},
				SettledAt: settledAt(2),
			},
		}}},
	}))

	// Assert: the retained attachment lands on the card without a WARN record.
	row := awaitRow(t, f, tail, "the write card carrying its early diagnostics", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetSimpleToolCall().GetReturned().GetDiagnostics() != nil
	})
	lines := row.GetActivity().GetSimpleToolCall().GetReturned().GetDiagnostics().GetLines()
	if len(lines) != 1 || lines[0] != "new.go:7 · warning · unused result" {
		t.Fatalf("the write card's diagnostics = %v, want the retained transcript attachment", lines)
	}
	for _, record := range f.d.WorkspaceLog(f.ws.GetDir(), "daemon") {
		if strings.EqualFold(record.Level, "warn") && record.Operation == "daemon.feed.diagnostics_without_card" {
			t.Fatalf("out-of-order diagnostics record = %+v, want no WARN", record)
		}
	}
}

func TestGlobToolCardDrawsLinesOutput(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-glob", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("glob-1"),
		Item: &conversationv1.AgentActivity_Glob{Glob: &conversationv1.AgentGlob{Result: &conversationv1.AgentGlob_Start{
			Start: &conversationv1.AgentGlobStart{Query: &conversationv1.AgentGlobQuery{Pattern: "*.go"}, StartedAt: startedAt(1)},
		}}},
	}))
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("glob-1"),
		Item: &conversationv1.AgentActivity_Glob{Glob: &conversationv1.AgentGlob{Result: &conversationv1.AgentGlob_Success{
			Success: &conversationv1.AgentGlobSuccess{
				Query:     &conversationv1.AgentGlobQuery{Pattern: "*.go"},
				Paths:     []string{"a.go", "b.go"},
				Extent:    &conversationv1.AgentGlobSuccess_All{All: &conversationv1.AgentGlobAll{FilesReturned: 2}},
				SettledAt: settledAt(2),
			},
		}}},
	}))

	// Assert
	row := awaitRow(t, f, tail, "the glob's settled tool card", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetSimpleToolCall().GetReturned().GetLines() != nil
	})
	if got := row.GetActivity().GetSimpleToolCall().GetReturned().GetLines().GetLines(); len(got) != 2 {
		t.Fatalf("the glob's lines output = %v, want the two matched paths", got)
	}
}

func TestAProgressFrameRepushesRunningLastProgress(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-progress", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("bash-prog"),
		Item: &conversationv1.AgentActivity_Bash{Bash: &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Start{
			Start: &conversationv1.AgentBashStart{Command: &conversationv1.AgentBashCommand{Line: "sleep 5"}, StartedAt: startedAt(1)},
		}}},
	}))

	// Act
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("bash-prog"),
		Item: &conversationv1.AgentActivity_Bash{Bash: &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Progress{
			Progress: &conversationv1.AgentToolCallProgress{LastProgressAtMs: 42},
		}}},
	}))

	// Assert
	row := awaitRow(t, f, tail, "the running call's re-pushed last progress", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetSimpleToolCall().GetRunning().GetLastProgress() != nil
	})
	if got := row.GetActivity().GetSimpleToolCall().GetRunning().GetLastProgress().GetAtMs(); got != 42 {
		t.Fatalf("the running card's last progress = %d, want 42", got)
	}
}

func TestADeniedPermissionDrawsTheToolCardAsDenied(t *testing.T) {
	t.Parallel()
	// Arrange: the ruled sequence. A gated call arrives as a START, the
	// permission unit settles DENIED, and the tool unit then reaches its
	// `failure` terminal with NO content, because nothing ran. The permission
	// id IS the gated unit's AgentActivityId, which is what joins the two.
	const unit = "gated-bash"
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-denied", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID(unit),
		Item: &conversationv1.AgentActivity_Bash{Bash: &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Start{
			Start: &conversationv1.AgentBashStart{Command: &conversationv1.AgentBashCommand{Line: "rm -rf /"}, StartedAt: startedAt(1)},
		}}},
	}))
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Permission{Permission: openPermission(unit, unit)},
	}))

	// Act: the denial, then the tool unit's contentless terminal.
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Permission{Permission: &conversationv1.AgentPermission{
			Id:        &conversationv1.AgentPermissionId{Value: unit},
			GatedCall: activityID(unit),
			Result: &conversationv1.AgentPermission_Success{Success: &conversationv1.AgentPermissionSuccess{
				Decision: &conversationv1.AgentPermissionSuccess_Denied{Denied: &conversationv1.AgentPermissionDenied{
					By: &conversationv1.AgentPermissionDenied_User{User: &conversationv1.AgentPermissionDeniedByUser{Message: "no"}},
				}},
			}},
		}},
	}))
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID(unit),
		Item: &conversationv1.AgentActivity_Bash{Bash: &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Failure{
			// The settle restates its command so it stands alone (the contract).
			Failure: &conversationv1.AgentBashFailure{
				Command: &conversationv1.AgentBashCommand{Line: "rm -rf /"},
				Error:   &conversationv1.AgentToolFailure{SettledAt: settledAt(2)},
			},
		}}},
	}))

	// Assert: the gated call's own card says DENIED, never a generic failure.
	row := awaitRow(t, f, tail, "the gated call's denied card", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetSimpleToolCall().GetDenied() != nil
	})
	if got := row.GetActivity().GetSimpleToolCall().GetReturned(); got != nil {
		t.Fatalf("the gated call's outcome = %v, want denied and never a returned failure", got)
	}
}

// ==========================================================================
// Skill card.
// ==========================================================================

// ==========================================================================
// The outgoing send: a SendMessage is an agent-addressed prompt, so the
// SENDER's feed draws it with the agent_prompt component.
// ==========================================================================

// ==========================================================================
// Plan mode.
// ==========================================================================

func TestPlanModeEnterThenExitCoalesceOntoOneFeedId(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-plan", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act: enter.
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("plan-enter"),
		Item: &conversationv1.AgentActivity_PlanMode{PlanMode: &conversationv1.AgentPlanMode{State: &conversationv1.AgentPlanMode_Start{
			Start: &conversationv1.AgentPlanModeStart{Act: &conversationv1.AgentPlanModeStart_Enter{Enter: &conversationv1.AgentPlanModeEnter{}}, StartedAt: startedAt(1)},
		}}},
	}))
	entered := awaitRow(t, f, tail, "the planning bubble", func(r *frontendv1.FeedRow) bool { return r.GetActivity().GetPlan() != nil })
	if entered.GetActivity().GetPlan().GetPlanning() == nil {
		t.Fatalf("the entered plan bubble's state = %v, want planning", entered.GetActivity().GetPlan())
	}
	id := entered.GetId().GetValue()
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("plan-enter"),
		Item: &conversationv1.AgentActivity_PlanMode{PlanMode: &conversationv1.AgentPlanMode{State: &conversationv1.AgentPlanMode_Success{
			Success: &conversationv1.AgentPlanModeSuccess{Act: &conversationv1.AgentPlanModeSuccess_Entered{Entered: &conversationv1.AgentPlanModeEntered{}}, SettledAt: settledAt(2)},
		}}},
	}))

	// Act: exit, a DIFFERENT tool call, presenting the plan.
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("plan-exit"),
		Item: &conversationv1.AgentActivity_PlanMode{PlanMode: &conversationv1.AgentPlanMode{State: &conversationv1.AgentPlanMode_Start{
			Start: &conversationv1.AgentPlanModeStart{Act: &conversationv1.AgentPlanModeStart_Exit{Exit: &conversationv1.AgentPlanModeExit{}}, StartedAt: startedAt(3)},
		}}},
	}))
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("plan-exit"),
		Item: &conversationv1.AgentActivity_PlanMode{PlanMode: &conversationv1.AgentPlanMode{State: &conversationv1.AgentPlanMode_Success{
			Success: &conversationv1.AgentPlanModeSuccess{
				Act: &conversationv1.AgentPlanModeSuccess_Exited{Exited: &conversationv1.AgentPlanModeExited{
					Plan: &conversationv1.AgentResponseProse{Markdown: "1. do it"},
				}},
				SettledAt: settledAt(4),
			},
		}}},
	}))

	// Assert: the exit fills the SAME bubble id the enter opened.
	planned := awaitRow(t, f, tail, "the presented plan on the same bubble", func(r *frontendv1.FeedRow) bool {
		return r.GetId().GetValue() == id && r.GetActivity().GetPlan().GetPlanned() != nil
	})
	if md := planned.GetActivity().GetPlan().GetPlanned().GetProse().GetProse().GetSource().GetMarkdown(); md != "1. do it" {
		t.Fatalf("the coalesced plan bubble's document = %q, want the exit's plan", md)
	}
}

// ==========================================================================
// Worktree separation dividers.
// ==========================================================================

func TestWorktreeExitDrawsASeparationDividerWithNoTokenDelta(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-wt-exit", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("wt-exit"),
		Item: &conversationv1.AgentActivity_Worktree{Worktree: &conversationv1.AgentWorktree{State: &conversationv1.AgentWorktree_Start{
			Start: &conversationv1.AgentWorktreeStart{Act: &conversationv1.AgentWorktreeStart_Exit{Exit: &conversationv1.AgentWorktreeExit{
				Action: &conversationv1.AgentWorktreeExit_Keep{Keep: &conversationv1.AgentWorktreeExitKeep{}},
			}}, StartedAt: startedAt(1)},
		}}},
	}))
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("wt-exit"),
		Item: &conversationv1.AgentActivity_Worktree{Worktree: &conversationv1.AgentWorktree{State: &conversationv1.AgentWorktree_Success{
			Success: &conversationv1.AgentWorktreeSuccess{
				Act: &conversationv1.AgentWorktreeSuccess_Exited{Exited: &conversationv1.AgentWorktreeExited{
					Outcome:     &conversationv1.AgentWorktreeExited_Kept{Kept: &conversationv1.AgentWorktreeKept{}},
					OriginalCwd: "/repo",
					Path:        "/tmp/wt-1",
				}},
				SettledAt: settledAt(2),
			},
		}}},
	}))

	// Assert
	row := awaitRow(t, f, tail, "the left-worktree divider", func(r *frontendv1.FeedRow) bool {
		return r.GetSeparation().GetWorktreeLeft() != nil
	})
	if row.GetSeparation().GetTokens() != nil {
		t.Fatalf("a worktree divider carries a token delta %v, want none", row.GetSeparation().GetTokens())
	}
	if row.GetSeparation().GetWorktreeLeft().GetKept() == nil {
		t.Fatalf("the left divider's outcome = %v, want kept", row.GetSeparation().GetWorktreeLeft())
	}
}

// ==========================================================================
// AgentUpdate.context_cut.
// ==========================================================================

// THE REAL /clear PATH DRAWS ONLY ITS BAR, AND A FOLLOW-UP PROMPT IS THE ONLY
// BUBBLE. A /clear submitted as a directive runs as its own turn: the shim lays
// down the turn's prompt, an empty "(no content)" response, the ContextCut, and
// an interrupted-by-user terminal. None of those may draw a bubble — only the
// cleared divider stands. A normal prompt sent after it is held behind the
// uninterruptible clear and released when the clear ends, so its presence proves
// the whole clear turn routed; it must be the ONLY prompt bubble, drawn below
// the bar (the owner saw the /clear bubble render after such a follow-up).
func TestAClearDrawsOnlyItsBarThenTheFollowUpPromptOnTheRealPath(t *testing.T) {
	t.Parallel()
	// Arrange: a /clear run as a directive turn.
	f := newOpened(t, harness.Opts{})
	cutResp := f.submit("/clear", "k-clear-realpath", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	clearTurn := cutResp.GetSuccess().GetTurn().GetTurn()
	if clearTurn.GetValue() == "" {
		t.Fatalf("SubmitPrompt(/clear) = %v, want the minted turn", cutResp)
	}
	clearPrompt := &conversationv1.AgentPrompt{
		Id:     clearTurn,
		Agent:  &conversationv1.AgentId{Value: mainAgent},
		Said:   said("/clear"),
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT,
	}
	emptyResponse := activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("resp-clear"),
		Item: &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{
			Result: &conversationv1.AgentResponse_Success{Success: &conversationv1.AgentResponseSuccess{
				Prose: &conversationv1.AgentResponseProse{Markdown: ""},
			}},
		}},
	})
	cleared := updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_ContextCut{ContextCut: &conversationv1.ContextCut{
			Cut: &conversationv1.ContextCut_Cleared{Cleared: &conversationv1.ContextCleared{}},
		}},
	})

	// Act: the shim lays the whole /clear turn down, then a normal prompt is
	// sent (held behind the uninterruptible clear, released when it ends).
	f.shim.PushUserPrompt(mainAgent, clearPrompt)
	f.shim.PushAgentFrame(mainAgent, emptyResponse)
	f.shim.PushAgentFrame(mainAgent, cleared)
	f.shim.PushAgentFrame(mainAgent, interruptedFrame(mainAgent))
	f.submit("hello", "k-after-clear", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)

	// Assert: open the feed once it carries the follow-up prompt — which is drawn
	// only after the clear turn ended, so every /clear frame has routed — and
	// census the delivered page.
	page, _ := f.openFeedOnceCarrying("the follow-up prompt after the clear", func(p *frontendv1.FeedPage) bool {
		for _, r := range p.GetSuccess().GetRows() {
			if promptText(r) == "hello" {
				return true
			}
		}
		return false
	})
	dividers, prompts, responses, terminals := 0, 0, 0, 0
	for _, r := range page.GetSuccess().GetRows() {
		if r.GetSeparation().GetCleared() != nil {
			dividers++
		}
		if r.GetUserPrompt() != nil {
			prompts++
		}
		if r.GetActivity().GetResponse() != nil {
			responses++
		}
		if r.GetTurnEnded().GetInterrupted() != nil {
			terminals++
		}
	}
	if dividers != 1 {
		t.Fatalf("cleared dividers = %d, want exactly 1", dividers)
	}
	if prompts != 1 {
		t.Fatalf("user-prompt rows = %d, want only the follow-up 'hello' (the /clear draws none)", prompts)
	}
	if responses != 0 {
		t.Fatalf("response rows = %d, want none — the /clear's empty response draws no cut-short bubble", responses)
	}
	if terminals != 0 {
		t.Fatalf("interrupted terminals = %d, want none for a /clear", terminals)
	}
}

// ==========================================================================
// Permission and question cards.
// ==========================================================================

func TestPermissionStartDrawsOpenRowFooterAndBanner(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-permstart", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	footer := f.d.WatchFooter(f.ws)

	// Act
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Permission{Permission: openPermission("perm-open", "gated-1")},
	}))

	// Assert
	row := awaitRow(t, f, tail, "the open permission card", func(r *frontendv1.FeedRow) bool {
		return r.GetPermission().GetOpen() != nil
	})
	if row.GetPermission().GetHeadline().GetText() == "" {
		t.Fatal("the open permission card carries no headline")
	}
	awaitFooter(t, f, footer, "footer permission", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetPermission() != nil
	})
	f.d.AwaitLogRecord(f.d.RunLogPath(), "the permission ask's desktop banner", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.desktopnotify.post" && r.Message == "posting a desktop banner" &&
			r.Context["kind"] == "permission_requested"
	})
}

func TestQuestionAnsweredRepushesWithEchoedLabels(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-qanswer", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Question{Question: &conversationv1.AgentQuestion{
			Id: &conversationv1.AgentQuestionId{Value: "q-2"},
			Result: &conversationv1.AgentQuestion_Start{Start: &conversationv1.AgentQuestionStart{
				Batch: &conversationv1.AgentQuestionBatch{Questions: []*conversationv1.AgentQuestionAsked{{
					Question: &conversationv1.AgentQuestionText{Text: "Which env?"},
					Header:   "Env",
					Choices: &conversationv1.AgentQuestionAsked_SingleSelect{SingleSelect: &conversationv1.AgentQuestionSingleSelect{
						Options: []*conversationv1.AgentQuestionOption{{Label: &conversationv1.AgentQuestionOptionLabel{Label: "prod"}}},
					}},
				}}},
				StartedAt: startedAt(1),
			}},
		}},
	}))
	opened := awaitRow(t, f, tail, "the open question", func(r *frontendv1.FeedRow) bool { return r.GetQuestion().GetOpen() != nil })

	// Act
	f.shim.PushAgentFrame(mainAgent, updateFrame(mainAgent, &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_Question{Question: &conversationv1.AgentQuestion{
			Id: &conversationv1.AgentQuestionId{Value: "q-2"},
			Result: &conversationv1.AgentQuestion_Success{Success: &conversationv1.AgentQuestionSuccess{
				Batch: &conversationv1.AgentQuestionBatch{Questions: []*conversationv1.AgentQuestionAsked{{
					Question: &conversationv1.AgentQuestionText{Text: "Which env?"}, Header: "Env",
				}}},
				Outcome: &conversationv1.AgentQuestionSuccess_Answered{Answered: &conversationv1.AgentQuestionAnswers{
					Answers: []*conversationv1.AgentQuestionSelection{{
						Question: &conversationv1.AgentQuestionText{Text: "Which env?"},
						Chosen:   []*conversationv1.AgentQuestionChoice{{Label: &conversationv1.AgentQuestionOptionLabel{Label: "prod"}}},
					}},
				}},
			}},
		}},
	}))

	// Assert
	answered := awaitRow(t, f, tail, "the answered question on the same row", func(r *frontendv1.FeedRow) bool {
		return r.GetId().GetValue() == opened.GetId().GetValue() && r.GetQuestion().GetAnswered() != nil
	})
	got := answered.GetQuestion().GetAnswered().GetAnswers()
	if len(got) != 1 || len(got[0].GetChosen()) != 1 || got[0].GetChosen()[0] != "prod" {
		t.Fatalf("the answered question's echoed choices = %v, want [\"prod\"]", got)
	}
}

// ==========================================================================
// Subagents (sync and detached).
// ==========================================================================

func TestASyncSubagentSpawnDrawsABubbleHeadAndItsOwnFeedServesSubFeedRows(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-subagent", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act: spawn.
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("spawn-1"),
		Item: &conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{Result: &conversationv1.AgentSubagent_Start{
			Start: &conversationv1.AgentSubagentStart{
				CreatedAgentId: &conversationv1.AgentId{Value: "sub-1"},
				Prompt:         &conversationv1.AgentSubagentPrompt{Text: "explore the code", Description: proto.String("Explore the code")},
				StartedAt:      startedAt(1),
			},
		}}},
	}))
	bubble := awaitRow(t, f, tail, "the spawn's bubble head", func(r *frontendv1.FeedRow) bool { return r.GetActivity().GetSubagent() != nil })
	if bubble.GetActivity().GetSubagent().GetDescription().GetText() != "Explore the code" {
		t.Fatalf("the bubble's description = %q, want the commission's", bubble.GetActivity().GetSubagent().GetDescription().GetText())
	}

	// Act: a frame carrying the created agent's own id — routed to the sub-feed.
	f.shim.PushAgentFrame("sub-1", activityFrame("sub-1", &conversationv1.AgentActivity{
		ActivityId: activityID("sub-work-1"),
		Item: &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{Result: &conversationv1.AgentResponse_Success{
			Success: &conversationv1.AgentResponseSuccess{Prose: &conversationv1.AgentResponseProse{Markdown: "subagent said this"}},
		}}},
	}))

	// Assert: OpenFeed on the bubble's own FeedId serves the sub-feed, which
	// carries the frame addressed to the created agent.
	// The pin puts a row on EXACTLY ONE side of the seam: whichever side the
	// subagent's frame landed on, it is drawn once and never twice.
	isWork := func(r *frontendv1.FeedRow) bool { return r.GetActivity().GetResponse().GetSuccess() != nil }
	subPage, subToken := f.openFeed(bubble.GetId())
	subRow := findRow(subPage, isWork)
	if subRow == nil {
		subTail := f.d.WatchFeed(subToken)
		subRow = awaitRow(t, f, subTail, "the subagent's own work on its sub-feed", isWork)
	}
	if md := subRow.GetActivity().GetResponse().GetSuccess().GetProse().GetProse().GetSource().GetMarkdown(); md != "subagent said this" {
		t.Fatalf("the sub-feed's row = %q, want the subagent's own prose", md)
	}
}

func TestADetachedSubagentGetsDetachedSubagentAndItsOwnWatchAgentEagerly(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name string
		// detach pushes the frames that make the subagent detached work.
		detach func(f *fixture)
		// agent is the running agent the announcement names.
		agent string
		// work is the announcement's handle.
		work string
	}{
		{
			name: "a subagent spawned in the background by the Agent tool",
			detach: func(f *fixture) {
				f.shim.PushAgentFrame(mainAgent, detachedWorkFrame(mainAgent, detachedSubagent("work-sub-1", "sub-detached-1", "roam free")))
			},
			agent: "sub-detached-1",
			work:  "work-sub-1",
		},
		{
			// 2026-09-27: the resumed agent is detached from the SEND, whose
			// unit teaches the daemon no kind and no agent; the announcement's
			// own kind is what routes it.
			name: "a subagent resumed by SendMessage",
			detach: func(f *fixture) {
				f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
					ActivityId: activityID("send-resume-1"),
					Item: &conversationv1.AgentActivity_SendMessage{SendMessage: &conversationv1.AgentSendMessage{Result: &conversationv1.AgentSendMessage_Start{
						Start: &conversationv1.AgentSendMessageStart{AddressedTo: "a5583c88f8f4d5a90", StartedAt: startedAt(1)},
					}}},
				}))
				f.shim.PushAgentFrame(mainAgent, detachedWorkFrame(mainAgent, resumedSubagent("send-resume-1", "send-resume-1", "sub-resumed-1")))
				// The resumed run's running beat rides the send's unit and
				// names no agent of its own.
				f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
					ActivityId: activityID("send-resume-1"),
					Item: &conversationv1.AgentActivity_Subagent{Subagent: &conversationv1.AgentSubagent{
						Result: &conversationv1.AgentSubagent_Update{Update: &conversationv1.AgentSubagentUpdate{
							Prompt:   &conversationv1.AgentSubagentPrompt{Text: "resume the sweep"},
							Progress: &conversationv1.AgentSubagentProgress{TotalTokens: 1_200},
						}},
					}},
				}))
			},
			agent: "sub-resumed-1",
			work:  "send-resume-1",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			// Arrange
			f := newOpened(t, harness.Opts{})
			f.submit("go", "k-detachsub", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
			tail := f.watchRootFeed()
			footer := f.d.WatchFooter(f.ws)

			// Act
			tt.detach(f)

			// Assert: the fake saw WatchAgent for the running agent BEFORE
			// this test ever calls OpenFeed on its bubble.
			watched := f.shim.ExpectWatchAgentFor(tt.agent)
			if watched.GetTarget().GetValue() != tt.agent {
				t.Fatalf("the eager WatchAgent named %q, want the detached subagent's id %q", watched.GetTarget().GetValue(), tt.agent)
			}
			row := awaitRow(t, f, tail, "the detached_subagent row", func(r *frontendv1.FeedRow) bool { return r.GetDetachedSubagent() != nil })
			if got := row.GetDetachedSubagent().GetSubagent().GetWorkId().GetText(); got != tt.work {
				t.Fatalf("the detached bubble's work id = %q, want %q", got, tt.work)
			}
			// The footer is handed the agent as live work, which is what its
			// expanded section lists.
			awaitFooter(t, f, footer, "the running agent on the live-work chip", func(v *frontendv1.FooterView) bool {
				return v.GetStrip().GetLiveWork().GetAgents().GetCount() == 1
			})
		})
	}
}

// ==========================================================================
// Detached bash.
// ==========================================================================

// ==========================================================================
// Turn terminal.
// ==========================================================================

// A TURN THE VENDOR STARTED ON ITS OWN IS A REAL TURN. The shim adopts it and
// opens it with a VENDOR_STARTED prompt row: the daemon stands it in flight, a
// prompt sent meanwhile is held behind it rather than started into it, and its
// stamped terminal ends it through the ordinary path -- the concluded row names
// the answering response, and only then is the held prompt delivered.
func TestAVendorStartedTurnHoldsAPromptAndItsEndStampsTheAnsweringResponse(t *testing.T) {
	t.Parallel()
	// Arrange: the vendor starts a turn; a prompt is sent while it runs.
	f := newOpened(t, harness.Opts{})
	const vendorTurn = "turn-vendor-started"
	tail := f.watchRootFeed()
	holds := f.d.WatchHolds(f.ws)
	f.shim.PushUserPrompt(mainAgent, &conversationv1.AgentPrompt{
		Id:     &conversationv1.TurnId{Value: vendorTurn},
		Agent:  &conversationv1.AgentId{Value: mainAgent},
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_VENDOR_STARTED,
	})
	f.d.AwaitWorkspaceLogRecord(f.repo.Dir, "the queue's INFO record of the adopted turn", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.promptqueue.turn_adopted" && r.Level == "info" && r.Context["turn"] == vendorTurn
	})
	held := f.submit("the held prompt", "k-behind-vendor", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT).GetSuccess().GetTurn().GetTurn()
	awaitView(t, f, holds, "the prompt held for the vendor turn's end", func(tray *frontendv1.FeedHeld) bool {
		return promptHeldEntry(tray, held).GetHoldForTurnEnd() != nil
	})
	if n := f.shim.Count(harness.RPCStartTurn); n != 0 {
		t.Fatalf("StartTurn count = %d while the vendor turn runs, want the prompt held", n)
	}
	f.shim.PushAgentFrameIn(mainAgent, vendorTurn, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("resp-vendor-answer"),
		Item: &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{Result: &conversationv1.AgentResponse_Success{
			Success: &conversationv1.AgentResponseSuccess{Prose: &conversationv1.AgentResponseProse{Markdown: "the hand-back is in"}},
		}}},
	}))
	answer := awaitRow(t, f, tail, "the vendor turn's answering response", func(r *frontendv1.FeedRow) bool { return r.GetActivity().GetResponse().GetSuccess() != nil })

	// Act
	f.shim.PushAgentFrameIn(mainAgent, vendorTurn, successFrame(mainAgent, activityID("resp-vendor-answer")))

	// Assert: the turn's end marks and names its answer, and the held prompt
	// goes next.
	awaitRow(t, f, tail, "the answering response marked as the final answer", func(r *frontendv1.FeedRow) bool {
		return r.GetId().GetValue() == answer.GetId().GetValue() && r.GetActivity().GetResponse().GetFinalAnswer()
	})
	terminal := awaitRow(t, f, tail, "the vendor turn's terminal row", func(r *frontendv1.FeedRow) bool { return r.GetTurnEnded().GetConcluded() != nil })
	if terminal.GetTurn().GetValue() != vendorTurn {
		t.Fatalf("terminal row turn = %q, want %q", terminal.GetTurn().GetValue(), vendorTurn)
	}
	if terminal.GetTurnEnded().GetConcluded().GetAnswer().GetValue() != answer.GetId().GetValue() {
		t.Fatalf("the concluded terminal's answer = %v, want it to name the answering response %v", terminal.GetTurnEnded().GetConcluded().GetAnswer(), answer.GetId())
	}
	if got := text(f.shim.ExpectStartTurn().GetSaid()); got != "the held prompt" {
		t.Fatalf("the StartTurn after the vendor turn said %q, want the held prompt", got)
	}
}

// A PROMPT RACING A VENDOR-STARTED TURN RUNS BEHIND IT (ruled 2026-09-28).
// The prompt was delivered and accepted, and only then did the shim announce a
// turn the vendor started on its own: the shim matches each reply to its send
// by id and never refuses the prompt, so the vendor's turn runs first and the
// prompt's turn after it. The vendor turn's end must not end the prompt's turn
// nor pop a held prompt into it; the prompt's own stamped terminal ends it, and
// only then is the held prompt delivered.
func TestAPromptRacingAVendorStartedTurnEndsOnItsOwnTerminalAndHoldsTheNextBehindIt(t *testing.T) {
	t.Parallel()
	// Arrange: the racing prompt is accepted, the vendor's own turn is
	// announced behind its back, and a second prompt is sent while both run.
	f := newOpened(t, harness.Opts{})
	const vendorTurn = "turn-vendor-raced"
	tail := f.watchRootFeed()
	holds := f.d.WatchHolds(f.ws)
	racing := f.submit("the racing prompt", "k-racing", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT).GetSuccess().GetTurn().GetTurn().GetValue()
	if got := text(f.shim.ExpectStartTurn().GetSaid()); got != "the racing prompt" {
		t.Fatalf("the first StartTurn said %q, want the racing prompt", got)
	}
	f.shim.PushUserPrompt(mainAgent, &conversationv1.AgentPrompt{
		Id:     &conversationv1.TurnId{Value: vendorTurn},
		Agent:  &conversationv1.AgentId{Value: mainAgent},
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_VENDOR_STARTED,
	})
	f.d.AwaitWorkspaceLogRecord(f.repo.Dir, "the queue's INFO record of the adopted turn", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.promptqueue.turn_adopted" && r.Level == "info" && r.Context["turn"] == vendorTurn
	})
	held := f.submit("the held prompt", "k-behind-both", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT).GetSuccess().GetTurn().GetTurn()
	awaitView(t, f, holds, "the prompt held for a turn end", func(tray *frontendv1.FeedHeld) bool {
		return promptHeldEntry(tray, held).GetHoldForTurnEnd() != nil
	})
	f.shim.PushAgentFrameIn(mainAgent, vendorTurn, successFrame(mainAgent, nil))
	vendorEnd := awaitRow(t, f, tail, "the vendor turn's terminal row", func(r *frontendv1.FeedRow) bool {
		return r.GetTurnEnded() != nil && r.GetTurn().GetValue() == vendorTurn
	})
	f.d.AwaitWorkspaceLogRecord(f.repo.Dir, "the queue's INFO record that the vendor turn ended while the racing turn runs", func(r harness.LogRecord) bool {
		return r.Operation == harness.OpTurnEnded && r.Level == "info" && r.Context["turn_in_flight"] == racing
	})
	startsAfterVendorEnd := f.shim.Count(harness.RPCStartTurn)

	// Act
	f.shim.PushAgentFrameIn(mainAgent, racing, successFrame(mainAgent, nil))

	// Assert
	racingEnd := awaitRow(t, f, tail, "the racing turn's terminal row", func(r *frontendv1.FeedRow) bool {
		return r.GetTurnEnded() != nil && r.GetTurn().GetValue() == racing
	})
	if vendorEnd.GetTurnEnded().GetConcluded() == nil || racingEnd.GetTurnEnded().GetConcluded() == nil {
		t.Fatalf("terminal rows = (%v, %v), want both turns concluded by their own terminals", vendorEnd.GetTurnEnded(), racingEnd.GetTurnEnded())
	}
	if startsAfterVendorEnd != 1 {
		t.Fatalf("StartTurn count after the vendor turn ended = %d, want 1: nothing popped into the racing turn", startsAfterVendorEnd)
	}
	if got := text(f.shim.ExpectStartTurn().GetSaid()); got != "the held prompt" {
		t.Fatalf("the StartTurn after the racing turn ended said %q, want the held prompt", got)
	}
}

// A TERMINAL ENDS THE TURN ITS STAMP NAMES. A stray terminal stamped with a
// turn nobody opened arrives first; it must not end the open turn, and the
// open turn's own stamped terminal is the one that draws its end.
func TestAStampedTerminalEndsOnlyTheTurnItNames(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.d.ExpectWarnings("daemon.sessionwatcher.terminal_turn_unknown")
	turn := f.submit("go", "k-stamped", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT).GetSuccess().GetTurn().GetTurn().GetValue()
	tail := f.watchRootFeed()
	f.shim.PushAgentFrameIn(mainAgent, "turn-nobody-opened", interruptedFrame(mainAgent))

	// Act
	f.shim.PushAgentFrameIn(mainAgent, turn, successFrame(mainAgent, nil))

	// Assert
	row := awaitRow(t, f, tail, "the first terminal row", func(r *frontendv1.FeedRow) bool { return r.GetTurnEnded() != nil })
	if row.GetTurnEnded().GetConcluded() == nil {
		t.Fatalf("the first terminal row = %v, want the open turn's own concluded end", row.GetTurnEnded())
	}
	if row.GetTurn().GetValue() != turn {
		t.Fatalf("terminal row turn = %q, want %q", row.GetTurn().GetValue(), turn)
	}
}

func TestApiRequestFailedInvalidRequestRespellsAsARefusal(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-refusal", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act: the vendor refused the request outright as malformed.
	f.shim.PushAgentFrame(mainAgent, failureFrame(mainAgent, &conversationv1.AgentFailure{
		Failure: &conversationv1.AgentFailure_ApiRequestFailed{ApiRequestFailed: &conversationv1.ApiRequestFailed{
			Message: "the request was refused",
			Kind:    &conversationv1.ApiRequestFailed_InvalidRequest{InvalidRequest: &conversationv1.ApiInvalidRequest{}},
		}},
	}))

	// Assert
	row := awaitRow(t, f, tail, "the invalid-request (refusal) terminal", func(r *frontendv1.FeedRow) bool {
		return r.GetTurnEnded().GetErrored().GetInvalidRequest() != nil
	})
	if row.GetTurnEnded().GetErrored().GetHeadline().GetText() == "" {
		t.Fatal("the refused terminal carries no composed headline")
	}
}

// THE RUN'S OWN TERMINALS (landing 8). One end-to-end path per arm family:
// a FailureVendor* arm, which also resolves the workspace purple, and the
// Stop-hook arm, which does not.

func TestQueryDiedDrawsTheTurnsTerminal(t *testing.T) {
	t.Parallel()
	// Arrange: a turn in flight when the query dies out from under it.
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-querydied", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act: the shim reports the session's query died, rather than the agent
	// stream ending with its own success/failure frame.
	f.shim.PushSessionUpdate(&conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_QueryDied{QueryDied: &conversationv1.SessionQueryDied{
			Cause: &conversationv1.SessionQueryDied_UnexpectedEof{UnexpectedEof: &conversationv1.SessionQueryUnexpectedEof{}},
		}},
	})

	// Assert
	row := awaitRow(t, f, tail, "the query-died terminal", func(r *frontendv1.FeedRow) bool {
		return r.GetTurnEnded().GetErrored().GetQueryDied() != nil
	})
	if row.GetTurnEnded().GetErrored().GetHeadline().GetText() == "" {
		t.Fatal("the query-died terminal carries no composed headline")
	}
	// The query's death is logged from two places per route.go/turnended.go:
	// the session watcher's own routing (daemon.sessionwatcher.query_died,
	// ERROR) and the feed resolver's terminal draw (daemon.feed.query_died,
	// WARN).
	f.d.ExpectWarnings("daemon.sessionwatcher.query_died", "daemon.feed.query_died")
}

// ==========================================================================
// Hooks.
// ==========================================================================

// ==========================================================================
// Artifacts.
// ==========================================================================

func TestArtifactListDrawsNothing(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-artifactlist", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("artifact-list-1"),
		Item: &conversationv1.AgentActivity_Artifact{Artifact: &conversationv1.AgentArtifact{Result: &conversationv1.AgentArtifact_Start{
			Start: &conversationv1.AgentArtifactStart{Act: &conversationv1.AgentArtifactStart_List{List: &conversationv1.AgentArtifactList{}}, StartedAtMs: 1},
		}}},
	}))
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("artifact-list-1"),
		Item: &conversationv1.AgentActivity_Artifact{Artifact: &conversationv1.AgentArtifact{Result: &conversationv1.AgentArtifact_Success{
			Success: &conversationv1.AgentArtifactSuccess{Outcome: &conversationv1.AgentArtifactSuccess_Listed{Listed: &conversationv1.AgentArtifactListed{}}},
		}}},
	}))

	// Assert
	harness.ExpectNoPush(t, tail, harness.ProbeWindow, "a list act produces no feed row")
}

// ==========================================================================
// Findings.
// ==========================================================================

// ==========================================================================
// MCP tools: ordinary tool calls.
// ==========================================================================

func TestAnMcpToolCallDrawsTheOrdinaryToolCardAndNoTopbarWarning(t *testing.T) {
	t.Parallel()
	// Arrange: no unmodeled_activity warning is declared; the sweep fails the
	// test if one is recorded.
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-mcp", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	topbar := f.d.WatchTopbar(f.ws)
	tool := &conversationv1.AgentMcpTool{Name: "mcp__claude-in-chrome__tabs_context_mcp"}

	// Act
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("mcp-1"),
		Item: &conversationv1.AgentActivity_McpToolCall{McpToolCall: &conversationv1.AgentMcpToolCall{Result: &conversationv1.AgentMcpToolCall_Start{
			Start: &conversationv1.AgentMcpToolCallStart{Tool: tool, StartedAt: startedAt(1)},
		}}},
	}))
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("mcp-1"),
		Item: &conversationv1.AgentActivity_McpToolCall{McpToolCall: &conversationv1.AgentMcpToolCall{Result: &conversationv1.AgentMcpToolCall_Success{
			Success: &conversationv1.AgentMcpToolCallSuccess{
				Tool: tool,
				Content: &conversationv1.ToolResultContent{Blocks: []*conversationv1.ToolResultContentBlock{{
					Block: &conversationv1.ToolResultContentBlock_Text{Text: &conversationv1.TextBlock{Text: "Tab Context: 1 tab"}},
				}}},
				SettledAt: settledAt(2),
			},
		}}},
	}))

	// Assert: the one shared tool card, headed by the tool, with its text.
	row := awaitRow(t, f, tail, "the MCP call's settled tool card", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetSimpleToolCall().GetReturned().GetForm() != nil
	})
	card := row.GetActivity().GetSimpleToolCall()
	if card.GetName().GetText() != "mcp__claude-in-chrome__tabs_context_mcp" || card.GetReturned().GetText().GetText() != "Tab Context: 1 tab" {
		t.Fatalf("card = %v, want the tool's name and its returned text", card)
	}
	view := awaitTopbar(t, f, topbar, "the topbar after the MCP call", func(v *frontendv1.TopbarView) bool { return v != nil })
	if n := len(view.GetWarnings().GetWarnings()); n != 0 {
		t.Fatalf("topbar warnings = %d, want none for an MCP tool call", n)
	}
}

// ==========================================================================
// Unmodeled tools.
// ==========================================================================

// ==========================================================================
// Image blocks — the daemon resolves the record's reference, end to end.
// ==========================================================================

func TestAPromptsImageIsDrawnAsAnImageRowRatherThanAnUnsupportedBlock(t *testing.T) {
	t.Parallel()
	// Arrange: a session watching its main agent, and a pasted image — which
	// the shim carries on ImageBlock's url arm as a data url, the only arm any
	// producer emits.
	f := newOpened(t, harness.Opts{})
	feed := f.watchRootFeed()
	const src = "data:image/png;base64,aGk="

	// Act: the delivered prompt reaches the feed on the agent watch.
	f.shim.PushUserPrompt(mainAgent, &conversationv1.AgentPrompt{
		Id:     &conversationv1.TurnId{Value: "turn-image"},
		Agent:  &conversationv1.AgentId{Value: mainAgent},
		Origin: origin,
		Said: &conversationv1.UserSaid{Content: &conversationv1.UserContent{
			Blocks: []*conversationv1.UserContentBlock{{
				Block: &conversationv1.UserContentBlock_Image{Image: &conversationv1.ImageBlock{
					Location:  &conversationv1.ImageBlock_Url{Url: &conversationv1.ImageBlockUrl{Url: src}},
					MediaType: "image/png",
				}},
			}},
		}},
	})

	// Assert: the block reaches the row on the IMAGE arm. The whole daemon is
	// wired here, so this is the composition root's own resolver answering.
	row := awaitRow(t, f, feed, "the drawn prompt row carrying the image", func(r *frontendv1.FeedRow) bool {
		return r.GetUserPrompt() != nil && r.GetTurn().GetValue() == "turn-image"
	})
	blocks := row.GetUserPrompt().GetSuccess().GetBody().GetBlocks()
	if len(blocks) != 1 || blocks[0].GetImage().GetSrc() != src {
		t.Fatalf("blocks = %v, want one image block whose src is the resolved reference %q", blocks, src)
	}
}

// ==========================================================================
// A retired entry: the store no longer converts its record to that line.
// ==========================================================================

// ==========================================================================
// feed-suite-local helpers.
// ==========================================================================

// walkPageMargin is how far past harness.FeedPageSize a walk test pushes, so a
// second (and, for the at-start tests, a distinct oldest) page provably
// exists regardless of what the page size is.
const walkPageMargin = 20

// feedResponseFrames builds the start+success frame pair for one settled
// response unit, so callers can push a whole row in two calls.
func feedResponseFrames(activityIDValue, markdown string) [2]*conversationv1.AgentFrame {
	return [2]*conversationv1.AgentFrame{
		activityFrame(mainAgent, &conversationv1.AgentActivity{
			ActivityId: activityID(activityIDValue),
			Item:       &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{Result: &conversationv1.AgentResponse_Start{Start: &conversationv1.AgentResponseStart{}}}},
		}),
		activityFrame(mainAgent, &conversationv1.AgentActivity{
			ActivityId: activityID(activityIDValue),
			Item: &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{Result: &conversationv1.AgentResponse_Success{
				Success: &conversationv1.AgentResponseSuccess{Prose: &conversationv1.AgentResponseProse{Markdown: markdown}},
			}}},
		}),
	}
}

// feedRowLabeledResponse builds one single-frame settled response row, used
// to pad a feed's history past whatever page size the daemon uses.
func feedRowLabeledResponse(i int) *conversationv1.AgentFrame {
	id := "wall-" + itoa(i)
	return activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID(id),
		Item: &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{Result: &conversationv1.AgentResponse_Success{
			Success: &conversationv1.AgentResponseSuccess{Prose: &conversationv1.AgentResponseProse{Markdown: "row " + itoa(i)}},
		}}},
	})
}

// feedRowLabeledResponseFor is feedRowLabeledResponse addressed to an
// arbitrary agent, so a sub-feed can be padded past one page independently of
// the root feed.
func feedRowLabeledResponseFor(agent string, i int) *conversationv1.AgentFrame {
	id := agent + "-wall-" + itoa(i)
	return activityFrame(agent, &conversationv1.AgentActivity{
		ActivityId: activityID(id),
		Item: &conversationv1.AgentActivity_Response{Response: &conversationv1.AgentResponse{Result: &conversationv1.AgentResponse_Success{
			Success: &conversationv1.AgentResponseSuccess{Prose: &conversationv1.AgentResponseProse{Markdown: agent + " row " + itoa(i)}},
		}}},
	})
}

// secondWorkspaceOn opens a SECOND, independent workspace on the SAME daemon
// as an existing fixture — used only to prove a refusal is scoped to the
// addressed workspace, never to the reader's own.
func secondWorkspaceOn(t *testing.T, d *harness.Daemon) *fixture {
	t.Helper()
	repo := harness.NewRepo(t)
	ws := harness.Register(t, d, repo.Dir)
	other := &fixture{d: d, repo: repo, ws: ws, t: t}
	other.open()
	other.host = d.WatchHost(ws)
	other.web = d.WatchWeb(ws)
	return other
}

// itoa avoids importing strconv solely for this suite's synthetic row labels.
func itoa(i int) string {
	if i == 0 {
		return "0"
	}
	neg := i < 0
	if neg {
		i = -i
	}
	var buf [20]byte
	pos := len(buf)
	for i > 0 {
		pos--
		buf[pos] = byte('0' + i%10)
		i /= 10
	}
	if neg {
		pos--
		buf[pos] = '-'
	}
	return string(buf[pos:])
}

// findRow answers a page's first row satisfying the predicate, or nil.
func findRow(page *frontendv1.FeedPage, pred func(*frontendv1.FeedRow) bool) *frontendv1.FeedRow {
	for _, r := range page.GetSuccess().GetRows() {
		if pred(r) {
			return r
		}
	}
	return nil
}

// TestWatchFeedWithATokenWhosePinnedStartIsGoneIsRefusedAtTheTransport covers
// the OTHER Watch* refusal a feed token can meet: the token was minted by this
// daemon, but so many rows have been published since that its pinned start has
// fallen out of the retained log, and replaying the tail from it would silently
// skip everything in between. The reader is told to re-open the feed instead.
//
// The retention is compressed to one row through the daemon's own knob, which
// is what makes the refusal reachable at all: at the built-in 4096 no test
// could publish its way past the pin.
func TestWatchFeedWithATokenWhosePinnedStartIsGoneIsRefusedAtTheTransport(t *testing.T) {
	t.Parallel()
	// Arrange: a daemon retaining exactly one published row per feed, and a
	// token minted against a page opened before anything else lands.
	f := newOpened(t, harness.Opts{ExtraEnv: []string{"AGENT_REPL_FEED_TAIL_RETENTION=1"}})
	// The sweep covers every test; the declared records are evidence of a refusal the test provokes.
	f.d.ExpectWarnings("daemon.feed.tail_token_expired")
	f.submit("go", "k-expire", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	f.shim.PushAgentFrame(mainAgent, feedResponseFrames("resp-1", "first")[0])
	f.shim.PushAgentFrame(mainAgent, feedResponseFrames("resp-1", "first")[1])
	_, token := f.openFeedOnceCarrying("resp-1's settled prose", func(p *frontendv1.FeedPage) bool {
		for _, r := range p.GetSuccess().GetRows() {
			if r.GetActivity().GetResponse().GetSuccess().GetProse().GetProse().GetSource().GetMarkdown() == "first" {
				return true
			}
		}
		return false
	})

	// Act: publish past the pin, then open the tail on the stale token. The
	// second page open is the synchronization: it answers only once the daemon
	// has routed the frames that pushed the pin out of the retained log.
	f.shim.PushAgentFrame(mainAgent, feedResponseFrames("resp-2", "second")[0])
	f.shim.PushAgentFrame(mainAgent, feedResponseFrames("resp-2", "second")[1])
	f.shim.PushAgentFrame(mainAgent, feedResponseFrames("resp-3", "third")[0])
	f.shim.PushAgentFrame(mainAgent, feedResponseFrames("resp-3", "third")[1])
	f.openFeedOnceCarrying("resp-3's settled prose", func(p *frontendv1.FeedPage) bool {
		for _, r := range p.GetSuccess().GetRows() {
			if r.GetActivity().GetResponse().GetSuccess().GetProse().GetProse().GetSource().GetMarkdown() == "third" {
				return true
			}
		}
		return false
	})
	stream, err := f.d.Client().WatchFeed(f.d.Ctx(), connect.NewRequest(&agentreplv1.WatchFeedRequest{Watch: token}))

	// Assert: the stream never opens.
	if err == nil {
		if stream.Receive() {
			t.Fatalf("WatchFeed(expired token) delivered a row %v, want a transport refusal", stream.Msg())
		}
		err = stream.Err()
	}
	if err == nil {
		t.Fatal("WatchFeed(expired token) = success, want a transport-level refusal")
	}
	rec := f.d.AwaitWorkspaceLogRecord(f.ws.GetDir(), "the token_expired transport close", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.refusal.transport_closed" && r.Context["cause"] == "token_expired"
	})
	if !strings.EqualFold(rec.Level, "info") {
		t.Fatalf("the transport-closed record = level %q, want INFO", rec.Level)
	}
	if rec.Context["rpc"] != "WatchFeed" {
		t.Fatalf("the transport-closed record's context = %v, want rpc WatchFeed", rec.Context)
	}
}

// ==========================================================================
// A shell whose work MOVED to the background.
// ==========================================================================

// A BACKGROUNDED COMMAND DID NOT END, IT MOVED. The card the agent's call drew
// said `running` forever above a bubble already reporting `exit 0` (observed
// 2026-09-09).
//
// RETIRE THEN REDRAW is how that is answered now: ONE row carries the run, not
// two. The foreground card's kind (KindActivity) differs from the shell head's
// (KindShellHead), so the card cannot change arm under one identity the way a
// subagent bubble does -- it is RETIRED, its removal published on the tail
// (FeedRow.removed, "a foreground Bash that detached, its running tool card
// retired in favor of the shell bubble"), and the shell bubble is drawn in its
// place (FeedRow.shell_head, "a foreground->detached transition retires the
// running tool card and redraws THIS bubble in its place"). See
// internal/resolve/feed/subagent.go's detachForegroundShell.

func TestABackgroundedForegroundShellsCardIsRetiredAndRedrawnAsItsShellBubble(t *testing.T) {
	t.Parallel()
	// Arrange: a foreground shell, drawn running.
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-bash-moved", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("bash-moved"),
		Item: &conversationv1.AgentActivity_Bash{Bash: &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Start{
			Start: &conversationv1.AgentBashStart{
				Command:   &conversationv1.AgentBashCommand{Line: "sleep 600"},
				StartedAt: startedAt(1),
			},
		}}},
	}))
	card := awaitRow(t, f, tail, "the running bash card", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetSimpleToolCall().GetRunning() != nil
	})

	// Act: the command exceeded its own timeout and was moved to the
	// background rather than killed.
	f.shim.PushAgentFrame(mainAgent, detachedWorkFrame(mainAgent, movedShell("bash-moved")))

	// Assert: the running card's REMOVAL is published on the tail, so an
	// already-open feed drops it live rather than showing it stale.
	removal := awaitRow(t, f, tail, "the running bash card's removal", func(r *frontendv1.FeedRow) bool {
		return r.GetRemoved() != nil
	})
	if removal.GetId().GetValue() != card.GetId().GetValue() {
		t.Fatalf("the removal names row %q, want the running card's own id %q", removal.GetId().GetValue(), card.GetId().GetValue())
	}

	// Assert: and the shell bubble is drawn in its place, carrying the command
	// the retired card drew -- one row for the run, not two.
	head := awaitShellHead(t, f, tail, "the shell bubble redrawn in the retired card's place")
	if got := head.GetShellHead().GetCommand().GetText(); got != "sleep 600" {
		t.Fatalf("the redrawn bubble's command = %q, want the moved call's own %q", got, "sleep 600")
	}
	if head.GetShellHead().GetLive() == nil {
		t.Fatalf("the redrawn bubble's state = %v, want live — the work moved, it did not end", head.GetShellHead().GetState())
	}
}

func TestAReplayedDetachmentNeverRedrawsASettledShellLive(t *testing.T) {
	t.Parallel()
	// Arrange: a detached run that has ENDED. Its watch is reaped at the
	// terminal, so the next announcement of the same work opens a fresh one.
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-shell-resettle", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	pushDetachedShell(f.shim, "work-resettle-1", "make")
	head := awaitShellHead(t, f, tail, "the shell bubble's head on the root feed")
	f.shim.PushBash("work-resettle-1", &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Success{
		Success: &conversationv1.AgentBashSuccess{
			Command: &conversationv1.AgentBashCommand{Line: "make"},
			Outcome: &conversationv1.AgentBashSuccess_Completed{Completed: &conversationv1.AgentBashCompleted{
				Output:      &conversationv1.AgentBashOutput{Form: &conversationv1.AgentBashOutput_Text{Text: &conversationv1.AgentBashOutputText{Stdout: "EXIT=0\n", Extent: &conversationv1.AgentBashOutputText_Whole{Whole: &conversationv1.AgentBashOutputWhole{}}}}},
				Termination: &conversationv1.AgentBashTermination{How: &conversationv1.AgentBashTermination_Exited{Exited: &conversationv1.AgentBashExited{Code: 0}}},
			}},
			SettledAt: settledAt(2),
		},
	}})
	awaitRow(t, f, tail, "the settled shell head", func(r *frontendv1.FeedRow) bool {
		return r.GetId().GetValue() == head.GetId().GetValue() && r.GetShellHead().GetSettled() != nil
	})

	// Act: the next turn's live-work reconciliation announces the SAME work
	// again, carrying the run's start and no ending at all.
	pushDetachedShell(f.shim, "work-resettle-1", "make")

	// Assert: nothing the replay produced drew the run live. A bubble drawn
	// from the frame in hand walked back here -- an orange dot and a stop
	// button over a spool holding `EXIT=0`.
	//
	// THE BARRIER IS AN ORDINARY FRAME ON THE SAME STREAM, which is what makes
	// a negative observable: the announcement and it are delivered in order, so
	// reaching the barrier proves the replay has been drawn.
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("resettle-barrier"),
		Item: &conversationv1.AgentActivity_Read{Read: &conversationv1.AgentRead{Result: &conversationv1.AgentRead_Start{
			Start: &conversationv1.AgentReadStart{Path: &conversationv1.ReadPath{Path: "go.mod"}, StartedAt: startedAt(3)},
		}}},
	}))
	redrawnLive := false
	awaitRow(t, f, tail, "the barrier row behind the replayed announcement", func(r *frontendv1.FeedRow) bool {
		if r.GetId().GetValue() == head.GetId().GetValue() && r.GetShellHead().GetLive() != nil {
			redrawnLive = true
		}
		return r.GetActivity().GetSimpleToolCall().GetName().GetText() == "Read"
	})
	if redrawnLive {
		t.Fatal("a replayed announcement redrew the settled shell bubble as live")
	}
}

func TestDetachedShellDrawsHeadAndSpoolTailFromWatchBashTail(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-detachshell", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	pushDetachedShell(f.shim, "work-shell-1", "tail -f build.log")
	head := awaitShellHead(t, f, tail, "the shell bubble's head on the root feed")
	if got := head.GetShellHead().GetCommand().GetText(); got != "tail -f build.log" {
		t.Fatalf("the head's command = %q, want the announced command", got)
	}
	if head.GetShellHead().GetSpool() != nil {
		t.Fatalf("the head carries a spool %v, want none — the spool is the BODY row on the sub-feed", head.GetShellHead().GetSpool())
	}

	// Act
	f.shim.PushBash("work-shell-1", &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Tail{
		Tail: &conversationv1.AgentBashTail{Text: "building...\n"},
	}})

	// Assert: the spool rides the BODY row, on the sub-feed the head addresses.
	spool := awaitShellSpool(t, f, head, "the spool body drawn from the bash tail", func(sh *frontendv1.FeedShell) bool {
		return sh.GetSpool() != nil
	})
	if spool.GetSpool().GetText() != "building...\n" {
		t.Fatalf("the spool tail = %q, want the tail's text", spool.GetSpool().GetText())
	}
}

func TestADetachedBashTailPastTheCapIsRefusedAndLogged(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-spoolovercap", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()
	pushDetachedShell(f.shim, "work-overcap-1", "long-build")
	head := awaitShellHead(t, f, tail, "the shell bubble's head on the root feed")

	// Act: a tail longer than the contract's cap — a producer that no longer
	// agrees with the renderer on what is drawn.
	f.shim.PushBash("work-overcap-1", &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Tail{
		Tail: &conversationv1.AgentBashTail{Text: strings.Repeat("y", int(conversationv1.AgentBashTailCap_AGENT_BASH_TAIL_CAP_BYTES)+1)},
	}})

	// Assert: refused — the BODY row it would have drawn never appears.
	expectNoShellSpool(t, f, head, "an over-cap tail must not draw the shell's spool body")
	harness.ExpectNoPush(t, tail, harness.ProbeWindow, "an over-cap tail must not upsert the shell's head")
	// subagent.go logs daemon.feed.spool_over_cap at ERROR on the refusal.
	f.d.ExpectWarnings("daemon.feed.spool_over_cap")
}

// olderThanOldestOf asks for the page above the oldest row PAGE served, as a
// client holding that page asks.
func olderThanOldestOf(page *frontendv1.FeedPage) *agentreplv1.GetFeedPageRequest_OlderThan {
	return &agentreplv1.GetFeedPageRequest_OlderThan{OlderThan: &agentreplv1.GetFeedPageOlderThan{
		OldestHeld: page.GetSuccess().GetRows()[0].GetOrder(),
	}}
}

// A BOARD ON A DAEMON WITH NO CHECKOUT IS UNAVAILABLE, AND SAYS WHY. The
// harness states the host's explanation-engine checkout empty
// (harness.HostNeutralCheckoutEnv), so the daemon's real board backend cannot
// ready a widget, and the bubble names the call's session and that reason.
func TestABoardCallOnADaemonWithNoCheckoutDrawsTheBoardUnavailable(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newOpened(t, harness.Opts{})
	f.submit("go", "k-board", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	tail := f.watchRootFeed()

	// Act: the agent's board call returns.
	f.shim.PushAgentFrame(mainAgent, activityFrame(mainAgent, &conversationv1.AgentActivity{
		ActivityId: activityID("board-1"),
		Item: &conversationv1.AgentActivity_ChessBoard{ChessBoard: &conversationv1.AgentChessBoard{
			Result: &conversationv1.AgentChessBoard_Success{Success: &conversationv1.AgentChessBoardSuccess{
				Session: &conversationv1.AgentChessBoardSession{SessionId: "fake-cee-session", GameId: "g-1"},
			}},
		}},
	}))

	// Assert
	row := awaitRow(t, f, tail, "the chess board, unavailable for want of a checkout", func(r *frontendv1.FeedRow) bool {
		return strings.HasPrefix(r.GetActivity().GetChessBoard().GetUnavailable().GetReason().GetText(), "The explanation-engine checkout was not found")
	})
	if got := row.GetActivity().GetChessBoard().GetHeading().GetText(); got != "Chess board · CEE session fake-cee-session" {
		t.Fatalf("heading = %q, want the call's session", got)
	}
}
