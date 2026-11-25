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
	f := newCanonical(t)
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
	f := newCanonical(t)
	tail := f.watchRootFeed()

	// Act: nudge the daemon-global zoom, which pushes a FeedTextScale frame to
	// every open feed's watch -- this tail included.
	if _, err := f.d.Client().AdjustFeedTextScale(f.d.Ctx(), connect.NewRequest(&agentreplv1.AdjustFeedTextScaleRequest{
		Direction: agentreplv1.AdjustFeedTextScaleDirection_ADJUST_FEED_TEXT_SCALE_DIRECTION_INCREASE,
	})); err != nil {
		t.Fatalf("AdjustFeedTextScale = error %v, want the scale now in force", err)
	}

	// Assert: the zoom reached the watch, and NO row was drawn by it.
	harness.ExpectNoPush(t, tail, "a feed text scale frame is not a feed row")
}

func TestWatchFeedWithAnUnmintedTokenIsRefusedAtTheTransport(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newCanonical(t)

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
	// THE RECORD IS FOUND BY ITS OWN RPC AND CAUSE: on a shared daemon the
	// other rpcs' refusals, and other cases', are in the run log beside it.
	rec := f.d.AwaitUnscopedRunLogRecord("the WatchFeed unknown_token transport close", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.refusal.transport_closed" && r.Context["rpc"] == "WatchFeed" && r.Context["cause"] == "unknown_token"
	})
	if !strings.EqualFold(rec.Level, "info") {
		t.Fatalf("the transport-closed record = level %q, want INFO", rec.Level)
	}
	// No ExpectWarnings operation: the refusal must produce no WARN at all.
}

func TestGetFeedPageFirstThenNextWalksOlderPages(t *testing.T) {
	t.Parallel()
	// Arrange: push MORE than one page's worth of rows off harness.FeedPageSize
	// — the daemon's own feed.DefaultPageSize, not a guessed number — so a
	// second page provably exists regardless of what the page size is.
	f := newCanonical(t)
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

func TestAWalkContinuesOnAnyConnection(t *testing.T) {
	t.Parallel()
	// Arrange: a walk begun on connection A (owner's report, 2026-10-03: a
	// webview spreads its requests over several connections, so an `older`
	// click arrived on a connection the walk was not keyed by and was refused).
	f := newCanonical(t)
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

// ==========================================================================
// Tool cards.
// ==========================================================================

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

// ==========================================================================
// Worktree separation dividers.
// ==========================================================================

// ==========================================================================
// AgentUpdate.context_cut.
// ==========================================================================

// ==========================================================================
// Permission and question cards.
// ==========================================================================

func TestPermissionStartDrawsOpenRowFooterAndBanner(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newInGroup(t, groupDesktopBanner)
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
	f.d.AwaitOwnRunLogRecord("the permission ask's desktop banner", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.desktopnotify.post" && r.Message == "posting a desktop banner" &&
			r.Context["kind"] == "permission_requested"
	})
}

// ==========================================================================
// Subagents (sync and detached).
// ==========================================================================

func TestASyncSubagentSpawnDrawsABubbleHeadAndItsOwnFeedServesSubFeedRows(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newCanonical(t)
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
			f := newCanonical(t)
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
			// expanded section lists. A turn is in flight, so no status moves
			// with the set: the wait first reads the watcher's own live-work
			// edge, so the chip is never matched on a spawn not yet detached.
			awaitLiveWork(t, f, 1)
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

// THE RUN'S OWN TERMINALS (landing 8). One end-to-end path per arm family:
// a FailureVendor* arm, which also resolves the workspace purple, and the
// Stop-hook arm, which does not.

// ==========================================================================
// Hooks.
// ==========================================================================

// ==========================================================================
// Artifacts.
// ==========================================================================

// ==========================================================================
// Findings.
// ==========================================================================

// ==========================================================================
// MCP tools: ordinary tool calls.
// ==========================================================================

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
	f := newCanonical(t)
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
	// THE WAIT FOR RESP-1 WATCHES FROM BEFORE ANYTHING IS PUBLISHED: with one
	// row retained, a tail opened on a page from after the submit could
	// already find its pin gone (more than one row published before it was
	// accepted) and be refused, which is this test's subject, not its
	// arrangement. Accepted first, the watch delivers each row live.
	_, before := f.openFeed(nil)
	changes := f.d.WatchFeedOn(f.d.Client(), before)
	defer changes.Close()
	wait, cancelWait := f.d.WaitCtx()
	defer cancelWait()
	changes.AwaitHeaders(t, wait, "the feed tail opened before the submit")
	f.submit("go", "k-expire", conversationv1.PromptOrigin_PROMPT_ORIGIN_WEBAPP_USER_SENT)
	f.shim.PushAgentFrame(mainAgent, feedResponseFrames("resp-1", "first")[0])
	f.shim.PushAgentFrame(mainAgent, feedResponseFrames("resp-1", "first")[1])
	harness.AwaitView(t, wait, changes, "resp-1's settled prose", func(r *frontendv1.FeedRow) bool {
		return r.GetActivity().GetResponse().GetSuccess().GetProse().GetProse().GetSource().GetMarkdown() == "first"
	})
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

func TestDetachedShellDrawsHeadAndSpoolTailFromWatchBashTail(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newCanonical(t)
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
	f := newCanonical(t)
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
