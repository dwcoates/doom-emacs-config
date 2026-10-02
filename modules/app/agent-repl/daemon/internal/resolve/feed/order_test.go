package feed

import (
	"context"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/proto"

	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// ---- helpers: entries stated at a conversation place ----

// placeAt is a recorded conversation place at AT_MS, ordinal 0.
func placeAt(atMs int64) *conversationv1.ConversationPlace {
	return &conversationv1.ConversationPlace{AtMs: atMs}
}

// pagedEntry is one page entry and the place its serving side stated for it.
type pagedEntry struct {
	atMs  int64
	entry *conversationv1.HistoryEntry
}

// placedPage is a replayed history whose every entry carries a recorded place,
// NEWEST FIRST as a page is served, each at a pointer of its own.
func placedPage(boundary any, newestFirst ...pagedEntry) *conversationv1.HistoryPage {
	entries := make([]*conversationv1.HistoryEntry, 0, len(newestFirst))
	for _, e := range newestFirst {
		entries = append(entries, e.entry)
	}
	page := historyPage(boundary, entries...)
	for i, e := range newestFirst {
		page.Entries[i].Place = &conversationv1.HistoryEntryAt_RecordedPlace{RecordedPlace: placeAt(e.atMs)}
	}
	return page
}

// deliverPromptAt delivers a live prompt whose entry sits at AT_MS.
func (h *harness) deliverPromptAt(turn, text string, atMs int64) {
	h.t.Helper()
	h.resolver.OnPrompt(testWorkspace, mainAgent(), &conversationv1.AgentPrompt{
		Id:     &conversationv1.TurnId{Value: turn},
		Agent:  mainAgent(),
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT,
		Said: &conversationv1.UserSaid{Content: &conversationv1.UserContent{
			Blocks: []*conversationv1.UserContentBlock{textBlock(text)},
		}},
	}, placeAt(atMs))
}

// sendAt delivers a live activity whose entry sits at AT_MS.
func (h *harness) sendAt(act *conversationv1.AgentActivity, atMs int64) {
	h.t.Helper()
	h.resolver.OnActivity(testWorkspace, mainAgent(), act, nil, placeAt(atMs))
}

// cutPlaced delivers a live context cut whose entry sits at AT_MS.
func (h *harness) cutPlaced(at string, cut *conversationv1.ContextCut, atMs int64) {
	h.t.Helper()
	h.resolver.OnContextCut(testWorkspace, mainAgent(), cut,
		&conversationv1.HistoryPointer{Value: at}, nil, placeAt(atMs))
}

// orderOf is the order key a feed's row carries.
func (h *harness) orderOf(feed feedid.Feed, id string) string {
	h.t.Helper()
	return h.rowByID(feed, id).GetOrder().GetKey()
}

// settledResponse is a settled prose block of UNIT, as a live activity.
func settledResponse(unit, text string) *conversationv1.AgentActivity {
	return responseSuccessActivity(unit, text)
}

// ---- the key itself ----

func TestEntryBaseOrdersAsThePlace(t *testing.T) {
	// Arrange.
	tests := []struct {
		name          string
		before, after *conversationv1.ConversationPlace
	}{
		{name: "an earlier instant sorts first", before: placeAt(9), after: placeAt(10)},
		{name: "a wider instant still sorts numerically", before: placeAt(0xfff), after: placeAt(0x1000)},
		{name: "a lower ordinal of one instant sorts first", before: &conversationv1.ConversationPlace{AtMs: 5, Ordinal: 1}, after: &conversationv1.ConversationPlace{AtMs: 5, Ordinal: 2}},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			before, after := entryBase('2', tc.before), entryBase('2', tc.after)

			// Assert.
			if !(before < after) {
				t.Fatalf("key %q does not sort before %q", before, after)
			}
		})
	}
}

func TestAFollowerSortsBetweenItsBaseAndTheNextEntry(t *testing.T) {
	// Arrange.
	base := entryBase('2', placeAt(10)) + "00000000"
	next := entryBase('2', placeAt(11)) + "00000000"

	// Act.
	follower := baseOf(base) + string(followSep) + "00000001"

	// Assert.
	if !(base < follower && follower < next) {
		t.Fatalf("follower %q is not between %q and %q", follower, base, next)
	}
}

func TestBaseOf(t *testing.T) {
	// Arrange.
	tests := []struct {
		name, key, want string
	}{
		{name: "an entry's key is its own base", key: "2abc", want: "2abc"},
		{name: "a follower's base is the part before the separator", key: "2abc.00000003", want: "2abc"},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := baseOf(tc.key)

			// Assert.
			if got != tc.want {
				t.Fatalf("baseOf(%q) = %q, want %q", tc.key, got, tc.want)
			}
		})
	}
}

// ---- every row carries its order ----

func TestEveryPublishedRowCarriesAnOrderKey(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.deliverPromptAt("turn-1", "a prompt", 100)
	h.sendAt(settledResponse("unit-1", "an answer"), 200)

	// Assert.
	for _, row := range h.rows(rootFeed()) {
		if row.GetOrder().GetKey() == "" {
			t.Fatalf("row %s carries no order key", row.GetId().GetValue())
		}
	}
}

func TestARowDrawnWithoutAnEntryCarriesAnOrderKey(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act: a daemon-synthesized row, drawn from no entry at all.
	h.resolver.UpsertCommandRefused(testWorkspace, "/agents", "not supported", true)

	// Assert.
	for _, row := range h.rows(rootFeed()) {
		if row.GetOrder().GetKey() == "" {
			t.Fatalf("row %s carries no order key", row.GetId().GetValue())
		}
	}
}

func TestARemovalCarriesTheRemovedRowsKey(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.deliverPromptAt("turn-1", "a prompt", 100)
	id := h.promptRowID("turn-1")
	key := h.orderOf(rootFeed(), id)
	_, token := h.openPage(rootFeed(), "reader-1")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	tail, err := h.resolver.Tail(ctx, testWorkspace, rootFeed(), token)
	if err != nil {
		t.Fatalf("Tail: %v", err)
	}
	rows := tail.Rows(ctx)

	// Act.
	h.resolver.mu.Lock()
	h.resolver.retire(h.resolver.state(testWorkspace), rootFeed(), id)
	h.resolver.mu.Unlock()

	// Assert.
	removal := <-rows
	if removal.GetRemoved() == nil || removal.GetOrder().GetKey() != key {
		t.Fatalf("removal = %v, want the removed row's key %q", removal, key)
	}
}

// ---- THE INCIDENT: a late row lands where it belongs ----

// TestALateRowIsPlacedAmongItsNeighbors reproduces the 2026-09-27 incident:
// two response rows written at 13:30 and 13:44 reached the feed only after the
// 14:36 answer and its turn's end. They are placed between the rows around
// their own places, never below the rows that arrived before them.
func TestALateRowIsPlacedAmongItsNeighbors(t *testing.T) {
	// Arrange: the conversation up to the 14:36 answer, drawn as it happened.
	h := newHarness(t)
	h.deliverPromptAt("turn-1", "the 13:29 prompt", 1329)
	h.sendAt(settledResponse("answer-1400", "an answer at 14:00"), 1400)
	h.sendAt(settledResponse("answer-1436", "the answer at 14:36"), 1436)

	// Act: the 13:30 and 13:44 responses arrive late.
	h.sendAt(settledResponse("late-1330", "written at 13:30"), 1330)
	h.sendAt(settledResponse("late-1344", "written at 13:44"), 1344)

	// Assert: the feed's order, and every row's key, is conversation order.
	want := []string{
		h.promptRowID("turn-1"),
		h.responseRowID("late-1330"),
		h.responseRowID("late-1344"),
		h.responseRowID("answer-1400"),
		h.responseRowID("answer-1436"),
	}
	rows := h.rows(rootFeed())
	got := rowIDs(rows)
	if strings.Join(got, "\n") != strings.Join(want, "\n") {
		t.Fatalf("feed order =\n%s\nwant\n%s", strings.Join(got, "\n"), strings.Join(want, "\n"))
	}
	for i := 1; i < len(rows); i++ {
		if !(rows[i-1].GetOrder().GetKey() < rows[i].GetOrder().GetKey()) {
			t.Fatalf("row %d's key %q does not sort after row %d's %q", i, rows[i].GetOrder().GetKey(), i-1, rows[i-1].GetOrder().GetKey())
		}
	}
}

func TestALateRowIsPushedWithTheKeyOfItsTruePlace(t *testing.T) {
	// Arrange: a reader following the tail of a feed that has moved on.
	h := newHarness(t)
	h.deliverPromptAt("turn-1", "the prompt", 1329)
	h.sendAt(settledResponse("answer-1436", "the answer at 14:36"), 1436)
	_, token := h.openPage(rootFeed(), "reader-1")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	tail, err := h.resolver.Tail(ctx, testWorkspace, rootFeed(), token)
	if err != nil {
		t.Fatalf("Tail: %v", err)
	}
	rows := tail.Rows(ctx)

	// Act.
	h.sendAt(settledResponse("late-1330", "written at 13:30"), 1330)

	// Assert: the push states a key between the prompt and the later answer.
	pushed := <-rows
	key := pushed.GetOrder().GetKey()
	if !(h.orderOf(rootFeed(), h.promptRowID("turn-1")) < key && key < h.orderOf(rootFeed(), h.responseRowID("answer-1436"))) {
		t.Fatalf("the late row was pushed with key %q, not between its neighbors", key)
	}
}

// ---- an update never moves a row ----

func TestAnUpdateKeepsTheRowsKey(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.sendAt(settledResponse("unit-1", "first words"), 100)
	before := h.orderOf(rootFeed(), h.responseRowID("unit-1"))

	// Act: the same unit grows, delivered at a later place (the other plane).
	h.sendAt(settledResponse("unit-1", "first words, and more"), 900)

	// Assert.
	if after := h.orderOf(rootFeed(), h.responseRowID("unit-1")); after != before {
		t.Fatalf("the update moved the row: key %q became %q", before, after)
	}
}

func TestARestatementCarryingAnotherKeyIsAnErrorAndKeepsTheKey(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.sendAt(settledResponse("unit-1", "first words"), 100)
	id := h.responseRowID("unit-1")
	key := h.orderOf(rootFeed(), id)
	restated := cloneRow(t, h.rowByID(rootFeed(), id))
	restated.Order = &frontendv1.FeedRowOrder{Key: "2-not-its-key"}
	restated.Turn = &conversationv1.TurnId{Value: "turn-restated"}

	// Act.
	h.resolver.mu.Lock()
	h.resolver.upsert(h.resolver.state(testWorkspace), placement{feed: rootFeed()}, restated, true)
	h.resolver.mu.Unlock()

	// Assert.
	if !h.hasRecord("error", "daemon.feed.order_changed") {
		t.Fatal("a restated row carrying another key was not recorded at ERROR")
	}
	if got := h.orderOf(rootFeed(), id); got != key {
		t.Fatalf("the row's key became %q, want it kept at %q", got, key)
	}
}

// ---- keys survive a restart ----

func TestKeysAreIdenticalAcrossADaemonRestart(t *testing.T) {
	// Arrange: one daemon draws the conversation live.
	live := newHarness(t)
	live.deliverPromptAt("turn-1", "the prompt", 100)
	live.sendAt(settledResponse("unit-1", "the answer"), 200)
	want := map[string]string{}
	for _, row := range live.rows(rootFeed()) {
		want[row.GetId().GetValue()] = row.GetOrder().GetKey()
	}

	// Act: a restarted daemon draws the same entries from the store's page.
	restarted := newHarness(t)
	restarted.replay(placedPage(&conversationv1.HistoryFloor{},
		pagedEntry{atMs: 200, entry: frameEntry(mainAgent(), &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_Activity{Activity: settledResponse("unit-1", "the answer")},
		})},
		pagedEntry{atMs: 100, entry: promptEntry("turn-1", "the prompt")},
	))

	// Assert.
	for id, key := range want {
		if got := restarted.orderOf(rootFeed(), id); got != key {
			t.Fatalf("row %s: key %q after the restart, %q before", id, got, key)
		}
	}
}

// ---- rows with no stated place ----

func TestAnUnplacedEntrysRowFollowsTheFeedsNewestRow(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.sendAt(settledResponse("unit-1", "placed"), 500)

	// Act: the serving side stated no place for the next entry.
	h.send(settledResponse("unit-2", "unplaced"))

	// Assert: it lands after the newest row, and says why.
	if got := rowIDs(h.rows(rootFeed())); got[len(got)-1] != h.responseRowID("unit-2") {
		t.Fatalf("feed order = %v, want the unplaced row last", got)
	}
	if !h.hasRecordWith("info", "daemon.feed.row_placed", "how", "entry_unplaced") {
		t.Fatal("the receipt-order fallback was not recorded")
	}
}

func TestANonPositivePlaceIsAnErrorAndTheRowFollows(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.sendAt(settledResponse("unit-1", "placed"), 500)

	// Act.
	h.sendAt(settledResponse("unit-2", "misplaced"), 0)

	// Assert.
	if !h.hasRecord("error", "daemon.feed.place_invalid") {
		t.Fatal("a non-positive place was not recorded at ERROR")
	}
	if got := rowIDs(h.rows(rootFeed())); got[len(got)-1] != h.responseRowID("unit-2") {
		t.Fatalf("feed order = %v, want the misplaced row to follow the newest", got)
	}
}

func TestARowTheReplayMakesFollowsTheReplaysOwnRows(t *testing.T) {
	// Arrange: a live row stands at a later place than the page will reach.
	h := newHarness(t)
	h.deliverPromptAt("turn-9", "a later turn", 900)

	// Act: a page whose last turn has no terminal on it, closed by the record,
	// so the replay draws that turn's end itself.
	h.closes = map[ids.TurnID]wsm.RecordedClose{"turn-1": {How: wsm.CloseAgentDied, At: closedAt}}
	h.replay(placedPage(&conversationv1.HistoryFloor{},
		pagedEntry{atMs: 200, entry: frameEntry(mainAgent(), &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_Activity{Activity: settledResponse("unit-1", "the answer")},
		})},
		pagedEntry{atMs: 100, entry: promptEntry("turn-1", "the prompt")},
	))

	// Assert: the turn's end sits after its answer and before the later turn.
	got := rowIDs(h.rows(rootFeed()))
	end, later := indexOfID(got, h.turnEndedRowID("turn-1")), indexOfID(got, h.promptRowID("turn-9"))
	answer := indexOfID(got, h.responseRowID("unit-1"))
	if end < 0 || !(answer < end && end < later) {
		t.Fatalf("feed order = %v, want the replayed turn end between its answer and the later turn", got)
	}
}

// ---- republications are recorded ----

func TestAReplayRestatingAPlacedRowIsRecordedAtInfo(t *testing.T) {
	// Arrange: a reader follows a feed whose row was drawn live.
	h := newHarness(t)
	h.deliverPromptAt("turn-1", "the prompt", 100)
	h.sendAt(settledResponse("unit-1", "early words"), 200)
	_, token := h.openPage(rootFeed(), "reader-1")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	if _, err := h.resolver.Tail(ctx, testWorkspace, rootFeed(), token); err != nil {
		t.Fatalf("Tail: %v", err)
	}

	// Act: a replay restates it with more.
	h.replay(placedPage(&conversationv1.HistoryFloor{},
		pagedEntry{atMs: 200, entry: frameEntry(mainAgent(), &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_Activity{Activity: settledResponse("unit-1", "early words and the rest")},
		})},
		pagedEntry{atMs: 100, entry: promptEntry("turn-1", "the prompt")},
	))

	// Assert.
	if !h.hasRecordWith("info", "daemon.feed.row_republished", "reason", "history_replay") {
		t.Fatal("a replay's re-push of a placed row was not recorded at INFO")
	}
}

func TestALiveEntryGrowingItsRowIsNotRecordedAtInfo(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.sendAt(settledResponse("unit-1", "early words"), 200)
	_, token := h.openPage(rootFeed(), "reader-1")
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	if _, err := h.resolver.Tail(ctx, testWorkspace, rootFeed(), token); err != nil {
		t.Fatalf("Tail: %v", err)
	}

	// Act.
	h.sendAt(settledResponse("unit-1", "early words and more"), 200)

	// Assert.
	if h.hasRecord("info", "daemon.feed.row_republished") {
		t.Fatal("a live entry growing its own row was recorded at INFO")
	}
}

// ---- the page walk ----

func TestALateRowOlderThanTheLoadedPageIsServedInPlaceByTheNextPage(t *testing.T) {
	// Arrange: a reader holds the newest store page of a longer book.
	h := newHarness(t)
	h.mainBook(2, promptsBook(3))
	h.openPage(rootFeed(), "reader-1")

	// Act: a row placed before everything the reader loaded arrives late.
	h.sendAt(settledResponse("late", "written at 150"), 150)
	page, err := h.resolver.NextPage(context.Background(), testWorkspace, rootFeed(), "reader-1")
	if err != nil {
		t.Fatalf("NextPage: %v", err)
	}

	// Assert: the older page is exactly the rows before the reader's oldest,
	// the late row in its place among them.
	want := []string{h.promptRowID("turn-0"), h.responseRowID("late")}
	if got := rowIDs(pageRows(t, page)); strings.Join(got, ",") != strings.Join(want, ",") {
		t.Fatalf("next page = %v, want %v", got, want)
	}
}

// responseRowID is the identity a root-feed activity row of UNIT carries.
func (h *harness) responseRowID(unit string) string {
	return activityRowID(rootFeed(), unit)
}

// turnEndedRowID is the identity a root-feed turn-ended row of TURN carries.
func (h *harness) turnEndedRowID(turn string) string {
	return testEncode(feedid.Ref{
		WS: testWorkspace, Feed: rootFeed(),
		Row: feedid.RowKey{Kind: feedid.KindTurnEnded, ID: turn},
	}).GetValue()
}

// hasRecordWith reports whether a record of this level and operation carried
// FIELD with VALUE.
func (h *harness) hasRecordWith(level, operation, field string, value any) bool {
	for _, record := range h.records() {
		if record.Level == level && record.Operation == operation && record.Context[field] == value {
			return true
		}
	}
	return false
}

// cloneRow copies a stored row so a test can restate it.
func cloneRow(t *testing.T, row *frontendv1.FeedRow) *frontendv1.FeedRow {
	t.Helper()
	out, ok := proto.Clone(row).(*frontendv1.FeedRow)
	if !ok {
		t.Fatal("the row could not be cloned")
	}
	return out
}

// indexOfID is ID's index in IDS, or -1.
func indexOfID(ids []string, id string) int {
	for i, got := range ids {
		if got == id {
			return i
		}
	}
	return -1
}

func TestADaemonRowMadeAfterAnOlderDaemonRowSortsAtTheMomentItWasMade(t *testing.T) {
	// Arrange: before the newest page is loaded the daemon made one row
	// between the book's two entries (a merge, at 150) and one after both (a
	// cold gate, at 1000). The feed holds only daemon rows, so the first says
	// nothing about where the conversation's newest row stands.
	h := newHarness(t)
	h.mainBook(3, promptsBook(2))
	h.nowMs = 150
	h.resolver.UpsertSynthesized(testWorkspace, rootFeed(), &frontendv1.FeedRow{
		Id:  &frontendv1.FeedId{Value: "row|merge"},
		Row: &frontendv1.FeedRow_Activity{Activity: &frontendv1.FeedTurnActivity{}},
	})
	h.nowMs = 1_000
	h.resolver.UpsertSynthesized(testWorkspace, rootFeed(), &frontendv1.FeedRow{
		Id:  &frontendv1.FeedId{Value: "row|cold-gate"},
		Row: &frontendv1.FeedRow_Activity{Activity: &frontendv1.FeedTurnActivity{}},
	})

	// Act.
	page, _ := h.openPage(rootFeed(), "reader-1")

	// Assert: each daemon row stands where it happened among the history.
	got := strings.Join(rowIDs(pageRows(t, page)), ",")
	want := h.promptRowID("turn-0") + ",row|merge," + h.promptRowID("turn-1") + ",row|cold-gate"
	if got != want {
		t.Fatalf("page rows = %v, want %v", got, want)
	}
}
