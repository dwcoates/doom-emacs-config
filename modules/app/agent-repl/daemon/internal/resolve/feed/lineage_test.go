package feed

import (
	"fmt"
	"slices"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"

	"google.golang.org/protobuf/proto"
)

// A FORK'S INHERITED PAST IS ITS PARENT'S BOOK. The root feed's walk reads the
// fork's own book, then each generation it inherited from, bounded and without
// the turns that were open when the fork was taken.

// liveResponse sends one settled response of the main agent, stamped with
// TURN ("" unstamped), as the watch serves a live entry.
func (h *harness) liveResponse(unit, text, turn string) {
	h.t.Helper()
	var stamp *conversationv1.TurnId
	if turn != "" {
		stamp = &conversationv1.TurnId{Value: turn}
	}
	h.resolver.OnActivity(testWorkspace, mainAgent(), responseSuccessActivity(unit, text), stamp, nil)
}

// liveCut sends one unstamped compaction at pointer AT.
func (h *harness) liveCut(at string) {
	h.t.Helper()
	h.cutAt(at, compactedCut("summary at "+at))
}

// root is the root feed's activity row id for UNIT.
func root(unit string) string { return activityRowID(rootFeed(), unit) }

// parentBook and grandparentBook are the books a fork's generations are read
// from in these cases.
var (
	parentBook      = &conversationv1.AgentId{Value: "book-parent"}
	grandparentBook = &conversationv1.AgentId{Value: "book-grandparent"}
)

// bookTurn is one finished turn of AGENT's book: its prompt, its answer UNIT,
// and the terminal naming that answer, all stamped TURN.
func bookTurn(agent *conversationv1.AgentId, turn, unit string) []bookEntry {
	prompt := promptEntry(turn, "asked "+turn)
	prompt.GetUserPrompt().Agent = agent
	answer := frameEntry(agent, &conversationv1.AgentUpdate{Update: &conversationv1.AgentUpdate_Activity{Activity: responseSuccessActivity(unit, "answer "+unit)}})
	terminal := frameEntry(agent, completedWith(unit))
	return []bookEntry{{entry: prompt}, {turn: turn, entry: answer}, {turn: turn, entry: terminal}}
}

// forkHarness is a harness whose workspace is a fork: its own book holds OWN,
// and LINEAGE names its generations, whose books BOOKS holds. The books are
// placed as a conversation is: the furthest generation first, then each nearer
// one, then the fork's own (generationStart).
func forkHarness(t *testing.T, pageSize int, own []bookEntry, lineage []LineageSegment, books map[string][]bookEntry) (*harness, *fakeHistory) {
	t.Helper()
	h := newHarness(t)
	store := h.withHistory(newFakeHistory(pageSize))
	store.books[""] = newestFirstFrom(own, generationStart(0))
	for i, segment := range lineage {
		store.books[segment.Book.GetValue()] = newestFirstFrom(books[segment.Book.GetValue()], generationStart(i+1))
	}
	h.resolver.NoteLineage(testWorkspace, lineage)
	return h, store
}

// generationStart is the first place, epoch ms, of generation N's entries in a
// fork harness: 0 the fork's own book, 1 its parent's, and so on, each older
// than the one nearer the fork.
func generationStart(n int) int64 { return int64(10-n) * 1_000 }

// newestFirstFrom renders a scripted book as the store serves it, its oldest
// entry placed at FIRSTMS and each next one 100 ms later, its pointers unique
// to the place.
func newestFirstFrom(oldestFirst []bookEntry, firstMs int64) []*conversationv1.HistoryEntryAt {
	out := newestFirst(oldestFirst)
	for i, at := range out {
		index := len(out) - 1 - i
		at.Place = &conversationv1.HistoryEntryAt_RecordedPlace{RecordedPlace: placeAt(firstMs + int64(index)*100)}
		at.At = &conversationv1.HistoryPointer{Value: fmt.Sprintf("p-%d-%d", firstMs, index)}
	}
	return out
}

// walkRows opens the root feed and asks for older pages until the walk
// reports the conversation's start, answering every row it served, oldest
// first.
func (h *harness) walkRows(reader testReader) []*frontendv1.FeedRow {
	h.t.Helper()
	page, _ := h.openPage(rootFeed(), reader)
	served := slices.Clone(pageRows(h.t, page))
	for range 32 {
		if edgeOf(h.t, page) == "at_start" {
			return served
		}
		page = h.nextPage(reader)
		served = append(slices.Clone(pageRows(h.t, page)), served...)
	}
	h.t.Fatal("the walk never reached the conversation's start")
	return nil
}

// segment is one generation for NoteLineage.
func segment(ws ids.WorkspaceID, book *conversationv1.AgentId, throughMs int64, uninherited ...ids.TurnID) LineageSegment {
	open := map[ids.TurnID]bool{}
	for _, turn := range uninherited {
		open[turn] = true
	}
	return LineageSegment{Workspace: ws, Book: book, ThroughMs: throughMs, Uninherited: open}
}

func TestAForksWalkReadsItsParentsBookOnceItsOwnBookEnds(t *testing.T) {
	// Arrange: the fork's own turn, and a parent book of one finished turn.
	own := bookTurn(mainAgent(), "own-1", "own-answer")
	h, store := forkHarness(t, 10, own,
		[]LineageSegment{segment("ws-parent", parentBook, 10_000)},
		map[string][]bookEntry{parentBook.GetValue(): bookTurn(parentBook, "parent-1", "parent-answer")})

	// Act
	rows := h.walkRows("reader-1")

	// Assert: the parent's turn above the fork's own, both on the root feed.
	ids := rowIDs(rows)
	if !slices.Contains(ids, root("parent-answer")) || !slices.Contains(ids, root("own-answer")) {
		t.Fatalf("rows = %v, want the parent's answer and the fork's own", ids)
	}
	if slices.Index(ids, root("parent-answer")) > slices.Index(ids, root("own-answer")) {
		t.Fatalf("rows = %v, want the inherited answer above the fork's own", ids)
	}
	last := store.lastRead()
	if !last.inherited || last.target != parentBook.GetValue() || last.through != 10_000 {
		t.Fatalf("last read = %+v, want the parent's book through the fork's bound", last)
	}
}

func TestAForksInheritedAnswerIsDrawnAsItsParentDrawsIt(t *testing.T) {
	// Arrange: the same finished turn, once as a workspace's own book and once
	// as a fork's inherited past.
	plain := newHarness(t)
	plainStore := plain.withHistory(newFakeHistory(10))
	plainStore.books[""] = newestFirst(bookTurn(mainAgent(), "turn-1", "unit-1"))
	fork, _ := forkHarness(t, 10, nil,
		[]LineageSegment{segment("ws-parent", parentBook, 10_000)},
		map[string][]bookEntry{parentBook.GetValue(): bookTurn(parentBook, "turn-1", "unit-1")})
	fork.resolver.NoteFreshBook(testWorkspace)

	// Act
	parentRows := plain.walkRows("reader-1")
	forkRows := fork.walkRows("reader-1")

	// Assert: the answer is the green final answer in both, and the drawn
	// prompt and answer are the same rows apart from where they are keyed.
	if len(parentRows) != len(forkRows) {
		t.Fatalf("parent drew %v, fork drew %v", rowIDs(parentRows), rowIDs(forkRows))
	}
	for i := range parentRows {
		want, got := proto.Clone(parentRows[i]).(*frontendv1.FeedRow), proto.Clone(forkRows[i]).(*frontendv1.FeedRow)
		want.Order, got.Order = nil, nil
		if !proto.Equal(want, got) {
			t.Fatalf("row %d differs:\nparent %v\nfork   %v", i, want, got)
		}
	}
	if answer := fork.rowByID(rootFeed(), root("unit-1")); !answer.GetActivity().GetResponse().GetFinalAnswer() {
		t.Fatalf("the inherited answer %v is not the final answer", answer)
	}
}

func TestAForksWalkLeavesOutTheTurnsOpenWhenItWasTaken(t *testing.T) {
	// Arrange: the parent's second turn was in flight at the fork.
	books := append(bookTurn(parentBook, "parent-1", "done"), bookTurn(parentBook, "parent-2", "in-flight")...)
	h, _ := forkHarness(t, 10, nil,
		[]LineageSegment{segment("ws-parent", parentBook, 10_000, "parent-2")},
		map[string][]bookEntry{parentBook.GetValue(): books})
	h.resolver.NoteFreshBook(testWorkspace)

	// Act
	rows := rowIDs(h.walkRows("reader-1"))

	// Assert
	if !slices.Contains(rows, root("done")) || slices.Contains(rows, root("in-flight")) {
		t.Fatalf("rows = %v, want the finished turn and not the one in flight", rows)
	}
	if !h.hasRecord("debug", "daemon.feed.uninherited_entries_dropped") {
		t.Fatalf("records = %+v, want the dropped entries recorded", h.records())
	}
}

func TestAForksWalkReadsNothingPlacedAfterTheForkPoint(t *testing.T) {
	// Arrange: the parent's book holds two turns, placed from its generation's
	// start 100 ms apart; the fork was taken after the first turn's terminal.
	books := append(bookTurn(parentBook, "parent-1", "before"), bookTurn(parentBook, "parent-2", "after")...)
	h, _ := forkHarness(t, 10, nil,
		[]LineageSegment{segment("ws-parent", parentBook, generationStart(1)+200)},
		map[string][]bookEntry{parentBook.GetValue(): books})
	h.resolver.NoteFreshBook(testWorkspace)

	// Act
	rows := rowIDs(h.walkRows("reader-1"))

	// Assert
	if !slices.Contains(rows, root("before")) || slices.Contains(rows, root("after")) {
		t.Fatalf("rows = %v, want only the turn before the fork point", rows)
	}
}

func TestAForksWalkCrossesEveryGenerationToTheStart(t *testing.T) {
	// Arrange: fork -> parent -> grandparent, a page of two entries each.
	h, store := forkHarness(t, 2, bookTurn(mainAgent(), "own-1", "own"),
		[]LineageSegment{
			segment("ws-parent", parentBook, 10_000),
			segment("ws-grandparent", grandparentBook, 10_000),
		},
		map[string][]bookEntry{
			parentBook.GetValue():      bookTurn(parentBook, "parent-1", "from-parent"),
			grandparentBook.GetValue(): bookTurn(grandparentBook, "grand-1", "from-grandparent"),
		})

	// Act
	rows := rowIDs(h.walkRows("reader-1"))

	// Assert: every generation, oldest at the top.
	want := []string{root("from-grandparent"), root("from-parent"), root("own")}
	var got []string
	for _, id := range rows {
		if slices.Contains(want, id) {
			got = append(got, id)
		}
	}
	if !slices.Equal(got, want) {
		t.Fatalf("answers in order = %v, want %v (rows %v)", got, want, rows)
	}
	var targets []string
	for _, read := range store.reads {
		if read.inherited && read.after == "" {
			targets = append(targets, read.target)
		}
	}
	if !slices.Equal(targets, []string{parentBook.GetValue(), grandparentBook.GetValue()}) {
		t.Fatalf("generations entered = %v, want parent then grandparent", targets)
	}
}

func TestAWorkspaceThatIsNoForkStopsAtItsOwnStart(t *testing.T) {
	// Arrange
	h := newHarness(t)
	store := h.mainBook(10, bookTurn(mainAgent(), "turn-1", "unit-1"))

	// Act
	h.walkRows("reader-1")

	// Assert
	for _, read := range store.reads {
		if read.inherited {
			t.Fatalf("reads = %+v, want no inherited read", store.reads)
		}
	}
}

func TestAnInheritedPromptIsNoRollbackTarget(t *testing.T) {
	// Arrange
	h, _ := forkHarness(t, 10, nil,
		[]LineageSegment{segment("ws-parent", parentBook, 10_000)},
		map[string][]bookEntry{parentBook.GetValue(): bookTurn(parentBook, "parent-1", "unit-1")})
	h.resolver.NoteFreshBook(testWorkspace)
	h.walkRows("reader-1")

	// Act
	h.resolver.mu.Lock()
	prompts := h.resolver.rollbackPrompts(h.resolver.state(testWorkspace))
	h.resolver.mu.Unlock()

	// Assert
	if len(prompts) != 0 {
		t.Fatalf("rollback prompts = %+v, want none of the inherited past", prompts)
	}
}

func TestNoteLineageRecordsTheGenerations(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.resolver.NoteLineage(testWorkspace, []LineageSegment{segment("ws-parent", parentBook, 1)})

	// Assert
	if !h.hasRecordWith("info", "daemon.feed.lineage", "generations", 1) {
		t.Fatalf("records = %+v, want the lineage recorded with its generations", h.records())
	}
}

func TestAgentFeedPlacesAnInheritedBooksRowsOnTheRoot(t *testing.T) {
	cases := []struct {
		name string
		id   string
		want feedid.Feed
	}{
		{name: "the main agent", id: mainAgent().GetValue(), want: feedid.Feed{Root: true}},
		{name: "an inherited book", id: parentBook.GetValue(), want: feedid.Feed{Root: true}},
		{name: "a subagent", id: "agent-sub", want: feedid.Feed{Agent: &conversationv1.AgentId{Value: "agent-sub"}}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.resolver.NoteLineage(testWorkspace, []LineageSegment{segment("ws-parent", parentBook, 1)})
			h.resolver.mu.Lock()
			s := h.resolver.state(testWorkspace)

			// Act
			got, err := s.agentFeed(tc.id)
			rootAgent := s.rootFeedAgent(tc.id)
			h.resolver.mu.Unlock()

			// Assert
			if err != nil || got.Root != tc.want.Root || got.Agent.GetValue() != tc.want.Agent.GetValue() {
				t.Fatalf("agentFeed(%q) = (%+v, %v), want %+v", tc.id, got, err, tc.want)
			}
			if rootAgent != tc.want.Root {
				t.Fatalf("rootFeedAgent(%q) = %v, want %v", tc.id, rootAgent, tc.want.Root)
			}
		})
	}
}

func TestInheritedEntriesDropsTheTurnHeadsOfUninheritedTurns(t *testing.T) {
	// Arrange
	keep := newestFirst(bookTurn(parentBook, "keep", "k"))
	drop := newestFirst(bookTurn(parentBook, "drop", "d"))
	head := func(at *conversationv1.HistoryEntryAt) *conversationv1.HistoryTurnHead {
		return &conversationv1.HistoryTurnHead{Entry: at, Between: &conversationv1.HistoryTurnHead_Adjacent{Adjacent: &conversationv1.HistoryTurnHeadAdjacent{}}}
	}
	// newestFirst answers each turn prompt last: the head is that prompt, and
	// the page holds the turn's other entries.
	page := &conversationv1.HistoryPage{
		Entries: append(append([]*conversationv1.HistoryEntryAt{}, drop[:len(drop)-1]...), keep[:len(keep)-1]...),
		Heads:   []*conversationv1.HistoryTurnHead{head(drop[len(drop)-1]), head(keep[len(keep)-1])},
	}

	// Act
	kept, dropped := inheritedEntries(page, map[ids.TurnID]bool{"drop": true})

	// Assert
	if len(kept.GetHeads()) != 1 || entryTurn(kept.GetHeads()[0].GetEntry()) != "keep" {
		t.Fatalf("heads = %v, want only the inherited turn's", kept.GetHeads())
	}
	if want := len(drop) - 1 + 1; dropped != want {
		t.Fatalf("dropped = %d, want %d (the turn's entries and its head)", dropped, want)
	}
}

func TestInheritedEntriesKeepsTheBoundaryAndDropsOnlyUninheritedTurns(t *testing.T) {
	// Arrange
	entries := newestFirst(append(bookTurn(parentBook, "keep", "k"), bookTurn(parentBook, "drop", "d")...))
	page := &conversationv1.HistoryPage{
		Entries:  entries,
		Boundary: &conversationv1.HistoryPage_More{More: &conversationv1.HistoryMore{LastEntry: entries[len(entries)-1].GetAt()}},
	}

	// Act
	kept, dropped := inheritedEntries(page, map[ids.TurnID]bool{"drop": true})

	// Assert
	if dropped != 3 || len(kept.GetEntries()) != 3 {
		t.Fatalf("kept %d dropped %d, want three of each", len(kept.GetEntries()), dropped)
	}
	for _, at := range kept.GetEntries() {
		if entryTurn(at) != "keep" {
			t.Fatalf("kept an entry of %q", entryTurn(at))
		}
	}
	if kept.GetMore().GetLastEntry().GetValue() != page.GetMore().GetLastEntry().GetValue() {
		t.Fatal("the page's boundary was not kept")
	}
	if same, n := inheritedEntries(page, nil); same != page || n != 0 {
		t.Fatal("a page with nothing uninherited was not answered unchanged")
	}
}
