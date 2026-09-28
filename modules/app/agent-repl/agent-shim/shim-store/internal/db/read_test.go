package db

import (
	"context"
	"errors"
	"strconv"
	"strings"
	"sync"
	"testing"
	"time"

	storev1 "agentrepl/proto/store/v1"
)

// seedUnit counts the units seeded in one test process, so every seeded line
// gets identities of its own. Reusing one would be absorbed as a replay rather
// than written, which is correct behavior and a useless fixture.
var seedUnit int

// seedBook writes n ordinary page lines into a book and returns their
// pointers, oldest first.
func seedBook(t *testing.T, d *DB, book string, n int) []*storev1.StoreItemPointer {
	t.Helper()
	var out []*storev1.StoreItemPointer
	for i := 0; i < n; i++ {
		seedUnit++
		id := book + "-" + strconv.Itoa(seedUnit)
		result := writeOK(t, d, pageEntry("w-"+id, "u-"+id, book, frameItem(activityFrame(book, "act-"+id, prose()))))
		out = append(out, result.Lines[0].Line.GetAt())
	}
	return out
}

// seedUnitID names the unit seedBook created at 1-based offset `n` of the LAST
// seeding call of size `size`, so a test can upsert exactly that unit.
func seedUnitID(book string, size, n int) string {
	return book + "-" + strconv.Itoa(seedUnit-size+n)
}

// ---- OpenPage ----

func TestOpenPageRepaintsTheNewestPageNewestFirst(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	pointers := seedBook(t, d, "agent-1", 5)

	// Act
	opened, err := d.OpenPage(ctx(), "agent-1", 3, nil)

	// Assert
	if err != nil {
		t.Fatalf("OpenPage: %v", err)
	}
	if got := len(opened.Page.GetLines()); got != 3 {
		t.Fatalf("lines = %d, want 3", got)
	}
	if got := opened.Page.GetLines()[0].GetAt().GetValue(); got != pointers[4].GetValue() {
		t.Fatalf("first line = %q, want the newest %q", got, pointers[4].GetValue())
	}
	if opened.Page.GetMore() == nil {
		t.Fatal("boundary = floor, want more — two older lines remain")
	}
	if got := opened.Page.GetMore().GetLastItem().GetValue(); got != pointers[2].GetValue() {
		t.Fatalf("last_item = %q, want the page's oldest %q", got, pointers[2].GetValue())
	}
}

func TestOpenPageReportsTheFloorWhenTheBookFitsInOnePage(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	seedBook(t, d, "agent-1", 2)

	// Act
	opened, err := d.OpenPage(ctx(), "agent-1", 10, nil)

	// Assert
	if err != nil {
		t.Fatalf("OpenPage: %v", err)
	}
	if opened.Page.GetFloor() == nil {
		t.Fatal("boundary = more, want floor")
	}
}

func TestOpenPageAnswersAnEmptyBookWithAnEmptyPageAtTheFloor(t *testing.T) {
	// Arrange: an agent the store KNOWS but that has no rows is a LEGAL empty
	// book. Refusing it would make a freshly spawned subagent unwatchable until
	// it happened to speak, so the spawn frame that registers it is the fixture.
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w-spawn-empty", "u-spawn-empty", "agent-1",
		frameItem(activityFrame("agent-1", "act-spawn-empty", subagentStart("agent-never-spoke")))))

	// Act
	opened, err := d.OpenPage(ctx(), "agent-never-spoke", 10, nil)

	// Assert
	if err != nil {
		t.Fatalf("OpenPage refused an empty book: %v", err)
	}
	if len(opened.Page.GetLines()) != 0 {
		t.Fatalf("lines = %d, want 0", len(opened.Page.GetLines()))
	}
	if opened.Page.GetFloor() == nil {
		t.Fatal("boundary = more, want floor")
	}
}

// TestOpenPageRefusesAnAgentTheStoreHasNeverHeardOf: an id naming no book is
// refused rather than served an empty page, so a stale or mistyped target is
// distinguishable from a live agent that has said nothing yet.
func TestOpenPageRefusesAnAgentTheStoreHasNeverHeardOf(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	_, err := d.OpenPage(ctx(), "agent-never-existed", 10, nil)

	// Assert
	if !errors.Is(err, ErrUnknownAgent) {
		t.Fatalf("OpenPage error = %v, want ErrUnknownAgent", err)
	}
}

// TestOpenPageRefusalOfAnUnknownAgentNamesItsSite: the site is the vocabulary
// an operator counts refusals by, and it is this check's own, not the generic
// store_refused_request.
func TestOpenPageRefusalOfAnUnknownAgentNamesItsSite(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	_, err := d.OpenPage(ctx(), "agent-never-existed", 10, nil)

	// Assert
	if got := RefusalSite(err); got != SiteUnknownAgent {
		t.Fatalf("refusal site = %q, want %q", got, SiteUnknownAgent)
	}
}

// TestOpenPageRefusalOfAnUnknownAgentBlamesTheAgentField: the caller reads
// invalid_request-style field naming off every refusal, so the arm the wire
// carries can say which field was at fault.
func TestOpenPageRefusalOfAnUnknownAgentBlamesTheAgentField(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	_, err := d.OpenPage(ctx(), "agent-never-existed", 10, nil)

	// Assert
	if got := RefusalField(err); got != "agent" {
		t.Fatalf("refusal field = %q, want %q", got, "agent")
	}
}

// TestOpenPageRefusesAnUnknownAgentBeforeItJudgesThePointer: the register is
// asked first, because a known_through against a book that does not exist is
// stale only as a consequence, and stale_pointer would send the caller off to
// repaint a book nobody ever kept.
func TestOpenPageRefusesAnUnknownAgentBeforeItJudgesThePointer(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	pointers := seedBook(t, d, "agent-1", 1)

	// Act
	_, err := d.OpenPage(ctx(), "agent-never-existed", 10, pointers[0])

	// Assert
	if !errors.Is(err, ErrUnknownAgent) {
		t.Fatalf("OpenPage error = %v, want ErrUnknownAgent rather than a stale pointer", err)
	}
}

// TestOpenPageRefusalOfAnUnknownAgentIsNotAnErrorRecord: an id naming no book
// is the caller's business and the SERVER writes the one normal-level record;
// this layer only traces it, or a healthy store writes error records whenever
// a consumer holds a stale target.
func TestOpenPageRefusalOfAnUnknownAgentIsNotAnErrorRecord(t *testing.T) {
	// Arrange
	d, sink := newStore(t)

	// Act
	if _, err := d.OpenPage(ctx(), "agent-never-existed", 10, nil); err == nil {
		t.Fatal("OpenPage served an agent this store never heard of")
	}

	// Assert
	sink.assertTracedRefusal(t, "unknown agent")
}

func TestOpenPageCatchesUpFromKnownThrough(t *testing.T) {
	// Arrange: the caller states its own high-water mark; the store tracks
	// nothing about what it previously served.
	d, _ := newStore(t)
	pointers := seedBook(t, d, "agent-1", 4)

	// Act
	opened, err := d.OpenPage(ctx(), "agent-1", 10, pointers[1])

	// Assert
	if err != nil {
		t.Fatalf("OpenPage: %v", err)
	}
	if got := len(opened.Page.GetLines()); got != 2 {
		t.Fatalf("lines = %d, want only the two newer than the mark", got)
	}
	if opened.Page.GetFloor() == nil {
		t.Fatal("boundary = more, want floor — the page reached the caller's mark")
	}
}

func TestOpenPageReportsMoreWhenTheGapExceedsThePageSize(t *testing.T) {
	// Arrange: the caller then walks older via ReadAgentPage until it meets
	// its own mark.
	d, _ := newStore(t)
	pointers := seedBook(t, d, "agent-1", 6)

	// Act
	opened, err := d.OpenPage(ctx(), "agent-1", 2, pointers[0])

	// Assert
	if err != nil {
		t.Fatalf("OpenPage: %v", err)
	}
	if got := len(opened.Page.GetLines()); got != 2 {
		t.Fatalf("lines = %d, want the page budget of 2", got)
	}
	if opened.Page.GetMore() == nil {
		t.Fatal("boundary = floor, want more — the gap is wider than the page")
	}
}

func TestOpenPageRefusesAPointerFromAnotherBook(t *testing.T) {
	// Arrange: answering it would serve one agent's lines under another's name.
	d, s := newStore(t)
	other := seedBook(t, d, "agent-2", 1)
	seedBook(t, d, "agent-1", 1)

	// Act
	_, err := d.OpenPage(ctx(), "agent-1", 10, other[0])

	// Assert
	if !errors.Is(err, ErrStalePointer) {
		t.Fatalf("error = %v, want ErrStalePointer", err)
	}
	s.assertTracedRefusal(t, "names no line of book")
}

func TestOpenPageRefusesAPointerThatNamesNoRow(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	seedBook(t, d, "agent-1", 1)

	// Act
	_, err := d.OpenPage(ctx(), "agent-1", 10, encodePointer(9999))

	// Assert
	if !errors.Is(err, ErrStalePointer) {
		t.Fatalf("error = %v, want ErrStalePointer", err)
	}
}

func TestOpenPageRefusesAnEmptyAgentValue(t *testing.T) {
	// Arrange: "unknown agent" is ONLY an empty agent value.
	d, s := newStore(t)

	// Act
	_, err := d.OpenPage(ctx(), "", 10, nil)

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
	s.assertTracedRefusal(t, "agent id value is empty")
}

func TestOpenPageRefusesAZeroPageSize(t *testing.T) {
	// Arrange
	d, s := newStore(t)

	// Act
	_, err := d.OpenPage(ctx(), "agent-1", 0, nil)

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
	s.assertTracedRefusal(t, "page_size is zero")
}

func TestOpenPagePinsTheWatchAtTheGlobalWriteOrdinal(t *testing.T) {
	// Arrange: the pin is taken inside the page's own transaction, so nothing
	// is missed or doubled between the page and the stream that follows it.
	d, _ := newStore(t)
	seedBook(t, d, "agent-1", 2)
	want := scalar[uint64](t, d, `SELECT MAX(write_seq) FROM entry`)

	// Act
	opened, err := d.OpenPage(ctx(), "agent-1", 10, nil)

	// Assert
	if err != nil {
		t.Fatalf("OpenPage: %v", err)
	}
	if opened.PinSeq != want {
		t.Fatalf("PinSeq = %d, want %d", opened.PinSeq, want)
	}
}

// A WATCH OPENED BEHIND THE HEAD IS CAUGHT UP BY ITS OWN OPEN, and these three
// say so at each distance a caller can be behind by. The catch-up is the OPEN's
// page — every line newer than `known_through` — and the pin is taken in that
// same transaction, so the replay that follows has nothing left to serve. A
// caller whose tail stood on the pin while the page skipped the intervening
// rows would wait forever for lines already written, which is exactly the stall
// these pin: the shim's teardown concludes a tail through the book's HEAD, and
// a tail that never received the head cannot end on it.

func TestAWatchOpenedOneRowBehindTheHeadIsCaughtUpByItsPage(t *testing.T) {
	// Arrange: the caller has read everything but the newest line.
	d, _ := newStore(t)
	pointers := seedBook(t, d, "agent-1", 2)

	// Act
	opened, err := d.OpenPage(ctx(), "agent-1", 10, pointers[0])
	if err != nil {
		t.Fatalf("OpenPage: %v", err)
	}
	replay, err := d.LinesSince(ctx(), "agent-1", opened.PinSeq)
	if err != nil {
		t.Fatalf("LinesSince: %v", err)
	}

	// Assert: the head came in the page, and the tail begins after it.
	if got := pageValues(opened.Page); len(got) != 1 || got[0] != pointers[1].GetValue() {
		t.Fatalf("page = %v, want only the head %q", got, pointers[1].GetValue())
	}
	if len(replay) != 0 {
		t.Fatalf("replay = %d lines, want none — the page already carried them", len(replay))
	}
}

func TestAWatchOpenedManyRowsBehindTheHeadIsCaughtUpByItsPage(t *testing.T) {
	// Arrange: five lines landed since the caller's mark, and the page budget
	// is wide enough to carry all of them.
	d, _ := newStore(t)
	pointers := seedBook(t, d, "agent-1", 6)

	// Act
	opened, err := d.OpenPage(ctx(), "agent-1", 10, pointers[0])
	if err != nil {
		t.Fatalf("OpenPage: %v", err)
	}
	replay, err := d.LinesSince(ctx(), "agent-1", opened.PinSeq)
	if err != nil {
		t.Fatalf("LinesSince: %v", err)
	}

	// Assert: every line between the mark and the head, newest first, and
	// nothing left for the tail.
	want := []string{
		pointers[5].GetValue(), pointers[4].GetValue(), pointers[3].GetValue(),
		pointers[2].GetValue(), pointers[1].GetValue(),
	}
	got := pageValues(opened.Page)
	if len(got) != len(want) {
		t.Fatalf("page = %v, want the five lines newer than the mark", got)
	}
	for i := range want {
		if got[i] != want[i] {
			t.Fatalf("page[%d] = %q, want %q", i, got[i], want[i])
		}
	}
	if len(replay) != 0 {
		t.Fatalf("replay = %d lines, want none — the page already carried them", len(replay))
	}
}

func TestAWatchOpenedAtTheHeadIsCaughtUpWithNothing(t *testing.T) {
	// Arrange: the caller's mark IS the head, which is the ordinary re-open.
	d, _ := newStore(t)
	pointers := seedBook(t, d, "agent-1", 3)

	// Act
	opened, err := d.OpenPage(ctx(), "agent-1", 10, pointers[2])
	if err != nil {
		t.Fatalf("OpenPage: %v", err)
	}
	replay, err := d.LinesSince(ctx(), "agent-1", opened.PinSeq)
	if err != nil {
		t.Fatalf("LinesSince: %v", err)
	}

	// Assert
	if got := pageValues(opened.Page); len(got) != 0 {
		t.Fatalf("page = %v, want nothing — the caller is already at the head", got)
	}
	if len(replay) != 0 {
		t.Fatalf("replay = %d lines, want none", len(replay))
	}
}

// pageValues renders a page's pointers in the order it served them.
func pageValues(page *storev1.AgentSessionPage) []string {
	out := make([]string, 0, len(page.GetLines()))
	for _, line := range page.GetLines() {
		out = append(out, line.GetAt().GetValue())
	}
	return out
}

func TestOpenPageNeverReturnsAnUnservedRow(t *testing.T) {
	// Arrange: an unserved row carries NO BOOK, and the NULL book is what makes
	// it structurally unreachable from a page query.
	d, _ := newStore(t)
	writeOK(t, d, unservedEntry("w-k", "u-k", &storev1.StoreUnservedItem{
		UnservedItem: &storev1.StoreUnservedItem_VendorSpecific{VendorSpecific: &storev1.StoreVendorSpecific{Kind: "hook", Raw: rawRecord("hook")}},
	}))
	seedBook(t, d, "agent-1", 1)

	// Act
	opened, err := d.OpenPage(ctx(), "agent-1", 10, nil)

	// Assert
	if err != nil {
		t.Fatalf("OpenPage: %v", err)
	}
	if got := len(opened.Page.GetLines()); got != 1 {
		t.Fatalf("lines = %d, want only the real page line", got)
	}
}

// ---- ReadPage ----

func TestReadPageWalksOlderThanTheServedPointer(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	pointers := seedBook(t, d, "agent-1", 5)

	// Act
	page, err := d.ReadPage(ctx(), "agent-1", 2, pointers[4])

	// Assert
	if err != nil {
		t.Fatalf("ReadPage: %v", err)
	}
	if got := len(page.GetLines()); got != 2 {
		t.Fatalf("lines = %d, want 2", got)
	}
	if page.GetMore() == nil {
		t.Fatal("boundary = floor, want more")
	}
	if got := page.GetMore().GetLastItem().GetValue(); got != pointers[2].GetValue() {
		t.Fatalf("last_item = %q, want %q", got, pointers[2].GetValue())
	}
}

func TestReadPageReportsTheFloorAtTheOldestRetainedLine(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	pointers := seedBook(t, d, "agent-1", 3)

	// Act
	page, err := d.ReadPage(ctx(), "agent-1", 10, pointers[2])

	// Assert
	if err != nil {
		t.Fatalf("ReadPage: %v", err)
	}
	if got := len(page.GetLines()); got != 2 {
		t.Fatalf("lines = %d, want 2", got)
	}
	if page.GetFloor() == nil {
		t.Fatal("boundary = more, want floor")
	}
}

func TestReadPageRefusesAnUnsetAfterPointer(t *testing.T) {
	// Arrange: there is no first-page arm — the first page is the open's
	// answer and this verb only ever continues.
	d, s := newStore(t)
	seedBook(t, d, "agent-1", 1)

	// Act
	_, err := d.ReadPage(ctx(), "agent-1", 10, nil)

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
	s.assertTracedRefusal(t, "after is unset")
}

func TestReadPageRefusesAPointerFromAnotherBook(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	other := seedBook(t, d, "agent-2", 1)
	seedBook(t, d, "agent-1", 1)

	// Act
	_, err := d.ReadPage(ctx(), "agent-1", 10, other[0])

	// Assert
	if !errors.Is(err, ErrStalePointer) {
		t.Fatalf("error = %v, want ErrStalePointer", err)
	}
}

func TestReadPageRefusesAZeroPageSize(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	pointers := seedBook(t, d, "agent-1", 1)

	// Act
	_, err := d.ReadPage(ctx(), "agent-1", 0, pointers[0])

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
}

// ---- LinesSince ----

func TestLinesSinceReplaysInWriteOrder(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	seedBook(t, d, "agent-1", 3)

	// Act
	lines, err := d.LinesSince(ctx(), "agent-1", 0)

	// Assert
	if err != nil {
		t.Fatalf("LinesSince: %v", err)
	}
	if len(lines) != 3 {
		t.Fatalf("lines = %d, want 3", len(lines))
	}
	for i := 1; i < len(lines); i++ {
		if lines[i].WriteSeq <= lines[i-1].WriteSeq {
			t.Fatalf("write ordinals are not ascending: %d then %d", lines[i-1].WriteSeq, lines[i].WriteSeq)
		}
	}
}

func TestLinesSinceExcludesEverythingAtOrBelowThePin(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	seedBook(t, d, "agent-1", 3)
	pin := scalar[uint64](t, d, `SELECT MAX(write_seq) FROM entry`)
	seedBook(t, d, "agent-1", 1)

	// Act
	lines, err := d.LinesSince(ctx(), "agent-1", pin)

	// Assert
	if err != nil {
		t.Fatalf("LinesSince: %v", err)
	}
	if len(lines) != 1 {
		t.Fatalf("lines = %d, want only the one written after the pin", len(lines))
	}
}

func TestLinesSinceStreamsAnUpsertedOldRowAtItsOriginalPointer(t *testing.T) {
	// Arrange: the row keeps its place in the book but is NEW INFORMATION, so
	// it must reach a watcher — carrying the pointer the caller already holds.
	d, _ := newStore(t)
	pointers := seedBook(t, d, "agent-1", 2)
	pin := scalar[uint64](t, d, `SELECT MAX(write_seq) FROM entry`)

	// Act: the first unit settles.
	first := seedUnitID("agent-1", 2, 1)
	writeOK(t, d, pageEntry("w-settle", "u-"+first, "agent-1", frameItem(activityFrame("agent-1", "act-"+first, bashSuccess()))))
	lines, err := d.LinesSince(ctx(), "agent-1", pin)

	// Assert
	if err != nil {
		t.Fatalf("LinesSince: %v", err)
	}
	if len(lines) != 1 {
		t.Fatalf("lines = %d, want the upserted row", len(lines))
	}
	if got := lines[0].Line.GetAt().GetValue(); got != pointers[0].GetValue() {
		t.Fatalf("pointer = %q, want the ORIGINAL %q", got, pointers[0].GetValue())
	}
}

func TestLinesSinceNeverReplaysAnUnservedRow(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	seedBook(t, d, "agent-1", 1)
	writeOK(t, d, unservedEntry("w-k", "u-k", &storev1.StoreUnservedItem{
		UnservedItem: &storev1.StoreUnservedItem_VendorSpecific{VendorSpecific: &storev1.StoreVendorSpecific{Kind: "hook", Raw: rawRecord("hook")}},
	}))

	// Act
	lines, err := d.LinesSince(ctx(), "agent-1", 0)

	// Assert
	if err != nil {
		t.Fatalf("LinesSince: %v", err)
	}
	if len(lines) != 1 {
		t.Fatalf("lines = %d, want only the real page line", len(lines))
	}
}

func TestLinesSinceRefusesAnEmptyAgentValue(t *testing.T) {
	// Arrange
	d, s := newStore(t)

	// Act
	_, err := d.LinesSince(ctx(), "", 0)

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("error = %v, want ErrInvalid", err)
	}
	s.assertTracedRefusal(t, "agent id value is empty")
}

func TestLinesSinceReportsAStorageFailureOnAClosedDatabase(t *testing.T) {
	// Arrange
	d, s := newStore(t)
	if err := d.Close(); err != nil {
		t.Fatalf("close: %v", err)
	}

	// Act
	_, err := d.LinesSince(ctx(), "agent-1", 0)

	// Assert
	if !errors.Is(err, ErrStorage) {
		t.Fatalf("error = %v, want ErrStorage", err)
	}
	s.assertLogged(t, "error", "refused")
}

// TestAReadIsAnsweredWhileAWriterHoldsTheWriteLock pins the reason the read
// path begins DEFERRED. The owner's store answered two OpenAgentSession calls
// with `store.db.open-page` ERROR "begin read transaction: database is locked
// (5) (SQLITE_BUSY)": every transaction took the write lock at BEGIN, so a
// page repaint could be refused outright because a producer happened to be
// writing.
//
// THE BOUND IS 1s AND IT SEPARATES TWO OUTCOMES, not two speeds. A read that
// takes only its WAL snapshot answers in under a millisecond however busy the
// writer is; a read that queues for the write lock waits out the DSN's
// busy_timeout(5000) and then fails. Anything between the two is the failure
// this test exists to catch.
func TestAReadIsAnsweredWhileAWriterHoldsTheWriteLock(t *testing.T) {
	const bound = time.Second
	tests := []struct {
		name string
		read func(d *DB, seeded []*storev1.StoreItemPointer) error
	}{
		{
			name: "an opening page",
			read: func(d *DB, _ []*storev1.StoreItemPointer) error {
				_, err := d.OpenPage(context.Background(), "reader-book", 10, nil)
				return err
			},
		},
		{
			name: "a page walk back",
			read: func(d *DB, seeded []*storev1.StoreItemPointer) error {
				_, err := d.ReadPage(context.Background(), "reader-book", 10, seeded[len(seeded)-1])
				return err
			},
		},
		{
			name: "a bash run replay",
			read: func(d *DB, _ []*storev1.StoreItemPointer) error {
				_, err := d.BashRun(context.Background(), "no-such-run")
				return err
			},
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange: a book to read, and another connection holding the
			// write lock for the whole of this test.
			d, _ := newStore(t)
			seeded := seedBook(t, d, "reader-book", 3)
			writer, err := d.sql.BeginTx(context.Background(), nil)
			if err != nil {
				t.Fatalf("holding the write lock: %v", err)
			}
			defer writer.Rollback() //nolint:errcheck // the lock is released by the rollback

			// Act
			done := make(chan error, 1)
			go func() { done <- test.read(d, seeded) }()

			// Assert
			select {
			case err := <-done:
				if err != nil {
					t.Fatalf("read while a writer held the lock: %v", err)
				}
			case <-time.After(bound):
				t.Fatalf("the read did not answer within %v — it is queueing for the write lock", bound)
			}
		})
	}
}

// ---- the read pool: a read never waits on a write ----

// TestAReadCompletesWhileAWriteTransactionIsHeld is the structural assertion
// behind the read pool. The read runs SYNCHRONOUSLY while the write
// transaction is open and the single write connection is taken: if a read path
// ever reaches the write pool again, this does not fail slowly, it never
// returns, and the package timeout says so.
//
// It is what the DSN split buys over `beginRead`'s ReadOnly option alone. That
// option was a per-call-site fix for a per-connection property, and one
// forgotten option would have queued a page repaint behind a producer's write
// exactly as before.
func TestAReadCompletesWhileAWriteTransactionIsHeld(t *testing.T) {
	tests := []struct {
		name string
		// read is the read path exercised while the writer holds its
		// transaction. Each opens through a different door of the read pool:
		// a read transaction, and a pooled statement with no transaction.
		read func(t *testing.T, d *DB)
	}{
		{
			name: "a page open, which begins a read transaction",
			read: func(t *testing.T, d *DB) {
				if _, err := d.OpenPage(ctx(), "agent-1", 10, nil); err != nil {
					t.Fatalf("OpenPage while a write was held: %v", err)
				}
			},
		},
		{
			name: "a live-work scan, which runs a pooled statement",
			read: func(t *testing.T, d *DB) {
				if _, err := d.LiveWork(ctx(), "agent-main"); err != nil {
					t.Fatalf("LiveWork while a write was held: %v", err)
				}
			},
		},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange: a book to read, then a write transaction held open.
			d, _ := newStore(t)
			seedBook(t, d, "agent-1", 1)
			tx, release, err := d.beginWrite(ctx(), WriteInteractive)
			if err != nil {
				t.Fatalf("beginWrite: %v", err)
			}
			defer release()
			defer tx.Rollback() //nolint:errcheck // the fixture write is never committed
			if _, err := tx.ExecContext(ctx(), `UPDATE schema_meta SET version = version`); err != nil {
				t.Fatalf("the fixture write did not take the write lock: %v", err)
			}

			// Act + Assert: the read answers without the writer letting go.
			test.read(t, d)
		})
	}
}

// TestTheReadPoolRefusesAWrite pins the second half of the split: the pool
// carries `query_only(true)`, so a read path that grew a write is a hard error
// at the first attempt rather than a silent second writer the gate knows
// nothing about.
func TestTheReadPoolRefusesAWrite(t *testing.T) {
	tests := []struct {
		name      string
		statement string
	}{
		{name: "an insert", statement: `INSERT INTO schema_meta(version) VALUES (99)`},
		{name: "an update", statement: `UPDATE schema_meta SET version = 99`},
		{name: "a delete", statement: `DELETE FROM schema_meta`},
		{name: "DDL", statement: `CREATE TABLE smuggled (x INTEGER)`},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			d, _ := newStore(t)

			// Act
			_, err := d.read.ExecContext(ctx(), test.statement)

			// Assert
			if err == nil {
				t.Fatalf("the read pool applied %q; it must refuse every write", test.statement)
			}
			if !strings.Contains(err.Error(), "readonly") {
				t.Fatalf("the read pool refused %q with %v, want SQLite's readonly refusal", test.statement, err)
			}
			// The refusal is the store's own error the moment a read path
			// reports it: every read path wraps what the database hands back
			// through storagef, which is the class the server turns into a
			// storage_failure arm.
			if wrapped := storagef(err, "writing from a read path"); !errors.Is(wrapped, ErrStorage) {
				t.Fatalf("the refusal did not survive as an ErrStorage: %v", wrapped)
			}
			if got := scalar[int](t, d, `SELECT version FROM schema_meta`); got != SchemaVersion {
				t.Fatalf("schema_meta version = %d, want %d unchanged", got, SchemaVersion)
			}
		})
	}
}

// ---- the settle instant survives persistence and replay ----

// A settled response's settle instant is CONVERSATION CONTENT the store carries
// opaque inside the frame blob — it is never a column the store projects — so it
// must read back byte-identical after a write → read round trip. This is the
// invariant behind the response bubble's "N ago" corner: the daemon stamps the
// corner from the terminal's carried settled_at, and a re-resolved feed (a
// workspace re-opened, the daemon restarted, a page reconnected) reads the frame
// back from here. Were the instant lost in persistence, every re-resolve would
// fall back to the daemon's compose-time Now() and the age would reset to
// "seconds ago" for a response that settled long before — the bug this locks
// against. The store persists the whole StoreEntry proto, so the field survives
// for free; this test is the guard that it stays that way.
func TestASettledResponsesSettleInstantSurvivesReplay(t *testing.T) {
	// Arrange: a settled response carrying a real settle instant, written once.
	d, _ := newStore(t)
	const settled = int64(1_700_000_000_000)
	writeOK(t, d, pageEntry("w-settle", "u-settle", "agent-1",
		frameItem(activityFrame("agent-1", "act-settle", proseSettledAt("done", settled)))))

	// Act: replay the page, exactly as a re-resolved feed reads it back.
	opened, err := d.OpenPage(ctx(), "agent-1", 1, nil)
	if err != nil {
		t.Fatalf("OpenPage: %v", err)
	}

	// Assert: the carried instant read back unchanged, so the daemon stamps it
	// and never falls back to Now().
	got := opened.Page.GetLines()[0].GetLine().GetAgentItem().GetAgentFrame().
		GetUpdate().GetActivity().GetResponse().GetSuccess().GetSettledAt().GetAtMs()
	if got != settled {
		t.Fatalf("replayed settled_at = %d, want the original settle instant %d", got, settled)
	}
}

// The genuine "no instant observed" case must read back UNSET, not defaulted to
// some instant: it is the ONE case the daemon's Now() fallback legitimately
// serves, and defaulting it here would hide a producer that carried none.
func TestAResponseWithNoSettleInstantReadsBackUnset(t *testing.T) {
	// Arrange: a settled response the producer carried no instant for.
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w-none", "u-none", "agent-1",
		frameItem(activityFrame("agent-1", "act-none", proseSettledAt("done", 0)))))

	// Act.
	opened, err := d.OpenPage(ctx(), "agent-1", 1, nil)
	if err != nil {
		t.Fatalf("OpenPage: %v", err)
	}

	// Assert: settled_at reads back unset, leaving Now() as the genuine last resort.
	if got := opened.Page.GetLines()[0].GetLine().GetAgentItem().GetAgentFrame().
		GetUpdate().GetActivity().GetResponse().GetSuccess().GetSettledAt(); got != nil {
		t.Fatalf("replayed settled_at = %v, want unset", got)
	}
}

func TestOpenPageServesEachLineWithItsTurn(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, stampedTurn(pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-1", prose()))), "turn-a"))

	// Act
	opened, err := d.OpenPage(ctx(), "agent-1", 10, nil)

	// Assert
	if err != nil {
		t.Fatalf("OpenPage: %v", err)
	}
	if got := opened.Page.GetLines()[0].GetTurn().GetValue(); got != "turn-a" {
		t.Fatalf("served turn = %q, want %q", got, "turn-a")
	}
}

// ---- the read statements seek real indexes ----

func TestEveryReadStatementBuildsNoAutomaticIndex(t *testing.T) {
	tests := []struct {
		name      string
		statement string
		args      []any
	}{
		{name: "the newest page", statement: pageNewestSQL, args: []any{"agent-1", kindPageLine, 11}},
		{name: "the page before a line", statement: pageBeforeSQL, args: []any{"agent-1", kindPageLine, testNow, 0, 100, 11}},
		{name: "the page through an instant", statement: pageThroughSQL, args: []any{"agent-1", kindPageLine, testNow, 11}},
		{name: "the catch-up page", statement: pageWrittenAfterSQL, args: []any{"agent-1", kindPageLine, 100, 11}},
		{name: "the place of one row", statement: placeOfRowSQL, args: []any{100}},
		{name: "the stale-pointer probe", statement: pointerInBookSQL, args: []any{1, "agent-1", kindPageLine, kindRetired}},
		{name: "the cursor listing with its conversion", statement: cursorsSQL + ` WHERE c.file_id = ? ORDER BY c.file_id ASC`, args: []any{"12:34"}},
		{name: "the lines-since replay", statement: linesSinceSQL, args: []any{"agent-1", kindPageLine, kindRetired, 0}},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			d, _ := newStore(t)

			// Act
			plan := queryPlan(t, d, test.statement, test.args...)

			// Assert
			assertNoAutomaticIndex(t, test.name, plan)
		})
	}
}

// ---- cancelled reads and the WAL ----

// cancelledOpenPages runs many OpenPage calls whose deadlines land at spread
// instants inside the read, from several goroutines, and returns once every
// one of them has returned.
func cancelledOpenPages(t *testing.T, d *DB, book string) {
	t.Helper()
	var wg sync.WaitGroup
	for g := 0; g < 8; g++ {
		wg.Add(1)
		go func(g int) {
			defer wg.Done()
			for i := 0; i < 60; i++ {
				deadline := time.Duration((i*37+g*11)%3000) * time.Microsecond
				c, cancel := context.WithTimeout(context.Background(), deadline)
				_, _ = d.OpenPage(c, book, 2000, nil)
				cancel()
			}
		}(g)
	}
	wg.Wait()
}

// modernc.org/sqlite before v1.40.1 dropped a stepped statement unclosed when
// the context ended between its first step and the query's return. The
// statement kept its read snapshot for the life of the process, so every later
// checkpoint copied nothing and the WAL grew without bound (the owner's store
// sat at 0 of 47,506 frames for four hours). This fails on that driver.
func TestCancelledOpenPagesLeaveTheWALFullyCheckpointable(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	seedBook(t, d, "b", 3000)
	if _, err := d.Checkpoint(ctx(), TriggerIdle); err != nil {
		t.Fatalf("Checkpoint: %v", err)
	}
	cancelledOpenPages(t, d, "b")
	seedBook(t, d, "b", 50)

	// Act
	result, err := d.Checkpoint(ctx(), TriggerIdle)

	// Assert
	if err != nil {
		t.Fatalf("Checkpoint: %v", err)
	}
	if result.Checkpointed != result.WALFrames {
		t.Fatalf("checkpointed %d of %d frames; a cancelled read still pins the WAL", result.Checkpointed, result.WALFrames)
	}
}

func TestACancelledOpenPageHoldsNoConnectionOnceItReturns(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	seedBook(t, d, "b", 3000)

	// Act
	cancelledOpenPages(t, d, "b")

	// Assert
	if inUse := d.read.Stats().InUse; inUse != 0 {
		t.Fatalf("read pool InUse = %d after every OpenPage returned; a rollback is still running off the caller's goroutine", inUse)
	}
}

// ---- the conversation place ----

// pointerOf is the pointer a write published for its one line.
func pointerOf(t *testing.T, result WriteResult) string {
	t.Helper()
	if len(result.Lines) != 1 {
		t.Fatalf("published lines = %d, want 1", len(result.Lines))
	}
	return result.Lines[0].Line.GetAt().GetValue()
}

func TestOpenPageOrdersABookByPlaceNotByArrival(t *testing.T) {
	// Arrange: the later-placed line arrives first.
	d, _ := newStore(t)
	late := pointerOf(t, writeOK(t, d, placedLine("w1", "u1", "agent-1", 200, 0)))
	early := pointerOf(t, writeOK(t, d, placedLine("w2", "u2", "agent-1", 100, 0)))

	// Act
	opened, err := d.OpenPage(ctx(), "agent-1", 10, nil)

	// Assert
	if err != nil {
		t.Fatalf("OpenPage: %v", err)
	}
	if got := servedPointers(opened.Page.GetLines()); !slicesEqual(got, []string{late, early}) {
		t.Fatalf("served = %v, want descending place %v", got, []string{late, early})
	}
}

func TestOpenPageRanksLinesOfOneInstantByOrdinal(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	second := pointerOf(t, writeOK(t, d, placedLine("w1", "u1", "agent-1", 100, 1)))
	first := pointerOf(t, writeOK(t, d, placedLine("w2", "u2", "agent-1", 100, 0)))

	// Act
	opened, err := d.OpenPage(ctx(), "agent-1", 10, nil)

	// Assert
	if err != nil {
		t.Fatalf("OpenPage: %v", err)
	}
	if got := servedPointers(opened.Page.GetLines()); !slicesEqual(got, []string{second, first}) {
		t.Fatalf("served = %v, want ordinal 1 above ordinal 0", got)
	}
}

func TestOpenPageBreaksAnExactPlaceTieByFirstInsert(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	older := pointerOf(t, writeOK(t, d, placedLine("w1", "u1", "agent-1", 100, 0)))
	newer := pointerOf(t, writeOK(t, d, placedLine("w2", "u2", "agent-1", 100, 0)))

	// Act
	opened, err := d.OpenPage(ctx(), "agent-1", 10, nil)

	// Assert
	if err != nil {
		t.Fatalf("OpenPage: %v", err)
	}
	if got := servedPointers(opened.Page.GetLines()); !slicesEqual(got, []string{newer, older}) {
		t.Fatalf("served = %v, want the stable tiebreak %v", got, []string{newer, older})
	}
}

func TestOpenPageServesEveryLineWithItsPlace(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, placedLine("w1", "u1", "agent-1", 100, 3))

	// Act
	opened, err := d.OpenPage(ctx(), "agent-1", 10, nil)

	// Assert
	if err != nil {
		t.Fatalf("OpenPage: %v", err)
	}
	if got := opened.Page.GetLines()[0].GetRecordedPlace(); got.GetAtMs() != 100 || got.GetOrdinal() != 3 {
		t.Fatalf("place = %v, want recorded 100.3", got)
	}
}

func TestOpenPageCatchUpDeliversALineWrittenLaterButPlacedEarlier(t *testing.T) {
	// Arrange: the caller holds the line at 200; a transcript read late then
	// writes a line placed at 100.
	d, _ := newStore(t)
	mark := pointerOf(t, writeOK(t, d, placedLine("w1", "u1", "agent-1", 200, 0)))
	late := pointerOf(t, writeOK(t, d, placedLine("w2", "u2", "agent-1", 100, 0)))

	// Act
	opened, err := d.OpenPage(ctx(), "agent-1", 10, &storev1.StoreItemPointer{Value: mark})

	// Assert
	if err != nil {
		t.Fatalf("OpenPage: %v", err)
	}
	if got := servedPointers(opened.Page.GetLines()); !slicesEqual(got, []string{late}) {
		t.Fatalf("served = %v, want only the late-written line %v", got, []string{late})
	}
}

func TestOpenPageCatchUpOrdersWhatItDeliversByPlace(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	mark := pointerOf(t, writeOK(t, d, placedLine("w1", "u1", "agent-1", 50, 0)))
	lower := pointerOf(t, writeOK(t, d, placedLine("w2", "u2", "agent-1", 100, 0)))
	higher := pointerOf(t, writeOK(t, d, placedLine("w3", "u3", "agent-1", 300, 0)))

	// Act
	opened, err := d.OpenPage(ctx(), "agent-1", 10, &storev1.StoreItemPointer{Value: mark})

	// Assert
	if err != nil {
		t.Fatalf("OpenPage: %v", err)
	}
	if got := servedPointers(opened.Page.GetLines()); !slicesEqual(got, []string{higher, lower}) {
		t.Fatalf("served = %v, want descending place %v", got, []string{higher, lower})
	}
}

func TestReadPageWalksToTheLinesPlacedBeforeTheNamedLine(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	low := pointerOf(t, writeOK(t, d, placedLine("w1", "u1", "agent-1", 100, 0)))
	named := pointerOf(t, writeOK(t, d, placedLine("w2", "u2", "agent-1", 200, 0)))
	writeOK(t, d, placedLine("w3", "u3", "agent-1", 300, 0))

	// Act
	page, err := d.ReadPage(ctx(), "agent-1", 10, &storev1.StoreItemPointer{Value: named})

	// Assert
	if err != nil {
		t.Fatalf("ReadPage: %v", err)
	}
	if got := servedPointers(page.GetLines()); !slicesEqual(got, []string{low}) {
		t.Fatalf("served = %v, want only the line placed before %v", got, []string{low})
	}
}

func TestReadPageBoundsByTheNamedLinesCurrentPlace(t *testing.T) {
	// Arrange: the named line was served unplaced (at its receipt instant) and
	// has since gained a recorded place below another line.
	d, _ := newStore(t)
	named := pointerOf(t, writeOK(t, d, pageEntry("w1", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-u1", prose())))))
	below := pointerOf(t, writeOK(t, d, placedLine("w2", "u2", "agent-1", 100, 0)))
	writeOK(t, d, placed(pageEntry("w3", "u1", "agent-1", frameItem(activityFrame("agent-1", "act-u1", bashSuccess()))), 200, 0))

	// Act
	page, err := d.ReadPage(ctx(), "agent-1", 10, &storev1.StoreItemPointer{Value: named})

	// Assert
	if err != nil {
		t.Fatalf("ReadPage: %v", err)
	}
	if got := servedPointers(page.GetLines()); !slicesEqual(got, []string{below}) {
		t.Fatalf("served = %v, want the lines below its current place 200 %v", got, []string{below})
	}
}

func TestReadPageThroughServesTheLinesPlacedAtOrBeforeTheBound(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	low := pointerOf(t, writeOK(t, d, placedLine("w1", "u1", "agent-1", 100, 0)))
	atBound := pointerOf(t, writeOK(t, d, placedLine("w2", "u2", "agent-1", 200, 9)))
	writeOK(t, d, placedLine("w3", "u3", "agent-1", 201, 0))

	// Act
	page, err := d.ReadPageThrough(ctx(), "agent-1", 10, 200)

	// Assert
	if err != nil {
		t.Fatalf("ReadPageThrough: %v", err)
	}
	if got := servedPointers(page.GetLines()); !slicesEqual(got, []string{atBound, low}) {
		t.Fatalf("served = %v, want %v", got, []string{atBound, low})
	}
}

func TestReadPageThroughReportsMoreWhenTheBudgetCutsTheBookShort(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, placedLine("w1", "u1", "agent-1", 100, 0))
	newest := pointerOf(t, writeOK(t, d, placedLine("w2", "u2", "agent-1", 150, 0)))

	// Act
	page, err := d.ReadPageThrough(ctx(), "agent-1", 1, 200)

	// Assert
	if err != nil {
		t.Fatalf("ReadPageThrough: %v", err)
	}
	if got := page.GetMore().GetLastItem().GetValue(); got != newest {
		t.Fatalf("more = %q, want the last served line %q", got, newest)
	}
}

func TestReadPageThroughAnEmptyKnownBookIsTheFloor(t *testing.T) {
	// Arrange: the agent is known, and nothing it said is placed that early.
	d, _ := newStore(t)
	writeOK(t, d, placedLine("w1", "u1", "agent-1", 500, 0))

	// Act
	page, err := d.ReadPageThrough(ctx(), "agent-1", 10, 100)

	// Assert
	if err != nil {
		t.Fatalf("ReadPageThrough: %v", err)
	}
	if len(page.GetLines()) != 0 || page.GetFloor() == nil {
		t.Fatalf("page = %v, want an empty page at the floor", page)
	}
}

func TestReadPageThroughRefusesABookTheStoreNeverHeardOf(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	_, err := d.ReadPageThrough(ctx(), "nobody", 10, 100)

	// Assert
	if !errors.Is(err, ErrUnknownAgent) {
		t.Fatalf("ReadPageThrough = %v, want ErrUnknownAgent", err)
	}
}

func TestReadPageThroughRefusesANonPositiveBound(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	_, err := d.ReadPageThrough(ctx(), "agent-1", 10, 0)

	// Assert
	if RefusalSite(err) != SiteThroughNotPositive {
		t.Fatalf("site = %q (error: %v), want %q", RefusalSite(err), err, SiteThroughNotPositive)
	}
}

func TestLinesSinceServesEachLineWithItsPlace(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, placedLine("w1", "u1", "agent-1", 100, 3))

	// Act
	lines, err := d.LinesSince(ctx(), "agent-1", 0)

	// Assert
	if err != nil {
		t.Fatalf("LinesSince: %v", err)
	}
	if got := lines[0].Line.GetRecordedPlace(); got.GetAtMs() != 100 || got.GetOrdinal() != 3 {
		t.Fatalf("place = %v, want recorded 100.3", got)
	}
}

func TestAPageLineMissingFromThePlaceIndexIsAStorageFailure(t *testing.T) {
	// Arrange: a damaged index, which placeRow never leaves behind.
	d, _ := newStore(t)
	writeOK(t, d, placedLine("w1", "u1", "agent-1", 100, 0))
	if _, err := d.sql.Exec(`DELETE FROM entry_place`); err != nil {
		t.Fatalf("damaging the index: %v", err)
	}

	// Act
	_, err := d.LinesSince(ctx(), "agent-1", 0)

	// Assert
	if !errors.Is(err, ErrStorage) {
		t.Fatalf("LinesSince = %v, want a storage failure", err)
	}
}
