package db

import (
	"errors"
	"strconv"
	"testing"

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
	// Arrange: an agent with no rows is a LEGAL empty book. Refusing it would
	// make a freshly spawned subagent unwatchable until it happened to speak.
	d, _ := newStore(t)

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
	s.assertLogged(t, "error", "names no line of book")
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
	s.assertLogged(t, "error", "agent id value is empty")
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
	s.assertLogged(t, "error", "page_size is zero")
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

func TestOpenPageNeverReturnsAKeepAliveRow(t *testing.T) {
	// Arrange: a keep-alive is a well-formed fact with NO BOOK, and the NULL
	// book is what makes it structurally unreachable from a page query.
	d, _ := newStore(t)
	writeOK(t, d, unservedEntry("w-k", "u-k", &storev1.StoreUnservedItem{
		UnservedItem: &storev1.StoreUnservedItem_Keepalive{Keepalive: promptItem("agent-1")},
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
	s.assertLogged(t, "error", "after is unset")
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

func TestLinesSinceNeverReplaysAKeepAliveRow(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	seedBook(t, d, "agent-1", 1)
	writeOK(t, d, unservedEntry("w-k", "u-k", &storev1.StoreUnservedItem{
		UnservedItem: &storev1.StoreUnservedItem_Keepalive{Keepalive: promptItem("agent-1")},
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
	s.assertLogged(t, "error", "agent id value is empty")
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
