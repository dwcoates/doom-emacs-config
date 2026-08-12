package db

import (
	"context"
	"fmt"
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

// seedOwned ingests one PERSISTENT record and stamps its owning message on the
// stored row.
//
// Writing ownership at record-write time is a SEPARATE concern with its own
// owner; this suite needs only the column's contents, so it seeds them
// directly rather than depending on that path landing first.
func seedOwned(t *testing.T, d *DB, session, owner string) uint64 {
	t.Helper()
	ev := persistentCore(session)
	if _, err := d.Ingest("test", []*corev1.Event{ev}, nil); err != nil {
		t.Fatalf("Ingest: %v", err)
	}
	seq := ev.GetSeq()
	if _, err := d.sql.Exec(
		`UPDATE event SET top_level_message_id = ? WHERE session_id = ? AND seq = ?`,
		owner, session, seq); err != nil {
		t.Fatalf("stamping ownership: %v", err)
	}
	return seq
}

// seedUnowned ingests a record that composes no message. Its ownership column
// stays SQL NULL, which is what "unowned" means here.
func seedUnowned(t *testing.T, d *DB, session string) uint64 {
	t.Helper()
	ev := persistentCore(session)
	if _, err := d.Ingest("test", []*corev1.Event{ev}, nil); err != nil {
		t.Fatalf("Ingest: %v", err)
	}
	return ev.GetSeq()
}

// pageMessages reads the page's ten slots in order, stopping at the first
// empty one, so a test asserts on what the page CARRIES rather than on which
// field number it landed in.
func pageMessages(p *corev1.MessagePage) []*corev1.StoredMessage {
	slots := []*corev1.StoredMessage{
		p.GetMessage_1(), p.GetMessage_2(), p.GetMessage_3(), p.GetMessage_4(), p.GetMessage_5(),
		p.GetMessage_6(), p.GetMessage_7(), p.GetMessage_8(), p.GetMessage_9(), p.GetMessage_10(),
	}
	var out []*corev1.StoredMessage
	for _, m := range slots {
		if m == nil {
			break
		}
		out = append(out, m)
	}
	return out
}

func messageIDs(p *corev1.MessagePage) []string {
	var out []string
	for _, m := range pageMessages(p) {
		out = append(out, m.GetMessageId())
	}
	return out
}

func headRequest(session string) *corev1.MessagePageRequest {
	return &corev1.MessagePageRequest{
		RequestId: "r1",
		SessionId: session,
		Anchor:    &corev1.MessagePageRequest_Head{Head: &corev1.MessagePageHead{}},
	}
}

func TestMessagePageCarriesAtMostTenDistinctMessages(t *testing.T) {
	// Arrange: more messages than a page can hold, one record each.
	d := openTemp(t)
	for i := range 25 {
		seedOwned(t, d, "s1", fmt.Sprintf("m%02d", i))
	}

	// Act
	page, err := d.MessagePage(context.Background(), headRequest("s1"))

	// Assert: exactly ten slots, and ten DISTINCT messages.
	if err != nil {
		t.Fatalf("MessagePage: %v", err)
	}
	ids := messageIDs(page)
	if len(ids) != 10 {
		t.Fatalf("page carried %d messages (%v), want 10", len(ids), ids)
	}
	seen := map[string]bool{}
	for _, id := range ids {
		if seen[id] {
			t.Fatalf("page repeated message %q: %v", id, ids)
		}
		seen[id] = true
	}
}

func TestMessagePageOrdersMessagesNewestFirst(t *testing.T) {
	// Arrange
	d := openTemp(t)
	seedOwned(t, d, "s1", "old")
	seedOwned(t, d, "s1", "mid")
	seedOwned(t, d, "s1", "new")

	// Act
	page, err := d.MessagePage(context.Background(), headRequest("s1"))

	// Assert
	if err != nil {
		t.Fatalf("MessagePage: %v", err)
	}
	got := messageIDs(page)
	want := []string{"new", "mid", "old"}
	for i := range want {
		if i >= len(got) || got[i] != want[i] {
			t.Fatalf("page order=%v, want %v", got, want)
		}
	}
}

func TestMessagePageDeliversAHugeMessageWholeInOneSlot(t *testing.T) {
	// Arrange: one message owning hundreds of records, plus one neighbour.
	const records = 400
	d := openTemp(t)
	seedOwned(t, d, "s1", "neighbour")
	for range records {
		seedOwned(t, d, "s1", "fat")
	}

	// Act
	page, err := d.MessagePage(context.Background(), headRequest("s1"))

	// Assert: two slots, and the fat message arrived complete.
	if err != nil {
		t.Fatalf("MessagePage: %v", err)
	}
	msgs := pageMessages(page)
	if len(msgs) != 2 {
		t.Fatalf("page carried %d messages, want 2", len(msgs))
	}
	if msgs[0].GetMessageId() != "fat" {
		t.Fatalf("newest slot is %q, want %q", msgs[0].GetMessageId(), "fat")
	}
	if got := len(msgs[0].GetRecords()); got != records {
		t.Fatalf("fat message carried %d records, want %d", got, records)
	}
}

func TestMessagePageOrdersOneMessagesRecordsOldestFirst(t *testing.T) {
	// Arrange
	d := openTemp(t)
	first := seedOwned(t, d, "s1", "m")
	second := seedOwned(t, d, "s1", "m")

	// Act
	page, err := d.MessagePage(context.Background(), headRequest("s1"))

	// Assert
	if err != nil {
		t.Fatalf("MessagePage: %v", err)
	}
	recs := page.GetMessage_1().GetRecords()
	if len(recs) != 2 || recs[0].GetSeq() != first || recs[1].GetSeq() != second {
		t.Fatalf("records=%v, want seqs [%d %d] in that order", recs, first, second)
	}
}

func TestMessagePageNeverGivesAnUnownedRecordASlot(t *testing.T) {
	// Arrange: turn boundaries and heartbeats outnumber the real messages.
	// If "unowned" were an empty string rather than NULL it would sort into
	// SELECT DISTINCT and take slots, and the user would silently see a short
	// page.
	d := openTemp(t)
	seedOwned(t, d, "s1", "m1")
	for range 20 {
		seedUnowned(t, d, "s1")
	}
	seedOwned(t, d, "s1", "m2")

	// Act
	page, err := d.MessagePage(context.Background(), headRequest("s1"))

	// Assert
	if err != nil {
		t.Fatalf("MessagePage: %v", err)
	}
	got := messageIDs(page)
	if len(got) != 2 || got[0] != "m2" || got[1] != "m1" {
		t.Fatalf("page carried %v, want exactly [m2 m1]", got)
	}
}

func TestMessagePageExcludesUnownedRecordsFromAMessagesRecords(t *testing.T) {
	// Arrange
	d := openTemp(t)
	owned := seedOwned(t, d, "s1", "m1")
	seedUnowned(t, d, "s1")

	// Act
	page, err := d.MessagePage(context.Background(), headRequest("s1"))

	// Assert
	if err != nil {
		t.Fatalf("MessagePage: %v", err)
	}
	recs := page.GetMessage_1().GetRecords()
	if len(recs) != 1 || recs[0].GetSeq() != owned {
		t.Fatalf("message carried %d records, want only the owned seq=%d", len(recs), owned)
	}
}

func TestMessagePageHeadResolvesWithoutTheCallerNamingASeq(t *testing.T) {
	// Arrange: the caller supplies an EMPTY head anchor and no position at all.
	d := openTemp(t)
	seedOwned(t, d, "s1", "m1")
	newest := seedOwned(t, d, "s1", "m2")

	// Act
	page, err := d.MessagePage(context.Background(), headRequest("s1"))

	// Assert: the newest record is on the page, so the store resolved the head.
	if err != nil {
		t.Fatalf("MessagePage: %v", err)
	}
	recs := page.GetMessage_1().GetRecords()
	if len(recs) != 1 || recs[0].GetSeq() != newest {
		t.Fatalf("head page newest record=%v, want seq=%d", recs, newest)
	}
}

func TestMessagePageMintsLastPageSeqAsThePositionTheScanStoppedAt(t *testing.T) {
	// Arrange: a message whose records straddle a newer one, so the seq the
	// scan stopped at and the oldest seq the page covers are DIFFERENT values.
	d := openTemp(t)
	deep := seedOwned(t, d, "s1", "wide")
	stopped := seedOwned(t, d, "s1", "wide")
	seedOwned(t, d, "s1", "recent")

	// Act
	page, err := d.MessagePage(context.Background(), headRequest("s1"))

	// Assert: last_page_seq is where the last selected owner was ENCOUNTERED,
	// never the minimum seq over the page's records.
	if err != nil {
		t.Fatalf("MessagePage: %v", err)
	}
	if page.GetLastPageSeq() != stopped {
		t.Fatalf("last_page_seq=%d, want the scan's stopping position %d (deep record was %d)",
			page.GetLastPageSeq(), stopped, deep)
	}
}

func TestMessagePageDoesNotSkipMessagesBeneathAWideSpanningMessage(t *testing.T) {
	// Arrange: a detached message that starts early and ends late owns a
	// record beneath every other message in the session. Anchoring the next
	// page at that depth would swallow the entire history between.
	d := openTemp(t)
	seedOwned(t, d, "s1", "wide")
	var buried []string
	for i := range 12 {
		id := fmt.Sprintf("m%02d", i)
		seedOwned(t, d, "s1", id)
		buried = append(buried, id)
	}
	seedOwned(t, d, "s1", "wide")

	// Act: page the head, then continue below it.
	first, err := d.MessagePage(context.Background(), headRequest("s1"))
	if err != nil {
		t.Fatalf("MessagePage head: %v", err)
	}
	second, err := d.MessagePage(context.Background(), &corev1.MessagePageRequest{
		RequestId: "r2",
		SessionId: "s1",
		Anchor:    &corev1.MessagePageRequest_BeforeSeq{BeforeSeq: first.GetLastPageSeq()},
	})

	// Assert: every buried message is delivered by one of the two pages.
	if err != nil {
		t.Fatalf("MessagePage before_seq: %v", err)
	}
	seen := map[string]bool{}
	for _, id := range append(messageIDs(first), messageIDs(second)...) {
		seen[id] = true
	}
	for _, id := range buried {
		if !seen[id] {
			t.Fatalf("message %q was skipped beneath the wide-spanning message", id)
		}
	}
}

func TestMessagePageDoesNotRedeliverAWideSpanningMessage(t *testing.T) {
	// Arrange: the wide message's tendril sits below where the first page's
	// scan stopped, so the next page's scan meets it again.
	d := openTemp(t)
	seedOwned(t, d, "s1", "wide")
	for i := range 12 {
		seedOwned(t, d, "s1", fmt.Sprintf("m%02d", i))
	}
	seedOwned(t, d, "s1", "wide")
	first, err := d.MessagePage(context.Background(), headRequest("s1"))
	if err != nil {
		t.Fatalf("MessagePage head: %v", err)
	}

	// Act
	second, err := d.MessagePage(context.Background(), &corev1.MessagePageRequest{
		RequestId: "r2",
		SessionId: "s1",
		Anchor:    &corev1.MessagePageRequest_BeforeSeq{BeforeSeq: first.GetLastPageSeq()},
	})

	// Assert
	if err != nil {
		t.Fatalf("MessagePage before_seq: %v", err)
	}
	for _, id := range messageIDs(second) {
		if id == "wide" {
			t.Fatalf("the wide-spanning message was delivered twice: page1=%v page2=%v",
				messageIDs(first), messageIDs(second))
		}
	}
}

func TestMessagePageInterleavedHistoryPartitionsAcrossEveryPage(t *testing.T) {
	// Arrange: a long history whose messages interleave — every message owns a
	// record early and a record late — walked page by page to the floor.
	d := openTemp(t)
	var all []string
	for i := range 25 {
		id := fmt.Sprintf("m%02d", i)
		seedOwned(t, d, "s1", id)
		all = append(all, id)
	}
	for _, id := range all {
		seedOwned(t, d, "s1", id)
	}

	// Act: page until the store reports the retained floor.
	seen := map[string]int{}
	req := headRequest("s1")
	for {
		page, err := d.MessagePage(context.Background(), req)
		if err != nil {
			t.Fatalf("MessagePage: %v", err)
		}
		for _, id := range messageIDs(page) {
			seen[id]++
		}
		if page.GetFloor() != nil {
			break
		}
		req = &corev1.MessagePageRequest{
			RequestId: "rN",
			SessionId: "s1",
			Anchor:    &corev1.MessagePageRequest_BeforeSeq{BeforeSeq: page.GetLastPageSeq()},
		}
	}

	// Assert: every message exactly once — no gap, no overlap.
	for _, id := range all {
		switch seen[id] {
		case 1:
		case 0:
			t.Fatalf("message %q fell in a gap between pages", id)
		default:
			t.Fatalf("message %q was delivered %d times", id, seen[id])
		}
	}
	if len(seen) != len(all) {
		t.Fatalf("paging carried %d distinct messages, want %d", len(seen), len(all))
	}
}

func TestMessagePageBeforeSeqContinuesWithoutOverlapOrGap(t *testing.T) {
	// Arrange: fifteen single-record messages, so the head page holds ten and
	// exactly five remain below it.
	d := openTemp(t)
	var all []string
	for i := range 15 {
		id := fmt.Sprintf("m%02d", i)
		seedOwned(t, d, "s1", id)
		all = append(all, id)
	}
	first, err := d.MessagePage(context.Background(), headRequest("s1"))
	if err != nil {
		t.Fatalf("MessagePage head: %v", err)
	}

	// Act: continue with the served last_page_seq VERBATIM.
	second, err := d.MessagePage(context.Background(), &corev1.MessagePageRequest{
		RequestId: "r2",
		SessionId: "s1",
		Anchor:    &corev1.MessagePageRequest_BeforeSeq{BeforeSeq: first.GetLastPageSeq()},
	})

	// Assert: the two pages partition the fifteen messages exactly.
	if err != nil {
		t.Fatalf("MessagePage before_seq: %v", err)
	}
	seen := map[string]int{}
	for _, id := range append(messageIDs(first), messageIDs(second)...) {
		seen[id]++
	}
	for _, id := range all {
		switch seen[id] {
		case 1:
		case 0:
			t.Fatalf("message %q fell in the gap between the two pages", id)
		default:
			t.Fatalf("message %q appeared on both pages", id)
		}
	}
	if len(seen) != len(all) {
		t.Fatalf("pages carried %d distinct messages, want %d", len(seen), len(all))
	}
}

func TestMessagePageReportsHistoryRemainsBelowWhenOlderMessagesRemain(t *testing.T) {
	// Arrange
	d := openTemp(t)
	for i := range 11 {
		seedOwned(t, d, "s1", fmt.Sprintf("m%02d", i))
	}

	// Act
	page, err := d.MessagePage(context.Background(), headRequest("s1"))

	// Assert
	if err != nil {
		t.Fatalf("MessagePage: %v", err)
	}
	if page.GetMore() == nil {
		t.Fatalf("boundary=%T, want HistoryRemainsBelow", page.GetBoundary())
	}
}

func TestMessagePageReportsTheRetainedFloorWhenNothingOlderRemains(t *testing.T) {
	// Arrange: fewer messages than a page holds, so the page reaches the
	// oldest RETAINED record. That is the store's own retention fact and must
	// be distinguishable from "more remains", never inferred from a short page.
	d := openTemp(t)
	seedOwned(t, d, "s1", "m1")

	// Act
	page, err := d.MessagePage(context.Background(), headRequest("s1"))

	// Assert
	if err != nil {
		t.Fatalf("MessagePage: %v", err)
	}
	if page.GetFloor() == nil {
		t.Fatalf("boundary=%T, want HistoryAtRetainedFloor", page.GetBoundary())
	}
}

func TestMessagePageDoesNotCallAnUnownedTailMoreHistory(t *testing.T) {
	// Arrange: an unowned record sits below the only message. It renders as
	// nothing, so reporting it as remaining history would hand the caller a
	// load-earlier affordance that resolves to no message at all.
	d := openTemp(t)
	seedUnowned(t, d, "s1")
	seedOwned(t, d, "s1", "m1")

	// Act
	page, err := d.MessagePage(context.Background(), headRequest("s1"))

	// Assert
	if err != nil {
		t.Fatalf("MessagePage: %v", err)
	}
	if page.GetFloor() == nil {
		t.Fatalf("boundary=%T, want HistoryAtRetainedFloor", page.GetBoundary())
	}
}

func TestMessagePageOnAnEmptySessionIsTheRetainedFloor(t *testing.T) {
	// Arrange
	d := openTemp(t)

	// Act
	page, err := d.MessagePage(context.Background(), headRequest("s1"))

	// Assert
	if err != nil {
		t.Fatalf("MessagePage: %v", err)
	}
	if len(pageMessages(page)) != 0 || page.GetFloor() == nil {
		t.Fatalf("empty session page=%v, want no messages and the retained floor", page)
	}
}

func TestMessagePageReadsOnlyTheRequestedSession(t *testing.T) {
	// Arrange
	d := openTemp(t)
	seedOwned(t, d, "other", "theirs")
	seedOwned(t, d, "s1", "mine")

	// Act
	page, err := d.MessagePage(context.Background(), headRequest("s1"))

	// Assert
	if err != nil {
		t.Fatalf("MessagePage: %v", err)
	}
	got := messageIDs(page)
	if len(got) != 1 || got[0] != "mine" {
		t.Fatalf("page carried %v, want only the requested session's message", got)
	}
}

func TestMessagePageEchoesTheRequestID(t *testing.T) {
	// Arrange
	d := openTemp(t)
	seedOwned(t, d, "s1", "m1")

	// Act
	page, err := d.MessagePage(context.Background(), headRequest("s1"))

	// Assert
	if err != nil {
		t.Fatalf("MessagePage: %v", err)
	}
	if page.GetRequestId() != "r1" {
		t.Fatalf("request_id=%q, want %q", page.GetRequestId(), "r1")
	}
}

func TestMessagePageRefusesARequestWithNoSession(t *testing.T) {
	// Arrange
	d := openTemp(t)

	// Act
	_, err := d.MessagePage(context.Background(), &corev1.MessagePageRequest{
		RequestId: "r1",
		Anchor:    &corev1.MessagePageRequest_Head{Head: &corev1.MessagePageHead{}},
	})

	// Assert
	if err == nil {
		t.Fatal("MessagePage accepted a request naming no session")
	}
}

func TestMessagePageRefusesARequestWithNoAnchor(t *testing.T) {
	// Arrange: an unset anchor oneof is not "the head". Defaulting would turn
	// a caller bug into a silent tail read.
	d := openTemp(t)

	// Act
	_, err := d.MessagePage(context.Background(), &corev1.MessagePageRequest{RequestId: "r1", SessionId: "s1"})

	// Assert
	if err == nil {
		t.Fatal("MessagePage accepted a request with no anchor arm")
	}
}

func TestMessagePageSurfacesACorruptRecord(t *testing.T) {
	// Arrange
	d := openTemp(t)
	seq := seedOwned(t, d, "s1", "m1")
	if _, err := d.sql.Exec(`UPDATE event SET payload = X'00' WHERE session_id = 's1' AND seq = ?`, seq); err != nil {
		t.Fatalf("corrupting fixture row: %v", err)
	}

	// Act
	_, err := d.MessagePage(context.Background(), headRequest("s1"))

	// Assert
	if err == nil {
		t.Fatal("MessagePage returned a page built from an undecodable record")
	}
}

// queryPlan returns EXPLAIN QUERY PLAN's details for one statement, joined so
// a test can assert on the whole plan at once.
func queryPlan(t *testing.T, d *DB, sql string, args ...any) string {
	t.Helper()
	rows, err := d.sql.Query("EXPLAIN QUERY PLAN "+sql, args...)
	if err != nil {
		t.Fatalf("EXPLAIN QUERY PLAN: %v", err)
	}
	defer rows.Close()
	var plan []string
	for rows.Next() {
		var id, parent, aux int
		var detail string
		if err := rows.Scan(&id, &parent, &aux, &detail); err != nil {
			t.Fatalf("scanning plan: %v", err)
		}
		plan = append(plan, detail)
	}
	if err := rows.Err(); err != nil {
		t.Fatalf("iterating plan: %v", err)
	}
	return strings.Join(plan, " | ")
}

func TestMessagePageOwnerSelectRunsOnItsIndexWithNoSort(t *testing.T) {
	// Arrange: the page's cost claim is that owner selection is ONE indexed
	// backward pass. A temp b-tree in the plan means SQLite materialized and
	// sorted the session's history first, which is the unbounded scan the
	// whole contract exists to remove.
	d := openTemp(t)
	for i := range 40 {
		seedOwned(t, d, "s1", fmt.Sprintf("m%02d", i))
	}

	// Act
	plan := queryPlan(t, d, ownerSelectSQL, "s1", uint64(1<<62))

	// Assert
	if !strings.Contains(plan, "event_message_page") {
		t.Fatalf("owner selection plan does not use event_message_page: %s", plan)
	}
	if strings.Contains(plan, "TEMP B-TREE") {
		t.Fatalf("owner selection plan sorts instead of walking the index: %s", plan)
	}
}

func TestMessagePageAlreadyServedCheckRunsOnItsOwnerIndex(t *testing.T) {
	// Arrange: the across-page no-repeat check runs once per candidate owner,
	// so it must be an index seek. A scan or a sort here would restore the
	// whole-history visit the page contract exists to remove.
	d := openTemp(t)
	for i := range 40 {
		seedOwned(t, d, "s1", fmt.Sprintf("m%02d", i))
	}

	// Act
	plan := queryPlan(t, d, ownerAboveAnchorSQL, "s1", "m01", uint64(1))

	// Assert
	if !strings.Contains(plan, "event_message_owner") {
		t.Fatalf("already-served check does not use event_message_owner: %s", plan)
	}
	if strings.Contains(plan, "TEMP B-TREE") {
		t.Fatalf("already-served check sorts instead of seeking the index: %s", plan)
	}
}

func TestSetPageSlotRefusesAnEleventhMessage(t *testing.T) {
	// Arrange: the schema declares ten slots, so an eleventh has nowhere to
	// go. It must fail loudly rather than be dropped into a short page.
	page := &corev1.MessagePage{}

	// Act
	err := setPageSlot(page, MessagePageSize, &corev1.StoredMessage{MessageId: "overflow"})

	// Assert
	if err == nil {
		t.Fatal("setPageSlot accepted a message past the declared slots")
	}
}
