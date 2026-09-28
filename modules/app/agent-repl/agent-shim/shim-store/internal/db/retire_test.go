package db

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"google.golang.org/protobuf/proto"
)

// ---- fixtures ----

// retireFileID is the file every heal fixture's batches are read from.
const retireFileID = "12:34"

// filePageEntry is a FILE-plane page line produced by conversion `version`.
func filePageEntry(writeID, upsertKey, book string, item *storev1.StoreAgentItem, version uint32) *storev1.StoreEntry {
	entry := pageEntry(writeID, upsertKey, book, item)
	entry.Plane = &storev1.Plane{Plane: &storev1.Plane_File{File: &storev1.PlaneFile{}}}
	entry.ConversionVersion = &version
	return entry
}

// cursorAt is a cursor advance on retireFileID stating the given conversion.
func cursorAt(offset int64, version uint32) *storev1.CursorState {
	return &storev1.CursorState{
		FileId: retireFileID, Path: "/t/a.jsonl", Offset: offset,
		Conversion: &storev1.CursorConversion{
			Version: version,
			State:   &storev1.CursorConversion_Current{Current: &storev1.CursorConversionCurrent{}},
		},
	}
}

// retirement names one row a re-read under `version` no longer converts to.
func retirement(upsertKey string, version uint32) *storev1.StoreRetirement {
	return &storev1.StoreRetirement{UpsertKey: upsertKey, ConversionVersion: version}
}

// healBatch writes one file-plane batch at `offset` under `version`, carrying
// the given entries and retirements.
func healBatch(t *testing.T, d *DB, offset int64, version uint32, entries []*storev1.StoreEntry, retirements ...*storev1.StoreRetirement) WriteResult {
	t.Helper()
	result, err := d.WriteBatch(ctx(), "test-sidecar", WriteBulk, &storev1.EntryBatch{
		Entries:       entries,
		CursorAdvance: cursorAt(offset, version),
		Retirements:   retirements,
	}, nil)
	if err != nil {
		t.Fatalf("WriteBatch: %v", err)
	}
	return result
}

// legacyPrompt stores a file-plane prompt row AS A PRE-VERSIONING SIDECAR LEFT
// IT: the frame carries no conversion_version at all. The store now refuses to
// write such a row, so it is written at version 1 and its frame rewritten in
// place — exactly the bytes the live store holds for the rows the heal exists
// to retire.
func legacyPrompt(t *testing.T, d *DB, upsertKey string) {
	t.Helper()
	healBatch(t, d, 100, 1, []*storev1.StoreEntry{filePageEntry("w-"+upsertKey, upsertKey, "agent-1", promptItem("agent-1"), 1)})
	legacy := filePageEntry("w-"+upsertKey, upsertKey, "agent-1", promptItem("agent-1"), 1)
	legacy.ConversionVersion = nil
	frame, err := proto.Marshal(legacy)
	if err != nil {
		t.Fatalf("marshal: %v", err)
	}
	if _, err := d.sql.Exec(`UPDATE entry SET frame = ? WHERE upsert_key = ?`, frame, upsertKey); err != nil {
		t.Fatalf("rewriting the frame as a legacy one: %v", err)
	}
}

func kindOf(t *testing.T, d *DB, upsertKey string) string {
	t.Helper()
	return scalar[string](t, d, `SELECT kind FROM entry WHERE upsert_key = ?`, upsertKey)
}

// ---- what is retired ----

func TestARetirementRetiresAPromptAPreVersioningSidecarWrote(t *testing.T) {
	// Arrange: the live store's case — a task notification the old conversion
	// minted as a prompt, carrying no conversion version.
	d, _ := newStore(t)
	legacyPrompt(t, d, "prompt:u-notification")

	// Act
	result := healBatch(t, d, 200, 2, nil, retirement("prompt:u-notification", 2))

	// Assert
	if result.Retired != 1 || kindOf(t, d, "prompt:u-notification") != kindRetired {
		t.Fatalf("retired=%d kind=%q, want 1 and %q", result.Retired, kindOf(t, d, "prompt:u-notification"), kindRetired)
	}
}

func TestARetirementRetiresARowAnOlderVersionProduced(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	healBatch(t, d, 100, 1, []*storev1.StoreEntry{filePageEntry("w1", "prompt:u1", "agent-1", promptItem("agent-1"), 1)})

	// Act
	result := healBatch(t, d, 200, 2, nil, retirement("prompt:u1", 2))

	// Assert
	if result.Retired != 1 {
		t.Fatalf("retired = %d, want 1", result.Retired)
	}
}

func TestARetirementRetiresAPeerMessage(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	healBatch(t, d, 100, 1, []*storev1.StoreEntry{filePageEntry("w1", "peer:u1", "agent-1", peerItem("agent-1"), 1)})

	// Act
	result := healBatch(t, d, 200, 2, nil, retirement("peer:u1", 2))

	// Assert
	if result.Retired != 1 {
		t.Fatalf("retired = %d, want 1", result.Retired)
	}
}

func TestARetirementPublishesTheRetiredLineToItsBook(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	healBatch(t, d, 100, 1, []*storev1.StoreEntry{filePageEntry("w1", "prompt:u1", "agent-1", promptItem("agent-1"), 1)})

	// Act
	result := healBatch(t, d, 200, 2, nil, retirement("prompt:u1", 2))

	// Assert
	if len(result.Lines) != 1 || !result.Lines[0].Retired || result.Lines[0].AgentID != "agent-1" ||
		result.Lines[0].Line.GetLine().GetAgentItem().GetAgentPrompt() == nil {
		t.Fatalf("lines = %+v, want one retirement of the prompt in agent-1's book", result.Lines)
	}
}

// ---- what is left ----

func TestARetirementLeavesARowTheSameVersionProduced(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	healBatch(t, d, 100, 2, []*storev1.StoreEntry{filePageEntry("w1", "prompt:u1", "agent-1", promptItem("agent-1"), 2)})

	// Act
	result := healBatch(t, d, 200, 2, nil, retirement("prompt:u1", 2))

	// Assert
	if result.Retired != 0 || kindOf(t, d, "prompt:u1") != kindPageLine {
		t.Fatalf("retired=%d kind=%q, want the row left as a page line", result.Retired, kindOf(t, d, "prompt:u1"))
	}
}

func TestARetirementLeavesARowTheStreamPlaneWroteLast(t *testing.T) {
	// Arrange: the shim's own prompt under the same key is its to keep.
	d, _ := newStore(t)
	writeOK(t, d, pageEntry("w1", "prompt:u1", "agent-1", promptItem("agent-1")))

	// Act
	result := healBatch(t, d, 200, 2, nil, retirement("prompt:u1", 2))

	// Assert
	if result.Retired != 0 || kindOf(t, d, "prompt:u1") != kindPageLine {
		t.Fatalf("retired=%d kind=%q, want the stream row left", result.Retired, kindOf(t, d, "prompt:u1"))
	}
}

func TestARetirementNamingNoRowIsNotARefusal(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	result := healBatch(t, d, 200, 2, nil, retirement("prompt:never-written", 2))

	// Assert
	if result.Retired != 0 {
		t.Fatalf("retired = %d, want 0", result.Retired)
	}
}

func TestARetirementOfAnAlreadyRetiredRowChangesNothing(t *testing.T) {
	// Arrange: a heal that runs twice (a restart mid-way, a second boot).
	d, _ := newStore(t)
	legacyPrompt(t, d, "prompt:u1")
	healBatch(t, d, 200, 2, nil, retirement("prompt:u1", 2))
	seqBefore := scalar[int64](t, d, `SELECT write_seq FROM entry WHERE upsert_key = 'prompt:u1'`)

	// Act
	result := healBatch(t, d, 300, 2, nil, retirement("prompt:u1", 2))

	// Assert
	seqAfter := scalar[int64](t, d, `SELECT write_seq FROM entry WHERE upsert_key = 'prompt:u1'`)
	if result.Retired != 0 || len(result.Lines) != 0 || seqAfter != seqBefore {
		t.Fatalf("retired=%d lines=%d write_seq %d -> %d, want nothing to change", result.Retired, len(result.Lines), seqBefore, seqAfter)
	}
}

func TestARetirementLeavesAnActivityRowAndSaysSoAtError(t *testing.T) {
	// Arrange: an activity drove the lifecycle tables; the store cannot take
	// the line away alone without its tables disagreeing with its books.
	d, s := newStore(t)
	healBatch(t, d, 100, 1, []*storev1.StoreEntry{filePageEntry("w1", "activity:a1", "agent-1", frameItem(activityFrame("agent-1", "a1", prose())), 1)})

	// Act
	result := healBatch(t, d, 200, 2, nil, retirement("activity:a1", 2))

	// Assert
	if result.Retired != 0 || kindOf(t, d, "activity:a1") != kindPageLine {
		t.Fatalf("retired=%d kind=%q, want the activity row left", result.Retired, kindOf(t, d, "activity:a1"))
	}
	s.assertLogged(t, "error", "drove the store's lifecycle tables")
}

// ---- what readers see ----

func TestNoPageServesARetiredLine(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	legacyPrompt(t, d, "prompt:u1")
	healBatch(t, d, 200, 2, nil, retirement("prompt:u1", 2))

	// Act
	opened, err := d.OpenPage(ctx(), "agent-1", 10, nil)

	// Assert
	if err != nil {
		t.Fatalf("OpenPage: %v", err)
	}
	if n := len(opened.Page.GetLines()); n != 0 {
		t.Fatalf("page lines = %d, want 0", n)
	}
}

func TestAReplayServesARetirementAsARetirement(t *testing.T) {
	// Arrange: a watch pinned before the retirement must still be told.
	d, _ := newStore(t)
	legacyPrompt(t, d, "prompt:u1")
	opened, err := d.OpenPage(ctx(), "agent-1", 10, nil)
	if err != nil {
		t.Fatalf("OpenPage: %v", err)
	}
	healBatch(t, d, 200, 2, nil, retirement("prompt:u1", 2))

	// Act
	lines, err := d.LinesSince(ctx(), "agent-1", opened.PinSeq)

	// Assert
	if err != nil {
		t.Fatalf("LinesSince: %v", err)
	}
	if len(lines) != 1 || !lines[0].Retired {
		t.Fatalf("replay = %+v, want exactly the retirement", lines)
	}
}

func TestARetiredLinesPointerStaysAValidKnownThrough(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	legacyPrompt(t, d, "prompt:u1")
	opened, err := d.OpenPage(ctx(), "agent-1", 10, nil)
	if err != nil {
		t.Fatalf("OpenPage: %v", err)
	}
	pointer := opened.Page.GetLines()[0].GetAt()
	healBatch(t, d, 200, 2, nil, retirement("prompt:u1", 2))

	// Act
	_, err = d.OpenPage(ctx(), "agent-1", 10, pointer)

	// Assert
	if err != nil {
		t.Fatalf("re-open with the retired line's pointer: %v, want an ordinary page", err)
	}
}

func TestARealLineTakesARetiredRowBackAtItsPosition(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	legacyPrompt(t, d, "prompt:u1")
	positionBefore := scalar[int64](t, d, `SELECT position FROM entry WHERE upsert_key = 'prompt:u1'`)
	healBatch(t, d, 200, 2, nil, retirement("prompt:u1", 2))

	// Act: the shim writes a real prompt under the same key.
	writeOK(t, d, pageEntry("w-live", "prompt:u1", "agent-1", promptItem("agent-1")))

	// Assert
	positionAfter := scalar[int64](t, d, `SELECT position FROM entry WHERE upsert_key = 'prompt:u1'`)
	if kindOf(t, d, "prompt:u1") != kindPageLine || positionAfter != positionBefore {
		t.Fatalf("kind=%q position %d -> %d, want a page line at its old position", kindOf(t, d, "prompt:u1"), positionBefore, positionAfter)
	}
}

// ---- validation ----

func TestRetirementsWithoutACursorAdvanceAreRefused(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	_, err := d.WriteBatch(ctx(), "test-sidecar", WriteBulk, &storev1.EntryBatch{
		Entries:     []*storev1.StoreEntry{filePageEntry("w1", "u1", "agent-1", promptItem("agent-1"), 2)},
		Retirements: []*storev1.StoreRetirement{retirement("prompt:u1", 2)},
	}, nil)

	// Assert
	if got := RefusalSite(err); got != SiteRetirementInvalid {
		t.Fatalf("site = %q (error: %v), want %q", got, err, SiteRetirementInvalid)
	}
}

func TestARetirementNamingNoKeyIsRefused(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	_, err := d.WriteBatch(ctx(), "test-sidecar", WriteBulk, &storev1.EntryBatch{
		CursorAdvance: cursorAt(10, 2),
		Retirements:   []*storev1.StoreRetirement{retirement("", 2)},
	}, nil)

	// Assert
	if got := RefusalField(err); got != "retirements[0].upsert_key" {
		t.Fatalf("field = %q (error: %v), want retirements[0].upsert_key", got, err)
	}
}

func TestARetirementAtVersionZeroIsRefused(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	_, err := d.WriteBatch(ctx(), "test-sidecar", WriteBulk, &storev1.EntryBatch{
		CursorAdvance: cursorAt(10, 2),
		Retirements:   []*storev1.StoreRetirement{retirement("prompt:u1", 0)},
	}, nil)

	// Assert
	if got := RefusalField(err); got != "retirements[0].conversion_version" {
		t.Fatalf("field = %q (error: %v), want retirements[0].conversion_version", got, err)
	}
}

// ---- the retirable kinds ----

func TestRetirableAnswersPerItem(t *testing.T) {
	apiError := &conversationv1.AgentFrame{
		AgentId: &conversationv1.AgentId{Value: "agent-1"},
		Result: &conversationv1.AgentFrame_Update{Update: &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_ApiError{ApiError: &conversationv1.ApiRequestFailed{}},
		}},
	}
	tests := []struct {
		name string
		item *storev1.StoreAgentItem
		want bool
	}{
		{name: "a prompt", item: promptItem("agent-1"), want: true},
		{name: "a peer message", item: peerItem("agent-1"), want: true},
		{name: "a non-activity update", item: frameItem(apiError), want: true},
		{name: "an activity", item: frameItem(activityFrame("agent-1", "a1", prose())), want: false},
		{name: "a terminal", item: frameItem(successFrame("agent-1")), want: false},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			// Arrange
			line := &storev1.StorePageLine{PageAgentId: &conversationv1.AgentId{Value: "agent-1"}, AgentItem: test.item}

			// Act
			_, got := retirable(line)

			// Assert
			if got != test.want {
				t.Fatalf("retirable = %t, want %t", got, test.want)
			}
		})
	}
}

func TestARetiredLineIsPublishedAtItsPlace(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	healBatch(t, d, 100, 1, []*storev1.StoreEntry{placed(filePageEntry("w1", "prompt:u1", "agent-1", promptItem("agent-1"), 1), 500, 2)})

	// Act
	result := healBatch(t, d, 200, 2, nil, retirement("prompt:u1", 2))

	// Assert
	if got := result.Lines[0].Line.GetRecordedPlace(); got.GetAtMs() != 500 || got.GetOrdinal() != 2 {
		t.Fatalf("retired line's place = %v, want 500.2", got)
	}
}
