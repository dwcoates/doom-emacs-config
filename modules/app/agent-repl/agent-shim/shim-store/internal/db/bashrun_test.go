package db

import (
	"errors"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	storev1 "agentrepl/proto/store/v1"
	"google.golang.org/protobuf/encoding/protowire"
	"google.golang.org/protobuf/proto"
)

func TestWriteBatchLandsABashFrameAsItsOwnEntryRow(t *testing.T) {
	// Arrange: the sidecar writes a run's spool in deltas, so the run's history
	// must live in the entry spine, not only as a lifecycle-table overwrite.
	d, _ := newStore(t)

	// Act
	writeOK(t, d, bashEntry("w1", "u1", "run-1", bashStart()))

	// Assert
	if got := scalar[string](t, d, `SELECT kind FROM entry WHERE upsert_key = 'u1'`); got != kindBash {
		t.Fatalf("kind = %q, want %q", got, kindBash)
	}
	if got := scalar[string](t, d, `SELECT run_id FROM entry WHERE upsert_key = 'u1'`); got != "run-1" {
		t.Fatalf("run_id = %q, want run-1", got)
	}
}

func TestWriteBatchLeavesABashRowWithNoBook(t *testing.T) {
	// Arrange: a detached run has no book, so no page query can reach its rows.
	d, _ := newStore(t)

	// Act
	writeOK(t, d, bashEntry("w1", "u1", "run-1", bashStart()))

	// Assert
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM entry WHERE upsert_key = 'u1' AND book_agent_id IS NULL`); got != 1 {
		t.Fatalf("bash rows with a NULL book = %d, want 1", got)
	}
}

func TestWriteBatchReturnsTheBashRowsItWrote(t *testing.T) {
	// Arrange: the fan-out publishes a run's rows exactly as it publishes a
	// book's lines, so the write must hand them back.
	d, _ := newStore(t)

	// Act
	result := writeOK(t, d, bashEntry("w1", "u1", "run-1", bashStart()))

	// Assert
	if len(result.BashRows) != 1 {
		t.Fatalf("bash rows = %d, want 1", len(result.BashRows))
	}
	if result.BashRows[0].RunID != "run-1" {
		t.Fatalf("bash row run = %q, want run-1", result.BashRows[0].RunID)
	}
}

func TestBashRunReplaysEveryStoredRowOfTheRun(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, bashEntry("w1", "bash:run-1:start", "run-1", bashStart()))
	writeOK(t, d, bashEntry("w2", "bash:run-1:terminal", "run-1", bashSuccess()))

	// Act
	replay, err := d.BashRun(ctx(), "run-1")

	// Assert
	if err != nil {
		t.Fatalf("BashRun = %v, want nil", err)
	}
	if len(replay.Rows) != 2 {
		t.Fatalf("rows = %d, want 2", len(replay.Rows))
	}
}

func TestBashRunReplaysInFirstInsertOrderRatherThanWriteOrder(t *testing.T) {
	// Arrange: a redelivered delta upserts its own row, and the run's spool
	// order is where that row has always been — not the end of the stream.
	d, _ := newStore(t)
	writeOK(t, d, bashEntry("w1", "bash:run-1:start", "run-1", bashStart()))
	writeOK(t, d, bashEntry("w2", "bash:run-1:0", "run-1", bashSuccess()))
	// The START row is written again, which bumps its write ordinal but not its
	// position.
	writeOK(t, d, bashEntry("w3", "bash:run-1:start", "run-1", bashStart()))

	// Act
	replay, err := d.BashRun(ctx(), "run-1")
	if err != nil {
		t.Fatalf("BashRun = %v, want nil", err)
	}

	// Assert
	if len(replay.Rows) != 2 {
		t.Fatalf("rows = %d, want 2", len(replay.Rows))
	}
	if replay.Rows[0].Row.GetFrame().GetStart() == nil {
		t.Fatalf("first replayed row = %v, want the start row to keep its place", replay.Rows[0].Row.GetFrame())
	}
}

func TestBashRunExcludesAnotherRunsRows(t *testing.T) {
	// Arrange
	d, _ := newStore(t)
	writeOK(t, d, bashEntry("w1", "u1", "run-1", bashStart()))
	writeOK(t, d, bashEntry("w2", "u2", "run-2", bashStart()))

	// Act
	replay, err := d.BashRun(ctx(), "run-1")
	if err != nil {
		t.Fatalf("BashRun = %v, want nil", err)
	}

	// Assert
	if len(replay.Rows) != 1 {
		t.Fatalf("rows = %d, want only run-1's own", len(replay.Rows))
	}
}

func TestBashRunAnswersNoRowsForARunItNeverStored(t *testing.T) {
	// Arrange: an unstored run is not an empty stream — it is the REFUSED OPEN
	// the server turns into CodeNotFound, and the absence of a row IS the
	// signal, so no sentinel is needed.
	d, _ := newStore(t)

	// Act
	replay, err := d.BashRun(ctx(), "never-written")

	// Assert
	if err != nil {
		t.Fatalf("BashRun = %v, want nil", err)
	}
	if len(replay.Rows) != 0 {
		t.Fatalf("rows = %d, want 0", len(replay.Rows))
	}
}

func TestBashRunTakesItsPinFromTheGlobalWriteOrdinal(t *testing.T) {
	// Arrange: the pin is what the live tail begins after, so it must be the
	// ordinal the replay was read at.
	d, _ := newStore(t)
	writeOK(t, d, bashEntry("w1", "u1", "run-1", bashStart()))
	want := scalar[uint64](t, d, `SELECT MAX(write_seq) FROM entry`)

	// Act
	replay, err := d.BashRun(ctx(), "run-1")
	if err != nil {
		t.Fatalf("BashRun = %v, want nil", err)
	}

	// Assert
	if replay.PinSeq != want {
		t.Fatalf("PinSeq = %d, want %d", replay.PinSeq, want)
	}
}

func TestBashRunRefusesAnEmptyRunIdentity(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	_, err := d.BashRun(ctx(), "")

	// Assert
	if !errors.Is(err, ErrInvalid) {
		t.Fatalf("BashRun(\"\") = %v, want ErrInvalid", err)
	}
}

func TestBashRowIsTerminalRecognizesTheConcludingArms(t *testing.T) {
	// Arrange: the arm IS the answer, which is what gives WatchBashRun its
	// natural end.
	tests := []struct {
		name string
		row  string
		want bool
	}{
		{name: "start is not terminal", row: "start", want: false},
		{name: "success is terminal", row: "success", want: true},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			frame := bashStart()
			if tc.row == "success" {
				frame = bashSuccess()
			}
			row := bashEntry("w", "u", "run-1", frame).GetAgentUpdate().GetBash()

			// Act
			got := BashRowIsTerminal(row)

			// Assert
			if got != tc.want {
				t.Fatalf("BashRowIsTerminal = %t, want %t", got, tc.want)
			}
		})
	}
}

// ---- the rendered tail ----

func bashTail(text string) *conversationv1.AgentBash {
	return &conversationv1.AgentBash{Result: &conversationv1.AgentBash_Tail{Tail: &conversationv1.AgentBashTail{Text: text}}}
}

// outmodedDelta rewrites a stored bash row's frame into the RETIRED
// contiguous-delta shape: an AgentBash whose only content is field 2 (the old
// `update` arm), which this build decodes to no arm at all. Every write path
// refuses such a frame, so the only way one exists is having been written
// before the arm was retired — which is exactly what this reproduces.
func outmodedDelta(t *testing.T, d *DB, upsertKey string) {
	t.Helper()
	stored := scalar[[]byte](t, d, `SELECT frame FROM entry WHERE upsert_key = ?`, upsertKey)
	entry := &storev1.StoreEntry{}
	if err := proto.Unmarshal(stored, entry); err != nil {
		t.Fatalf("decode the stored frame: %v", err)
	}
	legacy := &conversationv1.AgentBash{}
	var update []byte
	update = protowire.AppendTag(update, 1, protowire.BytesType)
	update = protowire.AppendString(update, "chunk-0")
	var field []byte
	field = protowire.AppendTag(field, 2, protowire.BytesType)
	field = protowire.AppendBytes(field, update)
	legacy.ProtoReflect().SetUnknown(field)
	entry.GetAgentUpdate().GetBash().Frame = legacy
	blob, err := proto.Marshal(entry)
	if err != nil {
		t.Fatalf("encode the outmoded frame: %v", err)
	}
	if _, err := d.sql.Exec(`UPDATE entry SET frame = ? WHERE upsert_key = ?`, blob, upsertKey); err != nil {
		t.Fatalf("store the outmoded frame: %v", err)
	}
}

func TestWriteBatchLandsATailAtTheCap(t *testing.T) {
	// Arrange: a tail of exactly the contract's cap is what the producer
	// writes for every run past it.
	d, _ := newStore(t)

	// Act
	_, err := d.WriteBatch(ctx(), "test-producer", WriteBulk,
		batch(bashEntry("w1", "bash:run-1:tail", "run-1", bashTail(strings.Repeat("y", bashTailCap)))), nil)

	// Assert
	if err != nil {
		t.Fatalf("WriteBatch = %v, want a tail at the cap stored", err)
	}
}

func TestWriteBatchRefusesATailPastTheCap(t *testing.T) {
	// Arrange: output beyond what is rendered is never stored.
	d, s := newStore(t)

	// Act
	_, err := d.WriteBatch(ctx(), "test-producer", WriteBulk,
		batch(bashEntry("w1", "bash:run-1:tail", "run-1", bashTail(strings.Repeat("y", bashTailCap+1)))), nil)

	// Assert
	if !errors.Is(err, ErrInvalid) || RefusalSite(err) != SiteBashTailOverCap {
		t.Fatalf("WriteBatch = %v (site %q), want ErrInvalid at %q", err, RefusalSite(err), SiteBashTailOverCap)
	}
	s.assertTracedRefusal(t, "AGENT_BASH_TAIL_CAP_BYTES")
}

func TestTheStoresTailBoundIsTheContractsCap(t *testing.T) {
	// Arrange: the producer, the renderer and the store read ONE number.
	want := int(conversationv1.AgentBashTailCap_AGENT_BASH_TAIL_CAP_BYTES)

	// Act
	got := bashTailCap

	// Assert
	if got != want {
		t.Fatalf("bashTailCap = %d, want the contract's %d", got, want)
	}
}

func TestBashRunReplaysASupersededTailAtItsFirstInsertPosition(t *testing.T) {
	// Arrange: the tail is one row every write supersedes; the terminal was
	// first inserted after it, so the replay serves the NEWEST window where
	// the tail has always been.
	d, _ := newStore(t)
	writeOK(t, d, bashEntry("w1", "bash:run-1:start", "run-1", bashStart()))
	writeOK(t, d, bashEntry("w2", "bash:run-1:tail", "run-1", bashTail("one\n")))
	writeOK(t, d, bashEntry("w3", "bash:run-1:terminal", "run-1", bashSuccess()))
	writeOK(t, d, bashEntry("w4", "bash:run-1:tail", "run-1", bashTail("one\ntwo\n")))

	// Act
	replay, err := d.BashRun(ctx(), "run-1")
	if err != nil {
		t.Fatalf("BashRun = %v, want nil", err)
	}

	// Assert
	if len(replay.Rows) != 3 {
		t.Fatalf("rows = %d, want start, one tail, terminal", len(replay.Rows))
	}
	if got := replay.Rows[1].Row.GetFrame().GetTail().GetText(); got != "one\ntwo\n" {
		t.Fatalf("second row's tail = %q, want the newest window in the tail's place", got)
	}
}

func TestBashRunSkipsAnOutmodedDeltaRow(t *testing.T) {
	// Arrange: a run stored before the delta arm was retired.
	d, _ := newStore(t)
	writeOK(t, d, bashEntry("w1", "bash:run-1:start", "run-1", bashStart()))
	writeOK(t, d, bashEntry("w2", "bash:run-1:0", "run-1", bashStart()))
	outmodedDelta(t, d, "bash:run-1:0")
	writeOK(t, d, bashEntry("w3", "bash:run-1:terminal", "run-1", bashSuccess()))

	// Act
	replay, err := d.BashRun(ctx(), "run-1")

	// Assert
	if err != nil {
		t.Fatalf("BashRun = %v, want the outmoded row skipped rather than a failure", err)
	}
	if len(replay.Rows) != 2 || replay.Rows[0].Row.GetFrame().GetStart() == nil || replay.Rows[1].Row.GetFrame().GetSuccess() == nil {
		t.Fatalf("rows = %d, want the start and the terminal only", len(replay.Rows))
	}
}

func TestBashRunLeavesAnOutmodedRowInPlace(t *testing.T) {
	// Arrange: old data is accepted as outmoded, never deleted.
	d, _ := newStore(t)
	writeOK(t, d, bashEntry("w1", "bash:run-1:0", "run-1", bashStart()))
	outmodedDelta(t, d, "bash:run-1:0")

	// Act
	if _, err := d.BashRun(ctx(), "run-1"); err != nil {
		t.Fatalf("BashRun = %v, want nil", err)
	}

	// Assert
	if got := scalar[int](t, d, `SELECT COUNT(*) FROM entry WHERE upsert_key = 'bash:run-1:0'`); got != 1 {
		t.Fatalf("outmoded rows stored = %d, want the row left in place", got)
	}
}

func TestBashRunStatesItsOutmodedRowsOnceAtInfo(t *testing.T) {
	// Arrange: a run holding many retired delta rows is one fact, not a flood.
	d, s := newStore(t)
	for _, key := range []string{"bash:run-1:0", "bash:run-1:7", "bash:run-1:12"} {
		writeOK(t, d, bashEntry("w-"+key, key, "run-1", bashStart()))
		outmodedDelta(t, d, key)
	}

	// Act
	if _, err := d.BashRun(ctx(), "run-1"); err != nil {
		t.Fatalf("BashRun = %v, want nil", err)
	}

	// Assert
	var stated []string
	for _, message := range recordsAtLevel(t, s, "info") {
		if strings.Contains(message, "outmoded row") {
			stated = append(stated, message)
		}
	}
	if len(stated) != 1 || !strings.Contains(stated[0], "skipped 3 outmoded") {
		t.Fatalf("outmoded records at info = %q, want exactly one stating all 3", stated)
	}
	if got := recordsAtLevel(t, s, "error"); len(got) != 0 {
		t.Fatalf("error records = %q, want none for outmoded rows", got)
	}
}

func TestTheBashRunReadBuildsNoAutomaticIndex(t *testing.T) {
	// Arrange
	d, _ := newStore(t)

	// Act
	plan := queryPlan(t, d, bashRunRowsSQL, "run-1", kindBash)

	// Assert
	assertNoAutomaticIndex(t, "the bash-run read", plan)
}

// fileBashEntry is a bash row the sidecar wrote from the run's spool.
func fileBashEntry(writeID, upsertKey, run string, frame *conversationv1.AgentBash) *storev1.StoreEntry {
	entry := bashEntry(writeID, upsertKey, run, frame)
	entry.Plane = &storev1.Plane{Plane: &storev1.Plane_File{File: &storev1.PlaneFile{}}}
	entry.ConversionVersion = fileVersion()
	return entry
}

func TestARunWrittenFromBothPlanesReplaysInFirstInsertOrder(t *testing.T) {
	// Arrange: the shim's start and terminal interleave with the sidecar's
	// tail, which is superseded after the terminal.
	d, _ := newStore(t)
	writeOK(t, d, bashEntry("w-start", "bash:run-1:start", "run-1", bashStart()))
	writeOK(t, d, fileBashEntry("w-t0", "bash:run-1:tail", "run-1", bashTail("chunk-0")))
	writeOK(t, d, bashEntry("w-term", "bash:run-1:terminal", "run-1", bashSuccess()))
	writeOK(t, d, fileBashEntry("w-t1", "bash:run-1:tail", "run-1", bashTail("chunk-0chunk-1")))

	// Act
	replay, err := d.BashRun(ctx(), "run-1")

	// Assert: start, the tail where it was first inserted (now its latest
	// text), then the terminal.
	if err != nil {
		t.Fatalf("BashRun = %v, want nil", err)
	}
	if len(replay.Rows) != 3 {
		t.Fatalf("rows = %d, want 3", len(replay.Rows))
	}
	frames := []*conversationv1.AgentBash{replay.Rows[0].Row.GetFrame(), replay.Rows[1].Row.GetFrame(), replay.Rows[2].Row.GetFrame()}
	if frames[0].GetStart() == nil || frames[1].GetTail().GetText() != "chunk-0chunk-1" || frames[2].GetSuccess() == nil {
		t.Fatalf("replay = %v, want start, the latest tail, then success", frames)
	}
}

func TestAReplayDrawsTheSameTailTheLastWriteHandedTheLiveWatcher(t *testing.T) {
	// Arrange: the live watcher sees exactly what each write returns.
	d, _ := newStore(t)
	writeOK(t, d, bashEntry("w-start", "bash:run-1:start", "run-1", bashStart()))
	writeOK(t, d, fileBashEntry("w-t0", "bash:run-1:tail", "run-1", bashTail("chunk-0")))
	live := writeOK(t, d, fileBashEntry("w-t1", "bash:run-1:tail", "run-1", bashTail("chunk-0chunk-1")))

	// Act
	replay, err := d.BashRun(ctx(), "run-1")

	// Assert
	if err != nil {
		t.Fatalf("BashRun = %v, want nil", err)
	}
	if len(live.BashRows) != 1 || len(replay.Rows) != 2 {
		t.Fatalf("live rows = %d, replay rows = %d; want 1 and 2", len(live.BashRows), len(replay.Rows))
	}
	if !proto.Equal(live.BashRows[0].Row, replay.Rows[1].Row) {
		t.Fatalf("replayed tail = %v, want the row the live watcher last saw %v", replay.Rows[1].Row, live.BashRows[0].Row)
	}
}
