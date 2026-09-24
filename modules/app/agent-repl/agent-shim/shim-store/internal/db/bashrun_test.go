package db

import (
	"errors"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
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
