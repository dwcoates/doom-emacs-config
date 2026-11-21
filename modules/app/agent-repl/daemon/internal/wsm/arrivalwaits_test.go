package wsm

import (
	"context"
	"database/sql"
	"errors"
	"testing"
	"time"


	"google.golang.org/protobuf/proto"

	"agent-repld/internal/dlog"
)

const (
	noticeOrigin    = "PROMPT_ORIGIN_MERGE_QUEUE_NOTICE"
	agentSentOrigin = "PROMPT_ORIGIN_AGENT_SENT"
	userSentOrigin  = "PROMPT_ORIGIN_USER_SENT"
)

// arrivalWait is a merge queue notice to WS waiting on the session, arrived at
// second AT.
func arrivalWait(ws WorkspaceID, at int64) ArrivalWait {
	return ArrivalWait{
		Workspace: ws, Turn: NewTurnID(), Said: said("a merge landed"), Origin: noticeOrigin,
		QueuedAt: time.Unix(at, 0).UTC(), Reason: WaitSession, Why: "the workspace has no shim",
	}
}

// sameWait reports whether two waits are the same row.
func sameWait(a, b ArrivalWait) bool {
	sameSender := (a.SentBy == nil) == (b.SentBy == nil) && (a.SentBy == nil || *a.SentBy == *b.SentBy)
	return a.Workspace == b.Workspace && a.Turn == b.Turn && proto.Equal(a.Said, b.Said) && a.Origin == b.Origin &&
		sameSender && a.QueuedAt.Equal(b.QueuedAt) && a.Reason == b.Reason && a.Why == b.Why
}

func TestAnArrivalWaitReadsBackWhole(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	ws := testWorkspace(t, s)
	sender := testWorkspaceNamed(t, s, "sender").ID
	want := arrivalWait(ws.ID, 100)
	want.Origin, want.SentBy = agentSentOrigin, &sender
	if err := s.PutArrivalWait(ctx, want); err != nil {
		t.Fatalf("PutArrivalWait: %v", err)
	}

	// Act
	got, err := s.ArrivalWaits(ctx, ws.ID)

	// Assert
	if err != nil || len(got) != 1 || !sameWait(got[0], want) {
		t.Fatalf("ArrivalWaits = %+v, %v; want exactly %+v", got, err, want)
	}
}

func TestAWaitRecordedAgainTakesItsNewReasonAndKeepsItsPlace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	ws := testWorkspace(t, s)
	first := arrivalWait(ws.ID, 100)
	later := arrivalWait(ws.ID, 200)
	for _, w := range []ArrivalWait{first, later} {
		if err := s.PutArrivalWait(ctx, w); err != nil {
			t.Fatalf("PutArrivalWait: %v", err)
		}
	}
	again := first
	again.Reason, again.Why, again.QueuedAt = WaitColdGate, "the session is parked at its cold gate", time.Unix(300, 0).UTC()

	// Act
	err := s.PutArrivalWait(ctx, again)

	// Assert
	got, readErr := s.ArrivalWaits(ctx, ws.ID)
	want := first
	want.Reason, want.Why = again.Reason, again.Why
	if err != nil || readErr != nil || len(got) != 2 || !sameWait(got[0], want) || got[1].Turn != later.Turn {
		t.Fatalf("after recording again: err %v, read %+v, %v; want %+v first, then %s", err, got, readErr, want, later.Turn)
	}
}

func TestDeletingAnArrivalWaitAnswersWhetherOneStood(t *testing.T) {
	tests := []struct {
		name   string
		stored bool
		want   bool
	}{
		{name: "a standing wait", stored: true, want: true},
		{name: "no wait", stored: false, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ctx := context.Background()
			w := arrivalWait(testWorkspace(t, s).ID, 100)
			if tt.stored {
				if err := s.PutArrivalWait(ctx, w); err != nil {
					t.Fatalf("PutArrivalWait: %v", err)
				}
			}

			// Act
			stood, err := s.DeleteArrivalWait(ctx, w.Turn)

			// Assert
			left, readErr := s.ArrivalWaits(ctx, w.Workspace)
			if err != nil || stood != tt.want || readErr != nil || len(left) != 0 {
				t.Fatalf("DeleteArrivalWait = %v, %v; left %+v, %v; want %v and nothing left", stood, err, left, readErr, tt.want)
			}
		})
	}
}

func TestAllArrivalWaitsReadsEveryWorkspaceInArrivalOrder(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	a, b := testWorkspaceNamed(t, s, "a").ID, testWorkspaceNamed(t, s, "b").ID
	later, earlier := arrivalWait(a, 200), arrivalWait(b, 100)
	for _, w := range []ArrivalWait{later, earlier} {
		if err := s.PutArrivalWait(ctx, w); err != nil {
			t.Fatalf("PutArrivalWait: %v", err)
		}
	}

	// Act
	got, err := s.AllArrivalWaits(ctx)

	// Assert
	if err != nil || len(got) != 2 || got[0].Turn != earlier.Turn || got[1].Turn != later.Turn {
		t.Fatalf("AllArrivalWaits = %+v, %v; want %s then %s", got, err, earlier.Turn, later.Turn)
	}
}

func TestForgettingAWorkspaceDropsItsArrivalWaits(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	ws := testWorkspace(t, s)
	if err := s.PutArrivalWait(ctx, arrivalWait(ws.ID, 100)); err != nil {
		t.Fatalf("PutArrivalWait: %v", err)
	}

	// Act
	_, err := s.Forget(ctx, ws.ID)

	// Assert
	if n := scalar[int](t, s, `SELECT count(*) FROM arrival_waits`); err != nil || n != 0 {
		t.Fatalf("Forget = %v, %d waits left; want none", err, n)
	}
}

func TestPutArrivalWaitRefusesAWaitTheRowMustNeverCarry(t *testing.T) {
	tests := []struct {
		name   string
		mutate func(*ArrivalWait)
		want   error
	}{
		{name: "no prompt", mutate: func(w *ArrivalWait) { w.Said = nil }},
		{name: "an undeclared reason", mutate: func(w *ArrivalWait) { w.Reason = arrivalWaitSentinel }},
		{name: "no reason", mutate: func(w *ArrivalWait) { w.Reason = arrivalWaitUnspecified }},
		{name: "no words for the reason", mutate: func(w *ArrivalWait) { w.Why = "" }},
		{name: "a held origin", mutate: func(w *ArrivalWait) { w.Origin = userSentOrigin }, want: ErrNotDeliveredOnArrival},
		{name: "an agent-sent prompt naming no sender", mutate: func(w *ArrivalWait) { w.Origin = agentSentOrigin }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, logs := testStore(t)
			ctx := context.Background()
			w := arrivalWait(testWorkspace(t, s).ID, 100)
			tt.mutate(&w)

			// Act
			err := s.PutArrivalWait(ctx, w)

			// Assert
			if err == nil || (tt.want != nil && !errors.Is(err, tt.want)) {
				t.Fatalf("PutArrivalWait = %v, want a refusal (%v)", err, tt.want)
			}
			if n := scalar[int](t, s, `SELECT count(*) FROM arrival_waits`); n != 0 {
				t.Fatalf("%d waits stored after a refusal, want none", n)
			}
			if _, found := dlog.FindRecord(logs.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.wsm.put_arrival_wait"}); !found {
				t.Fatalf("no ERROR record of the refusal; records %+v", logs.Records())
			}
		})
	}
}

func TestAnArrivalWaitWithAnUndeclaredReasonFailsTheRead(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	w := arrivalWait(testWorkspace(t, s).ID, 100)
	if err := s.PutArrivalWait(ctx, w); err != nil {
		t.Fatalf("PutArrivalWait: %v", err)
	}
	corrupt(t, s, `UPDATE arrival_waits SET reason = 99 WHERE turn_id = ?`, w.Turn)

	// Act
	_, err := s.ArrivalWaits(ctx, w.Workspace)

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Field != "reason" {
		t.Fatalf("ArrivalWaits = %v, want a decode error on the reason", err)
	}
}

func TestPutHeldPromptRefusesAPromptDeliveredOnArrival(t *testing.T) {
	tests := []struct {
		name   string
		origin string
	}{
		{name: "a daemon notice", origin: noticeOrigin},
		{name: "an agent-sent prompt", origin: agentSentOrigin},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, logs := testStore(t)
			ctx := context.Background()
			ws := testWorkspace(t, s)
			h := HeldPrompt{Workspace: ws.ID, Turn: NewTurnID(), Said: said("hello"), Origin: tt.origin, QueuedAt: instant}
			if tt.origin == agentSentOrigin {
				sender := testWorkspaceNamed(t, s, "sender").ID
				h.SentBy = &sender
			}

			// Act
			err := s.PutHeldPrompt(ctx, h)

			// Assert
			if !errors.Is(err, ErrHeldOnArrival) {
				t.Fatalf("PutHeldPrompt = %v, want %v", err, ErrHeldOnArrival)
			}
			if n := scalar[int](t, s, `SELECT count(*) FROM held_prompts`); n != 0 {
				t.Fatalf("%d holds stored, want none", n)
			}
			if _, found := dlog.FindRecord(logs.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.wsm.put_held_prompt"}); !found {
				t.Fatalf("no ERROR record of the refusal; records %+v", logs.Records())
			}
		})
	}
}

// storedHeldRow writes a standing held prompt of ORIGIN straight into the table, as
// an earlier build that held prompts delivered on arrival did.
func storedHeldRow(t *testing.T, s *store, ws WorkspaceID, origin string, tombstoned bool) TurnID {
	t.Helper()
	turn := NewTurnID()
	raw, err := proto.Marshal(said("words"))
	if err != nil {
		t.Fatalf("encode: %v", err)
	}
	var tomb any
	if tombstoned {
		tomb = "delivered"
	}
	if err := s.write(context.Background(), "test", nil, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx, `INSERT INTO held_prompts (turn_id, workspace_id, said, origin, accepted, queued_at, tombstone_kind, tombstone_at)
			VALUES (?, ?, ?, ?, 0, ?, ?, ?)`, turn, ws, raw, origin, nanos(instant), tomb, nanos(instant))
		return err
	}); err != nil {
		t.Fatalf("insert a held row: %v", err)
	}
	return turn
}

func TestTheRewriteMovesOnlyStandingHoldsOfPromptsDeliveredOnArrival(t *testing.T) {
	// Arrange
	s, logs := testStore(t)
	ws := testWorkspace(t, s).ID
	notice := storedHeldRow(t, s, ws, noticeOrigin, false)
	user := storedHeldRow(t, s, ws, userSentOrigin, false)
	delivered := storedHeldRow(t, s, ws, noticeOrigin, true)

	// Act
	err := s.write(context.Background(), "test", nil, func(ctx context.Context, tx *sql.Tx) error {
		return moveArrivalHolds(ctx, tx, s.log)
	})

	// Assert
	if err != nil {
		t.Fatalf("moveArrivalHolds: %v", err)
	}
	waits, err := s.ArrivalWaits(context.Background(), ws)
	if err != nil || len(waits) != 1 || waits[0].Turn != notice || waits[0].Reason != WaitSession || waits[0].Why != movedArrivalWhy ||
		firstText(waits[0].Said) != "words" || !waits[0].QueuedAt.Equal(instant) {
		t.Fatalf("waits = %+v, %v; want only %s, on the session, as it was held", waits, err, notice)
	}
	for _, tc := range []struct {
		turn TurnID
		want int
	}{{notice, 0}, {user, 1}, {delivered, 1}} {
		if n := scalar[int](t, s, `SELECT count(*) FROM held_prompts WHERE turn_id = ?`, tc.turn); n != tc.want {
			t.Fatalf("held rows of %s = %d, want %d", tc.turn, n, tc.want)
		}
	}
	if _, found := dlog.FindRecord(logs.Records(), dlog.RecordQuery{Level: "info", Message: "moved a standing hold of a prompt delivered on arrival to a wait on its session"}); !found {
		t.Fatalf("no record of the move; records %+v", logs.Records())
	}
}

func TestEveryArrivalWaitReasonIsNamed(t *testing.T) {
	for r := arrivalWaitUnspecified + 1; r < arrivalWaitSentinel; r++ {
		t.Run(r.String(), func(t *testing.T) {
			// Act, Assert
			if !r.Valid() {
				t.Fatalf("%d is declared but not valid", int(r))
			}
			if got := r.String(); got == "" || got == (ArrivalWaitReason(0)).String() {
				t.Fatalf("reason %d has no name: %q", int(r), got)
			}
		})
	}
}

