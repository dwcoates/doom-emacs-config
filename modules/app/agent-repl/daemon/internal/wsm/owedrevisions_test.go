package wsm

import (
	"context"
	"reflect"
	"testing"

	"agent-repld/internal/dlog"
)

// revisionOf is one owed revision of UNIT, revised to REVISED at AT ns.
func revisionOf(unit, original, revised string, at int64) OwedRevision {
	return OwedRevision{Unit: unit, Original: original, Revised: revised, OwedAtNs: at}
}

func TestTakingTheOwedRevisionsAnswersThemOnceOldestFirst(t *testing.T) {
	// Arrange
	s, _, ws := ownForkStore(t)
	ctx := context.Background()
	later := revisionOf("msg_2:0", "second", "second, revised", 20)
	earlier := revisionOf("msg_1:0", "first", "first, revised", 10)
	for _, owed := range []OwedRevision{later, earlier} {
		if err := s.OweRevision(ctx, ws.ID, owed); err != nil {
			t.Fatalf("OweRevision: %v", err)
		}
	}

	// Act
	first, firstErr := s.TakeOwedRevisions(ctx, ws.ID)
	second, secondErr := s.TakeOwedRevisions(ctx, ws.ID)

	// Assert
	if firstErr != nil || !reflect.DeepEqual(first, []OwedRevision{earlier, later}) {
		t.Fatalf("first take = (%+v, %v), want both oldest first", first, firstErr)
	}
	if secondErr != nil || len(second) != 0 {
		t.Fatalf("second take = (%+v, %v), want nothing left", second, secondErr)
	}
}

func TestARevisionOfARevisedResponseKeepsTheOriginalTheAgentWrote(t *testing.T) {
	// Arrange
	s, _, ws := ownForkStore(t)
	ctx := context.Background()
	if err := s.OweRevision(ctx, ws.ID, revisionOf("msg_1:0", "as written", "revised once", 10)); err != nil {
		t.Fatalf("OweRevision: %v", err)
	}

	// Act
	err := s.OweRevision(ctx, ws.ID, revisionOf("msg_1:0", "revised once", "revised twice", 20))

	// Assert
	got, _ := s.TakeOwedRevisions(ctx, ws.ID)
	if err != nil || !reflect.DeepEqual(got, []OwedRevision{revisionOf("msg_1:0", "as written", "revised twice", 20)}) {
		t.Fatalf("OweRevision = %v, owed = %+v; want the first original with the newest words", err, got)
	}
}

func TestRestoringTakenRevisionsKeepsANewerOne(t *testing.T) {
	// Arrange
	s, _, ws := ownForkStore(t)
	ctx := context.Background()
	if err := s.OweRevision(ctx, ws.ID, revisionOf("msg_1:0", "as written", "older", 10)); err != nil {
		t.Fatalf("OweRevision: %v", err)
	}
	taken, _ := s.TakeOwedRevisions(ctx, ws.ID)
	if err := s.OweRevision(ctx, ws.ID, revisionOf("msg_1:0", "as written", "newer", 20)); err != nil {
		t.Fatalf("OweRevision: %v", err)
	}

	// Act
	err := s.RestoreOwedRevisions(ctx, ws.ID, taken)

	// Assert
	got, _ := s.TakeOwedRevisions(ctx, ws.ID)
	if err != nil || !reflect.DeepEqual(got, []OwedRevision{revisionOf("msg_1:0", "as written", "newer", 20)}) {
		t.Fatalf("RestoreOwedRevisions = %v, owed = %+v; want the newer words kept", err, got)
	}
}

func TestABindForgetsTheRevisionsTheOldConversationOwed(t *testing.T) {
	// Arrange
	s, _, ws := ownForkStore(t)
	ctx := context.Background()
	if err := s.OweRevision(ctx, ws.ID, revisionOf("msg_1:0", "as written", "revised", 10)); err != nil {
		t.Fatalf("OweRevision: %v", err)
	}

	// Act
	err := s.BindSession(ctx, boundSession(t, s, ws), nil)

	// Assert
	got, _ := s.TakeOwedRevisions(ctx, ws.ID)
	if err != nil || len(got) != 0 {
		t.Fatalf("BindSession = %v, owed = %+v; want nothing owed of the old conversation", err, got)
	}
}

func TestAMalformedOwedRevisionIsRefusedAndRecorded(t *testing.T) {
	tests := []struct {
		name  string
		owed  OwedRevision
		write func(*store, OwedRevision) error
		op    string
	}{
		{name: "an owe with no revised words", owed: revisionOf("msg_1:0", "as written", "", 10),
			write: func(s *store, o OwedRevision) error { return s.OweRevision(context.Background(), "ws", o) }, op: "daemon.wsm.owe_revision"},
		{name: "a restore with no unit", owed: revisionOf("", "as written", "revised", 10),
			write: func(s *store, o OwedRevision) error {
				return s.RestoreOwedRevisions(context.Background(), "ws", []OwedRevision{o})
			}, op: "daemon.wsm.restore_owed_revisions"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, logs := testStore(t)

			// Act
			err := tt.write(s, tt.owed)

			// Assert
			if err == nil {
				t.Fatal("the malformed revision was written")
			}
			if _, ok := dlog.FindRecord(logs.Records(), dlog.RecordQuery{Level: "error", Operation: tt.op}); !ok {
				t.Fatalf("records = %+v, want the refusal at ERROR under %s", logs.Records(), tt.op)
			}
		})
	}
}
