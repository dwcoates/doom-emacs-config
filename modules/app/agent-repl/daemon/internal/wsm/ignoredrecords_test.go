package wsm

import (
	"context"
	"testing"

	"agent-repld/internal/dlog"
)

func TestIgnoredRecordKindMatchesItsOperationAndText(t *testing.T) {
	cases := []struct {
		name      string
		kind      IgnoredRecordKind
		operation string
		message   string
		want      bool
	}{
		{name: "a kind with no text matches every record of its operation", kind: IgnoredRecordKind{Operation: "op"}, operation: "op", message: "anything", want: true},
		{name: "a kind with text matches a message holding it", kind: IgnoredRecordKind{Operation: "op", MessageContains: "gone"}, operation: "op", message: "it is gone now", want: true},
		{name: "a kind with text passes a message without it", kind: IgnoredRecordKind{Operation: "op", MessageContains: "gone"}, operation: "op", message: "it broke"},
		{name: "another operation never matches", kind: IgnoredRecordKind{Operation: "op"}, operation: "op.other", message: "anything"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := tc.kind.Matches(tc.operation, tc.message)

			// Assert
			if got != tc.want {
				t.Fatalf("Matches(%q, %q) = %v, want %v", tc.operation, tc.message, got, tc.want)
			}
		})
	}
}

func TestTheIgnoredRecordsStartWithTheKindsTheMetapromptTableHeld(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	kinds, err := s.IgnoredRecordKinds(context.Background())

	// Assert
	if err != nil || len(kinds) != 2 || kinds[1].Operation != "daemon.ghclient.post" || kinds[1].MessageContains != "Parent comment not found" {
		t.Fatalf("IgnoredRecordKinds = (%+v, %v), want the spell checker's and GitHub's kinds", kinds, err)
	}
}

func TestAnIgnoredRecordKindIsRecordedRestatedAndWithdrawn(t *testing.T) {
	kind := IgnoredRecordKind{Operation: "daemon.shim", MessageContains: "tail lag", Why: "passes on its own", AddedAt: instant}
	steps := []struct {
		name string
		act  func(t *testing.T, s *store)
		// wantWhy is the kind's reason afterwards, empty when it is gone.
		wantWhy string
	}{
		{name: "a recorded kind is kept", act: func(t *testing.T, s *store) {}, wantWhy: "passes on its own"},
		{name: "recording it again takes the new reason and keeps one row", act: func(t *testing.T, s *store) {
			again := kind
			again.Why = "a network blip"
			if err := s.IgnoreRecordKind(context.Background(), again); err != nil {
				t.Fatal(err)
			}
		}, wantWhy: "a network blip"},
		{name: "withdrawing it removes it", act: func(t *testing.T, s *store) {
			if removed, err := s.UnignoreRecordKind(context.Background(), kind.Operation, kind.MessageContains); err != nil || !removed {
				t.Fatalf("UnignoreRecordKind = (%v, %v), want it removed", removed, err)
			}
		}},
		{name: "withdrawing one never recorded removes nothing", act: func(t *testing.T, s *store) {
			if removed, err := s.UnignoreRecordKind(context.Background(), kind.Operation, "another text"); err != nil || removed {
				t.Fatalf("UnignoreRecordKind = (%v, %v), want nothing removed", removed, err)
			}
		}, wantWhy: "passes on its own"},
	}
	for _, tc := range steps {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			if err := s.IgnoreRecordKind(context.Background(), kind); err != nil {
				t.Fatal(err)
			}

			// Act
			tc.act(t, s)

			// Assert
			kinds, err := s.IgnoredRecordKinds(context.Background())
			if err != nil {
				t.Fatal(err)
			}
			var why string
			matches := 0
			for _, k := range kinds {
				if k.Operation == kind.Operation && k.MessageContains == kind.MessageContains {
					why = k.Why
					matches++
				}
			}
			if why != tc.wantWhy || matches > 1 {
				t.Fatalf("the kind's reason is %q in %d rows, want %q in at most one", why, matches, tc.wantWhy)
			}
		})
	}
}

func TestRecordingAnIgnoredRecordKindRefusesAnIncompleteOne(t *testing.T) {
	cases := []struct {
		name string
		kind IgnoredRecordKind
	}{
		{name: "no operation", kind: IgnoredRecordKind{Why: "w", AddedAt: instant}},
		{name: "no reason", kind: IgnoredRecordKind{Operation: "op", Why: " ", AddedAt: instant}},
		{name: "no time", kind: IgnoredRecordKind{Operation: "op", Why: "w"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			before, _ := s.IgnoredRecordKinds(context.Background())

			// Act
			err := s.IgnoreRecordKind(context.Background(), tc.kind)

			// Assert
			after, _ := s.IgnoredRecordKinds(context.Background())
			if err == nil || len(after) != len(before) {
				t.Fatalf("IgnoreRecordKind = %v with %d kinds after %d, want a refusal and nothing recorded", err, len(after), len(before))
			}
			if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.wsm.ignore_record_kind", Message: "refused an ignored record kind that names no operation, reason or time"}); !ok {
				t.Fatalf("records = %+v, want the refusal recorded", log.Records())
			}
		})
	}
}
