package wsm

import (
	"context"
	"testing"
	"time"
)

var attentionAt = time.Date(2026, 11, 21, 12, 0, 0, 0, time.UTC)

func TestStampUserAttentionRecordsAndListsEveryKind(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ctx := context.Background()
	want := []UserAttention{
		{Kind: AttentionOpenedChildren, Target: "ws-a", At: attentionAt},
		{Kind: AttentionOpenedRepository, Target: "repo-a", At: attentionAt.Add(time.Minute)},
		{Kind: AttentionOpenedSubsection, Target: "sub-a", At: attentionAt.Add(2 * time.Minute)},
		{Kind: AttentionPrompt, Target: "ws-a", At: attentionAt.Add(3 * time.Minute)},
	}

	// Act
	for _, stamp := range want {
		if err := s.StampUserAttention(ctx, stamp.Kind, stamp.Target, stamp.At); err != nil {
			t.Fatalf("StampUserAttention(%v): %v", stamp, err)
		}
	}
	got, err := s.ListUserAttention(ctx)

	// Assert
	if err != nil {
		t.Fatalf("ListUserAttention: %v", err)
	}
	if len(got) != len(want) {
		t.Fatalf("ListUserAttention = %v, want %v", got, want)
	}
	for i := range want {
		if got[i].Kind != want[i].Kind || got[i].Target != want[i].Target || !got[i].At.Equal(want[i].At) {
			t.Fatalf("stamp %d = %v, want %v", i, got[i], want[i])
		}
	}
}

func TestStampUserAttentionOnlyMovesForward(t *testing.T) {
	tests := []struct {
		name  string
		later time.Time
		want  time.Time
	}{
		{name: "a later instant replaces the stamp", later: attentionAt.Add(time.Hour), want: attentionAt.Add(time.Hour)},
		{name: "an earlier instant leaves the stamp standing", later: attentionAt.Add(-time.Hour), want: attentionAt},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ctx := context.Background()
			if err := s.StampUserAttention(ctx, AttentionPrompt, "ws", attentionAt); err != nil {
				t.Fatalf("first stamp: %v", err)
			}

			// Act
			err := s.StampUserAttention(ctx, AttentionPrompt, "ws", tt.later)

			// Assert
			if err != nil {
				t.Fatalf("second stamp: %v", err)
			}
			got, err := s.ListUserAttention(ctx)
			if err != nil || len(got) != 1 || !got[0].At.Equal(tt.want) {
				t.Fatalf("ListUserAttention = %v, %v, want one stamp at %v", got, err, tt.want)
			}
		})
	}
}

func TestStampUserAttentionRefusesAnUnknownKind(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.StampUserAttention(context.Background(), AttentionKind("nonsense"), "ws", attentionAt)

	// Assert
	if err == nil {
		t.Fatal("StampUserAttention accepted an unknown kind, want the CHECK constraint to refuse it")
	}
	if got, lerr := s.ListUserAttention(context.Background()); lerr != nil || len(got) != 0 {
		t.Fatalf("ListUserAttention = %v, %v, want nothing recorded", got, lerr)
	}
}
