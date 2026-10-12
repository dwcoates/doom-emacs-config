package feed

import (
	"slices"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

func TestChangedFields(t *testing.T) {
	row := func(id, key, turn string) *frontendv1.FeedRow {
		out := &frontendv1.FeedRow{Id: &frontendv1.FeedId{Value: id}, Order: &frontendv1.FeedRowOrder{Key: key}}
		if turn != "" {
			out.Turn = &conversationv1.TurnId{Value: turn}
		}
		return out
	}
	tests := []struct {
		name          string
		before, after *frontendv1.FeedRow
		want          []string
	}{
		{name: "two equal rows", before: row("r", "k1", "t"), after: row("r", "k1", "t"), want: nil},
		{name: "a changed nested scalar names its path", before: row("r", "k1", "t"), after: row("r", "k2", "t"), want: []string{"order.key"}},
		{name: "a field set on one side only names the field", before: row("r", "k1", ""), after: row("r", "k1", "t"), want: []string{"turn"}},
		{name: "several changes are named in sorted order", before: row("r", "k1", "t1"), after: row("r", "k2", "t2"), want: []string{"order.key", "turn.value"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := changedFields(tt.before, tt.after)

			// Assert
			if !slices.Equal(got, tt.want) {
				t.Fatalf("changedFields = %v, want %v", got, tt.want)
			}
		})
	}
}
