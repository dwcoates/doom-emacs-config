package prwatch

import (
	"reflect"
	"testing"

	"agent-repld/internal/wsm"
)

func TestGroupedKeepsEachKeysFirstPlace(t *testing.T) {
	tests := []struct {
		name      string
		items     []string
		wantOrder []byte
		wantBy    map[byte][]int
	}{
		{name: "nothing", items: nil, wantOrder: nil, wantBy: map[byte][]int{}},
		{name: "interleaved keys", items: []string{"b1", "a22", "b333"}, wantOrder: []byte{'b', 'a'}, wantBy: map[byte][]int{'b': {2, 4}, 'a': {3}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			by, order := grouped(tt.items, func(s string) byte { return s[0] }, func(s string) int { return len(s) })

			// Assert
			if !reflect.DeepEqual(order, tt.wantOrder) || !reflect.DeepEqual(by, tt.wantBy) {
				t.Fatalf("grouped = %v, %v; want %v, %v", by, order, tt.wantBy, tt.wantOrder)
			}
		})
	}
}

func TestFailingOfReadsEachSubscriptionsStretch(t *testing.T) {
	// Arrange
	f := &wsm.PullRequestFailure{Kind: wsm.PullRequestFailureUnauthorized}
	subs := []wsm.GithubWorkflowRunSubscription{{Failing: f}, {}}

	// Act
	got := failingOf(subs, func(s wsm.GithubWorkflowRunSubscription) *wsm.PullRequestFailure { return s.Failing })

	// Assert
	if len(got) != 2 || got[0] != f || got[1] != nil {
		t.Fatalf("failingOf = %v, want the one stretch then nil", got)
	}
}
