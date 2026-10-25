package wsm

import (
	"context"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/encoding/protowire"
)

// retiredKindTab is a merge tab row as a build before the nested tabs stored
// it: its label, and a retired kind's field.
func retiredKindTab(t *testing.T, kind uint64) []byte {
	t.Helper()
	tab := withRetiredBytes(&frontendv1.FeedMergeTab{Label: &frontendv1.FeedMergeTabLabel{}}, protowire.Number(kind), nil)
	return marshalled(t, &frontendv1.FeedRow{Row: &frontendv1.FeedRow_MergeTab{MergeTab: tab}})
}

func TestIsRetiredMergeTabRow(t *testing.T) {
	tests := []struct {
		name string
		raw  func(t *testing.T) []byte
		want bool
	}{
		{name: "a conflicts tab", raw: func(t *testing.T) []byte { return retiredKindTab(t, 5) }, want: true},
		{name: "a fixes tab", raw: func(t *testing.T) []byte { return retiredKindTab(t, 7) }, want: true},
		{name: "a tab of today's kind", raw: func(t *testing.T) []byte {
			return marshalled(t, &frontendv1.FeedRow{Row: &frontendv1.FeedRow_MergeTab{MergeTab: &frontendv1.FeedMergeTab{
				Kind: &frontendv1.FeedMergeTab_Resolving{Resolving: &frontendv1.FeedMergeTabResolving{}},
			}}})
		}, want: false},
		{name: "a row that is no merge tab", raw: func(t *testing.T) []byte {
			return marshalled(t, &frontendv1.FeedRow{Row: &frontendv1.FeedRow_AgentPrompt{AgentPrompt: &frontendv1.FeedAgentPrompt{}}})
		}, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := isRetiredMergeTabRow(tt.raw(t))

			// Assert
			if err != nil || got != tt.want {
				t.Fatalf("isRetiredMergeTabRow = (%v, %v), want (%v, nil)", got, err, tt.want)
			}
		})
	}
}

func TestIsRetiredMergeTabRowRefusesARowThatDoesNotDecode(t *testing.T) {
	// Act
	_, err := isRetiredMergeTabRow([]byte{0xff, 0xff})

	// Assert
	if err == nil {
		t.Fatal("isRetiredMergeTabRow read a row that does not decode")
	}
}

func TestTheMigrationDropsStoredRetiredMergeTabsAndKeepsTheRest(t *testing.T) {
	// Arrange -- a layout-78 file holding a retired conflicts tab beside a prompt.
	path := fixtureAt(t, 78)
	execRaw(t, path, `INSERT INTO durable_feed_rows (workspace_id, row_id, plane, order_key, row) VALUES ('ws-1', 'tab-1', 0, 'k1', ?)`,
		retiredKindTab(t, 5))
	execRaw(t, path, `INSERT INTO durable_feed_rows (workspace_id, row_id, plane, order_key, row) VALUES ('ws-1', 'prompt-1', 0, 'k2', ?)`,
		marshalled(t, &frontendv1.FeedRow{Row: &frontendv1.FeedRow_AgentPrompt{AgentPrompt: &frontendv1.FeedAgentPrompt{}}}))

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-78 database: %v", err)
	}
	defer handle.Close()

	// Assert
	s := handle.(*store)
	if n := scalar[int](t, s, `SELECT COUNT(*) FROM durable_feed_rows WHERE row_id = 'tab-1'`); n != 0 {
		t.Fatalf("the retired tab survived the migration (%d rows)", n)
	}
	if n := scalar[int](t, s, `SELECT COUNT(*) FROM durable_feed_rows WHERE row_id = 'prompt-1'`); n != 1 {
		t.Fatalf("the prompt row = %d rows, want it kept", n)
	}
}
