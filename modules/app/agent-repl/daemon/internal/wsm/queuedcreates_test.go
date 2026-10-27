package wsm

import (
	"context"
	"slices"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

func heldCreate(key, id string, at int64, ops ...string) QueuedCreate {
	return QueuedCreate{Key: key, ID: id, Spec: `{"repo_dir":"/r"}`, OpIDs: ops, QueuedAt: time.Unix(at, 0).UTC()}
}

func TestQueuedCreates(t *testing.T) {
	tests := []struct {
		name    string
		puts    []QueuedCreate
		deletes []string
		want    []QueuedCreate
	}{
		{name: "a held create reads back whole",
			puts: []QueuedCreate{heldCreate("k1", "c1", 10, "op-1")},
			want: []QueuedCreate{heldCreate("k1", "c1", 10, "op-1")}},
		{name: "held creates read oldest first",
			puts: []QueuedCreate{heldCreate("k2", "c2", 20), heldCreate("k1", "c1", 10)},
			want: []QueuedCreate{heldCreate("k1", "c1", 10, []string{}...), heldCreate("k2", "c2", 20, []string{}...)}},
		{name: "a second put of one key replaces its ops and keeps one row",
			puts: []QueuedCreate{heldCreate("k1", "c1", 10, "op-1"), heldCreate("k1", "c1", 10, "op-1", "op-2")},
			want: []QueuedCreate{heldCreate("k1", "c1", 10, "op-1", "op-2")}},
		{name: "a delete releases only its own key",
			puts:    []QueuedCreate{heldCreate("k1", "c1", 10), heldCreate("k2", "c2", 20)},
			deletes: []string{"k1"},
			want:    []QueuedCreate{heldCreate("k2", "c2", 20, []string{}...)}},
		{name: "a delete of a key never held is no error",
			deletes: []string{"nothing"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ctx := context.Background()
			for _, q := range tc.puts {
				if err := s.PutQueuedCreate(ctx, q); err != nil {
					t.Fatalf("PutQueuedCreate(%+v): %v", q, err)
				}
			}
			// Act
			for _, key := range tc.deletes {
				if err := s.DeleteQueuedCreate(ctx, key); err != nil {
					t.Fatalf("DeleteQueuedCreate(%q): %v", key, err)
				}
			}
			got, err := s.QueuedCreates(ctx)
			// Assert
			if err != nil {
				t.Fatalf("QueuedCreates: %v", err)
			}
			for i := range tc.want {
				if tc.want[i].OpIDs == nil {
					tc.want[i].OpIDs = []string{}
				}
			}
			if !slices.EqualFunc(got, tc.want, func(a, b QueuedCreate) bool {
				return a.Key == b.Key && a.ID == b.ID && a.Spec == b.Spec && a.QueuedAt.Equal(b.QueuedAt) && slices.Equal(a.OpIDs, b.OpIDs)
			}) {
				t.Fatalf("QueuedCreates = %+v, want %+v", got, tc.want)
			}
		})
	}
}

func TestPutQueuedCreateRefusesAnUnnamedCreate(t *testing.T) {
	tests := []struct {
		name string
		q    QueuedCreate
	}{
		{name: "no key", q: QueuedCreate{ID: "c1", Spec: "{}"}},
		{name: "no id", q: QueuedCreate{Key: "k1", Spec: "{}"}},
		{name: "no request", q: QueuedCreate{Key: "k1", ID: "c1"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			// Act
			err := s.PutQueuedCreate(context.Background(), tc.q)
			// Assert: refused, recorded, and nothing held.
			if err == nil {
				t.Fatal("PutQueuedCreate accepted an unnamed create")
			}
			if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.wsm.put_queued_create"}); !ok {
				t.Fatalf("no ERROR record for the refusal: %v", log.Records())
			}
			if held, _ := s.QueuedCreates(context.Background()); len(held) != 0 {
				t.Fatalf("a refused create was held: %+v", held)
			}
		})
	}
}
