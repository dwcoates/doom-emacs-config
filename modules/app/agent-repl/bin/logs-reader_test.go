package main

import (
	"fmt"
	"strings"
	"testing"
	"time"
)

func TestOrderedBefore(t *testing.T) {
	tests := []struct {
		name                           string
		instantA, seqA, instantB, seqB int64
		want                           bool
	}{
		{name: "an earlier instant comes first", instantA: 1, seqA: 9, instantB: 2, seqB: 0, want: true},
		{name: "a later instant comes after", instantA: 2, seqA: 0, instantB: 1, seqB: 9, want: false},
		{name: "one instant orders by the read's order", instantA: 5, seqA: 1, instantB: 5, seqB: 2, want: true},
		{name: "one instant read later comes after", instantA: 5, seqA: 2, instantB: 5, seqB: 1, want: false},
		{name: "a record is not before itself", instantA: 5, seqA: 1, instantB: 5, seqB: 1, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := orderedBefore(tt.instantA, tt.seqA, tt.instantB, tt.seqB)

			// Assert
			if got != tt.want {
				t.Fatalf("orderedBefore(%d, %d, %d, %d) = %v, want %v", tt.instantA, tt.seqA, tt.instantB, tt.seqB, got, tt.want)
			}
		})
	}
}

// TestSpillReplayAndSortRecordsShareOneOrder pins that an ordered read and a
// follow batch put the same records in the same order: both go through
// orderedBefore, so a site sorting its own way fails here.
func TestSpillReplayAndSortRecordsShareOneOrder(t *testing.T) {
	// Arrange
	base := time.Date(2026, 9, 10, 10, 0, 0, 0, time.UTC)
	var records []record
	for index, offset := range []time.Duration{3 * time.Second, time.Second, 2 * time.Second, time.Second, 0} {
		instant := base.Add(offset)
		stamp := instant.Format(time.RFC3339Nano)
		raw := fmt.Sprintf(`{"timestamp":%q,"runtime":"daemon","level":"info","verbosity":"normal","operation":"op","message":"m%d","context":{}}`, stamp, index)
		rec, err := parseRecord([]byte(raw), int64(index))
		if err != nil {
			t.Fatalf("parseRecord: %v", err)
		}
		records = append(records, rec)
	}
	order, err := newSpillOrder(len(records))
	if err != nil {
		t.Fatalf("newSpillOrder: %v", err)
	}
	defer order.close()
	for _, rec := range records {
		if err := order.add(rec); err != nil {
			t.Fatalf("add: %v", err)
		}
	}

	// Act
	var replayed []string
	if err := order.replay(func(rec record) error {
		replayed = append(replayed, rec.Message)
		return nil
	}); err != nil {
		t.Fatalf("replay: %v", err)
	}
	sorted := append([]record(nil), records...)
	sortRecords(sorted)

	// Assert
	var want []string
	for _, rec := range sorted {
		want = append(want, rec.Message)
	}
	if strings.Join(replayed, ",") != strings.Join(want, ",") || strings.Join(want, ",") != "m4,m1,m3,m2,m0" {
		t.Fatalf("replay order %v, sortRecords order %v, want both m4,m1,m3,m2,m0", replayed, want)
	}
}

func TestHarvestAttribution(t *testing.T) {
	tests := []struct {
		name             string
		workspaceID, dir string
		wantID, wantDir  string
		wantComplete     bool
	}{
		{name: "no workspace is central", wantID: "central", wantDir: "-", wantComplete: true},
		{name: "a workspace's ID and directory are kept", workspaceID: "ws", dir: "/d", wantID: "ws", wantDir: "/d", wantComplete: true},
		{name: "an ID with no directory is incomplete", workspaceID: "ws", wantID: "ws", wantComplete: false},
		{name: "a directory with no ID is incomplete", dir: "/d", wantDir: "/d", wantComplete: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			id, dir, complete := harvestAttribution(tt.workspaceID, tt.dir)

			// Assert
			if id != tt.wantID || dir != tt.wantDir || complete != tt.wantComplete {
				t.Fatalf("harvestAttribution(%q, %q) = %q, %q, %v, want %q, %q, %v",
					tt.workspaceID, tt.dir, id, dir, complete, tt.wantID, tt.wantDir, tt.wantComplete)
			}
		})
	}
}

// TestHarvestRefusesIncompleteAttributionOfRecordsAndFindings pins that a
// harvest's records and its findings are attributed by the one rule.
func TestHarvestRefusesIncompleteAttributionOfRecordsAndFindings(t *testing.T) {
	tests := []struct {
		name string
		act  func(r *harvestRenderer) error
		want string
	}{
		{
			name: "a record",
			act:  func(r *harvestRenderer) error { return r.add(record{Operation: "op", WorkspaceID: "ws"}) },
			want: "harvest record op at  has incomplete workspace attribution",
		},
		{
			name: "a finding",
			act: func(r *harvestRenderer) error {
				return r.finish([]sinkFinding{{sink: sink{workspaceDir: "/d"}, message: "sink absent"}})
			},
			want: "harvest finding has incomplete workspace attribution",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var out strings.Builder
			renderer := &harvestRenderer{out: &out, counts: make(map[harvestKey]int)}

			// Act
			err := tt.act(renderer)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("err = %v, want it to contain %q", err, tt.want)
			}
			if out.Len() != 0 {
				t.Fatalf("a refused harvest printed %q", out.String())
			}
		})
	}
}
