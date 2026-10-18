package feed

import (
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
)

// mergeRowTold is one telling of Deps.MergeRow or Deps.MergeRowRetired.
type mergeRowTold struct {
	retired bool
	lease   ids.LeaseID
	row     string
}

// mergeRowsDaemon is a daemon over STORE whose merge-row deps record into TOLD.
func mergeRowsDaemon(t *testing.T, store DurableRows, told *[]mergeRowTold) (*resolver, *dlog.TestLogger) {
	t.Helper()
	return daemonOverWith(t, store, func(d *Deps) {
		d.MergeRow = func(ws ids.WorkspaceID, lease ids.LeaseID, row *frontendv1.FeedRow) {
			*told = append(*told, mergeRowTold{lease: lease, row: row.GetId().GetValue()})
		}
		d.MergeRowRetired = func(ws ids.WorkspaceID, lease ids.LeaseID, id string) {
			*told = append(*told, mergeRowTold{retired: true, lease: lease, row: id})
		}
	})
}

// keyedMergeHead is an async merge head keyed as the orchestrator keys one.
func keyedMergeHead() *frontendv1.FeedRow {
	row := asyncStateHead(stateRunning)
	row.Id = testEncode(feedid.Ref{WS: testWorkspace, Feed: rootFeed(), Row: feedid.RowKey{Kind: feedid.KindMergeHead, ID: "lease-7"}})
	return row
}

// toldString renders TOLD for comparison.
func toldString(told []mergeRowTold) string {
	var parts []string
	for _, t := range told {
		verb := "row"
		if t.retired {
			verb = "retired"
		}
		parts = append(parts, verb+":"+string(t.lease))
	}
	return strings.Join(parts, ",")
}

func TestAPublishedMergeHeadIsToldWithItsLedger(t *testing.T) {
	// Arrange
	var told []mergeRowTold
	r, _ := mergeRowsDaemon(t, nil, &told)
	head := keyedMergeHead()

	// Act
	r.UpsertDurable(testWorkspace, rootFeed(), head)

	// Assert
	if len(told) != 1 || told[0].retired || told[0].lease != "lease-7" || told[0].row != head.GetId().GetValue() {
		t.Fatalf("told = %+v, want the head under lease-7", told)
	}
}

func TestAPublishedMergeTabIsToldWithItsSubFeedsLedger(t *testing.T) {
	// Arrange
	var told []mergeRowTold
	r, _ := mergeRowsDaemon(t, nil, &told)
	feed, tab := mergeTabRow()

	// Act
	r.UpsertDurable(testWorkspace, feed, tab)

	// Assert
	if toldString(told) != "row:lease-7" {
		t.Fatalf("told = %v, want the tab under lease-7", toldString(told))
	}
}

func TestAToldMergeRowCarriesItsOrderKey(t *testing.T) {
	// Arrange
	var keys []string
	r, _ := daemonOverWith(t, nil, func(d *Deps) {
		d.MergeRow = func(_ ids.WorkspaceID, _ ids.LeaseID, row *frontendv1.FeedRow) {
			keys = append(keys, row.GetOrder().GetKey())
		}
	})

	// Act
	r.UpsertDurable(testWorkspace, rootFeed(), keyedMergeHead())

	// Assert
	if len(keys) != 1 || keys[0] == "" {
		t.Fatalf("keys = %v, want the head's stamped order key", keys)
	}
}

func TestARowThatIsNoMergeRowIsNotTold(t *testing.T) {
	// Arrange
	var told []mergeRowTold
	r, _ := mergeRowsDaemon(t, nil, &told)

	// Act
	r.UpsertCommandPanel(testWorkspace, &frontendv1.FeedCommandPanel{})

	// Assert
	if len(told) != 0 {
		t.Fatalf("told = %v, want nothing for a command panel", toldString(told))
	}
}

func TestARetiredMergeRowIsTold(t *testing.T) {
	// Arrange
	var told []mergeRowTold
	r, _ := mergeRowsDaemon(t, nil, &told)
	head := keyedMergeHead()
	r.UpsertDurable(testWorkspace, rootFeed(), head)

	// Act
	r.mu.Lock()
	r.retire(r.state(testWorkspace), rootFeed(), head.GetId().GetValue())
	r.mu.Unlock()

	// Assert
	if toldString(told) != "row:lease-7,retired:lease-7" {
		t.Fatalf("told = %v, want the head then its retirement", toldString(told))
	}
}

func TestAResetTellsEveryMergeRowRetired(t *testing.T) {
	// Arrange
	var told []mergeRowTold
	r, _ := mergeRowsDaemon(t, nil, &told)
	r.UpsertDurable(testWorkspace, rootFeed(), keyedMergeHead())
	feed, tab := mergeTabRow()
	r.UpsertDurable(testWorkspace, feed, tab)
	told = nil

	// Act
	r.ResetWorkspace(testWorkspace, "bound to another conversation")

	// Assert
	if toldString(told) != "retired:lease-7,retired:lease-7" {
		t.Fatalf("told = %v, want the head and the tab retired", toldString(told))
	}
}

func TestARestoredMergeRowIsToldToTheNewDaemon(t *testing.T) {
	// Arrange: one daemon draws the head; a second restores it.
	store := newFakeDurableRows()
	var first, second []mergeRowTold
	old, _ := mergeRowsDaemon(t, store, &first)
	old.UpsertDurable(testWorkspace, rootFeed(), keyedMergeHead())
	fresh, _ := mergeRowsDaemon(t, store, &second)

	// Act
	feedRows(fresh, rootFeed())

	// Assert
	if toldString(second) != "row:lease-7" {
		t.Fatalf("told = %v, want the restored head", toldString(second))
	}
}

func TestAMergeTabOffAMergeFeedIsRecordedAndNotTold(t *testing.T) {
	// Arrange
	var told []mergeRowTold
	r, log := mergeRowsDaemon(t, nil, &told)
	_, tab := mergeTabRow()

	// Act
	r.UpsertDurable(testWorkspace, rootFeed(), tab)

	// Assert
	if len(told) != 0 {
		t.Fatalf("told = %v, want nothing", toldString(told))
	}
	records := dlog.FilterRecords(log.Records(), dlog.RecordQuery{Operation: "daemon.feed.merge_tab_off_merge_feed"})
	if len(records) != 1 || records[0].Level != dlog.LevelError || records[0].Context["row"] != tab.GetId().GetValue() {
		t.Fatalf("records = %v, want one ERROR naming the row", records)
	}
}

func TestAMergeHeadNotKeyedAsOneIsRecordedAndNotTold(t *testing.T) {
	// Arrange: a head row keyed as a tab.
	var told []mergeRowTold
	r, log := mergeRowsDaemon(t, nil, &told)
	head := asyncStateHead(stateRunning)

	// Act
	r.UpsertDurable(testWorkspace, rootFeed(), head)

	// Assert
	if len(told) != 0 {
		t.Fatalf("told = %v, want nothing", toldString(told))
	}
	records := dlog.FilterRecords(log.Records(), dlog.RecordQuery{Operation: "daemon.feed.merge_head_unkeyed"})
	if len(records) != 1 || records[0].Level != dlog.LevelError || records[0].Context["cause"] == "" {
		t.Fatalf("records = %v, want one ERROR carrying its cause", records)
	}
}
