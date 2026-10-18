package footer

import (
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/reflect/protoreflect"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// mergeHead is a merge bubble's head row as the feed holds it: keyed ID, at
// order KEY, async or sync, carrying RESULT (nil for the in-flight `update`).
func mergeHead(id, key, label string, startedMs int64, async bool, result any) *frontendv1.FeedRow {
	mode := &frontendv1.FeedMergeMode{Mode: &frontendv1.FeedMergeMode_Sync{Sync: &frontendv1.FeedMergeModeSync{}}}
	if async {
		mode = &frontendv1.FeedMergeMode{Mode: &frontendv1.FeedMergeMode_Async{Async: &frontendv1.FeedMergeModeAsync{}}}
	}
	merge := &frontendv1.FeedMerge{Head: &frontendv1.FeedMergeHead{
		Label:   &frontendv1.FeedMergeLabel{Text: label},
		Runtime: &frontendv1.FeedMergeRuntime{StartedAtMs: startedMs},
		Mode:    mode,
	}}
	switch r := result.(type) {
	case *frontendv1.FeedMergeSuccess:
		merge.Result = &frontendv1.FeedMerge_Success{Success: r}
	case *frontendv1.FeedMergeError:
		merge.Result = &frontendv1.FeedMerge_Error{Error: r}
	default:
		merge.Result = &frontendv1.FeedMerge_Update{Update: &frontendv1.FeedMergeUpdate{}}
	}
	return &frontendv1.FeedRow{
		Id:    &frontendv1.FeedId{Value: id},
		Order: &frontendv1.FeedRowOrder{Key: key},
		Row:   &frontendv1.FeedRow_Activity{Activity: &frontendv1.FeedTurnActivity{Unit: &frontendv1.FeedTurnActivity_Merge{Merge: merge}}},
	}
}

// tabStates are the shared badge leaves, by name.
var tabStates = map[string]func() *frontendv1.FeedMergeTabTests{
	"live": func() *frontendv1.FeedMergeTabTests {
		return &frontendv1.FeedMergeTabTests{State: &frontendv1.FeedMergeTabTests_Live{Live: &frontendv1.FeedMergeTabLive{}}}
	},
	"settled": func() *frontendv1.FeedMergeTabTests {
		return &frontendv1.FeedMergeTabTests{State: &frontendv1.FeedMergeTabTests_Settled{Settled: &frontendv1.FeedMergeTabSettled{}}}
	},
	"waiting": func() *frontendv1.FeedMergeTabTests {
		return &frontendv1.FeedMergeTabTests{State: &frontendv1.FeedMergeTabTests_WaitingOnUser{WaitingOnUser: &frontendv1.FeedMergeTabWaitingOnUser{}}}
	},
}

// mergeTabRow is a top-level tests-kind tab row labelled WORDS at ROUND, in
// STATE ("live", "settled", "waiting"), at order KEY.
func mergeTabRow(id, key, words string, round uint32, state string) *frontendv1.FeedRow {
	return &frontendv1.FeedRow{
		Id:    &frontendv1.FeedId{Value: id},
		Order: &frontendv1.FeedRowOrder{Key: key},
		Row: &frontendv1.FeedRow_MergeTab{MergeTab: &frontendv1.FeedMergeTab{
			Label: &frontendv1.FeedMergeTabLabel{Words: &frontendv1.FeedMergeTabLabel_Text{Text: words}, Round: round},
			Kind:  &frontendv1.FeedMergeTab_Tests{Tests: tabStates[state]()},
		}},
	}
}

const testLease ids.LeaseID = "lease-1"

func TestAnAsyncMergeInFlightRaisesTheMergesChipAndRow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnMergeRow(testWS, testLease, mergeHead("r|merge_head|lease-1", "k1", "fix → master", 1234, true, nil))

	// Assert
	view := h.view(t)
	if got := view.GetStrip().GetLiveWork().GetMerges().GetCount(); got != 1 {
		t.Fatalf("merges chip = %d, want 1", got)
	}
	rows := view.GetExpanded().GetMerges().GetRows()
	if len(rows) != 1 {
		t.Fatalf("merges rows = %d, want 1", len(rows))
	}
	row := rows[0]
	if row.GetLabel().GetText() != "fix → master" || row.GetRuntime().GetStartedAtMs() != 1234 ||
		row.GetWork().GetValue() != "lease-1" || row.GetJump().GetEntry().GetValue() != "r|merge_head|lease-1" {
		t.Fatalf("row = %v, want the head's label, clock, ledger and FeedId", row)
	}
	if row.Step != nil || row.WaitingOnUser != nil {
		t.Fatalf("a bubble with no tab drew a step or a waiting marker: %v", row)
	}
}

func TestASyncMergeHeadDrawsNoMergesRow(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnMergeRow(testWS, testLease, mergeHead("r|merge_head|lease-1", "k1", "fix → master", 1, false, nil))

	// Assert
	if h.view(t).GetStrip().GetLiveWork().GetMerges() != nil {
		t.Fatalf("a sync merge raised the merges chip")
	}
	if n := len(h.view(t).GetExpanded().GetMerges().GetRows()); n != 0 {
		t.Fatalf("merges rows = %d, want 0 for a sync merge", n)
	}
}

func TestAResolvedAsyncHeadLeavesTheMergesPanel(t *testing.T) {
	cases := []struct {
		name   string
		result any
	}{
		{"landed", &frontendv1.FeedMergeSuccess{Commit: "abc"}},
		{"failed", &frontendv1.FeedMergeError{Reason: &frontendv1.FeedMergeError_Failed{Failed: &frontendv1.FeedMergeFailed{Summary: "tests"}}}},
		{"abandoned", &frontendv1.FeedMergeError{Reason: &frontendv1.FeedMergeError_Abandoned{Abandoned: &frontendv1.FeedMergeAbandoned{Summary: "closed"}}}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			h.r.OnMergeRow(testWS, testLease, mergeHead("r|merge_head|lease-1", "k1", "fix → master", 1, true, nil))
			h.r.OnMergeRow(testWS, testLease, mergeTabRow("m|tab|queue", "t1", "queue", 1, "live"))

			// Act
			h.r.OnMergeRow(testWS, testLease, mergeHead("r|merge_head|lease-1", "k1", "fix → master", 1, true, tc.result))

			// Assert
			if h.view(t).GetStrip().GetLiveWork().GetMerges() != nil {
				t.Fatalf("the merges chip survived the head's result")
			}
			s := h.r.states[testWS]
			if len(s.merges.tabs) != 0 {
				t.Fatalf("a resolved merge's tabs were kept: %v", s.merges.tabs)
			}
		})
	}
}

func TestATabDrawnAfterItsHeadResolvedIsNotKept(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnMergeRow(testWS, testLease, mergeHead("r|merge_head|lease-1", "k1", "fix → master", 1, true, &frontendv1.FeedMergeSuccess{}))

	// Act
	h.r.OnMergeRow(testWS, testLease, mergeTabRow("m|tab|post", "t9", "post-prompt", 1, "settled"))

	// Assert
	if len(h.r.states[testWS].merges.tabs) != 0 {
		t.Fatalf("a tab of a resolved merge was kept")
	}
}

func TestTheMergeRowsStepIsTheCurrentTopLevelTab(t *testing.T) {
	cases := []struct {
		name string
		tabs []*frontendv1.FeedRow
		want string
	}{
		{
			name: "the newest live tab",
			tabs: []*frontendv1.FeedRow{
				mergeTabRow("m|tab|queue", "t1", "queue", 1, "settled"),
				mergeTabRow("m|tab|tests", "t3", "tests", 1, "live"),
				mergeTabRow("m|tab|rebasing", "t2", "rebasing", 1, "settled"),
			},
			want: "tests",
		},
		{
			name: "a live tab drawn again outranks a newer settled one",
			tabs: []*frontendv1.FeedRow{
				mergeTabRow("m|tab|rebasing", "t2", "rebasing", 1, "live"),
				mergeTabRow("m|tab|committing", "t4", "committing", 1, "settled"),
			},
			want: "rebasing",
		},
		{
			name: "the newest tab when every tab is settled",
			tabs: []*frontendv1.FeedRow{
				mergeTabRow("m|tab|queue", "t1", "queue", 1, "settled"),
				mergeTabRow("m|tab|tests", "t3", "tests", 1, "settled"),
			},
			want: "tests",
		},
		{
			name: "a round beyond the first is decorated",
			tabs: []*frontendv1.FeedRow{mergeTabRow("m|tab|committing-2", "t5", "committing", 2, "live")},
			want: "committing (2)",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			h.r.OnMergeRow(testWS, testLease, mergeHead("r|merge_head|lease-1", "k1", "fix → master", 1, true, nil))

			// Act
			for _, tab := range tc.tabs {
				h.r.OnMergeRow(testWS, testLease, tab)
			}

			// Assert
			if got := h.view(t).GetExpanded().GetMerges().GetRows()[0].GetStep().GetText(); got != tc.want {
				t.Fatalf("step = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestATabDrawnBeforeItsHeadIsTheRowsStep(t *testing.T) {
	// Arrange: a restore draws the tab first.
	h := newHarness(t)
	connected(h)
	h.r.OnMergeRow(testWS, testLease, mergeTabRow("m|tab|queue", "t1", "queue", 1, "live"))

	// Act
	h.r.OnMergeRow(testWS, testLease, mergeHead("r|merge_head|lease-1", "k1", "fix → master", 1, true, nil))

	// Assert
	if got := h.view(t).GetExpanded().GetMerges().GetRows()[0].GetStep().GetText(); got != "queue" {
		t.Fatalf("step = %q, want the tab drawn before the head", got)
	}
}

func TestASubtabIsNeverTheRowsStep(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnMergeRow(testWS, testLease, mergeHead("r|merge_head|lease-1", "k1", "fix → master", 1, true, nil))
	h.r.OnMergeRow(testWS, testLease, mergeTabRow("m|tab|tests", "t1", "tests", 1, "live"))
	sub := mergeTabRow("m|tab|run-1", "t2", "run 1", 1, "live")
	sub.Parent = &frontendv1.FeedRowParent{Row: &frontendv1.FeedId{Value: "m|tab|tests"}}

	// Act
	h.r.OnMergeRow(testWS, testLease, sub)

	// Assert
	if got := h.view(t).GetExpanded().GetMerges().GetRows()[0].GetStep().GetText(); got != "tests" {
		t.Fatalf("step = %q, want the top-level tab, not its subtab", got)
	}
}

func TestACurrentTabWaitingOnTheUserMarksTheRowAndTheChip(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnMergeRow(testWS, testLease, mergeHead("r|merge_head|lease-1", "k1", "fix → master", 1, true, nil))
	h.r.OnMergeRow(testWS, "lease-2", mergeHead("r|merge_head|lease-2", "k2", "other → master", 1, true, nil))

	// Act
	h.r.OnMergeRow(testWS, testLease, mergeTabRow("m|tab|tests", "t1", "tests", 1, "waiting"))

	// Assert
	view := h.view(t)
	chip := view.GetStrip().GetLiveWork().GetMerges()
	if chip.GetCount() != 2 || chip.GetWaitingOnUser().GetCount() != 1 {
		t.Fatalf("chip = %v, want 2 merges, 1 waiting on the user", chip)
	}
	rows := view.GetExpanded().GetMerges().GetRows()
	if rows[0].WaitingOnUser == nil || rows[1].WaitingOnUser != nil {
		t.Fatalf("waiting markers = %v, %v; want only the waiting merge's", rows[0].WaitingOnUser, rows[1].WaitingOnUser)
	}
}

func TestNoMergeWaitingDrawsNoChipWaitingGlyph(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnMergeRow(testWS, testLease, mergeHead("r|merge_head|lease-1", "k1", "fix → master", 1, true, nil))

	// Act
	h.r.OnMergeRow(testWS, testLease, mergeTabRow("m|tab|tests", "t1", "tests", 1, "live"))

	// Assert
	if h.view(t).GetStrip().GetLiveWork().GetMerges().WaitingOnUser != nil {
		t.Fatalf("the chip drew the waiting glyph with no merge waiting")
	}
}

func TestTheMergesPanelDrawsInTheFeedsHeadOrder(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act: the later-placed head arrives first.
	h.r.OnMergeRow(testWS, "lease-2", mergeHead("r|merge_head|lease-2", "k2", "second", 1, true, nil))
	h.r.OnMergeRow(testWS, testLease, mergeHead("r|merge_head|lease-1", "k1", "first", 1, true, nil))

	// Assert
	var got []string
	for _, row := range h.view(t).GetExpanded().GetMerges().GetRows() {
		got = append(got, row.GetLabel().GetText())
	}
	if strings.Join(got, ",") != "first,second" {
		t.Fatalf("rows = %v, want the feed's order", got)
	}
}

func TestARetiredHeadTakesItsMergesRowAway(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnMergeRow(testWS, testLease, mergeHead("r|merge_head|lease-1", "k1", "fix → master", 1, true, nil))
	h.r.OnMergeRow(testWS, testLease, mergeTabRow("m|tab|queue", "t1", "queue", 1, "live"))

	// Act
	h.r.OnMergeRowRetired(testWS, testLease, "r|merge_head|lease-1")

	// Assert
	if h.view(t).GetStrip().GetLiveWork().GetMerges() != nil {
		t.Fatalf("the merges chip survived its head's retirement")
	}
	if len(h.r.states[testWS].merges.tabs) != 0 {
		t.Fatalf("a retired head's tabs were kept")
	}
}

func TestARetiredTabTakesItsStepAway(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnMergeRow(testWS, testLease, mergeHead("r|merge_head|lease-1", "k1", "fix → master", 1, true, nil))
	h.r.OnMergeRow(testWS, testLease, mergeTabRow("m|tab|queue", "t1", "queue", 1, "live"))

	// Act
	h.r.OnMergeRowRetired(testWS, testLease, "m|tab|queue")

	// Assert
	row := h.view(t).GetExpanded().GetMerges().GetRows()[0]
	if row.Step != nil {
		t.Fatalf("step = %v, want none once its tab is retired", row.Step)
	}
}

func TestAMergeRowsJumpIsRecordedAsAMerge(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnMergeRow(testWS, testLease, mergeHead("r|merge_head|lease-1", "k1", "fix → master", 1, true, nil))

	// Assert
	notes := recordsOf(h.log.Records(), "daemon.footer.jump_resolution")
	for _, note := range notes {
		if note.Context["kind"] == "merge" && note.Context["resolution"] == "entry" {
			return
		}
	}
	t.Fatalf("no merge jump resolution was recorded: %v", notes)
}

func TestARowNeitherHeadNorTabIsRecordedAndTakenAsNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.OnMergeRow(testWS, testLease, &frontendv1.FeedRow{Id: &frontendv1.FeedId{Value: "r|prompt|p1"}})

	// Assert
	records := recordsOf(h.log.Records(), "daemon.footer.merge_row_unknown")
	if len(records) != 1 || records[0].Level != dlog.LevelError ||
		records[0].Context["lease"] != "lease-1" || records[0].Context["row"] != "r|prompt|p1" {
		t.Fatalf("records = %v, want one ERROR naming the lease and the row", records)
	}
	if h.view(t).GetStrip().GetLiveWork().GetMerges() != nil {
		t.Fatalf("an unknown row raised the merges chip")
	}
}

func TestAMalformedTabPanics(t *testing.T) {
	cases := []struct {
		name string
		tab  *frontendv1.FeedMergeTab
	}{
		{"no kind", &frontendv1.FeedMergeTab{}},
		{"no state", &frontendv1.FeedMergeTab{Kind: &frontendv1.FeedMergeTab_Tests{Tests: &frontendv1.FeedMergeTabTests{}}}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			defer func() {
				// Assert
				if recover() == nil {
					t.Fatalf("a malformed tab did not panic")
				}
			}()

			// Act
			mergeTabState("m|tab|x", tc.tab)
		})
	}
}

// EVERY TAB KIND CARRIES THE `state` ONEOF mergeTabState reads, and every one
// of its arms is a shared badge leaf or a kind-specific pending leaf, so a kind
// added without one fails here rather than panicking in production.
func TestEveryMergeTabKindCarriesAStateOneof(t *testing.T) {
	kinds := (&frontendv1.FeedMergeTab{}).ProtoReflect().Descriptor().Oneofs().ByName("kind")
	fields := kinds.Fields()
	for i := 0; i < fields.Len(); i++ {
		field := fields.Get(i)
		t.Run(string(field.Name()), func(t *testing.T) {
			// Arrange
			var states protoreflect.OneofDescriptor = field.Message().Oneofs().ByName("state")

			// Act
			present := states != nil

			// Assert
			if !present {
				t.Fatalf("tab kind %s has no state oneof", field.Name())
			}
		})
	}
}
