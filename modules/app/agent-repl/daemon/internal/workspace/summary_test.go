package workspace

import (
	"context"
	"errors"
	"reflect"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// footerSummary is a whole summary of the footer work.
var footerSummary = wsm.WorkspaceSummary{
	Long:  "The footer's stall detail reads wrong at a cold gate. Fix what it states.",
	Short: "Fix the footer's stall detail.",
}

func TestSetSummaryRecordsTheSummaryThroughTheKeeper(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	got, err := f.verbs.SetSummary(context.Background(), "w1", footerSummary)

	// Assert.
	if err != nil || got != footerSummary {
		t.Fatalf("SetSummary = %+v, %v; want the summary", got, err)
	}
	if want := []setSummary{{ws: "w1", summary: footerSummary}}; !reflect.DeepEqual(f.summaries.set, want) {
		t.Fatalf("the keeper recorded %+v, want %+v", f.summaries.set, want)
	}
	r, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opSetSummary})
	if !ok || r.Context["short_summary"] != footerSummary.Short || r.Context["long_summary"] != footerSummary.Long {
		t.Fatalf("record = %+v, want an INFO %s naming both parts", r, opSetSummary)
	}
}

func TestSetSummaryRefusesAPartThatBreaksItsForm(t *testing.T) {
	tests := []struct {
		name     string
		summary  wsm.WorkspaceSummary
		part     any
		textKey  string
		wantText string
	}{
		{name: "a long summary of one sentence",
			summary: wsm.WorkspaceSummary{Long: "Fix the footer.", Short: "Fix the footer"},
			part:    summaryPartLong, textKey: "long_summary", wantText: "Fix the footer."},
		{name: "a short summary joining clauses",
			summary: wsm.WorkspaceSummary{Long: footerSummary.Long, Short: "Fix the footer, then the topbar"},
			part:    summaryPartShort, textKey: "short_summary", wantText: "Fix the footer, then the topbar"},
		{name: "both broken names the long one first",
			summary: wsm.WorkspaceSummary{Long: "Fix the footer.", Short: "Fix the footer, then the topbar"},
			part:    summaryPartLong, textKey: "long_summary", wantText: "Fix the footer."},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())

			// Act.
			_, err := f.verbs.SetSummary(context.Background(), "w1", tt.summary)

			// Assert: refused naming the part, its text and the validator's
			// reason; nothing recorded.
			refusal := asRefusal(t, err, ArmInvalidSummary)
			if refusal.Fields["part"] != tt.part || refusal.Fields[tt.textKey] != tt.wantText || refusal.Fields["reason"] == "" {
				t.Errorf("refusal fields = %v, want part %v, its text and a reason", refusal.Fields, tt.part)
			}
			if len(f.summaries.set) != 0 {
				t.Errorf("the keeper recorded %+v for a refused summary", f.summaries.set)
			}
			r, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opRefusal})
			if !ok || r.Context["arm"] != ArmInvalidSummary || r.Context["rpc"] != rpcSetSummary {
				t.Errorf("refusal record = %+v, want an INFO refusal of %s naming %s", r, rpcSetSummary, ArmInvalidSummary)
			}
		})
	}
}

func TestSetSummaryRefusesAWorkspaceThatIsNotRegistered(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	_, err := f.verbs.SetSummary(context.Background(), "absent", footerSummary)

	// Assert.
	asRefusal(t, err, ArmUnknownWorkspace)
	if len(f.summaries.set) != 0 {
		t.Errorf("the keeper recorded %+v for an unknown workspace", f.summaries.set)
	}
}

func TestSetSummarySurfacesTheKeepersFailure(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.summaries.setErr = errors.New("the state database is locked")

	// Act.
	_, err := f.verbs.SetSummary(context.Background(), "w1", footerSummary)

	// Assert: the keeper owns the record; the verb surfaces the failure and
	// claims no success.
	if !errors.Is(err, f.summaries.setErr) {
		t.Fatalf("SetSummary = %v, want the keeper's failure", err)
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opSetSummary}); ok {
		t.Errorf("an INFO %s claimed a summary that was never recorded", opSetSummary)
	}
}
