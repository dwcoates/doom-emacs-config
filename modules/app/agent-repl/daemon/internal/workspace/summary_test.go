package workspace

import (
	"context"
	"errors"
	"reflect"
	"testing"

	"agent-repld/internal/dlog"
)

func TestSetSummaryRecordsTheSummaryThroughTheKeeper(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	got, err := f.verbs.SetSummary(context.Background(), "w1", "Fix the footer's stall detail.")

	// Assert.
	if err != nil || got != "Fix the footer's stall detail." {
		t.Fatalf("SetSummary = %q, %v; want the summary", got, err)
	}
	if want := []setSummary{{ws: "w1", summary: "Fix the footer's stall detail."}}; !reflect.DeepEqual(f.summaries.set, want) {
		t.Fatalf("the keeper recorded %+v, want %+v", f.summaries.set, want)
	}
	r, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opSetSummary})
	if !ok || r.Context["summary"] != "Fix the footer's stall detail." {
		t.Fatalf("record = %+v, want an INFO %s naming the summary", r, opSetSummary)
	}
}

func TestSetSummaryRefusesASummaryTheTitleLineDoesNotDraw(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	_, err := f.verbs.SetSummary(context.Background(), "w1", "Fix the footer, then the topbar")

	// Assert: refused with the summary and the validator's reason; nothing
	// recorded.
	refusal := asRefusal(t, err, ArmInvalidSummary)
	if refusal.Fields["summary"] != "Fix the footer, then the topbar" || refusal.Fields["reason"] == "" {
		t.Errorf("refusal fields = %v, want the summary and a reason", refusal.Fields)
	}
	if len(f.summaries.set) != 0 {
		t.Errorf("the keeper recorded %+v for a refused summary", f.summaries.set)
	}
	r, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opRefusal})
	if !ok || r.Context["arm"] != ArmInvalidSummary || r.Context["rpc"] != rpcSetSummary {
		t.Errorf("refusal record = %+v, want an INFO refusal of %s naming %s", r, rpcSetSummary, ArmInvalidSummary)
	}
}

func TestSetSummaryRefusesAWorkspaceThatIsNotRegistered(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	_, err := f.verbs.SetSummary(context.Background(), "absent", "Fix the footer")

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
	_, err := f.verbs.SetSummary(context.Background(), "w1", "Fix the footer")

	// Assert: the keeper owns the record; the verb surfaces the failure and
	// claims no success.
	if !errors.Is(err, f.summaries.setErr) {
		t.Fatalf("SetSummary = %v, want the keeper's failure", err)
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opSetSummary}); ok {
		t.Errorf("an INFO %s claimed a summary that was never recorded", opSetSummary)
	}
}
