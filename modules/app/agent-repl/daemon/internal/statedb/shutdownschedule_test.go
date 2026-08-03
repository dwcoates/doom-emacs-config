package statedb

import (
	"path/filepath"
	"strings"
	"testing"
)

// openSchedules opens a fresh state store on disk and installs the
// drain-lease tables on it.
func openSchedules(t *testing.T) *ShutdownSchedules {
	t.Helper()
	db, err := Open(filepath.Join(t.TempDir(), "state.db"))
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() { _ = db.Close() })
	s, err := NewShutdownSchedules(db)
	if err != nil {
		t.Fatalf("NewShutdownSchedules: %v", err)
	}
	return s
}

func TestNewShutdownSchedulesRejectsANilStore(t *testing.T) {
	// Arrange, Act.
	_, err := NewShutdownSchedules(nil)

	// Assert.
	if err == nil {
		t.Fatal("NewShutdownSchedules(nil) = nil error, want a refusal")
	}
}

func TestAFreshStoreHoldsNoSchedule(t *testing.T) {
	// Arrange.
	s := openSchedules(t)

	// Act.
	_, ok, err := s.Schedule()

	// Assert.
	if err != nil {
		t.Fatalf("Schedule: %v", err)
	}
	if ok {
		t.Fatal("Schedule reported a lease on a fresh store, want none")
	}
}

func TestARecordedScheduleReadsBackWhole(t *testing.T) {
	// Arrange.
	s := openSchedules(t)
	want := ShutdownSchedule{ScheduleID: "sd_1", ScheduledAtMs: 4242, Cause: "merge rebuilt the daemon", StopShims: true}

	// Act.
	if err := s.PutSchedule(want); err != nil {
		t.Fatalf("PutSchedule: %v", err)
	}
	got, ok, err := s.Schedule()

	// Assert.
	if err != nil || !ok {
		t.Fatalf("Schedule = %v, %v, %v; want the recorded lease", got, ok, err)
	}
	if got != want {
		t.Fatalf("Schedule = %+v, want %+v", got, want)
	}
}

func TestASecondPutReplacesTheSingletonRatherThanAddingARow(t *testing.T) {
	// Arrange.
	s := openSchedules(t)
	if err := s.PutSchedule(ShutdownSchedule{ScheduleID: "sd_1", ScheduledAtMs: 1}); err != nil {
		t.Fatalf("PutSchedule: %v", err)
	}

	// Act.
	if err := s.PutSchedule(ShutdownSchedule{ScheduleID: "sd_2", ScheduledAtMs: 2}); err != nil {
		t.Fatalf("PutSchedule: %v", err)
	}
	got, _, err := s.Schedule()

	// Assert.
	if err != nil {
		t.Fatalf("Schedule: %v", err)
	}
	if got.ScheduleID != "sd_2" {
		t.Fatalf("Schedule id = %q, want sd_2 (the singleton must be replaced, not duplicated)", got.ScheduleID)
	}
}

func TestPutScheduleRefusesAScheduleWithNoID(t *testing.T) {
	// Arrange.
	s := openSchedules(t)

	// Act.
	err := s.PutSchedule(ShutdownSchedule{ScheduledAtMs: 1})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "schedule id") {
		t.Fatalf("PutSchedule with no id = %v, want a refusal naming the missing id", err)
	}
}

func TestClearScheduleReportsThatARowWasThere(t *testing.T) {
	// Arrange.
	s := openSchedules(t)
	if err := s.PutSchedule(ShutdownSchedule{ScheduleID: "sd_1"}); err != nil {
		t.Fatalf("PutSchedule: %v", err)
	}

	// Act.
	cleared, err := s.ClearSchedule()

	// Assert.
	if err != nil {
		t.Fatalf("ClearSchedule: %v", err)
	}
	if !cleared {
		t.Fatal("ClearSchedule reported no row, want the recorded lease")
	}
}

func TestClearScheduleOnAnIdleStoreReportsNoRow(t *testing.T) {
	// Arrange.
	s := openSchedules(t)

	// Act.
	cleared, err := s.ClearSchedule()

	// Assert.
	if err != nil {
		t.Fatalf("ClearSchedule: %v", err)
	}
	if cleared {
		t.Fatal("ClearSchedule reported a row on an idle store, want none")
	}
}

func TestARecordedHeldPromptReadsBackForItsWorkspace(t *testing.T) {
	// Arrange.
	s := openSchedules(t)
	want := HeldPrompt{
		EntryID: "q_1", ScheduleID: "sd_1", Workspace: "/ws/a", SessionID: "s1",
		RequestID: "r1", Text: "hello", PermissionMode: "default", QueuedAtMs: 7,
	}

	// Act.
	if err := s.RecordHeldPrompt(want); err != nil {
		t.Fatalf("RecordHeldPrompt: %v", err)
	}
	got, err := s.HeldPrompts("/ws/a")

	// Assert.
	if err != nil {
		t.Fatalf("HeldPrompts: %v", err)
	}
	if len(got) != 1 || got[0] != want {
		t.Fatalf("HeldPrompts = %+v, want exactly %+v", got, want)
	}
}

func TestHeldPromptsOfAnotherWorkspaceAreNotReturned(t *testing.T) {
	// Arrange.
	s := openSchedules(t)
	if err := s.RecordHeldPrompt(HeldPrompt{EntryID: "q_1", ScheduleID: "sd_1", Workspace: "/ws/a"}); err != nil {
		t.Fatalf("RecordHeldPrompt: %v", err)
	}

	// Act.
	got, err := s.HeldPrompts("/ws/b")

	// Assert.
	if err != nil {
		t.Fatalf("HeldPrompts: %v", err)
	}
	if len(got) != 0 {
		t.Fatalf("HeldPrompts(/ws/b) = %+v, want none", got)
	}
}

func TestHeldPromptsComeBackInSubmitOrder(t *testing.T) {
	// Arrange.
	s := openSchedules(t)
	for _, p := range []HeldPrompt{
		{EntryID: "q_late", ScheduleID: "sd_1", Workspace: "/ws/a", QueuedAtMs: 20},
		{EntryID: "q_early", ScheduleID: "sd_1", Workspace: "/ws/a", QueuedAtMs: 10},
	} {
		if err := s.RecordHeldPrompt(p); err != nil {
			t.Fatalf("RecordHeldPrompt: %v", err)
		}
	}

	// Act.
	got, err := s.HeldPrompts("/ws/a")

	// Assert.
	if err != nil {
		t.Fatalf("HeldPrompts: %v", err)
	}
	if len(got) != 2 || got[0].EntryID != "q_early" || got[1].EntryID != "q_late" {
		t.Fatalf("HeldPrompts order = %+v, want q_early then q_late", got)
	}
}

func TestRecordHeldPromptRefusesAPromptWithNoEntryID(t *testing.T) {
	// Arrange.
	s := openSchedules(t)

	// Act.
	err := s.RecordHeldPrompt(HeldPrompt{ScheduleID: "sd_1", Workspace: "/ws/a"})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "entry id") {
		t.Fatalf("RecordHeldPrompt with no entry id = %v, want a refusal naming it", err)
	}
}

func TestRecordHeldPromptRefusesAPromptNamingNoSchedule(t *testing.T) {
	// Arrange.
	s := openSchedules(t)

	// Act.
	err := s.RecordHeldPrompt(HeldPrompt{EntryID: "q_1", Workspace: "/ws/a"})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "schedule") {
		t.Fatalf("RecordHeldPrompt with no schedule = %v, want a refusal naming it", err)
	}
}

func TestRecordHeldPromptRefusesAPromptWithNoWorkspace(t *testing.T) {
	// Arrange.
	s := openSchedules(t)

	// Act.
	err := s.RecordHeldPrompt(HeldPrompt{EntryID: "q_1", ScheduleID: "sd_1"})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "workspace") {
		t.Fatalf("RecordHeldPrompt with no workspace = %v, want a refusal naming it", err)
	}
}

func TestDropHeldPromptReportsThatARowWasThere(t *testing.T) {
	// Arrange.
	s := openSchedules(t)
	if err := s.RecordHeldPrompt(HeldPrompt{EntryID: "q_1", ScheduleID: "sd_1", Workspace: "/ws/a"}); err != nil {
		t.Fatalf("RecordHeldPrompt: %v", err)
	}

	// Act.
	dropped, err := s.DropHeldPrompt("q_1")

	// Assert.
	if err != nil {
		t.Fatalf("DropHeldPrompt: %v", err)
	}
	if !dropped {
		t.Fatal("DropHeldPrompt reported no row, want the recorded prompt")
	}
}

func TestDropHeldPromptOfAnUnknownEntryReportsNoRow(t *testing.T) {
	// Arrange.
	s := openSchedules(t)

	// Act.
	dropped, err := s.DropHeldPrompt("q_missing")

	// Assert.
	if err != nil {
		t.Fatalf("DropHeldPrompt: %v", err)
	}
	if dropped {
		t.Fatal("DropHeldPrompt reported a row for an unknown entry, want none")
	}
}

func TestDropHeldPromptsForScheduleLeavesAnotherSchedulesPrompts(t *testing.T) {
	// Arrange.
	s := openSchedules(t)
	for _, p := range []HeldPrompt{
		{EntryID: "q_1", ScheduleID: "sd_1", Workspace: "/ws/a"},
		{EntryID: "q_2", ScheduleID: "sd_2", Workspace: "/ws/a"},
	} {
		if err := s.RecordHeldPrompt(p); err != nil {
			t.Fatalf("RecordHeldPrompt: %v", err)
		}
	}

	// Act.
	n, err := s.DropHeldPromptsForSchedule("sd_1")

	// Assert.
	if err != nil {
		t.Fatalf("DropHeldPromptsForSchedule: %v", err)
	}
	if n != 1 {
		t.Fatalf("DropHeldPromptsForSchedule dropped %d rows, want 1", n)
	}
	got, err := s.HeldPrompts("/ws/a")
	if err != nil {
		t.Fatalf("HeldPrompts: %v", err)
	}
	if len(got) != 1 || got[0].EntryID != "q_2" {
		t.Fatalf("HeldPrompts after the drop = %+v, want only sd_2's q_2", got)
	}
}

func TestTheHeldPromptsOutliveTheScheduleRow(t *testing.T) {
	// The bounce is over; the prompts it delayed are not. Clearing the lease
	// must never take the parked prompts with it.
	// Arrange.
	s := openSchedules(t)
	if err := s.PutSchedule(ShutdownSchedule{ScheduleID: "sd_1"}); err != nil {
		t.Fatalf("PutSchedule: %v", err)
	}
	if err := s.RecordHeldPrompt(HeldPrompt{EntryID: "q_1", ScheduleID: "sd_1", Workspace: "/ws/a"}); err != nil {
		t.Fatalf("RecordHeldPrompt: %v", err)
	}

	// Act.
	if _, err := s.ClearSchedule(); err != nil {
		t.Fatalf("ClearSchedule: %v", err)
	}
	got, err := s.HeldPrompts("/ws/a")

	// Assert.
	if err != nil {
		t.Fatalf("HeldPrompts: %v", err)
	}
	if len(got) != 1 {
		t.Fatalf("HeldPrompts after clearing the schedule = %+v, want the parked prompt to survive", got)
	}
}
