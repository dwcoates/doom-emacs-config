package workspace

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"testing"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/wsm"
)

func TestSelectRecordsTheSelectionInstant(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert.
	if f.db.current == nil || *f.db.current != "w1" || !f.db.currentAt.Equal(fixedNow) {
		t.Fatalf("current = (%v, %v), want w1 at %v", f.db.current, f.db.currentAt, fixedNow)
	}
}

func TestSelectClearsTheAttentionMarker(t *testing.T) {
	// Arrange: the user has now looked at whatever raised it.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.attention["w1"] = true

	// Act.
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert.
	if f.db.attention["w1"] {
		t.Fatal("Select() left the attention marker set")
	}
}

func TestSelectTellsTheRoster(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert.
	if len(f.sidebar.selected) != 1 || f.sidebar.selected[0] != "w1" {
		t.Fatalf("roster selections = %v, want w1 once", f.sidebar.selected)
	}
}

func TestSelectIsIdempotent(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("first Select: %v", err)
	}
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("second Select: %v", err)
	}

	// Assert.
	if *f.db.current != "w1" {
		t.Fatalf("current = %v, want w1", *f.db.current)
	}
}

func TestSelectRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	err := f.verbs.Select(context.Background(), "nope")

	// Assert.
	asRefusal(t, err, ArmUnknownWorkspace)
}

func TestSetPriorityRecordsThePriority(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	priority := wsm.PriorityP05

	// Act.
	if err := f.verbs.SetPriority(context.Background(), "w1", &priority); err != nil {
		t.Fatalf("SetPriority: %v", err)
	}

	// Assert.
	got := f.db.priorities["w1"]
	if got == nil || *got != wsm.PriorityP05 {
		t.Fatalf("recorded priority = %v, want P05", got)
	}
}

func TestSetPriorityClearsThePriority(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.SetPriority(context.Background(), "w1", nil); err != nil {
		t.Fatalf("SetPriority: %v", err)
	}

	// Assert.
	got, recorded := f.db.priorities["w1"]
	if !recorded || got != nil {
		t.Fatalf("recorded priority = (%v, %v), want a recorded clear", got, recorded)
	}
}

func TestSetPriorityRepublishesTheRoster(t *testing.T) {
	// Arrange: clients follow roster order strictly, so the order changing is
	// a roster push.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	priority := wsm.PriorityP2

	// Act.
	if err := f.verbs.SetPriority(context.Background(), "w1", &priority); err != nil {
		t.Fatalf("SetPriority: %v", err)
	}

	// Assert.
	if len(f.sidebar.registries) != 1 {
		t.Fatalf("roster republications = %d, want exactly one", len(f.sidebar.registries))
	}
}

// TestSelectTellsTheRosterAsOnePush pins that a select reaches the roster as
// ONE mutation carrying both the refreshed registry and the selection, so
// every client receives one roster per switch rather than two.
func TestSelectTellsTheRosterAsOnePush(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert.
	if want := []string{"registry+selected"}; !slices.Equal(f.sidebar.calls, want) {
		t.Fatalf("roster calls = %v, want %v", f.sidebar.calls, want)
	}
}

// TestSelectWithAnUnreadableRegistryStillTellsTheRosterTheSelection pins the
// failure arm: a registry read that fails is recorded at ERROR, and the
// selection still reaches the roster on its own push.
func TestSelectWithAnUnreadableRegistryStillTellsTheRosterTheSelection(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.listWorkspacesErr = errFake

	// Act.
	_ = f.verbs.Select(context.Background(), "w1")

	// Assert.
	if want := []string{"selected"}; !slices.Equal(f.sidebar.calls, want) {
		t.Fatalf("roster calls = %v, want %v", f.sidebar.calls, want)
	}
	if got := recordLevel(f, "could not list the workspaces for the roster"); got != "error" {
		t.Fatalf("the failed registry read was recorded at %q, want error", got)
	}
}

// TestSelectPublishesARegistryCarryingTheSelection pins that the refreshed
// registry is the one WSM holds AFTER the write, not before it.
func TestSelectPublishesARegistryCarryingTheSelection(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert.
	if len(f.sidebar.registries) != 1 {
		t.Fatalf("roster republications = %d, want exactly one", len(f.sidebar.registries))
	}
	got := f.sidebar.registries[0].Current
	if got == nil || *got != "w1" {
		t.Fatalf("the published registry's current = %v, want w1", got)
	}
}

// TestSelectPublishesARegistryCarryingTheSelectionInstant pins that the roster
// push a selection makes carries the new durable last-selected instant, which
// is what clients order most-recently-selected by.
func TestSelectPublishesARegistryCarryingTheSelectionInstant(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert.
	if len(f.sidebar.registries) != 1 {
		t.Fatalf("roster republications = %d, want exactly one", len(f.sidebar.registries))
	}
	for _, ws := range f.sidebar.registries[0].Workspaces {
		if ws.ID != "w1" {
			continue
		}
		if ws.LastSelectedAt == nil || !ws.LastSelectedAt.Equal(fixedNow) {
			t.Fatalf("the published last-selected instant = %v, want %v", ws.LastSelectedAt, fixedNow)
		}
		return
	}
	t.Fatalf("the published registry carries no w1")
}

// TestReselectingDoesNotRestampTheSelectionInstant pins the idempotence the
// contract states: re-selecting the workspace already being looked at is a
// success that CHANGES NO VIEW. The instant answers "when did the user last
// switch to this", and the user did not switch.
func TestReselectingDoesNotRestampTheSelectionInstant(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("first Select: %v", err)
	}
	f.db.currentAt = fixedNow.Add(-time.Hour)

	// Act.
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("second Select: %v", err)
	}

	// Assert.
	if !f.db.currentAt.Equal(fixedNow.Add(-time.Hour)) {
		t.Fatalf("the selection instant = %v, want the re-selection to leave it alone", f.db.currentAt)
	}
}

// TestReselectingStillClearsTheAttentionMarker pins that idempotence does not
// swallow the clear: a notification can raise the marker while the workspace
// IS current, and looking at it again is what clears it.
func TestReselectingStillClearsTheAttentionMarker(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("first Select: %v", err)
	}
	f.db.attention["w1"] = true

	// Act.
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("second Select: %v", err)
	}

	// Assert.
	if f.db.attention["w1"] {
		t.Fatal("re-selecting left the attention marker set")
	}
}

func TestSelectRevivesAndUnparksAHibernatedWorkspace(t *testing.T) {
	// Arrange: the idle sweep stood this workspace's session down.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.hibernate("w1")

	// Act.
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert.
	if len(f.fleet.started) != 1 || f.fleet.started[0] != "w1" {
		t.Fatalf("started = %v, want [w1]", f.fleet.started)
	}
	if len(f.topbarParked) != 1 || f.topbarParked[0] {
		t.Fatalf("topbar parked = %v, want the park lifted", f.topbarParked)
	}
}

func TestSelectStartsNothingForALiveWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.fleet.live["w1"] = true

	// Act.
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert.
	if len(f.fleet.started) != 0 {
		t.Fatalf("started = %v, want none: a live workspace has nothing to revive", f.fleet.started)
	}
}

func TestSelectFailsWhenTheRevivalFails(t *testing.T) {
	// Arrange: a selected workspace with no session is the state the revival
	// exists to abolish, so answering success would hide it.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.hibernate("w1")
	f.fleet.startErr = errors.New("boom")

	// Act.
	err := f.verbs.Select(context.Background(), "w1")

	// Assert.
	if err == nil {
		t.Fatal("Select() answered no error for a failed revival")
	}
}

// recordLevel returns the level of the first captured record whose message
// equals want, or "" when none matches.
func recordLevel(f *fixture, want string) string {
	for _, r := range f.log.logger.Records() {
		if r.Message == want {
			return r.Level
		}
	}
	return ""
}

func TestSelectLogsTheSwitchReceiptAtInfo(t *testing.T) {
	// Arrange: a switch to a workspace that is not already current.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert: a switch must leave an info-level trace in production logs.
	if got := recordLevel(f, "selected the workspace"); got != "info" {
		t.Fatalf("the select receipt is %q, want info", got)
	}
}

func TestSelectLogsTheReselectionReceiptAtInfo(t *testing.T) {
	// Arrange: the workspace is already current, so the next select re-selects it.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("first Select: %v", err)
	}

	// Act.
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("second Select: %v", err)
	}

	// Assert.
	if got := recordLevel(f, "the workspace was already current; the selection instant stands"); got != "info" {
		t.Fatalf("the reselection receipt is %q, want info", got)
	}
}

// ---- MarkViewed ------------------------------------------------------------

func TestMarkViewedTellsTheRoster(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.MarkViewed(context.Background(), "w1"); err != nil {
		t.Fatalf("MarkViewed: %v", err)
	}

	// Assert.
	if len(f.sidebar.viewed) != 1 || f.sidebar.viewed[0] != "w1" {
		t.Fatalf("roster viewed reports = %v, want w1 once", f.sidebar.viewed)
	}
}

func TestMarkViewedWritesNoDurableRecord(t *testing.T) {
	// Arrange: the display mode is a VIEW fact and must not outlive a restart.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.MarkViewed(context.Background(), "w1"); err != nil {
		t.Fatalf("MarkViewed: %v", err)
	}

	// Assert: looking at a workspace is not selecting it either.
	if f.db.current != nil {
		t.Fatalf("current = %v, want MarkViewed to have recorded no selection", f.db.current)
	}
}

func TestMarkViewedIsIdempotent(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act.
	if err := f.verbs.MarkViewed(context.Background(), "w1"); err != nil {
		t.Fatalf("first MarkViewed: %v", err)
	}
	if err := f.verbs.MarkViewed(context.Background(), "w1"); err != nil {
		t.Fatalf("second MarkViewed: %v", err)
	}

	// Assert: a second report is a success that changes nothing downstream.
	if len(f.sidebar.viewed) != 2 {
		t.Fatalf("roster viewed reports = %v, want both reports forwarded", f.sidebar.viewed)
	}
}

func TestMarkViewedRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	err := f.verbs.MarkViewed(context.Background(), "nope")

	// Assert: an unregistered workspace has no row to mark.
	if err == nil {
		t.Fatal("MarkViewed() accepted a workspace the registry does not hold")
	}
	if len(f.sidebar.viewed) != 0 {
		t.Fatalf("roster viewed reports = %v, want none for a refused mark", f.sidebar.viewed)
	}
}

// ---- The selection goes first; the revival follows -------------------------
//
// Owner ruling, 2026-09-19: the selection is made current and pushed to the
// roster BEFORE any revival, the row carries REVIVING while the revival runs,
// and the stamped current reflects REQUEST order — never the order revivals
// happen to finish in.

func TestSelectTellsTheRosterInOrder(t *testing.T) {
	cases := []struct {
		name     string
		parked   bool
		startErr error
		want     []string
	}{
		{name: "a live workspace", want: []string{"registry+selected"}},
		{name: "a parked workspace that revives", parked: true,
			want: []string{"registry+selected", "reviving", "revived"}},
		{name: "a parked workspace whose revival fails", parked: true, startErr: errFake,
			want: []string{"registry+selected", "reviving", "revived"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", t.TempDir())
			if tc.parked {
				f.hibernate("w1")
			}
			f.fleet.startErr = tc.startErr

			// Act.
			_ = f.verbs.Select(context.Background(), "w1")

			// Assert: the selection lands whole before the revival begins, and
			// the marker is lowered however the revival ended.
			calls, _, _ := f.sidebar.snapshot()
			if !slices.Equal(calls, tc.want) {
				t.Fatalf("roster calls = %v, want %v", calls, tc.want)
			}
		})
	}
}

func TestSelectStampsTheSelectionBeforeTheRevivalStarts(t *testing.T) {
	// Arrange: a parked workspace whose bring-up is held at the gate.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.hibernate("w1")
	entered, release := f.gateStarts(1)
	done := f.selectAsync(context.Background(), "w1")

	// Act: the revival has begun.
	receive(t, entered, "the revival's Start")

	// Assert: the selection is already current AND already on the roster.
	_, selected, _ := f.sidebar.snapshot()
	close(release)
	if err := receive(t, done, "the select's answer"); err != nil {
		t.Fatalf("Select: %v", err)
	}
	if !slices.Equal(selected, []ids.WorkspaceID{"w1"}) {
		t.Fatalf("roster selections while reviving = %v, want [w1]", selected)
	}
}

func TestSelectMarksTheRowRevivingWhileTheRevivalRuns(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.hibernate("w1")
	entered, release := f.gateStarts(1)
	done := f.selectAsync(context.Background(), "w1")

	// Act.
	receive(t, entered, "the revival's Start")

	// Assert: the marker stands for the whole bring-up.
	_, _, reviving := f.sidebar.snapshot()
	close(release)
	if err := receive(t, done, "the select's answer"); err != nil {
		t.Fatalf("Select: %v", err)
	}
	if !slices.Equal(reviving, []revivingEdge{{"w1", true}}) {
		t.Fatalf("reviving edges during the bring-up = %v, want [{w1 true}]", reviving)
	}
}

func TestSelectLowersTheRevivingMarkerWhenTheRevivalFails(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.hibernate("w1")
	f.fleet.startErr = errFake

	// Act.
	_ = f.verbs.Select(context.Background(), "w1")

	// Assert: a failed bring-up is not still "coming back".
	_, _, reviving := f.sidebar.snapshot()
	if !slices.Equal(reviving, []revivingEdge{{"w1", true}, {"w1", false}}) {
		t.Fatalf("reviving edges = %v, want raised then lowered", reviving)
	}
}

func TestSelectAnswersTheRevivalFailureToTheCaller(t *testing.T) {
	// Arrange: the selection is pushed before the revival, and the failure
	// must still reach whoever asked.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.hibernate("w1")
	f.fleet.startErr = errFake

	// Act.
	err := f.verbs.Select(context.Background(), "w1")

	// Assert.
	if !errors.Is(err, errFake) {
		t.Fatalf("Select() = %v, want the revival's failure", err)
	}
}

func TestAFailedRevivalLeavesTheSelectionStanding(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.hibernate("w1")
	f.fleet.startErr = errFake

	// Act.
	_ = f.verbs.Select(context.Background(), "w1")

	// Assert: the user DID switch; the error says what did not come up.
	if f.db.current == nil || *f.db.current != "w1" {
		t.Fatalf("current = %v, want w1", f.db.current)
	}
}

func TestSelectRaisesNoRevivingMarkerForALiveWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.fleet.live["w1"] = true

	// Act.
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert.
	if _, _, reviving := f.sidebar.snapshot(); len(reviving) != 0 {
		t.Fatalf("reviving edges = %v, want none for a live workspace", reviving)
	}
}

// arrangeSwitchDuringRevival selects parked workspace "slow" and, while its
// bring-up is held at the gate, selects live workspace "fast": the user's
// LAST switch. It answers the gate's release and the slow select's answer.
func arrangeSwitchDuringRevival(t *testing.T) (*fixture, chan struct{}, <-chan error) {
	t.Helper()
	f := newFixture(t)
	f.workspace("slow", t.TempDir())
	f.workspace("fast", t.TempDir())
	f.hibernate("slow")
	f.fleet.live["fast"] = true
	entered, release := f.gateStarts(1)
	slow := f.selectAsync(context.Background(), "slow")
	receive(t, entered, "the slow revival's Start")
	if err := f.verbs.Select(context.Background(), "fast"); err != nil {
		t.Fatalf("Select(fast): %v", err)
	}
	return f, release, slow
}

func TestSelectLandsOnTheLastRequestedWorkspaceWhileAnEarlierRevivalRuns(t *testing.T) {
	// Arrange, Act: "fast" was requested after "slow", whose revival is still
	// in flight.
	f, release, slow := arrangeSwitchDuringRevival(t)
	current := *f.db.current
	_, selected, _ := f.sidebar.snapshot()
	close(release)
	if err := receive(t, slow, "the slow select's answer"); err != nil {
		t.Fatalf("Select(slow): %v", err)
	}

	// Assert: the last request is current, in WSM and on the roster alike.
	if current != "fast" {
		t.Fatalf("current = %v, want fast", current)
	}
	if !slices.Equal(selected, []ids.WorkspaceID{"slow", "fast"}) {
		t.Fatalf("roster selections = %v, want [slow fast]", selected)
	}
}

func TestARevivalFinishingNeverRestampsTheSelection(t *testing.T) {
	// Arrange.
	f, release, slow := arrangeSwitchDuringRevival(t)

	// Act: the earlier revival completes AFTER the later switch landed.
	close(release)
	if err := receive(t, slow, "the slow select's answer"); err != nil {
		t.Fatalf("Select(slow): %v", err)
	}

	// Assert: nothing re-stamped "slow".
	_, selected, _ := f.sidebar.snapshot()
	if *f.db.current != "fast" {
		t.Fatalf("current = %v, want fast: the revival's completion re-stamped it", *f.db.current)
	}
	if !slices.Equal(selected, []ids.WorkspaceID{"slow", "fast"}) {
		t.Fatalf("roster selections = %v, want [slow fast]", selected)
	}
}

func TestConcurrentSelectsOfAParkedWorkspaceStartOneSession(t *testing.T) {
	// Arrange: the user switches to a parked workspace, and three more selects
	// of it arrive while its bring-up is held at the gate.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.hibernate("w1")
	joined := make(chan ids.WorkspaceID, 3)
	f.verbs.(*verbs).revivals.observeJoin = func(ws ids.WorkspaceID) { joined <- ws }
	entered, release := f.gateStarts(4)
	outcomes := []<-chan error{f.selectAsync(context.Background(), "w1")}
	receive(t, entered, "the leading select's Start")
	for range 3 {
		outcomes = append(outcomes, f.selectAsync(context.Background(), "w1"))
	}
	for range 3 {
		receive(t, joined, "a select joining the in-flight revival")
	}

	// Act.
	close(release)
	for _, outcome := range outcomes {
		if err := receive(t, outcome, "a select's answer"); err != nil {
			t.Fatalf("Select: %v", err)
		}
	}

	// Assert: one bring-up for four selects.
	if !slices.Equal(f.fleet.startCalls, []ids.WorkspaceID{"w1"}) {
		t.Fatalf("starts = %v, want exactly one for w1", f.fleet.startCalls)
	}
}

// ---- a select's revival outlives the select ----

// arrangeCancelledSelect selects parked "w1", holds its DETACHED revival until
// the select's caller has left, and answers the select's error. The revival is
// then released, and the call returns once its flight has finished.
func arrangeCancelledSelect(t *testing.T, f *fixture, startErr error) error {
	t.Helper()
	f.workspace("w1", t.TempDir())
	f.hibernate("w1")
	f.fleet.startErr = startErr
	f.fleet.detachEntered, f.fleet.detachHold = make(chan struct{}), make(chan struct{})
	finished := make(chan ids.WorkspaceID, 1)
	f.verbs.(*verbs).revivals.observeFinish = func(ws ids.WorkspaceID) { finished <- ws }
	ctx, cancel := context.WithCancel(context.Background())
	done := f.selectAsync(ctx, "w1")
	receive(t, f.fleet.detachEntered, "the detached revival")
	cancel()
	err := receive(t, done, "the cancelled select's answer")
	close(f.fleet.detachHold)
	receive(t, finished, "the revival's finish")
	return err
}

// errorRecords answers every ERROR the fixture recorded.
func errorRecords(f *fixture) []dlog.Record {
	var out []dlog.Record
	for _, r := range f.log.logger.Records() {
		if r.Level == dlog.LevelError {
			out = append(out, r)
		}
	}
	return out
}

// TestASelectCancelledMidRevivalLeavesTheWorkspaceRevived is the 18:28:56
// switch: the user moved on while the parked workspace was coming back, and
// the bring-up was torn in half at its session record's write.
func TestASelectCancelledMidRevivalLeavesTheWorkspaceRevived(t *testing.T) {
	// Arrange, Act.
	f := newFixture(t)
	err := arrangeCancelledSelect(t, f, nil)

	// Assert.
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("Select() = %v, want the caller's own cancellation", err)
	}
	if !slices.Equal(f.fleet.started, []ids.WorkspaceID{"w1"}) || f.fleet.startCtxErr != nil {
		t.Fatalf("started = %v (ctx err %v), want w1 started whole on a live context", f.fleet.started, f.fleet.startCtxErr)
	}
	if len(f.topbarParked) != 1 || f.topbarParked[0] {
		t.Fatalf("topbar parked = %v, want the revival to have unparked it", f.topbarParked)
	}
}

// TestASelectCancelledMidRevivalRecordsNoError pins that a caller leaving is
// not a failure.
func TestASelectCancelledMidRevivalRecordsNoError(t *testing.T) {
	// Arrange, Act.
	f := newFixture(t)
	_ = arrangeCancelledSelect(t, f, nil)

	// Assert.
	if errs := errorRecords(f); len(errs) != 0 {
		t.Fatalf("errors = %+v, want none for a caller that left", errs)
	}
}

// TestADetachedRevivalEndedByTheDaemonsExitRecordsNoError covers the fleet's
// lifetime ending under the revival: the daemon leaving, not a failure.
func TestADetachedRevivalEndedByTheDaemonsExitRecordsNoError(t *testing.T) {
	// Arrange, Act.
	f := newFixture(t)
	_ = arrangeCancelledSelect(t, f, context.Canceled)

	// Assert.
	if errs := errorRecords(f); len(errs) != 0 {
		t.Fatalf("errors = %+v, want none for a revival the daemon's exit ended", errs)
	}
}

// TestADetachedRevivalThatFailsIsStillAnError keeps a real failure loud after
// its caller left.
func TestADetachedRevivalThatFailsIsStillAnError(t *testing.T) {
	// Arrange, Act.
	f := newFixture(t)
	_ = arrangeCancelledSelect(t, f, errFake)

	// Assert.
	if errs := errorRecords(f); len(errs) != 1 || errs[0].Message != "the hibernated workspace's session did not come back up" {
		t.Fatalf("errors = %+v, want the failed bring-up at ERROR once", errs)
	}
}

func TestSelectReturnsTheFeedToItsTailOnASwitch(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.workspace("w2", t.TempDir())
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("Select w1: %v", err)
	}

	// Act.
	if err := f.verbs.Select(context.Background(), "w2"); err != nil {
		t.Fatalf("Select w2: %v", err)
	}

	// Assert.
	if !slices.Equal(f.host.tailReturns, []ids.WorkspaceID{"w1", "w2"}) {
		t.Fatalf("tail returns = %v, want [w1 w2]", f.host.tailReturns)
	}
}

func TestReselectingLeavesTheFeedWhereItIs(t *testing.T) {
	// Arrange: a re-selection is Emacs re-asserting, never a switch.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("first Select: %v", err)
	}

	// Act.
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("second Select: %v", err)
	}

	// Assert.
	if !slices.Equal(f.host.tailReturns, []ids.WorkspaceID{"w1"}) {
		t.Fatalf("tail returns = %v, want [w1] (the first select only)", f.host.tailReturns)
	}
}

func TestSelectStartsAnOpenWorkspaceWithNoSession(t *testing.T) {
	// Arrange: neither parked nor live, as a workspace a bring-up left down is.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())

	// Act
	if err := f.verbs.Select(context.Background(), "w1"); err != nil {
		t.Fatalf("Select: %v", err)
	}

	// Assert
	if len(f.fleet.started) != 1 || f.fleet.started[0] != "w1" {
		t.Fatalf("started = %v, want [w1]: a looked-at workspace is never session-less", f.fleet.started)
	}
}

// standingDownStart is the error a revival meets on a daemon standing down,
// spelled as the bring-up spells it: the sentinel wrapped beside the OPEN's
// spawn_failed refusal.
func standingDownStart() error {
	return fmt.Errorf("%w: %w", shimclient.ErrStandingDown, &Refusal{Rpc: "OpenWorkspace", Arm: ArmSpawnFailed})
}

func TestSelectOnADaemonStandingDownAnswersTheStandingDownArm(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.hibernate("w1")
	f.fleet.startErr = standingDownStart()

	// Act.
	err := f.verbs.Select(context.Background(), "w1")

	// Assert: SelectWorkspaceError's own arm, never the open's spawn_failed.
	asRefusal(t, err, ArmStandingDown)
}

func TestSelectOnADaemonStandingDownLeavesTheSelectionStanding(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.hibernate("w1")
	f.fleet.startErr = standingDownStart()

	// Act.
	_ = f.verbs.Select(context.Background(), "w1")

	// Assert: the next daemon serves this selection.
	if f.db.current == nil || *f.db.current != "w1" {
		t.Fatalf("current = %v, want w1", f.db.current)
	}
}
