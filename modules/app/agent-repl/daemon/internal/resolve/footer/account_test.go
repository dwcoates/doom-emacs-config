package footer

import (
	"errors"
	"sync"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/health"
	"agent-repld/internal/ids"
	"agent-repld/internal/ratelimittest"
	"agent-repld/internal/shimclient"
	"agent-repld/internal/wsm"
)

// The account roots the tests bind workspaces to.
const (
	personalRoot = "/home/dev/.claude"
	workRoot     = "/home/dev/.claude-work"
)

// otherWS is a second workspace beside testWS.
const otherWS = ids.WorkspaceID("ws-2")

// keptUsage is a sink double: every record the resolver kept, in order.
type keptUsage struct {
	mu      sync.Mutex
	records []wsm.AccountUsage
	err     error
}

func (k *keptUsage) sink(usage wsm.AccountUsage) error {
	k.mu.Lock()
	defer k.mu.Unlock()
	k.records = append(k.records, usage)
	return k.err
}

func (k *keptUsage) last(t *testing.T) wsm.AccountUsage {
	t.Helper()
	k.mu.Lock()
	defer k.mu.Unlock()
	if len(k.records) == 0 {
		t.Fatal("no account usage was kept")
	}
	return k.records[len(k.records)-1]
}

// bindOther binds the second workspace's directory and marks it connected.
func bindOther(t *testing.T, h *harness) {
	t.Helper()
	if err := h.r.SetWorkspaceDir(otherWS, t.TempDir()); err != nil {
		t.Fatalf("SetWorkspaceDir: %v", err)
	}
	h.r.SetParticipants(otherWS, true, true)
	h.r.OnLink(otherWS, shimclient.LinkConnected)
}

// enduringOfWS is the workspace's idle enduring line.
func enduringOfWS(t *testing.T, h *harness, ws ids.WorkspaceID) *frontendv1.FooterActivityEnduring {
	t.Helper()
	view, ok := h.r.Topic(ws).Latest()
	if !ok {
		t.Fatalf("no footer view was published for %s", ws)
	}
	return view.GetStrip().GetStatus().GetIdle().GetActivity().GetUnpinned().GetEnduring()
}

func TestASecondWorkspaceOnTheAccountDrawsTheFiguresTheFirstLearns(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	bindOther(t, h)
	h.r.SetAccount(testWS, personalRoot)
	h.r.SetAccount(otherWS, personalRoot)

	// Act
	fileWeekly(h, 53)

	// Assert
	usage := enduringOfWS(t, h, otherWS).GetUsage()
	if usage.GetWeekly().GetOverall().GetUtilization() != 0.53 {
		t.Fatalf("second workspace's usage = %+v, want the account's 53%%", usage)
	}
}

func TestAWorkspaceOnAnotherAccountDoesNotDrawTheFigures(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	bindOther(t, h)
	h.r.SetAccount(testWS, personalRoot)
	h.r.SetAccount(otherWS, workRoot)

	// Act
	fileWeekly(h, 53)

	// Assert
	if got := enduringOfWS(t, h, otherWS); got.GetUnobserved() == nil {
		t.Fatalf("other account's enduring = %+v, want unobserved", got)
	}
}

func TestAWorkspaceBoundAfterTheFiguresDrawsThemAtOnce(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetAccount(testWS, personalRoot)
	fileWeekly(h, 53)
	bindOther(t, h)

	// Act
	h.r.SetAccount(otherWS, personalRoot)

	// Assert
	if got := enduringOfWS(t, h, otherWS).GetUsage().GetWeekly().GetOverall().GetUtilization(); got != 0.53 {
		t.Fatalf("late workspace's weekly utilization = %v, want the account's 0.53", got)
	}
}

func TestARateLimitVerdictIsSharedAcrossTheAccount(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	bindOther(t, h)
	h.r.SetAccount(testWS, personalRoot)
	h.r.SetAccount(otherWS, personalRoot)
	h.r.OnSessionUpdate(testWS, withVerdict(rateLimitStatus(ratelimittest.SevenDay(), 20, week), ratelimittest.Statusless))

	// Act
	h.r.OnSessionUpdate(testWS, rateLimitStatus(ratelimittest.SevenDay(), 30, 5*time.Hour))

	// Assert
	if got := enduringOfWS(t, h, otherWS).GetUsage().GetWeekly().GetOverall(); got.GetAllowed() == nil {
		t.Fatalf("second workspace's weekly allowance = %+v, want the account's allowed verdict", got)
	}
}

func TestAnUnboundWorkspacesFiguresPassToItsRootOnBinding(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	fileWeekly(h, 53)
	h.r.SetAccount(testWS, personalRoot)
	bindOther(t, h)

	// Act
	h.r.SetAccount(otherWS, personalRoot)

	// Assert
	if got := enduringOfWS(t, h, otherWS).GetUsage().GetWeekly().GetOverall().GetUtilization(); got != 0.53 {
		t.Fatalf("second workspace's weekly utilization = %v, want the figures read before binding", got)
	}
}

func TestBindingToAnUnnamedRootIsRecordedAsAnError(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetAccount(testWS, "")

	// Assert
	for _, rec := range recordsOf(h.log.Records(), "daemon.footer.set_account") {
		if rec.Level == dlog.LevelError {
			return
		}
	}
	t.Fatalf("no error record for an unnamed root in %+v", h.log.Records())
}

func TestAWorkspaceOnANeverObservedAccountDrawsUnobserved(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)

	// Act
	h.r.SetAccount(testWS, workRoot)

	// Assert
	if got := enduringOf(t, h); got.GetUnobserved() == nil {
		t.Fatalf("enduring = %+v, want unobserved for an account never seen", got)
	}
}

func TestAFiveHourEventAloneLeavesTheLineUnobserved(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetAccount(testWS, workRoot)

	// Act
	h.r.OnSessionUpdate(testWS, rateLimitStatus(ratelimittest.FiveHour(), 50, 5*time.Hour))

	// Assert
	if got := enduringOf(t, h); got.GetUnobserved() == nil {
		t.Fatalf("enduring = %+v, want unobserved: the five-hour window is not drawn", got)
	}
}

func TestAChangedUsageIsKeptForItsRoot(t *testing.T) {
	// Arrange
	kept := &keptUsage{}
	h := newHarness(t, WithAccountUsageSink(kept.sink))
	connected(h)
	h.r.SetAccount(testWS, personalRoot)

	// Act
	fileWeekly(h, 53)

	// Assert
	got := kept.last(t)
	if got.ConfigDir != personalRoot || got.Weekly == nil || got.Weekly.Utilization != 0.53 ||
		got.Weekly.ResetsAtS != instant.Add(week).Unix() || !got.ObservedAt.Equal(instant) {
		t.Fatalf("kept = %+v, want the root's figures, reset and observation time", got)
	}
}

func TestAnUnboundWorkspacesUsageIsNotKept(t *testing.T) {
	// Arrange
	kept := &keptUsage{}
	h := newHarness(t, WithAccountUsageSink(kept.sink))
	connected(h)

	// Act
	fileWeekly(h, 53)

	// Assert
	if len(kept.records) != 0 {
		t.Fatalf("kept = %+v, want nothing kept under no root", kept.records)
	}
}

func TestARestartedResolverDrawsTheKeptFigures(t *testing.T) {
	// Arrange
	kept := &keptUsage{}
	before := newHarness(t, WithAccountUsageSink(kept.sink))
	connected(before)
	before.r.SetAccount(testWS, personalRoot)
	fileWeekly(before, 53)
	after := newHarness(t, WithAccountUsages([]wsm.AccountUsage{kept.last(t)}))

	// Act
	connected(after)
	after.r.SetAccount(testWS, personalRoot)

	// Assert
	usage := enduringOf(t, after).GetUsage()
	if usage.GetWeekly().GetOverall().GetUtilization() != 0.53 {
		t.Fatalf("restarted usage = %+v, want the kept 53%%", usage)
	}
}

func TestARestartedResolverDrawsTheKeptModelWindowsAndVerdictScope(t *testing.T) {
	// Arrange: an opus rejection of a week that resets in an hour, kept.
	kept := &keptUsage{}
	before := newHarness(t, WithAccountUsageSink(kept.sink))
	connected(before)
	before.r.SetAccount(testWS, personalRoot)
	fileWeeklyWindows(before, 53, 12, 30)
	before.r.OnSessionUpdate(testWS, withVerdict(rateLimitStatus(ratelimittest.SevenDayOpus(), 100, time.Hour), ratelimittest.Rejected))
	record := kept.last(t)
	after := newHarness(t, WithAccountUsages([]wsm.AccountUsage{record}))

	// Act: the restarted resolver draws after that hour has passed.
	after.clock.Advance(2 * time.Hour)
	connected(after)
	after.r.SetAccount(testWS, personalRoot)

	// Assert
	if record.WeeklyOpus == nil || record.WeeklyOpus.VerdictResetsAtS != instant.Add(time.Hour).Unix() {
		t.Fatalf("kept opus window = %+v, want its verdict scoped to the hour's reset", record.WeeklyOpus)
	}
	models := enduringOf(t, after).GetUsage().GetWeekly().GetModels()
	if models.GetOpus().GetUtilization() != 1 || models.GetSonnet().GetUtilization() != 0.3 || models.GetOpus().GetStatus() != nil {
		t.Fatalf("restarted models = %+v, want the kept 100%% (the event's) and 30%% with the lapsed verdict undrawn", models)
	}
}

func TestAKeptFigureIsShippedWithItsResetInstantSoALapseCanBeDrawn(t *testing.T) {
	// Arrange: a figure kept before a restart whose window has since reset.
	lapsed := instant.Add(-time.Hour).Unix()
	h := newHarness(t, WithAccountUsages([]wsm.AccountUsage{{
		ConfigDir: personalRoot, ObservedAt: instant.Add(-6 * time.Hour),
		Weekly: &wsm.AllowanceFigures{Utilization: 0.97, ResetsAtS: lapsed},
	}}))

	// Act
	connected(h)
	h.r.SetAccount(testWS, personalRoot)

	// Assert
	if got := enduringOf(t, h).GetUsage().GetWeekly().GetOverall().GetResetsAtS(); got != lapsed {
		t.Fatalf("resets_at_s = %d, want the kept instant %d the client lapses the figure by", got, lapsed)
	}
}

func TestNewRefusesKeptUsageForNoRoot(t *testing.T) {
	// Arrange, Act
	_, err := newResolver(testColors(), dlog.NewTestSurfaces(), WithAccountUsages([]wsm.AccountUsage{{}}))

	// Assert
	if err == nil {
		t.Fatal("newResolver accepted kept usage for no account root")
	}
}

func TestAFailedKeepIsRecordedAtError(t *testing.T) {
	// Arrange
	kept := &keptUsage{err: errors.New("disk full")}
	h := newHarness(t, WithAccountUsageSink(kept.sink))
	connected(h)
	h.r.SetAccount(testWS, personalRoot)

	// Act
	fileWeekly(h, 53)

	// Assert
	for _, rec := range recordsOf(h.log.Records(), "daemon.footer.account_usage_persist") {
		if rec.Level == dlog.LevelError {
			return
		}
	}
	t.Fatalf("no error record for a failed keep in %+v", h.log.Records())
}

func TestAnOlderKeepNeverOverwritesANewerOne(t *testing.T) {
	// Arrange
	kept := &keptUsage{}
	h := newHarness(t, WithAccountUsageSink(kept.sink))
	h.r.persistUsage(usageWrite{generation: 2, record: wsm.AccountUsage{ConfigDir: personalRoot, Weekly: &wsm.AllowanceFigures{Utilization: 0.5}}})

	// Act
	h.r.persistUsage(usageWrite{generation: 1, record: wsm.AccountUsage{ConfigDir: personalRoot}})

	// Assert
	if len(kept.records) != 1 || kept.records[0].Weekly == nil {
		t.Fatalf("kept = %+v, want only the newer write", kept.records)
	}
}

func TestTheAccountsArmChangeIsRecordedAtInfo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetAccount(testWS, workRoot)

	// Act
	fileWeekly(h, 53)

	// Assert
	for _, rec := range recordsOf(h.log.Records(), "daemon.footer.account_usage") {
		if rec.Level == dlog.LevelInfo && rec.Context["arm"] == "usage" && rec.Context["previous_arm"] == "unobserved" {
			return
		}
	}
	t.Fatalf("no info record of the arm change in %+v", h.log.Records())
}

// THE ACTIVITY CELL IS NEVER EMPTY (owner ruling, 2026-10-06): whatever the
// status, the cell carries a salient line, a transient, or an enduring line
// whose arm is set. Every arm the footer draws is reached here, on an account
// that was never observed — the case that once drew nothing.
func TestTheActivityCellIsNeverEmptyUnderAnyStatus(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, h *harness)
		arm     string
	}{
		{"idle", func(*testing.T, *harness) {}, "idle"},
		{"working", func(_ *testing.T, h *harness) { h.r.SetTurn(testWS, &TurnStarted{At: instant}) }, "working"},
		{"interrupted", func(_ *testing.T, h *harness) {
			turn := testTurnID
			h.r.SetTurn(testWS, &TurnStarted{At: instant})
			h.r.OnAgentTerminal(testWS, mainAgent, &turn, interruptedByUserStop(), nil)
		}, "interrupted"},
		{"loading", func(_ *testing.T, h *harness) {
			h.r.SetTurn(testWS, &TurnStarted{At: instant})
			h.r.OnActivity(testWS, mainAgent, memoryInjection("CLAUDE.md"))
		}, "loading"},
		{"waiting", func(_ *testing.T, h *harness) {
			h.r.SetColdGate(testWS, ColdGate{Standing: true, Cost: FiguredLine{Lead: "cold"}})
		}, "waiting"},
		{"permission", func(_ *testing.T, h *harness) {
			h.r.OnPermission(testWS, mainAgent, permissionStart("p-1", "Claude wants to run make"))
		}, "permission"},
		{"question", func(_ *testing.T, h *harness) { h.r.OnQuestion(testWS, mainAgent, questionStart("q-1", "which?")) }, "question"},
		{"merging", func(_ *testing.T, h *harness) { h.r.SetMerge(testWS, MergeFacts{State: "merging", Step: StepTesting}) }, "merging"},
		{"merge_failed", func(_ *testing.T, h *harness) {
			h.r.SetMerge(testWS, MergeFacts{State: "failed", FailedArea: FailedConflicts})
		}, "merge_failed"},
		{"merged", func(_ *testing.T, h *harness) { h.r.SetMerge(testWS, MergeFacts{State: "merged"}) }, "merged"},
		{"background", func(_ *testing.T, h *harness) {
			h.r.OnLiveWorkChanged(testWS, liveSet(nil, []string{"shell-1"}, nil))
		}, "background"},
		{"degraded", func(_ *testing.T, h *harness) { h.r.SetStateUnreported(testWS, true) }, "degraded"},
		{"vendor_fault", func(_ *testing.T, h *harness) { h.r.OnSessionUpdate(testWS, rejectedFiveHour()) }, "vendor_fault"},
		{"agent_repl_fault", func(_ *testing.T, h *harness) { h.r.OnLink(testWS, shimclient.LinkRedialing) }, "agent_repl_fault"},
		{"network_fault", func(t *testing.T, h *harness) {
			h.r.OpenFault(testWS, faultOf(t, "f-1", health.KindNetworkUnreachable, false))
		}, "network_fault"},
		{"closing", func(_ *testing.T, h *harness) {
			h.r.SetClosing(testWS, &CloseBlocked{Reason: "turn_in_flight", Detail: "a turn is in flight"})
		}, "closing"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			connected(h)
			h.r.SetAccount(testWS, workRoot)

			// Act
			tt.arrange(t, h)

			// Assert
			status := h.view(t).GetStrip().GetStatus()
			if got := statusName(status); got != tt.arm {
				t.Fatalf("status = %q, want %q", got, tt.arm)
			}
			line := activityLineOf(status)
			if line.tier == "" {
				t.Fatalf("the %s activity cell carries no line at all", tt.arm)
			}
			if line.tier == "enduring" && unpinnedOf(status).GetEnduring().GetLine() == nil {
				t.Fatalf("the %s activity cell's enduring line has no arm: an empty cell", tt.arm)
			}
		})
	}
}
