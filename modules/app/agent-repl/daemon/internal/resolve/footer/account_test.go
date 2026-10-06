package footer

import (
	"errors"
	"sync"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/wsm"
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

// noFiveHourSample is a sample the usage service answered with no five-hour
// window, as it does for an account billed by spend.
func noFiveHourSample(observedAtMs int64) *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_AccountUsage{
			AccountUsage: &conversationv1.SessionAccountUsage{
				ObservedAtMs: observedAtMs,
				Outcome: &conversationv1.SessionAccountUsage_Unavailable{
					Unavailable: &conversationv1.SessionAccountUsageUnavailable{
						Reason: &conversationv1.SessionAccountUsageUnavailable_WindowUnavailable{
							WindowUnavailable: &conversationv1.SessionUsageWindowUnavailable{},
						},
					},
				},
			},
		},
	}
}

func TestASecondWorkspaceOnTheAccountDrawsTheFiguresTheFirstLearns(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	bindOther(t, h)
	h.r.SetAccount(testWS, personalRoot)
	h.r.SetAccount(otherWS, personalRoot)

	// Act
	h.r.OnSessionUpdate(testWS, usageSample(44, 53, instant.UnixMilli()))

	// Assert
	usage := enduringOfWS(t, h, otherWS).GetUsage()
	if usage.GetSession().GetUtilization() != 0.44 || usage.GetWeekly().GetUtilization() != 0.53 {
		t.Fatalf("second workspace's usage = %+v, want the account's 44%% and 53%%", usage)
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
	h.r.OnSessionUpdate(testWS, usageSample(44, 53, instant.UnixMilli()))

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
	h.r.OnSessionUpdate(testWS, usageSample(44, 53, instant.UnixMilli()))
	bindOther(t, h)

	// Act
	h.r.SetAccount(otherWS, personalRoot)

	// Assert
	if got := enduringOfWS(t, h, otherWS).GetUsage().GetSession().GetUtilization(); got != 0.44 {
		t.Fatalf("late workspace's session utilization = %v, want the account's 0.44", got)
	}
}

func TestARateLimitVerdictIsSharedAcrossTheAccount(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	bindOther(t, h)
	h.r.SetAccount(testWS, personalRoot)
	h.r.SetAccount(otherWS, personalRoot)

	// Act
	h.r.OnSessionUpdate(testWS, rateLimitStatus(fiveHourWindow(), 30, 5*time.Hour))

	// Assert
	if got := enduringOfWS(t, h, otherWS).GetUsage().GetSession(); got.GetAllowed() == nil {
		t.Fatalf("second workspace's session allowance = %+v, want the account's allowed verdict", got)
	}
}

func TestAnUnboundWorkspacesFiguresPassToItsRootOnBinding(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.OnSessionUpdate(testWS, usageSample(44, 53, instant.UnixMilli()))
	h.r.SetAccount(testWS, personalRoot)
	bindOther(t, h)

	// Act
	h.r.SetAccount(otherWS, personalRoot)

	// Assert
	if got := enduringOfWS(t, h, otherWS).GetUsage().GetSession().GetUtilization(); got != 0.44 {
		t.Fatalf("second workspace's session utilization = %v, want the figures read before binding", got)
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

func TestASampleWithNoFiveHourWindowDrawsNoAllowance(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetAccount(testWS, workRoot)

	// Act
	h.r.OnSessionUpdate(testWS, noFiveHourSample(instant.UnixMilli()))

	// Assert
	if got := enduringOf(t, h); got.GetNoAllowance() == nil {
		t.Fatalf("enduring = %+v, want no_allowance", got)
	}
}

func TestAServiceThatDidNotAnswerLeavesTheLineUnobserved(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetAccount(testWS, workRoot)

	// Act
	h.r.OnSessionUpdate(testWS, unavailableUsageSample(instant.UnixMilli()))

	// Assert
	if got := enduringOf(t, h); got.GetUnobserved() == nil {
		t.Fatalf("enduring = %+v, want unobserved: a failed read says nothing about the account", got)
	}
}

func TestAFigureReplacesNoAllowance(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetAccount(testWS, workRoot)
	h.r.OnSessionUpdate(testWS, noFiveHourSample(instant.UnixMilli()))

	// Act
	h.r.OnSessionUpdate(testWS, usageSample(12, -1, instant.UnixMilli()+1))

	// Assert
	if got := enduringOf(t, h).GetUsage().GetSession().GetUtilization(); got != 0.12 {
		t.Fatalf("session utilization = %v, want the figure over no_allowance", got)
	}
}

func TestAChangedUsageIsKeptForItsRoot(t *testing.T) {
	// Arrange
	kept := &keptUsage{}
	h := newHarness(t, WithAccountUsageSink(kept.sink))
	connected(h)
	h.r.SetAccount(testWS, personalRoot)

	// Act
	h.r.OnSessionUpdate(testWS, usageSample(44, 53, instant.UnixMilli()))

	// Assert
	got := kept.last(t)
	if got.ConfigDir != personalRoot || got.Session == nil || got.Session.Utilization != 0.44 ||
		got.Session.ResetsAtS != instant.Add(5*time.Hour).Unix() || !got.ObservedAt.Equal(instant) {
		t.Fatalf("kept = %+v, want the root's figures, reset and observation time", got)
	}
}

func TestAnUnboundWorkspacesUsageIsNotKept(t *testing.T) {
	// Arrange
	kept := &keptUsage{}
	h := newHarness(t, WithAccountUsageSink(kept.sink))
	connected(h)

	// Act
	h.r.OnSessionUpdate(testWS, usageSample(44, 53, instant.UnixMilli()))

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
	before.r.OnSessionUpdate(testWS, usageSample(44, 53, instant.UnixMilli()))
	after := newHarness(t, WithAccountUsages([]wsm.AccountUsage{kept.last(t)}))

	// Act
	connected(after)
	after.r.SetAccount(testWS, personalRoot)

	// Assert
	usage := enduringOf(t, after).GetUsage()
	if usage.GetSession().GetUtilization() != 0.44 || usage.GetWeekly().GetUtilization() != 0.53 {
		t.Fatalf("restarted usage = %+v, want the kept 44%% and 53%%", usage)
	}
}

func TestARestartedResolverDrawsTheKeptNoAllowance(t *testing.T) {
	// Arrange
	h := newHarness(t, WithAccountUsages([]wsm.AccountUsage{{ConfigDir: workRoot, ObservedAt: instant, NoAllowance: true}}))

	// Act
	connected(h)
	h.r.SetAccount(testWS, workRoot)

	// Assert
	if got := enduringOf(t, h); got.GetNoAllowance() == nil {
		t.Fatalf("enduring = %+v, want the kept no_allowance", got)
	}
}

func TestAKeptFigureIsShippedWithItsResetInstantSoALapseCanBeDrawn(t *testing.T) {
	// Arrange: a figure kept before a restart whose window has since reset.
	lapsed := instant.Add(-time.Hour).Unix()
	h := newHarness(t, WithAccountUsages([]wsm.AccountUsage{{
		ConfigDir: personalRoot, ObservedAt: instant.Add(-6 * time.Hour),
		Session: &wsm.AllowanceFigures{Utilization: 0.97, ResetsAtS: lapsed},
	}}))

	// Act
	connected(h)
	h.r.SetAccount(testWS, personalRoot)

	// Assert
	if got := enduringOf(t, h).GetUsage().GetSession().GetResetsAtS(); got != lapsed {
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
	h.r.OnSessionUpdate(testWS, usageSample(44, 53, instant.UnixMilli()))

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
	h.r.persistUsage(usageWrite{generation: 2, record: wsm.AccountUsage{ConfigDir: personalRoot, NoAllowance: true}})

	// Act
	h.r.persistUsage(usageWrite{generation: 1, record: wsm.AccountUsage{ConfigDir: personalRoot}})

	// Assert
	if len(kept.records) != 1 || !kept.records[0].NoAllowance {
		t.Fatalf("kept = %+v, want only the newer write", kept.records)
	}
}

func TestTheAccountsArmChangeIsRecordedAtInfo(t *testing.T) {
	// Arrange
	h := newHarness(t)
	connected(h)
	h.r.SetAccount(testWS, workRoot)

	// Act
	h.r.OnSessionUpdate(testWS, noFiveHourSample(instant.UnixMilli()))

	// Assert
	for _, rec := range recordsOf(h.log.Records(), "daemon.footer.account_usage") {
		if rec.Level == dlog.LevelInfo && rec.Context["arm"] == "no_allowance" && rec.Context["previous_arm"] == "unobserved" {
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
		{"waiting", func(_ *testing.T, h *harness) { h.r.OnQuestion(testWS, mainAgent, questionStart("q-1", "which?")) }, "waiting"},
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
