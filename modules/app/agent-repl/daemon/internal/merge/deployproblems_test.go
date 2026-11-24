package merge

import (
	"context"
	"fmt"
	"slices"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// sinkRecord is a record of the durable log sink warning, the problem the
// deploy checks never kept before (2026-11-16).
var sinkRecord = rec("warn", "agent-repl.elisp-core-log-central-fallback", "elisp.core.log-central-fallback")

// sinkKey is sinkRecord's problem key.
var sinkKey = wsm.ProblemKey{Operation: sinkRecord.Operation, Message: sinkRecord.Message}

// toldWith records the owed check's window as holding GROUPS.
func toldWith(t *testing.T, h *harness, groups ...wsm.WindowGroup) {
	t.Helper()
	if ok, err := h.db.RecordMediationWindow(context.Background(), mediationKey(h), deployStarted, wsm.CheckWindow{Start: deployStarted, Through: deployStarted.Add(time.Minute), Groups: groups}); err != nil || !ok {
		t.Fatalf("RecordMediationWindow = (%v, %v)", ok, err)
	}
}

// sinkGroup is the sink warning's group standing as CLASS.
func sinkGroup(class wsm.WindowClass) wsm.WindowGroup {
	return wsm.WindowGroup{Operation: sinkKey.Operation, Message: sinkKey.Message, Class: class}
}

func TestTheIntakesWindowStartsWhereTheLastCheckRead(t *testing.T) {
	tests := []struct {
		name      string
		arrange   func(t *testing.T, h *harness)
		wantSince time.Time
	}{
		{name: "a repository never checked reads from the deploy's start", wantSince: deployStarted},
		// The span between the last check's read and this landing's start is
		// the production check's (owner ruling, 2026-11-23).
		{name: "a later check reads from its landing's start, not from where the last one read to",
			arrange: func(t *testing.T, h *harness) {
				if ok, err := h.db.RecordMediationWindow(context.Background(), mediationKey(h), deployStarted, wsm.CheckWindow{Start: deployStarted, Through: deployStarted.Add(30 * time.Second)}); err != nil || !ok {
					t.Fatalf("RecordMediationWindow = (%v, %v)", ok, err)
				}
				oweAt(t, h, deployStarted.Add(time.Minute))
			},
			wantSince: deployStarted.Add(time.Minute)},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			if tt.arrange != nil {
				tt.arrange(t, h)
			}
			h.now = deployStarted.Add(2 * time.Minute)
			var sinces []time.Time
			h.logRecords = func(since, until time.Time) ([]LogRecord, error) {
				sinces = append(sinces, since)
				return nil, nil
			}
			q, _, err := h.o.checkedQueueOf(context.Background(), mediationKey(h))
			if err != nil {
				t.Fatal(err)
			}

			// Act
			_, window, err := h.o.intakeBrief(context.Background(), q, *owedCheck(t, h))

			// Assert
			if err != nil || len(sinces) != 2 || !sinces[0].Equal(tt.wantSince) || !window.Start.Equal(tt.wantSince) || !window.Through.Equal(h.now) {
				t.Fatalf("intakeBrief = %v, read from %v, window %+v; want it read from %v through %v", err, sinces, window, tt.wantSince, h.now)
			}
		})
	}
}

func TestAWindowGroupStandsAgainstTheProblemsKept(t *testing.T) {
	fixed := deployStarted
	tests := []struct {
		name     string
		group    FindingGroup
		problems []wsm.DeployProblem
		want     wsm.WindowClass
	}{
		{name: "a group nobody names, unseen before, is new", group: FindingGroup{Operation: "a", Message: "m"}, want: wsm.WindowNew},
		{name: "a group nobody names, seen before, is seen before", group: FindingGroup{Operation: "a", Message: "m", Before: 2}, want: wsm.WindowSeenBefore},
		{name: "a group a problem at work names is owned", group: FindingGroup{Operation: "a", Message: "m"},
			problems: []wsm.DeployProblem{{Key: wsm.ProblemKey{Operation: "a", Message: "m"}}}, want: wsm.WindowOwned},
		{name: "a group a fixed problem names recurred", group: FindingGroup{Operation: "a", Message: "m", Before: 1},
			problems: []wsm.DeployProblem{{Key: wsm.ProblemKey{Operation: "a", Message: "m"}, FixedAt: &fixed}}, want: wsm.WindowRecurred},
		{name: "a recurred group carries its problem's fixes", group: FindingGroup{Operation: "a", Message: "m", Before: 1},
			problems: []wsm.DeployProblem{{Key: wsm.ProblemKey{Operation: "a", Message: "m"}, FixedAt: &fixed, Fixes: 3}}, want: wsm.WindowRecurred},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := classifyFindings([]FindingGroup{tt.group}, tt.problems)

			// Assert
			if len(got) != 1 || got[0].Class != tt.want {
				t.Fatalf("classifyFindings = %+v, want %s", got, tt.want)
			}
			if tt.problems != nil && got[0].Fixes != tt.problems[0].Fixes {
				t.Fatalf("fixes = %d, want the problem's %d", got[0].Fixes, tt.problems[0].Fixes)
			}
		})
	}
}

func TestAnApprovalIsNeverJudgedAgainstTheWindow(t *testing.T) {
	tests := []struct {
		name   string
		groups []wsm.WindowGroup
	}{
		{name: "an empty window is approved"},
		{name: "a window of owned groups is approved", groups: []wsm.WindowGroup{sinkGroup(wsm.WindowOwned)}},
		{name: "a new group does not refuse it", groups: []wsm.WindowGroup{sinkGroup(wsm.WindowNew)}},
		{name: "an old group never taken on does not refuse it", groups: []wsm.WindowGroup{sinkGroup(wsm.WindowSeenBefore)}},
		{name: "a group back after its fix does not refuse it", groups: []wsm.WindowGroup{sinkGroup(wsm.WindowRecurred)}},
		{name: "a group back after more than five fixes does not refuse it", groups: []wsm.WindowGroup{
			{Operation: sinkKey.Operation, Message: sinkKey.Message, Class: wsm.WindowRecurred, Fixes: 6}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			withMediator(t, h)
			toldWith(t, h, tt.groups...)

			// Act
			err := decideTold(h, Decision{Kind: MediationApprove, Summary: "clean"})

			// Assert
			if err != nil || owedCheck(t, h) != nil {
				t.Fatalf("DecideMediation = %v, want the approval to release the check", err)
			}
		})
	}
}

func TestAnApprovalPassesTheGroupsItNamesIgnorable(t *testing.T) {
	other := wsm.WindowGroup{Operation: "daemon.persistentwifi.failover", Message: "no networks", Class: wsm.WindowNew}
	ignoreSink := []DeployProblem{{Statement: "a one-off user-driven error", Evidence: []string{sinkRecord.Raw}}}
	tests := []struct {
		name   string
		groups []wsm.WindowGroup
	}{
		{name: "an approval naming every unowned group ignorable is approved", groups: []wsm.WindowGroup{sinkGroup(wsm.WindowNew)}},
		{name: "an approval naming a recurred group ignorable is approved", groups: []wsm.WindowGroup{sinkGroup(wsm.WindowRecurred)}},
		{name: "an approval naming one group of two is approved", groups: []wsm.WindowGroup{sinkGroup(wsm.WindowNew), other}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			withMediator(t, h)
			toldWith(t, h, tt.groups...)

			// Act
			err := decideTold(h, Decision{Kind: MediationApprove, Summary: "clean", Problems: ignoreSink})

			// Assert
			if err != nil || owedCheck(t, h) != nil {
				t.Fatalf("DecideMediation = %v, want the approval to release the check", err)
			}
			problems, readErr := h.db.DeployProblems(context.Background(), mediationKey(h))
			if readErr != nil || len(problems) != 0 {
				t.Fatalf("problems (%+v, %v), want the ignored group never taken on", problems, readErr)
			}
			logged := h.recordsFor("info", opMediation, "the mediator approves past the groups it judged ignorable, naming each")
			if len(logged) != 1 || fmt.Sprint(logged[0].Context["ignored"]) != fmt.Sprint([]string{"a one-off user-driven error"}) {
				t.Fatalf("records %+v, want the ignored groups named once", logged)
			}
		})
	}
}

func TestADispatchTakesOnTheGroupsItQuotes(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withMediator(t, h)
	toldWith(t, h, sinkGroup(wsm.WindowSeenBefore))

	// Act
	err := dispatched(h, Decision{Problems: []DeployProblem{{Statement: "the sink warning recurs", Evidence: []string{sinkRecord.Raw}}}})

	// Assert
	problems, readErr := h.db.DeployProblems(context.Background(), mediationKey(h))
	if err != nil || readErr != nil || len(problems) != 1 || problems[0].Key != sinkKey || problems[0].FixedAt != nil || problems[0].Findings != "the sink warning recurs" {
		t.Fatalf("dispatch = %v, problems (%+v, %v); want the quoted group taken on", err, problems, readErr)
	}
}

func TestAnEscalationTakesOnTheGroupsNeverTakenOn(t *testing.T) {
	// Arrange
	other := wsm.WindowGroup{Operation: "daemon.persistentwifi.failover", Message: "no networks", Class: wsm.WindowNew}
	h := checked(t)
	owe(t, h)
	withMediator(t, h)
	toldWith(t, h, sinkGroup(wsm.WindowRecurred), other)

	// Act
	err := decideTold(h, Decision{Kind: MediationEscalate, Summary: "the user must grant the helper"})

	// Assert
	problems, readErr := h.db.DeployProblems(context.Background(), mediationKey(h))
	if err != nil || readErr != nil || len(problems) != 2 {
		t.Fatalf("escalate = %v, problems (%+v, %v); want both groups taken on", err, problems, readErr)
	}
	for _, p := range problems {
		if p.FixedAt != nil {
			t.Fatalf("problem %+v, want it open", p)
		}
	}
}

func TestADaemonsEscalationBriefListsTheOldGroups(t *testing.T) {
	// Arrange
	other := wsm.WindowGroup{Operation: "daemon.persistentwifi.failover", Message: "no networks", Class: wsm.WindowNew}
	h := checked(t)
	owe(t, h)
	withMediator(t, h)
	toldWith(t, h, sinkGroup(wsm.WindowRecurred), other)

	// Act
	escalatedByTheDaemon(t, h, "the deploy mediator ended its turn without deciding the deploy")

	// Assert
	old := h.briefValues[len(h.briefValues)-1]["old_problems"]
	if !strings.Contains(old, sinkKey.Operation+": "+sinkKey.Message+" (came back after its fix deployed") || strings.Contains(old, other.Operation) {
		t.Fatalf("old_problems = %q, want the recurred group listed and the new one left to the findings", old)
	}
}

func TestARemediationsDeployMarksItsProblemsFixed(t *testing.T) {
	// Arrange
	h := finishing(t)
	ctx := context.Background()
	if err := h.db.TakeOnDeployProblems(ctx, mediationKey(h), []wsm.ProblemKey{sinkKey}, "the sink warning", deployStarted); err != nil {
		t.Fatal(err)
	}
	if _, err := h.db.BeginRemediationFinish(ctx, wsm.RemediationFinish{Workspace: "ws-fixer", Repo: mediationKey(h), StartedAt: deployStarted}); err != nil {
		t.Fatal(err)
	}
	if _, err := h.db.AdvanceRemediationFinish(ctx, "ws-fixer", wsm.RemediationGating, wsm.RemediationLanding, deployStarted); err != nil {
		t.Fatal(err)
	}
	if _, err := h.db.RecordRemediationLanded(ctx, "ws-fixer", tipAfter, nil, deployStarted); err != nil {
		t.Fatal(err)
	}

	// Act
	err := h.o.remediationDeployed(ctx, mediationKey(h), deployStarted.Add(time.Minute))

	// Assert
	problems, readErr := h.db.DeployProblems(ctx, mediationKey(h))
	if err != nil || readErr != nil || len(problems) != 1 || problems[0].FixedAt == nil {
		t.Fatalf("remediationDeployed = %v, problems (%+v, %v); want the problem fixed", err, problems, readErr)
	}
}

func TestAProblemsEvidenceNamesTheGroupsItQuotes(t *testing.T) {
	tests := []struct {
		name     string
		evidence []string
		want     []wsm.ProblemKey
	}{
		{name: "a record names its group", evidence: []string{sinkRecord.Raw}, want: []wsm.ProblemKey{sinkKey}},
		{name: "a record quoted twice names it once", evidence: []string{sinkRecord.Raw, " " + sinkRecord.Raw}, want: []wsm.ProblemKey{sinkKey}},
		{name: "a line that is no record names nothing", evidence: []string{"0 of 349 units reused"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := problemKeys([]DeployProblem{{Statement: "s", Evidence: tt.evidence}})

			// Assert
			if !slices.Equal(got, tt.want) {
				t.Fatalf("problemKeys = %+v, want %+v", got, tt.want)
			}
		})
	}
}

func TestDecidedProblemsThatCannotBeTakenOnAreAnError(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withMediator(t, h)

	// Act: a problem quoting a record whose group names no operation is
	// refused by the store.
	h.o.takeOnProblems(context.Background(), mediationKey(h), []DeployProblem{{Statement: "s"}}, []wsm.ProblemKey{{Message: "no operation"}}, "unstated", dlog.Context{})

	// Assert
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation, Message: "the decided problems could not be recorded as taken on; the next check finds them untaken"}); !ok {
		t.Fatalf("records = %v, want the failed record at ERROR", h.logs.Records())
	}
}

// TestADecisionIsNeverRefusedForAProblemThatCameBack pins the owner ruling of
// 2026-11-20: a window holding a group that came back after any number of
// its fixes refuses no decision, where a dispatch was once refused past five.
func TestADecisionIsNeverRefusedForAProblemThatCameBack(t *testing.T) {
	recurred := func(fixes int) wsm.WindowGroup {
		return wsm.WindowGroup{Operation: sinkKey.Operation, Message: sinkKey.Message, Class: wsm.WindowRecurred, Fixes: fixes}
	}
	tests := []struct {
		name     string
		group    wsm.WindowGroup
		decision MediationDecision
	}{
		{name: "a dispatch of a group past five fixes", group: recurred(6), decision: MediationDispatch},
		{name: "a dispatch of a group far past five fixes", group: recurred(40), decision: MediationDispatch},
		{name: "an approval of a group past five fixes", group: recurred(6), decision: MediationApprove},
		{name: "an escalation of a group past five fixes", group: recurred(6), decision: MediationEscalate},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			withMediator(t, h)
			toldWith(t, h, tt.group)

			// Act
			err := decideTold(h, Decision{Kind: tt.decision, Summary: "s", Problems: []DeployProblem{{Statement: "s", Evidence: []string{sinkRecord.Raw}}}})

			// Assert
			if err != nil {
				t.Fatalf("DecideMediation = %v, want it accepted", err)
			}
			if tt.decision == MediationDispatch {
				if m := owedCheck(t, h); m == nil || !m.Dispatched {
					t.Fatalf("check %+v, want it dispatched", m)
				}
			}
		})
	}
}
