package merge

import (
	"context"
	"errors"
	"slices"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
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
		{name: "a later check reads from where the last one read to",
			arrange: func(t *testing.T, h *harness) {
				if ok, err := h.db.RecordMediationWindow(context.Background(), mediationKey(h), deployStarted, wsm.CheckWindow{Start: deployStarted, Through: deployStarted.Add(30 * time.Second)}); err != nil || !ok {
					t.Fatalf("RecordMediationWindow = (%v, %v)", ok, err)
				}
				oweAt(t, h, deployStarted.Add(time.Minute))
			},
			wantSince: deployStarted.Add(30 * time.Second)},
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
			problems: []wsm.DeployProblem{{Key: wsm.ProblemKey{Operation: "a", Message: "m"}, Owner: "ws-fixer"}}, want: wsm.WindowOwned},
		{name: "a group a fixed problem names recurred", group: FindingGroup{Operation: "a", Message: "m", Before: 1},
			problems: []wsm.DeployProblem{{Key: wsm.ProblemKey{Operation: "a", Message: "m"}, Owner: "ws-fixer", FixedAt: &fixed}}, want: wsm.WindowRecurred},
		{name: "a recurred group carries its problem's fixes", group: FindingGroup{Operation: "a", Message: "m", Before: 1},
			problems: []wsm.DeployProblem{{Key: wsm.ProblemKey{Operation: "a", Message: "m"}, Owner: "ws-fixer", FixedAt: &fixed, Fixes: 3}}, want: wsm.WindowRecurred},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := classifyFindings([]FindingGroup{tt.group}, tt.problems, func(ws ids.WorkspaceID) (string, error) { return "name-of-" + string(ws), nil })

			// Assert
			if err != nil || len(got) != 1 || got[0].Class != tt.want {
				t.Fatalf("classifyFindings = (%+v, %v), want %s", got, err, tt.want)
			}
			if tt.problems != nil && got[0].Owner != "name-of-ws-fixer" {
				t.Fatalf("owner = %q, want the problem's owner by name", got[0].Owner)
			}
			if tt.problems != nil && got[0].Fixes != tt.problems[0].Fixes {
				t.Fatalf("fixes = %d, want the problem's %d", got[0].Fixes, tt.problems[0].Fixes)
			}
		})
	}
}

func TestAnOwnerWhoseNameCannotBeReadFailsTheClassing(t *testing.T) {
	// Arrange
	problems := []wsm.DeployProblem{{Key: wsm.ProblemKey{Operation: "a", Message: "m"}, Owner: "ws-gone"}}

	// Act
	_, err := classifyFindings([]FindingGroup{{Operation: "a", Message: "m"}}, problems, func(ids.WorkspaceID) (string, error) { return "", errors.New("no such workspace") })

	// Assert
	if err == nil {
		t.Fatal("classifyFindings drew an owner it could not name")
	}
}

func TestAnApprovalIsJudgedAgainstTheWindowsOwners(t *testing.T) {
	tests := []struct {
		name     string
		groups   []wsm.WindowGroup
		wantArm  string
		wantKept []string
	}{
		{name: "a window of owned groups is approved", groups: []wsm.WindowGroup{sinkGroup(wsm.WindowOwned)}},
		{name: "an empty window is approved"},
		{name: "a new group refuses it", groups: []wsm.WindowGroup{sinkGroup(wsm.WindowNew)}, wantArm: ArmProblemsUnowned,
			wantKept: []string{sinkKey.Operation + ": " + sinkKey.Message + " (new)"}},
		{name: "an old group nobody owns refuses it", groups: []wsm.WindowGroup{sinkGroup(wsm.WindowSeenBefore)}, wantArm: ArmProblemsUnowned,
			wantKept: []string{sinkKey.Operation + ": " + sinkKey.Message + " (seen_before)"}},
		{name: "a group back after its fix refuses it", groups: []wsm.WindowGroup{sinkGroup(wsm.WindowRecurred)}, wantArm: ArmProblemsUnowned,
			wantKept: []string{sinkKey.Operation + ": " + sinkKey.Message + " (recurred)"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			withIntake(t, h, deployStarted)
			toldWith(t, h, tt.groups...)

			// Act
			err := h.o.DecideMediation(context.Background(), "ws-intake", Decision{Kind: MediationApprove, Summary: "clean"})

			// Assert
			if tt.wantArm == "" {
				if err != nil || owedCheck(t, h) != nil {
					t.Fatalf("DecideMediation = %v, want the approval to release the check", err)
				}
				return
			}
			var refused *RefusalError
			if !errors.As(err, &refused) || refused.Arm != tt.wantArm || !slices.Equal(refused.Fields["groups"].([]string), tt.wantKept) {
				t.Fatalf("DecideMediation = %v, want the %s refusal naming %v", err, tt.wantArm, tt.wantKept)
			}
			if owedCheck(t, h) == nil || len(h.checks.stopped) != 0 {
				t.Fatalf("stopped %v, want the check still owed and the intake left at work", h.checks.stopped)
			}
		})
	}
}

func TestADispatchMakesItsRemediationTheOwnerOfTheGroupsItQuotes(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withIntake(t, h, deployStarted)
	toldWith(t, h, sinkGroup(wsm.WindowSeenBefore))

	// Act
	err := dispatched(h, Decision{Problems: []DeployProblem{{Statement: "the sink warning recurs", Evidence: []string{sinkRecord.Raw}}}})

	// Assert
	problems, readErr := h.db.DeployProblems(context.Background(), mediationKey(h))
	if err != nil || readErr != nil || len(problems) != 1 || problems[0].Key != sinkKey || problems[0].Owner != "ws-fixer" || problems[0].Findings != "the sink warning recurs" {
		t.Fatalf("dispatch = %v, problems (%+v, %v); want the remediation owning the quoted group", err, problems, readErr)
	}
}

func TestAnEscalationOwnsTheGroupsNobodyOwnedAndIsToldTheOldOnes(t *testing.T) {
	// Arrange
	other := wsm.WindowGroup{Operation: "daemon.persistentwifi.failover", Message: "no networks", Class: wsm.WindowNew}
	h := checked(t)
	owe(t, h)
	withIntake(t, h, deployStarted)
	toldWith(t, h, sinkGroup(wsm.WindowRecurred), other)

	// Act
	err := h.o.DecideMediation(context.Background(), "ws-intake", Decision{Kind: MediationEscalate, Summary: "the user must grant the helper"})

	// Assert
	problems, readErr := h.db.DeployProblems(context.Background(), mediationKey(h))
	if err != nil || readErr != nil || len(problems) != 2 {
		t.Fatalf("escalate = %v, problems (%+v, %v); want both unowned groups owned", err, problems, readErr)
	}
	for _, p := range problems {
		if p.Owner != "ws-fixer" {
			t.Fatalf("problem %+v, want the escalation owning it", p)
		}
	}
	old := h.briefValues[len(h.briefValues)-1]["old_problems"]
	if !strings.Contains(old, sinkKey.Operation+": "+sinkKey.Message+" (came back after its fix deployed") || strings.Contains(old, other.Operation) {
		t.Fatalf("old_problems = %q, want the recurred group listed and the new one left to the findings", old)
	}
}

func TestARemediationsDeployMarksItsProblemsFixed(t *testing.T) {
	// Arrange
	h := finishing(t)
	ctx := context.Background()
	if err := h.db.OwnDeployProblems(ctx, mediationKey(h), []wsm.ProblemKey{sinkKey}, "ws-fixer", "the sink warning", deployStarted); err != nil {
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

func TestDecidedProblemsThatCannotBeRecordedAsOwnedAreAnError(t *testing.T) {
	// Arrange
	h := checked(t)
	owe(t, h)
	withIntake(t, h, deployStarted)

	// Act: a problem quoting a record whose group names no operation is
	// refused by the store.
	h.o.ownProblems(context.Background(), mediationKey(h), "ws-fixer", []DeployProblem{{Statement: "s"}}, []wsm.ProblemKey{{Message: "no operation"}}, "unstated", dlog.Context{})

	// Assert
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opMediation, Message: "the decided problems could not be recorded as owned; the next check finds them unowned"}); !ok {
		t.Fatalf("records = %v, want the failed ownership at ERROR", h.logs.Records())
	}
}

func TestAProblemPastItsFixesIsOnlyEscalated(t *testing.T) {
	recurred := func(fixes int) wsm.WindowGroup {
		return wsm.WindowGroup{Operation: sinkKey.Operation, Message: sinkKey.Message, Class: wsm.WindowRecurred, Fixes: fixes}
	}
	tests := []struct {
		name     string
		group    wsm.WindowGroup
		decision MediationDecision
		wantArm  string
	}{
		{name: "a dispatch of a group past two fixes is refused", group: recurred(3), decision: MediationDispatch, wantArm: ArmProblemsRecurring},
		{name: "an approval of a group past two fixes is refused", group: recurred(3), decision: MediationApprove, wantArm: ArmProblemsRecurring},
		{name: "an escalation of a group past two fixes is accepted", group: recurred(3), decision: MediationEscalate},
		{name: "a dispatch of a group after two fixes is accepted", group: recurred(2), decision: MediationDispatch},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := checked(t)
			owe(t, h)
			withIntake(t, h, deployStarted)
			toldWith(t, h, tt.group)

			// Act
			err := h.o.DecideMediation(context.Background(), "ws-intake", Decision{Kind: tt.decision, Summary: "s", Problems: []DeployProblem{{Statement: "s", Evidence: []string{sinkRecord.Raw}}}})

			// Assert
			var refused *RefusalError
			if tt.wantArm == "" {
				if errors.As(err, &refused) {
					t.Fatalf("DecideMediation = %v, want it accepted", err)
				}
				return
			}
			if !errors.As(err, &refused) || refused.Arm != tt.wantArm {
				t.Fatalf("DecideMediation = %v, want the %s refusal", err, tt.wantArm)
			}
			if groups, _ := refused.Fields["groups"].([]string); len(groups) != 1 || !strings.Contains(groups[0], "(3 fixes)") {
				t.Fatalf("refusal fields %v, want the group named with its fixes", refused.Fields)
			}
			if m := owedCheck(t, h); m == nil || m.Dispatched || m.Escalated() {
				t.Fatalf("check %+v, want it owed and undecided", m)
			}
		})
	}
}

func TestTheBriefMarksAProblemPastItsFixes(t *testing.T) {
	tests := []struct {
		name  string
		fixes int
		want  bool
	}{
		{name: "after three fixes it is marked", fixes: 3, want: true},
		{name: "after two it is not", fixes: 2},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := RenderFindings([]FindingGroup{{Operation: "a", Message: "m", Level: "error", Count: 1, Class: wsm.WindowRecurred, Owner: "remedy", Fixes: tt.fixes}})

			// Assert
			if marked := strings.Contains(got, "only an escalation is accepted"); marked != tt.want {
				t.Fatalf("brief %q, want marked %v", got, tt.want)
			}
		})
	}
}
