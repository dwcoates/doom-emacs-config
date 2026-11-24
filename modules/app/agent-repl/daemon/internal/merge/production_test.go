package merge

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"slices"
	"strings"
	"testing"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	repoconfigv1 "agentrepl/proto/repoconfig/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/logsites"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/wsm"
)

// priorDeploy is when the deploy before the production harness's began: a
// repository no check read starts its span there.
var priorDeploy = deployStarted.Add(-2 * time.Hour)

// production is a harness whose repository checks its production and not
// its deploys, the deploy before deployStarted recorded, and every
// production brief registered.
func production(t *testing.T) *harness {
	t.Helper()
	h := newHarness(t)
	h.config.ProductionChecks = &repoconfigv1.ProductionChecks{}
	h.briefs[BriefProductionIntake] = []string{"span_since", "span_until", "new_sites", "refired_sites", "site_file", "baseline", "open_children", "metrics_window"}
	h.briefs[BriefProductionIntakeResumed] = []string{"span_since", "span_until"}
	h.briefs[BriefProductionChild] = []string{"kind", "findings", "siblings"}
	h.briefs[BriefProductionChildFolded] = []string{"folded", "branch", "dir"}
	if _, err := h.db.checks.RecordLogSiteDeploy(context.Background(), func() time.Time { return priorDeploy }); err != nil {
		t.Fatal(err)
	}
	h.now = deployStarted.Add(time.Minute)
	return h
}

// deployEnds owes the production span of the deploy at deployStarted and
// ends that deploy as ENDING.
func deployEnds(t *testing.T, h *harness, ending wsm.DeployEnding) {
	t.Helper()
	owe(t, h)
	if err := h.o.NoteDeployEnding(context.Background(), deployStarted, ending); err != nil {
		t.Fatalf("NoteDeployEnding: %v", err)
	}
}

// productionTellings are the production briefs submitted to the mediator.
func productionTellings(h *harness) []promptqueue.Submission {
	return submissionsTo(h, "ws-fixer", BriefProductionIntake+" ")
}

// site is a tally site of KEY at LEVEL, last seen at LAST.
func site(key, level string, last time.Time) logsites.Site {
	return logsites.Site{Key: key, Level: level, Template: "template " + key, Runtime: "daemon", Operation: "daemon." + key, Count: 2, FirstSeen: last.Add(-time.Minute), LastSeen: last, SampleMessage: "sample " + key}
}

func TestALandingOwesTheSpanBeforeItAndItsEndMakesItDue(t *testing.T) {
	cases := []struct {
		name   string
		ending wsm.DeployEnding
		// wantDue is whether the span is to be told after the ending.
		wantDue bool
	}{
		{name: "a finished deploy makes its span due", ending: wsm.DeployFinished, wantDue: true},
		{name: "a deploy that stopped short makes its span due", ending: wsm.DeployStoppedShort, wantDue: true},
		{name: "a crash drops its span, which runs on", ending: wsm.DaemonCrashed},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := production(t)

			// Act
			deployEnds(t, h, tc.ending)

			// Assert
			c, err := h.db.ProductionCheckOf(context.Background(), mediationKey(h))
			if err != nil || (c != nil) != tc.wantDue {
				t.Fatalf("ProductionCheckOf = (%+v, %v), want due %v", c, err, tc.wantDue)
			}
			if tc.wantDue && (!c.Since.Equal(priorDeploy) || !c.Until.Equal(deployStarted)) {
				t.Fatalf("span = %v to %v, want %v to %v", c.Since, c.Until, priorDeploy, deployStarted)
			}
		})
	}
}

func TestAProductionPassTheDaemonsExitCutIsNoFailure(t *testing.T) {
	cases := []struct {
		name      string
		cut       bool
		err       error
		wantLevel string
	}{
		{name: "a pass the daemon's exit cut is recorded at info", cut: true, err: context.Canceled, wantLevel: "info"},
		{name: "a pass that fails while the daemon runs is an error", err: errors.New("database is closed"), wantLevel: "error"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := production(t)
			h.db.reposErr = tc.err
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()
			if tc.cut {
				cancel()
			}

			// Act
			h.o.productionPass(ctx)

			// Assert
			rec, ok := h.recordFor(tc.wantLevel, opProduction)
			if !ok || !strings.Contains(fmt.Sprint(rec.Context), tc.err.Error()) {
				t.Fatalf("records = %v, want one at %s under %s carrying the cause", h.logs.Records(), tc.wantLevel, opProduction)
			}
			if tc.cut {
				if _, failed := h.recordFor("error", opProduction); failed {
					t.Fatalf("records = %v, want no error for a cut pass", h.logs.Records())
				}
			}
		})
	}
}

func TestARepositoryWithoutProductionChecksOwesNoSpan(t *testing.T) {
	// Arrange: the deploy checks alone are on.
	h := checked(t)
	if _, err := h.db.checks.RecordLogSiteDeploy(context.Background(), func() time.Time { return priorDeploy }); err != nil {
		t.Fatal(err)
	}

	// Act
	deployEnds(t, h, wsm.DeployFinished)

	// Assert
	if c, err := h.db.ProductionCheckOf(context.Background(), mediationKey(h)); err != nil || c != nil {
		t.Fatalf("ProductionCheckOf = (%+v, %v), want none", c, err)
	}
}

func TestADueSpanIsToldToTheProductionMediatorWithItsSiteFile(t *testing.T) {
	// Arrange: one site new to the span.
	h := production(t)
	deployEnds(t, h, wsm.DeployFinished)
	h.sites.answers[priorDeploy] = logsites.Answer{Sites: []logsites.Site{site("aaaaaaaaaaaa", "error", deployStarted.Add(-time.Minute))}}
	h.queue.onSubmit = func(sub promptqueue.Submission) {
		if strings.HasPrefix(submittedText(sub), BriefProductionIntake+" ") {
			if _, err := h.o.DecideProduction(context.Background(), "ws-fixer", ProductionDecisionRequest{Summary: "nothing new"}); err != nil {
				t.Errorf("DecideProduction: %v", err)
			}
		}
	}

	// Act
	h.o.Mediate(context.Background())

	// Assert
	told := productionTellings(h)
	if len(told) != 1 || told[0].Origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_PRODUCTION_MEDIATION {
		t.Fatalf("production tellings = %+v, want one with the production origin", told)
	}
	path := ProductionSitePath(LogsDir(h.stateDir), deployStarted)
	file, err := os.ReadFile(path)
	if err != nil || !strings.Contains(string(file), "aaaaaaaaaaaa") || !strings.Contains(submittedText(told[0]), path) {
		t.Fatalf("site file %s = (%q, %v), brief %q; want the new site listed and named", path, file, err, submittedText(told[0]))
	}
	if !slices.Equal(h.checks.made, []string{"production-mediator"}) {
		t.Fatalf("made %v, want the production mediator", h.checks.made)
	}
	if c, err := h.db.ProductionCheckOf(context.Background(), mediationKey(h)); err != nil || c != nil {
		t.Fatalf("after the decision the check to decide = (%+v, %v), want none", c, err)
	}
}

func TestAnUndecidedProductionTurnIsResumedOnceThenRecordedUnanswered(t *testing.T) {
	// Arrange: the mediator decides nothing in any turn.
	h := production(t)
	deployEnds(t, h, wsm.DeployFinished)

	// Act
	h.o.Mediate(context.Background())

	// Assert
	resumed := submissionsTo(h, "ws-fixer", BriefProductionIntakeResumed+" ")
	if len(productionTellings(h)) != 1 || len(resumed) != 1 {
		t.Fatalf("tellings %d, resumptions %d; want one of each", len(productionTellings(h)), len(resumed))
	}
	if _, ok := h.recordFor("error", opProduction); !ok {
		t.Fatalf("records = %v, want the unanswered span at ERROR", h.logs.Records())
	}
	if c, err := h.db.ProductionCheckOf(context.Background(), mediationKey(h)); err != nil || c != nil {
		t.Fatalf("the check to decide = (%+v, %v), want none: the span is recorded unanswered", c, err)
	}
	if !slices.Contains(h.meter.got, "production_check:unanswered:0") {
		t.Fatalf("metrics = %v, want the unanswered check counted", h.meter.got)
	}
}

func TestAProductionTurnTheUserInterruptedIsLeftToTheUser(t *testing.T) {
	// Arrange
	h := production(t)
	deployEnds(t, h, wsm.DeployFinished)
	h.turnCloses = []wsm.TurnClose{wsm.CloseKilled}

	// Act
	h.o.Mediate(context.Background())

	// Assert
	if resumed := submissionsTo(h, "ws-fixer", BriefProductionIntakeResumed+" "); len(resumed) != 0 {
		t.Fatalf("resumptions %d, want none", len(resumed))
	}
	if c, err := h.db.ProductionCheckOf(context.Background(), mediationKey(h)); err != nil || c == nil {
		t.Fatalf("the check to decide = (%+v, %v), want it still owed to the user's turns", c, err)
	}
}

func TestDiffProductionSitesListsWhatIsNewToTheSpan(t *testing.T) {
	fixedAt := deployStarted.Add(-time.Hour)
	cases := []struct {
		name string
		span []logsites.Site
		// before is the span before's sites; nil for no baseline.
		before      []logsites.Site
		owned       map[string]wsm.WorkspaceID
		fixed       []wsm.FixedSite
		kinds       []wsm.IgnoredRecordKind
		wantNew     []string
		wantRefired []string
		wantOwned   int
		wantIgnored int
	}{
		{
			name:    "a site the span before never fired is new",
			span:    []logsites.Site{site("aaaaaaaaaaaa", "warn", deployStarted), site("bbbbbbbbbbbb", "error", deployStarted)},
			before:  []logsites.Site{site("aaaaaaaaaaaa", "warn", priorDeploy)},
			wantNew: []string{"bbbbbbbbbbbb"},
		},
		{
			name:    "with no span before every site is new",
			span:    []logsites.Site{site("aaaaaaaaaaaa", "warn", deployStarted)},
			wantNew: []string{"aaaaaaaaaaaa"},
		},
		{
			name:      "a site an open child owns is left out",
			span:      []logsites.Site{site("aaaaaaaaaaaa", "warn", deployStarted)},
			owned:     map[string]wsm.WorkspaceID{"aaaaaaaaaaaa": "child"},
			wantOwned: 1,
		},
		{
			name:        "a site of an ignored kind is left out",
			span:        []logsites.Site{site("aaaaaaaaaaaa", "warn", deployStarted)},
			kinds:       []wsm.IgnoredRecordKind{{Operation: "daemon.aaaaaaaaaaaa", Why: "outside"}},
			wantIgnored: 1,
		},
		{
			name:        "a fixed site firing after its fix landed is refired, even when the span before had it",
			span:        []logsites.Site{site("aaaaaaaaaaaa", "warn", deployStarted)},
			before:      []logsites.Site{site("aaaaaaaaaaaa", "warn", priorDeploy)},
			fixed:       []wsm.FixedSite{{Site: "aaaaaaaaaaaa", Workspace: "child", LandedAt: fixedAt}},
			wantRefired: []string{"aaaaaaaaaaaa"},
		},
		{
			name:   "a fixed site last seen before its fix landed is not refired",
			span:   []logsites.Site{site("aaaaaaaaaaaa", "warn", fixedAt.Add(-time.Minute))},
			before: []logsites.Site{site("aaaaaaaaaaaa", "warn", priorDeploy)},
			fixed:  []wsm.FixedSite{{Site: "aaaaaaaaaaaa", Workspace: "child", LandedAt: fixedAt}},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			var before *logsites.Answer
			if tc.before != nil {
				before = &logsites.Answer{Sites: tc.before}
			}

			// Act
			got := DiffProductionSites(logsites.Answer{Sites: tc.span}, before, tc.owned, tc.fixed, tc.kinds)

			// Assert
			var gotNew, gotRefired []string
			for _, s := range got.New {
				gotNew = append(gotNew, s.Key)
			}
			for _, r := range got.Refired {
				gotRefired = append(gotRefired, r.Site.Key)
			}
			if !slices.Equal(gotNew, tc.wantNew) || !slices.Equal(gotRefired, tc.wantRefired) || got.Owned != tc.wantOwned || got.Ignored != tc.wantIgnored {
				t.Fatalf("diff = new %v, refired %v, owned %d, ignored %d; want %v, %v, %d, %d", gotNew, gotRefired, got.Owned, got.Ignored, tc.wantNew, tc.wantRefired, tc.wantOwned, tc.wantIgnored)
			}
		})
	}
}

// toldProduction makes the production harness's mediator told of a due span.
func toldProduction(t *testing.T, h *harness) {
	t.Helper()
	deployEnds(t, h, wsm.DeployFinished)
	q := productionQueueOf(t, h)
	if _, err := h.o.mediatorOf(context.Background(), productionRole, q, dlog.Context{}); err != nil {
		t.Fatal(err)
	}
	if told, err := h.db.MarkProductionCheckPrompted(context.Background(), mediationKey(h), deployStarted, deployStarted.Add(time.Minute), "turn-told"); err != nil || !told {
		t.Fatalf("MarkProductionCheckPrompted = (%v, %v)", told, err)
	}
}

// productionQueueOf is the production harness's queue.
func productionQueueOf(t *testing.T, h *harness) checkedQueue {
	t.Helper()
	queues, err := h.o.productionQueues(context.Background(), opProduction)
	if err != nil || len(queues) != 1 {
		t.Fatalf("productionQueues = (%v, %v)", queues, err)
	}
	return queues[0]
}

// twoChildren is a dispatch of a remediation and an investigation.
func twoChildren() ProductionDecisionRequest {
	return ProductionDecisionRequest{Summary: "two problems", Children: []ProductionChildRequest{
		{Name: "fix-tail", Kind: wsm.ProductionRemediation, Findings: []ProductionFinding{{Statement: "the shim's tail lags", Sites: []string{"aaaaaaaaaaaa"}, Evidence: []string{"tail lag 9s"}}}},
		{Name: "ask-footer", Kind: wsm.ProductionInvestigation, Findings: []ProductionFinding{{Statement: "the footer flickers", Sites: []string{"bbbbbbbbbbbb"}, Evidence: []string{"redraw x40"}}}},
	}}
}

func TestADispatchMakesEveryChildCommissionedWithItsSiblings(t *testing.T) {
	// Arrange
	h := production(t)
	toldProduction(t, h)

	// Act
	made, err := h.o.DecideProduction(context.Background(), "ws-fixer", twoChildren())

	// Assert
	if err != nil || len(made) != 2 || !slices.Equal(h.children.made, []string{"fix-tail", "ask-footer"}) {
		t.Fatalf("DecideProduction = (%v, %v), made %v; want both children", made, err, h.children.made)
	}
	if !strings.Contains(h.children.prompts[0], "ask-footer") || !strings.Contains(h.children.prompts[1], "fix-tail") || !strings.HasPrefix(h.children.prompts[0], BriefProductionChild+" remediation") {
		t.Fatalf("commissions = %q, want each naming its kind and its sibling", h.children.prompts)
	}
	owned, err := h.db.OwnedSites(context.Background(), mediationKey(h))
	if err != nil || owned["aaaaaaaaaaaa"] != made[0].ID || owned["bbbbbbbbbbbb"] != made[1].ID {
		t.Fatalf("owned = (%v, %v), want each child's site its own", owned, err)
	}
	if !slices.Contains(h.meter.got, "production_check:dispatched:2") {
		t.Fatalf("metrics = %v, want the dispatch counted", h.meter.got)
	}
}

func TestADispatchIsRefusedWholeAndUndone(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(t *testing.T, h *harness)
		arm     string
		// wantUndone is how many children were made and then undone.
		wantUndone int
	}{
		{name: "a site an open child owns refuses before anything is made", arm: ArmSiteOwned, arrange: func(t *testing.T, h *harness) {
			if err := h.db.DecideProductionCheck(context.Background(), mediationKey(h), deployStarted, deployStarted, wsm.ProductionDispatched, "held", []wsm.ProductionChild{{Workspace: "holder", Kind: wsm.ProductionRemediation, Sites: []string{"aaaaaaaaaaaa"}}}); err != nil {
				t.Fatal(err)
			}
			if _, _, err := h.db.checks.OweProductionCheck(context.Background(), mediationKey(h), deployStarted.Add(time.Hour), ""); err != nil {
				t.Fatal(err)
			}
			if _, err := h.db.MarkProductionChecksDue(context.Background(), mediationKey(h), deployStarted.Add(time.Hour)); err != nil {
				t.Fatal(err)
			}
			if _, err := h.db.MarkProductionCheckPrompted(context.Background(), mediationKey(h), deployStarted.Add(time.Hour), deployStarted.Add(time.Hour), "turn-2"); err != nil {
				t.Fatal(err)
			}
		}},
		{name: "a child that cannot be made undoes the ones made", arm: ArmChildRefused, wantUndone: 1, arrange: func(t *testing.T, h *harness) {
			h.children.createErrAt = 2
		}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := production(t)
			toldProduction(t, h)
			tc.arrange(t, h)
			before, _ := h.db.ProductionChildrenOf(context.Background(), mediationKey(h))

			// Act
			_, err := h.o.DecideProduction(context.Background(), "ws-fixer", twoChildren())

			// Assert
			refused, ok := Refused(err)
			after, _ := h.db.ProductionChildrenOf(context.Background(), mediationKey(h))
			if !ok || refused.Arm != tc.arm || len(h.children.undone) != tc.wantUndone || len(after) != len(before) {
				t.Fatalf("DecideProduction = %v, undone %v, children %d (before %d); want %s, %d undone, nothing recorded", err, h.children.undone, len(after), len(before), tc.arm, tc.wantUndone)
			}
		})
	}
}

func TestAProductionDecisionIsRefusedOutsideItsMediatorsToldSpan(t *testing.T) {
	cases := []struct {
		name string
		ws   ids.WorkspaceID
		told bool
		arm  string
	}{
		{name: "a workspace that is no production mediator", ws: "stranger", told: true, arm: ArmNotProductionMediator},
		{name: "a span the mediator was never told", ws: "ws-fixer", arm: ArmNoProductionCheckOwed},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := production(t)
			if tc.told {
				toldProduction(t, h)
			} else {
				deployEnds(t, h, wsm.DeployFinished)
				if _, err := h.o.mediatorOf(context.Background(), productionRole, productionQueueOf(t, h), dlog.Context{}); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			_, err := h.o.DecideProduction(context.Background(), tc.ws, ProductionDecisionRequest{Summary: "nothing"})

			// Assert
			if refused, ok := Refused(err); !ok || refused.Arm != tc.arm {
				t.Fatalf("DecideProduction = %v, want %s", err, tc.arm)
			}
		})
	}
}

func TestAFoldMovesTheSitesTellsTheSurvivorAndClosesTheFolded(t *testing.T) {
	// Arrange
	h := production(t)
	toldProduction(t, h)
	made, err := h.o.DecideProduction(context.Background(), "ws-fixer", twoChildren())
	if err != nil {
		t.Fatal(err)
	}

	// Act
	err = h.o.FoldProduction(context.Background(), "ws-fixer", made[1].ID, made[0].ID)

	// Assert
	owned, _ := h.db.OwnedSites(context.Background(), mediationKey(h))
	told := submissionsTo(h, made[0].ID, BriefProductionChildFolded+" ")
	if err != nil || owned["bbbbbbbbbbbb"] != made[0].ID || len(told) != 1 || !slices.Equal(h.children.closed, []ids.WorkspaceID{made[1].ID}) {
		t.Fatalf("FoldProduction = %v, owned %v, told %d, closed %v; want the sites moved, the survivor told and the folded closed", err, owned, len(told), h.children.closed)
	}
	if !strings.Contains(submittedText(told[0]), " ask-footer ") {
		t.Fatalf("the survivor's brief %q, want the folded child's branch", submittedText(told[0]))
	}
}

func TestAFoldOfAWorkspaceThatIsNoOpenChildIsRefused(t *testing.T) {
	// Arrange
	h := production(t)
	toldProduction(t, h)
	made, err := h.o.DecideProduction(context.Background(), "ws-fixer", twoChildren())
	if err != nil {
		t.Fatal(err)
	}

	// Act
	err = h.o.FoldProduction(context.Background(), "ws-fixer", "stranger", made[0].ID)

	// Assert
	if refused, ok := Refused(err); !ok || refused.Arm != ArmNotFoldable || len(h.children.closed) != 0 {
		t.Fatalf("FoldProduction = %v, closed %v; want not_foldable and nothing closed", err, h.children.closed)
	}
}

func TestAClosedChildIsEndedByThePassAsLandedOrClosed(t *testing.T) {
	landedAt := deployStarted.Add(30 * time.Minute)
	cases := []struct {
		name      string
		merged    bool
		wantFixed bool
		wantEvent string
	}{
		{name: "a child whose merge landed lands its sites", merged: true, wantFixed: true, wantEvent: "production_child:landed"},
		{name: "a child closed unlanded releases its sites", wantEvent: "production_child:closed"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := production(t)
			toldProduction(t, h)
			made, err := h.o.DecideProduction(context.Background(), "ws-fixer", twoChildren())
			if err != nil {
				t.Fatal(err)
			}
			h.db.mu.Lock()
			record := h.db.workspaces[made[0].ID]
			record.Closed = true
			if tc.merged {
				record.MergedAt = &landedAt
			}
			h.db.workspaces[made[0].ID] = record
			h.db.mu.Unlock()

			// Act
			h.o.endClosedChildren(context.Background(), productionQueueOf(t, h), dlog.Context{})

			// Assert
			owned, _ := h.db.OwnedSites(context.Background(), mediationKey(h))
			fixed, _ := h.db.FixedSites(context.Background(), mediationKey(h))
			if _, held := owned["aaaaaaaaaaaa"]; held || (len(fixed) == 1) != tc.wantFixed || !slices.Contains(h.meter.got, tc.wantEvent) {
				t.Fatalf("owned %v, fixed %v, metrics %v; want the site released, fixed %v, %s counted", owned, fixed, h.meter.got, tc.wantFixed, tc.wantEvent)
			}
		})
	}
}

func TestChildNamesForMintsFromTheFirstFinding(t *testing.T) {
	// Arrange
	children := []ProductionChildRequest{
		{Name: "named"},
		{Findings: []ProductionFinding{{Statement: "The Shim's tail lags, badly, on every resume!"}}},
		{Findings: []ProductionFinding{{Statement: "!!!"}}},
	}

	// Act
	got := childNamesFor(children, time.Unix(1795121928, 0))

	// Assert
	want := []string{"named", "prod-the-shims-tail-lags-badly-21928-2", "prod-finding-21928-3"}
	if !slices.Equal(got, want) {
		t.Fatalf("childNamesFor = %v, want %v", got, want)
	}
}

func TestAProductionSiteFileIsOneOfTheQueuesLogs(t *testing.T) {
	// Act
	path := ProductionSitePath("/state/merge-logs", time.Unix(1795121928, 0))

	// Assert
	if !IsLog(strings.TrimPrefix(path, "/state/merge-logs/")) {
		t.Fatalf("%s is no queue log, want it pruned with the rest", path)
	}
}

func TestSubmitMarkedTurnRecordsTheTurnBeforeItSubmitsTheBrief(t *testing.T) {
	cases := []struct {
		name        string
		marked      bool
		markErr     error
		submitErr   bool
		wantFailure turnFailure
		wantSubmits int
	}{
		{name: "a recorded turn is submitted", marked: true, wantSubmits: 1},
		{name: "a turn the check no longer takes submits nothing", marked: false},
		{name: "a record that fails submits nothing", markErr: errors.New("database is locked"), wantFailure: turnMarkFailed},
		{name: "a refused submission is the submit's failure", marked: true, submitErr: true, wantFailure: turnSubmitFailed, wantSubmits: 1},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := production(t)
			if tc.submitErr {
				h.queue.err = errors.New("the queue is closed")
			}
			var recorded ids.TurnID

			// Act
			turn, marked, failed, err := h.o.submitMarkedTurn(context.Background(), "ws-fixer", "brief", conversationv1.PromptOrigin_PROMPT_ORIGIN_PRODUCTION_MEDIATION, func(turn ids.TurnID) (bool, error) {
				recorded = turn
				return tc.marked, tc.markErr
			})

			// Assert
			if failed != tc.wantFailure || (err != nil) != (tc.wantFailure != turnNotFailed) || marked != (tc.marked && tc.markErr == nil) || turn != recorded {
				t.Fatalf("submitMarkedTurn = (%q, %v, %v, %v), recorded %q", turn, marked, failed, err, recorded)
			}
			if got := len(submissionsTo(h, "ws-fixer", "brief")); got != tc.wantSubmits {
				t.Fatalf("submissions %d, want %d", got, tc.wantSubmits)
			}
		})
	}
}

func TestEveryCheckTurnMarkGoesThroughSubmitMarkedTurn(t *testing.T) {
	// Arrange
	files, err := filepath.Glob("*.go")
	if err != nil {
		t.Fatal(err)
	}
	marks := regexp.MustCompile(`DB\.(MarkMediationResumed|MarkProductionCheckPrompted|MarkProductionCheckResumed)\(`)
	// Act, Assert: each such record is made inside submitMarkedTurn's mark.
	for _, file := range files {
		if strings.HasSuffix(file, "_test.go") {
			continue
		}
		body, err := os.ReadFile(file)
		if err != nil {
			t.Fatal(err)
		}
		for _, at := range marks.FindAllIndex(body, -1) {
			before := string(body[max(0, at[0]-300):at[0]])
			if !strings.Contains(before, "submitMarkedTurn(") {
				t.Fatalf("%s records a check turn outside submitMarkedTurn: %s", file, body[at[0]:at[1]])
			}
		}
	}
}
