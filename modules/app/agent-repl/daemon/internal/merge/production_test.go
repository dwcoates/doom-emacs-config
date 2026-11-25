package merge

import (
	"context"
	"errors"
	"fmt"
	"os"
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
	h.briefs[BriefProductionIntake] = []string{"span_since", "span_until", "new_sites", "refired_sites", "site_file", "baseline", "span_children", "metrics_window"}
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
	if !slices.Equal(h.checks.made, []string{wsm.MediatorProduction.Name()}) {
		t.Fatalf("made %v, want the production mediator", h.checks.made)
	}
	if c, err := h.db.ProductionCheckOf(context.Background(), mediationKey(h)); err != nil || c != nil {
		t.Fatalf("after the decision the check to decide = (%+v, %v), want none", c, err)
	}
}

func TestASpanTheTallyNoLongerKeepsIsToldWithItsLossDeclared(t *testing.T) {
	// Arrange: the span ended before the tally's oldest kept row.
	h := production(t)
	deployEnds(t, h, wsm.DeployFinished)
	oldest := deployStarted.Add(3 * time.Hour)
	h.sites.errs[priorDeploy] = &logsites.UnrecordedWindowError{Oldest: oldest}
	h.queue.onSubmit = func(sub promptqueue.Submission) {
		if strings.HasPrefix(submittedText(sub), BriefProductionIntake+" ") {
			if _, err := h.o.DecideProduction(context.Background(), "ws-fixer", ProductionDecisionRequest{Summary: "read from the raw logs"}); err != nil {
				t.Errorf("DecideProduction: %v", err)
			}
		}
	}

	// Act
	h.o.Mediate(context.Background())

	// Assert
	if told := productionTellings(h); len(told) != 1 {
		t.Fatalf("production tellings = %+v, want the unrecorded span told once", told)
	}
	file, err := os.ReadFile(ProductionSitePath(LogsDir(h.stateDir), deployStarted))
	command := "`bin/logs.sh --all --level warn --since " + priorDeploy.Format(time.RFC3339) + " --until " + deployStarted.Format(time.RFC3339) + " --tally --sample 1`"
	if err != nil || !strings.Contains(string(file), "UNRECORDED: the tally keeps no row before "+oldest.Format(time.RFC3339)) || !strings.Contains(string(file), command) {
		t.Fatalf("site file = (%q, %v), want the loss declared with %s", file, err, command)
	}
	rec, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "warn", Operation: opProduction,
		Message: "the span's log sites were pruned before its mediator read them; it is told to read the span from the raw logs"})
	if !ok || rec.Context["span_since"] != priorDeploy.Format(time.RFC3339Nano) || rec.Context["span_until"] != deployStarted.Format(time.RFC3339Nano) ||
		rec.Context["oldest_kept"] != oldest.Format(time.RFC3339Nano) {
		t.Fatalf("records = %v, want the loss at WARN with the span and the oldest kept row", h.logs.Records())
	}
	if _, failed := h.recordFor("error", opProduction); failed {
		t.Fatalf("records = %v, want no ERROR: the span is told, not failed", h.logs.Records())
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
	// spanEnd is when the span ended: the landing after it began.
	spanEnd := deployStarted.Add(time.Minute)
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
		// wantUnshipped is how many sites a fix landed for only after the
		// span ended.
		wantUnshipped int
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
			name:          "a site whose fix landed after the span ended is left out, not new",
			span:          []logsites.Site{site("aaaaaaaaaaaa", "warn", deployStarted)},
			fixed:         []wsm.FixedSite{{Site: "aaaaaaaaaaaa", Workspace: "child", LandedAt: spanEnd.Add(time.Second)}},
			wantUnshipped: 1,
		},
		{
			name:          "a site whose fix landed after the span ended is left out, not refired, when the span before had it",
			span:          []logsites.Site{site("aaaaaaaaaaaa", "warn", deployStarted)},
			before:        []logsites.Site{site("aaaaaaaaaaaa", "warn", priorDeploy)},
			fixed:         []wsm.FixedSite{{Site: "aaaaaaaaaaaa", Workspace: "child", LandedAt: spanEnd.Add(time.Second)}},
			wantUnshipped: 1,
		},
		{
			name:    "a site whose fix landed as the span ended was in production for it",
			span:    []logsites.Site{site("aaaaaaaaaaaa", "warn", deployStarted)},
			fixed:   []wsm.FixedSite{{Site: "aaaaaaaaaaaa", Workspace: "child", LandedAt: spanEnd}},
			wantNew: []string{"aaaaaaaaaaaa"},
		},
		{
			name:      "a site an open child owns is counted owned even when another child's fix is unshipped",
			span:      []logsites.Site{site("aaaaaaaaaaaa", "warn", deployStarted)},
			owned:     map[string]wsm.WorkspaceID{"aaaaaaaaaaaa": "child-2"},
			fixed:     []wsm.FixedSite{{Site: "aaaaaaaaaaaa", Workspace: "child", LandedAt: spanEnd.Add(time.Second)}},
			wantOwned: 1,
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
			got := DiffProductionSites(logsites.Answer{Sites: tc.span}, spanEnd, before, tc.owned, tc.fixed, tc.kinds)

			// Assert
			var gotNew, gotRefired []string
			for _, s := range got.New {
				gotNew = append(gotNew, s.Key)
			}
			for _, r := range got.Refired {
				gotRefired = append(gotRefired, r.Site.Key)
			}
			if !slices.Equal(gotNew, tc.wantNew) || !slices.Equal(gotRefired, tc.wantRefired) || got.Owned != tc.wantOwned || got.Ignored != tc.wantIgnored || got.Unshipped != tc.wantUnshipped {
				t.Fatalf("diff = new %v, refired %v, owned %d, ignored %d, unshipped %d; want %v, %v, %d, %d, %d", gotNew, gotRefired, got.Owned, got.Ignored, got.Unshipped, tc.wantNew, tc.wantRefired, tc.wantOwned, tc.wantIgnored, tc.wantUnshipped)
			}
		})
	}
}

func TestDiffProductionSitesCarriesTheHoursTheTallyCovered(t *testing.T) {
	// Arrange
	from, to := deployStarted.Truncate(time.Hour), deployStarted.Truncate(time.Hour).Add(time.Hour)
	span := logsites.Answer{From: from, To: to, Partial: true}

	// Act
	got := DiffProductionSites(span, deployStarted, nil, nil, nil, nil)

	// Assert
	if !got.From.Equal(from) || !got.To.Equal(to) || !got.Partial {
		t.Fatalf("coverage = %v to %v, partial %v; want %v to %v, partial", got.From, got.To, got.Partial, from, to)
	}
}

func TestRenderProductionSitesCountsWhatItLeftOut(t *testing.T) {
	// Arrange
	c := wsm.ProductionCheck{Since: priorDeploy, Until: deployStarted}
	sites := ProductionSites{Baseline: "no earlier span", Owned: 2, Unshipped: 3, Ignored: 4}

	// Act
	got := RenderProductionSites(sites, c)

	// Assert
	want := "Left out: 2 sites open children own, 3 sites whose fix landed, not yet in production, 4 sites of ignored kinds.\n"
	if !strings.Contains(got, want) {
		t.Fatalf("site file = %q, want %q", got, want)
	}
}

func TestRenderProductionSitesSaysHowMuchOfTheSpanTheTallyRead(t *testing.T) {
	from, to := time.Date(2026, 11, 25, 16, 0, 0, 0, time.UTC), time.Date(2026, 11, 25, 17, 0, 0, 0, time.UTC)
	c := wsm.ProductionCheck{Since: time.Date(2026, 11, 25, 4, 58, 6, 0, time.UTC), Until: time.Date(2026, 11, 25, 16, 31, 21, 0, time.UTC)}
	cases := []struct {
		name       string
		partial    bool
		unrecorded bool
		want       string
		notWant    string
	}{
		{
			name:    "a span read whole says so",
			want:    "Read whole from the tally: 2026-11-25T16:00:00Z to 2026-11-25T17:00:00Z.\n",
			notWant: "PARTIAL",
		},
		{
			name:    "a span begun before the oldest kept row says it was read only in part",
			partial: true,
			want:    "PARTIAL: the tally keeps no row before 2026-11-25T16:00:00Z, so it read only 2026-11-25T16:00:00Z to 2026-11-25T17:00:00Z; what fired before then is unknown, not absent.\n",
			notWant: "Read whole",
		},
		{
			name:       "a span wholly before the oldest kept row says none of it was read and how to read it",
			unrecorded: true,
			want:       "UNRECORDED: the tally keeps no row before 2026-11-25T16:00:00Z, so none of this span was read from it; read the span from the raw logs with `bin/logs.sh --all --level warn --since 2026-11-25T04:58:06Z --until 2026-11-25T16:31:21Z --tally --sample 1`.\n",
			notWant:    "Read whole",
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			sites := ProductionSites{Baseline: "no earlier span", From: from, To: to, Partial: tc.partial}
			if tc.unrecorded {
				sites = ProductionSites{Baseline: "no earlier span", Unrecorded: true, OldestKept: from}
			}

			// Act
			got := RenderProductionSites(sites, c)

			// Assert
			if !strings.Contains(got, tc.want) || strings.Contains(got, tc.notWant) {
				t.Fatalf("site file =\n%s\nwant it to hold %q and not %q", got, tc.want, tc.notWant)
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

func TestRenderSpanChildrenSaysHowEachStands(t *testing.T) {
	endedAt := deployStarted.Add(time.Hour)
	into := ids.WorkspaceID("ws-survivor")
	names := map[ids.WorkspaceID]string{"ws-a": "fix-tail", "ws-survivor": "fix-all"}
	cases := []struct {
		name  string
		child wsm.ProductionChild
		want  string
	}{
		{name: "an open child owns its sites", child: wsm.ProductionChild{Workspace: "ws-a", Kind: wsm.ProductionRemediation, Sites: []string{"aaaaaaaaaaaa"}},
			want: "\n- `fix-tail` (remediation): open, owning aaaaaaaaaaaa"},
		{name: "an open child that owns no site says so", child: wsm.ProductionChild{Workspace: "ws-a", Kind: wsm.ProductionInvestigation},
			want: "\n- `fix-tail` (investigation): open, owning no site"},
		{name: "a landed child names its landing and the sites it fixed", child: wsm.ProductionChild{Workspace: "ws-a", Kind: wsm.ProductionRemediation, Named: []string{"aaaaaaaaaaaa"}, Ending: wsm.ProductionChildLanded, EndedAt: &endedAt},
			want: "\n- `fix-tail` (remediation): landed 2026-11-13T22:00:00Z, its fix for aaaaaaaaaaaa"},
		{name: "a closed child names its close", child: wsm.ProductionChild{Workspace: "ws-a", Kind: wsm.ProductionRemediation, Named: []string{"aaaaaaaaaaaa"}, Ending: wsm.ProductionChildClosed, EndedAt: &endedAt},
			want: "\n- `fix-tail` (remediation): closed unlanded 2026-11-13T22:00:00Z, given aaaaaaaaaaaa"},
		{name: "a folded child names the child it went into", child: wsm.ProductionChild{Workspace: "ws-a", Kind: wsm.ProductionRemediation, Named: []string{"aaaaaaaaaaaa"}, Ending: wsm.ProductionChildFolded, EndedAt: &endedAt, FoldedInto: &into},
			want: "\n- `fix-tail` (remediation): folded into `fix-all` 2026-11-13T22:00:00Z, given aaaaaaaaaaaa"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := renderSpanChildren(names, []wsm.ProductionChild{tc.child})

			// Assert
			if got != tc.want {
				t.Fatalf("renderSpanChildren = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestRenderSpanChildrenOfNoneSaysNone(t *testing.T) {
	// Act
	got := renderSpanChildren(nil, nil)

	// Assert
	if got != "none" {
		t.Fatalf("renderSpanChildren = %q, want none", got)
	}
}

func TestChildStandingPanicsOnACorruptEnding(t *testing.T) {
	endedAt := deployStarted
	cases := []struct {
		name  string
		child wsm.ProductionChild
	}{
		{name: "an ending at no instant", child: wsm.ProductionChild{Workspace: "ws-a", Ending: wsm.ProductionChildLanded}},
		{name: "a fold into no child", child: wsm.ProductionChild{Workspace: "ws-a", Ending: wsm.ProductionChildFolded, EndedAt: &endedAt}},
		{name: "an unknown ending", child: wsm.ProductionChild{Workspace: "ws-a", Ending: wsm.ProductionChildEnding("vanished"), EndedAt: &endedAt}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			defer func() {
				if recover() == nil {
					t.Fatal("childStanding answered, want a panic")
				}
			}()

			// Act
			childStanding(map[ids.WorkspaceID]string{}, tc.child)
		})
	}
}

func TestTheProductionBriefListsTheChildrenOpenDuringTheSpan(t *testing.T) {
	// Arrange: two children; one lands at the start of the span the brief is
	// of, and so was open during it.
	h := production(t)
	toldProduction(t, h)
	made, err := h.o.DecideProduction(context.Background(), "ws-fixer", twoChildren())
	if err != nil {
		t.Fatal(err)
	}
	spanSince := deployStarted.Add(time.Hour)
	if _, err := h.db.EndProductionChild(context.Background(), made[0].ID, wsm.ProductionChildLanded, spanSince); err != nil {
		t.Fatal(err)
	}
	c := wsm.ProductionCheck{Since: spanSince, Until: spanSince.Add(time.Hour)}

	// Act
	brief, err := h.o.productionBrief(context.Background(), productionQueueOf(t, h), c)

	// Assert
	if err != nil || !strings.Contains(brief, "landed "+spanSince.UTC().Format(time.RFC3339)+", its fix for aaaaaaaaaaaa") || !strings.Contains(brief, "open, owning bbbbbbbbbbbb") {
		t.Fatalf("brief = %q, %v; want the landed child and the open one", brief, err)
	}
}

func TestAProductionBriefWhoseChildrenCannotBeReadIsAnError(t *testing.T) {
	// Arrange
	h := production(t)
	deployEnds(t, h, wsm.DeployFinished)
	h.db.mu.Lock()
	h.db.spanChildrenErr = errors.New("database is locked")
	h.db.mu.Unlock()

	// Act
	h.o.Mediate(context.Background())

	// Assert: recorded at ERROR, nothing told, and the span still due.
	rec, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: "error", Operation: opProduction,
		Message: "the production check's brief could not be composed; the span stays due"})
	if !ok || !strings.Contains(fmt.Sprint(rec.Context["error"]), "database is locked") {
		t.Fatalf("records = %v, want the brief's failure at ERROR carrying its cause", h.logs.Records())
	}
	if told := productionTellings(h); len(told) != 0 {
		t.Fatalf("tellings = %+v, want none", told)
	}
	if c, err := h.db.ProductionCheckOf(context.Background(), mediationKey(h)); err != nil || c == nil {
		t.Fatalf("the check to decide = (%+v, %v), want it still due", c, err)
	}
}
