package merge

import (
	"context"
	"errors"
	"fmt"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"agentrepl/logging/logsite"
	conversationv1 "agentrepl/proto/conversation/v1"
	repoconfigv1 "agentrepl/proto/repoconfig/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/inflight"
	"agent-repld/internal/logsites"
	"agent-repld/internal/wsm"
)

// THE PRODUCTION CHECKS (repoconfig.v1.LifecycleConfig.production_checks;
// docs/investigations/2026-11-23-production-mediator-plan.md). When a landing
// begins, its repository owes its production mediator the SPAN that closed
// then: from where the last check read to, to the landing's start
// (wsm.OweProductionCheck). The span falls due when the landing's deploy
// ends, and each pass tells the mediator the oldest due span with a file of
// the log sites new to it. The mediator decides it through
// agentrepl.v1.UpdateProductionMediation: nothing to do, or children to
// dispatch. Nothing here holds the queue or a deploy.

const opProduction = "daemon.merge.production"

// BriefProductionIntake tells the production mediator of a span.
const BriefProductionIntake = "production-intake"

// BriefProductionIntakeResumed resumes a production mediator whose turn on a
// span ended deciding nothing.
const BriefProductionIntakeResumed = "production-intake-resumed"

// BriefProductionChild is a production child's commission.
const BriefProductionChild = "production-child"

// BriefProductionChildFolded tells the surviving child of a fold what it
// takes over.
const BriefProductionChildFolded = "production-child-folded"

// LogSiteTally answers the log sites that fired in a window
// (logsites.Tallier).
type LogSiteTally interface {
	Window(ctx context.Context, since, until time.Time, minimum string) (logsites.Answer, error)
}

// ProductionChildren makes, undoes and closes a production mediator's
// children (workspace.Verbs).
type ProductionChildren interface {
	// CreateChild makes one child of MEDIATOR in the repository at REPODIR:
	// a one-shot named NAME, landing on the main branch at the front of the
	// queue, on the daemon's default model for children, commissioned with
	// PROMPT.
	CreateChild(ctx context.Context, repoDir string, mediator ids.WorkspaceID, name, prompt string) (wsm.Workspace, error)
	// Undo removes a child a refused dispatch made, its worktree and branch
	// with it.
	Undo(ctx context.Context, ws ids.WorkspaceID) error
	// Close closes a folded child, its work carried by the survivor.
	Close(ctx context.Context, ws ids.WorkspaceID) error
}

// ProductionFinding is one thing wrong in production, as the mediator states
// it to a child.
type ProductionFinding struct {
	Statement string
	Sites     []string
	Evidence  []string
}

// ProductionChildRequest is one child a dispatch asks for.
type ProductionChildRequest struct {
	// Name is the child's name; empty mints one from its first finding.
	Name     string
	Kind     wsm.ProductionChildKind
	Findings []ProductionFinding
}

// ProductionDecisionRequest is the production mediator's decision on the
// span it was told: Children empty for nothing to do.
type ProductionDecisionRequest struct {
	Summary  string
	Children []ProductionChildRequest
}

// The refusal arms of agentrepl.v1.UpdateProductionMediationError the
// orchestrator answers.
const (
	ArmNotProductionMediator = "not_production_mediator"
	ArmNoProductionCheckOwed = "no_check_owed"
	ArmSiteOwned             = "site_owned"
	ArmNotFoldable           = "not_foldable"
	ArmChildRefused          = "child_refused"
)

// The outcomes ObserveProductionCheck records.
const (
	ProductionCheckNothingToDo = "nothing_to_do"
	ProductionCheckDispatched  = "dispatched"
	ProductionCheckUnanswered  = "unanswered"
	// ProductionCheckPaused is a span a pause left unreviewed: dropped by
	// the pause, or skipped as it ended in or began in one.
	ProductionCheckPaused = "paused"
)

// The events ObserveProductionChild records.
const (
	ProductionChildFolded = "folded"
	ProductionChildLanded = "landed"
	ProductionChildClosed = "closed"
)

// productionQueues answers every registered repository whose lifecycle config
// checks its production.
func (o *orchestrator) productionQueues(ctx context.Context, op string) ([]checkedQueue, error) {
	return o.queuesWhere(ctx, op, func(cfg *repoconfigv1.LifecycleConfig) bool { return cfg.GetProductionChecks() != nil })
}

// oweProductionChecks owes, in every repository that checks its production,
// the span the landing of the deploy that began AT closed. A repository
// whose span cannot be owed is recorded at ERROR; the span runs on into the
// next one.
func (o *orchestrator) oweProductionChecks(ctx context.Context, at time.Time) error {
	queues, err := o.productionQueues(ctx, opProduction)
	if err != nil {
		o.mediationFailedUnder(ctx, opProduction, nil, logsite.Msg("could not list the repositories a landing owes a production check in"), err)
		return err
	}
	var errs []error
	for _, q := range queues {
		fields := dlog.Context{"repo": string(q.key), "deploy_started": at.UTC().Format(time.RFC3339Nano)}
		check, owing, err := o.deps.DB.OweProductionCheck(ctx, q.key, at, "")
		if err != nil {
			o.mediationFailedUnder(ctx, opProduction, fields, logsite.Msg("could not owe the production check of the span the landing closed; the span runs on into the next"), err)
			errs = append(errs, err)
			continue
		}
		span := withField(withField(fields,
			"span_since", check.Since.UTC().Format(time.RFC3339Nano)), "span_until", check.Until.UTC().Format(time.RFC3339Nano))
		switch owing {
		case wsm.ProductionSpanEmpty:
			o.deps.Log.Global().Info(opProduction, "the landing closed no span between landings; it owes no production check", fields)
		case wsm.ProductionSpanPaused:
			o.deps.Meter.ObserveProductionCheck(ProductionCheckPaused, 0)
			o.deps.Log.Global().Info(opProduction, "the landing closed a span while the production checks are paused; it is never reviewed", span)
		case wsm.ProductionSpanBeganInPause:
			o.deps.Meter.ObserveProductionCheck(ProductionCheckPaused, 0)
			o.deps.Log.Global().Info(opProduction, "the first landing after the production checks resumed closed a span begun in the pause; it is never reviewed, and the checks start at this landing", span)
		case wsm.ProductionSpanOwed:
			o.deps.Log.Global().Info(opProduction, "the landing owes a production check of the span before it", span)
		default:
			panic(fmt.Sprintf("merge: the store answered the undeclared production owing %q", owing))
		}
	}
	return errors.Join(errs...)
}

// noteProductionEnding records how the deploy that began AT ended on every
// production span it owed: a crash closes no span, so its span is dropped
// and runs on; any other ending makes the span due.
func (o *orchestrator) noteProductionEnding(ctx context.Context, at time.Time, ending wsm.DeployEnding) error {
	return o.endProductionSpans(ctx, at, ending, dlog.Context{"deploy_started": at.UTC().Format(time.RFC3339Nano)})
}

// noteTakenOverProductionEnding makes due, in every repository that checks
// its production, the spans the deploy this daemon took over ended. Its
// start is not in memory, and none is needed: a span is made due by the
// ending alone. A crash is never taken over (its successor owes it as a
// deploy of its own), so ENDING is never DaemonCrashed.
func (o *orchestrator) noteTakenOverProductionEnding(ctx context.Context, ending wsm.DeployEnding) error {
	return o.endProductionSpans(ctx, time.Time{}, ending, dlog.Context{"taken_over": true})
}

// endProductionSpans is noteProductionEnding and
// noteTakenOverProductionEnding: DEPLOY names the deploy that ended in each
// record, and AT, its start, names the span a crash drops.
func (o *orchestrator) endProductionSpans(ctx context.Context, at time.Time, ending wsm.DeployEnding, deploy dlog.Context) error {
	queues, err := o.productionQueues(ctx, opProduction)
	if err != nil {
		o.mediationFailedUnder(ctx, opProduction, nil, logsite.Msg("could not list the repositories a deploy's ending makes a production check due in"), err)
		return err
	}
	var errs []error
	for _, q := range queues {
		fields := withField(withField(deploy, "repo", string(q.key)), "ending", string(ending))
		if ending == wsm.DaemonCrashed {
			dropped, err := o.deps.DB.DropProductionCheck(ctx, q.key, at)
			if err != nil {
				o.mediationFailedUnder(ctx, opProduction, fields, logsite.Msg("could not drop the production span a crash owed; it is told as a span of its own"), err)
				errs = append(errs, err)
				continue
			}
			o.deps.Log.Global().Info(opProduction, "a crash closes no span; the production span runs on into the next landing", withField(fields, "dropped", dropped))
			continue
		}
		due, err := o.deps.DB.MarkProductionChecksDue(ctx, q.key, o.deps.Now())
		if err != nil {
			o.mediationFailedUnder(ctx, opProduction, fields, logsite.Msg("could not make the production checks the deploy ended due; the next deploy's end makes them due"), err)
			errs = append(errs, err)
			continue
		}
		o.deps.Log.Global().Info(opProduction, "the deploy ended; its production checks are due", withField(fields, "due", due))
	}
	return errors.Join(errs...)
}

// productionPass runs, for every repository that checks its production, a
// round of the oldest due span, and ends the children whose workspaces closed.
// Each round runs under the mediation's lifetime, off the pass.
func (o *orchestrator) productionPass(ctx context.Context) {
	queues, err := o.productionQueues(ctx, opProduction)
	if err != nil {
		o.mediationFailedUnder(ctx, opProduction, nil, logsite.Msg("could not list the repositories whose production checks are owed"), err)
		return
	}
	for _, q := range queues {
		fields := dlog.Context{"repo": string(q.key)}
		o.keepMediatorShown(ctx, productionRole, q)
		o.endClosedChildren(ctx, q, fields)
		check, err := o.deps.DB.ProductionCheckOf(ctx, q.key)
		if err != nil {
			if o.mediationFailedUnder(ctx, opProduction, fields, logsite.Msg("could not read the production check to decide next"), err) {
				return
			}
			continue
		}
		if check == nil {
			continue
		}
		round, leave, entered := o.enterMediation()
		if !entered {
			o.deps.Log.Global().Info(opProduction, mediationNotRun, fields)
			return
		}
		if !o.claimRound(productionRole, q.key, check.Until) {
			o.deps.Log.Global().Info(opProduction, "a round for this production check runs already; this pass leaves it to that round", fields)
			leave()
			continue
		}
		c := *check
		run := func() {
			defer leave()
			defer o.releaseRound(productionRole, q.key, c.Until)
			o.productionRound(round, q, c)
		}
		if !o.async {
			run()
			continue
		}
		inflight.Go(run)
	}
}

// endClosedChildren ends every open child of Q whose workspace closed: one
// whose merge landed lands its sites as fixed, any other releases them.
func (o *orchestrator) endClosedChildren(ctx context.Context, q checkedQueue, fields dlog.Context) {
	children, err := o.deps.DB.ProductionChildrenOf(ctx, q.key)
	if err != nil {
		o.mediationFailedUnder(ctx, opProduction, fields, logsite.Msg("could not read the production children to end those that closed"), err)
		return
	}
	for _, child := range children {
		record, err := o.deps.DB.Workspace(ctx, child.Workspace)
		how, at := wsm.ProductionChildClosed, o.deps.Now()
		switch {
		case errors.Is(err, wsm.ErrNotFound):
		case err != nil:
			o.mediationFailedUnder(ctx, opProduction, withField(fields, "child", string(child.Workspace)), logsite.Msg("could not read a production child's workspace"), err)
			continue
		case !record.Closed:
			continue
		case record.MergedAt != nil:
			how, at = wsm.ProductionChildLanded, *record.MergedAt
		}
		if _, err := o.deps.DB.EndProductionChild(ctx, child.Workspace, how, at); err != nil {
			o.mediationFailedUnder(ctx, opProduction, withField(fields, "child", string(child.Workspace)), logsite.Msg("could not end a production child whose workspace closed; its sites stay owned"), err)
			continue
		}
		o.deps.Meter.ObserveProductionChild(string(how))
		o.deps.Log.Global().Info(opProduction, "a production child ended; its sites are released", withField(withField(fields, "child", string(child.Workspace)), "ending", string(how)))
	}
}

// productionRound tells Q's production mediator of span C when it was not
// told yet, awaits the turn it was told in, and settles a turn that decided
// nothing (productionTurnEnded). A failure is recorded at ERROR and leaves
// the span due: the next pass tells it again.
func (o *orchestrator) productionRound(ctx context.Context, q checkedQueue, c wsm.ProductionCheck) {
	fields := dlog.Context{"repo": string(q.key), "span_since": c.Since.UTC().Format(time.RFC3339Nano), "span_until": c.Until.UTC().Format(time.RFC3339Nano)}
	brief, err := o.productionBrief(ctx, q, c)
	if err != nil {
		o.mediationFailedUnder(ctx, opProduction, fields, logsite.Msg("the production check's brief could not be composed; the span stays due"), err)
		return
	}
	mediator, err := o.readyMediator(ctx, productionRole, q, fields)
	if err != nil {
		return
	}
	ws := mediator.ws
	fields = withField(fields, "mediator", string(ws))
	var closed wsm.TurnClose
	if !c.Prompted() {
		// THE MARK AND THE SUBMIT RUN UNDER THE PRODUCTION LOCK, so a pause
		// either drops the span before the mark reads it or finds the brief
		// submitted and retires it (productionpause.go).
		var (
			turn   ids.TurnID
			marked bool
			failed turnFailure
		)
		o.underProductionLock(q.key, func() {
			turn, marked, failed, err = o.submitMarkedTurn(ctx, ws, brief, conversationv1.PromptOrigin_PROMPT_ORIGIN_PRODUCTION_MEDIATION, func(turn ids.TurnID) (bool, error) {
				return o.deps.DB.MarkProductionCheckPrompted(ctx, q.key, c.Until, o.deps.Now(), turn)
			})
		})
		switch {
		case failed == turnMarkFailed:
			o.mediationFailedUnder(ctx, opProduction, fields, logsite.Msg("could not record that the production mediator is told of the span; the span stays due"), err)
			return
		case !marked:
			o.deps.Log.Global().Info(opProduction, "the production check was decided before its mediator was told; this round ends", fields)
			return
		case failed == turnSubmitFailed:
			o.mediationFailedUnder(ctx, opProduction, fields, logsite.Msg("the production mediator's brief could not be submitted; the next pass submits it under the turn recorded"), err)
			return
		}
		if !mediator.briefed {
			o.markBriefed(ctx, productionRole, q, fields)
		}
		c.Turn = turn
		o.deps.Log.Global().Info(opProduction, "the production mediator was told of the span", withField(fields, "turn", string(turn)))
		closed, err = o.deps.AwaitTurnEnd(ctx, ws, turn)
		if err != nil {
			o.mediationFailedUnder(ctx, opProduction, fields, logsite.Msg("the production mediator's turn could not be awaited; the span stays due"), err)
			return
		}
	} else {
		told := brief
		if c.Resumed {
			if told, err = o.productionResumeBrief(c); err != nil {
				o.mediationFailedUnder(ctx, opProduction, fields, logsite.Msg("the undecided production mediator's resuming brief could not be composed; the span stays due"), err)
				return
			}
		}
		o.deps.Log.Global().Info(opProduction, "the production mediator was told of the span before; its turn is reattached", withField(withField(fields, "turn", string(c.Turn)), "resumed", c.Resumed))
		var ended bool
		closed, ended, err = o.reattachProductionTurn(ctx, q, ws, c, told, fields)
		if err != nil {
			o.mediationFailedUnder(ctx, opProduction, fields, logsite.Msg("the production mediator's reattached turn could not be awaited; the span stays due"), err)
			return
		}
		if !ended {
			o.deps.Log.Global().Info(opProduction, "the span was decided, or dropped by a pause, before its turn was reattached; this round ends", fields)
			return
		}
	}
	o.productionTurnEnded(ctx, q, ws, c, closed, fields)
}

// productionTurnEnded settles span TOLD once the turn the mediator was told
// of it in ended: a span decided is done; a turn the user interrupted leaves
// the span to the user's turns; a first undecided end resumes the mediator
// once to decide it; a second is recorded unanswered at ERROR, so the next
// span is told.
func (o *orchestrator) productionTurnEnded(ctx context.Context, q checkedQueue, ws ids.WorkspaceID, told wsm.ProductionCheck, closed wsm.TurnClose, fields dlog.Context) {
	now, err := o.deps.DB.ProductionCheckOf(ctx, q.key)
	if err != nil {
		o.mediationFailedUnder(ctx, opProduction, fields, logsite.Msg("could not read the production check the mediator's turn ended on; the next pass settles it"), err)
		return
	}
	if now == nil || !now.Until.Equal(told.Until) {
		pause, err := o.deps.DB.ProductionPauseOf(ctx, q.key)
		switch {
		case err != nil:
			o.mediationFailedUnder(ctx, opProduction, fields, logsite.Msg("could not read whether the production checks are paused at the mediator's turn end"), err)
		case pause.Paused:
			o.deps.Log.Global().Info(opProduction, "the production mediator's turn ended under a pause; its span was dropped, never reviewed", fields)
		default:
			o.deps.Log.Global().Info(opProduction, "the production mediator's turn ended with its span decided", fields)
		}
		return
	}
	if closed == wsm.CloseKilled {
		o.deps.Log.Global().Info(opProduction, "the user interrupted the production mediator's turn; the span is left to the user's turns with it", fields)
		return
	}
	if !now.Resumed {
		o.resumeProduction(ctx, q, ws, *now, fields)
		return
	}
	if err := o.deps.DB.DecideProductionCheck(ctx, q.key, told.Until, o.deps.Now(), wsm.ProductionUnanswered, "the production mediator ended its turn without deciding the span, again after it was resumed to decide it", nil); err != nil {
		o.mediationFailedUnder(ctx, opProduction, fields, logsite.Msg("could not record the production check unanswered; the next pass settles it"), err)
		return
	}
	o.deps.Meter.ObserveProductionCheck(ProductionCheckUnanswered, 0)
	o.deps.Log.Global().Error(opProduction, "the production mediator's turn ended deciding nothing again after it was resumed to decide; the span is recorded unanswered and the next one is told", fields)
}

// productionResumeBrief is the prompt resuming the production mediator
// undecided on span C.
func (o *orchestrator) productionResumeBrief(c wsm.ProductionCheck) (string, error) {
	return o.deps.Briefs(BriefProductionIntakeResumed, map[string]string{
		"span_since": c.Since.UTC().Format(time.RFC3339),
		"span_until": c.Until.UTC().Format(time.RFC3339),
	})
}

// resumeProduction resumes production mediator WS, whose turn on span C
// ended deciding nothing, in a turn recorded on C before it is submitted,
// then settles what that turn decided.
func (o *orchestrator) resumeProduction(ctx context.Context, q checkedQueue, ws ids.WorkspaceID, c wsm.ProductionCheck, fields dlog.Context) {
	brief, err := o.productionResumeBrief(c)
	if err != nil {
		o.mediationFailedUnder(ctx, opProduction, fields, logsite.Msg("the undecided production mediator's resuming brief could not be composed; the span stays due"), err)
		return
	}
	// Under the production lock, as productionRound's first telling.
	var (
		turn   ids.TurnID
		marked bool
		failed turnFailure
	)
	o.underProductionLock(q.key, func() {
		turn, marked, failed, err = o.submitMarkedTurn(ctx, ws, brief, conversationv1.PromptOrigin_PROMPT_ORIGIN_PRODUCTION_MEDIATION, func(turn ids.TurnID) (bool, error) {
			return o.deps.DB.MarkProductionCheckResumed(ctx, q.key, c.Until, turn)
		})
	})
	fields = withField(fields, "turn", string(turn))
	switch {
	case failed == turnMarkFailed:
		o.mediationFailedUnder(ctx, opProduction, fields, logsite.Msg("could not record that the undecided production mediator is resumed; the span stays due"), err)
		return
	case !marked:
		o.deps.Log.Global().Info(opProduction, "the production check was decided before its undecided mediator was resumed; this round ends", fields)
		return
	case failed == turnSubmitFailed:
		o.mediationFailedUnder(ctx, opProduction, fields, logsite.Msg("the undecided production mediator's resuming brief could not be submitted; the next pass submits it"), err)
		return
	}
	o.deps.Log.Global().Warn(opProduction, "the production mediator's turn ended deciding nothing about the span; it is resumed once to decide it", fields)
	c.Resumed, c.Turn = true, turn
	closed, err := o.deps.AwaitTurnEnd(ctx, ws, turn)
	if err != nil {
		o.mediationFailedUnder(ctx, opProduction, fields, logsite.Msg("the production mediator's turn could not be awaited; the span stays due"), err)
		return
	}
	o.productionTurnEnded(ctx, q, ws, c, closed, fields)
}

// reattachProductionTurn brings span C's told turn on again, under Q's
// production lock and only while C is still the span owed, then awaits its
// end. It answers false, bringing nothing on, for a span a pause dropped.
func (o *orchestrator) reattachProductionTurn(ctx context.Context, q checkedQueue, ws ids.WorkspaceID, c wsm.ProductionCheck, told string, fields dlog.Context) (wsm.TurnClose, bool, error) {
	var (
		place  turnPlace
		closed wsm.TurnClose
		owed   bool
		err    error
	)
	o.underProductionLock(q.key, func() {
		var now *wsm.ProductionCheck
		if now, err = o.deps.DB.ProductionCheckOf(ctx, q.key); err != nil || now == nil || !now.Until.Equal(c.Until) {
			return
		}
		owed = true
		place, closed, err = o.bringTurnOn(ctx, ws, fields, c.Turn, told, conversationv1.PromptOrigin_PROMPT_ORIGIN_PRODUCTION_MEDIATION)
	})
	if err != nil || !owed {
		return wsm.CloseFailed, false, err
	}
	if place == turnEnded {
		return closed, true, nil
	}
	closed, err = o.deps.AwaitTurnEnd(ctx, ws, c.Turn)
	return closed, true, err
}

// ProductionSitePath is where the site file of the span ending UNTIL is
// written, in the queue's logs directory DIR, pruned with its other logs.
func ProductionSitePath(dir string, until time.Time) string {
	return filepath.Join(dir, fmt.Sprintf("production_check-%d.log", until.Unix()))
}

// productionBrief composes span C's brief: the span, the build that served
// it, and the file of its new and recurring sites (ProductionSites).
func (o *orchestrator) productionBrief(ctx context.Context, q checkedQueue, c wsm.ProductionCheck) (string, error) {
	sites, err := o.productionSites(ctx, q, c)
	if err != nil {
		return "", err
	}
	path := ProductionSitePath(LogsDir(o.deps.StateDir), c.Until)
	if err := writeLog(path, RenderProductionSites(sites, c)); err != nil {
		return "", fmt.Errorf("merge: write the production check's site file: %w", err)
	}
	children, err := o.deps.DB.ProductionChildrenOf(ctx, q.key)
	if err != nil {
		return "", fmt.Errorf("merge: read the open production children: %w", err)
	}
	return o.deps.Briefs(BriefProductionIntake, map[string]string{
		"span_since":     c.Since.UTC().Format(time.RFC3339),
		"span_until":     c.Until.UTC().Format(time.RFC3339),
		"new_sites":      fmt.Sprint(len(sites.New)),
		"refired_sites":  fmt.Sprint(len(sites.Refired)),
		"site_file":      path,
		"baseline":       sites.Baseline,
		"open_children":  renderOpenChildren(o.childNames(ctx, children), children),
		"metrics_window": fmt.Sprintf("%s to %s", c.Since.UTC().Format(time.RFC3339), c.Until.UTC().Format(time.RFC3339)),
	})
}

// childNames answers each child's workspace name, its id when the name
// cannot be read (recorded at ERROR).
func (o *orchestrator) childNames(ctx context.Context, children []wsm.ProductionChild) map[ids.WorkspaceID]string {
	out := map[ids.WorkspaceID]string{}
	for _, child := range children {
		name, err := o.workspaceName(ctx, opProduction, child.Workspace)
		if err != nil || name == "" {
			name = string(child.Workspace)
		}
		out[child.Workspace] = name
	}
	return out
}

// renderOpenChildren lists CHILDREN, each by name with the sites it owns.
func renderOpenChildren(names map[ids.WorkspaceID]string, children []wsm.ProductionChild) string {
	if len(children) == 0 {
		return "none"
	}
	var b strings.Builder
	for _, child := range children {
		b.WriteString(fmt.Sprintf("\n- `%s` (%s): %s", names[child.Workspace], child.Kind, strings.Join(child.Sites, ", ")))
	}
	return b.String()
}

// ProductionSites are a span's sites the mediator reviews.
type ProductionSites struct {
	// New are the warning and error sites that fired in the span and not in
	// the span before it.
	New []logsites.Site
	// Refired are the sites a child's fix landed for that fired again since,
	// with when the fix landed.
	Refired []RefiredSite
	// Owned is how many of the span's sites an open child owns, left out.
	Owned int
	// Unshipped is how many of the span's sites a child's fix landed for
	// only after the span ended, left out: the build that served the span
	// did not carry the fix, so the site firing in it says nothing of it.
	Unshipped int
	// Ignored is how many of the span's sites matched an ignored kind, left
	// out.
	Ignored int
	// Baseline says what the span was compared against.
	Baseline string
	// From and To are the hours the tally's answer covered, and Partial
	// that the span began before its oldest kept row, so nothing before
	// From was read (logsites.Answer).
	From, To time.Time
	Partial  bool
	// Unrecorded is that the span ended before the tally's oldest kept row
	// (OldestKept), so none of it was read: it lists no site, and the
	// mediator is told to read the span from the raw logs.
	Unrecorded bool
	OldestKept time.Time
}

// RefiredSite is a fixed site that fired again.
type RefiredSite struct {
	Site    logsites.Site
	FixedAt time.Time
	FixedBy ids.WorkspaceID
}

// productionSites reads span C's sites from the tally and compares them with
// the span before it (DiffProductionSites). A span whose rows the tally no
// longer keeps is still told, with no sites and the loss declared
// (ProductionSites.Unrecorded), recorded at WARN: failing it would leave it
// due forever, and every span after it waiting.
func (o *orchestrator) productionSites(ctx context.Context, q checkedQueue, c wsm.ProductionCheck) (ProductionSites, error) {
	span, err := o.deps.LogSites.Window(ctx, c.Since, c.Until, "warn")
	if unrecorded := (*logsites.UnrecordedWindowError)(nil); errors.As(err, &unrecorded) {
		o.deps.Log.Global().Warn(opProduction, "the span's log sites were pruned before its mediator read them; it is told to read the span from the raw logs", dlog.Context{
			"repo":        string(q.key),
			"span_since":  c.Since.UTC().Format(time.RFC3339Nano),
			"span_until":  c.Until.UTC().Format(time.RFC3339Nano),
			"oldest_kept": unrecorded.Oldest.UTC().Format(time.RFC3339Nano),
		})
		return ProductionSites{Baseline: "nothing: the span was not read from the tally", Unrecorded: true, OldestKept: unrecorded.Oldest}, nil
	}
	if err != nil {
		return ProductionSites{}, fmt.Errorf("merge: read the span's log sites: %w", err)
	}
	var before *logsites.Answer
	baseline := "no earlier span: every site that fired is listed as new"
	prev, found, err := o.deps.DB.PreviousProductionSpan(ctx, q.key, c.Until)
	if err != nil {
		return ProductionSites{}, fmt.Errorf("merge: read the span before: %w", err)
	}
	if found {
		answer, err := o.deps.LogSites.Window(ctx, prev.Since, prev.Until, "warn")
		var unrecorded *logsites.UnrecordedWindowError
		switch {
		case errors.As(err, &unrecorded):
			baseline = "the span before is no longer kept: every site that fired is listed as new"
		case err != nil:
			return ProductionSites{}, fmt.Errorf("merge: read the span before's log sites: %w", err)
		default:
			before = &answer
			baseline = fmt.Sprintf("the span before, %s to %s", prev.Since.UTC().Format(time.RFC3339), prev.Until.UTC().Format(time.RFC3339))
		}
	}
	owned, err := o.deps.DB.OwnedSites(ctx, q.key)
	if err != nil {
		return ProductionSites{}, fmt.Errorf("merge: read the sites open children own: %w", err)
	}
	fixed, err := o.deps.DB.FixedSites(ctx, q.key)
	if err != nil {
		return ProductionSites{}, fmt.Errorf("merge: read the sites children fixed: %w", err)
	}
	kinds, err := o.deps.DB.IgnoredRecordKinds(ctx)
	if err != nil {
		return ProductionSites{}, fmt.Errorf("merge: read the kinds of record the checks leave out: %w", err)
	}
	out := DiffProductionSites(span, c.Until, before, owned, fixed, kinds)
	out.Baseline = baseline
	return out, nil
}

// DiffProductionSites is the one comparison of a span's sites (SPAN, the
// span that ended UNTIL) with the span before it (BEFORE, nil for none): the
// sites new to the span, and those a child's fix landed for that fired again
// after it. A site an open child owns (OWNED), of an ignored kind (KINDS), or
// whose fix landed only after the span ended (FIXED) is left out of both, and
// counted. UNTIL is the span's own end, not the tally's hours (SPAN.To), so a
// fix that landed within the span's last hour is judged by when it landed.
func DiffProductionSites(span logsites.Answer, until time.Time, before *logsites.Answer, owned map[string]wsm.WorkspaceID, fixed []wsm.FixedSite, kinds []wsm.IgnoredRecordKind) ProductionSites {
	seen := map[string]bool{}
	if before != nil {
		for _, site := range before.Sites {
			seen[site.Key] = true
		}
	}
	fixes := map[string]wsm.FixedSite{}
	for _, f := range fixed {
		fixes[f.Site] = f
	}
	out := ProductionSites{From: span.From, To: span.To, Partial: span.Partial}
	for _, site := range span.Sites {
		if _, held := owned[site.Key]; held {
			out.Owned++
			continue
		}
		if ignored(site, kinds) {
			out.Ignored++
			continue
		}
		fix, isFixed := fixes[site.Key]
		if isFixed && fix.LandedAt.After(until) {
			out.Unshipped++
			continue
		}
		if isFixed && site.LastSeen.After(fix.LandedAt) {
			out.Refired = append(out.Refired, RefiredSite{Site: site, FixedAt: fix.LandedAt, FixedBy: fix.Workspace})
			continue
		}
		if !seen[site.Key] {
			out.New = append(out.New, site)
		}
	}
	return out
}

// ignored reports whether SITE's records are of one of KINDS, judged by its
// operation and its sample.
func ignored(site logsites.Site, kinds []wsm.IgnoredRecordKind) bool {
	for _, k := range kinds {
		if k.Matches(site.Operation, site.SampleMessage) {
			return true
		}
	}
	return false
}

// RenderProductionSites is the site file of span C: how much of it the tally
// covered, its new sites, errors first and the most frequent first, then its
// refired ones, each with its template, counts and one sample.
func RenderProductionSites(sites ProductionSites, c wsm.ProductionCheck) string {
	var b strings.Builder
	b.WriteString(fmt.Sprintf("Production span %s to %s, compared with %s.\n", c.Since.UTC().Format(time.RFC3339), c.Until.UTC().Format(time.RFC3339), sites.Baseline))
	switch {
	case sites.Unrecorded:
		since, until := c.Since.UTC().Format(time.RFC3339), c.Until.UTC().Format(time.RFC3339)
		b.WriteString(fmt.Sprintf("UNRECORDED: the tally keeps no row before %s, so none of this span was read from it; read the span from the raw logs with `bin/logs.sh --all --level warn --since %s --until %s --tally --sample 1`.\n",
			sites.OldestKept.UTC().Format(time.RFC3339), since, until))
	case sites.Partial:
		b.WriteString(fmt.Sprintf("PARTIAL: the tally keeps no row before %s, so it read only %s to %s; what fired before then is unknown, not absent.\n",
			sites.From.UTC().Format(time.RFC3339), sites.From.UTC().Format(time.RFC3339), sites.To.UTC().Format(time.RFC3339)))
	default:
		b.WriteString(fmt.Sprintf("Read whole from the tally: %s to %s.\n", sites.From.UTC().Format(time.RFC3339), sites.To.UTC().Format(time.RFC3339)))
	}
	b.WriteString(fmt.Sprintf("Left out: %d sites open children own, %d sites whose fix landed, not yet in production, %d sites of ignored kinds.\n", sites.Owned, sites.Unshipped, sites.Ignored))
	ordered := append([]logsites.Site(nil), sites.New...)
	sort.SliceStable(ordered, func(i, j int) bool {
		if (ordered[i].Level == "error") != (ordered[j].Level == "error") {
			return ordered[i].Level == "error"
		}
		return ordered[i].Count > ordered[j].Count
	})
	b.WriteString(fmt.Sprintf("\n## New sites (%d)\n", len(ordered)))
	for _, site := range ordered {
		renderSite(&b, site)
	}
	b.WriteString(fmt.Sprintf("\n## Sites that fired again after their fix landed (%d)\n", len(sites.Refired)))
	for _, r := range sites.Refired {
		renderSite(&b, r.Site)
		b.WriteString(fmt.Sprintf("  fixed: %s by %s\n", r.FixedAt.UTC().Format(time.RFC3339), r.FixedBy))
	}
	return b.String()
}

// renderSite writes one site of the site file.
func renderSite(b *strings.Builder, site logsites.Site) {
	b.WriteString(fmt.Sprintf("\n%s %s %s x%d, first %s, last %s\n", site.Level, site.Runtime, site.Key, site.Count,
		site.FirstSeen.UTC().Format(time.RFC3339), site.LastSeen.UTC().Format(time.RFC3339)))
	b.WriteString(fmt.Sprintf("  template: %s\n  operation: %s\n  sample: %s\n  context: %s\n", site.Template, site.Operation, site.SampleMessage, site.SampleContext))
}

// DecideProduction implements Orchestrator: production mediator WS decides
// the span it was told. A dispatch makes every child, each commissioned with
// its findings and its siblings, then records them with the sites they own;
// a refusal at any step undoes every child made, so a dispatch is whole or
// nothing. It answers the children made, in the request's order.
func (o *orchestrator) DecideProduction(ctx context.Context, ws ids.WorkspaceID, d ProductionDecisionRequest) ([]wsm.Workspace, error) {
	fields := dlog.Context{"workspace": string(ws), "children": len(d.Children)}
	q, c, err := o.toldProductionCheck(ctx, ws, fields)
	if err != nil {
		return nil, err
	}
	fields = withField(withField(fields, "repo", string(q.key)), "span_until", c.Until.UTC().Format(time.RFC3339Nano))
	if len(d.Children) == 0 {
		if err := o.deps.DB.DecideProductionCheck(ctx, q.key, c.Until, o.deps.Now(), wsm.ProductionNothingToDo, d.Summary, nil); err != nil {
			return nil, o.productionDecisionFailed(fields, err)
		}
		o.deps.Meter.ObserveProductionCheck(ProductionCheckNothingToDo, 0)
		o.deps.Log.Global().Info(opProduction, "the production mediator found nothing to do in the span", withField(fields, "summary", d.Summary))
		return nil, nil
	}
	if err := o.refuseOwnedSites(ctx, q, d.Children, fields); err != nil {
		return nil, err
	}
	names := childNamesFor(d.Children, c.Until)
	made := make([]wsm.Workspace, 0, len(d.Children))
	undo := func() {
		for _, child := range made {
			if err := o.deps.ProductionChildren.Undo(ctx, child.ID); err != nil {
				o.deps.Log.Global().Error(opProduction, "a child of a refused dispatch could not be undone", withField(withField(fields, "child", string(child.ID)), "error", err.Error()))
			}
		}
	}
	for i, child := range d.Children {
		prompt, err := o.childCommission(ws, names, i, child)
		if err != nil {
			undo()
			return nil, o.productionDecisionFailed(fields, err)
		}
		created, err := o.deps.ProductionChildren.CreateChild(ctx, q.repository.Dir, ws, names[i], prompt)
		if err != nil {
			undo()
			o.deps.Log.Global().Warn(opProduction, "a production child could not be made; every child of the dispatch is undone", withField(withField(fields, "child", i), "error", err.Error()))
			return nil, refuseWith(ArmChildRefused, ws, err.Error(), map[string]any{"child": i})
		}
		made = append(made, created)
	}
	records := make([]wsm.ProductionChild, len(made))
	for i, child := range made {
		records[i] = wsm.ProductionChild{Workspace: child.ID, Kind: d.Children[i].Kind, Sites: childSites(d.Children[i])}
	}
	if err := o.deps.DB.DecideProductionCheck(ctx, q.key, c.Until, o.deps.Now(), wsm.ProductionDispatched, d.Summary, records); err != nil {
		undo()
		var owned *wsm.SiteOwnedError
		if errors.As(err, &owned) {
			return nil, siteOwnedRefusal(ws, owned)
		}
		return nil, o.productionDecisionFailed(fields, err)
	}
	o.deps.Meter.ObserveProductionCheck(ProductionCheckDispatched, len(made))
	o.deps.Log.Global().Info(opProduction, "the production mediator dispatched its children", withField(fields, "summary", d.Summary))
	return made, nil
}

// toldProductionCheck answers the repository WS is the production mediator
// of, and the span it was told and is to decide.
func (o *orchestrator) toldProductionCheck(ctx context.Context, ws ids.WorkspaceID, fields dlog.Context) (checkedQueue, wsm.ProductionCheck, error) {
	repo, isMediator, err := o.deps.DB.StandingMediatorRepoOf(ctx, wsm.MediatorProduction, ws)
	if err != nil {
		o.deps.Log.Global().Error(opProduction, "could not read whether the deciding workspace is a production mediator", withField(fields, "error", err.Error()))
		return checkedQueue{}, wsm.ProductionCheck{}, err
	}
	if !isMediator {
		o.deps.Log.Global().Warn(opProduction, "refused a production decision from a workspace that is no repository's production mediator", fields)
		return checkedQueue{}, wsm.ProductionCheck{}, refuse(ArmNotProductionMediator, ws, "this workspace is no repository's production mediator")
	}
	queues, err := o.productionQueues(ctx, opProduction)
	if err != nil {
		return checkedQueue{}, wsm.ProductionCheck{}, err
	}
	q, found := queueKeyed(queues, repo)
	if !found {
		o.deps.Log.Global().Warn(opProduction, "refused a production decision in a repository that checks its production no more", withField(fields, "repo", string(repo)))
		return checkedQueue{}, wsm.ProductionCheck{}, refuse(ArmNoProductionCheckOwed, ws, "the repository checks its production no more")
	}
	if err := o.refuseWhilePaused(ctx, q.key, ws, "decide", fields); err != nil {
		return checkedQueue{}, wsm.ProductionCheck{}, err
	}
	c, err := o.deps.DB.ProductionCheckOf(ctx, q.key)
	if err != nil {
		o.deps.Log.Global().Error(opProduction, "could not read the production check to decide", withField(fields, "error", err.Error()))
		return checkedQueue{}, wsm.ProductionCheck{}, err
	}
	if c == nil || !c.Prompted() {
		o.deps.Log.Global().Warn(opProduction, "refused a production decision while no told span is owed", fields)
		return checkedQueue{}, wsm.ProductionCheck{}, refuse(ArmNoProductionCheckOwed, ws, "no span the mediator was told is owed")
	}
	return q, *c, nil
}

// refuseOwnedSites refuses a dispatch naming a site an open child of Q owns,
// before anything is made.
func (o *orchestrator) refuseOwnedSites(ctx context.Context, q checkedQueue, children []ProductionChildRequest, fields dlog.Context) error {
	owned, err := o.deps.DB.OwnedSites(ctx, q.key)
	if err != nil {
		o.deps.Log.Global().Error(opProduction, "could not read the sites open children own", withField(fields, "error", err.Error()))
		return err
	}
	named := map[string]bool{}
	for i, child := range children {
		for _, site := range childSites(child) {
			if holder, held := owned[site]; held {
				o.deps.Log.Global().Info(opProduction, "refused a dispatch naming a site an open child owns", withField(withField(fields, "site", site), "holder", string(holder)))
				return siteOwnedRefusal("", &wsm.SiteOwnedError{Site: site, Holder: holder})
			}
			if named[site] {
				o.deps.Log.Global().Info(opProduction, "refused a dispatch naming one site in two children", withField(withField(fields, "site", site), "child", i))
				return siteOwnedRefusal("", &wsm.SiteOwnedError{Site: site, Index: i})
			}
			named[site] = true
		}
	}
	return nil
}

// refuseWith is a refusal under ARM of WS's request, REASON verbatim, with
// FIELDS the arm's message is filled from.
func refuseWith(arm string, ws ids.WorkspaceID, reason string, fields map[string]any) *RefusalError {
	return &RefusalError{Arm: arm, Workspace: ws, Reason: reason, Fields: fields}
}

// siteOwnedRefusal is the site_owned refusal of E.
func siteOwnedRefusal(ws ids.WorkspaceID, e *wsm.SiteOwnedError) error {
	fields := map[string]any{"site": e.Site}
	if e.Holder != "" {
		fields["holder"] = string(e.Holder)
	} else {
		fields["index"] = e.Index
	}
	return refuseWith(ArmSiteOwned, ws, e.Error(), fields)
}

// productionDecisionFailed records a production decision that failed for
// something other than a refusal, and answers ERR.
func (o *orchestrator) productionDecisionFailed(fields dlog.Context, err error) error {
	if errors.Is(err, wsm.ErrNoProductionCheckOwed) {
		return refuse(ArmNoProductionCheckOwed, "", "no span the mediator was told is owed")
	}
	o.deps.Log.Global().Error(opProduction, "the production decision could not be recorded; nothing of it stands", withField(fields, "error", err.Error()))
	return err
}

// childSites are every site CHILD's findings cover, in order, once each.
func childSites(child ProductionChildRequest) []string {
	var out []string
	seen := map[string]bool{}
	for _, f := range child.Findings {
		for _, site := range f.Sites {
			if !seen[site] {
				seen[site] = true
				out = append(out, site)
			}
		}
	}
	return out
}

// childNamesFor names each child of a dispatch: the name it asked for, or
// one minted from its first finding, `prod-` and the statement's first words,
// and the span's end so two dispatches never mint one name.
func childNamesFor(children []ProductionChildRequest, until time.Time) []string {
	out := make([]string, len(children))
	for i, child := range children {
		if child.Name != "" {
			out[i] = child.Name
			continue
		}
		out[i] = fmt.Sprintf("prod-%s-%d-%d", slugWords(child.Findings[0].Statement, 5), until.Unix()%100000, i+1)
	}
	return out
}

// slugWords is the first N words of TEXT, lowercased, letters and digits
// only, joined by hyphens.
func slugWords(text string, n int) string {
	var words []string
	for _, field := range strings.Fields(strings.ToLower(text)) {
		word := strings.Map(func(r rune) rune {
			if (r >= 'a' && r <= 'z') || (r >= '0' && r <= '9') {
				return r
			}
			return -1
		}, field)
		if word != "" {
			words = append(words, word)
		}
		if len(words) == n {
			break
		}
	}
	if len(words) == 0 {
		return "finding"
	}
	return strings.Join(words, "-")
}

// childCommission is child I's commission: its kind's work, its findings,
// and every sibling by name with its findings' statements, so it knows whom
// its work might converge with.
func (o *orchestrator) childCommission(mediator ids.WorkspaceID, names []string, i int, child ProductionChildRequest) (string, error) {
	var findings strings.Builder
	for _, f := range child.Findings {
		findings.WriteString(fmt.Sprintf("\n- %s", f.Statement))
		if len(f.Sites) > 0 {
			findings.WriteString(fmt.Sprintf("\n  - Log sites: %s", strings.Join(f.Sites, ", ")))
		}
		for _, e := range f.Evidence {
			findings.WriteString(fmt.Sprintf("\n  - Evidence: %s", e))
		}
	}
	var siblings strings.Builder
	for j, name := range names {
		if j != i {
			siblings.WriteString(fmt.Sprintf("\n- `%s`", name))
		}
	}
	if siblings.Len() == 0 {
		siblings.WriteString("none")
	}
	return o.deps.Briefs(BriefProductionChild, map[string]string{
		"kind":     string(child.Kind),
		"findings": findings.String(),
		"siblings": siblings.String(),
	})
}

// FoldProduction implements Orchestrator: production mediator WS folds child
// FOLDED into SURVIVING. The sites move first, in one write; then the
// survivor is told the folded child's branch and findings, and the folded
// child is closed.
func (o *orchestrator) FoldProduction(ctx context.Context, ws, folded, surviving ids.WorkspaceID) error {
	fields := dlog.Context{"workspace": string(ws), "folded": string(folded), "surviving": string(surviving)}
	repo, isMediator, err := o.deps.DB.StandingMediatorRepoOf(ctx, wsm.MediatorProduction, ws)
	if err != nil {
		o.deps.Log.Global().Error(opProduction, "could not read whether the folding workspace is a production mediator", withField(fields, "error", err.Error()))
		return err
	}
	if !isMediator {
		o.deps.Log.Global().Warn(opProduction, "refused a fold from a workspace that is no repository's production mediator", fields)
		return refuse(ArmNotProductionMediator, ws, "this workspace is no repository's production mediator")
	}
	if err := o.refuseWhilePaused(ctx, repo, ws, "fold", fields); err != nil {
		return err
	}
	for _, side := range []ids.WorkspaceID{folded, surviving} {
		child, found, err := o.deps.DB.ProductionChildOf(ctx, side)
		if err != nil {
			o.deps.Log.Global().Error(opProduction, "could not read a child the fold names", withField(withField(fields, "child", string(side)), "error", err.Error()))
			return err
		}
		if !found || !child.Open() || !sameQueue(child.Repo, repo) {
			o.deps.Log.Global().Info(opProduction, "refused a fold naming a workspace that is no open child of this mediator", withField(fields, "child", string(side)))
			return refuseWith(ArmNotFoldable, ws, "not an open child of this production mediator", map[string]any{"workspace": string(side)})
		}
	}
	foldedRecord, err := o.deps.DB.Workspace(ctx, folded)
	if err != nil {
		o.deps.Log.Global().Error(opProduction, "could not read the folded child's workspace", withField(fields, "error", err.Error()))
		return err
	}
	if err := o.deps.DB.FoldProductionChild(ctx, folded, surviving, o.deps.Now()); err != nil {
		if errors.Is(err, wsm.ErrNotFoldable) {
			return refuseWith(ArmNotFoldable, ws, err.Error(), map[string]any{"workspace": string(folded)})
		}
		o.deps.Log.Global().Error(opProduction, "the fold could not be recorded; nothing moved", withField(fields, "error", err.Error()))
		return err
	}
	brief, err := o.deps.Briefs(BriefProductionChildFolded, map[string]string{
		"folded": foldedRecord.Name, "branch": foldedRecord.Branch, "dir": foldedRecord.Dir,
	})
	if err != nil {
		o.deps.Log.Global().Error(opProduction, "the surviving child's fold brief could not be composed; it owns the folded child's sites but was not told", withField(fields, "error", err.Error()))
		return err
	}
	if _, err := o.submitCheckPrompt(ctx, surviving, wsm.NewTurnID(), brief, conversationv1.PromptOrigin_PROMPT_ORIGIN_PRODUCTION_MEDIATION); err != nil {
		o.deps.Log.Global().Error(opProduction, "the surviving child could not be told of the fold; it owns the folded child's sites but was not told", withField(fields, "error", err.Error()))
		return err
	}
	if err := o.deps.ProductionChildren.Close(ctx, folded); err != nil {
		o.deps.Log.Global().Error(opProduction, "the folded child could not be closed; its sites are the survivor's", withField(fields, "error", err.Error()))
		return err
	}
	o.deps.Meter.ObserveProductionChild(ProductionChildFolded)
	o.deps.Log.Global().Info(opProduction, "folded one production child into another", fields)
	return nil
}
