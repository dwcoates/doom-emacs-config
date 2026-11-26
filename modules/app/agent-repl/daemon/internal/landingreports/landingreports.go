// Package landingreports tells a workspace how its merge ended and every
// workspace whose landing a deploy carries how that deploy ended: the two
// reports a merge owes beyond the merge itself. The merge package reaches the
// first only through merge.HandBacks; the deploy and the rollout reach the
// second through CoverLandings and DeployEnded.
//
// A MERGE THAT LANDED CLOSED ITS WORKSPACE, so it hands nothing back to it: a
// child's landing reaches its parent as the child's one update, once the
// build that carries it runs (wsm.ParentFollowedEvents, wsedges.Hub). A merge
// that did not land is handed back to its own workspace, which stays open.
package landingreports

import (
	"context"
	"fmt"

	"agentrepl/logging/logsite"
	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/standdown"
	"agent-repld/internal/wsm"
)

// The operation every record of this package carries.
const op = "daemon.landingreports"

// BriefHandback is the brief a merge's handback is composed from. Its first
// line is the badge label (feed.MergeHandbackLabel).
const BriefHandback = "merge-handback"

// Prompts submits a handback to the workspace's session.
type Prompts = prompthandler.Submitter

// DeployReporter tells the workspaces whose landing a deploy carries that it
// ended.
type DeployReporter interface {
	DeployEnded(end merge.DeployEnd)
}

// Deps are the service's collaborators.
type Deps struct {
	DB      wsm.DB
	Prompts Prompts
	// Briefs loads and splices a brief at use time.
	Briefs merge.BriefLoader
	// MintKey mints an idempotency key for one submission
	// (prompthandler.MintKey): every handback is its own submission.
	MintKey func(prefix string) (string, error)
	Log     dlog.Surfaces
	// Deploys tells each deploy's end to the workspaces whose landing it
	// carries (wsedges.Hub.DeployEnded). Every deploy ends in DeployEnded,
	// so this is the one place a deploy's end is told.
	Deploys DeployReporter
}

// Service is the landing reports. It implements merge.HandBacks.
type Service struct {
	deps Deps
}

var _ merge.HandBacks = (*Service)(nil)

// New builds the service.
func New(deps Deps) *Service {
	return &Service{deps: deps}
}

// HandBack reports how WS's own merge ended to WS's conversation: a deferred
// prompt, so it never interrupts a running turn, whose first line is the
// feed's handback badge. Only a merge that did not land is reported: a landing
// closed the workspace.
//
// A MERGE THE USER OR THE OPERATOR RELEASED (merge.Ending.Released)
// HANDS NOTHING BACK: whoever released it already knows, and its bubble shows
// the abandoned terminal.
func (s *Service) HandBack(ctx context.Context, ws ids.WorkspaceID, record wsm.Workspace, out merge.Ending) error {
	log := s.deps.Log.Global()
	fields := dlog.Context{"workspace": string(ws), "outcome": out.Kind.String(), "branch": out.Branch}
	if out.Released() {
		fields["cause"] = string(out.Cause)
		log.Info(op, "a released merge hands no report back: whoever released it already knows", fields)
		return nil
	}
	label, report, err := handbackText(out)
	if err != nil {
		log.Error(op, "a merge ended in a way no report says; its workspace is not told", dlog.WithCause(fields, err))
		return err
	}
	key, err := s.deps.MintKey("merge-handback")
	if err != nil {
		log.Error(op, "could not mint a merge handback's idempotency key; its workspace is not told", dlog.WithCause(fields, err))
		return err
	}
	text, err := s.deps.Briefs(BriefHandback, map[string]string{"label": label, "report": report})
	if err != nil {
		log.Error(op, "could not compose a merge's handback; its workspace is not told", dlog.WithCause(fields, err))
		return err
	}
	outcome, err := s.deps.Prompts.Submit(ctx, ws, feed.SaidText(text), key,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_HANDBACK, nil, wsm.DeliveryDeferred, nil)
	if err != nil {
		failedUnlessStoodDown(ctx, log, "handback", logsite.Msg("could not submit a merge's handback to its workspace"), fields, err)
		return err
	}
	fields["turn"] = string(outcome.Turn)
	log.Info(op, "handed a merge's outcome back to its workspace", fields)
	return nil
}

// handbackText composes the badge label and the report of a merge that did
// not land: it ran in this very workspace, which stays open.
func handbackText(out merge.Ending) (string, string, error) {
	switch out.Kind {
	case merge.EndingFailed:
		return fmt.Sprintf("merge of %s failed", out.Branch),
			fmt.Sprintf("It failed: %s. This workspace stays open; the merge's own record is its bubble in this feed.", out.Summary), nil
	case merge.EndingAbandoned:
		return fmt.Sprintf("merge of %s did not run", out.Branch),
			fmt.Sprintf("It did not run: %s. This workspace stays open.", out.Summary), nil
	}
	return "", "", fmt.Errorf("landingreports: a merge is handed back only when it did not land, not %s", out.Kind)
}

// CoverLandings marks every landing waiting for its deploy, and every landed
// merge and remediation finish still deploying, covered by the deploy
// committing now: what that deploy builds carries every landing before this
// instant. A failure is recorded and returned; what was not covered waits for
// the next deploy.
func (s *Service) CoverLandings(ctx context.Context) error {
	log := s.deps.Log.Global()
	landings, err := s.deps.DB.CoverUndeployedLandings(ctx)
	if err != nil {
		log.Error(op, "could not mark the landings waiting for a deploy covered by it; the next deploy's notice names them", dlog.WithCause(nil, err))
		return err
	}
	// THE LANDED MERGES AND REMEDIATION FINISHES STILL DEPLOYING ARE COVERED
	// HERE TOO, after the deploy committed and before it reads the checkout:
	// the check this deploy owes takes the hold of exactly the landings it
	// carries (wsm.CoverDeployingLandings).
	deploying, err := s.deps.DB.CoverDeployingLandings(ctx)
	if err != nil {
		log.Error(op, "could not mark the landed merges and remediation finishes still deploying covered by the deploy; they hold their queues until a deploy covers them", dlog.WithCause(dlog.Context{"landings": landings}, err))
		return err
	}
	log.Info(op, "the deploy covers the landings before it", dlog.Context{"landings": landings,
		"deploying_merges": deploying.Merges, "remediation_finishes": deploying.Finishes})
	return nil
}

// DeployEnded tells every workspace whose landing a deploy carries that the
// deploy has ended: only the covered landings when coveredOnly (a deploy's
// own end), else every one (a fresh boot, whose build carries everything that
// landed before it). DEPLOYERR is how that deploy ended.
func (s *Service) DeployEnded(ctx context.Context, coveredOnly bool, deployErr error) {
	log := s.deps.Log.Global()
	end := merge.DeployEnd{Boot: !coveredOnly}
	if deployErr != nil {
		end.Failure = deployErr.Error()
	}
	// THE DEPLOY HAS ENDED, whatever became of the read: the workspaces whose
	// landings it carries are told.
	defer func() { s.deps.Deploys.DeployEnded(end) }()
	landings, err := s.deps.DB.TakeDeployedLandings(ctx, coveredOnly)
	if err != nil {
		failedUnlessStoodDown(ctx, log, "deploy end", logsite.Msg("could not take the landings the deploy carries; its notice names none of them"), dlog.Context{"covered_only": coveredOnly}, err)
		return
	}
	for _, landing := range landings {
		end.Landed = append(end.Landed, landing.MergeWorkspace)
	}
}

// failedUnlessStoodDown records a step of DURING that failed with err at
// ERROR, unless the daemon was standing down: the step runs on the daemon's
// lifetime, so a step its end cut short is the orderly exit it is, recorded at
// INFO with what it left undone.
func failedUnlessStoodDown(ctx context.Context, log dlog.Logger, during string, message logsite.Message, fields dlog.Context, err error) {
	if standdown.CutShort(ctx, err) {
		stood := dlog.WithCause(fields, err)
		stood["left_undone"] = message.Template()
		log.With(stood).Info(op, "the daemon stood down mid-{during}", dlog.Context{"during": during})
		return
	}
	log.Log(dlog.LevelError, op, message, dlog.WithCause(fields, err))
}
