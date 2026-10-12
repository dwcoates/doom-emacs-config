// Package mergeworkspace runs the parts of a DETACHED MERGE that are not the
// merge itself (agentrepl.v1.MergeWorkspaceSourceOwnBranchDetached): forking
// the merge workspace the merge runs in, handing the merge's outcome back to
// the conversation that asked for it, and removing merge workspaces nobody has
// used for 15 calendar days. The merge package reaches it only through
// merge.DetachedMerges, so nothing about detached merges leaks into the merge
// run.
package mergeworkspace

import (
	"context"
	"errors"
	"fmt"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/merge"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/resolve/feed"
	"agent-repld/internal/standdown"
	"agent-repld/internal/wsm"
)

// The operation every record of this package carries.
const op = "daemon.mergeworkspace"

// BriefHandback is the brief a detached merge's handback is composed from. Its
// first line is the badge label (feed.MergeHandbackLabel).
const BriefHandback = "merge-detached-handback"

// IdleLimit is how long a merge workspace may go without activity before the
// sweep removes it: 15 calendar days (owner ruling, 2026-10-11).
const IdleLimit = 15 * 24 * time.Hour

// Workspaces is the workspace verbs this package uses.
type Workspaces interface {
	// CreateMergeWorkspace creates a detached merge's merge workspace.
	CreateMergeWorkspace(ctx context.Context, requester wsm.Workspace, origin wsm.MergeOrigin, branch string) (wsm.Workspace, error)
	// Nuke destroys a workspace: its session, worktree, branch and records.
	Nuke(ctx context.Context, ws ids.WorkspaceID) error
}

// Prompts submits the handback to the requester's session.
type Prompts interface {
	Submit(ctx context.Context, ws ids.WorkspaceID, said *conversationv1.UserSaid, idempotencyKey string, origin conversationv1.PromptOrigin, delivery wsm.Delivery, target *feedid.Ref) (prompthandler.Outcome, error)
}

// Feed restates a removed merge workspace's switches in its requester's feed.
type Feed interface {
	MarkMergeWorkspaceRemoved(ws, mergeWS ids.WorkspaceID, removedAtMS int64) int
}

// Deps are the service's collaborators.
type Deps struct {
	DB         wsm.DB
	Workspaces Workspaces
	Prompts    Prompts
	Feed       Feed
	// Briefs loads and splices a brief at use time.
	Briefs merge.BriefLoader
	Log    dlog.Surfaces
	Now    func() time.Time
}

// Service is the detached-merge service. It implements merge.DetachedMerges.
type Service struct {
	deps Deps
}

var _ merge.DetachedMerges = (*Service)(nil)

// New builds the service.
func New(deps Deps) *Service {
	return &Service{deps: deps}
}

// Fork answers the merge workspace forked for REQUEST, creating it when none is
// registered: a restart that cut a hand-off short finds the one it forked.
func (s *Service) Fork(ctx context.Context, requester wsm.Workspace, request wsm.DetachedMergeID, branch string) (wsm.Workspace, error) {
	log := s.deps.Log.Global()
	fields := dlog.Context{"requester": string(requester.ID), "request": string(request), "branch": branch}
	existing, found, err := s.deps.DB.MergeWorkspaceFor(ctx, request)
	if err != nil {
		log.Error(op, "could not read whether a detached merge's workspace was already forked", withCause(fields, err))
		return wsm.Workspace{}, err
	}
	if found {
		fields["merge_workspace"] = string(existing.ID)
		log.Info(op, "found the merge workspace already forked for this detached merge", fields)
		return existing, nil
	}
	// Create records its own failures at their site.
	created, err := s.deps.Workspaces.CreateMergeWorkspace(ctx, requester, wsm.MergeOrigin{Requester: requester.ID, Request: request}, branch)
	if err != nil {
		return wsm.Workspace{}, err
	}
	fields["merge_workspace"] = string(created.ID)
	log.Info(op, "forked the merge workspace a detached merge runs in", fields)
	return created, nil
}

// HandBack reports how a detached merge ended to REQUESTER's conversation: a
// deferred prompt, so it never interrupts a running turn, whose first line is
// the feed's handback badge. MERGEWS is the zero value when the merge workspace
// was never forked.
func (s *Service) HandBack(ctx context.Context, requester ids.WorkspaceID, mergeWS wsm.Workspace, out merge.DetachedOutcome) error {
	log := s.deps.Log.Global()
	fields := dlog.Context{"requester": string(requester), "merge_workspace": string(mergeWS.ID), "outcome": out.Kind.String(), "branch": out.Branch}
	label, report, err := handbackText(mergeWS, out)
	if err != nil {
		log.Error(op, "a detached merge ended in an undeclared way; its requester is not told", withCause(fields, err))
		return err
	}
	text, err := s.deps.Briefs(BriefHandback, map[string]string{"label": label, "report": report})
	if err != nil {
		log.Error(op, "could not compose a detached merge's handback; its requester is not told", withCause(fields, err))
		return err
	}
	key, err := handbackKey()
	if err != nil {
		log.Error(op, "could not mint a detached merge handback's idempotency key; its requester is not told", withCause(fields, err))
		return err
	}
	outcome, err := s.deps.Prompts.Submit(ctx, requester, feed.SaidText(text), key,
		conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_HANDBACK, wsm.DeliveryDeferred, nil)
	if err != nil {
		log.Error(op, "could not submit a detached merge's handback to its requester", withCause(fields, err))
		return err
	}
	fields["turn"] = string(outcome.Turn)
	log.Info(op, "handed a detached merge's outcome back to its requester", fields)
	return nil
}

// handbackText composes the badge label and the report for one outcome.
func handbackText(mergeWS wsm.Workspace, out merge.DetachedOutcome) (string, string, error) {
	switch out.Kind {
	case merge.DetachedLanded:
		return fmt.Sprintf("merge of %s landed", out.Branch),
			fmt.Sprintf("%s. Its merge workspace `%s` is closed now; the user can still open it to read the merge's own conversation.", out.Summary, mergeWS.Name), nil
	case merge.DetachedFailed:
		return fmt.Sprintf("merge of %s failed", out.Branch),
			fmt.Sprintf("It failed: %s. Its merge workspace `%s` is still open, at %s, exactly where the merge stopped; the user can switch to it to finish the merge.", out.Summary, mergeWS.Name, mergeWS.Dir), nil
	case merge.DetachedAbandoned:
		return fmt.Sprintf("merge of %s did not run", out.Branch),
			fmt.Sprintf("It did not run: %s.", out.Summary), nil
	}
	return "", "", fmt.Errorf("mergeworkspace: undeclared detached merge outcome %s", out.Kind)
}

// handbackKey mints a handback's idempotency key: every ending is its own
// report.
func handbackKey() (string, error) {
	return prompthandler.MintKey("merge-handback")
}

// Run sweeps for idle merge workspaces at once and then on every TICK, until
// CTX ends.
func (s *Service) Run(ctx context.Context, tick <-chan time.Time) {
	s.Sweep(ctx)
	for {
		select {
		case <-ctx.Done():
			return
		case <-tick:
			s.Sweep(ctx)
		}
	}
}

// Sweep removes every merge workspace idle for IdleLimit whose merge is not in
// any queue, and answers how many it removed.
//
// THE REMOVAL IS RECORDED BEFORE THE WORKSPACE IS DESTROYED, so a bubble that
// names a workspace with no record left can always say when it went; a nuke
// that fails leaves the workspace in place and is tried again by the next
// sweep, which keeps the first recorded instant.
func (s *Service) Sweep(ctx context.Context) int {
	log := s.deps.Log.Global()
	all, err := s.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		sweepFailed(ctx, log, log.Error, "could not list the workspaces; no merge workspace is swept", dlog.Context{}, err)
		return 0
	}
	queued, err := s.inAnyQueue(ctx)
	if err != nil {
		sweepFailed(ctx, log, log.Error, "could not read the merge queues; no merge workspace is swept", dlog.Context{}, err)
		return 0
	}
	now := s.deps.Now()
	removed := 0
	for _, ws := range all {
		if ws.MergeOf == nil || queued[ws.ID] {
			continue
		}
		last := lastActive(ws)
		if now.Sub(last) < IdleLimit {
			continue
		}
		if s.remove(ctx, ws, last, now) {
			removed++
		}
	}
	return removed
}

// remove destroys one idle merge workspace and restates its requester's
// switches, answering whether it went.
func (s *Service) remove(ctx context.Context, ws wsm.Workspace, last, now time.Time) bool {
	log := s.deps.Log.Global()
	fields := dlog.Context{"merge_workspace": string(ws.ID), "requester": string(ws.MergeOf.Requester), "last_active": last, "idle_limit": IdleLimit.String()}
	if err := s.deps.DB.RecordMergeWorkspaceRemoved(ctx, ws.ID, ws.MergeOf.Requester, now); err != nil {
		sweepFailed(ctx, log, log.Error, "could not record an idle merge workspace's removal; it is left in place", fields, err)
		return false
	}
	at, _, err := s.deps.DB.MergeWorkspaceRemovedAt(ctx, ws.ID)
	if err != nil {
		sweepFailed(ctx, log, log.Error, "could not read back an idle merge workspace's removal; it is left in place", fields, err)
		return false
	}
	// The nuke records its own failures at their site.
	if err := s.deps.Workspaces.Nuke(ctx, ws.ID); err != nil {
		sweepFailed(ctx, log, log.Warn, "an idle merge workspace could not be destroyed; the next sweep tries again", fields, err)
		return false
	}
	// A REQUESTER THAT IS ITSELF GONE has no feed to restate.
	if _, err := s.deps.DB.Workspace(ctx, ws.MergeOf.Requester); err != nil {
		if errors.Is(err, wsm.ErrNotFound) {
			log.Info(op, "removed an idle merge workspace whose requester is gone", fields)
			return true
		}
		sweepFailed(ctx, log, log.Error, "could not read an idle merge workspace's requester; its switches still offer the removed workspace", fields, err)
		return true
	}
	fields["rows"] = s.deps.Feed.MarkMergeWorkspaceRemoved(ws.MergeOf.Requester, ws.ID, at.UnixMilli())
	log.Info(op, "removed a merge workspace idle for 15 calendar days", fields)
	return true
}

// inAnyQueue answers the workspaces with a row in any merge queue: their merge
// is requested, queued or running, so they are not idle whatever their stamps.
func (s *Service) inAnyQueue(ctx context.Context) (map[ids.WorkspaceID]bool, error) {
	queues, err := s.deps.DB.AllMergeQueues(ctx)
	if err != nil {
		return nil, err
	}
	out := map[ids.WorkspaceID]bool{}
	for _, entries := range queues {
		for _, entry := range entries {
			out[entry.Workspace] = true
		}
	}
	return out, nil
}

// lastActive is the latest instant anyone used a workspace: created, selected,
// ran a turn, or landed its merge.
func lastActive(ws wsm.Workspace) time.Time {
	last := ws.CreatedAt
	for _, at := range []*time.Time{ws.LastActivityAt, ws.LastSelectedAt, ws.MergedAt} {
		if at != nil && at.After(last) {
			last = *at
		}
	}
	return last
}

// sweepFailed records a sweep step that failed with err at level, unless the
// daemon was standing down: the sweep runs on the daemon's lifetime, so a step
// its end cut short is the orderly exit it is, recorded at INFO with what it
// left undone.
func sweepFailed(ctx context.Context, log dlog.Logger, level func(operation, message string, ctx dlog.Context), message string, fields dlog.Context, err error) {
	if standdown.CutShort(ctx, err) {
		stood := withCause(fields, err)
		stood["left_undone"] = message
		log.Info(op, "the daemon stood down mid-sweep", stood)
		return
	}
	level(op, message, withCause(fields, err))
}

// withCause adds an error's text to a record's fields.
func withCause(fields dlog.Context, err error) dlog.Context {
	out := dlog.Context{}
	for k, v := range fields {
		out[k] = v
	}
	out["cause"] = err.Error()
	return out
}
