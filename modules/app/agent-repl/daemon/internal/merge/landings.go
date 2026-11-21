package merge

import (
	"context"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// This file is EVERY LANDING'S RECORD: the one helper each landing site calls
// once it has landed on its repository's main branch, naming the path it took
// (wsm merge_landings), and the reads of those records
// (agentrepl.v1.ListMergeLandings). The landing sites:
//
//   - the queue's own landing (run.recordQueueLanding, from concludedTerminal);
//   - the deploy mediator's direct landing (remediationLand);
//   - land-direct.sh's report of a landing it made (RecordDirectLanding),
//     which the daemon classifies from its own state: the repository's
//     mediator's branch, its queue bypass holder, or neither.
//
// No site writes the record any other way, so a landing is counted the same
// whichever path it took, and a new path adds a call here.

const opRecordLanding = "daemon.merge.record_landing"

// recordLanding is EVERY LANDING'S ONE RECORD. It writes LANDING and records
// it at INFO with its path; a record of a head already recorded keeps the
// first. A failed write is recorded at ERROR and answered: the landing
// itself stands either way.
func (o *orchestrator) recordLanding(ctx context.Context, landing wsm.MergeLanding) error {
	fields := dlog.Context{
		"repo": string(landing.Repo), "head": landing.Head, "path": string(landing.Path), "branch": landing.Branch,
		"workspace": string(landing.Workspace), "workspace_name": landing.WorkspaceName, "landed_at": landing.LandedAt}
	added, err := o.deps.DB.RecordMergeLanding(ctx, landing)
	if err != nil {
		o.deps.Log.Global().Error(opRecordLanding, "could not record a landing on the main branch; the landing stands, and the landings read will not list it", dlog.WithCause(fields, err))
		return fmt.Errorf("merge: recording the landing of %s: %w", landing.Head, err)
	}
	if !added {
		o.deps.Log.Global().Info(opRecordLanding, "the landing was recorded already; its first record stands", fields)
		return nil
	}
	o.deps.Log.Global().Info(opRecordLanding, "recorded a landing on the main branch with the path it took", fields)
	return nil
}

// recordQueueLanding records a landed merge's landing as the queue's, when it
// landed a commit on its repository's main branch: a merge into a parent
// workspace's branch lands on no main branch, and a deploy whose lifecycle
// lands nothing locally names no commit.
func (r *run) recordQueueLanding(ctx context.Context, out outcome) {
	if out.landed == "" || !r.target.toMain {
		return
	}
	// A name that would not read leaves the record unnamed rather than
	// unwritten; workspaceName recorded the failed read.
	name, _ := r.o.workspaceName(ctx, opRecordLanding, r.ws)
	// recordLanding recorded a failed write; the landing stands.
	_ = r.o.recordLanding(ctx, wsm.MergeLanding{
		Repo: r.repo, Head: out.landed, Path: wsm.LandingThroughQueue, Branch: r.subject.branch,
		Workspace: r.ws, WorkspaceName: name, LandedAt: r.o.deps.Now()})
}

// DirectLanding is land-direct.sh's report of a landing it made.
type DirectLanding struct {
	// Repository is the repository landed on.
	Repository RepositoryScope
	// Branch is the branch it landed.
	Branch string
	// Head is the main branch's commit once it was done.
	Head string
	// Caller is the workspace whose landing it was, empty when no workspace
	// ran the script.
	Caller ids.WorkspaceID
}

// RecordDirectLanding implements Orchestrator: records a landing
// land-direct.sh made, classified by the daemon's own state, and answers the
// path it recorded. A branch the repository's deploy mediator stands on is
// the mediator's landing whoever ran the script (the daemon's finish runs it
// in the main checkout); otherwise a caller holding the repository's queue
// bypass mode made a queue-bypass landing, and any other caller a direct one.
func (o *orchestrator) RecordDirectLanding(ctx context.Context, report DirectLanding) (wsm.LandingPath, error) {
	fields := dlog.Context{"repository": string(report.Repository.ID), "dir": report.Repository.Dir, "branch": report.Branch, "head": report.Head, "caller": string(report.Caller)}
	repository, key, err := o.readScopedRepository(ctx, opRecordLanding, "a direct landing's record", report.Repository)
	if err != nil {
		return "", err
	}
	landing := wsm.MergeLanding{Repo: key, Head: report.Head, Path: wsm.LandingDirect, Branch: report.Branch, Workspace: report.Caller, LandedAt: o.deps.Now()}
	mediator, found, err := o.deps.DB.DeployMediatorOf(ctx, key)
	if err != nil {
		o.deps.Log.Global().Error(opRecordLanding, "could not read the repository's deploy mediator to classify a direct landing", dlog.WithCause(fields, err))
		return "", err
	}
	if found {
		record, err := o.deps.DB.Workspace(ctx, mediator.Workspace)
		if err != nil {
			o.deps.Log.Global().Error(opRecordLanding, "could not read the deploy mediator to classify a direct landing", dlog.WithCause(withField(fields, "mediator", string(mediator.Workspace)), err))
			return "", err
		}
		if record.Branch == report.Branch {
			landing.Path, landing.Workspace = wsm.LandingDeployMediator, mediator.Workspace
		}
	}
	if landing.Path == wsm.LandingDirect && report.Caller != "" {
		bypass, err := o.deps.DB.QueueBypass(ctx, report.Caller)
		if err != nil {
			o.deps.Log.Global().Error(opRecordLanding, "could not read the caller's queue bypass standing to classify a direct landing", dlog.WithCause(fields, err))
			return "", err
		}
		if bypass.Holds && bypass.RepoID == repository.ID {
			landing.Path = wsm.LandingQueueBypass
		}
	}
	if landing.Workspace != "" {
		if landing.WorkspaceName, err = o.workspaceName(ctx, opRecordLanding, landing.Workspace); err != nil {
			return "", err
		}
	}
	if err := o.recordLanding(ctx, landing); err != nil {
		return "", err
	}
	return landing.Path, nil
}

// LandingsWindow is one repository's landings in a window.
type LandingsWindow struct {
	// Repository is the repository read.
	Repository wsm.Repository
	// ThroughQueue are the landings its merge queue made, oldest first.
	ThroughQueue []wsm.MergeLanding
	// Otherwise are every other landing on its main branch, oldest first.
	Otherwise []wsm.MergeLanding
	// RecordingBegan is when the daemon began recording landings.
	RecordingBegan time.Time
}

// Landings implements Orchestrator: the scoped repository's landings with
// since <= landed at <= until, split into the queue's and every other.
func (o *orchestrator) Landings(ctx context.Context, scope RepositoryScope, since, until time.Time) (LandingsWindow, error) {
	const op = "daemon.merge.landings"
	repository, key, err := o.readScopedRepository(ctx, op, "a landings read", scope)
	if err != nil {
		return LandingsWindow{}, err
	}
	landings, began, err := o.deps.DB.MergeLandingsIn(ctx, key, since, until)
	if err != nil {
		return LandingsWindow{}, err
	}
	out := LandingsWindow{Repository: repository, RecordingBegan: began}
	for _, landing := range landings {
		if landing.Path == wsm.LandingThroughQueue {
			out.ThroughQueue = append(out.ThroughQueue, landing)
		} else {
			out.Otherwise = append(out.Otherwise, landing)
		}
	}
	o.deps.Log.Global().Debug(op, "answered a repository's landings in a window", dlog.Context{
		"repo": string(key), "since": since, "until": until, "through_queue": len(out.ThroughQueue), "otherwise": len(out.Otherwise)})
	return out, nil
}
