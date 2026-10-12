// Package branchcommits keeps each live workspace's branch commit count, the
// number the roster's detail panel states (frontend.v1.RosterRowDetailCommits):
// how many commits the branch holds beyond its merge-base with the branch it
// was cut from.
//
// THE COUNT IS TAKEN WHEN A TURN CLOSES (owner ruling, 2026-10-11). The prompt
// queue's turn-close door tells the Recounter, which counts with git and
// records the answer; the roster republishes it. A commit made between turns
// shows once the next turn closes.
//
// ONE WORKER COUNTS, SO TWO COUNTS FOR ONE WORKSPACE NEVER RACE. A turn close
// only marks its workspace as owed a recount; the single Run loop takes each
// owed workspace in turn. A later close while a count runs marks it owed again,
// so the last count recorded is always one taken after the last close, and a
// stale count can never land after a fresher one.
//
// A COUNT IS RECORDED WHOLE OR NOT AT ALL. Reading the record and running git
// follow the serving lifetime: the daemon's exit cuts them short, which is the
// exit and not a fault, and the next turn close recounts. Once git has
// answered, the write and the roster republish run to the end whatever the
// lifetime does: they are local and bounded, and the daemon joins this loop
// before it closes the state database, so a count is never taken and lost.
//
// A MERGED WORKSPACE IS NOT RECOUNTED: its branch is inside its parent, so the
// count would read zero. The merge stamps the count it brought in, and the
// store refuses a live write once merged_at is set (wsm.SetLiveCommitCount),
// which keeps a count racing the landing from overwriting it.
package branchcommits

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"sync"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// opRecount is the one operation every record of this package carries.
const opRecount = "daemon.branchcommits.recount"

// DB is the slice of the state client the Recounter reads and writes.
type DB interface {
	Workspace(ctx context.Context, id wsm.WorkspaceID) (wsm.Workspace, error)
	SetLiveCommitCount(ctx context.Context, id wsm.WorkspaceID, count uint32) (bool, error)
}

// Git counts the commits of a range.
type Git interface {
	CountCommits(ctx context.Context, dir, base, tip string) (uint32, error)
}

// Deps are the Recounter's collaborators. Every one is REQUIRED.
type Deps struct {
	DB  DB
	Git Git
	// PublishRegistry republishes the roster's durable half, which is where
	// the recorded count reaches the detail panel.
	PublishRegistry func(context.Context) error
	Log             dlog.Surfaces
	// WorktreeGone reports whether a workspace's worktree no longer exists
	// (nil is a stat of the directory). A seam for tests.
	WorktreeGone func(dir string) bool
}

// Recounter recounts a workspace's branch commits after each of its turns
// closes.
type Recounter struct {
	deps Deps

	mu sync.Mutex
	// owed is the set of workspaces owed a recount, and order the sequence
	// they became owed in, so every workspace is counted in turn.
	owed  map[ids.WorkspaceID]bool
	order []ids.WorkspaceID
	// wake is signalled when a workspace becomes owed. Its one slot coalesces
	// signals: the loop drains every owed workspace on each wake.
	wake chan struct{}
}

// New builds a Recounter. A missing collaborator is a wiring defect.
func New(deps Deps) (*Recounter, error) {
	switch {
	case deps.DB == nil:
		return nil, errors.New("branchcommits: the recounter needs a state client")
	case deps.Git == nil:
		return nil, errors.New("branchcommits: the recounter needs git")
	case deps.PublishRegistry == nil:
		return nil, errors.New("branchcommits: the recounter needs the roster republish")
	case deps.Log == nil:
		return nil, errors.New("branchcommits: the recounter needs log surfaces")
	}
	if deps.WorktreeGone == nil {
		deps.WorktreeGone = func(dir string) bool {
			_, err := os.Stat(dir)
			return errors.Is(err, fs.ErrNotExist)
		}
	}
	return &Recounter{deps: deps, owed: map[ids.WorkspaceID]bool{}, wake: make(chan struct{}, 1)}, nil
}

// TurnClosed marks WS owed a recount. It never blocks: the count is taken by
// Run.
func (r *Recounter) TurnClosed(ws ids.WorkspaceID) {
	r.mu.Lock()
	if !r.owed[ws] {
		r.owed[ws] = true
		r.order = append(r.order, ws)
	}
	r.mu.Unlock()
	select {
	case r.wake <- struct{}{}:
	default:
	}
}

// Run counts every owed workspace until ctx ends.
func (r *Recounter) Run(ctx context.Context) error {
	for {
		select {
		case <-ctx.Done():
			return nil
		case <-r.wake:
			for {
				ws, ok := r.next()
				if !ok {
					break
				}
				r.recount(ctx, ws)
			}
		}
	}
}

// next takes the longest-owed workspace off the set.
func (r *Recounter) next() (ids.WorkspaceID, bool) {
	r.mu.Lock()
	defer r.mu.Unlock()
	if len(r.order) == 0 {
		return "", false
	}
	ws := r.order[0]
	r.order = r.order[1:]
	delete(r.owed, ws)
	return ws, true
}

// recount counts one workspace's branch and records the count when it moved.
func (r *Recounter) recount(ctx context.Context, ws ids.WorkspaceID) {
	record, err := r.deps.DB.Workspace(ctx, ws)
	if err != nil && ctx.Err() != nil {
		r.deps.Log.Global().Info(opRecount, "the daemon's exit cut a recount short before the workspace was read; the next turn close recounts", dlog.Context{
			"workspace": string(ws), "cause": err.Error()})
		return
	}
	if errors.Is(err, wsm.ErrNotFound) {
		// The workspace was forgotten after its turn closed: nothing to count.
		r.deps.Log.Global().Debug(opRecount, "a workspace owed a recount is no longer registered", dlog.Context{
			"workspace": string(ws)})
		return
	}
	if err != nil {
		r.deps.Log.Global().Error(opRecount, "could not read the workspace owed a recount", dlog.Context{
			"workspace": string(ws), "error": err.Error()})
		return
	}
	log := r.deps.Log.WorkspaceOrCentral(record.Dir)
	fields := dlog.Context{"workspace": string(ws), "branch": record.Branch, "parent_branch": record.ParentBranch}
	if record.MergedAt != nil {
		log.Debug(opRecount, "the workspace has merged; it keeps the count its merge landed", fields)
		return
	}
	if record.Branch == "" || record.ParentBranch == "" {
		log.Debug(opRecount, "the workspace names no branch pair to count between", fields)
		return
	}
	count, err := r.deps.Git.CountCommits(ctx, record.Dir, record.ParentBranch, record.Branch)
	if err != nil && ctx.Err() != nil {
		fields["cause"] = err.Error()
		log.Info(opRecount, "the daemon's exit cut a recount short before git answered; the next turn close recounts", fields)
		return
	}
	if err != nil {
		fields["error"] = err.Error()
		// A COUNT THAT RACED ITS WORKSPACE'S OWN TEARDOWN IS NO FAULT. A turn
		// closing right as its merge lands owes a recount, and the landing
		// closes the workspace and removes its worktree while git runs in it
		// ("not a git repository"). The workspace is read again: one that was
		// forgotten, merged or closed meanwhile, or whose worktree is gone,
		// has nothing left to count. Any other failure is an error.
		if why := r.movedOn(ctx, ws, record.Dir); why != "" {
			fields["moved_on"] = why
			log.Info(opRecount, "the workspace moved on while its branch was counted; there is nothing left to count", fields)
			return
		}
		log.Error(opRecount, "could not count the branch's commits; the panel keeps the last count", fields)
		return
	}
	fields["commit_count"] = count
	if record.CommitCount != nil && *record.CommitCount == count {
		log.Debug(opRecount, "the branch's commit count is unchanged", fields)
		return
	}
	settle := context.WithoutCancel(ctx)
	applied, err := r.deps.DB.SetLiveCommitCount(settle, ws, count)
	if err != nil {
		fields["error"] = err.Error()
		log.Error(opRecount, "could not record the branch's commit count", fields)
		return
	}
	if !applied {
		log.Debug(opRecount, "the workspace merged while its branch was counted; the landed count stands", fields)
		return
	}
	log.Debug(opRecount, "recorded the branch's commit count", fields)
	if err := r.deps.PublishRegistry(settle); err != nil {
		fields["error"] = fmt.Sprintf("republish the roster: %v", err)
		log.Error(opRecount, "could not republish the roster after a recount", fields)
	}
}

// movedOn answers why a workspace whose count failed has nothing left to
// count -- forgotten, merged, closed, or its worktree removed -- or "" when it
// is still the live workspace the count was for.
func (r *Recounter) movedOn(ctx context.Context, ws ids.WorkspaceID, dir string) string {
	record, err := r.deps.DB.Workspace(context.WithoutCancel(ctx), ws)
	switch {
	case errors.Is(err, wsm.ErrNotFound):
		return "forgotten"
	case err != nil:
		return ""
	case record.MergedAt != nil:
		return "merged"
	case record.Closed:
		return "closed"
	case r.deps.WorktreeGone(dir):
		return "worktree removed"
	}
	return ""
}
