// Package foldsweep folds the sidebar's idle sections just before a deploy
// starts building: every repository section, subsection and workspace's
// children none of whose members the user has prompted within IdleAfter.
//
// A MEMBER IS A WORKSPACE THE FOLD WOULD HIDE (wsm.FoldMembers), so a
// workspace's own prompts never keep its own children's fold open: only its
// descendants' do. A PROMPT IS THE USER'S OWN, held or sent: the prompt
// handler stamps it (wsm.AttentionPrompt) and an agent's prompt, a daemon
// notice and a keep-alive leave no stamp.
//
// A SECTION THE USER OPENED THEMSELVES within OpenedGrace is spared, as is one
// holding the current workspace: the sweep never moves the selection, which a
// collapse over the current workspace would.
package foldsweep

import (
	"context"
	"errors"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/workspace"
	"agent-repld/internal/wsm"
)

// IdleAfter is how long a section's members go without a prompt from the
// user before the sweep folds it. It is the owner's ruling (2026-11-21), not
// a measurement.
const IdleAfter = 6 * time.Hour

// OpenedGrace is how long a section the user opened themselves is spared. It
// is the owner's ruling (2026-11-21), not a measurement.
const OpenedGrace = time.Hour

// RequesterRPC is the name every fold the sweep makes carries as its
// requester (workspace.FoldRequester.RPC).
const RequesterRPC = "DeployFoldSweep"

const opSweep = "daemon.foldsweep.sweep"

// DB is the slice of wsm.DB the sweep reads.
type DB interface {
	ReadArrangement(ctx context.Context) (wsm.Arrangement, error)
	ListUserAttention(ctx context.Context) ([]wsm.UserAttention, error)
}

// Folder is the verbs the sweep collapses with: the same ones the sidebar's
// own collapse reaches, so a swept fold hides tabs and stands sessions down
// exactly as the user's would.
type Folder interface {
	FoldRepository(ctx context.Context, repo ids.RepoID, folded bool, requester workspace.FoldRequester) error
	FoldSubsection(ctx context.Context, sub ids.SubsectionID, folded bool, requester workspace.FoldRequester) error
	FoldChildren(ctx context.Context, ws ids.WorkspaceID, folded bool, requester workspace.FoldRequester) error
}

// Deps are the sweeper's collaborators. All are required except Now, which is
// time.Now when nil.
type Deps struct {
	DB     DB
	Folder Folder
	Now    func() time.Time
	Log    dlog.Surfaces
}

// Sweeper runs the sweep.
type Sweeper struct {
	deps Deps
	log  dlog.Logger
}

// New builds a Sweeper, refusing a missing collaborator.
func New(deps Deps) (*Sweeper, error) {
	switch {
	case deps.DB == nil:
		return nil, errors.New("foldsweep: the state client is required")
	case deps.Folder == nil:
		return nil, errors.New("foldsweep: the fold verbs are required")
	case deps.Log == nil:
		return nil, errors.New("foldsweep: log surfaces are required")
	}
	if deps.Now == nil {
		deps.Now = time.Now
	}
	return &Sweeper{deps: deps, log: deps.Log.Global()}, nil
}

// candidate is one unfolded section the sweep may fold.
type candidate struct {
	// what and target name it in records.
	what, target string
	members      wsm.FoldMembers
	// opened is the stamp the user's own open of it leaves.
	opened wsm.UserAttention
	fold   func(ctx context.Context) error
}

// Sweep folds every idle section. A fold that fails is recorded and the rest
// still run; the error answers them all joined.
func (s *Sweeper) Sweep(ctx context.Context) error {
	arr, err := s.deps.DB.ReadArrangement(ctx)
	if err != nil {
		s.log.Error(opSweep, "could not read the arrangement; nothing was folded", dlog.WithCause(nil, err))
		return fmt.Errorf("foldsweep: read the arrangement: %w", err)
	}
	stamps, err := s.deps.DB.ListUserAttention(ctx)
	if err != nil {
		s.log.Error(opSweep, "could not read the user's attention; nothing was folded", dlog.WithCause(nil, err))
		return fmt.Errorf("foldsweep: read the user's attention: %w", err)
	}
	now := s.deps.Now()
	prompted := map[ids.WorkspaceID]time.Time{}
	opened := map[wsm.AttentionKind]map[string]time.Time{}
	for _, stamp := range stamps {
		if stamp.Kind == wsm.AttentionPrompt {
			prompted[ids.WorkspaceID(stamp.Target)] = stamp.At
			continue
		}
		if opened[stamp.Kind] == nil {
			opened[stamp.Kind] = map[string]time.Time{}
		}
		opened[stamp.Kind][stamp.Target] = stamp.At
	}

	var failures []error
	folded := 0
	for _, c := range s.candidates(ctx, arr) {
		if reason := spareReason(c, arr.Workspaces, prompted, opened[c.opened.Kind][c.target], now); reason != "" {
			s.log.Debug(opSweep, "the "+c.what+" is left unfolded", dlog.Context{"target": c.target, "reason": reason})
			continue
		}
		if err := c.fold(ctx); err != nil {
			s.log.Error(opSweep, "could not fold the idle "+c.what, dlog.WithCause(dlog.Context{"target": c.target}, err))
			failures = append(failures, fmt.Errorf("fold %s %q: %w", c.what, c.target, err))
			continue
		}
		folded++
		s.log.Info(opSweep, "folded the idle "+c.what, dlog.Context{"target": c.target, "idle_after": IdleAfter.String()})
	}
	s.log.Info(opSweep, "swept the sidebar before the deploy builds", dlog.Context{"folded": folded, "failed": len(failures)})
	return errors.Join(failures...)
}

// candidates lists every unfolded repository section, subsection and
// workspace's children.
func (s *Sweeper) candidates(ctx context.Context, arr wsm.Arrangement) []candidate {
	requester := workspace.FoldRequester{RPC: RequesterRPC}
	var out []candidate
	section := wsm.SectionRepos(arr.Workspaces)
	for _, repo := range arr.Repositories {
		if repo.Folded {
			continue
		}
		repo := repo
		out = append(out, candidate{
			what: "repository section", target: string(repo.ID), members: wsm.RepositoryMembers(section, repo.ID),
			opened: wsm.UserAttention{Kind: wsm.AttentionOpenedRepository},
			fold:   func(ctx context.Context) error { return s.deps.Folder.FoldRepository(ctx, repo.ID, true, requester) },
		})
	}
	for _, sub := range arr.Subsections {
		if sub.Folded {
			continue
		}
		sub := sub
		out = append(out, candidate{
			what: "subsection", target: string(sub.ID), members: wsm.SubsectionMembers(sub.ID),
			opened: wsm.UserAttention{Kind: wsm.AttentionOpenedSubsection},
			fold:   func(ctx context.Context) error { return s.deps.Folder.FoldSubsection(ctx, sub.ID, true, requester) },
		})
	}
	for _, ws := range arr.Workspaces {
		if ws.ChildrenFolded {
			continue
		}
		ws := ws
		out = append(out, candidate{
			what: "workspace's children", target: string(ws.ID), members: wsm.DescendantMembers(arr.Workspaces, ws.ID),
			opened: wsm.UserAttention{Kind: wsm.AttentionOpenedChildren},
			fold:   func(ctx context.Context) error { return s.deps.Folder.FoldChildren(ctx, ws.ID, true, requester) },
		})
	}
	return out
}

// spareReason answers why C stays unfolded, "" when the sweep folds it.
func spareReason(c candidate, all []wsm.Workspace, prompted map[ids.WorkspaceID]time.Time, openedAt time.Time, now time.Time) string {
	hasOpenMember := false
	for _, ws := range all {
		if !c.members(ws) {
			continue
		}
		if !ws.Closed {
			hasOpenMember = true
		}
		if ws.Current && !ws.Closed {
			return "holds_current_workspace"
		}
		if at, ok := prompted[ws.ID]; ok && now.Sub(at) < IdleAfter {
			return "member_prompted_recently"
		}
	}
	switch {
	case !hasOpenMember:
		return "no_open_member"
	case !openedAt.IsZero() && now.Sub(openedAt) < OpenedGrace:
		return "opened_by_user_recently"
	}
	return ""
}
