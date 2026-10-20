package workspace

import (
	"context"
	"errors"
	"fmt"
	"slices"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
)

// The ArrangeSidebar verbs: the repository grouping's arrangement, which the
// tab bar walks too (wsm's arrange.go, resolve/sidebar's arrangement.go).
// Each writes, then republishes the roster, so every page and the tab bar
// redraw from the one new order.

// The operations the arrangement's records carry, one per change.
const (
	opCreateSubsection = "daemon.workspace.create_subsection"
	opRenameSubsection = "daemon.workspace.rename_subsection"
	opDeleteSubsection = "daemon.workspace.delete_subsection"
	opMoveSubsection   = "daemon.workspace.move_subsection"
	opMoveRepository   = "daemon.workspace.move_repository"
	opPlaceWorkspace   = "daemon.workspace.place_workspaces"
	// opSidebarArrangement is the operation GetSidebarArrangement's reads carry.
	opSidebarArrangement = "daemon.workspace.sidebar_arrangement"
)

// arrangeRPC is the rpc every arrangement verb answers.
const arrangeRPC = "ArrangeSidebar"

// CreateSubsection adds a subsection named NAME at the bottom of REPO's
// section.
func (v *verbs) CreateSubsection(ctx context.Context, repo ids.RepoID, name string) (wsm.Subsection, error) {
	log := v.deps.Log.Global().With(dlog.Context{"repo_id": string(repo), "name": name})
	var created wsm.Subsection
	err := v.arrange(ctx, log, opCreateSubsection, "create the subsection", func() error {
		sub, err := v.deps.DB.CreateSubsection(ctx, repo, name)
		created = sub
		return err
	})
	if err != nil {
		return wsm.Subsection{}, err
	}
	return created, nil
}

// RenameSubsection relabels subsection ID to NAME.
func (v *verbs) RenameSubsection(ctx context.Context, id ids.SubsectionID, name string) error {
	log := v.deps.Log.Global().With(dlog.Context{"subsection": string(id), "name": name})
	return v.arrange(ctx, log, opRenameSubsection, "rename the subsection", func() error {
		return v.deps.DB.RenameSubsection(ctx, id, name)
	})
}

// DeleteSubsection removes subsection ID; its workspaces return to none.
func (v *verbs) DeleteSubsection(ctx context.Context, id ids.SubsectionID) error {
	log := v.deps.Log.Global().With(dlog.Context{"subsection": string(id)})
	return v.arrange(ctx, log, opDeleteSubsection, "delete the subsection", func() error {
		return v.deps.DB.DeleteSubsection(ctx, id)
	})
}

// MoveSubsection moves subsection ID among its repository's entries, to TO.
// A workspace anchor must be this daemon's to serve, as every per-workspace
// verb's is.
func (v *verbs) MoveSubsection(ctx context.Context, id ids.SubsectionID, to wsm.Anchor) error {
	if before, ok := to.(wsm.BeforeWorkspace); ok {
		if _, _, err := v.owned(ctx, arrangeRPC, before.ID); err != nil {
			return err
		}
	}
	log := v.deps.Log.Global().With(dlog.Context{"subsection": string(id), "to": wsm.AnchorField(to)})
	return v.arrange(ctx, log, opMoveSubsection, "move the subsection", func() error {
		return v.deps.DB.MoveSubsection(ctx, id, to)
	})
}

// MoveRepository moves repository ID before BEFORE (nil: last).
func (v *verbs) MoveRepository(ctx context.Context, id ids.RepoID, before *ids.RepoID) error {
	log := v.deps.Log.Global().With(dlog.Context{"repo_id": string(id), "before": ids.Optional(before)})
	return v.arrange(ctx, log, opMoveRepository, "move the repository", func() error {
		return v.deps.DB.MoveRepository(ctx, id, before)
	})
}

// PlaceWorkspaces puts root workspaces WS, in that order and with their
// families, into subsection INTO (nil: none) as one block at AT. Every
// workspace named must be this daemon's to serve, as every per-workspace
// verb's are.
func (v *verbs) PlaceWorkspaces(ctx context.Context, ws []ids.WorkspaceID, into *ids.SubsectionID, at wsm.Anchor) error {
	named := slices.Clone(ws)
	if before, ok := at.(wsm.BeforeWorkspace); ok {
		named = append(named, before.ID)
	}
	for _, id := range named {
		if _, _, err := v.owned(ctx, arrangeRPC, id); err != nil {
			return err
		}
	}
	log := v.deps.Log.Global().With(dlog.Context{"workspaces": ws, "into": ids.Optional(into), "at": wsm.AnchorField(at)})
	return v.arrange(ctx, log, opPlaceWorkspace, "place the workspaces", func() error {
		return v.deps.DB.PlaceWorkspaces(ctx, ws, into, at)
	})
}

// SidebarArrangement reads the repository grouping's arrangement from the
// one registry read every roster is drawn from.
func (v *verbs) SidebarArrangement(ctx context.Context) ([]sidebar.ArrangedRepository, error) {
	log := v.deps.Log.Global()
	reg, ok := v.readRegistry(ctx, log, opSidebarArrangement)
	if !ok {
		return nil, errors.New("read the sidebar arrangement: the registry could not be read")
	}
	out := sidebar.Arranged(reg, log)
	log.Debug(opSidebarArrangement, "read the sidebar arrangement", dlog.Context{"repositories": len(out)})
	return out, nil
}

// arrange runs WRITE, turns the store's refusals into ArrangeSidebar's arms,
// records a failed write at ERROR under OP, and republishes the roster after
// a write that landed. WHAT names the change for the records.
func (v *verbs) arrange(ctx context.Context, log dlog.Logger, op, what string, write func() error) error {
	if err := write(); err != nil {
		if refused := arrangementRefusal(log, err); refused != nil {
			return refused
		}
		log.Error(op, "could not "+what, dlog.Context{"cause": err.Error()})
		return fmt.Errorf("%s: %w", what, err)
	}
	log.Info(op, "arranged the sidebar: "+what, nil)
	v.republishRegistry(ctx, log, op)
	return nil
}

// arrangementRefusal is the ArrangeSidebar arm ERR answers, recorded as an
// ordinary refusal, or nil when ERR is the store failing rather than
// refusing.
func arrangementRefusal(log dlog.Logger, err error) *Refusal {
	var sub *wsm.UnknownSubsectionError
	switch {
	case errors.As(err, &sub):
		return refuseWith(log, arrangeRPC, ArmUnknownSubsection, err.Error(), true,
			map[string]any{"subsection": &agentreplv1.SubsectionRef{Id: string(sub.ID)}})
	case errors.Is(err, wsm.ErrUnknownRepository):
		return refuse(log, arrangeRPC, ArmUnknownRepository, err.Error(), true)
	case errors.Is(err, wsm.ErrNotFound):
		return refuse(log, arrangeRPC, ArmUnknownWorkspace, err.Error(), true)
	case errors.Is(err, wsm.ErrBlankSubsectionName):
		return refuse(log, arrangeRPC, ArmBlankName, err.Error(), false)
	case errors.Is(err, wsm.ErrOtherRepository):
		return refuse(log, arrangeRPC, ArmOtherRepository, err.Error(), false)
	case errors.Is(err, wsm.ErrAnchorElsewhere):
		return refuse(log, arrangeRPC, ArmAnchorElsewhere, err.Error(), false)
	default:
		return nil
	}
}
