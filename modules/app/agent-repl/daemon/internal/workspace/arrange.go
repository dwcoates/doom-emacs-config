package workspace

import (
	"context"
	"errors"
	"fmt"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
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
	opPlaceWorkspace   = "daemon.workspace.place_workspace"
)

// arrangeRPC is the rpc every arrangement verb answers.
const arrangeRPC = "ArrangeSidebar"

// CreateSubsection adds a subsection named NAME at the bottom of REPO's
// section.
func (v *verbs) CreateSubsection(ctx context.Context, repo ids.RepoID, name string) error {
	log := v.deps.Log.Global().With(dlog.Context{"repo_id": string(repo), "name": name})
	return v.arrange(ctx, log, opCreateSubsection, "create the subsection", func() error {
		_, err := v.deps.DB.CreateSubsection(ctx, repo, name)
		return err
	})
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

// MoveSubsection moves subsection ID before BEFORE (nil: last).
func (v *verbs) MoveSubsection(ctx context.Context, id ids.SubsectionID, before *ids.SubsectionID) error {
	log := v.deps.Log.Global().With(dlog.Context{"subsection": string(id), "before": ids.Optional(before)})
	return v.arrange(ctx, log, opMoveSubsection, "move the subsection", func() error {
		return v.deps.DB.MoveSubsection(ctx, id, before)
	})
}

// MoveRepository moves repository ID before BEFORE (nil: last).
func (v *verbs) MoveRepository(ctx context.Context, id ids.RepoID, before *ids.RepoID) error {
	log := v.deps.Log.Global().With(dlog.Context{"repo_id": string(id), "before": ids.Optional(before)})
	return v.arrange(ctx, log, opMoveRepository, "move the repository", func() error {
		return v.deps.DB.MoveRepository(ctx, id, before)
	})
}

// PlaceWorkspace puts workspace WS, with its nested family, into subsection
// INTO (nil: none) before workspace BEFORE (nil: last). Both workspaces must
// be this daemon's to serve, as every per-workspace verb's are.
func (v *verbs) PlaceWorkspace(ctx context.Context, ws ids.WorkspaceID, into *ids.SubsectionID, before *ids.WorkspaceID) error {
	_, log, err := v.owned(ctx, arrangeRPC, ws)
	if err != nil {
		return err
	}
	if before != nil {
		if _, _, err := v.owned(ctx, arrangeRPC, *before); err != nil {
			return err
		}
	}
	log = log.With(dlog.Context{"into": ids.Optional(into), "before": ids.Optional(before)})
	return v.arrange(ctx, log, opPlaceWorkspace, "place the workspace", func() error {
		return v.deps.DB.PlaceWorkspace(ctx, ws, into, before)
	})
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
	case errors.Is(err, wsm.ErrHasParent):
		return refuse(log, arrangeRPC, ArmHasParent, err.Error(), false)
	default:
		return nil
	}
}
