package workspace

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

const opSelfClose = "daemon.workspace.self_close"

// NewSelfCloses builds the runner that carries out, at the edge where a turn
// ends, the close a workspace asked of itself from inside that turn
// (CloseSelf), over CARRY, the verbs' CarryOutSelfClose. A self-close a daemon
// restart cut short is carried out once the successor learns the asking turn
// is over.
func NewSelfCloses(carry func(ctx context.Context, ws ids.WorkspaceID) error, log dlog.Logger) *TurnEdgeRunner {
	return newTurnEdgeRunner(turnEnds, opSelfClose,
		"a turn ended after the self-close carrier stopped, as the daemon stood down; the next daemon carries the close out",
		"could not carry out the close a workspace asked of itself at its turn's end", carry, log)
}

// CarryOutSelfClose implements Verbs.
func (v *verbs) CarryOutSelfClose(ctx context.Context, ws ids.WorkspaceID) error {
	record, log, err := v.record(ctx, opSelfClose, ws)
	if err != nil {
		return err
	}
	// AN INTAKE WORKSPACE IS RETIRED AT EVERY TURN END, asked or not: the
	// retirement closes it, so a self-close it also asked for needs no taking.
	intake, err := v.deps.DB.IsIntake(ctx, ws)
	if err != nil {
		log.Error(opSelfClose, "could not read whether the workspace whose turn ended is an intake workspace", dlog.WithCause(nil, err))
		return fmt.Errorf("turn end of %q: read whether it is an intake workspace: %w", ws, err)
	}
	if intake {
		return v.retireIntake(ctx, ws)
	}
	if record.CloseWhenTurnEnds == nil {
		return nil
	}
	return v.takeSelfClose(ctx, log, ws)
}
