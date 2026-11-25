package workspace

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agentrepl/logging/logsite"
)

const opIntakeRetirement = "daemon.workspace.intake_retirement"

// NewIntakeRetirements builds the runner that retires an intake workspace at
// the edge where each of its turns ends, over RETIRE, the verbs'
// RetireIntakeAtTurnEnd. A turn a daemon restart cut short is retired once the
// successor learns it is over.
func NewIntakeRetirements(retire func(ctx context.Context, ws ids.WorkspaceID) error, log dlog.Logger) *TurnEdgeRunner {
	return newTurnEdgeRunner(turnEnds, opIntakeRetirement,
		logsite.Msg("a turn ended after the intake retirer stopped, as the daemon stood down; the next daemon retires it"),
		logsite.Msg("could not retire an intake workspace at its turn's end"), retire, log)
}

// RetireIntakeAtTurnEnd implements Verbs: AN INTAKE WORKSPACE IS RETIRED AT
// EVERY TURN END (retireIntake); any other workspace is left alone. A close a
// workspace asked of itself is its standing close request, which the standing
// closes carry out (StandingCloses).
func (v *verbs) RetireIntakeAtTurnEnd(ctx context.Context, ws ids.WorkspaceID) error {
	_, log, err := v.record(ctx, opIntakeRetirement, ws)
	if err != nil {
		return err
	}
	intake, err := v.deps.DB.IsIntake(ctx, ws)
	if err != nil {
		log.Error(opIntakeRetirement, "could not read whether the workspace whose turn ended is an intake workspace", dlog.WithCause(nil, err))
		return fmt.Errorf("turn end of %q: read whether it is an intake workspace: %w", ws, err)
	}
	if !intake {
		return nil
	}
	return v.retireIntake(ctx, ws)
}
