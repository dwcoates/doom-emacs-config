package workspace

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wssummary"
)

// rpcSetSummary is the rpc a summary's refusals are spelled under.
const rpcSetSummary = "SetWorkspaceSummary"

// opSetSummary is the operation SetSummary's outcome is recorded under.
const opSetSummary = "daemon.workspace.set_summary"

// SetSummary records SUMMARY as the workspace's own statement of its task and
// draws it in the topbar (agentrepl.v1.SetWorkspaceSummary). A summary that
// is not one plain sentence of two to fifteen words is refused
// invalid_summary, with the validator's reason, before anything is written.
// It answers the summary the workspace now has.
func (v *verbs) SetSummary(ctx context.Context, ws ids.WorkspaceID, summary string) (string, error) {
	_, log, err := v.owned(ctx, rpcSetSummary, ws)
	if err != nil {
		return "", err
	}
	if err := wssummary.Validate(summary); err != nil {
		return "", refuseWith(log, rpcSetSummary, ArmInvalidSummary,
			fmt.Sprintf("%q is not a summary the title line draws: %v", summary, err), false,
			map[string]any{"summary": summary, "reason": err.Error()})
	}
	if err := v.deps.Summaries.Set(ctx, ws, summary); err != nil {
		return "", err
	}
	log.Info(opSetSummary, "set the workspace summary", dlog.Context{"summary": summary})
	return summary, nil
}
