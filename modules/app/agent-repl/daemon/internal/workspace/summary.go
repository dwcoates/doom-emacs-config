package workspace

import (
	"context"
	"fmt"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/refusalarm"
	"agent-repld/internal/wsm"
	"agent-repld/internal/wssummary"
)

// rpcSetSummary is the rpc a summary's refusals are spelled under.
const rpcSetSummary = "SetWorkspaceSummary"

// opSetSummary is the operation SetSummary's outcome is recorded under.
const opSetSummary = "daemon.workspace.set_summary"

// The parts of a summary an invalid_summary refusal names
// (agentrepl.v1.SetWorkspaceSummaryInvalidSummary.part).
const (
	summaryPartLong  = refusalarm.Nested("long_summary")
	summaryPartShort = refusalarm.Nested("short_summary")
)

// SetSummary records SUMMARY as the workspace's own statement of its task and
// draws it in the topbar (agentrepl.v1.SetWorkspaceSummary). A long summary
// that is not two to four sentences in one paragraph, or a short summary that
// is not one plain sentence of two to fifteen words, is refused
// invalid_summary naming the part and the validator's reason, before
// anything is written; the long summary is judged first. It answers the
// summary the workspace now has.
func (v *verbs) SetSummary(ctx context.Context, ws ids.WorkspaceID, summary wsm.WorkspaceSummary) (wsm.WorkspaceSummary, error) {
	_, log, err := v.owned(ctx, rpcSetSummary, ws)
	if err != nil {
		return wsm.WorkspaceSummary{}, err
	}
	if err := wssummary.ValidateLong(summary.Long); err != nil {
		return wsm.WorkspaceSummary{}, refuseWith(log, rpcSetSummary, ArmInvalidSummary,
			fmt.Sprintf("%q is not a long summary the title's hover shows: %v", summary.Long, err), false,
			map[string]any{"part": summaryPartLong, "long_summary": summary.Long, "reason": err.Error()})
	}
	if err := wssummary.ValidateShort(summary.Short); err != nil {
		return wsm.WorkspaceSummary{}, refuseWith(log, rpcSetSummary, ArmInvalidSummary,
			fmt.Sprintf("%q is not a short summary the title line draws: %v", summary.Short, err), false,
			map[string]any{"part": summaryPartShort, "short_summary": summary.Short, "reason": err.Error()})
	}
	if err := v.deps.Summaries.Set(ctx, ws, summary); err != nil {
		return wsm.WorkspaceSummary{}, err
	}
	log.Info(opSetSummary, "set the workspace summary", dlog.Context{"short_summary": summary.Short, "long_summary": summary.Long})
	return summary, nil
}
