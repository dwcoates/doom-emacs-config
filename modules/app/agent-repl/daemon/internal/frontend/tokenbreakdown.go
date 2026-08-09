package frontend

// tokenbreakdown.go resolves the TOKEN-BREAKDOWN MENU: the sections and rows
// the counter menu draws, with every figure and every share already computed.
//
// THE ARITHMETIC IS THE DAEMON'S. The webapp used to expand each aggregate
// into rows itself (webapp/src/tokens.ts), which put a second owner of the
// session's economics in a renderer: the vendor's three input counters are
// disjoint buckets, reading any one of them as "the cost" is the standing
// mistake, and a renderer that re-partitions them is a renderer that can make
// it. Rows arrive resolved, shares arrive precomputed, and the client renders
// the tree.
//
// THE SOURCE IS internal/tokenusage's canonical shape, reached through the
// aggregates in tokenutilization.go. The SessionTokenUtilization those produce
// is the same aggregate SessionViewFromRecordWithModelsAndUsage has always
// been handed and, until this view existed, never had anywhere to put — its
// `usage` parameter was carried to the shaper and dropped. This is where it
// lands.
//
// THE FENCE IS NOT MINTED HERE; see topbar.go.

import (
	"fmt"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/tokenusage"
)

// shareNotApplicable is the share_permille a row carries when no share applies
// to it — the section's own basis, and the output side, which is not part of
// the input partition at all. It is -1 rather than 0 because 0 is a real zero
// percent, and a client that saw one where the other was meant would print
// "0%" against a figure that has no percentage.
const shareNotApplicable int32 = -1

// TokenBreakdownView resolves one workspace's breakdown menu completely.
//
// It REFUSES rather than degrading, on the same terms as the topbar: no
// workspace is no routing key, no fence cannot be told from stale. A nil
// aggregate is NOT a refusal — a session that has spent nothing has a genuinely
// empty breakdown, and that is a resolved answer rather than a missing one.
//
// An aggregate the daemon failed to resolve canonical totals for IS a refusal.
// Falling back to a local partition of the vendor buckets here would silently
// install a second answer to what the session paid, on exactly the frames where
// the first one failed.
func TokenBreakdownView(workspace, fence string, usage *frontendv1.SessionTokenUtilization) (*frontendv1.TokenBreakdownView, error) {
	if workspace == "" {
		return nil, fmt.Errorf("frontend: token breakdown view requires a workspace")
	}
	if fence == "" {
		return nil, fmt.Errorf("frontend: token breakdown view for workspace %q requires the workspace's fence; an unfenced push cannot be told from a stale one", workspace)
	}
	view := &frontendv1.TokenBreakdownView{Workspace: workspace, Fence: fence}
	if usage == nil {
		return view, nil
	}
	if usage.GetMainAgentTokens() == nil || usage.GetAllAgentsTokens() == nil {
		return nil, fmt.Errorf("frontend: token breakdown view for workspace %q got a session aggregate with no daemon-resolved canonical totals; the menu is withheld rather than re-partitioned from the vendor buckets", workspace)
	}
	view.Sections = append(view.Sections,
		&frontendv1.TokenBreakdownSection{Label: "main agent", Rows: usageRows(usage.GetMainAgentTokens())},
		&frontendv1.TokenBreakdownSection{Label: "all agents", Rows: usageRows(usage.GetAllAgentsTokens())},
	)
	for index, model := range usage.GetModels() {
		tokens, err := tokenusage.FromTotals(model.GetTotals())
		if err != nil {
			return nil, fmt.Errorf("frontend: token breakdown view for workspace %q: model %d (%q) totals cannot be made canonical: %w", workspace, index, model.GetModel(), err)
		}
		view.Sections = append(view.Sections, &frontendv1.TokenBreakdownSection{
			Label: "model " + model.GetModel(),
			Rows:  usageRows(tokens),
		})
	}
	for index, agent := range usage.GetSubagents() {
		if agent.GetTokens() == nil {
			return nil, fmt.Errorf("frontend: token breakdown view for workspace %q: subagent %d has no daemon-resolved canonical totals", workspace, index)
		}
		view.Sections = append(view.Sections, &frontendv1.TokenBreakdownSection{
			Label: "subagent " + subagentLabel(agent.GetAgent()),
			Rows:  usageRows(agent.GetTokens()),
		})
	}
	return view, nil
}

// subagentLabel names a subagent section by whichever stable identifier the
// SDK gave it. An invocation with neither is labeled as unidentified rather
// than as an empty string, so a section can never render with no heading.
func subagentLabel(agent *frontendv1.TokenUtilizationSubagent) string {
	if id := agent.GetAgentId(); id != "" {
		return id
	}
	if id := agent.GetParentToolUseId(); id != "" {
		return id
	}
	return "(unidentified)"
}

// usageRows expands one canonical usage into the section's rows, in display
// order.
//
// THE SHAPE MIRRORS THE CANONICAL MESSAGE'S OWN ARGUMENT. The input total
// headlines; the three DISJOINT buckets sit indented beneath it, each with its
// share of that total; the expensive sum of the two misses gets its own
// unindented row so the figure every cost judgment reads is present as a
// figure rather than as an addition the reader performs; and output stands
// alone with no share, because there is no output cache to partition and it is
// not part of the input basis.
//
// The bucket row is named "fresh input", never "uncached": cache WRITES are
// uncached too, and calling the unwritten bucket that put the cheap/expensive
// split one row above where it actually is.
func usageRows(u *frontendv1.TokenUsage) []*frontendv1.TokenBreakdownRow {
	read := int64(u.GetInputHits().GetRead())
	written := int64(u.GetInputMisses().GetWritten())
	unwritten := int64(u.GetInputMisses().GetUnwritten())
	input := read + written + unwritten
	return []*frontendv1.TokenBreakdownRow{
		{Label: "input", Tokens: input, SharePermille: shareNotApplicable, Emphasized: true},
		{Label: "fresh input", Tokens: unwritten, SharePermille: sharePermille(unwritten, input), Depth: 1},
		{Label: "cache read", Tokens: read, SharePermille: sharePermille(read, input), Depth: 1},
		{Label: "cache write", Tokens: written, SharePermille: sharePermille(written, input), Depth: 1},
		{Label: "uncached input", Tokens: tokenusage.ExpensiveInput(u), SharePermille: sharePermille(written+unwritten, input), Emphasized: true},
		{Label: "output", Tokens: int64(u.GetOutputTokens()), SharePermille: shareNotApplicable, Emphasized: true},
	}
}

// sharePermille is part's share of basis in permille, rounded to nearest.
//
// A ZERO BASIS HAS NO SHARE, and says so. Reporting 0 permille for a section
// that spent nothing would render "0%" of nothing as though it were a measured
// result, which is the reading shareNotApplicable exists to keep distinct from
// a real zero percent.
func sharePermille(part, basis int64) int32 {
	if basis <= 0 {
		return shareNotApplicable
	}
	return int32((2*1000*part + basis) / (2 * basis))
}
