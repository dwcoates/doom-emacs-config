package topbar

import (
	"sort"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// tokenBreakdown resolves the context chip's hover content: the SESSION's token
// usage, always populated so the hover needs no round-trip.
//
// SESSION-SCOPED ONLY. Turn figures are the FOOTER's domain exclusively — the
// topbar is turn-nonspecific by nature — so no turn-scoped section is ever
// built here, and the per-model sections come from the session's own
// accumulation of usage frames rather than from any turn's.
func (r *resolver) tokenBreakdown(s *wsState) *frontendv1.TokenBreakdownView {
	out := &frontendv1.TokenBreakdownView{
		Sections: []*frontendv1.TokenBreakdownSection{
			breakdownSection("session", s.totals),
		},
	}
	models := make([]*modelTotals, 0, len(s.perModel))
	for _, model := range s.perModel {
		models = append(models, model)
	}
	sort.Slice(models, func(i, j int) bool { return models[i].order < models[j].order })
	for _, model := range models {
		out.Sections = append(out.Sections, breakdownSection(model.model, model.figures))
	}
	return out
}

// breakdownSection renders one titled section. The shares are precomputed
// against the section's OWN basis — its uncached input, its cache reads and its
// output together — so the client does no arithmetic and a per-model section's
// percentages read against that model rather than against the session.
func breakdownSection(heading string, figures usageFingerprint) *frontendv1.TokenBreakdownSection {
	basis := int64(figures.misses() + figures.read + figures.output)
	return &frontendv1.TokenBreakdownSection{
		Heading: &frontendv1.TokenBreakdownHeading{Text: heading},
		Rows: []*frontendv1.TokenBreakdownRow{
			headlineRow("uncached input", int64(figures.misses()), basis),
			detailRow("fresh input", int64(figures.unwritten), 1),
			detailRow("cache write", int64(figures.written), 1),
			headlineRow("cache read", int64(figures.read), basis),
			headlineRow("output", int64(figures.output), basis),
			detailRow("thinking", int64(figures.thinking), 1),
		},
	}
}

// headlineRow is a top-level row: emphasized, undented, and carrying its share
// of the section's basis.
func headlineRow(label string, tokens, basis int64) *frontendv1.TokenBreakdownRow {
	row := &frontendv1.TokenBreakdownRow{Label: label, Tokens: tokens, Emphasized: true}
	if share, ok := permilleOf(tokens, basis); ok {
		row.SharePermille = &share
	}
	return row
}

// detailRow is a nested row. It carries NO share: it is a partition of the
// headline above it, and a percentage of the section's basis would invite
// adding it to a figure it is already inside.
func detailRow(label string, tokens int64, depth int32) *frontendv1.TokenBreakdownRow {
	return &frontendv1.TokenBreakdownRow{Label: label, Tokens: tokens, Depth: depth}
}
