package frontend

import (
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// canonical builds one canonical usage from the three input buckets plus
// output, so each case below states the arithmetic it is about and nothing
// else.
func canonical(read, written, unwritten, output uint64) *frontendv1.TokenUsage {
	return &frontendv1.TokenUsage{
		InputHits:    &frontendv1.TokenCacheHits{Read: read},
		InputMisses:  &frontendv1.TokenCacheMisses{Written: written, Unwritten: unwritten},
		OutputTokens: output,
	}
}

// aggregate is a session aggregate carrying only the daemon-resolved canonical
// totals every section needs.
func aggregate() *frontendv1.SessionTokenUtilization {
	return &frontendv1.SessionTokenUtilization{
		MainAgentTokens: canonical(0, 0, 0, 0),
		AllAgentsTokens: canonical(0, 0, 0, 0),
	}
}

// rowByLabel finds one row of a section, failing the test when it is absent.
func rowByLabel(t *testing.T, section *frontendv1.TokenBreakdownSection, label string) *frontendv1.TokenBreakdownRow {
	t.Helper()
	for _, row := range section.GetRows() {
		if row.GetLabel() == label {
			return row
		}
	}
	t.Fatalf("section %q has no %q row", section.GetLabel(), label)
	return nil
}

func TestTokenBreakdownViewRefusesAnEmptyWorkspace(t *testing.T) {
	// Arrange + Act.
	view, err := TokenBreakdownView("", "s|g", aggregate())
	// Assert.
	if err == nil {
		t.Fatalf("a breakdown with no workspace resolved: %v", view)
	}
}

func TestTokenBreakdownViewRefusesAnEmptyFence(t *testing.T) {
	// Arrange + Act.
	view, err := TokenBreakdownView("/w", "", aggregate())
	// Assert.
	if err == nil {
		t.Fatalf("an unfenced breakdown resolved: %v", view)
	}
}

func TestTokenBreakdownViewRefusesAnAggregateWithNoCanonicalTotals(t *testing.T) {
	// Arrange — falling back to a local partition here would install a second
	// answer to what the session paid.
	usage := aggregate()
	usage.AllAgentsTokens = nil
	// Act.
	view, err := TokenBreakdownView("/w", "s|g", usage)
	// Assert.
	if err == nil {
		t.Fatalf("a breakdown resolved from an aggregate with no daemon-resolved totals: %v", view)
	}
}

func TestTokenBreakdownViewOfASessionThatSpentNothingIsAnEmptyMenu(t *testing.T) {
	// Arrange — no aggregate is a resolved answer, not a missing one.
	// Act.
	view, err := TokenBreakdownView("/w", "s|g", nil)
	// Assert.
	if err != nil {
		t.Fatalf("TokenBreakdownView: %v", err)
	}
	if len(view.GetSections()) != 0 {
		t.Fatalf("sections = %d, want an empty menu for a session with no aggregate", len(view.GetSections()))
	}
}

func TestTokenBreakdownViewCarriesTheFenceItWasGiven(t *testing.T) {
	// Arrange + Act.
	view, err := TokenBreakdownView("/w", "s_7|g_7", aggregate())
	// Assert.
	if err != nil {
		t.Fatalf("TokenBreakdownView: %v", err)
	}
	if view.GetFence() != "s_7|g_7" {
		t.Fatalf("fence = %q, want the fence supplied verbatim", view.GetFence())
	}
}

func TestTokenBreakdownViewLeadsWithTheMainAgentAndAllAgentsSections(t *testing.T) {
	// Arrange + Act.
	view, err := TokenBreakdownView("/w", "s|g", aggregate())
	// Assert.
	if err != nil {
		t.Fatalf("TokenBreakdownView: %v", err)
	}
	if view.GetSections()[0].GetLabel() != "main agent" || view.GetSections()[1].GetLabel() != "all agents" {
		t.Fatalf("leading sections = %q, %q", view.GetSections()[0].GetLabel(), view.GetSections()[1].GetLabel())
	}
}

func TestTokenBreakdownViewAddsOneSectionPerModel(t *testing.T) {
	// Arrange.
	usage := aggregate()
	usage.Models = []*frontendv1.ModelTokenUtilization{{Model: "opus-5", Totals: &frontendv1.TokenUsageTotals{}}}
	// Act.
	view, err := TokenBreakdownView("/w", "s|g", usage)
	// Assert.
	if err != nil {
		t.Fatalf("TokenBreakdownView: %v", err)
	}
	if view.GetSections()[2].GetLabel() != "model opus-5" {
		t.Fatalf("model section = %q", view.GetSections()[2].GetLabel())
	}
}

func TestTokenBreakdownViewRefusesASubagentWithNoCanonicalTotals(t *testing.T) {
	// Arrange.
	usage := aggregate()
	usage.Subagents = []*frontendv1.AgentTokenUtilization{{Agent: &frontendv1.TokenUtilizationSubagent{AgentId: "a1"}}}
	// Act.
	view, err := TokenBreakdownView("/w", "s|g", usage)
	// Assert.
	if err == nil {
		t.Fatalf("a breakdown resolved with an unresolved subagent total: %v", view)
	}
}

func TestTokenBreakdownViewLabelsASubagentWithNoIdentifierRatherThanBlank(t *testing.T) {
	// Arrange — a section can never render with no heading.
	usage := aggregate()
	usage.Subagents = []*frontendv1.AgentTokenUtilization{{
		Agent:  &frontendv1.TokenUtilizationSubagent{SubagentType: "explore"},
		Tokens: canonical(0, 0, 0, 0),
	}}
	// Act.
	view, err := TokenBreakdownView("/w", "s|g", usage)
	// Assert.
	if err != nil {
		t.Fatalf("TokenBreakdownView: %v", err)
	}
	if view.GetSections()[2].GetLabel() != "subagent (unidentified)" {
		t.Fatalf("subagent section = %q", view.GetSections()[2].GetLabel())
	}
}

func TestTheInputRowIsTheSumOfTheThreeDisjointBuckets(t *testing.T) {
	// Arrange.
	usage := aggregate()
	usage.MainAgentTokens = canonical(700, 200, 100, 50)
	// Act.
	view, err := TokenBreakdownView("/w", "s|g", usage)
	if err != nil {
		t.Fatalf("TokenBreakdownView: %v", err)
	}
	// Assert.
	if got := rowByLabel(t, view.GetSections()[0], "input").GetTokens(); got != 1000 {
		t.Fatalf("input = %d, want 1000", got)
	}
}

func TestTheUncachedInputRowIsBothMissesTogether(t *testing.T) {
	// Arrange — reading either miss alone reports near nothing for exactly the
	// case a cost figure exists to catch.
	usage := aggregate()
	usage.MainAgentTokens = canonical(700, 200, 100, 0)
	// Act.
	view, err := TokenBreakdownView("/w", "s|g", usage)
	if err != nil {
		t.Fatalf("TokenBreakdownView: %v", err)
	}
	// Assert.
	if got := rowByLabel(t, view.GetSections()[0], "uncached input").GetTokens(); got != 300 {
		t.Fatalf("uncached input = %d, want 300 (written + unwritten)", got)
	}
}

func TestABucketRowCarriesItsShareOfTheSectionsInput(t *testing.T) {
	// Arrange.
	usage := aggregate()
	usage.MainAgentTokens = canonical(700, 200, 100, 0)
	// Act.
	view, err := TokenBreakdownView("/w", "s|g", usage)
	if err != nil {
		t.Fatalf("TokenBreakdownView: %v", err)
	}
	// Assert.
	if got := rowByLabel(t, view.GetSections()[0], "cache read").GetSharePermille(); got != 700 {
		t.Fatalf("cache read share = %d permille, want 700", got)
	}
}

func TestAShareIsRoundedRatherThanTruncated(t *testing.T) {
	// Arrange — 1 of 3 is 333.33 permille, and 2 of 3 is 666.67.
	usage := aggregate()
	usage.MainAgentTokens = canonical(2, 0, 1, 0)
	// Act.
	view, err := TokenBreakdownView("/w", "s|g", usage)
	if err != nil {
		t.Fatalf("TokenBreakdownView: %v", err)
	}
	// Assert.
	if got := rowByLabel(t, view.GetSections()[0], "cache read").GetSharePermille(); got != 667 {
		t.Fatalf("cache read share = %d permille, want 667", got)
	}
}

func TestTheInputHeadlineCarriesNoShareBecauseItIsTheBasis(t *testing.T) {
	// Arrange.
	usage := aggregate()
	usage.MainAgentTokens = canonical(1, 1, 1, 1)
	// Act.
	view, err := TokenBreakdownView("/w", "s|g", usage)
	if err != nil {
		t.Fatalf("TokenBreakdownView: %v", err)
	}
	// Assert.
	if got := rowByLabel(t, view.GetSections()[0], "input").GetSharePermille(); got != shareNotApplicable {
		t.Fatalf("input share = %d, want %d", got, shareNotApplicable)
	}
}

func TestTheOutputRowCarriesNoShareBecauseItIsNotInTheInputPartition(t *testing.T) {
	// Arrange.
	usage := aggregate()
	usage.MainAgentTokens = canonical(1, 1, 1, 99)
	// Act.
	view, err := TokenBreakdownView("/w", "s|g", usage)
	if err != nil {
		t.Fatalf("TokenBreakdownView: %v", err)
	}
	// Assert.
	if got := rowByLabel(t, view.GetSections()[0], "output").GetSharePermille(); got != shareNotApplicable {
		t.Fatalf("output share = %d, want %d", got, shareNotApplicable)
	}
}

func TestASectionThatSpentNothingHasNoShareRatherThanZeroPercent(t *testing.T) {
	// Arrange — 0 permille is a measured zero, and this is not one.
	// Act.
	view, err := TokenBreakdownView("/w", "s|g", aggregate())
	if err != nil {
		t.Fatalf("TokenBreakdownView: %v", err)
	}
	// Assert.
	if got := rowByLabel(t, view.GetSections()[0], "cache read").GetSharePermille(); got != shareNotApplicable {
		t.Fatalf("cache read share = %d over a zero basis, want %d", got, shareNotApplicable)
	}
}

func TestABucketRowIsIndentedBeneathItsHeadline(t *testing.T) {
	// Arrange + Act.
	view, err := TokenBreakdownView("/w", "s|g", aggregate())
	if err != nil {
		t.Fatalf("TokenBreakdownView: %v", err)
	}
	// Assert.
	if got := rowByLabel(t, view.GetSections()[0], "fresh input").GetDepth(); got != 1 {
		t.Fatalf("fresh input depth = %d, want 1", got)
	}
}

func TestTheUncachedInputRowIsEmphasizedAndUnindented(t *testing.T) {
	// Arrange — the figure every cost judgment reads is present as a figure.
	// Act.
	view, err := TokenBreakdownView("/w", "s|g", aggregate())
	if err != nil {
		t.Fatalf("TokenBreakdownView: %v", err)
	}
	// Assert.
	row := rowByLabel(t, view.GetSections()[0], "uncached input")
	if !row.GetEmphasized() || row.GetDepth() != 0 {
		t.Fatalf("uncached input emphasized=%v depth=%d, want true/0", row.GetEmphasized(), row.GetDepth())
	}
}
