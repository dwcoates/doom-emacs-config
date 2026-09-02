package topbar

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// sections is the last published breakdown's sections.
func sections(t *testing.T, h *harness) []*frontendv1.TokenBreakdownSection {
	t.Helper()
	return h.view(t).GetContext().GetBreakdown().GetSections()
}

// rowByLabel finds one row of a section.
func rowByLabel(section *frontendv1.TokenBreakdownSection, label string) *frontendv1.TokenBreakdownRow {
	for _, row := range section.GetRows() {
		if row.GetLabel() == label {
			return row
		}
	}
	return nil
}

func TestTheBreakdownAlwaysCarriesTheSessionSection(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.ready(t)

	// Assert
	got := sections(t, h)
	if len(got) == 0 || got[0].GetHeading().GetText() != "session" {
		t.Fatalf("sections = %+v, want the session section first", got)
	}
}

func TestSessionTotalsSumEveryUnitsUsage(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.OnActivity(testWS, nil, responseSettled("r1", usage(1_000, 2_000, 500, 300, 100)))
	h.r.OnActivity(testWS, nil, responseSettled("r2", usage(3_000, 1_000, 500, 200, 50)))

	// Assert
	session := sections(t, h)[0]
	if got := rowByLabel(session, "uncached input").GetTokens(); got != 4_000 {
		t.Fatalf("uncached input = %d, want written + unwritten across both units", got)
	}
	if got := rowByLabel(session, "cache read").GetTokens(); got != 4_000 {
		t.Fatalf("cache read = %d, want 4000", got)
	}
	if got := rowByLabel(session, "output").GetTokens(); got != 500 {
		t.Fatalf("output = %d, want 500", got)
	}
}

func TestARepeatedUsageIsNotDoubleCounted(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)
	frame := responseSettled("r1", usage(0, 5_000, 0, 0, 0))

	// Act: the same unit upserts, re-reporting the same usage each time.
	h.r.OnActivity(testWS, nil, frame)
	h.r.OnActivity(testWS, nil, frame)
	h.r.OnActivity(testWS, nil, frame)

	// Assert
	if got := rowByLabel(sections(t, h)[0], "uncached input").GetTokens(); got != 5_000 {
		t.Fatalf("uncached input = %d, want 5000: usage is keyed by unit and REPLACED", got)
	}
}

func TestACorrectedUsageReplacesItsPredecessor(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)
	h.r.OnActivity(testWS, nil, responseSettled("r1", usage(0, 100, 0, 0, 0)))

	// Act
	h.r.OnActivity(testWS, nil, responseSettled("r1", usage(0, 900, 0, 0, 0)))

	// Assert
	if got := rowByLabel(sections(t, h)[0], "uncached input").GetTokens(); got != 900 {
		t.Fatalf("uncached input = %d, want the corrected figure alone, not the sum", got)
	}
}

func TestTheCacheSplitRidesItsOwnNestedRows(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.OnActivity(testWS, nil, responseSettled("r1", usage(0, 3_000, 500, 0, 0)))

	// Assert
	session := sections(t, h)[0]
	if got := rowByLabel(session, "cache write"); got.GetTokens() != 3_000 || got.GetDepth() != 1 {
		t.Fatalf("cache write = %+v, want 3000 nested under its headline", got)
	}
	if got := rowByLabel(session, "fresh input"); got.GetTokens() != 500 || got.GetDepth() != 1 {
		t.Fatalf("fresh input = %+v, want 500 nested", got)
	}
}

func TestANestedRowCarriesNoShare(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.OnActivity(testWS, nil, responseSettled("r1", usage(0, 3_000, 500, 100, 40)))

	// Assert
	session := sections(t, h)[0]
	for _, label := range []string{"fresh input", "cache write", "thinking"} {
		if row := rowByLabel(session, label); row.SharePermille != nil {
			t.Fatalf("%s carries a share; a partition of its headline must not", label)
		}
	}
}

func TestAHeadlineRowCarriesItsPrecomputedShare(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act: 1k uncached, 1k cache read, 2k output — a 4k basis.
	h.r.OnActivity(testWS, nil, responseSettled("r1", usage(1_000, 1_000, 0, 2_000, 0)))

	// Assert
	session := sections(t, h)[0]
	row := rowByLabel(session, "output")
	if row.SharePermille == nil || *row.SharePermille != 500 {
		t.Fatalf("output share = %+v, want 500 permille of the 4k basis", row.SharePermille)
	}
	if !row.GetEmphasized() {
		t.Fatalf("output row = %+v, want emphasized", row)
	}
}

func TestAnEmptyBasisLeavesEveryShareUnset(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.ready(t)

	// Assert
	for _, row := range sections(t, h)[0].GetRows() {
		if row.SharePermille != nil {
			t.Fatalf("row %q carries a share with no basis to divide by", row.GetLabel())
		}
	}
}

func TestUsageIsAttributedToTheModelInForce(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.r.SetModelCatalog(testWS, []*conversationv1.ModelOption{
		modelOption("claude-opus-5", "Opus 5"), modelOption("claude-haiku-4-5", "Haiku 4.5"),
	})
	h.ready(t)

	// Act
	h.r.OnActivity(testWS, nil, responseSettled("r1", usage(0, 1_000, 0, 0, 0)))
	h.r.OnSessionUpdate(testWS, modelChanged("claude-haiku-4-5"))
	h.r.OnActivity(testWS, nil, responseSettled("r2", usage(0, 4_000, 0, 0, 0)))

	// Assert
	got := sections(t, h)
	if len(got) != 3 {
		t.Fatalf("sections = %d, want the session plus one per model", len(got))
	}
	byHeading := map[string]*frontendv1.TokenBreakdownSection{}
	for _, section := range got {
		byHeading[section.GetHeading().GetText()] = section
	}
	if tokens := rowByLabel(byHeading["claude-opus-5"], "uncached input").GetTokens(); tokens != 1_000 {
		t.Fatalf("opus uncached input = %d, want 1000", tokens)
	}
	if tokens := rowByLabel(byHeading["claude-haiku-4-5"], "uncached input").GetTokens(); tokens != 4_000 {
		t.Fatalf("haiku uncached input = %d, want 4000", tokens)
	}
}

func TestPerModelSharesReadAgainstThatModelsOwnBasis(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act: one model, 1k uncached and 1k output — its own 2k basis.
	h.r.OnActivity(testWS, nil, responseSettled("r1", usage(0, 1_000, 0, 1_000, 0)))

	// Assert
	var model *frontendv1.TokenBreakdownSection
	for _, section := range sections(t, h) {
		if section.GetHeading().GetText() == "claude-opus-5" {
			model = section
		}
	}
	row := rowByLabel(model, "output")
	if row.SharePermille == nil || *row.SharePermille != 500 {
		t.Fatalf("share = %+v, want 500 permille of the model's own basis", row.SharePermille)
	}
}

func TestPerModelSectionsKeepTheirFirstSeenOrder(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.OnActivity(testWS, nil, responseSettled("r1", usage(0, 1, 0, 0, 0)))
	h.r.OnSessionUpdate(testWS, modelChanged("model-b"))
	h.r.OnActivity(testWS, nil, responseSettled("r2", usage(0, 1, 0, 0, 0)))
	h.r.OnSessionUpdate(testWS, modelChanged("model-c"))
	h.r.OnActivity(testWS, nil, responseSettled("r3", usage(0, 1, 0, 0, 0)))

	// Assert
	got := sections(t, h)
	headings := []string{got[1].GetHeading().GetText(), got[2].GetHeading().GetText(), got[3].GetHeading().GetText()}
	want := []string{"claude-opus-5", "model-b", "model-c"}
	for i := range want {
		if headings[i] != want[i] {
			t.Fatalf("headings = %v, want %v", headings, want)
		}
	}
}

func TestNoTurnScopedSectionEverAppears(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ready(t)

	// Act
	h.r.OnActivity(testWS, nil, responseSettled("r1", usage(0, 1_000, 0, 0, 0)))

	// Assert
	for _, section := range sections(t, h) {
		if section.GetHeading().GetText() == "turn" {
			t.Fatalf("a turn-scoped section appeared; turn figures are the footer's domain")
		}
	}
}
