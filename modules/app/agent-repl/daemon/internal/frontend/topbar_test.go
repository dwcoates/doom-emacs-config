package frontend

import (
	"encoding/json"
	"os"
	"path/filepath"
	"slices"
	"testing"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// validTopbarInputs is a topbar that resolves, so each case below can vary the
// one fact it is about and nothing else.
func validTopbarInputs() TopbarInputs {
	return TopbarInputs{
		Workspace:    "/home/u/workspace/agent-repl",
		Fence:        "s_1|g_1",
		Connectivity: frontendv1.SessionConnectivity_SESSION_CONNECTIVITY_OPERATIONAL,
	}
}

func TestTopbarViewRefusesAnEmptyWorkspace(t *testing.T) {
	// Arrange.
	in := validTopbarInputs()
	in.Workspace = ""
	// Act.
	view, err := TopbarView(in)
	// Assert.
	if err == nil {
		t.Fatalf("a topbar with no workspace resolved: %v", view)
	}
}

func TestTopbarViewRefusesAnEmptyFence(t *testing.T) {
	// Arrange — an unfenced push cannot be told from a stale one.
	in := validTopbarInputs()
	in.Fence = ""
	// Act.
	view, err := TopbarView(in)
	// Assert.
	if err == nil {
		t.Fatalf("an unfenced topbar resolved: %v", view)
	}
}

func TestTopbarViewRefusesAnUnknownConnectivity(t *testing.T) {
	// Arrange — a value from a wire this build was not built for.
	in := validTopbarInputs()
	in.Connectivity = frontendv1.SessionConnectivity(9999)
	// Act.
	view, err := TopbarView(in)
	// Assert.
	if err == nil {
		t.Fatalf("a topbar with an unresolvable connectivity resolved: %v", view)
	}
}

func TestTopbarViewCarriesTheFenceItWasGiven(t *testing.T) {
	// Arrange — the resolver carries a minted fence; it never composes one.
	in := validTopbarInputs()
	in.Fence = "s_abc|g_def"
	// Act.
	view, err := TopbarView(in)
	// Assert.
	if err != nil {
		t.Fatalf("TopbarView: %v", err)
	}
	if view.GetFence() != "s_abc|g_def" {
		t.Fatalf("fence = %q, want the fence supplied verbatim", view.GetFence())
	}
}

func TestTopbarTitleIsTheWorkspaceNameWithoutABranch(t *testing.T) {
	// Arrange.
	in := validTopbarInputs()
	in.Branch = ""
	// Act.
	view, err := TopbarView(in)
	// Assert.
	if err != nil {
		t.Fatalf("TopbarView: %v", err)
	}
	if view.GetTitle() != "agent-repl" {
		t.Fatalf("title = %q, want the workspace's base name alone", view.GetTitle())
	}
}

func TestTopbarTitleCarriesTheBranchWhenThereIsOne(t *testing.T) {
	// Arrange.
	in := validTopbarInputs()
	in.Branch = "feature-daemon-views"
	// Act.
	view, err := TopbarView(in)
	// Assert.
	if err != nil {
		t.Fatalf("TopbarView: %v", err)
	}
	if view.GetTitle() != "agent-repl (feature-daemon-views)" {
		t.Fatalf("title = %q, want the name and branch composed daemon-side", view.GetTitle())
	}
}

func TestTopbarSessionLineIsEmptyForAWorkspaceWithNoSession(t *testing.T) {
	// Arrange — a workspace between sessions has no identity to state.
	in := validTopbarInputs()
	in.SessionID = ""
	in.ClaudeSessionID = "c_1"
	// Act.
	view, err := TopbarView(in)
	// Assert.
	if err != nil {
		t.Fatalf("TopbarView: %v", err)
	}
	if view.GetSessionLine() != "" {
		t.Fatalf("session line = %q, want empty for a workspace with no session", view.GetSessionLine())
	}
}

func TestTopbarSessionLineNamesTheSessionAloneWithoutAConversation(t *testing.T) {
	// Arrange.
	in := validTopbarInputs()
	in.SessionID = "s_1"
	in.ClaudeSessionID = ""
	// Act.
	view, err := TopbarView(in)
	// Assert.
	if err != nil {
		t.Fatalf("TopbarView: %v", err)
	}
	if view.GetSessionLine() != "session s_1" {
		t.Fatalf("session line = %q", view.GetSessionLine())
	}
}

func TestTopbarSessionLineNamesBothIdentitiesWhenBothExist(t *testing.T) {
	// Arrange.
	in := validTopbarInputs()
	in.SessionID = "s_1"
	in.ClaudeSessionID = "c_9"
	// Act.
	view, err := TopbarView(in)
	// Assert.
	if err != nil {
		t.Fatalf("TopbarView: %v", err)
	}
	if view.GetSessionLine() != "session s_1 · conversation c_9" {
		t.Fatalf("session line = %q", view.GetSessionLine())
	}
}

func TestTopbarViewCarriesTheModelMenuInTheOrderGiven(t *testing.T) {
	// Arrange — the selector renders exactly this list.
	in := validTopbarInputs()
	in.ModelOptions = []*frontendv1.ModelOption{{Value: "opus-5"}, {Value: "sonnet-4"}}
	// Act.
	view, err := TopbarView(in)
	// Assert.
	if err != nil {
		t.Fatalf("TopbarView: %v", err)
	}
	got := []string{view.GetModelOptions()[0].GetValue(), view.GetModelOptions()[1].GetValue()}
	if !slices.Equal(got, []string{"opus-5", "sonnet-4"}) {
		t.Fatalf("model options = %v, want them in display order", got)
	}
}

func TestTopbarViewCarriesTheAccountingLineVerbatim(t *testing.T) {
	// Arrange — the topbar shows the sentence and never the verdict.
	in := validTopbarInputs()
	in.AccountingLine = "5h 1.0%→2.0% (1.0pp) · 4s"
	// Act.
	view, err := TopbarView(in)
	// Assert.
	if err != nil {
		t.Fatalf("TopbarView: %v", err)
	}
	if view.GetAccountingLine() != "5h 1.0%→2.0% (1.0pp) · 4s" {
		t.Fatalf("accounting line = %q, want it verbatim", view.GetAccountingLine())
	}
}

func TestEverySessionConnectivityResolvesAnIndicator(t *testing.T) {
	// Arrange — the GENERATED name table is the authority on membership, so a
	// connectivity added to the proto with no indicator fails here rather than
	// reaching a frontend that must invent one.
	for value := range frontendv1.SessionConnectivity_name {
		c := frontendv1.SessionConnectivity(value)
		// Act.
		got, err := TopbarConnectivity(c)
		// Assert.
		if err != nil {
			t.Errorf("SessionConnectivity %s has no indicator: %v", c, err)
			continue
		}
		if got.GetGlyph() == "" || got.GetTitle() == "" {
			t.Errorf("SessionConnectivity %s resolved an indicator with no glyph or no tooltip", c)
		}
	}
}

func TestEveryConnectivityToneIsInTheSharedColorVocabulary(t *testing.T) {
	// Arrange — the same fixture internal/errclass asserts its two tones
	// against. A tone this file invented would be a third answer to a question
	// that file exists to have exactly one of.
	colors := loadRenderColors(t)
	for value := range frontendv1.SessionConnectivity_name {
		// Act.
		got, err := TopbarConnectivity(frontendv1.SessionConnectivity(value))
		if err != nil {
			t.Fatalf("TopbarConnectivity(%d): %v", value, err)
		}
		// Assert.
		if got.GetTone() != "none" && !slices.Contains(colors, got.GetTone()) {
			t.Errorf("SessionConnectivity %d resolved tone %q, which is not in the shared vocabulary", value, got.GetTone())
		}
	}
}

func TestHibernatedConnectivityTakesTheTealTheSharedTableAssignsIt(t *testing.T) {
	// Arrange — hibernation is teal in render_states, and it must be teal here:
	// the benign half of the old blue is the whole reason teal exists.
	// Act.
	got, err := TopbarConnectivity(frontendv1.SessionConnectivity_SESSION_CONNECTIVITY_HIBERNATED)
	// Assert.
	if err != nil {
		t.Fatalf("TopbarConnectivity: %v", err)
	}
	if got.GetTone() != "teal" {
		t.Fatalf("hibernated tone = %q, want teal", got.GetTone())
	}
}

func TestAnUnresolvedConnectivityTakesNoColorAtAll(t *testing.T) {
	// Arrange — UNSPECIFIED is the absence of a verdict, not a verdict.
	// Act.
	got, err := TopbarConnectivity(frontendv1.SessionConnectivity_SESSION_CONNECTIVITY_UNSPECIFIED)
	// Assert.
	if err != nil {
		t.Fatalf("TopbarConnectivity: %v", err)
	}
	if got.GetTone() != "none" {
		t.Fatalf("unspecified tone = %q, want none", got.GetTone())
	}
}

// loadRenderColors reads the six-color list off the cross-language contract.
func loadRenderColors(t *testing.T) []string {
	t.Helper()
	raw, err := os.ReadFile(filepath.FromSlash("../../../proto/vocab/render-colors.json"))
	if err != nil {
		t.Fatalf("read the color fixture: %v", err)
	}
	var fixture struct {
		Colors []string `json:"colors"`
	}
	if err := json.Unmarshal(raw, &fixture); err != nil {
		t.Fatalf("decode the color fixture: %v", err)
	}
	return fixture.Colors
}
