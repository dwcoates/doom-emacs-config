package topbar

import (
	"strings"
	"testing"
)

func TestProductionChecksChipProjectsTheStanding(t *testing.T) {
	tests := []struct {
		name        string
		chip        ProductionChecks
		wantNil     bool
		wantPaused  bool
		wantTooltip string
	}{
		{name: "a repository that checks no production draws no chip", chip: ProductionChecks{}, wantNil: true},
		{name: "running checks draw the running arm naming the mediator", chip: ProductionChecks{On: true, Mediator: "prod-mediator", Repository: "repo-1", RepositoryDir: "/r"},
			wantTooltip: "Production checks are running, reviewed by prod-mediator · click to pause them"},
		{name: "paused checks draw the paused arm saying a click resumes", chip: ProductionChecks{On: true, Paused: true, Mediator: "prod-mediator", Repository: "repo-1", RepositoryDir: "/r"},
			wantPaused: true, wantTooltip: "Production checks are paused: prod-mediator is told no span · click to resume them"},
		{name: "checks with no mediator yet say so", chip: ProductionChecks{On: true, Repository: "repo-1", RepositoryDir: "/r"},
			wantTooltip: "reviewed by its production mediator, not made yet"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := productionChecksChip(tt.chip)

			// Assert.
			if (got == nil) != tt.wantNil {
				t.Fatalf("chip = %v, want nil %v", got, tt.wantNil)
			}
			if got == nil {
				return
			}
			if (got.GetPaused() != nil) != tt.wantPaused || (got.GetRunning() != nil) == tt.wantPaused {
				t.Fatalf("standing = %v, want paused %v", got.GetStanding(), tt.wantPaused)
			}
			if !strings.Contains(got.GetTooltip().GetText(), tt.wantTooltip) {
				t.Fatalf("tooltip = %q, want it to hold %q", got.GetTooltip().GetText(), tt.wantTooltip)
			}
			if got.GetRepository().GetId() != "repo-1" || got.GetRepository().GetDir() != "/r" {
				t.Fatalf("repository = %v, want the click's repository echoed", got.GetRepository())
			}
		})
	}
}

func TestAProductionChecksChangeIsPublishedAndKeepsTheName(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)

	// Act.
	h.r.SetProductionChecks(testWS, ProductionChecks{On: true, Paused: true, Repository: "repo-1", RepositoryDir: "/r"})

	// Assert.
	view := h.view(t)
	if view.GetProductionChecks().GetPaused() == nil {
		t.Fatalf("chip = %v, want paused after the change", view.GetProductionChecks())
	}
	if got := view.GetWorkspaceName().GetText(); got != "fix-flaky-reconnect" {
		t.Fatalf("name = %q, want the naming's name kept", got)
	}
}

func TestARenameKeepsTheProductionChecks(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)
	h.r.SetProductionChecks(testWS, ProductionChecks{On: true, Repository: "repo-1", RepositoryDir: "/r"})

	// Act.
	h.r.SetName(testWS, "renamed")

	// Assert.
	if chip := h.view(t).GetProductionChecks(); chip.GetRunning() == nil {
		t.Fatalf("chip = %v, want the standing kept across a rename", chip)
	}
}
