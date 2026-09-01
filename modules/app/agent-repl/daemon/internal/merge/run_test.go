package merge

import (
	"os"
	"path/filepath"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
)

// TestSameDirExcludesASiblingWorktree covers the self-reload's extra condition:
// two checkouts of one repository are not one directory.
func TestSameDirExcludesASiblingWorktree(t *testing.T) {
	tests := []struct {
		name string
		a, b string
		want bool
	}{
		{name: "the same directory", a: "/checkout", b: "/checkout", want: true},
		{name: "an unclean spelling of it", a: "/checkout", b: "/checkout/x/..", want: true},
		{name: "a sibling worktree", a: "/checkout", b: "/checkout-2", want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: two directory spellings.
			a, b := tc.a, tc.b

			// Act.
			got := sameDir(a, b)

			// Assert.
			if got != tc.want {
				t.Fatalf("sameDir(%q, %q) = %v, want %v", a, b, got, tc.want)
			}
		})
	}
}

// TestMethodNameNamesTheMethod covers the log record's own vocabulary, which is
// how a merge's method is read back after the fact.
func TestMethodNameNamesTheMethod(t *testing.T) {
	tests := []struct {
		name      string
		emacsRepo bool
		want      string
	}{
		{name: "the daemon's own repository", emacsRepo: true, want: "emacs_repo"},
		{name: "any other repository", emacsRepo: false, want: "other_repo"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: which repository the target is.
			emacsRepo := tc.emacsRepo

			// Act.
			got := methodName(emacsRepo)

			// Assert.
			if got != tc.want {
				t.Fatalf("methodName(%v) = %q, want %q", emacsRepo, got, tc.want)
			}
		})
	}
}

// TestPromptTabCannotPark covers the tab family's legality: the prompt tabs have
// no parked arm, because a pre-prompt failure fails the run and a post-prompt
// failure rides the terminal.
func TestPromptTabCannotPark(t *testing.T) {
	tests := []struct {
		name string
		kind string
	}{
		{name: "the pre-prompt tab", kind: TabPrePrompt},
		{name: "the post-prompt tab", kind: TabPostPrompt},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a settled prompt tab.
			tab := promptTab(tc.kind, nil, 1000, "")

			// Act.
			kind := tabKindOf(tab)

			// Assert.
			if kind != tc.kind {
				t.Fatalf("the tab is %q, want %q", kind, tc.kind)
			}
			if tc.kind == TabPrePrompt {
				if _, ok := tab.GetPrePrompt().GetState().(*frontendv1.FeedMergeTabPrePrompt_Settled); !ok {
					t.Fatalf("the pre-prompt tab is %T, want settled", tab.GetPrePrompt().GetState())
				}
			} else {
				if _, ok := tab.GetPostPrompt().GetState().(*frontendv1.FeedMergeTabPostPrompt_Settled); !ok {
					t.Fatalf("the post-prompt tab is %T, want settled", tab.GetPostPrompt().GetState())
				}
			}
		})
	}
}

// TestPromptTabCarriesItsFailureSummary covers what a failed prompt tab draws:
// the daemon's one-line account, with the detail in the tab's own content.
func TestPromptTabCarriesItsFailureSummary(t *testing.T) {
	// Arrange: a failed pre-prompt tab.
	tab := promptTab(TabPrePrompt, nil, 1000, "the before-merge prompt did not complete")

	// Act.
	settled := tab.GetPrePrompt().GetSettled()

	// Assert.
	if settled.GetFailed().GetSummary() != "the before-merge prompt did not complete" {
		t.Fatalf("the tab reads %q, want the composed summary", settled.GetFailed().GetSummary())
	}
}

// TestReadEscalationIgnoresAMissingRecord covers the ordinary case: no record
// means the agent is still working the problem.
func TestReadEscalationIgnoresAMissingRecord(t *testing.T) {
	// Arrange: a target with no record.
	dir := t.TempDir()

	// Act.
	_, escalated := readEscalation(dir)

	// Assert.
	if escalated {
		t.Fatal("a target with no record read as an escalation")
	}
}

// TestReadEscalationReturnsTheAgentsReason covers what the parked line carries:
// the agent's own words, which is what a human reads.
func TestReadEscalationReturnsTheAgentsReason(t *testing.T) {
	// Arrange: a record with a reason under the marker.
	dir := t.TempDir()
	body := EscalationMarker + "\nthe storage layer needs redesigning\n"
	if err := os.WriteFile(filepath.Join(dir, EscalationFile), []byte(body), 0o644); err != nil {
		t.Fatalf("writing the record: %v", err)
	}

	// Act.
	why, escalated := readEscalation(dir)

	// Assert.
	if !escalated || why != "the storage layer needs redesigning" {
		t.Fatalf("readEscalation answered (%q, %v), want the agent's reason", why, escalated)
	}
}
