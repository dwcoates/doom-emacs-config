package feed

import (
	"strings"
	"testing"

	"agent-repld/internal/linediff"
	"agent-repld/internal/pyedit"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
)

// pythonEdit is a command that states one edit to f.go: "b" becomes "B"
// between two shared lines.
const pythonEdit = "python3 - <<'EOF'\nimport pathlib\np = pathlib.Path('f.go')\ns = p.read_text()\n" +
	"s = s.replace('a\\nb\\nc', 'a\\nB\\nc', 1)\np.write_text(s)\nEOF"

// assertStatedEditOfF holds the card to the edits pythonEdit states.
func assertStatedEditOfF(t *testing.T, edits *frontendv1.FeedToolCallEdits) {
	t.Helper()
	files := edits.GetFiles()
	if len(files) != 1 || files[0].GetPath() != "f.go" {
		t.Fatalf("edits = %+v, want one edit of f.go", edits)
	}
	var got []string
	for _, line := range files[0].GetLines() {
		got = append(got, armName(line)+":"+line.GetText())
	}
	want := []string{"header:@@ first occurrence @@", "context:a", "removed:b", "added:B", "context:c"}
	if strings.Join(got, "|") != strings.Join(want, "|") {
		t.Fatalf("lines = %q, want %q", got, want)
	}
}

func armName(line *frontendv1.FeedDiffLine) string {
	switch {
	case line.GetHeader() != nil:
		return "header"
	case line.GetAdded() != nil:
		return "added"
	case line.GetRemoved() != nil:
		return "removed"
	case line.GetContext() != nil:
		return "context"
	}
	return "unset"
}

func completedBash(line string) *conversationv1.AgentBash {
	return &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Success{Success: &conversationv1.AgentBashSuccess{
			Command: &conversationv1.AgentBashCommand{Line: line},
			Outcome: &conversationv1.AgentBashSuccess_Completed{Completed: &conversationv1.AgentBashCompleted{
				Output: &conversationv1.AgentBashOutput{},
			}},
			SettledAt: &conversationv1.AgentActivitySettledAt{AtMs: 9_000},
		}},
	}
}

func TestARunningShellCardCarriesTheEditsItsCommandStates(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	startForegroundBash(h, "unit-1", pythonEdit)

	// Assert.
	if h.card().GetRunning() == nil {
		t.Fatalf("outcome = %T, want running", h.card().GetOutcome())
	}
	assertStatedEditOfF(t, h.card().GetEdits())
	if !h.hasRecord("debug", "daemon.feed.bash_edits_stated") {
		t.Fatalf("records = %+v, want the stated edits recorded", h.records())
	}
}

func TestAReturnedShellCardCarriesTheEditsItsCommandStates(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	startForegroundBash(h, "unit-1", pythonEdit)
	h.send(activityOf("unit-1", completedBash(pythonEdit)))

	// Assert.
	if h.card().GetReturned().GetSucceeded() == nil {
		t.Fatalf("outcome = %T, want returned", h.card().GetOutcome())
	}
	assertStatedEditOfF(t, h.card().GetEdits())
}

func TestALivenessBeatDoesNotReadTheCommandAgain(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	startForegroundBash(h, "unit-1", pythonEdit)

	// Act.
	h.send(activityOf("unit-1", &conversationv1.AgentBash{
		Result: &conversationv1.AgentBash_Progress{Progress: &conversationv1.AgentToolCallProgress{LastProgressAtMs: 2_000}},
	}))

	// Assert: the beat's card still carries the edits, read once.
	assertStatedEditOfF(t, h.card().GetEdits())
	if n := len(h.recordsAt("debug", "daemon.feed.bash_edits_stated")); n != 1 {
		t.Fatalf("the command was read %d times, want once", n)
	}
}

func TestAFailedShellCallStatesNoEdits(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	startForegroundBash(h, "unit-1", pythonEdit)

	// Act.
	h.send(activityOf("unit-1", failedCall(9_000)))

	// Assert.
	if h.card().GetReturned().GetFailed() == nil {
		t.Fatalf("outcome = %T, want a failed return", h.card().GetOutcome())
	}
	if h.card().GetEdits() != nil {
		t.Fatalf("edits = %+v, want none on a call that broke", h.card().GetEdits())
	}
}

func TestADeniedShellCallStatesNoEdits(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	startForegroundBash(h, "unit-1", pythonEdit)
	h.ask("ask-1", "unit-1", &conversationv1.AgentPermissionStart{
		Prompt:    &conversationv1.AgentPermissionPrompt{Title: "Claude wants to run a command"},
		StartedAt: &conversationv1.AgentActivityStartedAt{AtMs: 1_000},
	})

	// Act.
	h.ask("ask-1", "unit-1", &conversationv1.AgentPermissionSuccess{
		Decision: &conversationv1.AgentPermissionSuccess_Denied{Denied: &conversationv1.AgentPermissionDenied{
			By: &conversationv1.AgentPermissionDenied_Policy{Policy: &conversationv1.AgentPermissionDeniedByPolicy{Message: "no"}},
		}},
	})

	// Assert.
	for _, row := range h.rows(rootFeed()) {
		if card := row.GetActivity().GetSimpleToolCall(); card != nil && card.GetEdits() != nil {
			t.Fatalf("the denied card still states %+v", card.GetEdits())
		}
	}
}

func TestAStartAfterTheReturnKeepsTheReturnedCardsEdits(t *testing.T) {
	// Arrange: the return lands first.
	h := newHarness(t)
	h.send(activityOf("unit-1", completedBash(pythonEdit)))

	// Act.
	startForegroundBash(h, "unit-1", pythonEdit)

	// Assert.
	if h.card().GetReturned() == nil {
		t.Fatalf("outcome = %T, want the returned card to stand", h.card().GetOutcome())
	}
	assertStatedEditOfF(t, h.card().GetEdits())
}

func TestAShellCommandPyeditDeclinesDrawsNoEditsAndSaysWhy(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	startForegroundBash(h, "unit-1", "python3 - <<'EOF'\nimport re\nEOF\npython3 other.py")

	// Assert.
	if h.card().GetEdits() != nil {
		t.Fatalf("edits = %+v, want none", h.card().GetEdits())
	}
	if !h.hasRecordWith("debug", "daemon.feed.bash_edits_declined", "unit", "unit-1") {
		t.Fatalf("records = %+v, want the decline recorded against the unit", h.records())
	}
	records := h.recordsAt("debug", "daemon.feed.bash_edits_declined")
	if reason, _ := records[0].Context["reason"].(string); !strings.Contains(reason, "bare here-document") {
		t.Fatalf("reason = %q, want pyedit's", reason)
	}
}

func TestAShellCommandWithoutPythonStatesNothingAndRecordsNothing(t *testing.T) {
	// Arrange, Act.
	h := newHarness(t)
	startForegroundBash(h, "unit-1", "go test ./...")

	// Assert.
	if h.card().GetEdits() != nil {
		t.Fatalf("edits = %+v, want none", h.card().GetEdits())
	}
	if h.hasRecord("debug", "daemon.feed.bash_edits_declined") || h.hasRecord("debug", "daemon.feed.bash_edits_stated") {
		t.Fatalf("records = %+v, want no edit record for a command without python", h.records())
	}
}

func TestReplacementHeader(t *testing.T) {
	tests := []struct {
		name string
		i, n int
		rep  pyedit.Replacement
		want string
	}{
		{name: "a lone replace-all", i: 0, n: 1, rep: pyedit.Replacement{}, want: "@@ every occurrence @@"},
		{name: "a lone first-only", i: 0, n: 1, rep: pyedit.Replacement{First: true}, want: "@@ first occurrence @@"},
		{name: "one of several", i: 1, n: 3, rep: pyedit.Replacement{First: true}, want: "@@ 2 of 3 · first occurrence @@"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := replacementHeader(tt.i, tt.n, tt.rep)

			// Assert.
			if got != tt.want {
				t.Fatalf("replacementHeader = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestFileEditOpensEachReplacementWithAHeader(t *testing.T) {
	// Act.
	got := fileEdit(pyedit.FileEdit{Path: "f", Replacements: []pyedit.Replacement{{Old: "a", New: "b"}, {Old: "c", New: "d"}}})

	// Assert.
	var lines []string
	for _, line := range got.GetLines() {
		lines = append(lines, armName(line)+":"+line.GetText())
	}
	want := "header:@@ 1 of 2 · every occurrence @@|removed:a|added:b|header:@@ 2 of 2 · every occurrence @@|removed:c|added:d"
	if strings.Join(lines, "|") != want {
		t.Fatalf("lines = %q, want %q", strings.Join(lines, "|"), want)
	}
}

func TestLineOfKindPanicsOnAnUnknownKind(t *testing.T) {
	// Assert.
	defer func() {
		if recover() == nil {
			t.Fatal("lineOfKind accepted an unknown kind")
		}
	}()

	// Act.
	lineOfKind(linediff.Kind(99), "x")
}
