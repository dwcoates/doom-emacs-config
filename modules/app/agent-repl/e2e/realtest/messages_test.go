//go:build realtest

package realtest

import "testing"

func TestMessagesKeepsTheModulesOwnWarningRung(t *testing.T) {
	// Arrange: `agent-repl--warn` prepends the tag itself, so a call site
	// never spells it and a match is unambiguous.
	text := "WARNING: elisp.panels.host-subscribe: skipped ws=one reason=no-link\n"

	// Act.
	findings := HarvestMessages(text, 0, nil)

	// Assert.
	if len(findings) != 1 {
		t.Fatalf("harvested %d finding(s) from a module warning, want 1: %+v", len(findings), findings)
	}
	if findings[0].Kind != KindMessagesLine {
		t.Errorf("finding kind is %s, want %s", findings[0].Kind, KindMessagesLine)
	}
}

func TestMessagesKeepsAnEscapedProcessFilterError(t *testing.T) {
	// Arrange: an error escaping a process filter abandons the operation, and
	// this line is the only place that ever says so.
	text := "error in process filter: Wrong type argument: stringp, nil\n"

	// Act.
	findings := HarvestMessages(text, 0, nil)

	// Assert.
	if len(findings) != 1 {
		t.Fatalf("harvested %d finding(s), want 1: %+v", len(findings), findings)
	}
}

func TestMessagesDropsAnOrdinaryLine(t *testing.T) {
	// Arrange.
	text := "Loading /Users/someone/.config/doom/config.el...done\nMark set\n"

	// Act.
	findings := HarvestMessages(text, 0, nil)

	// Assert.
	if len(findings) != 0 {
		t.Fatalf("harvested %d finding(s) from ordinary lines: %+v", len(findings), findings)
	}
}

func TestMessagesReadsOnlyTheTailWhenAnOffsetIsGiven(t *testing.T) {
	// Arrange: a realtest against an already-standing Emacs records
	// `(buffer-size)` first and is answerable only for what came after.
	head := "WARNING: this was already there before the run\n"
	tailText := "ERROR: this one is the run's\n"

	// Act.
	findings := HarvestMessages(head+tailText, len(head), nil)

	// Assert.
	if len(findings) != 1 {
		t.Fatalf("harvested %d finding(s) past the offset, want 1: %+v", len(findings), findings)
	}
	if findings[0].Raw != "ERROR: this one is the run's" {
		t.Errorf("harvested %q, want the line past the offset", findings[0].Raw)
	}
}

func TestMessagesAttributesByTheWorkspaceNameInTheLine(t *testing.T) {
	// Arrange: *Messages* lines carry a workspace NAME, not an id, so it is
	// resolved through the state database's names.
	workspaces := []Workspace{{ID: "aaaa1111", Dir: "/tmp/one", Name: "one"}}
	text := "WARNING: kill-workspace-buffers: error on ws=one\n"

	// Act.
	findings := HarvestMessages(text, 0, workspaces)

	// Assert.
	if findings[0].Workspace != "aaaa1111" {
		t.Errorf("the finding is attributed to %q, want the id behind the name", findings[0].Workspace)
	}
}

func TestMessagesAttributesALineNamingNothingToGlobal(t *testing.T) {
	// Arrange.
	text := "ERROR: elisp.daemon.binary-missing binary=\"claude-repld\"\n"

	// Act.
	findings := HarvestMessages(text, 0, nil)

	// Assert.
	if findings[0].Workspace != GlobalWorkspace {
		t.Errorf("the finding is attributed to %q, want %q", findings[0].Workspace, GlobalWorkspace)
	}
}

func TestMessagesCarriesAnUnknownWorkspaceNameThrough(t *testing.T) {
	// Arrange: a name the state database does not hold names something the
	// run saw and the database does not, which is itself worth reading.
	text := "WARNING: something happened ws=ghost\n"

	// Act.
	findings := HarvestMessages(text, 0, nil)

	// Assert.
	if findings[0].Workspace != "unknown-workspace:ghost" {
		t.Errorf("the finding is attributed to %q, want the name carried through", findings[0].Workspace)
	}
}
