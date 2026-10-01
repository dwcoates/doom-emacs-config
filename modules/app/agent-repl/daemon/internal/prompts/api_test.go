package prompts

import (
	"os"
	"path/filepath"
	"testing"
)

// repoPromptsDir is the checked-in prompts directory, which is the contract
// every brief in the repository is asserted against.
const repoPromptsDir = "../../../prompts"

// write puts one brief in a temp dir and answers the dir.
func write(t *testing.T, name, content string) string {
	t.Helper()
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, name+Suffix), []byte(content), 0o644); err != nil {
		t.Fatalf("writing: %v", err)
	}
	return dir
}

func TestLoadReadsHeaderAndBody(t *testing.T) {
	// Arrange.
	dir := write(t, "b", "<!-- used by: x.go (F); placeholders: {{a}} -->\nhello {{a}}\n")

	// Act.
	got, err := Load(dir, "b")

	// Assert.
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	if got.Name != "b" {
		t.Fatalf("Name = %q, want b", got.Name)
	}
	if got.Header[HeaderUsedBy] != "x.go (F)" {
		t.Fatalf("used by = %q, want x.go (F)", got.Header[HeaderUsedBy])
	}
	if got.Body != "hello {{a}}" {
		t.Fatalf("Body = %q, want %q", got.Body, "hello {{a}}")
	}
}

func TestLoadDropsOnlyTheFinalNewline(t *testing.T) {
	// Arrange: a brief that must end with a blank line carries two newlines.
	dir := write(t, "b", "<!-- used by: x; placeholders: none -->\nbody\n\n")

	// Act.
	got, err := Load(dir, "b")

	// Assert.
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	if got.Body != "body\n" {
		t.Fatalf("Body = %q, want %q", got.Body, "body\n")
	}
}

func TestLoadKeepsLeadingBlankLinesInTheBody(t *testing.T) {
	// Arrange.
	dir := write(t, "b", "<!-- used by: x; placeholders: none -->\n\nbody\n")

	// Act.
	got, err := Load(dir, "b")

	// Assert.
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	if got.Body != "\nbody" {
		t.Fatalf("Body = %q, want %q", got.Body, "\nbody")
	}
}

func TestLoadAnswersPlaceholdersInFirstUseOrder(t *testing.T) {
	// Arrange.
	dir := write(t, "b", "<!-- used by: x; placeholders: {{a}}, {{b}} -->\n{{b}} then {{a}} then {{b}}\n")

	// Act.
	got, err := Load(dir, "b")

	// Assert.
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	if len(got.Placeholders) != 2 || got.Placeholders[0] != "b" || got.Placeholders[1] != "a" {
		t.Fatalf("Placeholders = %v, want [b a]", got.Placeholders)
	}
}

func TestLoadAcceptsABriefDeclaringNoPlaceholders(t *testing.T) {
	// Arrange.
	dir := write(t, "b", "<!-- used by: x; placeholders: none -->\nbody\n")

	// Act.
	got, err := Load(dir, "b")

	// Assert.
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	if len(got.Placeholders) != 0 {
		t.Fatalf("Placeholders = %v, want none", got.Placeholders)
	}
}

func TestLoadFailsLoudly(t *testing.T) {
	tests := []struct {
		name    string
		content string
	}{
		{name: "empty file", content: ""},
		{name: "whitespace only", content: "   \n\n"},
		{name: "header with no body", content: "<!-- used by: x; placeholders: none -->"},
		{name: "first line is not the header", content: "body\n<!-- used by: x; placeholders: none -->\n"},
		{name: "header is not a comment", content: "used by: x; placeholders: none\nbody\n"},
		{name: "header field is not key value", content: "<!-- used by: x; placeholders -->\nbody\n"},
		{name: "header declares no call site", content: "<!-- used by: ; placeholders: none -->\nbody\n"},
		{name: "header declares no placeholder list", content: "<!-- used by: x -->\nbody\n"},
		{name: "empty placeholder list", content: "<!-- used by: x; placeholders: -->\nbody\n"},
		{name: "duplicate header key", content: "<!-- used by: x; used by: y; placeholders: none -->\nbody\n"},
		{name: "undeclared placeholder in the body", content: "<!-- used by: x; placeholders: none -->\n{{a}}\n"},
		{name: "declared placeholder missing from the body", content: "<!-- used by: x; placeholders: {{a}} -->\nbody\n"},
		{name: "misspelled placeholder token in the body", content: "<!-- used by: x; placeholders: {{a}} -->\n{{a}} {{Bad Name}}\n"},
		{name: "placeholder list names nothing", content: "<!-- used by: x; placeholders: several -->\nbody\n"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			dir := write(t, "b", tc.content)

			// Act.
			_, err := Load(dir, "b")

			// Assert.
			if err == nil {
				t.Fatal("Load accepted a malformed brief")
			}
		})
	}
}

func TestLoadFailsOnAMissingFile(t *testing.T) {
	// Act.
	_, err := Load(t.TempDir(), "absent")

	// Assert.
	if err == nil {
		t.Fatal("Load accepted a missing brief")
	}
}

func TestLoadFailsOnAnEmptyName(t *testing.T) {
	// Act.
	_, err := Load(t.TempDir(), "")

	// Assert.
	if err == nil {
		t.Fatal("Load accepted an empty brief name")
	}
}

func TestSpliceFillsEveryPlaceholder(t *testing.T) {
	// Arrange.
	dir := write(t, "b", "<!-- used by: x; placeholders: {{a}}, {{b}} -->\n{{a}} and {{b}} and {{a}}\n")
	p, err := Load(dir, "b")
	if err != nil {
		t.Fatalf("Load: %v", err)
	}

	// Act.
	got, err := p.Splice(map[string]string{"a": "1", "b": "2"})

	// Assert.
	if err != nil {
		t.Fatalf("Splice: %v", err)
	}
	if got != "1 and 2 and 1" {
		t.Fatalf("Splice = %q, want %q", got, "1 and 2 and 1")
	}
}

func TestSpliceRefusesAMissingValue(t *testing.T) {
	// Arrange.
	dir := write(t, "b", "<!-- used by: x; placeholders: {{a}}, {{b}} -->\n{{a}} {{b}}\n")
	p, err := Load(dir, "b")
	if err != nil {
		t.Fatalf("Load: %v", err)
	}

	// Act.
	_, err = p.Splice(map[string]string{"a": "1"})

	// Assert.
	if err == nil {
		t.Fatal("Splice accepted values that leave a placeholder unfilled")
	}
}

func TestSpliceRefusesAnUnknownPlaceholder(t *testing.T) {
	// Arrange.
	dir := write(t, "b", "<!-- used by: x; placeholders: {{a}} -->\n{{a}}\n")
	p, err := Load(dir, "b")
	if err != nil {
		t.Fatalf("Load: %v", err)
	}

	// Act.
	_, err = p.Splice(map[string]string{"a": "1", "invented": "2"})

	// Assert.
	if err == nil {
		t.Fatal("Splice accepted a value for a placeholder the brief does not use")
	}
}

func TestSpliceAcceptsAnEmptyValue(t *testing.T) {
	// Arrange: an empty string is a value, not an absence.
	dir := write(t, "b", "<!-- used by: x; placeholders: {{a}} -->\n[{{a}}]\n")
	p, err := Load(dir, "b")
	if err != nil {
		t.Fatalf("Load: %v", err)
	}

	// Act.
	got, err := p.Splice(map[string]string{"a": ""})

	// Assert.
	if err != nil {
		t.Fatalf("Splice: %v", err)
	}
	if got != "[]" {
		t.Fatalf("Splice = %q, want %q", got, "[]")
	}
}

func TestSpliceRefusesAValueThatReintroducesAPlaceholder(t *testing.T) {
	// Arrange.
	dir := write(t, "b", "<!-- used by: x; placeholders: {{a}} -->\n{{a}}\n")
	p, err := Load(dir, "b")
	if err != nil {
		t.Fatalf("Load: %v", err)
	}

	// Act.
	_, err = p.Splice(map[string]string{"a": "{{b}}"})

	// Assert.
	if err == nil {
		t.Fatal("Splice shipped a brief with a hole in it")
	}
}

func TestSpliceOnABriefWithNoPlaceholdersTakesNoValues(t *testing.T) {
	// Arrange.
	dir := write(t, "b", "<!-- used by: x; placeholders: none -->\nbody\n")
	p, err := Load(dir, "b")
	if err != nil {
		t.Fatalf("Load: %v", err)
	}

	// Act.
	got, err := p.Splice(nil)

	// Assert.
	if err != nil {
		t.Fatalf("Splice: %v", err)
	}
	if got != "body" {
		t.Fatalf("Splice = %q, want body", got)
	}
}

func TestEveryCheckedInBriefLoads(t *testing.T) {
	// Arrange.
	entries, err := os.ReadDir(repoPromptsDir)
	if err != nil {
		t.Fatalf("reading the prompts directory: %v", err)
	}

	for _, entry := range entries {
		name := entry.Name()
		if entry.IsDir() || filepath.Ext(name) != Suffix || name == "README.md" {
			continue
		}
		base := name[:len(name)-len(Suffix)]
		t.Run(base, func(t *testing.T) {
			// Act.
			_, err := Load(repoPromptsDir, base)

			// Assert.
			if err != nil {
				t.Fatalf("Load: %v", err)
			}
		})
	}
}

func TestTheRebaseConflictBriefKeepsItsPlaceholderSet(t *testing.T) {
	// Arrange.
	want := []string{"conflict_commit", "source_branch", "worktree_dir", "target_branch", "conflicted_files"}

	// Act.
	got, err := Load(repoPromptsDir, "merge-conflict-resolve")

	// Assert.
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	if err := sameSet("placeholders", want, got.Placeholders); err != nil {
		t.Fatalf("merge-conflict-resolve: %v", err)
	}
}

func TestTheFixingAttemptBriefKeepsItsPlaceholderSet(t *testing.T) {
	// Arrange.
	want := []string{"source_branch", "worktree_dir", "target_branch", "failing_suites", "archive_path", "failure_tail", "attempt", "max_attempts", "escalation_file", "escalation_marker"}

	// Act.
	got, err := Load(repoPromptsDir, "merge-test-failure-resolve")

	// Assert.
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	if err := sameSet("placeholders", want, got.Placeholders); err != nil {
		t.Fatalf("merge-test-failure-resolve: %v", err)
	}
}
