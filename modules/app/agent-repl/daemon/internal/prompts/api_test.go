package prompts

import (
	"os"
	"path/filepath"
	"strings"
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
	want := []string{"source_branch", "worktree_dir", "target_branch", "failing_suites", "archive_path", "failure_tail", "attempt", "max_attempts", "escalation_file", "escalation_marker", "failing_tests_file", "failing_tests"}

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

// unsaid answers the first of want that body does not say, and whether one
// is missing: every test pinning a brief's wording asserts through it.
func unsaid(body string, want []string) (string, bool) {
	for _, phrase := range want {
		if !strings.Contains(body, phrase) {
			return phrase, true
		}
	}
	return "", false
}

func TestUnsaid(t *testing.T) {
	tests := []struct {
		name        string
		body        string
		want        []string
		wantPhrase  string
		wantMissing bool
	}{
		{name: "every phrase said", body: "alpha beta gamma", want: []string{"alpha", "gamma"}},
		{name: "nothing wanted", body: "alpha", want: nil},
		{name: "one phrase missing", body: "alpha beta", want: []string{"alpha", "delta"}, wantPhrase: "delta", wantMissing: true},
		{name: "the first missing phrase is named", body: "alpha", want: []string{"beta", "gamma"}, wantPhrase: "beta", wantMissing: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			phrase, missing := unsaid(tt.body, tt.want)

			// Assert.
			if phrase != tt.wantPhrase || missing != tt.wantMissing {
				t.Fatalf("unsaid = (%q, %v), want (%q, %v)", phrase, missing, tt.wantPhrase, tt.wantMissing)
			}
		})
	}
}

func TestEveryBriefWordingPinAssertsThroughUnsaid(t *testing.T) {
	// Arrange: a pin that hand-rolls its own phrase check drifts from the
	// rest, so none may.
	src, err := os.ReadFile("api_test.go")
	if err != nil {
		t.Fatalf("reading api_test.go: %v", err)
	}
	handRolled := "!strings.Contains(got" + ".Body"

	// Act.
	n := strings.Count(string(src), handRolled)

	// Assert.
	if n != 0 {
		t.Fatalf("api_test.go hand-rolls %d phrase checks on a brief's body; assert through unsaid", n)
	}
}

func TestTheFixingAttemptBriefPrescribesRunningOnlyTheFailingTestsUntilTheyPass(t *testing.T) {
	// Arrange.
	want := []string{
		"{{failing_tests}}",
		"RUN ONLY THE FAILING TEST CASES, AND KEEP FIXING AND RERUNNING THEM UNTIL THEY PASS, BEFORE YOU END YOUR TURN",
		"the `agent-repld test run` command that runs only its failing cases through the daemon",
		"never the whole suite, and never a test command by hand",
		"Fix what fails, then rerun those same targeted cases. Repeat until every one of them passes.",
		"Only then is the remediation resolved: commit and end your turn",
		"The queue then runs the selected suites in full again",
	}

	// Act.
	got, err := Load(repoPromptsDir, "merge-test-failure-resolve")

	// Assert.
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	if phrase, missing := unsaid(got.Body, want); missing {
		t.Fatalf("merge-test-failure-resolve does not say %q", phrase)
	}
}

func TestTheFixingAttemptBriefSaysTheSelectedSuitesRunAgain(t *testing.T) {
	// Act.
	got, err := Load(repoPromptsDir, "merge-test-failure-resolve")

	// Assert.
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	if phrase, missing := unsaid(got.Body, []string{"runs the selected suites again"}); missing {
		t.Fatalf("merge-test-failure-resolve does not say %q", phrase)
	}
	if strings.Contains(got.Body, "runs the whole suite again") {
		t.Fatalf("merge-test-failure-resolve still says the whole suite runs again: %q", got.Body)
	}
}

func TestTheFixingAttemptBriefRunsTheTestsTheFailingTestsFileLists(t *testing.T) {
	// Act.
	got, err := Load(repoPromptsDir, "merge-test-failure-resolve")

	// Assert.
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	if phrase, missing := unsaid(got.Body, []string{"Run exactly the tests listed in {{failing_tests_file}}"}); missing {
		t.Fatalf("merge-test-failure-resolve does not point the agent at the failing tests file: it does not say %q", phrase)
	}
}

func TestTheFixingAttemptBriefHandsAFailingTestThatCannotRunAloneToAnInvestigatingWorkspace(t *testing.T) {
	// Arrange.
	want := []string{
		"IF A FAILING TEST CANNOT BE RUN ON ITS OWN",
		"do not weaken the test or the code",
		"without compromising coverage or production UI/UX",
		"`/agent-repl-workspaces`",
		"`agent-repld workspace list`",
		"send it a prompt (`agent-repld workspace prompt`)",
		"create a new NON-one-shot workspace",
		"`agent-repld workspace create`",
		"Then carry on with this fix",
	}

	// Act.
	got, err := Load(repoPromptsDir, "merge-test-failure-resolve")

	// Assert.
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	if phrase, missing := unsaid(got.Body, want); missing {
		t.Fatalf("merge-test-failure-resolve does not say %q", phrase)
	}
}

func TestPathNamesTheBriefsFile(t *testing.T) {
	// Act.
	got := Path("/p", "b")

	// Assert.
	if got != filepath.Join("/p", "b"+Suffix) {
		t.Fatalf("Path = %q, want /p/b%s", got, Suffix)
	}
}

func TestParseReadsContentNotOnDisk(t *testing.T) {
	// Arrange.
	content := "<!-- used by: x; placeholders: {{a}} -->\nhello {{a}}\n"

	// Act.
	got, err := Parse("b", "a rewrite", content)

	// Assert.
	if err != nil {
		t.Fatalf("Parse: %v", err)
	}
	if got.Body != "hello {{a}}" || len(got.Placeholders) != 1 {
		t.Fatalf("Parse = %+v, want the body and its one placeholder", got)
	}
}

func TestParseNamesWhereInItsErrors(t *testing.T) {
	// Arrange: the body drops a declared placeholder.
	content := "<!-- used by: x; placeholders: {{a}} -->\nhello\n"

	// Act.
	_, err := Parse("b", "the rewrite", content)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "the rewrite") {
		t.Fatalf("Parse error = %v, want one naming the rewrite", err)
	}
}

func TestLoadSharesParsesVerdict(t *testing.T) {
	// Arrange: Load is Parse over the file's bytes, so the two agree on any
	// content, malformed included.
	content := "<!-- used by: x; placeholders: {{a}} -->\nhello {{b}}\n"
	dir := write(t, "b", content)

	// Act.
	_, loadErr := Load(dir, "b")
	_, parseErr := Parse("b", Path(dir, "b"), content)

	// Assert.
	if loadErr == nil || parseErr == nil || loadErr.Error() != parseErr.Error() {
		t.Fatalf("Load error %v and Parse error %v, want the same refusal", loadErr, parseErr)
	}
}

func TestTheRoutingBriefRoutesAMessageAfterTheToolCall(t *testing.T) {
	// Each case is a kind of new message the routing brief passes through the
	// gate after the tool call and never holds: joining the running turn, or,
	// against a queued prompt, coalescing into it (promptqueue.settleQueued).
	tests := []struct {
		name string
		want []string
	}{
		{
			// A prompt opening "also, ..." appends to the request ahead of it.
			name: "an addendum opener",
			want: []string{
				"A new message that ADDS TO the request the running turn is working on always answers {{token_after_tool_call}}",
				`It may OPEN as an addendum, starting with "also", "also,"`,
				"so it never waits for the turn to finish ({{token_hold}})",
			},
		},
		{
			// A prompt carrying its addition mid-sentence ("we should also do
			// X") appends to the request ahead of it just as an opener does.
			name: "an addendum inside the sentence",
			want: []string{
				"wherever in the message the addition shows and however it is phrased",
				`"we should also do X", "and when doing X we should also do Y"`,
				"ANYTHING that suggests the user is appending to, extending, or widening the request they already sent is an addendum",
			},
		},
		{
			// A prompt clarifying one the user already sent refines the request
			// the running turn works on.
			name: "a clarification",
			want: []string{
				"A new message that CLARIFIES a previous message always answers {{token_after_tool_call}}",
				`"I meant the other file"`,
				"Such a message refines the request the running turn is working on, not a new request, so it never waits for the turn to finish ({{token_hold}})",
			},
		},
		{
			// A prompt marking itself urgent ("asap") is an important addendum
			// to the last one sent, and never interrupts on the urgency alone.
			name: "an urgent message",
			want: []string{
				"A new message that marks itself URGENT always answers {{token_after_tool_call}}",
				`"asap"`,
				"so it never waits for the turn to finish ({{token_hold}})",
				"The urgency alone does not make the running work wrong",
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got, err := Load(repoPromptsDir, "queue-routing-classifier")

			// Assert.
			if err != nil {
				t.Fatalf("Load: %v", err)
			}
			if phrase, missing := unsaid(got.Body, tt.want); missing {
				t.Fatalf("queue-routing-classifier does not say %q", phrase)
			}
		})
	}
}

func TestTheRoutingBriefHoldsASeparateFollowUpTask(t *testing.T) {
	// Arrange: a prompt lining up a self-contained task for afterwards
	// ("after, let's make sure ...") changes nothing about the running work,
	// so the routing brief holds it, and it is no turn-ending instruction.
	want := []string{
		"A new message that only lines up a SEPARATE FOLLOW-UP TASK for afterwards is NOT an addendum",
		`"after, let's make sure ..."`,
		"Such a message answers {{token_hold}}",
		"such a task is not a step of the running turn's ending, so it holds under the follow-up rule above",
	}

	// Act.
	got, err := Load(repoPromptsDir, "queue-routing-classifier")

	// Assert.
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	if phrase, missing := unsaid(got.Body, want); missing {
		t.Fatalf("queue-routing-classifier does not say %q", phrase)
	}
}

func TestTheRoutingBriefPassesAMessageThroughOnAnyDoubt(t *testing.T) {
	// Arrange: passing a held prompt through after the tool call is the
	// default, so the routing brief holds one only when it is certain the
	// prompt is independent, and answers after the tool call on any doubt.
	want := []string{
		"Answer {{token_hold}} only when you are CERTAIN the new message is genuinely independent",
		"PASSING THE MESSAGE THROUGH IS THE DEFAULT",
		"If there is ANY doubt about the route, answer {{token_after_tool_call}}, never {{token_hold}}",
		"Such a message answers {{token_hold}} only when it is clearly such a task",
	}

	// Act.
	got, err := Load(repoPromptsDir, "queue-routing-classifier")

	// Assert.
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	if phrase, missing := unsaid(got.Body, want); missing {
		t.Fatalf("queue-routing-classifier does not say %q", phrase)
	}
}
