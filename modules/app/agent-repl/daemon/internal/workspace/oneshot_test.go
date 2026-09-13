package workspace

import (
	"strings"
	"testing"

	"claude-repld/internal/prompts"
)

// testPolicy is the policy source the decoration unit tests read from. The
// fixture's loader ignores the directory, so the source's identity is all that
// matters here: the briefs come from the fixture's own table.
var testPolicy = prompts.Source{Dir: "/prompts", Kind: prompts.SourceCorpus, RepositoryRoot: "/repo"}

func TestDecorateOneShotBracketsWhatTheUserDidNotType(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	oneShotBriefs(f)
	v := f.verbs.(*verbs)

	// Act.
	got, err := v.decorateOneShot("USER TEXT", testPolicy)

	// Assert: the user's own words are the only unbracketed span.
	if err != nil {
		t.Fatalf("decorateOneShot: %v", err)
	}
	if !strings.Contains(got, prompts.MetaOpen+"PREAMBLE\n"+prompts.MetaClose+"USER TEXT"+prompts.MetaOpen) {
		t.Fatalf("decorated prompt = %q, want the injected spans meta-wrapped around the user's words", got)
	}
}

func TestDecorateOneShotComposesThePreambleTheCommissionAndTheDirective(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	oneShotBriefs(f)
	v := f.verbs.(*verbs)

	// Act.
	got, err := v.decorateOneShot("USER TEXT", testPolicy)

	// Assert: the whole composition, verbatim and in order.
	if err != nil {
		t.Fatalf("decorateOneShot: %v", err)
	}
	want := prompts.Wrap("PREAMBLE\n") + "USER TEXT" +
		prompts.Wrap("\n"+completionDirectiveLead+"DIRECTIVE")
	if got != want {
		t.Fatalf("decorated prompt = %q, want %q", got, want)
	}
}

func TestTheCompletionDirectiveLeadIsTheRuledSentence(t *testing.T) {
	// Arrange: the owner ruled the framing sentence's exact words, and the
	// directive is what follows it.
	const want = "when you're all done, please do the following postprocessing directive: "

	// Act. Assert.
	if completionDirectiveLead != want {
		t.Fatalf("completionDirectiveLead = %q, want %q", completionDirectiveLead, want)
	}
}

func TestDecorateOneShotRefusesAMissingDirective(t *testing.T) {
	// Arrange: a policy that states a preamble and no completion directive.
	f := newFixture(t)
	oneShotBriefs(f)
	delete(f.briefs, BriefOneShotCompletionDirective)
	v := f.verbs.(*verbs)

	// Act.
	_, err := v.decorateOneShot("task", testPolicy)

	// Assert.
	if err == nil {
		t.Fatal("decorateOneShot(no directive) = nil error, want a refusal")
	}
}

func TestDecorateOneShotRefusesADirectiveDeclaringAPlaceholder(t *testing.T) {
	// Arrange: the directive is plain English, and the daemon has nothing to
	// fill a placeholder in a repository's own completion statement from.
	f := newFixture(t)
	oneShotBriefs(f)
	f.briefs[BriefOneShotCompletionDirective] = prompts.Prompt{
		Body: "merge into {{target}}", Placeholders: []string{"target"},
	}
	v := f.verbs.(*verbs)

	// Act.
	_, err := v.decorateOneShot("task", testPolicy)

	// Assert.
	if err == nil {
		t.Fatal("decorateOneShot(placeholder directive) = nil error, want a refusal")
	}
}

func TestDecorateOneShotRefusesAMisspelledPreamblePlaceholder(t *testing.T) {
	// Arrange: an edited brief whose placeholder no longer matches the call
	// site fails the operation rather than shipping a hole.
	f := newFixture(t)
	oneShotBriefs(f)
	f.briefs[BriefAutonomousPreamble] = prompts.Prompt{
		Body: "here is the task {{taks}}", Placeholders: []string{"taks"},
	}
	v := f.verbs.(*verbs)

	// Act.
	_, err := v.decorateOneShot("task", testPolicy)

	// Assert.
	if err == nil {
		t.Fatal("decorateOneShot(misspelled placeholder) = nil error, want a refusal")
	}
}

func TestOneShotPolicyBriefsAreTheSameTwoForEveryOneShot(t *testing.T) {
	// Arrange: there is no finish to vary the required set by.
	want := []string{BriefAutonomousPreamble, BriefOneShotCompletionDirective}

	// Act.
	got := oneShotPolicyBriefs()

	// Assert.
	if strings.Join(got, ",") != strings.Join(want, ",") {
		t.Fatalf("oneShotPolicyBriefs() = %v, want %v", got, want)
	}
}

// TestOneShotMetaSentinelsAreTheCrossSystemLiterals pins the exact bytes the
// one-shot decoration emits. The webapp and the elisp feed strip these same
// markers, so a drifted spelling on this side would leave raw sentinels drawn
// in the bubble. The literals are asserted here, not read from a constant.
func TestOneShotMetaSentinelsAreTheCrossSystemLiterals(t *testing.T) {
	// Arrange.
	const want = "<!--agent-repl:meta-->span<!--/agent-repl:meta-->"

	// Act.
	got := prompts.Wrap("span")

	// Assert.
	if got != want {
		t.Fatalf("prompts.Wrap = %q, want %q", got, want)
	}
}
