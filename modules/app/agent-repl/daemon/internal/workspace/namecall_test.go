package workspace

import (
	"context"
	"errors"
	"fmt"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/headless"
	"agent-repld/internal/netstate"
	"agent-repld/internal/prompts"
	"agent-repld/internal/sourcescan"
	"agent-repld/internal/wsm"
)

// script points the fixture's naming call at a sequence of answers, one per
// attempt.
func script(f *fixture, answers ...headlessAnswer) {
	f.headless.answers = answers
}

// TestTheShippedNamingBriefTakesTheConversation pins the brief the daemon
// ships against the values mintName splices: the prompt and the conversation a
// fork continues — no more, no fewer.
func TestTheShippedNamingBriefTakesTheConversation(t *testing.T) {
	// Arrange.
	brief, err := prompts.Load(filepath.Join("..", "..", "..", "prompts"), BriefWorkspaceName)
	if err != nil {
		t.Fatalf("Load: %v", err)
	}

	// Act.
	got, err := brief.Splice(map[string]string{
		"prompt": "", "conversation": "The user's requests:\n- wire iterm2\n",
	})

	// Assert.
	if err != nil || !strings.Contains(got, "wire iterm2") {
		t.Fatalf("Splice = %q, %v; want the conversation spliced in", got, err)
	}
}

func TestCreateUsesTheModelsName(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	t.Setenv(PrefixEnv, "")
	t.Setenv(LegacyPrefixEnv, "")
	script(f, headlessAnswer{text: "flaky-login-test"})

	// Act.
	if _, err := f.verbs.Create(context.Background(), standardSpec(t, f)); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if f.git.created[0].Branch != "flaky-login-test" {
		t.Fatalf("branch = %q, want the model's answer", f.git.created[0].Branch)
	}
}

func TestCreateTrimsTheModelsAnswer(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	t.Setenv(PrefixEnv, "")
	t.Setenv(LegacyPrefixEnv, "")
	script(f, headlessAnswer{text: "  flaky-login-test\n"})

	// Act.
	if _, err := f.verbs.Create(context.Background(), standardSpec(t, f)); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if f.git.created[0].Branch != "flaky-login-test" {
		t.Fatalf("branch = %q, want the trimmed answer", f.git.created[0].Branch)
	}
}

func TestCreatePrefixesTheModelsName(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	t.Setenv(PrefixEnv, "ABC")
	script(f, headlessAnswer{text: "flaky-login-test"})

	// Act.
	if _, err := f.verbs.Create(context.Background(), standardSpec(t, f)); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert: the model answers a BARE slug and the daemon prefixes it, so the
	// model has no way to get the prefix wrong.
	if f.git.created[0].Branch != "ABC/flaky-login-test" {
		t.Fatalf("branch = %q, want the daemon's prefix on the model's bare slug", f.git.created[0].Branch)
	}
}

func TestCreateAsksTheNamingCallForHaiku(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	if _, err := f.verbs.Create(context.Background(), standardSpec(t, f)); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if len(f.headless.calls) != 1 || f.headless.calls[0].Model != headless.ModelHaiku {
		t.Fatalf("calls = %+v, want one call asking %s", f.headless.calls, headless.ModelHaiku)
	}
}

// TestCreateBoundsTheNamingCallByTheShortAnswerTimeout pins the naming call
// to the bound every short Haiku answer shares, so a hand-set bound of its own
// fails here rather than drifting from the abbreviation call's.
func TestCreateBoundsTheNamingCallByTheShortAnswerTimeout(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	if _, err := f.verbs.Create(context.Background(), standardSpec(t, f)); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if got := f.headless.calls[0].Timeout; got != headless.ShortAnswerTimeout {
		t.Fatalf("timeout = %v, want headless.ShortAnswerTimeout (%v)", got, headless.ShortAnswerTimeout)
	}
}

func TestCreateAsksTheNamingCallUnderItsOwnGuardSite(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	if _, err := f.verbs.Create(context.Background(), standardSpec(t, f)); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if f.headless.calls[0].Site != NamingSite {
		t.Fatalf("site = %q, want %q", f.headless.calls[0].Site, NamingSite)
	}
}

// TestCreateNamesFromTheRawPrompt pins that the name is minted from the user's
// own commission, never from a decorated or directive-appended form: the
// decoration is composed later, at submission.
func TestCreateNamesFromTheRawPrompt(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	spec := standardSpec(t, f)
	spec.InitialPrompt = "fix the login bug"

	// Act.
	if _, err := f.verbs.Create(context.Background(), spec); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if !strings.Contains(f.headless.calls[0].Prompt, "fix the login bug") {
		t.Fatalf("naming prompt = %q, want the raw commission in it", f.headless.calls[0].Prompt)
	}
}

func TestCreateBillsTheWorkspacesOwnAccount(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	if _, err := f.verbs.Create(context.Background(), standardSpec(t, f)); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if f.headless.calls[0].ConfigDir != "/config" {
		t.Fatalf("config dir = %q, want the repository's own account root", f.headless.calls[0].ConfigDir)
	}
}

// TestCreateRetriesExactlyOnceOnAnInvalidAnswer pins the owner's retry ruling:
// the common failure — the model wrapped the name in a sentence — costs one
// more call and no more.
func TestCreateRetriesExactlyOnceOnAnInvalidAnswer(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	t.Setenv(PrefixEnv, "")
	t.Setenv(LegacyPrefixEnv, "")
	script(f,
		headlessAnswer{text: "The name is flaky-login-test."},
		headlessAnswer{text: "flaky-login-test"})

	// Act.
	if _, err := f.verbs.Create(context.Background(), standardSpec(t, f)); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if len(f.headless.calls) != 2 {
		t.Fatalf("calls = %d, want exactly two", len(f.headless.calls))
	}
	if f.git.created[0].Branch != "flaky-login-test" {
		t.Fatalf("branch = %q, want the retry's answer", f.git.created[0].Branch)
	}
}

// TestCreateHandsTheRejectedNameBackToBeShortened pins that the retry is the
// SHORTEN brief carrying the model's own rejected answer, not the naming
// question asked again.
func TestCreateHandsTheRejectedNameBackToBeShortened(t *testing.T) {
	tests := []struct {
		name    string
		want    string
		present bool
	}{
		{name: "the rejected answer is quoted back", want: "agent-repl-input-box-shorter", present: true},
		{name: "the retry is the shorten brief", want: "shorten the name:", present: true},
		{name: "the naming question is not asked again", want: "name the work:", present: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			script(f,
				headlessAnswer{text: "agent-repl-input-box-shorter"},
				headlessAnswer{text: "input-shorter"})

			// Act.
			if _, err := f.verbs.Create(context.Background(), standardSpec(t, f)); err != nil {
				t.Fatalf("Create: %v", err)
			}

			// Assert.
			if got := strings.Contains(f.headless.calls[1].Prompt, tt.want); got != tt.present {
				t.Fatalf("retry prompt = %q, want %q present=%v", f.headless.calls[1].Prompt, tt.want, tt.present)
			}
		})
	}
}

// TestShortenRetryStatesTheLimitAndTheCount pins that the shorten retry after
// a five-word answer tells the model how many words its answer had and the
// limit, read off the validator's own constant.
func TestShortenRetryStatesTheLimitAndTheCount(t *testing.T) {
	tests := []struct {
		name string
		want string
	}{
		{name: "the word limit", want: fmt.Sprintf("over the %d-word limit %d", SlugWordLimit, SlugWordLimit)},
		{name: "the previous answer's word count", want: "the answer is 5 words"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			script(f,
				headlessAnswer{text: "agent-repl-input-box-shorter"},
				headlessAnswer{text: "input-shorter"})

			// Act.
			if _, err := f.verbs.Create(context.Background(), standardSpec(t, f)); err != nil {
				t.Fatalf("Create: %v", err)
			}

			// Assert.
			if !strings.Contains(f.headless.calls[1].Prompt, tt.want) {
				t.Fatalf("retry prompt = %q, want %q stated", f.headless.calls[1].Prompt, tt.want)
			}
		})
	}
}

// TestCreateReasksTheNamingQuestionAfterACallThatDidNotAnswer pins that a
// failed call leaves nothing to shorten, so the retry is the naming question.
func TestCreateReasksTheNamingQuestionAfterACallThatDidNotAnswer(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	script(f,
		headlessAnswer{err: &headless.Error{Cause: headless.CauseTimeout, Detail: "signal: killed"}},
		headlessAnswer{text: "flaky-login-test"})

	// Act.
	if _, err := f.verbs.Create(context.Background(), standardSpec(t, f)); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if !strings.HasPrefix(f.headless.calls[1].Prompt, "name the work:") {
		t.Fatalf("retry prompt = %q, want the naming question asked again", f.headless.calls[1].Prompt)
	}
}

// TestCreateSettlesForAWellShapedNameOnceTheRetryIsSpent pins the owner's
// 2026-10-14 ruling: a well-shaped answer over the limits is taken rather
// than the create refused, and the later such answer wins.
func TestCreateSettlesForAWellShapedNameOnceTheRetryIsSpent(t *testing.T) {
	tests := []struct {
		name    string
		answers []headlessAnswer
		want    string
	}{
		{
			name: "the second answer is over the word limit",
			answers: []headlessAnswer{
				{text: "agent-repl-workspace-summary-haiku-names"},
				{text: "agent-repl-workspace-summary-haiku"},
			},
			want: "agent-repl-workspace-summary-haiku",
		},
		{
			name: "the second answer is over the length bound",
			answers: []headlessAnswer{
				{text: "Not A Slug"},
				{text: "supercalifragilisticexpialidocious-names"},
			},
			want: "supercalifragilisticexpialidocious-names",
		},
		{
			name: "the second answer is not a slug at all",
			answers: []headlessAnswer{
				{text: "agent-repl-workspace-summary-haiku"},
				{text: "Sure! Here it is"},
			},
			want: "agent-repl-workspace-summary-haiku",
		},
		{
			name: "the second call did not answer",
			answers: []headlessAnswer{
				{text: "agent-repl-workspace-summary-haiku"},
				{err: &headless.Error{Cause: headless.CauseTimeout, Detail: "signal: killed"}},
			},
			want: "agent-repl-workspace-summary-haiku",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			t.Setenv(PrefixEnv, "")
			t.Setenv(LegacyPrefixEnv, "")
			script(f, tt.answers...)

			// Act.
			if _, err := f.verbs.Create(context.Background(), standardSpec(t, f)); err != nil {
				t.Fatalf("Create: %v", err)
			}

			// Assert.
			if f.git.created[0].Branch != tt.want {
				t.Fatalf("branch = %q, want %q", f.git.created[0].Branch, tt.want)
			}
		})
	}
}

// TestCreateRecordsTheSettledName pins the canonical record of a name taken
// over the limits: it names what was taken and by how much it is over.
func TestCreateRecordsTheSettledName(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	script(f,
		headlessAnswer{text: "agent-repl-workspace-summary-haiku"},
		headlessAnswer{text: "agent-repl-workspace-summary-haiku"})

	// Act.
	if _, err := f.verbs.Create(context.Background(), standardSpec(t, f)); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	record, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{
		Operation: opNaming, Message: "took the last well-shaped naming answer over the naming limits",
	})
	if !ok {
		t.Fatalf("records = %+v, want the settled name recorded", f.log.logger.Records())
	}
	if record.Context["name"] != "agent-repl-workspace-summary-haiku" || record.Context["words"] != 5 {
		t.Fatalf("context = %+v, want the name and its five words", record.Context)
	}
}

// TestCreateRecordsEveryRejectedAnswer pins that each rejected answer reaches
// the persisted log, not only the last: the first is what diagnoses a brief.
func TestCreateRecordsEveryRejectedAnswer(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	script(f,
		headlessAnswer{text: "Sure! Here is a name:"},
		headlessAnswer{text: "Fix The Login Bug Today"})

	// Act.
	_, _ = f.verbs.Create(context.Background(), standardSpec(t, f))

	// Assert.
	var answers []any
	for _, record := range f.log.logger.Records() {
		if record.Operation == opNaming && record.Message == "the naming answer did not validate" && record.Level == "info" {
			answers = append(answers, record.Context["answer"])
		}
	}
	if len(answers) != 2 || answers[0] != "Sure! Here is a name:" || answers[1] != "Fix The Login Bug Today" {
		t.Fatalf("rejected answers recorded = %v, want both, at info", answers)
	}
}

// TestTheShippedShortenBriefStatesTheLimit pins the shipped shorten brief to
// the values namingQuestion splices and to the validator's limit.
func TestTheShippedShortenBriefStatesTheLimit(t *testing.T) {
	// Arrange.
	brief, err := prompts.Load(filepath.Join("..", "..", "..", "prompts"), BriefWorkspaceNameShorten)
	if err != nil {
		t.Fatalf("Load: %v", err)
	}

	// Act.
	got, err := brief.Splice(map[string]string{
		"answer": "agent-repl-input-box-shorter", "problem": "the answer is 5 words", "limit": fmt.Sprint(SlugWordLimit),
	})

	// Assert.
	want := fmt.Sprintf("AT MOST %d WORDS", SlugWordLimit)
	if err != nil || !strings.Contains(got, want) || !strings.Contains(got, "agent-repl-input-box-shorter") {
		t.Fatalf("Splice = %q, %v; want the answer and the limit %q", got, err, want)
	}
}

// TestCreateRefusesWhenTheShortenBriefIsAbsent pins that the retry's brief is
// a deployment fact like the naming brief: missing, it is its own arm.
func TestCreateRefusesWhenTheShortenBriefIsAbsent(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	delete(f.briefs, BriefWorkspaceNameShorten)
	script(f,
		headlessAnswer{text: "agent-repl-workspace-summary-haiku"},
		headlessAnswer{text: "summary-haiku"})

	// Act.
	_, err := f.verbs.Create(context.Background(), standardSpec(t, f))

	// Assert.
	refusal := asRefusal(t, err, ArmBriefMissing)
	if refusal.Fields["name"] != BriefWorkspaceNameShorten {
		t.Fatalf("name = %v, want %q", refusal.Fields["name"], BriefWorkspaceNameShorten)
	}
}

// TestTheShippedNamingBriefStatesTheWordLimit pins the brief's stated limit to
// the validator's constant, so the two can never disagree.
func TestTheShippedNamingBriefStatesTheWordLimit(t *testing.T) {
	// Arrange.
	brief, err := prompts.Load(filepath.Join("..", "..", "..", "prompts"), BriefWorkspaceName)
	if err != nil {
		t.Fatalf("Load: %v", err)
	}

	// Act.
	got, err := brief.Splice(map[string]string{"prompt": "p", "conversation": ""})

	// Assert.
	want := fmt.Sprintf("THE NAME IS AT MOST %d WORDS", SlugWordLimit)
	if err != nil || !strings.Contains(got, want) {
		t.Fatalf("Splice = %q, %v; want the limit %q stated", got, err, want)
	}
}

func TestCreateRefusesWhenBothAnswersAreInvalid(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	script(f,
		headlessAnswer{text: "Sure! Here is a name:"},
		headlessAnswer{text: "Fix The Login Bug Today"})

	// Act.
	_, err := f.verbs.Create(context.Background(), standardSpec(t, f))

	// Assert.
	refusal := asRefusal(t, err, ArmNamingFailed)
	if refusal.Fields["cause"] != NamingCauseInvalidAnswer {
		t.Fatalf("cause = %v, want %q", refusal.Fields["cause"], NamingCauseInvalidAnswer)
	}
}

func TestNamingRefusalCarriesTheAttemptCount(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	script(f,
		headlessAnswer{text: "Sure! Here is a name:"},
		headlessAnswer{text: "Fix The Login Bug Today"})

	// Act.
	_, err := f.verbs.Create(context.Background(), standardSpec(t, f))

	// Assert.
	refusal := asRefusal(t, err, ArmNamingFailed)
	if refusal.Fields["attempts"] != uint32(NamingAttempts) {
		t.Fatalf("attempts = %v, want %d", refusal.Fields["attempts"], NamingAttempts)
	}
}

// TestNamingRefusalCarriesTheLastAnswer pins the one field that carries
// model-authored text to a client: it is what diagnoses a bad brief.
func TestNamingRefusalCarriesTheLastAnswer(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	script(f,
		headlessAnswer{text: "Sure! Here is a name:"},
		headlessAnswer{text: "Fix The Login Bug Today"})

	// Act.
	_, err := f.verbs.Create(context.Background(), standardSpec(t, f))

	// Assert.
	refusal := asRefusal(t, err, ArmNamingFailed)
	if refusal.Fields["answer"] != "Fix The Login Bug Today" {
		t.Fatalf("answer = %v, want the last answer the model gave", refusal.Fields["answer"])
	}
}

func TestNamingRefusalNamesTheModel(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	script(f, headlessAnswer{text: "not a slug"}, headlessAnswer{text: "still not a slug!!"})

	// Act.
	_, err := f.verbs.Create(context.Background(), standardSpec(t, f))

	// Assert.
	refusal := asRefusal(t, err, ArmNamingFailed)
	if refusal.Fields["model"] != headless.ModelHaiku {
		t.Fatalf("model = %v, want %q", refusal.Fields["model"], headless.ModelHaiku)
	}
}

func TestCreateRefusesWhenTheNamingCallFails(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	script(f,
		headlessAnswer{err: &headless.Error{Cause: headless.CauseExitStatus, Detail: "exit status 3"}},
		headlessAnswer{err: &headless.Error{Cause: headless.CauseExitStatus, Detail: "exit status 3"}})

	// Act.
	_, err := f.verbs.Create(context.Background(), standardSpec(t, f))

	// Assert.
	refusal := asRefusal(t, err, ArmNamingFailed)
	if refusal.Fields["cause"] != headless.CauseExitStatus {
		t.Fatalf("cause = %v, want %q", refusal.Fields["cause"], headless.CauseExitStatus)
	}
}

func TestCreateRefusesWhenTheNamingCallTimesOut(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	timeout := &headless.Error{Cause: headless.CauseTimeout, Detail: "signal: killed"}
	script(f, headlessAnswer{err: timeout}, headlessAnswer{err: timeout})

	// Act.
	_, err := f.verbs.Create(context.Background(), standardSpec(t, f))

	// Assert.
	refusal := asRefusal(t, err, ArmNamingFailed)
	if refusal.Fields["cause"] != headless.CauseTimeout {
		t.Fatalf("cause = %v, want %q", refusal.Fields["cause"], headless.CauseTimeout)
	}
}

// TestCreateDoesNotRetryAGuardRefusal pins that a standing fact about the
// process is not asked twice: a second call would be refused identically and
// would only cost the user another wait.
func TestCreateDoesNotRetryAGuardRefusal(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	refused := &headless.Error{Cause: headless.CauseGuardRefused, Detail: "vendor calls are forbidden"}
	script(f, headlessAnswer{err: refused}, headlessAnswer{err: refused})

	// Act.
	_, err := f.verbs.Create(context.Background(), standardSpec(t, f))

	// Assert.
	refusal := asRefusal(t, err, ArmNamingFailed)
	if refusal.Fields["cause"] != headless.CauseGuardRefused {
		t.Fatalf("cause = %v, want %q", refusal.Fields["cause"], headless.CauseGuardRefused)
	}
	if len(f.headless.calls) != 1 {
		t.Fatalf("calls = %d, want exactly one", len(f.headless.calls))
	}
}

func TestCreateRefusesWhenTheNamingBriefIsAbsent(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	delete(f.briefs, BriefWorkspaceName)

	// Act.
	_, err := f.verbs.Create(context.Background(), standardSpec(t, f))

	// Assert.
	refusal := asRefusal(t, err, ArmBriefMissing)
	if refusal.Fields["name"] != BriefWorkspaceName {
		t.Fatalf("name = %v, want %q", refusal.Fields["name"], BriefWorkspaceName)
	}
}

// TestCreateRefusesWithNoNamingRunnerAtAll pins that a daemon built without
// the headless facility refuses rather than inventing a name.
func TestCreateRefusesWithNoNamingRunnerAtAll(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.verbs.(*verbs).deps.Headless = nil

	// Act.
	_, err := f.verbs.Create(context.Background(), standardSpec(t, f))

	// Assert.
	refusal := asRefusal(t, err, ArmNamingFailed)
	if refusal.Fields["cause"] != headless.CauseNoBinary {
		t.Fatalf("cause = %v, want %q", refusal.Fields["cause"], headless.CauseNoBinary)
	}
}

func TestCreateSkipsTheNamingCallWhenANameIsSupplied(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	spec := standardSpec(t, f)
	spec.Name = "chosen-name"

	// Act.
	if _, err := f.verbs.Create(context.Background(), spec); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if len(f.headless.calls) != 0 {
		t.Fatalf("calls = %+v, want the naming call never made for a supplied name", f.headless.calls)
	}
}

func TestCreateSkipsTheNamingCallWithNoPrompt(t *testing.T) {
	// Arrange: a promptless standard create, named after its own minted id.
	f := newFixture(t)
	spec := standardSpec(t, f)
	spec.InitialPrompt = ""

	// Act.
	if _, err := f.verbs.Create(context.Background(), spec); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if len(f.headless.calls) != 0 {
		t.Fatalf("calls = %+v, want no naming call with nothing to name from", f.headless.calls)
	}
}

func TestCreateSuffixesAMintedNameThatCollidesWithABranch(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	t.Setenv(PrefixEnv, "")
	t.Setenv(LegacyPrefixEnv, "")
	f.git.existingBranches = map[string]bool{"flaky-login-test": true}
	script(f, headlessAnswer{text: "flaky-login-test"})

	// Act.
	if _, err := f.verbs.Create(context.Background(), standardSpec(t, f)); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if f.git.created[0].Branch != "flaky-login-test-2" {
		t.Fatalf("branch = %q, want the -2 suffix", f.git.created[0].Branch)
	}
}

func TestCreateWalksToTheNextFreeSuffix(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	t.Setenv(PrefixEnv, "")
	t.Setenv(LegacyPrefixEnv, "")
	f.git.existingBranches = map[string]bool{"flaky-login-test": true, "flaky-login-test-2": true}
	script(f, headlessAnswer{text: "flaky-login-test"})

	// Act.
	if _, err := f.verbs.Create(context.Background(), standardSpec(t, f)); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if f.git.created[0].Branch != "flaky-login-test-3" {
		t.Fatalf("branch = %q, want the -3 suffix", f.git.created[0].Branch)
	}
}

// TestCreateSuffixesAMintedNameThatCollidesWithAWorkspace pins that a
// REGISTERED name collides even when no branch does — the roster is the other
// half of the namespace.
func TestCreateSuffixesAMintedNameThatCollidesWithAWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	t.Setenv(PrefixEnv, "")
	t.Setenv(LegacyPrefixEnv, "")
	f.db.workspaces["ws-existing"] = wsm.Workspace{ID: "ws-existing", Dir: "/elsewhere", Name: "flaky-login-test"}
	script(f, headlessAnswer{text: "flaky-login-test"})

	// Act.
	if _, err := f.verbs.Create(context.Background(), standardSpec(t, f)); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if f.git.created[0].Branch != "flaky-login-test-2" {
		t.Fatalf("branch = %q, want the -2 suffix", f.git.created[0].Branch)
	}
}

func TestCreateDoesNotSuffixAnUncollidedName(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	t.Setenv(PrefixEnv, "")
	t.Setenv(LegacyPrefixEnv, "")
	script(f, headlessAnswer{text: "flaky-login-test"})

	// Act.
	if _, err := f.verbs.Create(context.Background(), standardSpec(t, f)); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if f.git.created[0].Branch != "flaky-login-test" {
		t.Fatalf("branch = %q, want no suffix on a free name", f.git.created[0].Branch)
	}
}

// TestCreateDoesNotDisambiguateASuppliedName pins that the user's own name is
// taken as it stands: they typed it, and are owed git's own refusal if it is
// taken rather than a silently different workspace.
func TestCreateDoesNotDisambiguateASuppliedName(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	t.Setenv(PrefixEnv, "")
	t.Setenv(LegacyPrefixEnv, "")
	f.git.existingBranches = map[string]bool{"chosen-name": true}
	spec := standardSpec(t, f)
	spec.Name = "chosen-name"

	// Act.
	if _, err := f.verbs.Create(context.Background(), spec); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if f.git.created[0].Branch != "chosen-name" {
		t.Fatalf("branch = %q, want the supplied name unchanged", f.git.created[0].Branch)
	}
}

// The headless-failure cause helper moved to the headless package as
// headless.CauseOf; its tests live in internal/headless/api_test.go.

func TestFreeNameAgainstSkipsWhatElseHoldsAName(t *testing.T) {
	cases := []struct {
		name     string
		held     map[string]bool
		probeErr error
		want     string
		wantErr  bool
	}{
		{name: "a free name is taken as it is", want: "why-sky"},
		{name: "a held name takes the next suffix", held: map[string]bool{"why-sky": true}, want: "why-sky-2"},
		{name: "every held suffix is skipped", held: map[string]bool{"why-sky": true, "why-sky-2": true}, want: "why-sky-3"},
		{name: "a failed probe is surfaced", probeErr: errors.New("stat failed"), wantErr: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			probe := func(candidate string) (bool, error) { return tc.held[candidate], tc.probeErr }

			// Act
			got, release, err := f.verbs.(*verbs).freeNameAgainst(context.Background(), f.log.logger, "why-sky", probe)

			// Assert
			if (err != nil) != tc.wantErr {
				t.Fatalf("err = %v, want error %v", err, tc.wantErr)
			}
			if tc.wantErr {
				return
			}
			defer release()
			if got != tc.want {
				t.Fatalf("name = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestEveryNameIsDisambiguatedByFreeNameAgainst(t *testing.T) {
	// Arrange / Act: the suffix loop is written once.
	loops := sourcescan.Count(t, `"-" + strconv.Itoa(suffix)`)

	// Assert
	if loops != 1 {
		t.Fatalf("name suffixes are composed at %d sites, want 1: freeNameAgainst", loops)
	}
}

func TestATimedOutNamingCallTheNetworkClearedRecordsTheCheck(t *testing.T) {
	// Arrange: the reachability check answered, so the timeout is no outage.
	f := newFixture(t)
	f.network.verdict = netstate.Verdict{Detail: "https://api.anthropic.com answered HEAD with 404"}
	script(f, headlessAnswer{err: namingTimeout}, headlessAnswer{err: namingTimeout})

	// Act
	_, err := f.verbs.Create(context.Background(), standardSpec(t, f))

	// Assert: refused, and each attempt's record names what cleared the network.
	asRefusal(t, err, ArmNamingFailed)
	records := f.log.logger.Records()
	var seen int
	for _, r := range records {
		if r.Operation != opNaming || r.Message != "the workspace naming call did not answer" {
			continue
		}
		seen++
		if r.Context["cause"] != headless.CauseTimeout || r.Context["network"] != false ||
			r.Context["network_detail"] != "https://api.anthropic.com answered HEAD with 404" {
			t.Fatalf("did-not-answer record = %+v, want the timeout with the check's answer", r.Context)
		}
	}
	if seen != NamingAttempts {
		t.Fatalf("did-not-answer records = %d, want %d: %+v", seen, NamingAttempts, records)
	}
}

func TestANamingCallTheNetworkCannotExplainRecordsNoCheck(t *testing.T) {
	// Arrange: a guard refusal says nothing about the network.
	f := newFixture(t)
	refused := &headless.Error{Cause: headless.CauseGuardRefused, Detail: "forbidden"}
	script(f, headlessAnswer{err: refused})

	// Act
	_, err := f.verbs.Create(context.Background(), standardSpec(t, f))

	// Assert
	asRefusal(t, err, ArmNamingFailed)
	r, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opNaming, Message: "the workspace naming call did not answer"})
	if !ok {
		t.Fatalf("no did-not-answer record")
	}
	if _, has := r.Context["network"]; has {
		t.Fatalf("record = %+v, want no network check on a guard refusal", r.Context)
	}
}
