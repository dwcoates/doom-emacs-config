package workspace

import (
	"context"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/internal/headless"
	"claude-repld/internal/prompts"
	"claude-repld/internal/wsm"
)

// script points the fixture's naming call at a sequence of answers, one per
// attempt.
func script(f *fixture, answers ...headlessAnswer) {
	f.headless.answers = answers
}

// TestTheShippedNamingBriefTakesTheConversation pins the brief the daemon
// ships against the values mintName splices: the prompt, the conversation a
// fork continues, and the retry's correction — no more, no fewer.
func TestTheShippedNamingBriefTakesTheConversation(t *testing.T) {
	// Arrange.
	brief, err := prompts.Load(filepath.Join("..", "..", "..", "prompts"), BriefWorkspaceName)
	if err != nil {
		t.Fatalf("Load: %v", err)
	}

	// Act.
	got, err := brief.Splice(map[string]string{
		"prompt": "", "conversation": "The user's requests:\n- wire iterm2\n", "correction": "",
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
	t.Setenv(PrefixEnv, "DWC")
	script(f, headlessAnswer{text: "flaky-login-test"})

	// Act.
	if _, err := f.verbs.Create(context.Background(), standardSpec(t, f)); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert: the model answers a BARE slug and the daemon prefixes it, so the
	// model has no way to get the prefix wrong.
	if f.git.created[0].Branch != "DWC/flaky-login-test" {
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

// TestCreateTellsTheModelWhatWasWrongWithItsFirstAnswer pins that the retry is
// a CORRECTED question, not the identical one asked twice.
func TestCreateTellsTheModelWhatWasWrongWithItsFirstAnswer(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	script(f,
		headlessAnswer{text: "The name is flaky-login-test."},
		headlessAnswer{text: "flaky-login-test"})

	// Act.
	if _, err := f.verbs.Create(context.Background(), standardSpec(t, f)); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if !strings.Contains(f.headless.calls[1].Prompt, "The name is flaky-login-test.") {
		t.Fatalf("retry prompt = %q, want the rejected answer quoted back", f.headless.calls[1].Prompt)
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
