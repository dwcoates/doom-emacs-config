package classifier

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/internal/envc"
	"claude-repld/internal/prompts"
)

func TestVendorJudgeTakesTheFastPathWithoutAskingTheGuard(t *testing.T) {
	// Arrange: the forbidding guard every test process runs under.
	j := newVendorJudge(forbiddingGuard(t), "fake-claude", "unused")
	j.run = func(context.Context, string, string) (string, error) {
		t.Fatal("the fast path must not run the vendor")
		return "", nil
	}
	// Act
	got, err := j.Judge(context.Background(), "running", "halt")
	// Assert
	if err != nil {
		t.Fatalf("Judge: %v", err)
	}
	if !got.Interject || !got.FastPath {
		t.Fatalf("verdict = %+v, want an interjecting fast-path verdict", got)
	}
}

func TestVendorJudgeRefusesWhenVendorCallsAreForbidden(t *testing.T) {
	// Arrange
	j := newVendorJudge(forbiddingGuard(t), "fake-claude", "unused")
	// Act
	_, err := j.Judge(context.Background(), "running", "an ordinary follow-up")
	// Assert
	var forbidden *envc.ForbiddenError
	if !errors.As(err, &forbidden) {
		t.Fatalf("err = %v, want a vendor-guard refusal", err)
	}
	if forbidden.Site != VendorSite {
		t.Fatalf("site = %q, want %q", forbidden.Site, VendorSite)
	}
}

func TestVendorJudgeInterjectsOnTheJumpToken(t *testing.T) {
	// Arrange
	j, _ := answering(t, permissiveGuard(t), TokenJump+"\n", nil)
	// Act
	got, err := j.Judge(context.Background(), "running", "also update the docs")
	// Assert
	if err != nil {
		t.Fatalf("Judge: %v", err)
	}
	if !got.Interject {
		t.Fatalf("verdict = %+v, want an interjecting verdict", got)
	}
}

func TestVendorJudgeHoldsOnTheHoldToken(t *testing.T) {
	// Arrange
	j, _ := answering(t, permissiveGuard(t), " "+TokenHold+" ", nil)
	// Act
	got, err := j.Judge(context.Background(), "running", "an unrelated question")
	// Assert
	if err != nil {
		t.Fatalf("Judge: %v", err)
	}
	if got.Interject {
		t.Fatalf("verdict = %+v, want a holding verdict", got)
	}
}

func TestVendorJudgeRefusesAnAnswerThatIsNeitherToken(t *testing.T) {
	// Arrange
	j, _ := answering(t, permissiveGuard(t), "maybe interject?", nil)
	// Act
	_, err := j.Judge(context.Background(), "running", "an ordinary follow-up")
	// Assert: a verdict is never guessed from an unrecognized answer.
	if err == nil {
		t.Fatal("an unrecognized answer must be an error, never a verdict")
	}
}

func TestVendorJudgeSplicesBothPromptsIntoTheQuestion(t *testing.T) {
	// Arrange
	j, asked := answering(t, permissiveGuard(t), TokenHold, nil)
	// Act
	if _, err := j.Judge(context.Background(), "the running turn", "the new message"); err != nil {
		t.Fatalf("Judge: %v", err)
	}
	// Assert
	if !strings.Contains(*asked, "the running turn") || !strings.Contains(*asked, "the new message") {
		t.Fatalf("question = %q, want both prompts spliced in", *asked)
	}
}

func TestVendorJudgeSurfacesAnAbsentBrief(t *testing.T) {
	// Arrange
	j := newVendorJudge(permissiveGuard(t), "fake-claude", "unused")
	j.load = func(string, string) (prompts.Prompt, error) { return prompts.Prompt{}, errLoad }
	// Act
	_, err := j.Judge(context.Background(), "running", "an ordinary follow-up")
	// Assert
	if !errors.Is(err, errLoad) {
		t.Fatalf("err = %v, want the loader's failure surfaced", err)
	}
}

func TestVendorJudgeSurfacesARunFailure(t *testing.T) {
	// Arrange
	runErr := errors.New("the vendor exited 1")
	j, _ := answering(t, permissiveGuard(t), "", runErr)
	// Act
	_, err := j.Judge(context.Background(), "running", "an ordinary follow-up")
	// Assert
	if !errors.Is(err, runErr) {
		t.Fatalf("err = %v, want the run's failure surfaced", err)
	}
}

func TestVendorJudgeRefusesWithNoVendorBinaryConfigured(t *testing.T) {
	// Arrange
	j := newVendorJudge(permissiveGuard(t), "", "unused")
	// Act
	_, err := j.Judge(context.Background(), "running", "an ordinary follow-up")
	// Assert
	if err == nil {
		t.Fatal("a judge with no vendor binary must refuse, never exec an empty name")
	}
}

// TestSpliceBriefSubstitutesEveryPlaceholder pins the production splicer: the
// brief's own substitution, which is what keeps the question and the answer
// parser from drifting apart.
func TestSpliceBriefSubstitutesEveryPlaceholder(t *testing.T) {
	// Arrange.
	values := map[string]string{
		"token_jump":   TokenJump,
		"token_hold":   TokenHold,
		"running_turn": "the running turn",
		"new_message":  "the new message",
	}

	// Act.
	got, err := spliceBrief(routingBrief, values)

	// Assert.
	if err != nil {
		t.Fatalf("spliceBrief() error = %v, want nil", err)
	}
	for _, want := range []string{TokenJump, TokenHold, "the running turn", "the new message"} {
		if !strings.Contains(got, want) {
			t.Fatalf("question = %q, want it to contain %q", got, want)
		}
	}
}

// TestSpliceBriefSurfacesAnUnknownPlaceholder pins that the splicer's own
// refusal reaches the judge rather than producing a half-composed question.
func TestSpliceBriefSurfacesAnUnknownPlaceholder(t *testing.T) {
	// Arrange: a value the brief never declared.
	values := map[string]string{"not_a_placeholder": "x"}

	// Act.
	_, err := spliceBrief(routingBrief, values)

	// Assert.
	if err == nil {
		t.Fatal("spliceBrief() = nil error, want the brief's own refusal")
	}
}

// TestRunVendorAnswersWithStdout pins the exec site's ordinary case: the
// binary's stdout is the answer, verbatim.
func TestRunVendorAnswersWithStdout(t *testing.T) {
	// Arrange.
	bin := fakeVendor(t, "printf '"+TokenHold+"\\n'")

	// Act.
	got, err := runVendor(context.Background(), bin, "the question")

	// Assert.
	if err != nil {
		t.Fatalf("runVendor() error = %v, want nil", err)
	}
	if strings.TrimSpace(got) != TokenHold {
		t.Fatalf("runVendor() = %q, want %q", got, TokenHold)
	}
}

// TestRunVendorPutsTheQuestionOnStdin pins WHY the exec site exists in this
// shape: the composed question never rides an argv a process listing shows.
func TestRunVendorPutsTheQuestionOnStdin(t *testing.T) {
	// Arrange.
	seen := filepath.Join(t.TempDir(), "stdin")
	bin := fakeVendor(t, "cat > "+seen+"\nprintf '"+TokenHold+"'")

	// Act.
	if _, err := runVendor(context.Background(), bin, "the composed question"); err != nil {
		t.Fatalf("runVendor() error = %v, want nil", err)
	}

	// Assert.
	body, err := os.ReadFile(seen)
	if err != nil {
		t.Fatalf("read the recorded stdin: %v", err)
	}
	if string(body) != "the composed question" {
		t.Fatalf("stdin = %q, want the composed question", body)
	}
}

// TestRunVendorPinsTheHeadlessArgv pins the invocation: a headless print run,
// and nothing that would make the binary interactive.
func TestRunVendorPinsTheHeadlessArgv(t *testing.T) {
	// Arrange.
	seen := filepath.Join(t.TempDir(), "argv")
	bin := fakeVendor(t, `printf '%s\n' "$@" > `+seen+"\nprintf '"+TokenHold+"'")

	// Act.
	if _, err := runVendor(context.Background(), bin, "q"); err != nil {
		t.Fatalf("runVendor() error = %v, want nil", err)
	}

	// Assert.
	body, err := os.ReadFile(seen)
	if err != nil {
		t.Fatalf("read the recorded argv: %v", err)
	}
	if got, want := string(body), "-p\n--output-format\ntext\n"; got != want {
		t.Fatalf("argv = %q, want %q", got, want)
	}
}

// TestRunVendorSurfacesAnAbsentBinary pins the case an operator most needs to
// see: the configured binary is not there at all.
func TestRunVendorSurfacesAnAbsentBinary(t *testing.T) {
	// Arrange.
	bin := filepath.Join(t.TempDir(), "not-installed")

	// Act.
	_, err := runVendor(context.Background(), bin, "q")

	// Assert.
	if err == nil {
		t.Fatal("runVendor() = nil error, want a refusal naming the absent binary")
	}
	if !strings.Contains(err.Error(), bin) {
		t.Fatalf("err = %v, want it to name %q", err, bin)
	}
}

// TestRunVendorSurfacesANonZeroExitWithItsStderr pins that a failing vendor
// run carries its own diagnosis: the stderr is what says why.
func TestRunVendorSurfacesANonZeroExitWithItsStderr(t *testing.T) {
	// Arrange.
	bin := fakeVendor(t, "echo 'credit balance is too low' >&2\nexit 3")

	// Act.
	_, err := runVendor(context.Background(), bin, "q")

	// Assert.
	if err == nil {
		t.Fatal("runVendor() = nil error, want the non-zero exit surfaced")
	}
	if !strings.Contains(err.Error(), "credit balance is too low") {
		t.Fatalf("err = %v, want it to carry the binary's stderr", err)
	}
}

// TestRunVendorSurfacesASignalDeath pins that a vendor killed by a signal is
// an error, never an empty answer read as an unrecognized token.
func TestRunVendorSurfacesASignalDeath(t *testing.T) {
	// Arrange.
	bin := fakeVendor(t, "kill -TERM $$")

	// Act.
	out, err := runVendor(context.Background(), bin, "q")

	// Assert.
	if err == nil {
		t.Fatalf("runVendor() = (%q, nil), want a signal death reported as an error", out)
	}
	if !strings.Contains(err.Error(), "signal") {
		t.Fatalf("err = %v, want it to name the signal", err)
	}
}

// TestRunVendorHonorsACancelledContext pins that a classification the caller
// abandoned does not exec on regardless.
func TestRunVendorHonorsACancelledContext(t *testing.T) {
	// Arrange.
	bin := fakeVendor(t, "printf '"+TokenHold+"'")
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	_, err := runVendor(ctx, bin, "q")

	// Assert.
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("err = %v, want context.Canceled", err)
	}
}

// TestJudgeThroughTheRealExecSite pins the whole vendor path end to end over a
// scripted binary: the brief is spliced, the question rides stdin, and the
// binary's token becomes the verdict.
func TestJudgeThroughTheRealExecSite(t *testing.T) {
	// Arrange.
	j := newVendorJudge(permissiveGuard(t), fakeVendor(t, "printf '"+TokenJump+"'"), "unused")
	j.load = func(string, string) (prompts.Prompt, error) { return routingBrief, nil }

	// Act.
	got, err := j.Judge(context.Background(), "the running turn", "also update the docs")

	// Assert.
	if err != nil {
		t.Fatalf("Judge() error = %v, want nil", err)
	}
	if !got.Interject || got.FastPath {
		t.Fatalf("verdict = %+v, want a model-decided interjecting verdict", got)
	}
}

// TestVendorJudgeSurfacesASpliceFailure pins that a brief whose placeholders
// cannot be filled refuses the classification rather than asking the model a
// half-composed question.
func TestVendorJudgeSurfacesASpliceFailure(t *testing.T) {
	// Arrange.
	spliceErr := errors.New("the brief declares a placeholder nothing fills")
	j := newVendorJudge(permissiveGuard(t), "fake-claude", "unused")
	j.load = func(string, string) (prompts.Prompt, error) { return routingBrief, nil }
	j.splice = func(prompts.Prompt, map[string]string) (string, error) { return "", spliceErr }
	j.run = func(context.Context, string, string) (string, error) {
		t.Fatal("a question that could not be composed must never be asked")
		return "", nil
	}

	// Act.
	_, err := j.Judge(context.Background(), "running", "an ordinary follow-up")

	// Assert.
	if !errors.Is(err, spliceErr) {
		t.Fatalf("err = %v, want the splicer's failure surfaced", err)
	}
}
