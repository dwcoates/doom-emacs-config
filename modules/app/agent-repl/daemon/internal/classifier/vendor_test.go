package classifier

import (
	"context"
	"errors"
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
