package classifier

import (
	"context"
	"errors"
	"strings"
	"testing"

	"claude-repld/internal/envc"
	"claude-repld/internal/headless"
	"claude-repld/internal/prompts"
)

func TestVendorJudgeTakesTheFastPathWithoutAskingTheGuard(t *testing.T) {
	// Arrange: the forbidding guard every test process runs under.
	j := newVendorJudge(forbiddingGuard(t), headless.New(forbiddingGuard(t), "fake-claude"), "unused")
	j.run = func(context.Context, string) (string, error) {
		t.Fatal("the fast path must not run the vendor")
		return "", nil
	}
	// Act
	got, err := j.Judge(context.Background(), "running", "halt")
	// Assert
	if err != nil {
		t.Fatalf("Judge: %v", err)
	}
	if got.Route != RouteInterrupt || !got.FastPath {
		t.Fatalf("verdict = %+v, want an interrupting fast-path verdict", got)
	}
}

func TestVendorJudgeRefusesWhenVendorCallsAreForbidden(t *testing.T) {
	// Arrange
	j := newVendorJudge(forbiddingGuard(t), headless.New(forbiddingGuard(t), "fake-claude"), "unused")
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

func TestVendorJudgeRoutesEachAnswerToken(t *testing.T) {
	tests := []struct {
		name   string
		answer string
		want   Route
	}{
		{name: "interrupt", answer: TokenInterrupt + "\n", want: RouteInterrupt},
		{name: "after this tool call", answer: TokenAfterToolCall, want: RouteAfterToolCall},
		{name: "hold, padded", answer: " " + TokenHold + " ", want: RouteQueue},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			j, _ := answering(t, permissiveGuard(t), tt.answer, nil)
			// Act
			got, err := j.Judge(context.Background(), "running", "also update the docs")
			// Assert
			if err != nil {
				t.Fatalf("Judge: %v", err)
			}
			if got.Route != tt.want || got.Reason == "" || got.FastPath {
				t.Fatalf("verdict = %+v, want route %s with a stated reason", got, tt.want)
			}
		})
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
	j := newVendorJudge(permissiveGuard(t), headless.New(permissiveGuard(t), "fake-claude"), "unused")
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
	j := newVendorJudge(permissiveGuard(t), nil, "unused")
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
		"token_interrupt":       TokenInterrupt,
		"token_after_tool_call": TokenAfterToolCall,
		"token_hold":            TokenHold,
		"running_turn":          "the running turn",
		"new_message":           "the new message",
	}

	// Act.
	got, err := spliceBrief(routingBrief, values)

	// Assert.
	if err != nil {
		t.Fatalf("spliceBrief() error = %v, want nil", err)
	}
	for _, want := range []string{TokenInterrupt, TokenAfterToolCall, TokenHold, "the running turn", "the new message"} {
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
