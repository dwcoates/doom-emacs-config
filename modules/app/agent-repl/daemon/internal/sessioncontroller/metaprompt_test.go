package sessioncontroller

import (
	"fmt"
	"testing"
)

// directiveFor builds the read-directive naming PATH, exactly as a
// pre-migration daemon (and as Emacs's on-demand re-read) writes it.
func directiveFor(path string) string {
	return fmt.Sprintf(metapromptDirectiveTemplate, path)
}

func TestIsMetapromptDirectiveTextStandalone(t *testing.T) {
	// Arrange: the directive alone, which is machinery talking to the agent.
	text := directiveFor("/repo/modules/app/agent-repl/metaprompt.md")

	// Act + Assert.
	if !isMetapromptDirectiveText(text) {
		t.Fatal("a standalone read-directive must be recognized so its transcript line draws no bubble")
	}
}

func TestIsMetapromptDirectiveTextSurroundingWhitespace(t *testing.T) {
	// Arrange: the transcript may carry the directive with padding.
	text := "\n\t " + directiveFor("/repo/modules/app/agent-repl/metaprompt.md") + " \n"

	// Act + Assert.
	if !isMetapromptDirectiveText(text) {
		t.Fatal("padding must not hide a standalone directive")
	}
}

func TestIsMetapromptDirectiveTextFoldedKeepsUserPrompt(t *testing.T) {
	// Arrange: a pre-migration FOLDED directive, with the user's real prompt
	// after it.
	text := directiveFor("/repo/modules/app/agent-repl/metaprompt.md") + "\n\nfix the parser"

	// Act + Assert: the tail no longer matches, so the bubble is drawn —
	// correctly, because the user did type the prompt inside it.
	if isMetapromptDirectiveText(text) {
		t.Fatal("a folded directive carries a real user prompt and must still be shown")
	}
}

func TestIsMetapromptDirectiveTextOrdinaryPrompt(t *testing.T) {
	// Arrange + Act + Assert.
	if isMetapromptDirectiveText("read the metaprompt and tell me what it says") {
		t.Fatal("an ordinary prompt that merely mentions the metaprompt is not the directive")
	}
}

func TestIsMetapromptDirectiveTextEmpty(t *testing.T) {
	// Arrange + Act + Assert.
	if isMetapromptDirectiveText("") {
		t.Fatal("empty text is not the directive")
	}
}
