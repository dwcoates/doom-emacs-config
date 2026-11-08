package namingdigest

import (
	"fmt"
	"path/filepath"
	"strings"
	"testing"
	"unicode/utf8"

	"agent-repld/internal/prompts"
)

func TestComposeListsTheRequestsOldestFirstUnderOneHeader(t *testing.T) {
	// Arrange, Act.
	got := Compose([]string{"fix the footer", "  then the topbar  "})

	// Assert.
	want := "The user's requests:\n- fix the footer\n- then the topbar\n"
	if got != want {
		t.Fatalf("Compose = %q, want %q", got, want)
	}
}

func TestComposeQuotesOnlyTheMostRecentRequests(t *testing.T) {
	// Arrange.
	var requests []string
	for i := 0; i <= MaxPrompts; i++ {
		requests = append(requests, fmt.Sprintf("request-%02d", i))
	}

	// Act.
	got := Compose(requests)

	// Assert.
	if strings.Contains(got, "request-00") {
		t.Fatalf("Compose = %q, want the request past MaxPrompts dropped", got)
	}
	if !strings.Contains(got, fmt.Sprintf("request-%02d", MaxPrompts)) {
		t.Fatalf("Compose = %q, want the newest request kept", got)
	}
}

func TestComposeBoundsOneRequest(t *testing.T) {
	// Arrange, Act.
	got := Compose([]string{strings.Repeat("x", MaxPromptRunes*2)})

	// Assert — the request is cut to MaxPromptRunes and marked as cut.
	if n := strings.Count(got, "x"); n != MaxPromptRunes || !strings.Contains(got, "…") {
		t.Fatalf("Compose kept %d runes of one request, want MaxPromptRunes (%d) and an ellipsis", n, MaxPromptRunes)
	}
}

func TestComposeTrimmingToTheTotalCapKeepsTheNewestRequests(t *testing.T) {
	// Arrange — enough padded, uniquely-marked requests that the rendered
	// digest must drop some of them to fit under the total cap.
	var requests []string
	for i := 0; i < MaxPrompts; i++ {
		requests = append(requests, fmt.Sprintf("marker-%02d %s", i, strings.Repeat("x", 400)))
	}

	// Act.
	got := Compose(requests)

	// Assert.
	if n := utf8.RuneCountInString(got); n > MaxDigestTotalRunes {
		t.Fatalf("Compose rendered %d runes, want at most MaxDigestTotalRunes (%d)", n, MaxDigestTotalRunes)
	}
	if !strings.Contains(got, fmt.Sprintf("marker-%02d", MaxPrompts-1)) {
		t.Fatalf("Compose dropped the newest request")
	}
	if strings.Contains(got, "marker-00") {
		t.Fatalf("Compose kept the oldest request while trimming")
	}
}

func TestComposeOpensTemplateDelimitersAUserTyped(t *testing.T) {
	cases := []struct {
		name   string
		prompt string
	}{
		{name: "a placeholder token", prompt: "why does {{prompt}} not splice"},
		{name: "a run of three braces", prompt: "a {{{ b }}} c"},
		{name: "an unclosed opener", prompt: "a {{ b"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got := Compose([]string{tc.prompt})

			// Assert.
			if strings.Contains(got, "{{") || strings.Contains(got, "}}") {
				t.Fatalf("Compose = %q, want no template delimiter left", got)
			}
		})
	}
}

func TestTheNamingBriefSplicesADigestThatTalksAboutTemplates(t *testing.T) {
	// Arrange — the real naming brief, and a conversation that names a
	// placeholder.
	brief, err := prompts.Load(filepath.Join("..", "..", "..", "prompts"), "workspace-name-from-prompt")
	if err != nil {
		t.Fatalf("load the naming brief: %v", err)
	}

	// Act.
	_, err = brief.Splice(map[string]string{"prompt": "name it", "conversation": Compose([]string{"rename {{prompt}}"})})

	// Assert.
	if err != nil {
		t.Fatalf("Splice = %v, want the quoted digest accepted", err)
	}
}
