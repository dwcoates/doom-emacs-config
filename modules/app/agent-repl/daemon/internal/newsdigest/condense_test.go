package newsdigest

import (
	"context"
	"errors"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/headless"
	"claude-repld/internal/prompts"
)

// allowedFixture is the link set the answer tests validate against.
var allowedFixture = map[string]bool{"https://fixture.test/releases/new": true, "https://fixture.test/news": true}

// item is one well-formed answer item linking the new release.
const item = `{"title":"T","summary":"S","links":[{"label":"L","url":"https://fixture.test/releases/new"}]}`

func TestParseAnswerOrdersSectionsByRankWithTheDaemonsHeadings(t *testing.T) {
	// Arrange
	text := `{"sections":[{"kind":"feature","items":[` + item + `]},{"kind":"backend","items":[` + item + `]}]}`

	// Act
	sections, err := parseAnswer(text, allowedFixture)

	// Assert
	if err != nil {
		t.Fatalf("parseAnswer: %v", err)
	}
	if len(sections) != 2 || sections[0].GetKind().GetBackend() == nil || sections[1].GetKind().GetFeature() == nil {
		t.Fatalf("sections = %v, want backend then feature", sections)
	}
	if sections[0].GetHeading().GetText() != "Affects the agent-repl backend" || sections[1].GetHeading().GetText() != "New features" {
		t.Fatalf("headings = %q, %q, want the daemon's", sections[0].GetHeading().GetText(), sections[1].GetHeading().GetText())
	}
}

func TestParseAnswerKeepsAnItemWhole(t *testing.T) {
	// Act
	sections, err := parseAnswer(answerJSON, allowedFixture)

	// Assert
	if err != nil {
		t.Fatalf("parseAnswer: %v", err)
	}
	got := sections[0].GetItems()[0]
	if got.GetTitle().GetText() != "SDK drops subscription billing" || got.GetSummary().GetText() != "The SDK now needs an API key." ||
		got.GetEffective().GetText() != "2026-11-01" || got.GetLinks()[0].GetUrl() != "https://fixture.test/releases/new" ||
		got.GetLinks()[0].GetLabel() != "Release new" {
		t.Fatalf("item = %v, want the answered item verbatim", got)
	}
}

func TestParseAnswerLeavesAnUnstatedEffectiveDateUnset(t *testing.T) {
	// Act
	sections, err := parseAnswer(`{"sections":[{"kind":"release","items":[`+item+`]}]}`, allowedFixture)

	// Assert
	if err != nil || sections[0].GetItems()[0].Effective != nil {
		t.Fatalf("parseAnswer = (%v, %v), want an unset effective date", sections, err)
	}
}

func TestParseAnswerAcceptsOneEnclosingCodeFence(t *testing.T) {
	// Act
	sections, err := parseAnswer("```json\n"+answerJSON+"\n```", allowedFixture)

	// Assert
	if err != nil || len(sections) != 1 {
		t.Fatalf("parseAnswer = (%v, %v), want the fenced answer read", sections, err)
	}
}

func TestParseAnswerReadsNoSectionsAsNothingWorthReporting(t *testing.T) {
	// Act
	sections, err := parseAnswer(`{"sections":[]}`, allowedFixture)

	// Assert
	if err != nil || len(sections) != 0 {
		t.Fatalf("parseAnswer = (%v, %v), want no sections and no error", sections, err)
	}
}

func TestParseAnswerRefusesAMalformedAnswer(t *testing.T) {
	tests := []struct {
		name string
		text string
	}{
		{name: "prose", text: "Here is the digest."},
		{name: "an unknown field", text: `{"sections":[],"note":"x"}`},
		{name: "trailing data", text: `{"sections":[]} and more`},
		{name: "no sections list", text: `{}`},
		{name: "an unknown kind", text: `{"sections":[{"kind":"gossip","items":[` + item + `]}]}`},
		{name: "a repeated kind", text: `{"sections":[{"kind":"release","items":[` + item + `]},{"kind":"release","items":[` + item + `]}]}`},
		{name: "a section with no items", text: `{"sections":[{"kind":"release","items":[]}]}`},
		{name: "an item with no title", text: `{"sections":[{"kind":"release","items":[{"title":" ","summary":"S","links":[{"label":"L","url":"https://fixture.test/news"}]}]}]}`},
		{name: "an item with no summary", text: `{"sections":[{"kind":"release","items":[{"title":"T","summary":"","links":[{"label":"L","url":"https://fixture.test/news"}]}]}]}`},
		{name: "an item with no links", text: `{"sections":[{"kind":"release","items":[{"title":"T","summary":"S","links":[]}]}]}`},
		{name: "a blank effective date", text: `{"sections":[{"kind":"release","items":[{"title":"T","summary":"S","effective":" ","links":[{"label":"L","url":"https://fixture.test/news"}]}]}]}`},
		{name: "a link with no label", text: `{"sections":[{"kind":"release","items":[{"title":"T","summary":"S","links":[{"label":"","url":"https://fixture.test/news"}]}]}]}`},
		{name: "a link that is not https", text: `{"sections":[{"kind":"release","items":[{"title":"T","summary":"S","links":[{"label":"L","url":"http://fixture.test/news"}]}]}]}`},
		{name: "a link the sources never gave", text: `{"sections":[{"kind":"release","items":[{"title":"T","summary":"S","links":[{"label":"L","url":"https://elsewhere.test/made-up"}]}]}]}`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, err := parseAnswer(tt.text, allowedFixture)

			// Assert
			if err == nil {
				t.Fatal("parseAnswer = nil, want a refusal")
			}
		})
	}
}

func TestStripFence(t *testing.T) {
	tests := []struct {
		name, text, want string
	}{
		{name: "no fence", text: ` {"a":1} `, want: `{"a":1}`},
		{name: "a json fence", text: "```json\n{\"a\":1}\n```", want: `{"a":1}`},
		{name: "a bare fence", text: "```\n{\"a\":1}\n```", want: `{"a":1}`},
		{name: "a one-line fence is left alone", text: "```{}```", want: "```{}```"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := stripFence(tt.text)

			// Assert
			if got != tt.want {
				t.Fatalf("stripFence = %q, want %q", got, tt.want)
			}
		})
	}
}

// material is one source's new entry, as the condense tests hand it.
var material = []sourceNews{{src: feedSource, entries: []entry{{
	ID: "new", Title: "Release new", Link: "https://fixture.test/releases/new", Body: "Notes for new", At: now,
}}}}

func TestCondenseAsksSonnetUnderTheDigestsSiteWithTheMaterial(t *testing.T) {
	// Arrange
	runner := &fakeRunner{text: answerJSON}
	c := condenser{headless: runner, promptsDir: repoPromptsDir, configDir: "/accounts/default", timeout: DefaultModelTimeout}

	// Act
	_, err := c.condense(context.Background(), "yesterday to today", material, nil)

	// Assert
	if err != nil {
		t.Fatalf("condense: %v", err)
	}
	req := runner.asked()[0]
	if req.Site != Site || req.Model != headless.ModelSonnet || req.Format != headless.FormatJSON ||
		req.ConfigDir != "/accounts/default" || req.Timeout != DefaultModelTimeout {
		t.Fatalf("request = %+v, want Sonnet under %s with a JSON envelope on the default account", req, Site)
	}
	for _, want := range []string{"yesterday to today", "=== SOURCE: SDK releases", "URL: https://fixture.test/releases/new", "Notes for new", "No earlier digest is still unread."} {
		if !strings.Contains(req.Prompt, want) {
			t.Fatalf("the prompt lacks %q:\n%s", want, req.Prompt)
		}
	}
}

func TestCondenseHandsTheModelTheStillUnreadDigest(t *testing.T) {
	// Arrange
	runner := &fakeRunner{text: answerJSON}
	c := condenser{headless: runner, promptsDir: repoPromptsDir}
	carried := []*frontendv1.NewsDigestSection{{
		Kind: kinds[3].arm(),
		Items: []*frontendv1.NewsDigestItem{{
			Title:     &frontendv1.NewsDigestItemTitle{Text: "Earlier item"},
			Summary:   &frontendv1.NewsDigestItemSummary{Text: "Still unread."},
			Effective: &frontendv1.NewsDigestItemEffective{Text: "soon"},
			Links:     []*frontendv1.NewsDigestLink{{Label: "Earlier", Url: "https://fixture.test/earlier"}},
		}},
	}}

	// Act
	_, err := c.condense(context.Background(), "p", material, carried)

	// Assert
	if err != nil {
		t.Fatalf("condense: %v", err)
	}
	prompt := runner.asked()[0].Prompt
	for _, want := range []string{"KEEP every one of its items", "kind: feature", "title: Earlier item", "effective: soon", "link: Earlier <https://fixture.test/earlier>"} {
		if !strings.Contains(prompt, want) {
			t.Fatalf("the prompt lacks %q:\n%s", want, prompt)
		}
	}
}

func TestCondenseAllowsTheCarriedDigestsLinks(t *testing.T) {
	// Arrange
	runner := &fakeRunner{text: `{"sections":[{"kind":"feature","items":[{"title":"T","summary":"S","links":[{"label":"E","url":"https://fixture.test/earlier"}]}]}]}`}
	c := condenser{headless: runner, promptsDir: repoPromptsDir}
	carried := []*frontendv1.NewsDigestSection{{Kind: kinds[3].arm(), Items: []*frontendv1.NewsDigestItem{{
		Links: []*frontendv1.NewsDigestLink{{Label: "E", Url: "https://fixture.test/earlier"}},
	}}}}

	// Act
	sections, err := c.condense(context.Background(), "p", material, carried)

	// Assert
	if err != nil || len(sections) != 1 {
		t.Fatalf("condense = (%v, %v), want the carried link accepted", sections, err)
	}
}

func TestCondenseReportsAFailedCallAsAModelFailure(t *testing.T) {
	// Arrange
	runner := &fakeRunner{err: &headless.Error{Cause: headless.CauseTimeout, Detail: "took too long"}}
	c := condenser{headless: runner, promptsDir: repoPromptsDir}

	// Act
	_, err := c.condense(context.Background(), "p", material, nil)

	// Assert
	var failed *ModelFailedError
	if !errors.As(err, &failed) || !strings.HasPrefix(failed.Reason, headless.CauseTimeout) {
		t.Fatalf("condense = %v, want a model failure naming the cause", err)
	}
}

func TestCondenseReportsAMalformedAnswerAsAModelFailure(t *testing.T) {
	// Arrange
	c := condenser{headless: &fakeRunner{text: "Sure! Here is your digest."}, promptsDir: repoPromptsDir}

	// Act
	_, err := c.condense(context.Background(), "p", material, nil)

	// Assert
	var failed *ModelFailedError
	if !errors.As(err, &failed) || !strings.Contains(failed.Reason, "not a well-formed digest") {
		t.Fatalf("condense = %v, want a model failure naming the malformed answer", err)
	}
}

func TestCondenseReportsAMissingBriefAsAModelFailure(t *testing.T) {
	// Arrange
	c := condenser{headless: &fakeRunner{text: answerJSON}, promptsDir: t.TempDir()}

	// Act
	_, err := c.condense(context.Background(), "p", material, nil)

	// Assert
	var failed *ModelFailedError
	if !errors.As(err, &failed) || !strings.Contains(failed.Reason, "brief") {
		t.Fatalf("condense = %v, want a model failure naming the brief", err)
	}
}

func TestTheBriefKeepsItsPlaceholderSet(t *testing.T) {
	// Act
	brief, err := prompts.Load(repoPromptsDir, Brief)

	// Assert
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	want := map[string]bool{"period": true, "material": true, "carried": true}
	if len(brief.Placeholders) != len(want) {
		t.Fatalf("placeholders = %v, want period, material and carried", brief.Placeholders)
	}
	for _, p := range brief.Placeholders {
		if !want[p] {
			t.Fatalf("placeholders = %v, want period, material and carried", brief.Placeholders)
		}
	}
}

func TestKindTokenNamesEveryArm(t *testing.T) {
	for _, spec := range kinds {
		t.Run(spec.token, func(t *testing.T) {
			// Act
			got := kindToken(spec.arm())

			// Assert
			if got != spec.token {
				t.Fatalf("kindToken = %q, want %q", got, spec.token)
			}
		})
	}
}

func TestKindTokenPanicsOnASectionWithNoKind(t *testing.T) {
	// Arrange
	defer func() {
		if recover() == nil {
			t.Fatal("kindToken did not panic")
		}
	}()

	// Act
	kindToken(&frontendv1.NewsDigestSectionKind{})
}
