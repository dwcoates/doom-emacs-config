package newsdigest

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"
	"google.golang.org/protobuf/proto"

	"agent-repld/internal/headless"
	"agent-repld/internal/prompts"
)

// ipoGuessJSON is one well-formed company guess.
const ipoGuessJSON = `{"estimate":"Q2 2027","basis":"B.","links":[{"label":"L","url":"https://press.test/a"}]}`

// ipoCondenser is a condenser over runner and the checked-in brief.
func ipoCondenser(runner *fakeRunner) condenser {
	return condenser{headless: runner, promptsDir: repoPromptsDir, configDir: "/accounts/default", timeout: DefaultModelTimeout}
}

// wholeIpo is a whole outlook, as a digest carries it.
func wholeIpo(anthropicEstimate string) *frontendv1.NewsDigestIpoOutlook {
	guess := func(company, estimate string) *frontendv1.NewsDigestIpoGuess {
		return &frontendv1.NewsDigestIpoGuess{
			Company:  &frontendv1.NewsDigestIpoCompany{Text: company},
			Estimate: &frontendv1.NewsDigestIpoEstimate{Text: estimate},
			Basis:    &frontendv1.NewsDigestIpoBasis{Text: company + " basis."},
			Links:    []*frontendv1.NewsDigestLink{{Label: "Report", Url: "https://press.test/" + company}},
		}
	}
	return &frontendv1.NewsDigestIpoOutlook{
		Heading:   &frontendv1.NewsDigestSectionHeading{Text: ipoHeading},
		Anthropic: guess(ipoAnthropic, anthropicEstimate),
		Openai:    guess(ipoOpenAI, "late 2027"),
	}
}

func TestParseIpoAnswerNamesBothCompaniesUnderTheDaemonsHeading(t *testing.T) {
	// Act
	got, err := parseIpoAnswer(ipoJSON)

	// Assert
	if err != nil {
		t.Fatalf("parseIpoAnswer: %v", err)
	}
	a, o := got.GetAnthropic(), got.GetOpenai()
	if got.GetHeading().GetText() != "IPO outlook" ||
		a.GetCompany().GetText() != "Anthropic" || a.GetEstimate().GetText() != "Q2 2027" ||
		a.GetBasis().GetText() != "Banks were hired in September." || a.GetLinks()[0].GetUrl() != "https://press.test/anthropic" ||
		o.GetCompany().GetText() != "OpenAI" || o.GetEstimate().GetText() != "late 2027" {
		t.Fatalf("outlook = %v, want each company's guess under the daemon's heading", got)
	}
}

func TestParseIpoAnswerTrimsTheModelsWords(t *testing.T) {
	// Arrange
	text := `{"anthropic":{"estimate":"  Q2 2027 ","basis":" B. ","links":[{"label":" L ","url":"https://press.test/a"}]},"openai":` + ipoGuessJSON + `}`

	// Act
	got, err := parseIpoAnswer(text)

	// Assert
	if err != nil {
		t.Fatalf("parseIpoAnswer: %v", err)
	}
	a := got.GetAnthropic()
	if a.GetEstimate().GetText() != "Q2 2027" || a.GetBasis().GetText() != "B." || a.GetLinks()[0].GetLabel() != "L" {
		t.Fatalf("guess = %v, want its words trimmed", a)
	}
}

func TestParseIpoAnswerAcceptsOneEnclosingCodeFence(t *testing.T) {
	// Act
	_, err := parseIpoAnswer("```json\n" + ipoJSON + "\n```")

	// Assert
	if err != nil {
		t.Fatalf("parseIpoAnswer = %v, want the fenced answer read", err)
	}
}

func TestParseIpoAnswerRefusesAMalformedAnswer(t *testing.T) {
	tests := []struct {
		name string
		text string
		want string
	}{
		{name: "prose", text: "Probably next year.", want: "did not decode"},
		{name: "an unknown field", text: `{"anthropic":` + ipoGuessJSON + `,"openai":` + ipoGuessJSON + `,"xai":{}}`, want: "did not decode"},
		{name: "trailing data", text: `{"anthropic":` + ipoGuessJSON + `,"openai":` + ipoGuessJSON + `} more`, want: "after its json object"},
		{name: "no anthropic guess", text: `{"openai":` + ipoGuessJSON + `}`, want: "no Anthropic guess"},
		{name: "no openai guess", text: `{"anthropic":` + ipoGuessJSON + `}`, want: "no OpenAI guess"},
		{name: "a blank estimate", text: `{"anthropic":{"estimate":" ","basis":"B.","links":[{"label":"L","url":"https://press.test/a"}]},"openai":` + ipoGuessJSON + `}`, want: "estimate is blank"},
		{name: "an estimate over two lines", text: `{"anthropic":{"estimate":"Q2\n2027","basis":"B.","links":[{"label":"L","url":"https://press.test/a"}]},"openai":` + ipoGuessJSON + `}`, want: "more than one line"},
		{name: "a blank basis", text: `{"anthropic":{"estimate":"Q2 2027","basis":"","links":[{"label":"L","url":"https://press.test/a"}]},"openai":` + ipoGuessJSON + `}`, want: "no basis"},
		{name: "no links", text: `{"anthropic":` + ipoGuessJSON + `,"openai":{"estimate":"2027","basis":"B.","links":[]}}`, want: "links to no source"},
		{name: "a link with no label", text: `{"anthropic":{"estimate":"Q2 2027","basis":"B.","links":[{"label":"","url":"https://press.test/a"}]},"openai":` + ipoGuessJSON + `}`, want: "has no label"},
		{name: "a link that is not https", text: `{"anthropic":` + ipoGuessJSON + `,"openai":{"estimate":"2027","basis":"B.","links":[{"label":"L","url":"http://press.test/o"}]}}`, want: "not an absolute https url"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, err := parseIpoAnswer(tt.text)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("parseIpoAnswer = %v, want a refusal naming %q", err, tt.want)
			}
		})
	}
}

func TestIpoOutlookAsksSonnetToSearchTheWebUnderItsSite(t *testing.T) {
	// Arrange
	runner := &fakeRunner{}

	// Act
	_, err := ipoCondenser(runner).ipoOutlook(context.Background(), now, nil)

	// Assert
	if err != nil {
		t.Fatalf("ipoOutlook: %v", err)
	}
	req := runner.asked()[0]
	if req.Site != IpoSite || req.Model != headless.ModelSonnet || req.Format != headless.FormatJSON ||
		req.ConfigDir != "/accounts/default" || req.Timeout != DefaultModelTimeout ||
		len(req.Tools) != 1 || req.Tools[0] != "WebSearch" {
		t.Fatalf("request = %+v, want Sonnet under %s with WebSearch alone, a JSON envelope, on the default account", req, IpoSite)
	}
	for _, want := range []string{now.Local().Format("January 2, 2006"), "No earlier outlook exists"} {
		if !strings.Contains(req.Prompt, want) {
			t.Fatalf("the prompt lacks %q:\n%s", want, req.Prompt)
		}
	}
}

func TestIpoOutlookHandsTheModelThePriorOutlook(t *testing.T) {
	// Arrange
	runner := &fakeRunner{}

	// Act
	_, err := ipoCondenser(runner).ipoOutlook(context.Background(), now, wholeIpo("H1 2027"))

	// Assert
	if err != nil {
		t.Fatalf("ipoOutlook: %v", err)
	}
	prompt := runner.asked()[0].Prompt
	for _, want := range []string{"company: Anthropic\n  estimate: H1 2027\n  basis: Anthropic basis.", "company: OpenAI\n  estimate: late 2027", "link: Report <https://press.test/OpenAI>"} {
		if !strings.Contains(prompt, want) {
			t.Fatalf("the prompt lacks %q:\n%s", want, prompt)
		}
	}
}

func TestIpoOutlookReportsEveryFailureAsAModelFailure(t *testing.T) {
	tests := []struct {
		name       string
		runner     *fakeRunner
		promptsDir string
		want       string
	}{
		{name: "a failed call", runner: &fakeRunner{ipoErr: &headless.Error{Cause: headless.CauseTimeout, Detail: "5m"}}, promptsDir: repoPromptsDir, want: "the IPO outlook: timeout"},
		{name: "a malformed answer", runner: &fakeRunner{ipoText: `{"anthropic":` + ipoGuessJSON + `}`}, promptsDir: repoPromptsDir, want: "not well formed"},
		{name: "a missing brief", runner: &fakeRunner{}, promptsDir: t.TempDir(), want: "brief could not be read"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			c := condenser{headless: tt.runner, promptsDir: tt.promptsDir}

			// Act
			_, err := c.ipoOutlook(context.Background(), now, nil)

			// Assert
			var failed *ModelFailedError
			if !errors.As(err, &failed) || !strings.Contains(failed.Reason, tt.want) {
				t.Fatalf("ipoOutlook = %v, want a model failure naming %q", err, tt.want)
			}
		})
	}
}

func TestTheIpoBriefKeepsItsPlaceholderSet(t *testing.T) {
	// Act
	brief, err := prompts.Load(repoPromptsDir, IpoBrief)

	// Assert
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	if len(brief.Placeholders) != 2 || !((brief.Placeholders[0] == "today" && brief.Placeholders[1] == "prior") ||
		(brief.Placeholders[0] == "prior" && brief.Placeholders[1] == "today")) {
		t.Fatalf("placeholders = %v, want today and prior", brief.Placeholders)
	}
}

func TestDecodeStandingRefusesAnIpoOutlookThatIsNotWhole(t *testing.T) {
	tests := []struct {
		name   string
		mutate func(*frontendv1.NewsDigestIpoOutlook)
		want   string
	}{
		{name: "no anthropic guess", mutate: func(o *frontendv1.NewsDigestIpoOutlook) { o.Anthropic = nil }, want: "no Anthropic guess"},
		{name: "no openai guess", mutate: func(o *frontendv1.NewsDigestIpoOutlook) { o.Openai = nil }, want: "no OpenAI guess"},
		{name: "no estimate", mutate: func(o *frontendv1.NewsDigestIpoOutlook) { o.Anthropic.Estimate = nil }, want: "no estimate"},
		{name: "no basis", mutate: func(o *frontendv1.NewsDigestIpoOutlook) { o.Openai.Basis = nil }, want: "no basis"},
		{name: "no links", mutate: func(o *frontendv1.NewsDigestIpoOutlook) { o.Openai.Links = nil }, want: "no links"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			ipo := wholeIpo("Q2 2027")
			tt.mutate(ipo)
			stored := overlayWithIpo(t, ipo)

			// Act
			_, err := decodeStanding(stored)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("decodeStanding = %v, want a refusal naming %q", err, tt.want)
			}
		})
	}
}

func TestDecodeStandingKeepsAWholeIpoOutlook(t *testing.T) {
	// Arrange
	stored := overlayWithIpo(t, wholeIpo("Q2 2027"))

	// Act
	got, err := decodeStanding(stored)

	// Assert
	if err != nil || got.GetIpo().GetAnthropic().GetEstimate().GetText() != "Q2 2027" {
		t.Fatalf("decodeStanding = (%v, %v), want the outlook kept", got.GetIpo(), err)
	}
}

func TestDecodeStandingReadsADigestMadeBeforeTheOutlookExisted(t *testing.T) {
	// Act
	got, err := decodeStanding(encodedOverlay(t, "digest-old", now.UnixMilli()))

	// Assert
	if err != nil || got.GetIpo() != nil {
		t.Fatalf("decodeStanding = (%v, %v), want the digest read with no outlook", got, err)
	}
}

// overlayWithIpo is a stored overlay carrying ipo.
func overlayWithIpo(t *testing.T, ipo *frontendv1.NewsDigestIpoOutlook) []byte {
	t.Helper()
	overlay := &frontendv1.NewsDigestOverlay{}
	if err := proto.Unmarshal(encodedOverlay(t, "digest-1", now.Add(-time.Hour).UnixMilli()), overlay); err != nil {
		t.Fatalf("unmarshal: %v", err)
	}
	overlay.Ipo = ipo
	encoded, err := proto.Marshal(overlay)
	if err != nil {
		t.Fatalf("marshal: %v", err)
	}
	return encoded
}
