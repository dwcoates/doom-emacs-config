package newsdigest

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"strings"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/headless"
	"claude-repld/internal/prompts"
)

// "IPO outlook": the best guess at when Anthropic and OpenAI go public, drawn
// first in every digest (docs/protobuf-design/news-digest.md, Addendum).
const (
	// IpoBrief is the prompts-directory brief the outlook call is composed
	// from.
	IpoBrief = "news-digest-ipo-outlook"
	// IpoSite is the vendor-guard site the outlook call asks under.
	IpoSite = "news_digest_ipo"
	// IpoTool is the one CLI tool the outlook call may use: the watched
	// sources carry no IPO reporting, so the model reads the web for it.
	IpoTool = "WebSearch"
	// ipoHeading is the outlook's heading (owner, 2026-10-11).
	ipoHeading = "IPO outlook"
)

// The companies, by the names the outlook draws.
const (
	ipoAnthropic = "Anthropic"
	ipoOpenAI    = "OpenAI"
)

// ipoAnswer is the outlook call's JSON answer, decoded strictly.
type ipoAnswer struct {
	Anthropic *ipoAnswerGuess `json:"anthropic"`
	OpenAI    *ipoAnswerGuess `json:"openai"`
}

type ipoAnswerGuess struct {
	Estimate string       `json:"estimate"`
	Basis    string       `json:"basis"`
	Links    []answerLink `json:"links"`
}

// ipoOutlook asks the model, searching the web, for each company's IPO guess
// as of today, handing it the previous digest's outlook so a guess moves only
// when the reporting does. Every failure is a *ModelFailedError.
func (c condenser) ipoOutlook(ctx context.Context, today time.Time, prior *frontendv1.NewsDigestIpoOutlook) (*frontendv1.NewsDigestIpoOutlook, error) {
	brief, err := prompts.Load(c.promptsDir, IpoBrief)
	if err != nil {
		return nil, &ModelFailedError{Reason: "the IPO outlook's brief could not be read: " + err.Error()}
	}
	question, err := brief.Splice(map[string]string{
		"today": today.Local().Format("Monday, January 2, 2006"),
		"prior": renderPriorIpo(prior),
	})
	if err != nil {
		return nil, &ModelFailedError{Reason: "the IPO outlook's brief could not be spliced: " + err.Error()}
	}
	resp, err := c.headless.Run(ctx, headless.Request{
		Site:      IpoSite,
		Model:     Model,
		Format:    headless.FormatJSON,
		ConfigDir: c.configDir,
		Prompt:    question,
		Timeout:   c.timeout,
		Tools:     []string{IpoTool},
	})
	if err != nil {
		return nil, &ModelFailedError{Reason: "the IPO outlook: " + headless.CauseOf(err) + ": " + err.Error()}
	}
	outlook, err := parseIpoAnswer(resp.Text)
	if err != nil {
		return nil, &ModelFailedError{Reason: "the IPO outlook was not well formed: " + err.Error()}
	}
	return outlook, nil
}

// renderPriorIpo tells the model the previous digest's outlook, or that there
// is none.
func renderPriorIpo(prior *frontendv1.NewsDigestIpoOutlook) string {
	if prior == nil {
		return "No earlier outlook exists: this is the first."
	}
	var b strings.Builder
	b.WriteString("The previous digest's outlook is below. Keep a guess unless newer reporting moves it, and say in the basis what moved it when it does:\n")
	for _, g := range []*frontendv1.NewsDigestIpoGuess{prior.GetAnthropic(), prior.GetOpenai()} {
		b.WriteString(fmt.Sprintf("\n- company: %s\n  estimate: %s\n  basis: %s\n",
			g.GetCompany().GetText(), g.GetEstimate().GetText(), g.GetBasis().GetText()))
		for _, link := range g.GetLinks() {
			b.WriteString(fmt.Sprintf("  link: %s <%s>\n", link.GetLabel(), link.GetUrl()))
		}
	}
	return strings.TrimRight(b.String(), "\n")
}

// parseIpoAnswer decodes and validates the outlook call's answer HARD: unknown
// fields, trailing data, a missing company, a blank or multi-line estimate, a
// blank basis, no links, and a link validLink refuses each refuse the whole
// answer. The links come from the model's own search, so they are checked for
// shape, not against a set. The one leniency is a single enclosing markdown
// code fence.
func parseIpoAnswer(text string) (*frontendv1.NewsDigestIpoOutlook, error) {
	dec := json.NewDecoder(strings.NewReader(stripFence(text)))
	dec.DisallowUnknownFields()
	var a ipoAnswer
	if err := dec.Decode(&a); err != nil {
		return nil, fmt.Errorf("the json did not decode: %w", err)
	}
	if _, err := dec.Token(); !errors.Is(err, io.EOF) {
		return nil, errors.New("the answer carries data after its json object")
	}
	anthropic, err := validIpoGuess(ipoAnthropic, a.Anthropic)
	if err != nil {
		return nil, err
	}
	openai, err := validIpoGuess(ipoOpenAI, a.OpenAI)
	if err != nil {
		return nil, err
	}
	return &frontendv1.NewsDigestIpoOutlook{
		Heading:   &frontendv1.NewsDigestSectionHeading{Text: ipoHeading},
		Anthropic: anthropic,
		Openai:    openai,
	}, nil
}

// validIpoGuess converts one company's answered guess, refusing one that is
// absent or not whole. company names it, in the refusal and on the wire.
func validIpoGuess(company string, g *ipoAnswerGuess) (*frontendv1.NewsDigestIpoGuess, error) {
	if g == nil {
		return nil, fmt.Errorf("the answer carries no %s guess", company)
	}
	estimate, err := oneLine(g.Estimate, "estimate")
	if err != nil {
		return nil, fmt.Errorf("the %s guess: %w", company, err)
	}
	basis := strings.TrimSpace(g.Basis)
	switch {
	case basis == "":
		return nil, fmt.Errorf("the %s guess has no basis", company)
	case len(g.Links) == 0:
		return nil, fmt.Errorf("the %s guess links to no source", company)
	}
	guess := &frontendv1.NewsDigestIpoGuess{
		Company:  &frontendv1.NewsDigestIpoCompany{Text: company},
		Estimate: &frontendv1.NewsDigestIpoEstimate{Text: estimate},
		Basis:    &frontendv1.NewsDigestIpoBasis{Text: basis},
	}
	for _, l := range g.Links {
		link, err := validLink(l)
		if err != nil {
			return nil, fmt.Errorf("the %s guess: %w", company, err)
		}
		guess.Links = append(guess.Links, link)
	}
	return guess, nil
}

// checkStoredIpo refuses a stored outlook that is not whole: both guesses,
// each with its estimate, basis and links. A digest made before the outlook
// existed carries none, which is not this function's to judge.
func checkStoredIpo(ipo *frontendv1.NewsDigestIpoOutlook) error {
	for _, g := range []struct {
		company string
		guess   *frontendv1.NewsDigestIpoGuess
	}{{ipoAnthropic, ipo.GetAnthropic()}, {ipoOpenAI, ipo.GetOpenai()}} {
		switch {
		case g.guess == nil:
			return fmt.Errorf("newsdigest: the stored overlay's IPO outlook carries no %s guess", g.company)
		case g.guess.GetEstimate().GetText() == "":
			return fmt.Errorf("newsdigest: the stored overlay's %s IPO guess has no estimate", g.company)
		case g.guess.GetBasis().GetText() == "":
			return fmt.Errorf("newsdigest: the stored overlay's %s IPO guess has no basis", g.company)
		case len(g.guess.GetLinks()) == 0:
			return fmt.Errorf("newsdigest: the stored overlay's %s IPO guess has no links", g.company)
		}
	}
	return nil
}
