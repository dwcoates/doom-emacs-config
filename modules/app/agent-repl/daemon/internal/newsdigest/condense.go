package newsdigest

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/url"
	"strings"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/headless"
	"claude-repld/internal/prompts"
)

// The condensing call.
const (
	// Brief is the prompts-directory brief the condensing call is composed
	// from.
	Brief = "news-digest-from-sources"
	// Site is the vendor-guard site the condensing call asks under.
	Site = "news_digest"
	// Model is the model that condenses: Sonnet, the daemon's model for prose
	// a person reads closely.
	Model = headless.ModelSonnet
	// DefaultModelTimeout bounds the condensing call: Sonnet reading up to
	// every source's bounded material and writing a short JSON answer.
	DefaultModelTimeout = 5 * time.Minute
)

// ModelFailedError is a condensing call that did not produce a digest: the
// call failed, or its answer was not a well-formed digest. Never a guessed
// digest in its place.
type ModelFailedError struct {
	// Reason is the failure in the daemon's words.
	Reason string
}

func (e *ModelFailedError) Error() string { return "the model failed: " + e.Reason }

// kindSpec is one section kind: its answer token, its arm, and the heading
// the daemon composes for it. kinds is in rank order: backend first.
type kindSpec struct {
	token   string
	heading string
	arm     func() *frontendv1.NewsDigestSectionKind
}

// kinds are the section kinds, most important first.
var kinds = []kindSpec{
	{token: "backend", heading: "Affects the agent-repl backend", arm: func() *frontendv1.NewsDigestSectionKind {
		return &frontendv1.NewsDigestSectionKind{Kind: &frontendv1.NewsDigestSectionKind_Backend{Backend: &frontendv1.NewsDigestKindBackend{}}}
	}},
	{token: "deprecation", heading: "Deprecations", arm: func() *frontendv1.NewsDigestSectionKind {
		return &frontendv1.NewsDigestSectionKind{Kind: &frontendv1.NewsDigestSectionKind_Deprecation{Deprecation: &frontendv1.NewsDigestKindDeprecation{}}}
	}},
	{token: "policy", heading: "Pricing and policy", arm: func() *frontendv1.NewsDigestSectionKind {
		return &frontendv1.NewsDigestSectionKind{Kind: &frontendv1.NewsDigestSectionKind_Policy{Policy: &frontendv1.NewsDigestKindPolicy{}}}
	}},
	{token: "feature", heading: "New features", arm: func() *frontendv1.NewsDigestSectionKind {
		return &frontendv1.NewsDigestSectionKind{Kind: &frontendv1.NewsDigestSectionKind_Feature{Feature: &frontendv1.NewsDigestKindFeature{}}}
	}},
	{token: "release", heading: "Releases", arm: func() *frontendv1.NewsDigestSectionKind {
		return &frontendv1.NewsDigestSectionKind{Kind: &frontendv1.NewsDigestSectionKind_Release{Release: &frontendv1.NewsDigestKindRelease{}}}
	}},
	{token: "incident", heading: "Incidents", arm: func() *frontendv1.NewsDigestSectionKind {
		return &frontendv1.NewsDigestSectionKind{Kind: &frontendv1.NewsDigestSectionKind_Incident{Incident: &frontendv1.NewsDigestKindIncident{}}}
	}},
}

// sourceNews is one source's new material, as the model is handed it.
type sourceNews struct {
	src     Source
	entries []entry
}

// answer is the condensing call's JSON answer, decoded strictly.
type answer struct {
	Sections []answerSection `json:"sections"`
}

type answerSection struct {
	Kind  string       `json:"kind"`
	Items []answerItem `json:"items"`
}

type answerItem struct {
	Title     string       `json:"title"`
	Summary   string       `json:"summary"`
	Effective *string      `json:"effective,omitempty"`
	Links     []answerLink `json:"links"`
}

type answerLink struct {
	Label string `json:"label"`
	URL   string `json:"url"`
}

// condenser asks the model to condense new material into sections.
type condenser struct {
	headless   headless.Runner
	promptsDir string
	configDir  string
	timeout    time.Duration
}

// condense composes the brief over material (and the still-standing digest's
// sections, carried forward), asks the model, and answers the validated
// sections in rank order. Empty sections mean the model judged nothing worth
// reporting. Every failure is a *ModelFailedError.
func (c condenser) condense(ctx context.Context, period string, material []sourceNews, carried []*frontendv1.NewsDigestSection) ([]*frontendv1.NewsDigestSection, error) {
	brief, err := prompts.Load(c.promptsDir, Brief)
	if err != nil {
		return nil, &ModelFailedError{Reason: "the brief could not be read: " + err.Error()}
	}
	allowed := allowedLinks(material, carried)
	question, err := brief.Splice(map[string]string{
		"period":   period,
		"material": renderMaterial(material),
		"carried":  renderCarried(carried),
	})
	if err != nil {
		return nil, &ModelFailedError{Reason: "the brief could not be spliced: " + err.Error()}
	}
	resp, err := c.headless.Run(ctx, headless.Request{
		Site:      Site,
		Model:     Model,
		Format:    headless.FormatJSON,
		ConfigDir: c.configDir,
		Prompt:    question,
		Timeout:   c.timeout,
	})
	if err != nil {
		return nil, &ModelFailedError{Reason: headless.CauseOf(err) + ": " + err.Error()}
	}
	sections, err := parseAnswer(resp.Text, allowed)
	if err != nil {
		return nil, &ModelFailedError{Reason: "the answer was not a well-formed digest: " + err.Error()}
	}
	return sections, nil
}

// renderMaterial lays the new material out for the model: every source with
// its URL, and every entry with its date, its own link and its text.
func renderMaterial(material []sourceNews) string {
	var b strings.Builder
	for _, m := range material {
		b.WriteString(fmt.Sprintf("=== SOURCE: %s\nURL: %s\n", m.src.Name, m.src.Home))
		for _, e := range m.entries {
			b.WriteString(fmt.Sprintf("\n--- ENTRY: %s\n", e.Title))
			if !e.At.IsZero() {
				b.WriteString(fmt.Sprintf("DATE: %s\n", e.At.Format(time.RFC3339)))
			}
			b.WriteString(fmt.Sprintf("URL: %s\n%s\n", e.Link, e.Body))
		}
		b.WriteString("\n")
	}
	return strings.TrimRight(b.String(), "\n")
}

// renderCarried tells the model about the standing digest nobody has
// dismissed yet: its items are still unread, so the new digest keeps them.
func renderCarried(carried []*frontendv1.NewsDigestSection) string {
	if len(carried) == 0 {
		return "No earlier digest is still unread."
	}
	var b strings.Builder
	b.WriteString("An earlier digest is still unread. KEEP every one of its items in your answer (merge an item with a new one when they tell the same story), in its section kind, with its links:\n")
	for _, section := range carried {
		token := kindToken(section.GetKind())
		for _, item := range section.GetItems() {
			b.WriteString(fmt.Sprintf("\n- kind: %s\n  title: %s\n  summary: %s\n", token, item.GetTitle().GetText(), item.GetSummary().GetText()))
			if item.Effective != nil {
				b.WriteString(fmt.Sprintf("  effective: %s\n", item.GetEffective().GetText()))
			}
			for _, link := range item.GetLinks() {
				b.WriteString(fmt.Sprintf("  link: %s <%s>\n", link.GetLabel(), link.GetUrl()))
			}
		}
	}
	return strings.TrimRight(b.String(), "\n")
}

// allowedLinks is every URL an item may link to: each source's own page, each
// new entry's link, and every link the carried digest already holds.
func allowedLinks(material []sourceNews, carried []*frontendv1.NewsDigestSection) map[string]bool {
	allowed := map[string]bool{}
	for _, m := range material {
		allowed[m.src.Home] = true
		for _, e := range m.entries {
			allowed[e.Link] = true
		}
	}
	for _, section := range carried {
		for _, item := range section.GetItems() {
			for _, link := range item.GetLinks() {
				allowed[link.GetUrl()] = true
			}
		}
	}
	return allowed
}

// parseAnswer decodes and validates the model's answer HARD: unknown fields,
// trailing data, an unknown or repeated kind, an empty section, an item with
// no title, summary or links, a blank effective date, and a link that is not
// an absolute https URL from the allowed set each refuse the whole answer.
// The one leniency is a single enclosing markdown code fence, which carries
// no meaning of its own.
func parseAnswer(text string, allowed map[string]bool) ([]*frontendv1.NewsDigestSection, error) {
	dec := json.NewDecoder(strings.NewReader(stripFence(text)))
	dec.DisallowUnknownFields()
	var a answer
	if err := dec.Decode(&a); err != nil {
		return nil, fmt.Errorf("the json did not decode: %w", err)
	}
	if _, err := dec.Token(); !errors.Is(err, io.EOF) {
		return nil, errors.New("the answer carries data after its json object")
	}
	if a.Sections == nil {
		return nil, errors.New("the answer carries no sections list")
	}
	byToken := map[string]*frontendv1.NewsDigestSection{}
	for i, s := range a.Sections {
		spec, ok := kindByToken(s.Kind)
		if !ok {
			return nil, fmt.Errorf("section %d names the unknown kind %q", i, s.Kind)
		}
		if byToken[s.Kind] != nil {
			return nil, fmt.Errorf("the kind %q appears in more than one section", s.Kind)
		}
		if len(s.Items) == 0 {
			return nil, fmt.Errorf("the %q section has no items", s.Kind)
		}
		section := &frontendv1.NewsDigestSection{
			Heading: &frontendv1.NewsDigestSectionHeading{Text: spec.heading},
			Kind:    spec.arm(),
		}
		for j, it := range s.Items {
			item, err := validItem(it, allowed)
			if err != nil {
				return nil, fmt.Errorf("the %q section's item %d: %w", s.Kind, j, err)
			}
			section.Items = append(section.Items, item)
		}
		byToken[s.Kind] = section
	}
	var out []*frontendv1.NewsDigestSection
	for _, spec := range kinds {
		if section := byToken[spec.token]; section != nil {
			out = append(out, section)
		}
	}
	return out, nil
}

// validItem converts one answered item, refusing one that is not whole.
func validItem(it answerItem, allowed map[string]bool) (*frontendv1.NewsDigestItem, error) {
	title, summary := strings.TrimSpace(it.Title), strings.TrimSpace(it.Summary)
	switch {
	case title == "":
		return nil, errors.New("it has no title")
	case summary == "":
		return nil, errors.New("it has no summary")
	case len(it.Links) == 0:
		return nil, errors.New("it links to no source")
	}
	item := &frontendv1.NewsDigestItem{
		Title:   &frontendv1.NewsDigestItemTitle{Text: title},
		Summary: &frontendv1.NewsDigestItemSummary{Text: summary},
	}
	if it.Effective != nil {
		effective := strings.TrimSpace(*it.Effective)
		if effective == "" {
			return nil, errors.New("its effective date is blank")
		}
		item.Effective = &frontendv1.NewsDigestItemEffective{Text: effective}
	}
	for _, l := range it.Links {
		label := strings.TrimSpace(l.Label)
		if label == "" {
			return nil, fmt.Errorf("the link %q has no label", l.URL)
		}
		u, err := url.Parse(l.URL)
		if err != nil || u.Scheme != "https" || u.Host == "" {
			return nil, fmt.Errorf("the link %q is not an absolute https url", l.URL)
		}
		if !allowed[l.URL] {
			return nil, fmt.Errorf("the link %q is not one of the urls the sources gave", l.URL)
		}
		item.Links = append(item.Links, &frontendv1.NewsDigestLink{Label: label, Url: l.URL})
	}
	return item, nil
}

// stripFence removes one markdown code fence enclosing the whole text.
func stripFence(text string) string {
	t := strings.TrimSpace(text)
	if !strings.HasPrefix(t, "```") || !strings.HasSuffix(t, "```") || len(t) < 6 {
		return t
	}
	inner := strings.TrimSuffix(t, "```")
	newline := strings.IndexByte(inner, '\n')
	if newline < 0 {
		return t
	}
	return strings.TrimSpace(inner[newline+1:])
}

// kindByToken finds a kind by its answer token.
func kindByToken(token string) (kindSpec, bool) {
	for _, spec := range kinds {
		if spec.token == token {
			return spec, true
		}
	}
	return kindSpec{}, false
}

// kindToken names a section's kind arm by its answer token. A section with
// no arm is a digest this daemon never makes, so it panics.
func kindToken(k *frontendv1.NewsDigestSectionKind) string {
	switch k.GetKind().(type) {
	case *frontendv1.NewsDigestSectionKind_Backend:
		return "backend"
	case *frontendv1.NewsDigestSectionKind_Deprecation:
		return "deprecation"
	case *frontendv1.NewsDigestSectionKind_Policy:
		return "policy"
	case *frontendv1.NewsDigestSectionKind_Feature:
		return "feature"
	case *frontendv1.NewsDigestSectionKind_Release:
		return "release"
	case *frontendv1.NewsDigestSectionKind_Incident:
		return "incident"
	default:
		panic(fmt.Sprintf("newsdigest: a section carries no known kind: %v", k))
	}
}
