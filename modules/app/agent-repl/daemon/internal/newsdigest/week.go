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
	"google.golang.org/protobuf/proto"

	"claude-repld/internal/dlog"
	"claude-repld/internal/headless"
	"claude-repld/internal/prompts"
	"claude-repld/internal/wsm"
)

// "Since last week": the digest of the past week's digests, holding only the
// items the condensing call marked as able to regress agent-repl, each told
// once (docs/protobuf-design/news-digest.md, Addendum).
const (
	// WeekBrief is the prompts-directory brief the week's merge call is
	// composed from.
	WeekBrief = "news-digest-week-risks"
	// WeekSite is the vendor-guard site the week's merge call asks under.
	WeekSite = "news_digest_week"
	// Week is the span the weekly section covers, back from the run's end.
	Week = 7 * 24 * time.Hour
	// Retention is how long kept items outlive their run: twice the week, so
	// a week is always whole however the cadence drifts.
	Retention = 2 * Week
	// weekHeading is the weekly section's heading (owner, 2026-10-06).
	weekHeading = "Since last week"
)

// riskItem is one item marked as a regression risk, with when its run ended.
type riskItem struct {
	item   *frontendv1.NewsDigestItem
	reason string
	runEnd time.Time
}

// weekInput is what the weekly section is computed from.
type weekInput struct {
	// ended is the run's end: the week reaches back Week from it.
	ended time.Time
	// coversFrom is the start of the span this run's digest covers.
	coversFrom time.Time
	// historySince is the start of the kept history; zero when none is kept.
	historySince time.Time
	// current are this run's marked items, in the digest's order.
	current []riskItem
}

// week computes the weekly section: the kept marked items of the runs that
// ended in the past Week, and this run's, merged so each story is told once.
// A store failure is answered as is; a merge call that fails is a
// *ModelFailedError.
func (d *Digester) week(ctx context.Context, in weekInput, log dlog.Logger) (*frontendv1.NewsDigestWeek, error) {
	windowStart := in.ended.Add(-Week)
	kept, err := d.deps.Store.NewsDigestRisksSince(ctx, windowStart)
	if err != nil {
		log.Error(opWeek, "the week's kept news digest risks could not be read", dlog.Context{"cause": err.Error()})
		return nil, fmt.Errorf("newsdigest: read the week's risks: %w", err)
	}
	all := make([]riskItem, 0, len(kept)+len(in.current))
	for i, k := range kept {
		item := &frontendv1.NewsDigestItem{}
		if err := proto.Unmarshal(k.Item, item); err != nil {
			log.Error(opWeek, "a kept news digest item did not decode", dlog.Context{
				"run_end": k.RunEnd.Format(time.RFC3339), "index": i, "cause": err.Error(),
			})
			return nil, fmt.Errorf("newsdigest: a kept item of the run ended %s did not decode: %w", k.RunEnd.Format(time.RFC3339), err)
		}
		all = append(all, riskItem{item: item, reason: k.Reason, runEnd: k.RunEnd})
	}
	all = append(all, in.current...)

	heading := &frontendv1.NewsDigestSectionHeading{Text: weekHeading}
	if len(all) == 0 {
		text := quietText(windowStart, in.coversFrom, in.historySince)
		log.Info(opWeek, "nothing in the past week could regress agent-repl", dlog.Context{"text": text})
		return &frontendv1.NewsDigestWeek{Heading: heading, Outcome: &frontendv1.NewsDigestWeek_Quiet{
			Quiet: &frontendv1.NewsDigestWeekQuiet{Text: text},
		}}, nil
	}
	merged, mergeCall := all, spansRuns(all)
	if mergeCall {
		if merged, err = d.condenser.mergeWeek(ctx, all); err != nil {
			return nil, err
		}
	}
	risks := &frontendv1.NewsDigestWeekRisks{}
	for _, r := range merged {
		risks.Items = append(risks.Items, &frontendv1.NewsDigestRiskItem{
			Item: r.item, Reason: &frontendv1.NewsDigestRiskReason{Text: r.reason},
		})
	}
	log.Info(opWeek, "the past week holds news that could regress agent-repl", dlog.Context{
		"marked": len(all), "kept": len(kept), "items": len(merged), "merge_call": mergeCall,
	})
	return &frontendv1.NewsDigestWeek{Heading: heading, Outcome: &frontendv1.NewsDigestWeek_Risks{Risks: risks}}, nil
}

// spansRuns reports whether the items come from more than one run. Items of
// one run come from one condensing answer, which already merged them; only
// items of different runs can tell one story twice.
func spansRuns(items []riskItem) bool {
	for _, r := range items[1:] {
		if !r.runEnd.Equal(items[0].runEnd) {
			return true
		}
	}
	return false
}

// quietText tells a week with nothing regressive, never claiming more than
// the kept history saw: when the history began inside the week (or this run
// is its first), it names the day it began.
func quietText(windowStart, coversFrom, historySince time.Time) string {
	began := historySince
	if began.IsZero() {
		began = coversFrom
	}
	if began.After(windowStart) {
		return "Nothing since " + began.Local().Format("Jan 2") + " could regress agent-repl."
	}
	return "Nothing since last week could regress agent-repl."
}

// keptHistory is what the run keeps of its digest's items: every item in the
// digest's order with its mark, the span it covers, and the retention prune.
func keptHistory(sections []*frontendv1.NewsDigestSection, marked risks, coversFrom, ended time.Time) (*wsm.NewsDigestHistory, error) {
	h := &wsm.NewsDigestHistory{CoversFrom: coversFrom, KeepSince: ended.Add(-Retention)}
	for _, section := range sections {
		for _, item := range section.GetItems() {
			encoded, err := proto.Marshal(item)
			if err != nil {
				return nil, fmt.Errorf("newsdigest: encode a kept item: %w", err)
			}
			h.Items = append(h.Items, wsm.NewsDigestKeptItem{Item: encoded, Risk: marked[item]})
		}
	}
	return h, nil
}

// markedItems are this run's items the model marked, in the digest's order.
func markedItems(sections []*frontendv1.NewsDigestSection, marked risks, ended time.Time) []riskItem {
	var out []riskItem
	for _, section := range sections {
		for _, item := range section.GetItems() {
			if reason, ok := marked[item]; ok {
				out = append(out, riskItem{item: item, reason: reason, runEnd: ended})
			}
		}
	}
	return out
}

// weekAnswer is the merge call's JSON answer, decoded strictly.
type weekAnswer struct {
	Items []weekGroup `json:"items"`
}

type weekGroup struct {
	Members   []string `json:"members"`
	Title     string   `json:"title"`
	Summary   string   `json:"summary"`
	Reason    string   `json:"reason"`
	Effective *string  `json:"effective,omitempty"`
}

// mergeWeek asks the model to tell each story among items once, and answers
// the merged items most important first. Every failure is a
// *ModelFailedError.
func (c condenser) mergeWeek(ctx context.Context, items []riskItem) ([]riskItem, error) {
	brief, err := prompts.Load(c.promptsDir, WeekBrief)
	if err != nil {
		return nil, &ModelFailedError{Reason: "the week's brief could not be read: " + err.Error()}
	}
	question, err := brief.Splice(map[string]string{"items": renderWeekItems(items)})
	if err != nil {
		return nil, &ModelFailedError{Reason: "the week's brief could not be spliced: " + err.Error()}
	}
	resp, err := c.headless.Run(ctx, headless.Request{
		Site:      WeekSite,
		Model:     Model,
		Format:    headless.FormatJSON,
		ConfigDir: c.configDir,
		Prompt:    question,
		Timeout:   c.timeout,
	})
	if err != nil {
		return nil, &ModelFailedError{Reason: "the week's merge: " + headless.CauseOf(err) + ": " + err.Error()}
	}
	merged, err := parseWeekAnswer(resp.Text, items)
	if err != nil {
		return nil, &ModelFailedError{Reason: "the week's merge was not well formed: " + err.Error()}
	}
	return merged, nil
}

// weekID names the i'th item for the merge call.
func weekID(i int) string { return fmt.Sprintf("r%d", i+1) }

// renderWeekItems lays the marked items out for the model, oldest run first.
func renderWeekItems(items []riskItem) string {
	var b strings.Builder
	for i, r := range items {
		b.WriteString(fmt.Sprintf("- id: %s\n  digest of: %s\n  title: %s\n  summary: %s\n  reason: %s\n",
			weekID(i), r.runEnd.Local().Format(time.RFC1123), r.item.GetTitle().GetText(), r.item.GetSummary().GetText(), r.reason))
		if r.item.Effective != nil {
			b.WriteString(fmt.Sprintf("  effective: %s\n", r.item.GetEffective().GetText()))
		}
		for _, link := range r.item.GetLinks() {
			b.WriteString(fmt.Sprintf("  link: %s <%s>\n", link.GetLabel(), link.GetUrl()))
		}
	}
	return strings.TrimRight(b.String(), "\n")
}

// parseWeekAnswer decodes and validates the merge call's answer HARD: every
// item is a member of exactly one group (none dropped, none invented, none
// told twice), each group has a title, a summary and a one-line reason, and a
// group carries an effective date exactly when a member states one, copied
// from one of its members. The links are the members' own, composed here, so
// the model can neither drop nor invent one.
func parseWeekAnswer(text string, items []riskItem) ([]riskItem, error) {
	dec := json.NewDecoder(strings.NewReader(stripFence(text)))
	dec.DisallowUnknownFields()
	var a weekAnswer
	if err := dec.Decode(&a); err != nil {
		return nil, fmt.Errorf("the json did not decode: %w", err)
	}
	if _, err := dec.Token(); !errors.Is(err, io.EOF) {
		return nil, errors.New("the answer carries data after its json object")
	}
	byID := map[string]int{}
	for i := range items {
		byID[weekID(i)] = i
	}
	grouped := map[string]bool{}
	var out []riskItem
	for g, group := range a.Items {
		if len(group.Members) == 0 {
			return nil, fmt.Errorf("group %d has no members", g)
		}
		var members []riskItem
		for _, id := range group.Members {
			i, ok := byID[id]
			if !ok {
				return nil, fmt.Errorf("group %d names the unknown item %q", g, id)
			}
			if grouped[id] {
				return nil, fmt.Errorf("the item %q is in more than one group", id)
			}
			grouped[id] = true
			members = append(members, items[i])
		}
		merged, err := mergedItem(group, members)
		if err != nil {
			return nil, fmt.Errorf("group %d: %w", g, err)
		}
		out = append(out, merged)
	}
	for i := range items {
		if !grouped[weekID(i)] {
			return nil, fmt.Errorf("the item %q is in no group", weekID(i))
		}
	}
	return out, nil
}

// mergedItem builds one group's item from the model's words and its members'
// facts.
func mergedItem(group weekGroup, members []riskItem) (riskItem, error) {
	title, summary := strings.TrimSpace(group.Title), strings.TrimSpace(group.Summary)
	switch {
	case title == "":
		return riskItem{}, errors.New("it has no title")
	case summary == "":
		return riskItem{}, errors.New("it has no summary")
	}
	reason, err := oneLine(group.Reason, "reason")
	if err != nil {
		return riskItem{}, err
	}
	item := &frontendv1.NewsDigestItem{
		Title:   &frontendv1.NewsDigestItemTitle{Text: title},
		Summary: &frontendv1.NewsDigestItemSummary{Text: summary},
	}
	stated := map[string]bool{}
	seen := map[string]bool{}
	latest := members[0].runEnd
	for _, m := range members {
		if m.item.Effective != nil {
			stated[m.item.GetEffective().GetText()] = true
		}
		for _, link := range m.item.GetLinks() {
			if !seen[link.GetUrl()] {
				seen[link.GetUrl()] = true
				item.Links = append(item.Links, &frontendv1.NewsDigestLink{Label: link.GetLabel(), Url: link.GetUrl()})
			}
		}
		if m.runEnd.After(latest) {
			latest = m.runEnd
		}
	}
	switch {
	case group.Effective == nil && len(stated) > 0:
		return riskItem{}, errors.New("its members state an effective date and it carries none")
	case group.Effective != nil && !stated[strings.TrimSpace(*group.Effective)]:
		return riskItem{}, fmt.Errorf("its effective date %q is none of its members'", *group.Effective)
	case group.Effective != nil:
		item.Effective = &frontendv1.NewsDigestItemEffective{Text: strings.TrimSpace(*group.Effective)}
	}
	return riskItem{item: item, reason: reason, runEnd: latest}, nil
}
