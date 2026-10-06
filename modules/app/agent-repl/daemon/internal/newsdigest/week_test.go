package newsdigest

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"
	"google.golang.org/protobuf/proto"

	"claude-repld/internal/headless"
	"claude-repld/internal/prompts"
	"claude-repld/internal/wsm"
)

// newsItem is a digest item titled title linking url, with an effective date
// when effective is not empty.
func newsItem(title, url, effective string) *frontendv1.NewsDigestItem {
	item := &frontendv1.NewsDigestItem{
		Title:   &frontendv1.NewsDigestItemTitle{Text: title},
		Summary: &frontendv1.NewsDigestItemSummary{Text: title + " summary."},
		Links:   []*frontendv1.NewsDigestLink{{Label: title + " link", Url: url}},
	}
	if effective != "" {
		item.Effective = &frontendv1.NewsDigestItemEffective{Text: effective}
	}
	return item
}

// risk is a marked item of the run that ended at runEnd.
func risk(item *frontendv1.NewsDigestItem, runEnd time.Time) riskItem {
	return riskItem{item: item, reason: "Breaks " + item.GetTitle().GetText() + ".", runEnd: runEnd}
}

// keep scripts the store with r kept, as a run that ended at r.runEnd kept it.
func keep(t *testing.T, s *fakeStore, r riskItem) {
	t.Helper()
	encoded, err := proto.Marshal(r.item)
	if err != nil {
		t.Fatalf("marshal: %v", err)
	}
	s.kept = append(s.kept, wsm.NewsDigestRisk{RunEnd: r.runEnd, Item: encoded, Reason: r.reason})
}

// weekOf computes the weekly section of a run ending now.
func weekOf(t *testing.T, w *world, current ...riskItem) (*frontendv1.NewsDigestWeek, error) {
	t.Helper()
	return w.digester().week(context.Background(), weekInput{
		ended: now, coversFrom: now.Add(-24 * time.Hour), historySince: now.Add(-30 * 24 * time.Hour), current: current,
	}, w.log)
}

func TestAWeekWithNothingMarkedIsTheQuietArm(t *testing.T) {
	// Arrange
	w := newWorld(t)

	// Act
	week, err := weekOf(t, w)

	// Assert
	if err != nil || week.GetQuiet().GetText() != "Nothing since last week could regress agent-repl." || week.GetHeading().GetText() != "Since last week" {
		t.Fatalf("week = (%v, %v), want the quiet arm under its heading", week, err)
	}
	if len(w.runner.asked()) != 0 {
		t.Fatalf("a quiet week asked the model: %v", w.runner.asked())
	}
}

func TestQuietText(t *testing.T) {
	windowStart := now.Add(-Week)
	tests := []struct {
		name         string
		coversFrom   time.Time
		historySince time.Time
		want         string
	}{
		{name: "a history older than the week claims the week", coversFrom: now.Add(-24 * time.Hour), historySince: windowStart.Add(-time.Hour),
			want: "Nothing since last week could regress agent-repl."},
		{name: "a history begun inside the week names its start", coversFrom: now.Add(-24 * time.Hour), historySince: now.Add(-3 * 24 * time.Hour),
			want: "Nothing since " + now.Add(-3*24*time.Hour).Local().Format("Jan 2") + " could regress agent-repl."},
		{name: "the first kept run names what it covers", coversFrom: now.Add(-24 * time.Hour),
			want: "Nothing since " + now.Add(-24*time.Hour).Local().Format("Jan 2") + " could regress agent-repl."},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := quietText(windowStart, tt.coversFrom, tt.historySince)

			// Assert
			if got != tt.want {
				t.Fatalf("quietText = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestTheWeeksWindow(t *testing.T) {
	tests := []struct {
		name   string
		age    time.Duration
		wantIn bool
	}{
		{name: "a run 6.9 days old is in the week", age: time.Duration(6.9 * float64(24*time.Hour)), wantIn: true},
		{name: "a run 7.1 days old is out of the week", age: time.Duration(7.1 * float64(24*time.Hour)), wantIn: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			w := newWorld(t)
			keep(t, w.store, risk(newsItem("Old", "https://fixture.test/old", ""), now.Add(-tt.age)))

			// Act
			week, err := weekOf(t, w)

			// Assert
			if err != nil {
				t.Fatalf("week: %v", err)
			}
			if gotIn := week.GetRisks() != nil; gotIn != tt.wantIn {
				t.Fatalf("week = %v, want the kept item in=%v", week, tt.wantIn)
			}
		})
	}
}

func TestOneRunsMarkedItemsAreToldWithoutAMergeCall(t *testing.T) {
	// Arrange
	w := newWorld(t)
	a := risk(newsItem("A", "https://fixture.test/a", ""), now)
	b := risk(newsItem("B", "https://fixture.test/b", ""), now)

	// Act
	week, err := weekOf(t, w, a, b)

	// Assert
	if err != nil || len(week.GetRisks().GetItems()) != 2 {
		t.Fatalf("week = (%v, %v), want both items", week, err)
	}
	first := week.GetRisks().GetItems()[0]
	if !proto.Equal(first.GetItem(), a.item) || first.GetReason().GetText() != a.reason {
		t.Fatalf("first = %v, want A verbatim with its reason", first)
	}
	if len(w.runner.asked()) != 0 {
		t.Fatalf("one run's items asked the model: %v", w.runner.asked())
	}
}

func TestTheSameStoryAcrossRunsIsToldOnce(t *testing.T) {
	// Arrange
	w := newWorld(t)
	keep(t, w.store, risk(newsItem("Sonnet 4 retires", "https://fixture.test/deprecations", "2026-11-01"), now.Add(-2*24*time.Hour)))
	current := risk(newsItem("Sonnet 4 retirement reminder", "https://fixture.test/news", "2026-11-01"), now)
	w.runner.weekText = `{"items":[{"members":["r1","r2"],"title":"Sonnet 4 retires","summary":"It retires.","reason":"The daemon's headless calls name it.","effective":"2026-11-01"}]}`

	// Act
	week, err := weekOf(t, w, current)

	// Assert
	if err != nil || len(week.GetRisks().GetItems()) != 1 {
		t.Fatalf("week = (%v, %v), want one merged item", week, err)
	}
	merged := week.GetRisks().GetItems()[0]
	if len(merged.GetItem().GetLinks()) != 2 || merged.GetReason().GetText() != "The daemon's headless calls name it." {
		t.Fatalf("merged = %v, want both members' links and the merged reason", merged)
	}
	asked := w.runner.asked()
	if len(asked) != 1 || asked[0].Site != WeekSite || asked[0].Model != headless.ModelSonnet ||
		!strings.Contains(asked[0].Prompt, "id: r1") || !strings.Contains(asked[0].Prompt, "id: r2") {
		t.Fatalf("asked = %+v, want one Sonnet merge call under the week's site naming both items", asked)
	}
}

func TestAFutureDatedItemKeepsItsDateThroughTheMerge(t *testing.T) {
	// Arrange
	w := newWorld(t)
	keep(t, w.store, risk(newsItem("Billing moves", "https://fixture.test/a", "early November"), now.Add(-24*time.Hour)))
	current := risk(newsItem("Billing moves (dated)", "https://fixture.test/b", "2026-11-03"), now)
	w.runner.weekText = `{"items":[{"members":["r2","r1"],"title":"Billing moves","summary":"S.","reason":"R.","effective":"2026-11-03"}]}`

	// Act
	week, err := weekOf(t, w, current)

	// Assert
	if err != nil || week.GetRisks().GetItems()[0].GetItem().GetEffective().GetText() != "2026-11-03" {
		t.Fatalf("week = (%v, %v), want the stated date kept", week, err)
	}
}

func TestAFailedMergeIsAModelFailure(t *testing.T) {
	// Arrange
	w := newWorld(t)
	keep(t, w.store, risk(newsItem("A", "https://fixture.test/a", ""), now.Add(-24*time.Hour)))
	w.runner.weekErr = &headless.Error{Cause: headless.CauseTimeout, Detail: "late"}

	// Act
	_, err := weekOf(t, w, risk(newsItem("B", "https://fixture.test/b", ""), now))

	// Assert
	var failed *ModelFailedError
	if !errors.As(err, &failed) || !strings.Contains(failed.Reason, headless.CauseTimeout) {
		t.Fatalf("week = %v, want a model failure naming the cause", err)
	}
}

func TestAMalformedMergeIsAModelFailure(t *testing.T) {
	// Arrange
	w := newWorld(t)
	keep(t, w.store, risk(newsItem("A", "https://fixture.test/a", ""), now.Add(-24*time.Hour)))
	w.runner.weekText = `{"items":[{"members":["r1"],"title":"A","summary":"S.","reason":"R."}]}`

	// Act
	_, err := weekOf(t, w, risk(newsItem("B", "https://fixture.test/b", ""), now))

	// Assert
	var failed *ModelFailedError
	if !errors.As(err, &failed) || !strings.Contains(failed.Reason, `"r2" is in no group`) {
		t.Fatalf("week = %v, want a model failure naming the dropped item", err)
	}
}

func TestAnUnreadableStoreFailsTheWeekLoudly(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.store.risksErr = errScripted

	// Act
	_, err := weekOf(t, w)

	// Assert
	var failed *ModelFailedError
	if !errors.Is(err, errScripted) || errors.As(err, &failed) {
		t.Fatalf("week = %v, want the store's failure, not a model failure", err)
	}
	if len(records(w.log, "error", opWeek)) != 1 {
		t.Fatalf("records = %v, want the store failure at ERROR", w.log.Records())
	}
}

func TestAnUndecodableKeptItemFailsTheWeekLoudly(t *testing.T) {
	// Arrange
	w := newWorld(t)
	w.store.kept = []wsm.NewsDigestRisk{{RunEnd: now.Add(-time.Hour), Item: []byte{0xff, 0xff}, Reason: "R."}}

	// Act
	_, err := weekOf(t, w)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "did not decode") {
		t.Fatalf("week = %v, want the decode failure", err)
	}
	if len(records(w.log, "error", opWeek)) != 1 {
		t.Fatalf("records = %v, want the decode failure at ERROR", w.log.Records())
	}
}

func TestSpansRuns(t *testing.T) {
	tests := []struct {
		name  string
		items []riskItem
		want  bool
	}{
		{name: "one item", items: []riskItem{{runEnd: now}}, want: false},
		{name: "two items of one run", items: []riskItem{{runEnd: now}, {runEnd: now}}, want: false},
		{name: "items of two runs", items: []riskItem{{runEnd: now.Add(-time.Hour)}, {runEnd: now}}, want: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := spansRuns(tt.items)

			// Assert
			if got != tt.want {
				t.Fatalf("spansRuns = %v, want %v", got, tt.want)
			}
		})
	}
}

// weekPair is the two items the merge-answer tests group: A states a date,
// B does not.
var weekPair = []riskItem{
	risk(newsItem("A", "https://fixture.test/a", "2026-11-01"), now.Add(-24*time.Hour)),
	risk(newsItem("B", "https://fixture.test/b", ""), now),
}

func TestParseWeekAnswerKeepsSeparateStoriesApart(t *testing.T) {
	// Arrange
	text := `{"items":[{"members":["r2"],"title":"B","summary":"S.","reason":"R."},{"members":["r1"],"title":"A","summary":"S.","reason":"R.","effective":"2026-11-01"}]}`

	// Act
	got, err := parseWeekAnswer(text, weekPair)

	// Assert
	if err != nil || len(got) != 2 || got[0].item.GetTitle().GetText() != "B" || got[1].item.GetEffective().GetText() != "2026-11-01" {
		t.Fatalf("parseWeekAnswer = (%v, %v), want B then A in the model's order", got, err)
	}
}

func TestParseWeekAnswerUnionsTheMembersLinks(t *testing.T) {
	// Arrange
	text := `{"items":[{"members":["r1","r2"],"title":"AB","summary":"S.","reason":"R.","effective":"2026-11-01"}]}`

	// Act
	got, err := parseWeekAnswer(text, weekPair)

	// Assert
	if err != nil || len(got) != 1 {
		t.Fatalf("parseWeekAnswer = (%v, %v), want one group", got, err)
	}
	links := got[0].item.GetLinks()
	if len(links) != 2 || links[0].GetUrl() != "https://fixture.test/a" || links[1].GetUrl() != "https://fixture.test/b" {
		t.Fatalf("links = %v, want both members' links in member order", links)
	}
}

func TestParseWeekAnswerRefusesAMalformedAnswer(t *testing.T) {
	tests := []struct {
		name string
		text string
	}{
		{name: "prose", text: "Merged."},
		{name: "an unknown field", text: `{"items":[],"note":"x"}`},
		{name: "trailing data", text: `{"items":[]} more`},
		{name: "an item in no group", text: `{"items":[{"members":["r1"],"title":"A","summary":"S.","reason":"R.","effective":"2026-11-01"}]}`},
		{name: "an unknown item", text: `{"items":[{"members":["r1","r2","r9"],"title":"A","summary":"S.","reason":"R.","effective":"2026-11-01"}]}`},
		{name: "an item in two groups", text: `{"items":[{"members":["r1","r2"],"title":"A","summary":"S.","reason":"R.","effective":"2026-11-01"},{"members":["r2"],"title":"B","summary":"S.","reason":"R."}]}`},
		{name: "a group with no members", text: `{"items":[{"members":["r1","r2"],"title":"A","summary":"S.","reason":"R.","effective":"2026-11-01"},{"members":[],"title":"C","summary":"S.","reason":"R."}]}`},
		{name: "a group with no title", text: `{"items":[{"members":["r1","r2"],"title":" ","summary":"S.","reason":"R.","effective":"2026-11-01"}]}`},
		{name: "a group with no summary", text: `{"items":[{"members":["r1","r2"],"title":"A","summary":"","reason":"R.","effective":"2026-11-01"}]}`},
		{name: "a group with no reason", text: `{"items":[{"members":["r1","r2"],"title":"A","summary":"S.","reason":"","effective":"2026-11-01"}]}`},
		{name: "a reason over two lines", text: `{"items":[{"members":["r1","r2"],"title":"A","summary":"S.","reason":"R.\nMore.","effective":"2026-11-01"}]}`},
		{name: "a dated member's date dropped", text: `{"items":[{"members":["r1","r2"],"title":"A","summary":"S.","reason":"R."}]}`},
		{name: "a date no member states", text: `{"items":[{"members":["r1","r2"],"title":"A","summary":"S.","reason":"R.","effective":"2027-01-01"}]}`},
		{name: "a date on a group of undated members", text: `{"items":[{"members":["r1"],"title":"A","summary":"S.","reason":"R.","effective":"2026-11-01"},{"members":["r2"],"title":"B","summary":"S.","reason":"R.","effective":"2026-11-01"}]}`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, err := parseWeekAnswer(tt.text, weekPair)

			// Assert
			if err == nil {
				t.Fatal("parseWeekAnswer = nil, want a refusal")
			}
		})
	}
}

func TestKeptHistoryKeepsEveryItemWithItsMark(t *testing.T) {
	// Arrange
	a, b := newsItem("A", "https://fixture.test/a", ""), newsItem("B", "https://fixture.test/b", "")
	sections := []*frontendv1.NewsDigestSection{{Items: []*frontendv1.NewsDigestItem{a}}, {Items: []*frontendv1.NewsDigestItem{b}}}
	from := now.Add(-24 * time.Hour)

	// Act
	h, err := keptHistory(sections, risks{b: "Breaks B."}, from, now)

	// Assert
	if err != nil || len(h.Items) != 2 || h.Items[0].Risk != "" || h.Items[1].Risk != "Breaks B." {
		t.Fatalf("history = (%+v, %v), want A unmarked then B marked", h, err)
	}
	if !h.CoversFrom.Equal(from) || !h.KeepSince.Equal(now.Add(-14*24*time.Hour)) {
		t.Fatalf("history spans %v, prunes before %v, want %v and fourteen days back", h.CoversFrom, h.KeepSince, from)
	}
	decoded := &frontendv1.NewsDigestItem{}
	if err := proto.Unmarshal(h.Items[1].Item, decoded); err != nil || !proto.Equal(decoded, b) {
		t.Fatalf("kept item = (%v, %v), want B encoded", decoded, err)
	}
}

func TestTheWeekBriefKeepsItsPlaceholderSet(t *testing.T) {
	// Act
	brief, err := prompts.Load(repoPromptsDir, WeekBrief)

	// Assert
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	if len(brief.Placeholders) != 1 || brief.Placeholders[0] != "items" {
		t.Fatalf("placeholders = %v, want items", brief.Placeholders)
	}
}
