package chat

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/account"
	"agent-repld/internal/dlog"
	"agent-repld/internal/headless"
	"agent-repld/internal/ids"
	"agent-repld/internal/prompts"
)

// A QUESTION'S ABBREVIATION: the rule, the headless call, and the service's
// delivery of it to the node, whichever finishes first.

const longQuestion = "Why does fusion in a star stop once it reaches iron?"

func TestNeedsAbbreviation(t *testing.T) {
	cases := []struct {
		question string
		want     bool
	}{
		{"What are quarks?", false},
		{"one two three four five six", false},
		{"one two three four five six seven", true},
		{strings.Repeat("x", AbbreviationMaxChars), false},
		{strings.Repeat("x", AbbreviationMaxChars+1), true},
		{"two\nlines", true},
		{"  padded   words  ", false},
	}
	for _, tc := range cases {
		t.Run(tc.question, func(t *testing.T) {
			// Act
			got := NeedsAbbreviation(tc.question)

			// Assert
			if got != tc.want {
				t.Fatalf("NeedsAbbreviation(%q) = %v, want %v", tc.question, got, tc.want)
			}
		})
	}
}

func TestValidateAbbreviation(t *testing.T) {
	cases := []struct {
		answer string
		want   string
		ok     bool
	}{
		{"Why fusion stops at iron", "Why fusion stops at iron", true},
		{`"Quoted gist."`, "Quoted gist", true},
		{"  spaced   out  ", "spaced out", true},
		{"Photosynthesis", "Photosynthesis", true},
		{"", "", false},
		{"one two three four five six seven", "", false},
		{strings.Repeat("x", AbbreviationMaxChars+1), "", false},
		{"two\nlines", "", false},
	}
	for _, tc := range cases {
		t.Run(tc.answer, func(t *testing.T) {
			// Act
			got, err := ValidateAbbreviation(tc.answer)

			// Assert
			if (err == nil) != tc.ok || got != tc.want {
				t.Fatalf("ValidateAbbreviation(%q) = %q, %v; want %q, ok %v", tc.answer, got, err, tc.want, tc.ok)
			}
		})
	}
}

// scriptedRunner answers each headless Run with the next answer.
type scriptedRunner struct {
	answers []string
	calls   []headless.Request
}

func (r *scriptedRunner) Bin() string { return "scripted" }

func (r *scriptedRunner) Run(_ context.Context, req headless.Request) (headless.Response, error) {
	r.calls = append(r.calls, req)
	a := r.answers[0]
	if len(r.answers) > 1 {
		r.answers = r.answers[1:]
	}
	return headless.Response{Text: a, Duration: time.Millisecond}, nil
}

type configDirsFunc func(context.Context, ids.WorkspaceID) (string, error)

func (f configDirsFunc) ConfigDirFor(ctx context.Context, ws ids.WorkspaceID) (string, error) {
	return f(ctx, ws)
}

func abbreviationBrief(string, string) (prompts.Prompt, error) {
	return prompts.Prompt{Name: BriefQuestionAbbreviation, Body: "abbreviate: {{question}}", Placeholders: []string{"question"}}, nil
}

func TestHeadlessAbbreviator(t *testing.T) {
	acct := configDirsFunc(func(context.Context, ids.WorkspaceID) (string, error) { return "/acct", nil })
	cases := []struct {
		name      string
		answers   []string
		load      func(string, string) (prompts.Prompt, error)
		dirs      account.WorkspaceConfigDirs
		want      string
		wantErr   string
		wantCalls int
	}{
		{name: "a valid answer", answers: []string{"Fusion at iron"}, load: abbreviationBrief, dirs: acct, want: "Fusion at iron", wantCalls: 1},
		{name: "an invalid answer is asked again", answers: []string{strings.Repeat("word ", 9), "Fusion at iron"}, load: abbreviationBrief, dirs: acct, want: "Fusion at iron", wantCalls: 2},
		{name: "no valid answer", answers: []string{strings.Repeat("word ", 9)}, load: abbreviationBrief, dirs: acct, wantErr: headless.CauseInvalidAnswer, wantCalls: 2},
		{name: "an unreadable brief", load: func(string, string) (prompts.Prompt, error) { return prompts.Prompt{}, errors.New("gone") }, dirs: acct, wantErr: "unreadable"},
		{name: "no account root", load: abbreviationBrief, dirs: configDirsFunc(func(context.Context, ids.WorkspaceID) (string, error) { return "", errors.New("forgotten") }), wantErr: "no account root"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			runner := &scriptedRunner{answers: tc.answers}
			a := HeadlessAbbreviator{Headless: runner, ConfigDirs: tc.dirs, PromptsDir: "/prompts", Load: tc.load}

			// Act
			got, err := a.Abbreviate(context.Background(), dlog.NewTestLogger(), thr, longQuestion)

			// Assert
			if len(runner.calls) != tc.wantCalls {
				t.Fatalf("calls = %d, want %d", len(runner.calls), tc.wantCalls)
			}
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("err = %v, want one naming %q", err, tc.wantErr)
				}
				return
			}
			if err != nil || got != tc.want {
				t.Fatalf("Abbreviate = %q, %v; want %q", got, err, tc.want)
			}
			req := runner.calls[0]
			if req.Site != AbbreviationSite || req.Model != headless.ModelHaiku || req.ConfigDir != "/acct" || req.Prompt != "abbreviate: "+longQuestion {
				t.Fatalf("request = %+v, want the brief spliced, under the thread's account", req)
			}
		})
	}
}

// answer runs one thread question from asked to recorded.
func (f *fixture) answer(turn ids.TurnID, question, parent string) {
	f.t.Helper()
	f.turns.ask(turn, question, parent, "")
	if err := f.svc.Asked(context.Background(), thr, nil); err != nil {
		f.t.Fatal(err)
	}
	f.svc.TurnAnswered(thr, turn, "an answer")
	f.turns.close()
	f.svc.TurnEnded(thr, turn)
}

// rowLabel is the outline row of NODE's label.
func (f *fixture) rowLabel(node string) *frontendv1.RosterThreadOutlineLabel {
	f.t.Helper()
	for _, r := range f.outline().GetRows() {
		if r.GetNode().GetId() == node {
			return r.GetLabel()
		}
	}
	f.t.Fatalf("no outline row for %s", node)
	return nil
}

func TestAnAbbreviationMintedBeforeTheAnswerRidesTheRecording(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.open()

	// Act
	f.answer("turn-1", longQuestion, "")

	// Assert
	if len(f.zettel.recordedAbbreviations) != 1 || f.zettel.recordedAbbreviations[0] == nil || *f.zettel.recordedAbbreviations[0] != "short: Why" {
		t.Fatalf("recorded abbreviations = %v, want the minted one carried", f.zettel.recordedAbbreviations)
	}
	if len(f.zettel.set) != 0 {
		t.Fatalf("set = %v, want none: the abbreviation rode the recording", f.zettel.set)
	}
	if got := f.rowLabel("n1"); got.GetText() != "short: Why" || got.GetQuestion() != longQuestion {
		t.Fatalf("label = %v, want the abbreviation drawn and the question whole", got)
	}
}

func TestAnAbbreviationMintedAfterTheRecordingIsSetOnTheNode(t *testing.T) {
	// Arrange: the abbreviation job is held back until the node exists.
	f := newFixture(t)
	f.open()
	f.deferJobs()
	f.turns.ask("turn-1", longQuestion, "", "")
	if err := f.svc.Asked(context.Background(), thr, nil); err != nil {
		t.Fatal(err)
	}
	abbreviate := f.jobs[0]
	f.svc.TurnAnswered(thr, "turn-1", "an answer")
	record := f.jobs[1]
	record()

	// Act
	abbreviate()

	// Assert
	if f.zettel.recordedAbbreviations[0] != nil {
		t.Fatalf("recorded abbreviation = %q, want none yet", *f.zettel.recordedAbbreviations[0])
	}
	if !equal(f.zettel.set, []string{"n1=short: Why"}) {
		t.Fatalf("set = %v, want the abbreviation set on n1", f.zettel.set)
	}
	if got := f.rowLabel("n1").GetText(); got != "short: Why" {
		t.Fatalf("label = %q, want the abbreviation drawn once set", got)
	}
}

func TestAnAbbreviationMintedWhileTheRecordingIsInFlightIsSetOnTheNode(t *testing.T) {
	// Arrange: the abbreviation finishes inside RecordExchange, after the
	// recording took nothing.
	f := newFixture(t)
	f.open()
	f.deferJobs()
	f.turns.ask("turn-1", longQuestion, "", "")
	if err := f.svc.Asked(context.Background(), thr, nil); err != nil {
		t.Fatal(err)
	}
	abbreviate := f.jobs[0]
	f.zettel.onRecord = func() { abbreviate() }
	f.svc.TurnAnswered(thr, "turn-1", "an answer")
	record := f.jobs[1]

	// Act
	record()

	// Assert
	if f.zettel.recordedAbbreviations[0] != nil {
		t.Fatalf("recorded abbreviation = %q, want none: it was minted mid-recording", *f.zettel.recordedAbbreviations[0])
	}
	if !equal(f.zettel.set, []string{"n1=short: Why"}) {
		t.Fatalf("set = %v, want the held abbreviation set once the node exists", f.zettel.set)
	}
	if got := f.rowLabel("n1").GetText(); got != "short: Why" {
		t.Fatalf("label = %q, want the abbreviation drawn", got)
	}
}

func TestAShortQuestionIsItsOwnAbbreviation(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.open()

	// Act
	f.answer("turn-1", "What are quarks?", "")

	// Assert
	if len(f.abbrev.asked) != 0 || len(f.zettel.set) != 0 || f.zettel.recordedAbbreviations[0] != nil {
		t.Fatalf("asked = %v, set = %v; want no abbreviation for a short question", f.abbrev.asked, f.zettel.set)
	}
	if got := f.rowLabel("n1"); got.GetText() != "What are quarks?" || got.GetQuestion() != "What are quarks?" {
		t.Fatalf("label = %v, want the question drawn whole", got)
	}
}

func TestALoadedNodeWithoutAnAbbreviationIsAbbreviated(t *testing.T) {
	// Arrange: a node a stopped daemon never abbreviated.
	f := newFixture(t)
	id := f.zettel.seed("", 0)
	f.zettel.nodes[0].Question.Text = longQuestion

	// Act
	f.open()

	// Assert
	if !equal(f.abbrev.asked, []string{longQuestion}) || !equal(f.zettel.set, []string{id + "=short: Why"}) {
		t.Fatalf("asked = %v, set = %v; want the stored node abbreviated", f.abbrev.asked, f.zettel.set)
	}
}

func TestAQuestionIsAbbreviatedOnceAcrossRefreshes(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.open()
	f.turns.ask("turn-1", longQuestion, "", "")

	// Act: every edge that re-reads the questions in flight.
	for range 3 {
		if err := f.svc.Asked(context.Background(), thr, nil); err != nil {
			t.Fatal(err)
		}
	}
	f.svc.TurnAnswered(thr, "turn-1", "an answer")

	// Assert
	if len(f.abbrev.asked) != 1 {
		t.Fatalf("asked = %v, want the question asked once", f.abbrev.asked)
	}
}

func TestAFailedAbbreviationIsRecordedAndTheQuestionDrawnWhole(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.abbrev.err = errors.New("the model is down")
	f.open()

	// Act
	f.answer("turn-1", longQuestion, "")

	// Assert
	r, ok := dlog.FindRecord(f.log.Records(), dlog.RecordQuery{Level: "error", Operation: opAbbreviate})
	if !ok || r.Context["thread"] != string(thr) || r.Context["turn"] != "turn-1" || r.Context["cause"] != "the model is down" {
		t.Fatalf("record = %+v, found %v; want the failure recorded with the thread, turn and cause", r, ok)
	}
	if got := f.rowLabel("n1").GetText(); got != longQuestion {
		t.Fatalf("label = %q, want the question drawn whole", got)
	}
	if len(f.abbrev.asked) != 1 || len(f.zettel.set) != 0 {
		t.Fatalf("asked = %v, set = %v; want one failed ask and nothing set", f.abbrev.asked, f.zettel.set)
	}
}

func TestAnAbbreviationZettelWillNotSetIsRecorded(t *testing.T) {
	// Arrange: a stored node, and zettel refusing the set.
	f := newFixture(t)
	f.zettel.seed("", 0)
	f.zettel.nodes[0].Question.Text = longQuestion
	f.zettel.setErr = errors.New("zettel is down")

	// Act
	f.open()

	// Assert
	r, ok := dlog.FindRecord(f.log.Records(), dlog.RecordQuery{Level: "error", Operation: opAbbreviate})
	if !ok || r.Context["node"] != "n1" || r.Context["abbreviation"] != "short: Why" || r.Context["cause"] != "zettel is down" {
		t.Fatalf("record = %+v, found %v; want the refused set recorded with the node, abbreviation and cause", r, ok)
	}
	if got := f.rowLabel("n1").GetText(); got != longQuestion {
		t.Fatalf("label = %q, want the question drawn whole", got)
	}
}

func TestAFailedRecordingKeepsTheAbbreviationForTheNextRecording(t *testing.T) {
	// Arrange: the first recording fails after taking the abbreviation.
	f := newFixture(t)
	f.open()
	f.zettel.recordErr = errors.New("zettel is down")
	f.answer("turn-1", longQuestion, "")
	f.zettel.recordErr = nil

	// Act: the same turn's answer is learned again (a replay).
	f.svc.TurnAnswered(thr, "turn-1", "an answer")

	// Assert
	last := f.zettel.recordedAbbreviations[len(f.zettel.recordedAbbreviations)-1]
	if last == nil || *last != "short: Why" {
		t.Fatalf("recorded abbreviations = %v, want the held abbreviation carried by the second recording", f.zettel.recordedAbbreviations)
	}
}
