package wssummary

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/prompts"
	"agent-repld/internal/textline"
)

const testWS = ids.WorkspaceID("ws-1")

// fakeStore is an in-memory summary store whose every call can be failed.
type fakeStore struct {
	summaries map[ids.WorkspaceID]string
	putErr    error
	dropErr   error
	readErr   error
}

func newFakeStore() *fakeStore { return &fakeStore{summaries: map[ids.WorkspaceID]string{}} }

func (s *fakeStore) PutWorkspaceSummary(_ context.Context, ws ids.WorkspaceID, summary string) error {
	if s.putErr != nil {
		return s.putErr
	}
	s.summaries[ws] = summary
	return nil
}

func (s *fakeStore) DropWorkspaceSummary(_ context.Context, ws ids.WorkspaceID) (bool, error) {
	if s.dropErr != nil {
		return false, s.dropErr
	}
	_, stood := s.summaries[ws]
	delete(s.summaries, ws)
	return stood, nil
}

func (s *fakeStore) WorkspaceSummaryOf(_ context.Context, ws ids.WorkspaceID) (string, bool, error) {
	if s.readErr != nil {
		return "", false, s.readErr
	}
	summary, ok := s.summaries[ws]
	return summary, ok, nil
}

// fakeTopbar records every summary it is handed.
type fakeTopbar struct {
	installed []string
}

func (t *fakeTopbar) SetSummary(_ ids.WorkspaceID, summary string) {
	t.installed = append(t.installed, summary)
}

// harness is one keeper over fakes, reading its brief from a temp corpus.
type harness struct {
	k      *Keeper
	store  *fakeStore
	topbar *fakeTopbar
	log    *dlog.TestLogger
	dir    string
}

func newHarness(t *testing.T) *harness {
	t.Helper()
	dir := t.TempDir()
	writeBrief(t, dir, "<!-- used by: test; placeholders: none -->\nset your summary\n")
	h := &harness{store: newFakeStore(), topbar: &fakeTopbar{}, log: dlog.NewTestLogger(), dir: dir}
	h.k = New(Deps{Store: h.store, Topbar: h.topbar, PromptsDir: dir, Log: h.log})
	return h
}

// writeBrief writes BriefOwed into DIR.
func writeBrief(t *testing.T, dir, body string) {
	t.Helper()
	if err := os.WriteFile(filepath.Join(dir, BriefOwed+".md"), []byte(body), 0o600); err != nil {
		t.Fatalf("write the brief: %v", err)
	}
}

// recorded answers the record at LEVEL under OP, failing the test when none
// was written.
func (h *harness) recorded(t *testing.T, level, op string) dlog.Record {
	t.Helper()
	rec, ok := dlog.FindRecord(h.log.Records(), dlog.RecordQuery{Level: level, Operation: op})
	if !ok {
		t.Fatalf("no %s record under %s: %+v", level, op, h.log.Records())
	}
	return rec
}

func TestValidateAcceptsOnePlainImperativeSentence(t *testing.T) {
	tests := []struct {
		name    string
		summary string
	}{
		{name: "with its period", summary: "Fix the footer's stall detail for cold gates."},
		{name: "without a period", summary: "Fix the footer"},
		{name: "at the fewest words", summary: "Fix footers"},
		{name: "at the most words", summary: strings.TrimSpace(strings.Repeat("word ", MaxWords))},
		{name: "a hyphenated word", summary: "Make the merge-queue gate faster."},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange, Act
			err := Validate(tt.summary)

			// Assert
			if err != nil {
				t.Fatalf("Validate(%q) = %v, want nil", tt.summary, err)
			}
		})
	}
}

func TestValidateRefusesWhatTheTitleLineDoesNotDraw(t *testing.T) {
	tests := []struct {
		name    string
		summary string
		reason  string
	}{
		{name: "one word", summary: "Fix", reason: "a summary is 2 to 15 words, not 1"},
		{name: "too many words", summary: strings.TrimSpace(strings.Repeat("word ", MaxWords+1)), reason: "a summary is 2 to 15 words, not 16"},
		{name: "two sentences", summary: "Fix the footer. Then the topbar", reason: `a summary is one sentence, and "footer." ends one before its last word`},
		{name: "a line break", summary: "Fix the\nfooter", reason: "a summary is one line"},
		{name: "surrounding whitespace", summary: " Fix the footer", reason: "a summary carries no whitespace at either end"},
		{name: "a comma", summary: "Fix the footer, then the topbar", reason: `a summary is one plain sentence, so it carries no "," joining clauses`},
		{name: "a semicolon", summary: "Fix the footer; and the topbar", reason: `a summary is one plain sentence, so it carries no ";" joining clauses`},
		{name: "a colon", summary: "Footer: fix the stall", reason: `a summary is one plain sentence, so it carries no ":" joining clauses`},
		{name: "a parenthesis", summary: "Fix the footer (and topbar)", reason: `a summary is one plain sentence, so it carries no "(" joining clauses`},
		{name: "an em dash", summary: "Fix the footer — then topbar", reason: `a summary is one plain sentence, so it carries no "—" joining clauses`},
		{name: "a spaced hyphen", summary: "Fix the footer - then topbar", reason: `a summary is one plain sentence, so it carries no "-" joining clauses`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange, Act
			err := Validate(tt.summary)

			// Assert
			if err == nil || err.Error() != tt.reason {
				t.Fatalf("Validate(%q) = %v, want %q", tt.summary, err, tt.reason)
			}
		})
	}
}

func TestValidateSharesTheOneSentenceRule(t *testing.T) {
	// Arrange — a summary breaking the shared rule.
	summary := "Fix the footer. Then the topbar"

	// Act
	got := Validate(summary)

	// Assert — the refusal is textline.OneSentence's own, never a second
	// spelling of the rule.
	want := textline.OneSentence("summary", summary, MinWords, MaxWords)
	if got == nil || want == nil || got.Error() != want.Error() {
		t.Fatalf("Validate = %v, want textline.OneSentence's %v", got, want)
	}
}

func TestRestoreDrawsTheStoredSummary(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.store.summaries[testWS] = "Fix the footer"

	// Act
	err := h.k.Restore(context.Background(), testWS)

	// Assert
	if err != nil {
		t.Fatalf("Restore: %v", err)
	}
	if !reflect.DeepEqual(h.topbar.installed, []string{"Fix the footer"}) {
		t.Fatalf("installed %q, want the stored summary", h.topbar.installed)
	}
}

func TestRestoreWithNoSummaryDrawsNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	err := h.k.Restore(context.Background(), testWS)

	// Assert
	if err != nil {
		t.Fatalf("Restore: %v", err)
	}
	if len(h.topbar.installed) != 0 {
		t.Fatalf("installed %q, want nothing", h.topbar.installed)
	}
	h.recorded(t, "debug", opRestore)
}

func TestAFailedRestoreIsSurfacedAndLogged(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.store.readErr = errors.New("the state database is locked")

	// Act
	err := h.k.Restore(context.Background(), testWS)

	// Assert
	if !errors.Is(err, h.store.readErr) {
		t.Fatalf("Restore = %v, want the read's failure", err)
	}
	if len(h.topbar.installed) != 0 {
		t.Fatalf("installed %q after a failed read, want nothing", h.topbar.installed)
	}
	if rec := h.recorded(t, "error", opRestore); rec.Context["workspace"] != string(testWS) || rec.Context["cause"] == nil {
		t.Fatalf("the record = %+v, want the workspace and the cause", rec.Context)
	}
}

func TestSetRecordsAndDrawsTheSummary(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	err := h.k.Set(context.Background(), testWS, "Fix the footer")

	// Assert
	if err != nil {
		t.Fatalf("Set: %v", err)
	}
	if h.store.summaries[testWS] != "Fix the footer" {
		t.Fatalf("stored %q, want the summary", h.store.summaries[testWS])
	}
	if !reflect.DeepEqual(h.topbar.installed, []string{"Fix the footer"}) {
		t.Fatalf("installed %q, want the summary", h.topbar.installed)
	}
	if rec := h.recorded(t, "info", opSet); rec.Context["summary"] != "Fix the footer" {
		t.Fatalf("the record = %+v, want the summary", rec.Context)
	}
}

func TestAFailedSetDrawsNothingAndIsLogged(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.store.putErr = errors.New("the state database is locked")

	// Act
	err := h.k.Set(context.Background(), testWS, "Fix the footer")

	// Assert
	if !errors.Is(err, h.store.putErr) {
		t.Fatalf("Set = %v, want the write's failure", err)
	}
	if len(h.topbar.installed) != 0 {
		t.Fatalf("installed %q after a failed write, want nothing", h.topbar.installed)
	}
	if rec := h.recorded(t, "error", opSet); rec.Context["summary"] != "Fix the footer" {
		t.Fatalf("the record = %+v, want the summary", rec.Context)
	}
}

func TestAClearDropsAndRetractsTheSummary(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.store.summaries[testWS] = "Fix the footer"

	// Act
	h.k.OnContextReset(testWS)

	// Assert
	if _, stands := h.store.summaries[testWS]; stands {
		t.Fatalf("the summary still stands after a /clear")
	}
	if !reflect.DeepEqual(h.topbar.installed, []string{""}) {
		t.Fatalf("installed %q, want the retraction", h.topbar.installed)
	}
	if rec := h.recorded(t, "info", opReset); rec.Context["dropped"] != true {
		t.Fatalf("the record = %+v, want dropped=true", rec.Context)
	}
}

func TestAFailedClearDropKeepsTheSummaryDrawnAndIsLogged(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.store.summaries[testWS] = "Fix the footer"
	h.store.dropErr = errors.New("the state database is locked")

	// Act
	h.k.OnContextReset(testWS)

	// Assert
	if len(h.topbar.installed) != 0 {
		t.Fatalf("installed %q after a failed drop, want the summary left drawn", h.topbar.installed)
	}
	if rec := h.recorded(t, "error", opReset); rec.Context["workspace"] != string(testWS) || rec.Context["cause"] == nil {
		t.Fatalf("the record = %+v, want the workspace and the cause", rec.Context)
	}
}

func TestAWorkspaceWithNoSummaryOwesTheWrappedNote(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	note, owed, err := h.k.OwedNote(context.Background(), testWS)

	// Assert
	if err != nil || !owed {
		t.Fatalf("OwedNote = (%v, %v), want owed", owed, err)
	}
	if want := prompts.Wrap("\n\nset your summary"); note != want {
		t.Fatalf("note = %q, want the brief meta-wrapped %q", note, want)
	}
}

func TestAWorkspaceWithASummaryOwesNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.store.summaries[testWS] = "Fix the footer"

	// Act
	note, owed, err := h.k.OwedNote(context.Background(), testWS)

	// Assert
	if err != nil || owed || note != "" {
		t.Fatalf("OwedNote = (%q, %v, %v), want nothing owed", note, owed, err)
	}
}

func TestAFailedOwedReadIsSurfacedAndLogged(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.store.readErr = errors.New("the state database is locked")

	// Act
	_, owed, err := h.k.OwedNote(context.Background(), testWS)

	// Assert
	if !errors.Is(err, h.store.readErr) || owed {
		t.Fatalf("OwedNote = (%v, %v), want the read's failure", owed, err)
	}
	h.recorded(t, "error", opOwed)
}

func TestAMissingBriefIsSurfacedAndLogged(t *testing.T) {
	// Arrange
	h := newHarness(t)
	if err := os.Remove(filepath.Join(h.dir, BriefOwed+".md")); err != nil {
		t.Fatalf("remove the brief: %v", err)
	}

	// Act
	_, owed, err := h.k.OwedNote(context.Background(), testWS)

	// Assert
	if err == nil || owed {
		t.Fatalf("OwedNote = (%v, %v), want the brief's failure", owed, err)
	}
	if rec := h.recorded(t, "error", opOwed); rec.Context["brief"] != BriefOwed {
		t.Fatalf("the record = %+v, want the brief named", rec.Context)
	}
}

func TestNoteIsTheBriefMetaWrappedAsOwedNoteAppendsIt(t *testing.T) {
	// Arrange
	h := newHarness(t)
	owedNote, _, err := h.k.OwedNote(context.Background(), testWS)
	if err != nil {
		t.Fatalf("OwedNote: %v", err)
	}

	// Act
	note, err := Note(h.dir)

	// Assert
	if err != nil || note != prompts.Wrap("\n\nset your summary") {
		t.Fatalf("Note = (%q, %v), want the brief meta-wrapped", note, err)
	}
	if note != owedNote {
		t.Fatalf("Note = %q, want exactly the note OwedNote appends %q", note, owedNote)
	}
}

func TestNoteWithAMissingBriefIsTheBriefsFailure(t *testing.T) {
	// Arrange
	dir := t.TempDir()

	// Act
	note, err := Note(dir)

	// Assert
	if err == nil || note != "" || !strings.Contains(err.Error(), BriefOwed) {
		t.Fatalf("Note = (%q, %v), want the brief named in the failure", note, err)
	}
}

func TestTheShippedBriefLoadsAndNamesTheVerb(t *testing.T) {
	// Arrange
	dir := filepath.Join("..", "..", "..", "prompts")

	// Act
	brief, err := prompts.Load(dir, BriefOwed)

	// Assert
	if err != nil {
		t.Fatalf("load the shipped brief: %v", err)
	}
	note, err := brief.Splice(nil)
	if err != nil {
		t.Fatalf("splice the shipped brief: %v", err)
	}
	if !strings.Contains(note, "agent-repld workspace summary here -text") {
		t.Fatalf("the shipped brief names no summary verb: %q", note)
	}
}

func TestNewRefusesAMissingCollaborator(t *testing.T) {
	tests := []struct {
		name  string
		blank func(*Deps)
	}{
		{name: "store", blank: func(d *Deps) { d.Store = nil }},
		{name: "topbar", blank: func(d *Deps) { d.Topbar = nil }},
		{name: "prompts dir", blank: func(d *Deps) { d.PromptsDir = "" }},
		{name: "log", blank: func(d *Deps) { d.Log = nil }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			deps := Deps{Store: newFakeStore(), Topbar: &fakeTopbar{}, PromptsDir: "p", Log: dlog.NewTestLogger()}
			tt.blank(&deps)

			// Act, Assert
			defer func() {
				if recover() == nil {
					t.Fatalf("New with no %s did not panic", tt.name)
				}
			}()
			New(deps)
		})
	}
}
