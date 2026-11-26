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
	"agent-repld/internal/wsm"
)

const testWS = ids.WorkspaceID("ws-1")

// footerSummary is a whole summary of the footer work.
var footerSummary = wsm.WorkspaceSummary{Long: "Fix the footer's stall detail. It reads wrong at a cold gate.", Short: "Fix the footer"}

// fakeStore is an in-memory summary store whose every call can be failed.
type fakeStore struct {
	summaries map[ids.WorkspaceID]wsm.WorkspaceSummary
	titles    map[ids.WorkspaceID]string
	putErr    error
	dropErr   error
	readErr   error
	// titlePutErr and titleDropErr fail the vendor title's writes.
	titlePutErr  error
	titleDropErr error
}

func newFakeStore() *fakeStore {
	return &fakeStore{summaries: map[ids.WorkspaceID]wsm.WorkspaceSummary{}, titles: map[ids.WorkspaceID]string{}}
}

func (s *fakeStore) PutVendorTitle(_ context.Context, ws ids.WorkspaceID, title string) error {
	if s.titlePutErr != nil {
		return s.titlePutErr
	}
	s.titles[ws] = title
	return nil
}

func (s *fakeStore) DropVendorTitle(_ context.Context, ws ids.WorkspaceID) (bool, error) {
	if s.titleDropErr != nil {
		return false, s.titleDropErr
	}
	_, stood := s.titles[ws]
	delete(s.titles, ws)
	return stood, nil
}

func (s *fakeStore) PutWorkspaceSummary(_ context.Context, ws ids.WorkspaceID, summary wsm.WorkspaceSummary) error {
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

func (s *fakeStore) WorkspaceSummaryOf(_ context.Context, ws ids.WorkspaceID) (wsm.WorkspaceSummary, bool, error) {
	if s.readErr != nil {
		return wsm.WorkspaceSummary{}, false, s.readErr
	}
	summary, ok := s.summaries[ws]
	return summary, ok, nil
}

// fakeTopbar records every summary it is handed.
type fakeTopbar struct {
	installed []wsm.WorkspaceSummary
}

func (t *fakeTopbar) SetSummary(_ ids.WorkspaceID, summary wsm.WorkspaceSummary) {
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

func TestValidateShortAcceptsOnePlainImperativeSentence(t *testing.T) {
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
			err := ValidateShort(tt.summary)

			// Assert
			if err != nil {
				t.Fatalf("ValidateShort(%q) = %v, want nil", tt.summary, err)
			}
		})
	}
}

func TestValidateShortRefusesWhatTheTitleLineDoesNotDraw(t *testing.T) {
	tests := []struct {
		name    string
		summary string
		reason  string
	}{
		{name: "one word", summary: "Fix", reason: "a short summary is 2 to 15 words, not 1"},
		{name: "too many words", summary: strings.TrimSpace(strings.Repeat("word ", MaxWords+1)), reason: "a short summary is 2 to 15 words, not 16"},
		{name: "two sentences", summary: "Fix the footer. Then the topbar", reason: `a short summary is one sentence, and "footer." ends one before its last word`},
		{name: "a line break", summary: "Fix the\nfooter", reason: "a short summary is one line"},
		{name: "surrounding whitespace", summary: " Fix the footer", reason: "a short summary carries no whitespace at either end"},
		{name: "a comma", summary: "Fix the footer, then the topbar", reason: `a short summary is one plain sentence, so it carries no "," joining clauses`},
		{name: "a semicolon", summary: "Fix the footer; and the topbar", reason: `a short summary is one plain sentence, so it carries no ";" joining clauses`},
		{name: "a colon", summary: "Footer: fix the stall", reason: `a short summary is one plain sentence, so it carries no ":" joining clauses`},
		{name: "a parenthesis", summary: "Fix the footer (and topbar)", reason: `a short summary is one plain sentence, so it carries no "(" joining clauses`},
		{name: "an em dash", summary: "Fix the footer — then topbar", reason: `a short summary is one plain sentence, so it carries no "—" joining clauses`},
		{name: "a spaced hyphen", summary: "Fix the footer - then topbar", reason: `a short summary is one plain sentence, so it carries no "-" joining clauses`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange, Act
			err := ValidateShort(tt.summary)

			// Assert
			if err == nil || err.Error() != tt.reason {
				t.Fatalf("ValidateShort(%q) = %v, want %q", tt.summary, err, tt.reason)
			}
		})
	}
}

func TestValidateShortSharesTheOneSentenceRule(t *testing.T) {
	// Arrange — a summary breaking the shared rule.
	summary := "Fix the footer. Then the topbar"

	// Act
	got := ValidateShort(summary)

	// Assert — the refusal is textline.OneSentence's own, never a second
	// spelling of the rule.
	want := textline.OneSentence("short summary", summary, MinWords, MaxWords)
	if got == nil || want == nil || got.Error() != want.Error() {
		t.Fatalf("ValidateShort = %v, want textline.OneSentence's %v", got, want)
	}
}

func TestRestoreDrawsTheStoredSummary(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.store.summaries[testWS] = footerSummary

	// Act
	err := h.k.Restore(context.Background(), testWS)

	// Assert
	if err != nil {
		t.Fatalf("Restore: %v", err)
	}
	if !reflect.DeepEqual(h.topbar.installed, []wsm.WorkspaceSummary{footerSummary}) {
		t.Fatalf("installed %+v, want the stored summary", h.topbar.installed)
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
		t.Fatalf("installed %+v, want nothing", h.topbar.installed)
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
		t.Fatalf("installed %+v after a failed read, want nothing", h.topbar.installed)
	}
	if rec := h.recorded(t, "error", opRestore); rec.Context["workspace"] != string(testWS) || rec.Context["cause"] == nil {
		t.Fatalf("the record = %+v, want the workspace and the cause", rec.Context)
	}
}

func TestSetRecordsAndDrawsTheSummary(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	err := h.k.Set(context.Background(), testWS, footerSummary)

	// Assert
	if err != nil {
		t.Fatalf("Set: %v", err)
	}
	if h.store.summaries[testWS] != footerSummary {
		t.Fatalf("stored %+v, want the summary", h.store.summaries[testWS])
	}
	if !reflect.DeepEqual(h.topbar.installed, []wsm.WorkspaceSummary{footerSummary}) {
		t.Fatalf("installed %+v, want the summary", h.topbar.installed)
	}
	if rec := h.recorded(t, "info", opSet); rec.Context["short_summary"] != footerSummary.Short || rec.Context["long_summary"] != footerSummary.Long {
		t.Fatalf("the record = %+v, want both parts", rec.Context)
	}
}

func TestAFailedSetDrawsNothingAndIsLogged(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.store.putErr = errors.New("the state database is locked")

	// Act
	err := h.k.Set(context.Background(), testWS, footerSummary)

	// Assert
	if !errors.Is(err, h.store.putErr) {
		t.Fatalf("Set = %v, want the write's failure", err)
	}
	if len(h.topbar.installed) != 0 {
		t.Fatalf("installed %+v after a failed write, want nothing", h.topbar.installed)
	}
	if rec := h.recorded(t, "error", opSet); rec.Context["short_summary"] != footerSummary.Short || rec.Context["long_summary"] != footerSummary.Long {
		t.Fatalf("the record = %+v, want both parts", rec.Context)
	}
}

func TestAClearDropsAndRetractsTheSummary(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.store.summaries[testWS] = footerSummary

	// Act
	h.k.OnContextReset(testWS)

	// Assert
	if _, stands := h.store.summaries[testWS]; stands {
		t.Fatalf("the summary still stands after a /clear")
	}
	if !reflect.DeepEqual(h.topbar.installed, []wsm.WorkspaceSummary{{}}) {
		t.Fatalf("installed %+v, want the retraction", h.topbar.installed)
	}
	if rec := h.recorded(t, "info", opReset); rec.Context["dropped"] != true {
		t.Fatalf("the record = %+v, want dropped=true", rec.Context)
	}
}

func TestAFailedClearDropKeepsTheSummaryDrawnAndIsLogged(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.store.summaries[testWS] = footerSummary
	h.store.dropErr = errors.New("the state database is locked")

	// Act
	h.k.OnContextReset(testWS)

	// Assert
	if len(h.topbar.installed) != 0 {
		t.Fatalf("installed %+v after a failed drop, want the summary left drawn", h.topbar.installed)
	}
	if rec := h.recorded(t, "error", opReset); rec.Context["workspace"] != string(testWS) || rec.Context["cause"] == nil {
		t.Fatalf("the record = %+v, want the workspace and the cause", rec.Context)
	}
}

func TestAVendorTitleIsRecordedAndOwesTheSummaryStill(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	h.k.OnVendorTitle(testWS, "Add SPC j keybinding support")

	// Assert
	if got := h.store.titles[testWS]; got != "Add SPC j keybinding support" {
		t.Fatalf("stored title = %q, want the vendor's", got)
	}
	if _, owed, err := h.k.OwedNote(context.Background(), testWS); err != nil || !owed {
		t.Fatalf("OwedNote owed = %v, %v; want a summary still owed", owed, err)
	}
}

func TestAnEmptyVendorTitleDropsTheStoredOne(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.store.titles[testWS] = "Add SPC j keybinding support"

	// Act
	h.k.OnVendorTitle(testWS, "")

	// Assert
	if _, stood := h.store.titles[testWS]; stood {
		t.Fatal("the retracted title still stands")
	}
}

func TestAFailedVendorTitleWriteIsLogged(t *testing.T) {
	tests := []struct {
		name  string
		title string
		fail  func(s *fakeStore)
	}{
		{name: "a title that could not be recorded", title: "Add SPC j keybinding support", fail: func(s *fakeStore) { s.titlePutErr = errors.New("the state database is locked") }},
		{name: "a retraction that could not be dropped", title: "", fail: func(s *fakeStore) { s.titleDropErr = errors.New("the state database is locked") }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.store.titles[testWS] = "Fix the footer"
			tt.fail(h.store)

			// Act
			h.k.OnVendorTitle(testWS, tt.title)

			// Assert
			if got := h.store.titles[testWS]; got != "Fix the footer" {
				t.Fatalf("stored title = %q, want the one standing left as it was", got)
			}
			if rec := h.recorded(t, "error", opTitle); rec.Context["workspace"] != string(testWS) || rec.Context["cause"] == nil {
				t.Fatalf("the record = %+v, want the workspace and the cause", rec.Context)
			}
		})
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
	h.store.summaries[testWS] = footerSummary

	// Act
	note, owed, err := h.k.OwedNote(context.Background(), testWS)

	// Assert
	if err != nil || owed || note != "" {
		t.Fatalf("OwedNote = (%q, %v, %v), want nothing owed", note, owed, err)
	}
}

func TestAWorkspaceWithOnlyAShortSummaryOwesTheNote(t *testing.T) {
	// Arrange: a summary set before long summaries existed.
	h := newHarness(t)
	h.store.summaries[testWS] = wsm.WorkspaceSummary{Short: "Fix the footer"}

	// Act
	note, owed, err := h.k.OwedNote(context.Background(), testWS)

	// Assert
	if err != nil || !owed || note == "" {
		t.Fatalf("OwedNote = (%q, %v, %v), want the note owed", note, owed, err)
	}
	if rec := h.recorded(t, "debug", opOwed); rec.Context["short_only"] != true {
		t.Fatalf("the record = %+v, want short_only=true", rec.Context)
	}
}

func TestRestoreDrawsAShortOnlySummary(t *testing.T) {
	// Arrange
	h := newHarness(t)
	shortOnly := wsm.WorkspaceSummary{Short: "Fix the footer"}
	h.store.summaries[testWS] = shortOnly

	// Act
	err := h.k.Restore(context.Background(), testWS)

	// Assert
	if err != nil {
		t.Fatalf("Restore: %v", err)
	}
	if !reflect.DeepEqual(h.topbar.installed, []wsm.WorkspaceSummary{shortOnly}) {
		t.Fatalf("installed %+v, want the short summary drawn", h.topbar.installed)
	}
	if rec := h.recorded(t, "debug", opRestore); rec.Context["complete"] != false {
		t.Fatalf("the record = %+v, want complete=false", rec.Context)
	}
}

func TestValidateLongAcceptsTwoToFourSentences(t *testing.T) {
	tests := []struct {
		name string
		long string
	}{
		{name: "two sentences", long: "Fix the footer. It stalls at a cold gate."},
		{name: "four sentences", long: "Fix the footer. It stalls. Users notice. Ship it today."},
		{name: "at the most words", long: strings.TrimSpace(strings.Repeat("word ", LongMaxWords-1)) + ". Done."},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange, Act
			err := ValidateLong(tt.long)

			// Assert
			if err != nil {
				t.Fatalf("ValidateLong(%q) = %v, want nil", tt.long, err)
			}
		})
	}
}

func TestValidateLongRefusesWhatTheHoverDoesNotShow(t *testing.T) {
	tests := []struct {
		name   string
		long   string
		reason string
	}{
		{name: "one sentence", long: "Fix the footer.", reason: "a long summary is 2 to 4 sentences, not 1"},
		{name: "five sentences", long: "One. Two. Three. Four. Five.", reason: "a long summary is 2 to 4 sentences, not 5"},
		{name: "too many words", long: strings.TrimSpace(strings.Repeat("word ", LongMaxWords)) + ". Done.", reason: "a long summary is at most 80 words, not 81"},
		{name: "a line break", long: "Fix the footer.\nIt stalls.", reason: "a long summary is one paragraph, with no line break"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange, Act
			err := ValidateLong(tt.long)

			// Assert
			if err == nil || err.Error() != tt.reason {
				t.Fatalf("ValidateLong(%q) = %v, want %q", tt.long, err, tt.reason)
			}
		})
	}
}

func TestValidateLongSharesTheParagraphRule(t *testing.T) {
	// Arrange
	long := "Fix the footer."

	// Act
	got := ValidateLong(long)

	// Assert
	want := textline.Paragraph("long summary", long, LongMinSentences, LongMaxSentences, LongMaxWords)
	if got == nil || want == nil || got.Error() != want.Error() {
		t.Fatalf("ValidateLong = %v, want textline.Paragraph's %v", got, want)
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
	if !strings.Contains(note, "agent-repld workspace summary here -long") || !strings.Contains(note, "-short") {
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
