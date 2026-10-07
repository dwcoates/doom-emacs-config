package classifierupdate

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/internal/classifier"
	"claude-repld/internal/dlog"
	"claude-repld/internal/headless"
	"claude-repld/internal/prompts"
)

// repoPromptsDir is the checked-in prompts directory: the rewrite brief every
// test composes from is the real one.
const repoPromptsDir = "../../../prompts"

// routingBrief is a small routing brief with the real one's header shape.
const routingBrief = "<!-- used by: x; placeholders: {{token_hold}}, {{new_message}} -->\n" +
	"Answer {{token_hold}} for an independent request.\n\n<new-message>\n{{new_message}}\n</new-message>\n"

// rewrittenBody is a usable rewrite of routingBrief, as the model would
// answer it: slots in their shown form.
const rewrittenBody = "Answer ⟦token_hold⟧ for an independent request.\nAlways interrupt for an \"after\" ordering.\n\n<new-message>\n⟦new_message⟧\n</new-message>"

// fakeRunner is a scripted headless runner.
type fakeRunner struct {
	answer string
	err    error
	// during runs inside Run, before the answer: a test's hook into the
	// window the model is thinking in.
	during func()
	asked  []headless.Request
}

func (r *fakeRunner) Run(_ context.Context, req headless.Request) (headless.Response, error) {
	r.asked = append(r.asked, req)
	if r.during != nil {
		r.during()
	}
	if r.err != nil {
		return headless.Response{}, r.err
	}
	return headless.Response{Text: r.answer}, nil
}

func (r *fakeRunner) Bin() string { return "/fake/claude" }

// fakeGit is a scripted git.
type fakeGit struct {
	dirty     bool
	cleanErr  error
	commitErr error
	probed    []string
	committed []commitCall
}

type commitCall struct {
	dir, path, message string
}

func (g *fakeGit) PathClean(_ context.Context, _ string, path string) (bool, error) {
	g.probed = append(g.probed, path)
	return !g.dirty, g.cleanErr
}

func (g *fakeGit) CommitPath(_ context.Context, dir, path, message string) (string, error) {
	g.committed = append(g.committed, commitCall{dir: dir, path: path, message: message})
	if g.commitErr != nil {
		return "", g.commitErr
	}
	return "aaaabbbbccccdddd", nil
}

// failingFiles is OnDisk with a scripted write failure.
type failingFiles struct {
	OnDisk
	writeErrs []error
	writes    int
}

func (f *failingFiles) Write(path string, content []byte) error {
	defer func() { f.writes++ }()
	if f.writes < len(f.writeErrs) && f.writeErrs[f.writes] != nil {
		return f.writeErrs[f.writes]
	}
	return f.OnDisk.Write(path, content)
}

// fixture is one updater over a temp prompts directory.
type fixture struct {
	dir    string
	path   string
	runner *fakeRunner
	git    *fakeGit
	log    *dlog.TestLogger
	u      *Updater
}

func newFixture(t *testing.T, files Files) *fixture {
	t.Helper()
	dir := t.TempDir()
	rewrite, err := os.ReadFile(prompts.Path(repoPromptsDir, BriefRewrite))
	if err != nil {
		t.Fatalf("reading the rewrite brief: %v", err)
	}
	if err := os.WriteFile(prompts.Path(dir, BriefRewrite), rewrite, 0o644); err != nil {
		t.Fatalf("writing the rewrite brief: %v", err)
	}
	path := prompts.Path(dir, classifier.BriefRouting)
	if err := os.WriteFile(path, []byte(routingBrief), 0o644); err != nil {
		t.Fatalf("writing the routing brief: %v", err)
	}
	f := &fixture{dir: dir, path: path, runner: &fakeRunner{answer: rewrittenBody}, git: &fakeGit{}, log: dlog.NewTestLogger()}
	if files == nil {
		files = OnDisk{}
	}
	u, err := New(f.runner, f.git, files, dir, f.log)
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	f.u = u
	return f
}

func request() Request {
	return Request{
		Instruction: "always interrupt when a prompt says something must happen after something else",
		Example:     Example{Text: "after the tests pass, also bump the version", Route: classifier.RouteAfterToolCall},
	}
}

func (f *fixture) onDisk(t *testing.T) string {
	t.Helper()
	raw, err := os.ReadFile(f.path)
	if err != nil {
		t.Fatalf("reading the routing brief: %v", err)
	}
	return string(raw)
}

func (f *fixture) refusalRecord(t *testing.T, arm string) dlog.Record {
	t.Helper()
	for _, record := range f.log.Records() {
		if record.Operation == op && record.Context["arm"] == arm {
			return record
		}
	}
	t.Fatalf("no %s refusal was recorded: %+v", arm, f.log.Records())
	return dlog.Record{}
}

func requireRefusal(t *testing.T, err error, arm string) *Refusal {
	t.Helper()
	refusal, ok := AsRefusal(err)
	if !ok || refusal.Arm != arm {
		t.Fatalf("Update error = %v, want the %s refusal", err, arm)
	}
	return refusal
}

func TestUpdateWritesTheRewriteUnderTheOriginalHeader(t *testing.T) {
	// Arrange.
	f := newFixture(t, nil)

	// Act.
	if _, err := f.u.Update(context.Background(), request()); err != nil {
		t.Fatalf("Update: %v", err)
	}

	// Assert.
	want := "<!-- used by: x; placeholders: {{token_hold}}, {{new_message}} -->\n" +
		"Answer {{token_hold}} for an independent request.\nAlways interrupt for an \"after\" ordering.\n\n<new-message>\n{{new_message}}\n</new-message>\n"
	if got := f.onDisk(t); got != want {
		t.Fatalf("the brief on disk = %q, want %q", got, want)
	}
}

func TestUpdateCommitsTheBriefAloneWithTheInstruction(t *testing.T) {
	// Arrange.
	f := newFixture(t, nil)

	// Act.
	result, err := f.u.Update(context.Background(), request())

	// Assert.
	if err != nil {
		t.Fatalf("Update: %v", err)
	}
	if len(f.git.committed) != 1 {
		t.Fatalf("committed %d times, want once", len(f.git.committed))
	}
	call := f.git.committed[0]
	if call.dir != f.dir || call.path != f.path {
		t.Fatalf("committed %s in %s, want %s in %s", call.path, call.dir, f.path, f.dir)
	}
	if call.message != commitMessage(request().Instruction) {
		t.Fatalf("commit message = %q, want commitMessage's", call.message)
	}
	if result != (Result{Commit: "aaaabbbbccccdddd", Path: f.path}) {
		t.Fatalf("Update = %+v, want the commit and the brief's path", result)
	}
}

func TestUpdateRecordsTheCommittedRewrite(t *testing.T) {
	// Arrange.
	f := newFixture(t, nil)

	// Act.
	if _, err := f.u.Update(context.Background(), request()); err != nil {
		t.Fatalf("Update: %v", err)
	}

	// Assert.
	records := f.log.Records()
	last := records[len(records)-1]
	if last.Level != "info" || last.Operation != op || last.Context["commit"] != "aaaabbbbccccdddd" {
		t.Fatalf("last record = %+v, want the INFO record naming the commit", last)
	}
}

func TestUpdateAsksTheRewriteUnderItsOwnSiteAndModel(t *testing.T) {
	// Arrange.
	f := newFixture(t, nil)

	// Act.
	if _, err := f.u.Update(context.Background(), request()); err != nil {
		t.Fatalf("Update: %v", err)
	}

	// Assert.
	asked := f.runner.asked[0]
	if asked.Site != Site || asked.Model != Model || asked.Format != headless.FormatText || asked.Timeout != RunTimeout {
		t.Fatalf("asked %+v, want site %s, model %s, text, %s", asked, Site, Model, RunTimeout)
	}
}

func TestUpdateShowsTheModelTheChangeTheExampleAndTheBriefInSlots(t *testing.T) {
	// Arrange.
	f := newFixture(t, nil)
	req := request()

	// Act.
	if _, err := f.u.Update(context.Background(), req); err != nil {
		t.Fatalf("Update: %v", err)
	}

	// Assert.
	question := f.runner.asked[0].Prompt
	for _, want := range []string{req.Instruction, req.Example.Text, routeWords(req.Example.Route), "Answer ⟦token_hold⟧ for"} {
		if !strings.Contains(question, want) {
			t.Fatalf("the question does not carry %q:\n%s", want, question)
		}
	}
	if strings.Contains(question, "{{") || strings.Contains(question, "used by: x") {
		t.Fatalf("the question carries a raw placeholder or the brief's header:\n%s", question)
	}
}

func TestUpdateRefusesASecondUpdateWhileOneRuns(t *testing.T) {
	// Arrange: the first update is held inside its model call while the
	// second is asked.
	f := newFixture(t, nil)
	var second error
	f.runner.during = func() {
		f.runner.during = nil
		_, second = f.u.Update(context.Background(), request())
	}

	// Act.
	if _, err := f.u.Update(context.Background(), request()); err != nil {
		t.Fatalf("the first Update: %v", err)
	}

	// Assert.
	requireRefusal(t, second, ArmInProgress)
	if len(f.runner.asked) != 1 {
		t.Fatalf("the model ran %d times, want only the first update's", len(f.runner.asked))
	}
}

func TestUpdateRefusesABriefWithUncommittedChanges(t *testing.T) {
	// Arrange.
	f := newFixture(t, nil)
	f.git.dirty = true

	// Act.
	_, err := f.u.Update(context.Background(), request())

	// Assert.
	refusal := requireRefusal(t, err, ArmUncommittedChanges)
	if refusal.Fields["path"] != f.path {
		t.Fatalf("the refusal names %v, want %s", refusal.Fields["path"], f.path)
	}
	if len(f.runner.asked) != 0 || f.onDisk(t) != routingBrief {
		t.Fatalf("the model ran or the brief changed for a dirty brief")
	}
	if record := f.refusalRecord(t, ArmUncommittedChanges); record.Level != "info" {
		t.Fatalf("the refusal was recorded at %s, want INFO", record.Level)
	}
}

func TestUpdateFailsWhenTheCleanProbeFails(t *testing.T) {
	// Arrange.
	f := newFixture(t, nil)
	f.git.cleanErr = errors.New("fatal: not a git repository")

	// Act.
	_, err := f.u.Update(context.Background(), request())

	// Assert: a failure outside the contract, not a refusal, and nothing ran.
	if _, refused := AsRefusal(err); err == nil || refused || !strings.Contains(err.Error(), "not a git repository") {
		t.Fatalf("Update error = %v, want the probe's failure", err)
	}
	if len(f.runner.asked) != 0 {
		t.Fatalf("the model ran after a failed probe")
	}
}

func TestUpdateRefusesARewriteThatDidNotAnswer(t *testing.T) {
	// Arrange.
	f := newFixture(t, nil)
	f.runner.err = &headless.Error{Cause: headless.CauseTimeout, Detail: "no answer in 3m"}

	// Act.
	_, err := f.u.Update(context.Background(), request())

	// Assert.
	requireRefusal(t, err, ArmRewriteFailed)
	record := f.refusalRecord(t, ArmRewriteFailed)
	if record.Level != "error" || !strings.Contains(record.Context["reason"].(string), headless.CauseTimeout) {
		t.Fatalf("record = %+v, want an ERROR naming the timeout", record)
	}
	if f.onDisk(t) != routingBrief || len(f.git.committed) != 0 {
		t.Fatalf("a failed rewrite wrote or committed")
	}
}

func TestUpdateRefusesAnUnusableRewrite(t *testing.T) {
	// Arrange.
	tests := []struct {
		name   string
		answer string
		says   string
	}{
		{name: "empty", answer: "  \n", says: "nothing"},
		{name: "fenced", answer: "```\n" + rewrittenBody + "\n```", says: "code fence"},
		{name: "slot dropped", answer: "Answer ⟦token_hold⟧ always.", says: "new_message"},
		{name: "slot invented", answer: rewrittenBody + " ⟦running_turn⟧", says: "running_turn"},
		{name: "slot misspelled", answer: rewrittenBody + " ⟦Token Hold⟧", says: "well-formed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			f := newFixture(t, nil)
			f.runner.answer = tt.answer

			// Act.
			_, err := f.u.Update(context.Background(), request())

			// Assert.
			refusal := requireRefusal(t, err, ArmRewriteRejected)
			if !strings.Contains(refusal.Reason, tt.says) {
				t.Fatalf("the refusal says %q, want it to mention %q", refusal.Reason, tt.says)
			}
			if f.onDisk(t) != routingBrief || len(f.git.committed) != 0 {
				t.Fatalf("an unusable rewrite was written or committed")
			}
			if record := f.refusalRecord(t, ArmRewriteRejected); record.Level != "error" {
				t.Fatalf("the refusal was recorded at %s, want ERROR", record.Level)
			}
		})
	}
}

func TestUpdateRefusesARewriteThatChangedNothing(t *testing.T) {
	// Arrange.
	f := newFixture(t, nil)
	f.runner.answer = toSlots(strings.TrimSuffix(strings.SplitN(routingBrief, "\n", 2)[1], "\n"))

	// Act.
	_, err := f.u.Update(context.Background(), request())

	// Assert.
	requireRefusal(t, err, ArmUnchanged)
	if len(f.git.committed) != 0 {
		t.Fatalf("an unchanged brief was committed")
	}
}

func TestUpdateRefusesWhenTheBriefChangedWhileTheModelRan(t *testing.T) {
	// Arrange: somebody saves the brief while the model is thinking.
	f := newFixture(t, nil)
	edited := routingBrief + "edited\n"
	f.runner.during = func() {
		if err := os.WriteFile(f.path, []byte(edited), 0o644); err != nil {
			t.Fatalf("editing the brief: %v", err)
		}
	}

	// Act.
	_, err := f.u.Update(context.Background(), request())

	// Assert: the other edit stands, untouched and uncommitted.
	requireRefusal(t, err, ArmChangedDuringRewrite)
	if f.onDisk(t) != edited || len(f.git.committed) != 0 {
		t.Fatalf("the rewrite overwrote or committed over a concurrent edit")
	}
}

func TestUpdatePutsTheBriefBackWhenTheCommitFails(t *testing.T) {
	// Arrange.
	f := newFixture(t, nil)
	f.git.commitErr = errors.New("[merge-queue] REFUSED")

	// Act.
	_, err := f.u.Update(context.Background(), request())

	// Assert.
	refusal := requireRefusal(t, err, ArmCommitFailed)
	if !strings.Contains(refusal.Reason, "REFUSED") {
		t.Fatalf("the refusal says %q, want git's words", refusal.Reason)
	}
	if f.onDisk(t) != routingBrief {
		t.Fatalf("the brief was not put back after the failed commit")
	}
	if record := f.refusalRecord(t, ArmCommitFailed); record.Level != "error" {
		t.Fatalf("the refusal was recorded at %s, want ERROR", record.Level)
	}
}

func TestUpdateFailsLoudlyWhenTheBriefCannotBePutBack(t *testing.T) {
	// Arrange: the rewrite's write lands, the restore's does not.
	files := &failingFiles{writeErrs: []error{nil, errors.New("disk full")}}
	f := newFixture(t, files)
	f.git.commitErr = errors.New("commit refused")

	// Act.
	_, err := f.u.Update(context.Background(), request())

	// Assert: not a refusal — the brief is NOT as it was — and both causes are named.
	if _, refused := AsRefusal(err); err == nil || refused {
		t.Fatalf("Update error = %v, want a failure outside the contract", err)
	}
	if !strings.Contains(err.Error(), "commit refused") || !strings.Contains(err.Error(), "disk full") {
		t.Fatalf("Update error = %v, want both the commit's and the restore's cause", err)
	}
}

func TestUpdateFailsWithoutCommittingWhenTheWriteFails(t *testing.T) {
	// Arrange.
	files := &failingFiles{writeErrs: []error{errors.New("read-only file system")}}
	f := newFixture(t, files)

	// Act.
	_, err := f.u.Update(context.Background(), request())

	// Assert.
	if _, refused := AsRefusal(err); err == nil || refused || !strings.Contains(err.Error(), "read-only") {
		t.Fatalf("Update error = %v, want the write's failure", err)
	}
	if len(f.git.committed) != 0 || f.onDisk(t) != routingBrief {
		t.Fatalf("a failed write was committed or changed the brief")
	}
}

func TestUpdateFailsOnARoutingBriefCarryingASlotMarker(t *testing.T) {
	// Arrange: a ⟦ in the brief would come back as a {{ it never had.
	f := newFixture(t, nil)
	if err := os.WriteFile(f.path, []byte(strings.Replace(routingBrief, "independent", "⟦independent", 1)), 0o644); err != nil {
		t.Fatalf("writing: %v", err)
	}

	// Act.
	_, err := f.u.Update(context.Background(), request())

	// Assert.
	if err == nil || !strings.Contains(err.Error(), slotOpen) || len(f.runner.asked) != 0 {
		t.Fatalf("Update error = %v with %d model runs, want a refusal before the model", err, len(f.runner.asked))
	}
}

func TestUpdateFailsOnARoutingBriefThatDoesNotParse(t *testing.T) {
	// Arrange.
	f := newFixture(t, nil)
	if err := os.WriteFile(f.path, []byte("no header\n"), 0o644); err != nil {
		t.Fatalf("writing: %v", err)
	}

	// Act.
	_, err := f.u.Update(context.Background(), request())

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "does not parse") || len(f.runner.asked) != 0 {
		t.Fatalf("Update error = %v, want the parse failure before the model", err)
	}
}

func TestSlotsRoundTripThePlaceholders(t *testing.T) {
	// Arrange.
	text := "a {{token_hold}} b {{new_message}}"

	// Act.
	shown := toSlots(text)

	// Assert.
	if shown != "a ⟦token_hold⟧ b ⟦new_message⟧" || fromSlots(shown) != text {
		t.Fatalf("toSlots = %q, fromSlots(toSlots) = %q", shown, fromSlots(shown))
	}
}

func TestRouteWordsSaysEveryRoute(t *testing.T) {
	// Arrange.
	tests := []struct {
		route classifier.Route
		want  string
	}{
		{route: classifier.RouteInterrupt, want: "interrupt the running turn"},
		{route: classifier.RouteAfterToolCall, want: "join the running turn after its current tool call"},
		{route: classifier.RouteQueue, want: "wait for the running turn to end"},
	}
	for _, tt := range tests {
		t.Run(tt.route.String(), func(t *testing.T) {
			// Act, Assert.
			if got := routeWords(tt.route); got != tt.want {
				t.Fatalf("routeWords(%s) = %q, want %q", tt.route, got, tt.want)
			}
		})
	}
}

func TestRouteWordsPanicsOnAnUnknownRoute(t *testing.T) {
	// Arrange.
	defer func() {
		if recover() == nil {
			t.Fatalf("routeWords(99) did not panic")
		}
	}()

	// Act.
	routeWords(classifier.Route(99))
}

func TestCommitMessageCarriesTheSubjectAndTheInstruction(t *testing.T) {
	// Act.
	got := commitMessage("  interrupt for 'after'  ")

	// Assert.
	if got != CommitSubject+"\n\nRequested change: interrupt for 'after'\n" {
		t.Fatalf("commitMessage = %q", got)
	}
}

func TestTheCheckedInRewriteBriefSpellsTheSlotMarkers(t *testing.T) {
	// Arrange: the brief tells the model what a slot looks like, in the
	// characters toSlots uses.
	brief, err := prompts.Load(repoPromptsDir, BriefRewrite)
	if err != nil {
		t.Fatalf("Load: %v", err)
	}

	// Act, Assert.
	if !strings.Contains(brief.Body, slotOpen+"token_hold"+slotClose) {
		t.Fatalf("the rewrite brief does not show a slot as %stoken_hold%s", slotOpen, slotClose)
	}
}

func TestOnDiskWriteReplacesTheContentAndKeepsTheMode(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "b.md")
	if err := os.WriteFile(path, []byte("old"), 0o640); err != nil {
		t.Fatalf("writing: %v", err)
	}

	// Act.
	if err := (OnDisk{}).Write(path, []byte("new")); err != nil {
		t.Fatalf("Write: %v", err)
	}

	// Assert.
	raw, _ := os.ReadFile(path)
	info, _ := os.Stat(path)
	if string(raw) != "new" || info.Mode().Perm() != 0o640 {
		t.Fatalf("content %q mode %v, want \"new\" and 0640", raw, info.Mode().Perm())
	}
	if entries, _ := os.ReadDir(filepath.Dir(path)); len(entries) != 1 {
		t.Fatalf("the write left %d entries behind, want the one file", len(entries))
	}
}

func TestOnDiskWriteFailsForAMissingFile(t *testing.T) {
	// Act.
	err := (OnDisk{}).Write(filepath.Join(t.TempDir(), "absent.md"), []byte("x"))

	// Assert.
	if !errors.Is(err, os.ErrNotExist) {
		t.Fatalf("Write = %v, want not-exist", err)
	}
}

func TestNewRefusesAMissingDependency(t *testing.T) {
	// Arrange.
	runner, git, log := &fakeRunner{}, &fakeGit{}, dlog.NewTestLogger()
	tests := []struct {
		name string
		make func() (*Updater, error)
	}{
		{name: "runner", make: func() (*Updater, error) { return New(nil, git, OnDisk{}, "/p", log) }},
		{name: "git", make: func() (*Updater, error) { return New(runner, nil, OnDisk{}, "/p", log) }},
		{name: "files", make: func() (*Updater, error) { return New(runner, git, nil, "/p", log) }},
		{name: "prompts dir", make: func() (*Updater, error) { return New(runner, git, OnDisk{}, "", log) }},
		{name: "log", make: func() (*Updater, error) { return New(runner, git, OnDisk{}, "/p", nil) }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			_, err := tt.make()

			// Assert.
			if err == nil {
				t.Fatalf("New with no %s = nil error", tt.name)
			}
		})
	}
}
