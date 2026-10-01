package main

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/commandfile"
)

// --- fixtures -------------------------------------------------------------

// scriptStep is one thing a fake roster stream does: push a frame, or run an
// action (the fake ingress retiring the command file).
type scriptStep struct {
	roster *frontendv1.WorkspaceRoster
	act    func()
}

// fakeMergeDaemon is one daemon: a scripted roster stream and a footer.
type fakeMergeDaemon struct {
	script []scriptStep
	// end is what the stream answers once the script is spent; nil holds the
	// stream open until the verb is done with it.
	end     error
	footer  *frontendv1.FooterView
	footErr error
	// footerRef is the ref the footer was asked for.
	footerRef *workspacev1.WorkspaceRef
}

func (f *fakeMergeDaemon) WatchRoster(ctx context.Context, yield func(*frontendv1.WorkspaceRoster) error) error {
	for _, step := range f.script {
		if step.act != nil {
			step.act()
			continue
		}
		if err := yield(step.roster); err != nil {
			return err
		}
	}
	if f.end != nil {
		return f.end
	}
	<-ctx.Done()
	return ctx.Err()
}

func (f *fakeMergeDaemon) Footer(_ context.Context, ref *workspacev1.WorkspaceRef) (*frontendv1.FooterView, error) {
	f.footerRef = ref
	return f.footer, f.footErr
}

// mergeFixture is a state root with a daemon.addr, the CALLING workspace's
// worktree (the caller's directory is inside it), another workspace's
// worktree, and the daemons the verb dials, in order.
type mergeFixture struct {
	stateDir string
	worktree string
	other    string
	daemons  []*fakeMergeDaemon
	dialed   int
	cwd      string
	out      bytes.Buffer
	errOut   bytes.Buffer
}

func newMergeFixture(t *testing.T) *mergeFixture {
	t.Helper()
	f := &mergeFixture{stateDir: t.TempDir(), worktree: t.TempDir(), other: t.TempDir()}
	f.cwd = filepath.Join(f.worktree, "sub")
	if err := os.MkdirAll(f.cwd, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	if err := os.WriteFile(filepath.Join(f.stateDir, "daemon.addr"), []byte("127.0.0.1:4242"), 0o644); err != nil {
		t.Fatalf("write daemon.addr: %v", err)
	}
	return f
}

func (f *mergeFixture) run(t *testing.T, args ...string) int {
	t.Helper()
	dial := func(string) mergeQueueDaemon {
		if f.dialed >= len(f.daemons) {
			t.Fatalf("the verb dialed a daemon %d times; the fixture scripts %d", f.dialed+1, len(f.daemons))
		}
		d := f.daemons[f.dialed]
		f.dialed++
		return d
	}
	env := mergeQueueEnv{getwd: func() (string, error) { return f.cwd, nil }, poll: time.Hour}
	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()
	return runMergeQueueVerb(ctx, append([]string{"-state-dir", f.stateDir}, args...), dial, env, &f.out, &f.errOut)
}

// outputDir is the fixture's command-file ingress.
func (f *mergeFixture) outputDir() string {
	return filepath.Join(f.stateDir, "output")
}

// commandFiles lists the command files written to the ingress, wherever the
// ingress has retired them.
func (f *mergeFixture) commandFiles(t *testing.T) []string {
	t.Helper()
	var out []string
	for _, dir := range []string{"", "applied", "quarantine"} {
		matches, err := filepath.Glob(filepath.Join(f.outputDir(), dir, commandfile.DefaultGlob))
		if err != nil {
			t.Fatalf("glob the ingress: %v", err)
		}
		out = append(out, matches...)
	}
	return out
}

// retireTheCommand is the fake ingress retiring the command file into one of
// its ends: applied or quarantine.
func (f *mergeFixture) retireTheCommand(t *testing.T, end string) func() {
	return func() {
		files, err := filepath.Glob(filepath.Join(f.outputDir(), commandfile.DefaultGlob))
		if err != nil || len(files) != 1 {
			t.Errorf("the ingress holds %v (%v), want one command file", files, err)
			return
		}
		dir := filepath.Join(f.outputDir(), end)
		if err := os.MkdirAll(dir, 0o755); err != nil {
			t.Errorf("create %s: %v", end, err)
			return
		}
		if err := os.Rename(files[0], filepath.Join(dir, filepath.Base(files[0]))); err != nil {
			t.Errorf("retire the command file: %v", err)
		}
	}
}

// rosterRow is a row for dir in the given status.
func rosterRow(dir string, status string, mergedAt int64) *frontendv1.RosterRow {
	row := &frontendv1.RosterRow{
		Workspace: &frontendv1.RosterRowWorkspace{Workspace: &workspacev1.WorkspaceRef{Id: "ws-1", Dir: dir}},
	}
	switch status {
	case statusQueued:
		row.Status = &frontendv1.RosterRow_MergeQueued{MergeQueued: &frontendv1.RosterRowStatusMergeQueued{}}
	case statusMerging:
		row.Status = &frontendv1.RosterRow_Merging{Merging: &frontendv1.RosterRowStatusMerging{}}
	case statusMergeFail:
		row.Status = &frontendv1.RosterRow_MergeFailed{MergeFailed: &frontendv1.RosterRowStatusMergeFailed{}}
	case statusMergedDone:
		row.Status = &frontendv1.RosterRow_Merged{Merged: &frontendv1.RosterRowStatusMerged{}}
		row.When = &frontendv1.RosterRowWhen{Shown: &frontendv1.RosterRowWhen_Merged{Merged: &frontendv1.RosterRowWhenMerged{AtMs: mergedAt}}}
	case "thinking":
		row.Status = &frontendv1.RosterRow_Thinking{Thinking: &frontendv1.RosterRowStatusThinking{}}
	case "done":
		row.Status = &frontendv1.RosterRow_Done{Done: &frontendv1.RosterRowStatusDone{}}
	}
	return row
}

// roster is a roster holding rows in a repository section.
func roster(rows ...*frontendv1.RosterRow) *frontendv1.WorkspaceRoster {
	return &frontendv1.WorkspaceRoster{Repository: &frontendv1.RosterRepositoryView{
		Sections: []*frontendv1.RosterRepoSection{{Rows: &frontendv1.RosterRows{Rows: rows}}},
	}}
}

// frames turns rosters into script steps.
func frames(rosters ...*frontendv1.WorkspaceRoster) []scriptStep {
	steps := make([]scriptStep, 0, len(rosters))
	for _, r := range rosters {
		steps = append(steps, scriptStep{roster: r})
	}
	return steps
}

// failedFooter is a footer standing on merge_failed in one area.
func failedFooter(area string) *frontendv1.FooterView {
	failed := &frontendv1.FooterStatusMergeFailed{}
	switch area {
	case "conflicts":
		failed.Substatus = &frontendv1.FooterStatusMergeFailed_Conflicts{Conflicts: &frontendv1.FooterSubStatusMergeFailedConflicts{}}
	case "tests":
		failed.Substatus = &frontendv1.FooterStatusMergeFailed_Tests{Tests: &frontendv1.FooterSubStatusMergeFailedTests{}}
	case "other":
		failed.Substatus = &frontendv1.FooterStatusMergeFailed_Other{Other: &frontendv1.FooterSubStatusMergeFailedOther{}}
	}
	return &frontendv1.FooterView{Strip: &frontendv1.FooterStrip{Status: &frontendv1.FooterStatus{
		Status: &frontendv1.FooterStatus_MergeFailed{MergeFailed: failed}}}}
}

// writtenEntry reads back the one merge entry the verb wrote.
func (f *mergeFixture) writtenEntry(t *testing.T) commandfile.Entry {
	t.Helper()
	files := f.commandFiles(t)
	if len(files) != 1 {
		t.Fatalf("command files = %v, want one", files)
	}
	data, err := os.ReadFile(files[0])
	if err != nil {
		t.Fatalf("read the command file: %v", err)
	}
	var entries []commandfile.Entry
	if err := json.Unmarshal(data, &entries); err != nil || len(entries) != 1 {
		t.Fatalf("decode the command file = (%+v, %v), want one entry", entries, err)
	}
	return entries[0]
}

// appliedScript is a daemon whose ingress applies the command.
func (f *mergeFixture) appliedScript(t *testing.T) *fakeMergeDaemon {
	idle := roster(rosterRow(f.worktree, "thinking", 0))
	// THE SECOND FRAME IS THE RENDEZVOUS: its send blocks until the verb has
	// written the command and gone back to the stream.
	return &fakeMergeDaemon{script: []scriptStep{{roster: idle}, {roster: idle}, {act: f.retireTheCommand(t, "applied")}, {roster: idle}}}
}

// --- the command it writes ------------------------------------------------

func TestTheMergeQueueVerbAsksFromTheCallingWorkspace(t *testing.T) {
	// Arrange.
	f := newMergeFixture(t)
	f.daemons = []*fakeMergeDaemon{f.appliedScript(t)}

	// Act.
	code := f.run(t, "-own")

	// Assert.
	entry := f.writtenEntry(t)
	if code != exitSuccess || entry.Type != commandfile.TypeMerge || entry.ProjectDir != f.worktree || entry.Workspace != "ws-1" {
		t.Fatalf("exit %d, entry %+v; want a merge asked by the calling workspace %s", code, entry, f.worktree)
	}
}

func TestTheMergeQueueVerbNamesEachSource(t *testing.T) {
	tests := []struct {
		name  string
		args  func(f *mergeFixture) []string
		check func(f *mergeFixture, e commandfile.Entry) bool
	}{
		{name: "own branch", args: func(*mergeFixture) []string { return []string{"-own"} },
			check: func(_ *mergeFixture, e commandfile.Entry) bool {
				return !e.KeepOpen && e.Branch == "" && e.SourceDir == "" && !e.PRWasMerged
			}},
		{name: "own branch kept open", args: func(*mergeFixture) []string { return []string{"-own", "-keep-open"} },
			check: func(_ *mergeFixture, e commandfile.Entry) bool { return e.KeepOpen }},
		{name: "a branch", args: func(*mergeFixture) []string { return []string{"-branch", "agent-1/fix"} },
			check: func(_ *mergeFixture, e commandfile.Entry) bool { return e.Branch == "agent-1/fix" }},
		{name: "another workspace", args: func(f *mergeFixture) []string { return []string{"-dir", f.other} },
			check: func(f *mergeFixture, e commandfile.Entry) bool { return e.SourceDir == f.other }},
		{name: "merged upstream", args: func(*mergeFixture) []string { return []string{"-pr-merged"} },
			check: func(_ *mergeFixture, e commandfile.Entry) bool { return e.PRWasMerged }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newMergeFixture(t)
			f.daemons = []*fakeMergeDaemon{f.appliedScript(t)}

			// Act.
			code := f.run(t, tt.args(f)...)

			// Assert.
			if entry := f.writtenEntry(t); code != exitSuccess || !tt.check(f, entry) {
				t.Fatalf("exit %d, entry %+v; want the %s source", code, entry, tt.name)
			}
		})
	}
}

func TestTheMergeQueueVerbNamesTheRequestingWorktree(t *testing.T) {
	// Arrange.
	f := newMergeFixture(t)
	f.daemons = []*fakeMergeDaemon{f.appliedScript(t)}

	// Act.
	f.run(t, "-branch", "agent-1/fix")

	// Assert.
	if want := worktreeLinePrefix + canonicalDir(f.worktree) + "\n"; !strings.Contains(f.out.String(), want) {
		t.Fatalf("stdout %q does not carry %q", f.out.String(), want)
	}
}

func TestTheMergeQueueVerbRefusesAMalformedRequest(t *testing.T) {
	tests := []struct {
		name string
		args []string
	}{
		{name: "no source", args: nil},
		{name: "two sources", args: []string{"-own", "-branch", "b"}},
		{name: "keep-open off the own branch", args: []string{"-branch", "b", "-keep-open"}},
		{name: "a dir that is not there", args: []string{"-dir", "/no/such/worktree"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newMergeFixture(t)

			// Act.
			code := f.run(t, tt.args...)

			// Assert.
			if code != exitFailure || len(f.commandFiles(t)) != 0 || f.dialed != 0 {
				t.Fatalf("exit %d, stderr %q; want a refusal before any daemon is dialed", code, f.errOut.String())
			}
		})
	}
}

func TestTheMergeQueueVerbRefusesACallerInNoWorkspace(t *testing.T) {
	// Arrange: the roster holds only another workspace.
	f := newMergeFixture(t)
	f.daemons = []*fakeMergeDaemon{{script: frames(roster(rosterRow(f.other, "done", 0)))}}

	// Act.
	code := f.run(t, "-own")

	// Assert.
	if code != exitFailure || !strings.Contains(f.errOut.String(), "in no workspace") || len(f.commandFiles(t)) != 0 {
		t.Fatalf("exit %d, stderr %q; want the caller-in-no-workspace refusal", code, f.errOut.String())
	}
}

func TestTheMergeQueueVerbPicksTheDeepestWorkspaceHoldingTheCaller(t *testing.T) {
	// Arrange: a workspace nested inside another's tree.
	f := newMergeFixture(t)
	outer := rosterRow(filepath.Dir(f.worktree), "done", 0)
	outer.Workspace.Workspace.Id = "ws-outer"
	inner := rosterRow(f.worktree, "thinking", 0)
	f.daemons = []*fakeMergeDaemon{{script: []scriptStep{{roster: roster(outer, inner)}, {roster: roster(outer, inner)}, {act: f.retireTheCommand(t, "applied")}, {roster: roster(outer, inner)}}}}

	// Act.
	f.run(t, "-own")

	// Assert.
	if entry := f.writtenEntry(t); entry.Workspace != "ws-1" {
		t.Fatalf("entry %+v, want the innermost workspace ws-1", entry)
	}
}

// --- without -wait --------------------------------------------------------

func TestTheMergeQueueVerbReturnsOnceTheCommandIsApplied(t *testing.T) {
	// Arrange.
	f := newMergeFixture(t)
	f.daemons = []*fakeMergeDaemon{f.appliedScript(t)}

	// Act.
	code := f.run(t, "-own")

	// Assert.
	if code != exitSuccess || !strings.Contains(f.out.String(), "put in line once this turn ends") {
		t.Fatalf("exit %d, stdout %q; want the request reported applied", code, f.out.String())
	}
}

func TestTheMergeQueueVerbReportsARefusedCommand(t *testing.T) {
	// Arrange.
	f := newMergeFixture(t)
	idle := roster(rosterRow(f.worktree, "thinking", 0))
	f.daemons = []*fakeMergeDaemon{{script: []scriptStep{{roster: idle}, {roster: idle}, {act: f.retireTheCommand(t, "quarantine")}, {roster: idle}}}}

	// Act.
	code := f.run(t, "-branch", "no-such")

	// Assert.
	if code != exitMergeRefused || !strings.Contains(f.errOut.String(), "REFUSED") {
		t.Fatalf("exit %d, stderr %q; want the refusal", code, f.errOut.String())
	}
}

// --- with -wait -----------------------------------------------------------

func TestTheMergeQueueVerbRefusesToWaitWhileTheCallersTurnRuns(t *testing.T) {
	// Arrange.
	f := newMergeFixture(t)
	f.daemons = []*fakeMergeDaemon{{script: frames(roster(rosterRow(f.worktree, "thinking", 0)))}}

	// Act.
	code := f.run(t, "-own", "-wait")

	// Assert.
	if code != exitFailure || !strings.Contains(f.errOut.String(), "starts only when that turn ends") || len(f.commandFiles(t)) != 0 {
		t.Fatalf("exit %d, stderr %q; want the refusal and no command", code, f.errOut.String())
	}
}

func TestTheMergeQueueVerbReportsALanding(t *testing.T) {
	// Arrange.
	f := newMergeFixture(t)
	f.daemons = []*fakeMergeDaemon{{script: frames(
		roster(rosterRow(f.worktree, "done", 0)),
		roster(rosterRow(f.worktree, statusQueued, 0)),
		roster(rosterRow(f.worktree, statusMerging, 0)),
		roster(rosterRow(f.worktree, statusMergedDone, 7)),
	)}}

	// Act.
	code := f.run(t, "-branch", "agent-1/fix", "-wait")

	// Assert.
	if code != exitSuccess || !strings.Contains(f.out.String(), "LANDED") {
		t.Fatalf("exit %d, stdout %q; want the landing", code, f.out.String())
	}
}

func TestTheMergeQueueVerbReportsAFailureWithItsArea(t *testing.T) {
	tests := []struct{ area string }{{"conflicts"}, {"tests"}, {"other"}}
	for _, tt := range tests {
		t.Run(tt.area, func(t *testing.T) {
			// Arrange.
			f := newMergeFixture(t)
			daemon := &fakeMergeDaemon{script: frames(
				roster(rosterRow(f.worktree, "done", 0)),
				roster(rosterRow(f.worktree, statusMerging, 0)),
				roster(rosterRow(f.worktree, statusMergeFail, 0)),
			), footer: failedFooter(tt.area)}
			f.daemons = []*fakeMergeDaemon{daemon}

			// Act.
			code := f.run(t, "-own", "-wait")

			// Assert.
			if code != exitMergeFailed || !strings.Contains(f.out.String(), "FAILED ("+tt.area+")") {
				t.Fatalf("exit %d, stdout %q; want the failure in %s", code, f.out.String(), tt.area)
			}
			if daemon.footerRef.GetId() != "ws-1" {
				t.Fatalf("the footer was read for %v, want the requester ws-1", daemon.footerRef)
			}
		})
	}
}

func TestTheMergeQueueVerbSurfacesAnUnreadableFailedFooter(t *testing.T) {
	// Arrange.
	f := newMergeFixture(t)
	f.daemons = []*fakeMergeDaemon{{script: frames(
		roster(rosterRow(f.worktree, "done", 0)),
		roster(rosterRow(f.worktree, statusMergeFail, 0)),
	), footErr: errors.New("footer stream refused")}}

	// Act.
	code := f.run(t, "-own", "-wait")

	// Assert.
	if code != exitMergeFailed || !strings.Contains(f.errOut.String(), "footer stream refused") {
		t.Fatalf("exit %d, stderr %q; want failed with the footer's failure surfaced", code, f.errOut.String())
	}
}

func TestTheMergeQueueVerbIgnoresAnEarlierMergesConclusion(t *testing.T) {
	tests := []struct {
		name       string
		baseline   string
		baselineAt int64
	}{
		{name: "an earlier failure", baseline: statusMergeFail},
		{name: "an earlier landing", baseline: statusMergedDone, baselineAt: 3},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newMergeFixture(t)
			earlier := rosterRow(f.worktree, tc.baseline, tc.baselineAt)
			f.daemons = []*fakeMergeDaemon{{script: frames(
				roster(earlier),
				roster(earlier),
				roster(rosterRow(f.worktree, statusMerging, 0)),
				roster(rosterRow(f.worktree, statusMergedDone, 9)),
			)}}

			// Act.
			code := f.run(t, "-own", "-keep-open", "-wait")

			// Assert.
			if code != exitSuccess || !strings.Contains(f.out.String(), "LANDED") {
				t.Fatalf("exit %d, stdout %q; want THIS merge's landing", code, f.out.String())
			}
		})
	}
}

func TestTheMergeQueueVerbTakesAFreshLandingWithoutSeeingItInFlight(t *testing.T) {
	// Arrange.
	f := newMergeFixture(t)
	f.daemons = []*fakeMergeDaemon{{script: frames(
		roster(rosterRow(f.worktree, statusMergedDone, 3)),
		roster(rosterRow(f.worktree, statusMergedDone, 9)),
	)}}

	// Act.
	code := f.run(t, "-own", "-keep-open", "-wait")

	// Assert.
	if code != exitSuccess || !strings.Contains(f.out.String(), "LANDED") {
		t.Fatalf("exit %d, stdout %q; want the fresh landing", code, f.out.String())
	}
}

func TestTheMergeQueueVerbReattachesAcrossAPlannedStandDown(t *testing.T) {
	// Arrange.
	f := newMergeFixture(t)
	f.daemons = []*fakeMergeDaemon{
		{script: frames(
			roster(rosterRow(f.worktree, "done", 0)),
			roster(rosterRow(f.worktree, statusMerging, 0)),
		), end: errStreamEnding},
		{script: frames(roster(rosterRow(f.worktree, statusMergedDone, 5)))},
	}

	// Act.
	code := f.run(t, "-own", "-wait")

	// Assert.
	if code != exitSuccess || f.dialed != 2 {
		t.Fatalf("exit %d after %d dials, stderr %q; want the successor's landing", code, f.dialed, f.errOut.String())
	}
}

func TestTheMergeQueueVerbFailsLoudlyOnABrokenStream(t *testing.T) {
	// Arrange.
	f := newMergeFixture(t)
	f.daemons = []*fakeMergeDaemon{{script: frames(roster(rosterRow(f.worktree, "done", 0))), end: errors.New("connection reset")}}

	// Act.
	code := f.run(t, "-own", "-wait")

	// Assert.
	if code != exitFailure || !strings.Contains(f.errOut.String(), "connection reset") {
		t.Fatalf("exit %d, stderr %q; want the stream's failure", code, f.errOut.String())
	}
}

func TestTheMergeQueueVerbNeedsAServingDaemon(t *testing.T) {
	// Arrange.
	f := newMergeFixture(t)
	if err := os.Remove(filepath.Join(f.stateDir, "daemon.addr")); err != nil {
		t.Fatalf("remove daemon.addr: %v", err)
	}

	// Act.
	code := f.run(t, "-own")

	// Assert.
	if code != exitFailure || !strings.Contains(f.errOut.String(), "no daemon is serving") || len(f.commandFiles(t)) != 0 {
		t.Fatalf("exit %d, stderr %q; want no daemon serving and no command", code, f.errOut.String())
	}
}

func TestFailedAreaNamesEachArea(t *testing.T) {
	for _, area := range []string{"conflicts", "tests", "other"} {
		t.Run(area, func(t *testing.T) {
			// Act.
			got := failedArea(failedFooter(area).GetStrip().GetStatus().GetMergeFailed())

			// Assert.
			if got != area {
				t.Fatalf("failedArea = %q, want %q", got, area)
			}
		})
	}
}

// --- the pure helpers ------------------------------------------------------

func TestCanonicalDirResolvesAPathThatNoLongerExists(t *testing.T) {
	// Arrange: a symlinked parent (macOS's /var -> /private/var is one).
	real := t.TempDir()
	link := filepath.Join(t.TempDir(), "link")
	if err := os.Symlink(real, link); err != nil {
		t.Fatalf("symlink: %v", err)
	}

	// Act.
	got := canonicalDir(filepath.Join(link, "gone"))

	// Assert.
	want := filepath.Join(canonicalDir(real), "gone")
	if got != want {
		t.Fatalf("canonicalDir = %q, want %q", got, want)
	}
}

func TestRowStatusNamesTheArm(t *testing.T) {
	tests := []struct {
		name string
		row  *frontendv1.RosterRow
		want string
	}{
		{name: "absent", row: nil, want: statusAbsent},
		{name: "merging", row: rosterRow("/d", statusMerging, 0), want: statusMerging},
		{name: "merged", row: rosterRow("/d", statusMergedDone, 1), want: statusMergedDone},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := rowStatus(tc.row)

			// Assert.
			if got != tc.want {
				t.Fatalf("rowStatus = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestFindRosterRowLooksInTheRecentlyMergedSection(t *testing.T) {
	// Arrange: a landed workspace is hoisted out of its repository section.
	dir := t.TempDir()
	r := &frontendv1.WorkspaceRoster{RecentlyMerged: &frontendv1.RosterMergedSection{
		Rows: &frontendv1.RosterRows{Rows: []*frontendv1.RosterRow{rosterRow(dir, statusMergedDone, 1)}},
	}}

	// Act.
	row := findRosterRow(r, canonicalDir(dir))

	// Assert.
	if rowStatus(row) != statusMergedDone {
		t.Fatalf("found %v, want the recently merged row", row)
	}
}
