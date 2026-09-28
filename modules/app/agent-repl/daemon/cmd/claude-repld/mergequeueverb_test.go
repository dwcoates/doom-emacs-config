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
// action (the fake ingress quarantining the command file).
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

// mergeFixture is a state root with a daemon.addr, a worktree to merge, and
// the daemons the verb dials, in order.
type mergeFixture struct {
	stateDir string
	worktree string
	daemons  []*fakeMergeDaemon
	dialed   int
	cwd      string
	out      bytes.Buffer
	errOut   bytes.Buffer
}

func newMergeFixture(t *testing.T) *mergeFixture {
	t.Helper()
	f := &mergeFixture{stateDir: t.TempDir(), worktree: t.TempDir(), cwd: t.TempDir()}
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

// commandFiles lists the command files written to the ingress.
func (f *mergeFixture) commandFiles(t *testing.T) []string {
	t.Helper()
	matches, err := filepath.Glob(filepath.Join(f.outputDir(), commandfile.DefaultGlob))
	if err != nil {
		t.Fatalf("glob the ingress: %v", err)
	}
	return matches
}

// quarantineTheCommand is the fake ingress refusing the command file.
func (f *mergeFixture) quarantineTheCommand(t *testing.T) func() {
	return func() {
		files := f.commandFiles(t)
		if len(files) != 1 {
			t.Errorf("the ingress holds %d command files, want 1", len(files))
			return
		}
		dir := filepath.Join(f.outputDir(), "quarantine")
		if err := os.MkdirAll(dir, 0o755); err != nil {
			t.Errorf("create the quarantine: %v", err)
			return
		}
		if err := os.Rename(files[0], filepath.Join(dir, filepath.Base(files[0]))); err != nil {
			t.Errorf("quarantine the command file: %v", err)
		}
	}
}

// rosterRow is a row for dir in the given status.
func rosterRow(dir string, status string, mergedAt int64) *frontendv1.RosterRow {
	row := &frontendv1.RosterRow{
		Workspace: &frontendv1.RosterRowWorkspace{Workspace: &workspacev1.WorkspaceRef{Id: "ws-1", Dir: dir}},
	}
	switch status {
	case statusEnqueuing:
		row.Status = &frontendv1.RosterRow_MergeEnqueuing{MergeEnqueuing: &frontendv1.RosterRowStatusMergeEnqueuing{}}
	case statusQueued:
		row.Status = &frontendv1.RosterRow_MergeQueued{MergeQueued: &frontendv1.RosterRowStatusMergeQueued{}}
	case statusMerging:
		row.Status = &frontendv1.RosterRow_Merging{Merging: &frontendv1.RosterRowStatusMerging{}}
	case statusConflict:
		row.Status = &frontendv1.RosterRow_MergeConflict{MergeConflict: &frontendv1.RosterRowStatusMergeConflict{}}
	case statusMergeFail:
		row.Status = &frontendv1.RosterRow_MergeFailed{MergeFailed: &frontendv1.RosterRowStatusMergeFailed{}}
	case statusMergedDone:
		row.Status = &frontendv1.RosterRow_Merged{Merged: &frontendv1.RosterRowStatusMerged{}}
		row.When = &frontendv1.RosterRowWhen{Shown: &frontendv1.RosterRowWhen_Merged{Merged: &frontendv1.RosterRowWhenMerged{AtMs: mergedAt}}}
	case "done":
		row.Status = &frontendv1.RosterRow_Done{Done: &frontendv1.RosterRowStatusDone{}}
	}
	return row
}

// roster is a roster holding one row in a repository section, or none.
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

func parkedFooter(line string) *frontendv1.FooterView {
	return &frontendv1.FooterView{Strip: &frontendv1.FooterStrip{Status: &frontendv1.FooterStatus{
		Status: &frontendv1.FooterStatus_MergeConflict{MergeConflict: &frontendv1.FooterStatusMergeConflict{
			Substatus: &frontendv1.FooterStatusMergeConflict_Parked{Parked: &frontendv1.FooterSubStatusMergingParked{Line: line}},
		}},
	}}}
}

// --- the command it writes ----------------------------------------------

func TestTheMergeQueueVerbEnqueuesAWorkspaceThroughTheIngress(t *testing.T) {
	// Arrange.
	f := newMergeFixture(t)
	f.daemons = []*fakeMergeDaemon{{script: frames(
		roster(rosterRow(f.worktree, "done", 0)),
		roster(rosterRow(f.worktree, statusQueued, 0)),
	)}}

	// Act.
	code := f.run(t, "-dir", f.worktree)

	// Assert.
	files := f.commandFiles(t)
	if code != exitSuccess || len(files) != 1 {
		t.Fatalf("exit %d with %d command files, want 0 with one; stderr: %s", code, len(files), f.errOut.String())
	}
	var entries []commandfile.Entry
	data, err := os.ReadFile(files[0])
	if err != nil {
		t.Fatalf("read the command file: %v", err)
	}
	if err := json.Unmarshal(data, &entries); err != nil {
		t.Fatalf("decode the command file: %v", err)
	}
	if len(entries) != 1 || entries[0].Type != commandfile.TypeMerge || entries[0].ProjectDir != f.worktree {
		t.Fatalf("command file = %+v, want one merge of %s", entries, f.worktree)
	}
}

func TestTheMergeQueueVerbLandsABranchThroughAWorkspaceCutFromIt(t *testing.T) {
	// Arrange: a repository main worktree (a `.git` directory beside it).
	f := newMergeFixture(t)
	repo := filepath.Join(t.TempDir(), "repo")
	if err := os.MkdirAll(filepath.Join(repo, ".git"), 0o755); err != nil {
		t.Fatalf("make the repository: %v", err)
	}
	landing := filepath.Join(filepath.Dir(repo), "repo-worktrees", "hook-landing")
	f.daemons = []*fakeMergeDaemon{{script: frames(
		roster(),
		roster(rosterRow(landing, statusQueued, 0)),
	)}}

	// Act.
	code := f.run(t, "-branch", "feat/hook", "-repo", repo)

	// Assert.
	files := f.commandFiles(t)
	if code != exitSuccess || len(files) != 1 {
		t.Fatalf("exit %d with %d command files, want 0 with one; stderr: %s", code, len(files), f.errOut.String())
	}
	data, err := os.ReadFile(files[0])
	if err != nil {
		t.Fatalf("read the command file: %v", err)
	}
	var entries []commandfile.Entry
	if err := json.Unmarshal(data, &entries); err != nil {
		t.Fatalf("decode the command file: %v", err)
	}
	want := []commandfile.Entry{
		{Type: commandfile.TypeCreate, GitRoot: repo, Name: "merge-queue/hook-landing", BaseRef: "feat/hook"},
		{Type: commandfile.TypeMerge, ProjectDir: landing, Workspace: "merge-queue/hook-landing"},
	}
	if len(entries) != 2 || entries[0] != want[0] || entries[1] != want[1] {
		t.Fatalf("command file = %+v, want %+v", entries, want)
	}
}

func TestTheMergeQueueVerbRefusesAnEarlierLandingStillOnDisk(t *testing.T) {
	// Arrange.
	f := newMergeFixture(t)
	repo := filepath.Join(t.TempDir(), "repo")
	if err := os.MkdirAll(filepath.Join(repo, ".git"), 0o755); err != nil {
		t.Fatalf("make the repository: %v", err)
	}
	if err := os.MkdirAll(filepath.Join(filepath.Dir(repo), "repo-worktrees", "hook-landing"), 0o755); err != nil {
		t.Fatalf("make the earlier landing: %v", err)
	}

	// Act.
	code := f.run(t, "-branch", "feat/hook", "-repo", repo)

	// Assert.
	if code != exitFailure || !strings.Contains(f.errOut.String(), "already exists") || len(f.commandFiles(t)) != 0 {
		t.Fatalf("exit %d, stderr %q; want a refusal naming the earlier landing, and no command", code, f.errOut.String())
	}
}

func TestTheMergeQueueVerbRefusesAMalformedRequest(t *testing.T) {
	tests := []struct {
		name string
		args []string
		want string
	}{
		{name: "nothing named", args: nil, want: "name the merge"},
		{name: "both a dir and a branch", args: []string{"-dir", "/tmp", "-branch", "b", "-repo", "/tmp"}, want: "two different merges"},
		{name: "a branch without a repository", args: []string{"-branch", "b"}, want: "-branch needs -repo"},
		{name: "a repository with a dir", args: []string{"-dir", "/tmp", "-repo", "/tmp"}, want: "-repo goes with -branch"},
		{name: "a dir that is not on disk", args: []string{"-dir", "/nowhere/at/all"}, want: "not a worktree directory"},
		{name: "a stray argument", args: []string{"-dir", "/tmp", "extra"}, want: "unexpected arguments"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newMergeFixture(t)

			// Act.
			code := f.run(t, tc.args...)

			// Assert.
			if code != exitFailure || !strings.Contains(f.errOut.String(), tc.want) {
				t.Fatalf("exit %d, stderr %q; want exit %d naming %q", code, f.errOut.String(), exitFailure, tc.want)
			}
		})
	}
}

// --- waiting ----------------------------------------------------------------

func TestTheMergeQueueVerbRefusesToWaitOnItsOwnWorkspace(t *testing.T) {
	tests := []struct {
		name string
		sub  string
	}{
		{name: "from the worktree root", sub: ""},
		{name: "from beneath it", sub: "lisp"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the agent's own turn would be the one waiting.
			f := newMergeFixture(t)
			f.cwd = filepath.Join(f.worktree, tc.sub)
			if err := os.MkdirAll(f.cwd, 0o755); err != nil {
				t.Fatalf("make the working directory: %v", err)
			}

			// Act.
			code := f.run(t, "-dir", f.worktree, "-wait")

			// Assert.
			if code != exitFailure || !strings.Contains(f.errOut.String(), "starts only when its turn ends") || len(f.commandFiles(t)) != 0 {
				t.Fatalf("exit %d, stderr %q; want the own-workspace refusal and no command", code, f.errOut.String())
			}
		})
	}
}

func TestTheMergeQueueVerbReportsEachOutcome(t *testing.T) {
	tests := []struct {
		name     string
		outcome  string
		footer   *frontendv1.FooterView
		wantCode int
		wantOut  string
	}{
		{name: "landed", outcome: statusMergedDone, wantCode: exitSuccess, wantOut: "LANDED"},
		{name: "failed", outcome: statusMergeFail, wantCode: exitMergeFailed, wantOut: "daemon.merge.abort"},
		{name: "parked", outcome: statusConflict, footer: parkedFooter("parked for your input — 2 conflicts remain"),
			wantCode: exitMergeParked, wantOut: "PARKED: " + "WT" + ": parked for your input — 2 conflicts remain"},
		{name: "stopped on a conflict", outcome: statusConflict,
			footer: &frontendv1.FooterView{Strip: &frontendv1.FooterStrip{Status: &frontendv1.FooterStatus{
				Status: &frontendv1.FooterStatus_MergeConflict{MergeConflict: &frontendv1.FooterStatusMergeConflict{}},
			}}},
			wantCode: exitMergeParked, wantOut: "stopped on a conflict"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newMergeFixture(t)
			f.daemons = []*fakeMergeDaemon{{
				script: frames(
					roster(rosterRow(f.worktree, "done", 0)),
					roster(rosterRow(f.worktree, statusQueued, 0)),
					roster(rosterRow(f.worktree, statusMerging, 0)),
					roster(rosterRow(f.worktree, tc.outcome, 7)),
				),
				footer: tc.footer,
			}}

			// Act.
			code := f.run(t, "-dir", f.worktree, "-wait")

			// Assert.
			want := strings.ReplaceAll(tc.wantOut, "WT", filepath.Base(f.worktree))
			if code != tc.wantCode || !strings.Contains(f.out.String(), want) {
				t.Fatalf("exit %d, stdout %q, stderr %q; want exit %d naming %q", code, f.out.String(), f.errOut.String(), tc.wantCode, want)
			}
		})
	}
}

func TestTheMergeQueueVerbAsksTheParkedWorkspacesOwnFooter(t *testing.T) {
	// Arrange.
	f := newMergeFixture(t)
	daemon := &fakeMergeDaemon{
		script: frames(
			roster(rosterRow(f.worktree, "done", 0)),
			roster(rosterRow(f.worktree, statusMerging, 0)),
			roster(rosterRow(f.worktree, statusConflict, 0)),
		),
		footer: parkedFooter("parked"),
	}
	f.daemons = []*fakeMergeDaemon{daemon}

	// Act.
	f.run(t, "-dir", f.worktree, "-wait")

	// Assert.
	if daemon.footerRef.GetId() != "ws-1" {
		t.Fatalf("the footer was read for %v, want the row's own workspace ws-1", daemon.footerRef)
	}
}

func TestTheMergeQueueVerbSurfacesAnUnreadableParkedFooter(t *testing.T) {
	// Arrange.
	f := newMergeFixture(t)
	f.daemons = []*fakeMergeDaemon{{
		script: frames(
			roster(rosterRow(f.worktree, "done", 0)),
			roster(rosterRow(f.worktree, statusMerging, 0)),
			roster(rosterRow(f.worktree, statusConflict, 0)),
		),
		footErr: errors.New("footer stream refused"),
	}}

	// Act.
	code := f.run(t, "-dir", f.worktree, "-wait")

	// Assert.
	if code != exitMergeParked || !strings.Contains(f.errOut.String(), "footer stream refused") {
		t.Fatalf("exit %d, stderr %q; want parked with the footer's failure surfaced", code, f.errOut.String())
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
			// Arrange: the row still shows the earlier merge's end when the
			// command is written; this merge then runs and lands.
			f := newMergeFixture(t)
			earlier := rosterRow(f.worktree, tc.baseline, tc.baselineAt)
			f.daemons = []*fakeMergeDaemon{{script: frames(
				roster(earlier),
				roster(earlier),
				roster(rosterRow(f.worktree, statusMerging, 0)),
				roster(rosterRow(f.worktree, statusMergedDone, 9)),
			)}}

			// Act.
			code := f.run(t, "-dir", f.worktree, "-wait")

			// Assert.
			if code != exitSuccess || !strings.Contains(f.out.String(), "LANDED") {
				t.Fatalf("exit %d, stdout %q; want THIS merge's landing, not the earlier end", code, f.out.String())
			}
		})
	}
}

func TestTheMergeQueueVerbTakesAFreshLandingWithoutSeeingItInFlight(t *testing.T) {
	// Arrange: a merge that concluded between two frames (already on its
	// target) still differs from the baseline by its merged instant.
	f := newMergeFixture(t)
	f.daemons = []*fakeMergeDaemon{{script: frames(
		roster(rosterRow(f.worktree, statusMergedDone, 3)),
		roster(rosterRow(f.worktree, statusMergedDone, 9)),
	)}}

	// Act.
	code := f.run(t, "-dir", f.worktree, "-wait")

	// Assert.
	if code != exitSuccess || !strings.Contains(f.out.String(), "LANDED") {
		t.Fatalf("exit %d, stdout %q; want the fresh landing", code, f.out.String())
	}
}

func TestTheMergeQueueVerbReportsARefusedCommand(t *testing.T) {
	// Arrange: the ingress quarantines the command; the roster never moves.
	f := newMergeFixture(t)
	idle := roster(rosterRow(f.worktree, "done", 0))
	f.daemons = []*fakeMergeDaemon{{script: []scriptStep{
		{roster: idle},
		{roster: idle},
		{act: f.quarantineTheCommand(t)},
		{roster: idle},
	}}}

	// Act.
	code := f.run(t, "-dir", f.worktree, "-wait")

	// Assert.
	if code != exitMergeRefused || !strings.Contains(f.errOut.String(), "REFUSED") {
		t.Fatalf("exit %d, stderr %q; want the refusal", code, f.errOut.String())
	}
}

func TestTheMergeQueueVerbReattachesAcrossAPlannedStandDown(t *testing.T) {
	// Arrange: the first daemon hands over mid-merge; its successor lands it.
	f := newMergeFixture(t)
	f.daemons = []*fakeMergeDaemon{
		{script: frames(
			roster(rosterRow(f.worktree, "done", 0)),
			roster(rosterRow(f.worktree, statusMerging, 0)),
		), end: errStreamEnding},
		{script: frames(roster(rosterRow(f.worktree, statusMergedDone, 5)))},
	}

	// Act.
	code := f.run(t, "-dir", f.worktree, "-wait")

	// Assert.
	if code != exitSuccess || f.dialed != 2 || len(f.commandFiles(t)) != 1 {
		t.Fatalf("exit %d after %d dials with %d command files; want a landing through the successor and ONE command",
			code, f.dialed, len(f.commandFiles(t)))
	}
}

func TestTheMergeQueueVerbFailsLoudlyOnABrokenStream(t *testing.T) {
	// Arrange.
	f := newMergeFixture(t)
	f.daemons = []*fakeMergeDaemon{{
		script: frames(roster(rosterRow(f.worktree, "done", 0))),
		end:    errors.New("connection reset"),
	}}

	// Act.
	code := f.run(t, "-dir", f.worktree, "-wait")

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
	code := f.run(t, "-dir", f.worktree)

	// Assert.
	if code != exitFailure || !strings.Contains(f.errOut.String(), "no daemon is serving") || len(f.commandFiles(t)) != 0 {
		t.Fatalf("exit %d, stderr %q; want no daemon serving and no command", code, f.errOut.String())
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
