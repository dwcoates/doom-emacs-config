package storagesweep

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"testing"
	"time"

	"agent-repld/internal/clock/clocktest"
	"agent-repld/internal/dlog"
	"agent-repld/internal/sessionlock"
	"agent-repld/internal/wsm"
)

var now = time.Date(2026, 10, 25, 12, 0, 0, 0, time.UTC)

// registry is the scripted state client.
type registry struct {
	workspaces []wsm.Workspace
	err        error
}

func (r *registry) ListWorkspaces(context.Context) ([]wsm.Workspace, error) {
	return r.workspaces, r.err
}

// world is one sweep's state root, registry, clock and log.
type world struct {
	t     *testing.T
	root  string
	reg   *registry
	clock *clocktest.Scripted
	log   *dlog.TestLogger
	aged  []AgedDir
}

func newWorld(t *testing.T) *world {
	t.Helper()
	return &world{t: t, root: t.TempDir(), reg: &registry{}, clock: clocktest.New(now), log: dlog.NewTestLogger()}
}

func (w *world) shimDir() string { return filepath.Join(w.root, "shim") }

func (w *world) sweeper() *Sweeper {
	w.t.Helper()
	s, err := New(Deps{
		Registry: w.reg, Clock: w.clock, LockPath: filepath.Join(w.root, "run", "sweep.lock"),
		ShimDir: w.shimDir(), Aged: w.aged,
		LandedAfter: DefaultLandedAfter, ClosedAfter: DefaultClosedAfter,
		StartDelay: DefaultStartDelay, Every: DefaultEvery, Log: w.log,
	})
	if err != nil {
		w.t.Fatalf("New: %v", err)
	}
	return s
}

func (w *world) sweep(dryRun bool) Report {
	w.t.Helper()
	report, err := w.sweeper().Sweep(context.Background(), dryRun)
	if err != nil {
		w.t.Fatalf("Sweep: %v", err)
	}
	return report
}

// workspace registers a workspace in dir and makes its shim directory, with
// an identity and one backup, last written at written.
func (w *world) workspace(dir string, closed bool, closure *wsm.Closure, written time.Time) string {
	w.t.Helper()
	w.reg.workspaces = append(w.reg.workspaces, wsm.Workspace{Dir: dir, Closed: closed, Closure: closure})
	key, err := sessionlock.WorkspaceKey(dir)
	if err != nil {
		w.t.Fatalf("WorkspaceKey: %v", err)
	}
	return w.shim(key, written)
}

// shim makes one shim directory with an identity and one backup.
func (w *world) shim(key string, written time.Time) string {
	w.t.Helper()
	dir := filepath.Join(w.shimDir(), key)
	w.write(filepath.Join(dir, "agent-id.json"), "{}\n", written)
	w.write(filepath.Join(dir, backupsDir, "v-1.jsonl"), "0123456789", written)
	w.touch(filepath.Join(dir, backupsDir), written)
	w.touch(dir, written)
	return dir
}

func (w *world) write(path, body string, at time.Time) {
	w.t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		w.t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
		w.t.Fatal(err)
	}
	w.touch(path, at)
}

func (w *world) touch(path string, at time.Time) {
	w.t.Helper()
	if err := os.Chtimes(path, at, at); err != nil {
		w.t.Fatal(err)
	}
}

func exists(path string) bool {
	_, err := os.Lstat(path)
	return err == nil
}

func closedBy(by wsm.CloseCause, ago time.Duration) *wsm.Closure {
	return &wsm.Closure{At: now.Add(-ago), By: by}
}

func (w *world) errors() []dlog.Record {
	var out []dlog.Record
	for _, r := range w.log.Records() {
		if r.Level == dlog.LevelError {
			out = append(out, r)
		}
	}
	return out
}

const day = 24 * time.Hour

// --- the shim rule -------------------------------------------------------

func TestTheShimRule(t *testing.T) {
	tests := []struct {
		name    string
		closed  bool
		closure *wsm.Closure
		written time.Time
		// wantDir and wantBackups are whether each survives the sweep.
		wantDir, wantBackups bool
		wantReason           Reason
	}{
		{"an open workspace keeps everything, however old", false, nil, now.Add(-400 * day), true, true, ""},
		{"a landed workspace within its window keeps everything", true, closedBy(wsm.CloseByMerge, 2*day), now.Add(-2 * day), true, true, ""},
		{"a landed workspace past its window loses its whole directory", true, closedBy(wsm.CloseByMerge, 4*day), now.Add(-4 * day), false, false, ReasonLanded},
		{"a closed workspace within its window keeps everything", true, closedBy(wsm.CloseByUser, 29*day), now.Add(-29 * day), true, true, ""},
		{"a closed workspace past its window loses only its backups", true, closedBy(wsm.CloseByUser, 31*day), now.Add(-31 * day), true, false, ReasonClosedBackups},
		{"an agent's close is a close, not a landing", true, closedBy(wsm.CloseByAgent, 4*day), now.Add(-4 * day), true, true, ""},
		{"an undated close counts from the last write, kept while recent", true, nil, now.Add(-29 * day), true, true, ""},
		{"an undated close counts from the last write, swept once old", true, nil, now.Add(-31 * day), true, false, ReasonClosedBackups},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			w := newWorld(t)
			dir := w.workspace("/ws/one", tt.closed, tt.closure, tt.written)

			// Act.
			report := w.sweep(false)

			// Assert.
			if exists(dir) != tt.wantDir || exists(filepath.Join(dir, backupsDir)) != tt.wantBackups {
				t.Fatalf("dir %v backups %v, want dir %v backups %v", exists(dir), exists(filepath.Join(dir, backupsDir)), tt.wantDir, tt.wantBackups)
			}
			var reasons []Reason
			for _, r := range report.Removed {
				reasons = append(reasons, r.Reason)
			}
			if tt.wantReason == "" && len(reasons) != 0 || tt.wantReason != "" && !slices.Equal(reasons, []Reason{tt.wantReason}) {
				t.Fatalf("removed for %v, want %q", reasons, tt.wantReason)
			}
		})
	}
}

func TestAShimDirectoryNoWorkspaceKeysToIsDeleted(t *testing.T) {
	// Arrange: the workspace was forgotten (nuked), so no row keys to it.
	w := newWorld(t)
	dir := w.shim("deadbeef", now)

	// Act.
	report := w.sweep(false)

	// Assert.
	if exists(dir) || len(report.Removed) != 1 || report.Removed[0].Reason != ReasonNoWorkspace {
		t.Fatalf("dir exists %v, removed %+v, want it deleted as an orphan", exists(dir), report.Removed)
	}
}

func TestADirectorySharedByAnOpenWorkspaceIsKept(t *testing.T) {
	// Arrange: a landed workspace's directory reused by an open one.
	w := newWorld(t)
	dir := w.workspace("/ws/reused", true, closedBy(wsm.CloseByMerge, 10*day), now.Add(-10*day))
	w.reg.workspaces = append(w.reg.workspaces, wsm.Workspace{Dir: "/ws/reused"})

	// Act.
	w.sweep(false)

	// Assert.
	if !exists(filepath.Join(dir, backupsDir)) {
		t.Fatal("the backups of a directory an open workspace uses were deleted")
	}
}

func TestADirectorySharedByTwoClosesTakesTheLatestAndTheLeast(t *testing.T) {
	// Arrange: an old landing and a recent user close of the same directory.
	w := newWorld(t)
	dir := w.workspace("/ws/reused", true, closedBy(wsm.CloseByMerge, 40*day), now.Add(-40*day))
	w.reg.workspaces = append(w.reg.workspaces, wsm.Workspace{Dir: "/ws/reused", Closed: true, Closure: closedBy(wsm.CloseByUser, 31*day)})

	// Act.
	w.sweep(false)

	// Assert: past both windows, but the user close only allows the backups.
	if !exists(dir) || exists(filepath.Join(dir, backupsDir)) {
		t.Fatalf("dir %v backups %v, want the directory kept and its backups gone", exists(dir), exists(filepath.Join(dir, backupsDir)))
	}
}

func TestAClosedDirectoryWithNoBackupsIsKept(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	dir := w.workspace("/ws/one", true, closedBy(wsm.CloseByUser, 40*day), now.Add(-40*day))
	if err := os.RemoveAll(filepath.Join(dir, backupsDir)); err != nil {
		t.Fatal(err)
	}

	// Act.
	report := w.sweep(false)

	// Assert.
	if !exists(dir) || len(report.Removed) != 0 || report.Kept[KeepNoBackups] != 1 {
		t.Fatalf("removed %+v kept %v, want nothing deleted", report.Removed, report.Kept)
	}
}

func TestAFileInTheShimDirectoryIsKept(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	file := filepath.Join(w.shimDir(), "stray")
	w.write(file, "x", now.Add(-400*day))

	// Act.
	report := w.sweep(false)

	// Assert.
	if !exists(file) || report.Kept[KeepNotADirectory] != 1 {
		t.Fatalf("file exists %v, kept %v, want it kept", exists(file), report.Kept)
	}
}

func TestAMissingShimDirectoryIsNothingToSweep(t *testing.T) {
	// Arrange.
	w := newWorld(t)

	// Act.
	report := w.sweep(false)

	// Assert.
	if len(report.Removed) != 0 || report.Failures != 0 || len(w.errors()) != 0 {
		t.Fatalf("report %+v errors %v, want a quiet empty sweep", report, w.errors())
	}
}

func TestAnUnreadableRegistryDeletesNoShimState(t *testing.T) {
	// Arrange: an orphan that would otherwise go.
	w := newWorld(t)
	dir := w.shim("deadbeef", now)
	w.reg.err = errors.New("db gone")

	// Act.
	report, err := w.sweeper().Sweep(context.Background(), false)

	// Assert.
	if err == nil || !exists(dir) || report.Failures != 1 {
		t.Fatalf("Sweep = (%+v, %v), dir exists %v, want the failure and nothing deleted", report, err, exists(dir))
	}
	if errs := w.errors(); len(errs) != 1 || errs[0].Operation != opShim || errs[0].Context["cause"] != "db gone" {
		t.Fatalf("errors %+v, want one naming the cause", errs)
	}
}

func TestAnUnkeyableWorkspaceDeletesNoShimState(t *testing.T) {
	// Arrange: a row with no directory names no key, so no directory can be
	// shown to be no workspace's.
	w := newWorld(t)
	dir := w.shim("deadbeef", now)
	w.reg.workspaces = []wsm.Workspace{{Dir: ""}}

	// Act.
	report := w.sweep(false)

	// Assert.
	if !exists(dir) || report.Failures != 1 || len(w.errors()) != 1 {
		t.Fatalf("dir exists %v, report %+v, errors %v, want nothing deleted and one ERROR", exists(dir), report, w.errors())
	}
}

func TestAnUnlistableShimDirectoryIsAnError(t *testing.T) {
	// Arrange: the shim directory is a file.
	w := newWorld(t)
	w.write(w.shimDir(), "x", now)

	// Act.
	report := w.sweep(false)

	// Assert.
	if report.Failures != 1 || len(w.errors()) != 1 || w.errors()[0].Context["dir"] != w.shimDir() {
		t.Fatalf("report %+v errors %v, want one ERROR naming the directory", report, w.errors())
	}
}

// --- the aged rule -------------------------------------------------------

func TestTheAgedRule(t *testing.T) {
	// Arrange: one entry each side of the age, and one kept by name.
	w := newWorld(t)
	dir := filepath.Join(w.root, "profiles")
	old := filepath.Join(dir, "20260901T000000Z")
	young := filepath.Join(dir, "20261020T000000Z")
	active := filepath.Join(dir, "20260801T000000Z")
	w.write(filepath.Join(old, "cpu.pprof"), "12345", now.Add(-40*day))
	w.touch(old, now.Add(-40*day))
	w.write(filepath.Join(young, "cpu.pprof"), "1", now.Add(-5*day))
	w.write(filepath.Join(active, "cpu.pprof"), "1", now.Add(-90*day))
	w.touch(active, now.Add(-90*day))
	w.aged = []AgedDir{{Name: "profiles", Dir: dir, After: DefaultAgedAfter, Keep: func() (map[string]bool, error) {
		return map[string]bool{"20260801T000000Z": true}, nil
	}}}

	// Act.
	report := w.sweep(false)

	// Assert.
	if exists(old) || !exists(young) || !exists(active) {
		t.Fatalf("old %v young %v active %v, want only the old entry deleted", exists(old), exists(young), exists(active))
	}
	if len(report.Removed) != 1 || report.Removed[0] != (Removal{Path: old, Reason: ReasonAged, Bytes: 5}) {
		t.Fatalf("removed %+v, want the old entry with its 5 bytes", report.Removed)
	}
	if report.Kept[KeepYoung] != 1 || report.Kept[KeepKeptByDirectory] != 1 {
		t.Fatalf("kept %v, want one young and one marked", report.Kept)
	}
}

func TestAnAgedDirectoryWhoseKeepFailsIsNotSwept(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	dir := filepath.Join(w.root, "profiles")
	old := filepath.Join(dir, "old")
	w.write(old, "x", now.Add(-40*day))
	w.aged = []AgedDir{{Name: "profiles", Dir: dir, After: DefaultAgedAfter, Keep: func() (map[string]bool, error) {
		return nil, errors.New("active unreadable")
	}}}

	// Act.
	report := w.sweep(false)

	// Assert.
	if !exists(old) || report.Failures != 1 || w.errors()[0].Context["cause"] != "active unreadable" {
		t.Fatalf("old exists %v report %+v errors %v, want nothing deleted and the cause recorded", exists(old), report, w.errors())
	}
}

func TestAMissingAgedDirectoryIsNothingToSweep(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	w.aged = []AgedDir{{Name: "test-passes", Dir: filepath.Join(w.root, "absent"), After: DefaultAgedAfter}}

	// Act.
	report := w.sweep(false)

	// Assert.
	if report.Failures != 0 || len(w.errors()) != 0 {
		t.Fatalf("report %+v errors %v, want a quiet sweep", report, w.errors())
	}
}

// --- the dry run, the records and the guard ------------------------------

func TestADryRunDeletesNothingAndReportsWhatWouldGo(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	dir := w.shim("deadbeef", now)

	// Act.
	report := w.sweep(true)

	// Assert.
	if !exists(dir) || !report.DryRun || len(report.Removed) != 1 || report.Removed[0].Bytes != 13 {
		t.Fatalf("dir exists %v, report %+v, want it reported with its 13 bytes and kept", exists(dir), report)
	}
}

func TestEveryDeletionIsRecorded(t *testing.T) {
	// Arrange.
	w := newWorld(t)
	dir := w.shim("deadbeef", now)

	// Act.
	w.sweep(false)

	// Assert.
	var found bool
	for _, r := range w.log.Records() {
		if r.Operation == opShim && r.Level == dlog.LevelInfo && r.Context["path"] == dir && r.Context["reason"] == ReasonNoWorkspace && r.Context["bytes"] == int64(13) {
			found = true
		}
	}
	if !found {
		t.Fatalf("records %+v, want an INFO naming the deleted directory, its reason and bytes", w.log.Records())
	}
}

func TestADeletionThatFailsIsAnErrorAndTheSweepGoesOn(t *testing.T) {
	// Arrange: an orphan the sweep cannot delete, then one it can.
	w := newWorld(t)
	stuck := w.shim("0000aaaa", now)
	freed := w.shim("ffffbbbb", now)
	if err := os.Chmod(stuck, 0o500); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { os.Chmod(stuck, 0o755) })

	// Act.
	report := w.sweep(false)

	// Assert.
	if !exists(stuck) || exists(freed) || report.Failures != 1 {
		t.Fatalf("stuck %v freed %v report %+v, want the failure counted and the next deleted", exists(stuck), exists(freed), report)
	}
	if errs := w.errors(); len(errs) != 1 || errs[0].Context["path"] != stuck {
		t.Fatalf("errors %+v, want one naming the stuck directory", errs)
	}
}

func TestASweepHeldByAnotherDaemonDeletesNothing(t *testing.T) {
	// Arrange: a second sweeper on the same lock sweeps while the first runs.
	w := newWorld(t)
	dir := w.shim("deadbeef", now)
	outer := w.sweeper()
	inner := w.sweeper()
	var err error

	// Act.
	outer.guard.Do(w.log, func() { _, err = inner.Sweep(context.Background(), false) })

	// Assert.
	if !exists(dir) || !errors.Is(err, ErrHeldElsewhere) {
		t.Fatalf("dir exists %v, err %v, want nothing deleted and ErrHeldElsewhere", exists(dir), err)
	}
}

func TestNewRefusesAZeroValue(t *testing.T) {
	tests := []struct {
		name string
		edit func(*Deps)
	}{
		{"no registry", func(d *Deps) { d.Registry = nil }},
		{"no clock", func(d *Deps) { d.Clock = nil }},
		{"no lock", func(d *Deps) { d.LockPath = "" }},
		{"no shim directory", func(d *Deps) { d.ShimDir = "" }},
		{"no log", func(d *Deps) { d.Log = nil }},
		{"a zero window", func(d *Deps) { d.ClosedAfter = 0 }},
		{"an unnamed aged directory", func(d *Deps) { d.Aged = []AgedDir{{Dir: "/d", After: day}} }},
		{"an aged directory with no age", func(d *Deps) { d.Aged = []AgedDir{{Name: "n", Dir: "/d"}} }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			deps := Deps{
				Registry: &registry{}, Clock: clocktest.New(now), LockPath: "/l", ShimDir: "/s",
				LandedAfter: day, ClosedAfter: day, StartDelay: day, Every: day, Log: dlog.NewTestLogger(),
			}
			tt.edit(&deps)

			// Act.
			_, err := New(deps)

			// Assert.
			if err == nil {
				t.Fatal("New = nil error, want a refusal")
			}
		})
	}
}
