package discover

import (
	"os"
	"path/filepath"
	"testing"
	"time"
)

// THE DIRECTORY MTIMES THESE SUBJECTS DEPEND ON ARE SET EXPLICITLY, never left
// to the filesystem's own clock. The probe's whole decision is "did this
// directory's mtime move since I last enumerated it", and a filesystem whose
// timestamp granularity swallowed a write inside one tick would make a subject
// about the CHANGED arm pass or fail on the host's clock rather than on the
// code. bumpDir states the change the subject is about; holdDir states the
// absence of one.
func bumpDir(t *testing.T, dir string, by time.Duration) {
	t.Helper()
	info, err := os.Stat(dir)
	if err != nil {
		t.Fatalf("stat %s: %v", dir, err)
	}
	stamp := info.ModTime().Add(by)
	if err := os.Chtimes(dir, stamp, stamp); err != nil {
		t.Fatalf("stamping %s: %v", dir, err)
	}
}

// holdDir restores a directory to the mtime it had before, which is what a
// filesystem whose granularity did not notice the write looks like — and the
// exact condition the "unchanged directory is not re-enumerated" subject needs.
func holdDir(t *testing.T, dir string) func() {
	t.Helper()
	info, err := os.Stat(dir)
	if err != nil {
		t.Fatalf("stat %s: %v", dir, err)
	}
	stamp := info.ModTime()
	return func() {
		t.Helper()
		if err := os.Chtimes(dir, stamp, stamp); err != nil {
			t.Fatalf("restoring the mtime of %s: %v", dir, err)
		}
	}
}

// probed flattens one probe's changed directories into the targets they held.
func probed(changed []ChangedDir) []Target {
	var out []Target
	for _, dir := range changed {
		out = append(out, dir.Targets...)
	}
	return out
}

func TestChangeProbeFindsANewFileInAKnownProjectDirectory(t *testing.T) {
	// Arrange: one scan has already enumerated the project directory, so it is
	// a probe candidate at the mtime it had then.
	d, base, _, _ := fixture(t, "config-a/projects/proj/sess-1.jsonl")
	d.Scan()
	project := filepath.Join(base, "config-a", "projects", "proj")
	write(t, filepath.Join(project, "sess-2.jsonl"))
	bumpDir(t, project, time.Second)

	// Act: one poll tick's probe, with no rescan in between.
	got := find(t, probed(d.ScanChanged()), "sess-2.jsonl")

	// Assert.
	if got.SessionID != "sess-2" {
		t.Fatalf("target = %+v, want the newly written transcript identified sess-2", got)
	}
}

func TestChangeProbeFindsATranscriptInABrandNewProjectDirectory(t *testing.T) {
	// Arrange: the fresh-workspace case the realtest caught — the project
	// directory itself did not exist when the last scan ran.
	d, base, _, _ := fixture(t, "config-a/projects/proj/sess-1.jsonl")
	d.Scan()
	projects := filepath.Join(base, "config-a", "projects")
	write(t, filepath.Join(projects, "fresh", "sess-9.jsonl"))
	bumpDir(t, projects, time.Second)

	// Act.
	got := find(t, probed(d.ScanChanged()), "sess-9.jsonl")

	// Assert.
	if got.SessionID != "sess-9" {
		t.Fatalf("target = %+v, want the fresh workspace's first transcript", got)
	}
}

func TestChangeProbeDoesNotEnumerateAnUnchangedDirectory(t *testing.T) {
	// Arrange: a file appears, but the directory's mtime does not move — which
	// is the only fact the probe is allowed to act on. A probe that re-globbed
	// regardless would find it, and would be the blanket per-second rescan this
	// design exists to avoid.
	d, base, _, _ := fixture(t, "config-a/projects/proj/sess-1.jsonl")
	d.Scan()
	project := filepath.Join(base, "config-a", "projects", "proj")
	restore := holdDir(t, project)
	write(t, filepath.Join(project, "sess-2.jsonl"))
	restore()

	// Act.
	changed := d.ScanChanged()

	// Assert.
	if len(changed) != 0 {
		t.Fatalf("the probe enumerated %d unchanged director(ies): %+v", len(changed), changed)
	}
}

func TestChangeProbeSkipsAVanishedDirectoryWithoutAnErrorRecord(t *testing.T) {
	// Arrange: a spool tree is scanned, then deleted — which is what a reaped
	// task looks like, and is ordinary rather than a fault.
	d, base, _, logs := fixture(t, "spool/claude-501/proj/sess-1/tasks/b1.output")
	d.Scan()
	if err := os.RemoveAll(filepath.Join(base, "spool", "claude-501")); err != nil {
		t.Fatalf("removing the spool tree: %v", err)
	}

	// Act.
	d.ScanChanged()

	// Assert: the vanishing states nothing at all.
	records := parseLogLines(t, *logs)
	for _, level := range []string{"warn", "error"} {
		if got := opsAt(records, "discover-change", level); len(got) != 0 {
			t.Fatalf("a vanished directory wrote %d discover-change record(s) at %s: %+v", len(got), level, got)
		}
	}
}

func TestChangeProbeForgetsAVanishedDirectory(t *testing.T) {
	// Arrange.
	d, base, _, _ := fixture(t, "spool/claude-501/proj/sess-1/tasks/b1.output")
	d.Scan()
	tasks := Normalize(filepath.Join(base, "spool", "claude-501", "proj", "sess-1", "tasks"))
	if err := os.RemoveAll(tasks); err != nil {
		t.Fatalf("removing the tasks directory: %v", err)
	}

	// Act.
	d.ScanChanged()

	// Assert: a directory that is gone stops costing a stat every poll.
	if _, known := d.probed[tasks]; known {
		t.Fatalf("the vanished tasks directory is still a probe candidate")
	}
}

func TestChangeProbeFindsANewSpoolInAKnownTasksDirectory(t *testing.T) {
	// Arrange.
	d, base, _, _ := fixture(t, "spool/claude-501/proj/sess-1/tasks/b1.output")
	d.Scan()
	tasks := filepath.Join(base, "spool", "claude-501", "proj", "sess-1", "tasks")
	write(t, filepath.Join(tasks, "b2.output"))
	bumpDir(t, tasks, time.Second)

	// Act.
	got := find(t, probed(d.ScanChanged()), "b2.output")

	// Assert.
	if got.TaskID != "b2" {
		t.Fatalf("target = %+v, want the newly written spool b2", got)
	}
}

func TestChangeProbeStatesEveryTickAtDebug(t *testing.T) {
	// Arrange: a probe that found nothing at all.
	d, _, _, logs := fixture(t, "config-a/projects/proj/sess-1.jsonl")
	d.Scan()
	*logs = nil

	// Act.
	d.ScanChanged()

	// Assert: the per-tick record exists and is debug, so a 1Hz probe never
	// contributes a line to a normal-verbosity log.
	got := opsAt(parseLogLines(t, *logs), "discover-change", "")
	if len(got) != 1 {
		t.Fatalf("one probe wrote %d discover-change record(s), want exactly one", len(got))
	}
	if got[0].Level != "debug" {
		t.Fatalf("the per-tick probe record is level %q, want debug", got[0].Level)
	}
}

func TestChangeProbeStatesAnUnreadableDirectoryOnce(t *testing.T) {
	// Arrange: a directory that is REAL and cannot be read is not a vanishing —
	// every file under it is now found only by the full rescan, which is a fault
	// an operator has to see, stated once rather than once per poll.
	if os.Geteuid() == 0 {
		t.Skip("root reads a 0o000 directory, so the refusal this subject needs cannot be staged")
	}
	d, base, _, logs := fixture(t, "config-a/projects/proj/sess-1.jsonl")
	d.Scan()
	project := filepath.Join(base, "config-a", "projects", "proj")
	if err := os.Chmod(project, 0o000); err != nil {
		t.Fatalf("chmod %s: %v", project, err)
	}
	t.Cleanup(func() { _ = os.Chmod(project, 0o755) })
	bumpDir(t, project, time.Second)

	// Act: two ticks of the same standing condition.
	d.ScanChanged()
	d.ScanChanged()

	// Assert.
	rec := requireOnceIn(t, parseLogLines(t, *logs), "discover-change", "warn")
	if got := ctxString(t, rec, "path"); got != Normalize(project) {
		t.Fatalf("the refusal names path %q, want the unreadable directory %q", got, Normalize(project))
	}
}

func TestScanRegistersTheProjectsRootItself(t *testing.T) {
	// Arrange: a config root whose projects directory holds nothing yet. It has
	// no match to be registered from, and it is precisely where a fresh
	// workspace's first directory appears.
	d, base, _, _ := fixture(t)
	projects := filepath.Join(base, "config-a", "projects")
	if err := os.MkdirAll(projects, 0o755); err != nil {
		t.Fatalf("creating %s: %v", projects, err)
	}

	// Act.
	d.Scan()

	// Assert.
	if _, known := d.probed[Normalize(projects)]; !known {
		t.Fatalf("the projects root is not a probe candidate; candidates were %v", sortedDirs(d.probed))
	}
}

func TestScanDoesNotRestampADirectoryTheProbeHasNotEnumerated(t *testing.T) {
	// Arrange: a file appears after the scan globbed the directory. The rescan
	// that runs next must not overwrite the probe's memory with the mtime the
	// change already moved to — that would erase the very change the probe
	// exists to notice.
	d, base, _, _ := fixture(t, "config-a/projects/proj/sess-1.jsonl")
	d.Scan()
	project := filepath.Join(base, "config-a", "projects", "proj")
	write(t, filepath.Join(project, "sess-2.jsonl"))
	bumpDir(t, project, time.Second)

	// Act: a second full scan, then the probe.
	d.Scan()
	got := find(t, probed(d.ScanChanged()), "sess-2.jsonl")

	// Assert.
	if got.SessionID != "sess-2" {
		t.Fatalf("target = %+v, want the transcript the intervening scan must not have hidden", got)
	}
}
