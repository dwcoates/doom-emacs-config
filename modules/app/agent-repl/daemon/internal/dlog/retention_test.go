package dlog

import (
	"errors"
	"io"
	"os"
	"path/filepath"
	"testing"
	"time"

	"agentrepl/logging"

	"agent-repld/internal/sourcescan"

	"golang.org/x/sys/unix"
)

func TestIsDaemonLogFamily(t *testing.T) {
	cases := []struct {
		base string
		want bool
	}{
		{"daemon.run.log", true},
		{"daemon.stdio.log", true},
		{"agent-repl-0054899b8c24410e-daemon-4268611927.log", true},
		{"agent-repl-0054899b8c24410e-shim-1.log", true},
		{"agent-repl-0054899b8c24410e-webapp-1.log", true},
		{"agent-repl-0054899b8c24410e-sidecar-1.log", true},
		{"agent-repl-9e138edb-daemon-1.log", true},
		{"agent-repl-0054899b8c24410e-emacs-9A8fUl.log", false},
		{"emacs.central.log", false},
		{"daemon.run.log.1", false},
		{"merge-diag.txt", false},
	}
	for _, tc := range cases {
		t.Run(tc.base, func(t *testing.T) {
			// Act / Assert.
			if got := isDaemonLogFamily(tc.base); got != tc.want {
				t.Fatalf("isDaemonLogFamily(%q) = %v, want %v", tc.base, got, tc.want)
			}
		})
	}
}

// retentionFixture is real surfaces over a temp logs directory, with the
// files a pass must tell apart.
type retentionFixture struct {
	s       *surfaces
	runLog  string
	logsDir string
	now     time.Time
}

func newRetentionFixture(t *testing.T) retentionFixture {
	t.Helper()
	s, runLog := testSurfaces(t)
	return retentionFixture{s: s, runLog: runLog, logsDir: filepath.Dir(runLog), now: time.Now()}
}

// file writes name under the logs directory with its last write at at.
func (f retentionFixture) file(t *testing.T, name string, at time.Time) string {
	t.Helper()
	path := filepath.Join(f.logsDir, name)
	if err := os.WriteFile(path, []byte("x\n"), 0o644); err != nil {
		t.Fatalf("write %s: %v", path, err)
	}
	age(t, path, at)
	return path
}

func (f retentionFixture) stale() time.Time { return f.now.Add(-logging.DefaultRetention - time.Hour) }

func (f retentionFixture) passRecord(t *testing.T) map[string]any {
	t.Helper()
	var last map[string]any
	for _, rec := range readRecords(t, f.runLog) {
		if rec["operation"] == "daemon.dlog.retention_pass" {
			last = rec
		}
	}
	if last == nil {
		t.Fatalf("no daemon.dlog.retention_pass record in the run log")
	}
	return last
}

func TestRetainOnceRemovesOnlyExpiredDaemonFamiliesNobodyHolds(t *testing.T) {
	cases := []struct {
		name     string
		file     string
		stale    bool
		wantKept bool
	}{
		{name: "an expired orphan workspace target", file: "agent-repl-0054899b8c24410e-daemon-1.log", stale: true},
		{name: "an expired orphan generation", file: "agent-repl-0054899b8c24410e-shim-1.log.3", stale: true},
		{name: "an expired run log generation", file: "daemon.run.log.4", stale: true},
		{name: "a fresh orphan workspace target", file: "agent-repl-0054899b8c24410e-webapp-1.log", wantKept: true},
		{name: "an expired Emacs target", file: "agent-repl-0054899b8c24410e-emacs-9A8fUl.log", stale: true, wantKept: true},
		{name: "an expired Emacs central generation", file: "emacs.central.log.2", stale: true, wantKept: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newRetentionFixture(t)
			at := f.now
			if tc.stale {
				at = f.stale()
			}
			path := f.file(t, tc.file, at)

			// Act.
			f.s.retainOnce(f.now)

			// Assert.
			_, err := os.Lstat(path)
			if kept := err == nil; kept != tc.wantKept {
				t.Fatalf("%s kept = %v, want %v", tc.file, kept, tc.wantKept)
			}
		})
	}
}

func TestRetainOnceAgesTheRunLogItHolds(t *testing.T) {
	// Arrange: the run log's current file is past the retention.
	f := newRetentionFixture(t)
	f.s.Global().Info("test.seed", "seed", nil)
	age(t, f.runLog, f.stale())

	// Act.
	f.s.retainOnce(f.now)

	// Assert: rolled away, and the pass record is the first in the fresh file.
	rec := f.passRecord(t)
	if rec["context"].(map[string]any)["run_log_rolled"] != true {
		t.Fatalf("pass record %v, want run_log_rolled", rec)
	}
	if _, err := os.Lstat(f.runLog + ".1"); !os.IsNotExist(err) {
		t.Errorf("the expired run log survived as %s.1", f.runLog)
	}
}

func TestRetainOnceAgesTheSinksItHolds(t *testing.T) {
	// Arrange: an open workspace sink whose target is past the retention.
	f := newRetentionFixture(t)
	dir := t.TempDir()
	log, err := f.s.Workspace(dir)
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	log.Info("test.seed", "seed", nil)
	_, sk, err := f.s.resolve(dir, "daemon")
	if err != nil {
		t.Fatalf("resolve: %v", err)
	}
	age(t, sk.target, f.stale())

	// Act.
	f.s.retainOnce(f.now)

	// Assert.
	ctx := f.passRecord(t)["context"].(map[string]any)
	if ctx["sinks_rolled"] != float64(1) {
		t.Fatalf("pass context %v, want one sink rolled", ctx)
	}
	if _, err := os.Lstat(sk.target); err != nil {
		t.Errorf("the held sink's current target is gone (%v); a held family is the holder's, and its path must stay", err)
	}
}

func TestRetainOnceRecordsAPruneFailure(t *testing.T) {
	// Arrange: a logs directory the pass cannot read.
	f := newRetentionFixture(t)
	if err := os.Chmod(f.logsDir, 0o300); err != nil {
		t.Fatalf("chmod: %v", err)
	}
	t.Cleanup(func() { _ = os.Chmod(f.logsDir, 0o755) })

	// Act.
	f.s.retainOnce(f.now)

	// Assert.
	if err := os.Chmod(f.logsDir, 0o755); err != nil {
		t.Fatalf("chmod: %v", err)
	}
	var failure map[string]any
	for _, rec := range readRecords(t, f.runLog) {
		if rec["operation"] == "daemon.dlog.retention_failed" {
			failure = rec
		}
	}
	if failure == nil || failure["level"] != "error" {
		t.Fatalf("no error-level daemon.dlog.retention_failed record; run log %v", readRecords(t, f.runLog))
	}
	ctx := failure["context"].(map[string]any)
	if ctx["logs_dir"] != f.logsDir || ctx["cause"] == "" {
		t.Errorf("failure context %v, want the logs directory and the cause", ctx)
	}
	if f.passRecord(t)["context"].(map[string]any)["failures"] != float64(1) {
		t.Errorf("the pass record does not count the failure")
	}
}

func TestSurfacesAdoptStdio(t *testing.T) {
	cases := []struct {
		name          string
		std           func(f stdioFixture) *os.File
		dup           func(int, int) error
		wantErr       bool
		wantAdopted   bool
		wantOperation string
	}{
		{name: "the stdio log is taken over", std: func(f stdioFixture) *os.File { return f.std }, dup: unix.Dup2,
			wantAdopted: true, wantOperation: "daemon.dlog.stdio_adopted"},
		{name: "another stderr is left alone", std: func(f stdioFixture) *os.File { return f.target }, dup: unix.Dup2,
			wantOperation: "daemon.dlog.stdio_not_adopted"},
		{name: "a failed takeover is a boot failure", std: func(f stdioFixture) *os.File { return f.std },
			dup: func(int, int) error { return errors.New("dup2 refused") }, wantErr: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: surfaces whose logs directory holds the stdio log.
			f := newStdioFixture(t)
			runLog := filepath.Join(f.logsDir, "daemon.run.log")
			s, err := openSurfaces(runLog, LevelDebug, io.Discard)
			if err != nil {
				t.Fatalf("openSurfaces: %v", err)
			}
			t.Cleanup(func() { s.Close() })

			// Act.
			err = s.adoptStdio(tc.std(f), []int{int(f.target.Fd())}, tc.dup)

			// Assert.
			if (err != nil) != tc.wantErr || (s.stdio != nil) != tc.wantAdopted {
				t.Fatalf("adoptStdio = %v, adopted %v; want err=%v adopted=%v", err, s.stdio != nil, tc.wantErr, tc.wantAdopted)
			}
			if tc.wantOperation != "" && !hasOperation(readRecords(t, runLog), tc.wantOperation) {
				t.Errorf("no %s record in the run log", tc.wantOperation)
			}
		})
	}
}

func TestRetainOncePrunesARegisteredDirectory(t *testing.T) {
	// Arrange: a registered directory holding an expired owned file and an
	// expired file it does not own.
	f := newRetentionFixture(t)
	dir := t.TempDir()
	f.s.RetainDirectory(dir, func(base string) bool { return filepath.Ext(base) == ".log" })
	owned := filepath.Join(dir, "lease-tests-1.log")
	other := filepath.Join(dir, "notes.txt")
	for _, path := range []string{owned, other} {
		if err := os.WriteFile(path, []byte("x\n"), 0o644); err != nil {
			t.Fatalf("write: %v", err)
		}
		age(t, path, f.stale())
	}

	// Act.
	f.s.retainOnce(f.now)

	// Assert.
	if _, err := os.Lstat(owned); !os.IsNotExist(err) {
		t.Errorf("the expired owned file survived the pass")
	}
	if _, err := os.Lstat(other); err != nil {
		t.Errorf("the file the predicate does not own was removed: %v", err)
	}
	if ctx := f.passRecord(t)["context"].(map[string]any); ctx["directories"] != float64(2) {
		t.Errorf("pass context %v, want two directories", ctx)
	}
}

func TestRetainOnceAnAbsentRegisteredDirectoryIsNotAFailure(t *testing.T) {
	// Arrange.
	f := newRetentionFixture(t)
	absent := filepath.Join(t.TempDir(), "merge-logs")
	f.s.RetainDirectory(absent, func(string) bool { return true })

	// Act.
	f.s.retainOnce(f.now)

	// Assert.
	if ctx := f.passRecord(t)["context"].(map[string]any); ctx["failures"] != float64(0) {
		t.Fatalf("pass context %v, want no failures", ctx)
	}
	if !hasOperation(readRecords(t, f.runLog), "daemon.dlog.retention_dir_absent") {
		t.Errorf("no daemon.dlog.retention_dir_absent record")
	}
}

func TestRetainDirectoryRefusesANilPredicate(t *testing.T) {
	// Arrange.
	f := newRetentionFixture(t)
	defer func() {
		// Assert.
		if recover() == nil {
			t.Fatal("RetainDirectory accepted a nil predicate")
		}
	}()

	// Act.
	f.s.RetainDirectory(t.TempDir(), nil)
}

func TestEveryMintedTargetIsADaemonLogFamily(t *testing.T) {
	for _, name := range SinkNames {
		t.Run(name, func(t *testing.T) {
			// Arrange.
			logsDir := t.TempDir()

			// Act.
			target, err := createTarget(logsDir, "0054899b8c24410e", name)

			// Assert.
			if err != nil {
				t.Fatalf("createTarget: %v", err)
			}
			if !isDaemonLogFamily(filepath.Base(target)) {
				t.Fatalf("minted target %q is not a daemon log family, so the retention pass would never prune it", filepath.Base(target))
			}
		})
	}
}

func TestEveryRunLogRetentionFailureGoesThroughRetentionFailed(t *testing.T) {
	// Act.
	viaHelper := sourcescan.Count(t, "s.retentionFailed(")
	byHand := sourcescan.Count(t, `Global().Error("daemon.dlog.retention_failed"`)

	// Assert: the run log, a directory and the stdio log call the helper, and the only hand-written record is the helper's own.
	if viaHelper != 3 || byHand != 1 {
		t.Fatalf("s.retentionFailed( appears %d times and a hand-rolled record %d times; want 3 and 1 (the helper's own record)", viaHelper, byHand)
	}
}
