package harness

import (
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"
)

// writeLogs makes a logs directory holding the named files.
func writeLogs(t *testing.T, files map[string]string) string {
	t.Helper()
	dir := t.TempDir()
	for name, body := range files {
		if err := os.WriteFile(filepath.Join(dir, name), []byte(body), 0o600); err != nil {
			t.Fatalf("write %s: %v", name, err)
		}
	}
	return dir
}

func TestPreserveFailureLogsCopiesEveryLogIntoTheArtifactsDir(t *testing.T) {
	// Arrange
	logs := writeLogs(t, map[string]string{"daemon.run.log": "{\"operation\":\"daemon.a\"}\n"})
	dest := filepath.Join(t.TempDir(), "TestX")

	// Act
	preserveFailureLogs(func(string, ...any) {}, logs, dest)

	// Assert
	got, err := os.ReadFile(filepath.Join(dest, "daemon.run.log"))
	if err != nil || string(got) != "{\"operation\":\"daemon.a\"}\n" {
		t.Fatalf("preserved run log = %q, %v; want the log copied whole", got, err)
	}
}

func TestPreserveFailureLogsSkipsWhatIsNotALog(t *testing.T) {
	// Arrange
	logs := writeLogs(t, map[string]string{"daemon.addr": "127.0.0.1:1"})
	dest := filepath.Join(t.TempDir(), "TestX")

	// Act
	preserveFailureLogs(func(string, ...any) {}, logs, dest)

	// Assert
	if _, err := os.Stat(filepath.Join(dest, "daemon.addr")); !os.IsNotExist(err) {
		t.Fatalf("a non-log file was preserved (stat err %v); only .log files are artifacts", err)
	}
}

func TestPreserveFailureLogsKeepsARotatedGenerationOfTheRunLog(t *testing.T) {
	// Arrange
	logs := writeLogs(t, map[string]string{"daemon.run.log.1": "{\"operation\":\"daemon.older\"}\n"})
	dest := filepath.Join(t.TempDir(), "TestX")

	// Act
	preserveFailureLogs(func(string, ...any) {}, logs, dest)

	// Assert
	if got, err := os.ReadFile(filepath.Join(dest, "daemon.run.log.1")); err != nil || !strings.Contains(string(got), "daemon.older") {
		t.Fatalf("preserved generation = %q, %v; want the rotated records kept", got, err)
	}
}

func TestPreserveFailureLogsKeepsAnExtraFileFromOutsideTheLogsDir(t *testing.T) {
	// Arrange: the stderr file a handover's successor inherits lives beside
	// the state root, not in its logs directory.
	logs := writeLogs(t, map[string]string{"daemon.run.log": "{}\n"})
	stderr := filepath.Join(t.TempDir(), "daemon.stderr.log")
	if err := os.WriteFile(stderr, []byte("panic: the successor died\n"), 0o600); err != nil {
		t.Fatalf("write the stderr file: %v", err)
	}
	dest := filepath.Join(t.TempDir(), "TestX")

	// Act
	preserveFailureLogs(func(string, ...any) {}, logs, dest, stderr)

	// Assert
	if got, err := os.ReadFile(filepath.Join(dest, "daemon.stderr.log")); err != nil || !strings.Contains(string(got), "panic: the successor died") {
		t.Fatalf("preserved stderr = %q, %v; want the successor's words kept", got, err)
	}
}

func TestIsLogGenerationTellsALogFromOtherFiles(t *testing.T) {
	tests := []struct {
		name string
		want bool
	}{
		{name: "daemon.run.log", want: true},
		{name: "daemon.run.log.1", want: true},
		{name: "daemon.run.log.12", want: true},
		{name: "daemon.run.log.bak", want: false},
		{name: "daemon.run.log.", want: false},
		{name: "daemon.addr", want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange, Act
			got := isLogGeneration(tt.name)

			// Assert
			if got != tt.want {
				t.Fatalf("isLogGeneration(%q) = %v, want %v", tt.name, got, tt.want)
			}
		})
	}
}

func TestPreserveFailureLogsTailsEachLogWithoutAnArtifactsDir(t *testing.T) {
	// Arrange
	logs := writeLogs(t, map[string]string{"daemon.run.log": "{\"operation\":\"daemon.marker\"}\n"})
	var said []string

	// Act
	preserveFailureLogs(func(format string, args ...any) { said = append(said, fmt.Sprintf(format, args...)) }, logs, "")

	// Assert
	if len(said) != 1 || !strings.Contains(said[0], "daemon.marker") {
		t.Fatalf("logged %q, want one tail carrying the run log's record", said)
	}
}

func TestPreserveFailureLogsSaysWhenThereAreNoLogs(t *testing.T) {
	// Arrange
	var said []string

	// Act
	preserveFailureLogs(func(format string, args ...any) { said = append(said, fmt.Sprintf(format, args...)) },
		filepath.Join(t.TempDir(), "absent"), "")

	// Assert
	if len(said) != 1 || !strings.Contains(said[0], "no logs to preserve") {
		t.Fatalf("logged %q, want the absence said", said)
	}
}

func TestTailBytesOpensOnARecordBoundary(t *testing.T) {
	// Arrange
	body := []byte("first record\nsecond record\n")

	// Act
	got := tailBytes(body, 20)

	// Assert
	if got != "second record\n" {
		t.Fatalf("tailBytes = %q, want the tail cut forward to the next whole record", got)
	}
}

func TestDefaultArtifactsKeepsOnlyTheNewestRuns(t *testing.T) {
	// Arrange: twelve older runs already stand.
	root := t.TempDir()
	base := time.Date(2026, 10, 12, 0, 0, 0, 0, time.UTC)
	for i := 0; i < 12; i++ {
		if _, err := defaultArtifacts(root, base.Add(time.Duration(i)*time.Minute), 100+i); err != nil {
			t.Fatalf("defaultArtifacts (older run %d): %v", i, err)
		}
	}

	// Act
	dir, err := defaultArtifacts(root, base.Add(time.Hour), 999)

	// Assert
	if err != nil {
		t.Fatalf("defaultArtifacts: %v", err)
	}
	entries, err := os.ReadDir(root)
	if err != nil {
		t.Fatalf("ReadDir: %v", err)
	}
	if len(entries) != keptArtifactRuns {
		t.Fatalf("%d runs kept, want %d", len(entries), keptArtifactRuns)
	}
	if last := entries[len(entries)-1].Name(); filepath.Join(root, last) != dir || !strings.HasSuffix(last, "-999") {
		t.Fatalf("newest kept run = %s, want this run's %s", last, dir)
	}
	if first := entries[0].Name(); !strings.HasSuffix(first, "-103") {
		t.Fatalf("oldest kept run = %s, want the fourth-oldest (pid 103)", first)
	}
}

func TestDefaultArtifactsRefusesARootItCannotCreate(t *testing.T) {
	// Arrange: the root's parent is a regular file.
	parent := filepath.Join(t.TempDir(), "file")
	if err := os.WriteFile(parent, nil, 0o600); err != nil {
		t.Fatalf("WriteFile: %v", err)
	}

	// Act
	_, err := defaultArtifacts(filepath.Join(parent, "root"), time.Now(), 1)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "create the run's artifacts directory") {
		t.Fatalf("defaultArtifacts = %v, want the create refused", err)
	}
}

func TestUseDefaultArtifactsLeavesANamedDirectoryAlone(t *testing.T) {
	// Arrange
	t.Setenv(ArtifactsEnv, "/named/by/the/run")

	// Act
	err := useDefaultArtifacts()

	// Assert
	if err != nil || os.Getenv(ArtifactsEnv) != "/named/by/the/run" {
		t.Fatalf("useDefaultArtifacts = %v, env %q; want the run's own directory kept", err, os.Getenv(ArtifactsEnv))
	}
}

func TestUseDefaultArtifactsNamesAFreshRunUnderTheUserCache(t *testing.T) {
	// Arrange
	home := t.TempDir()
	t.Setenv("HOME", home)
	t.Setenv("XDG_CACHE_HOME", filepath.Join(home, ".cache"))
	t.Setenv(ArtifactsEnv, "")

	// Act
	err := useDefaultArtifacts()

	// Assert
	got := os.Getenv(ArtifactsEnv)
	if err != nil || !strings.HasPrefix(got, home) || !strings.Contains(got, filepath.Join("agent-repl", "integration-artifacts")) {
		t.Fatalf("useDefaultArtifacts = %v, env %q; want a run directory under %s", err, got, home)
	}
	if info, statErr := os.Stat(got); statErr != nil || !info.IsDir() {
		t.Fatalf("the run directory %s was not created: %v", got, statErr)
	}
}

// fakeCleanupT records cleanups and runs them last-registered first, as
// *testing.T does; a step marks it failed through fail.
type fakeCleanupT struct {
	cleanups []func()
	failed   bool
}

func (f *fakeCleanupT) Cleanup(fn func()) { f.cleanups = append(f.cleanups, fn) }
func (f *fakeCleanupT) Failed() bool      { return f.failed }

func (f *fakeCleanupT) finish() {
	for i := len(f.cleanups) - 1; i >= 0; i-- {
		f.cleanups[i]()
	}
}

func TestRegisterLogChecks(t *testing.T) {
	tests := []struct {
		name       string
		bodyFails  bool
		sweepFails bool
		wantKept   bool
	}{
		{name: "a passing test keeps nothing"},
		{name: "a test whose body failed keeps its logs", bodyFails: true, wantKept: true},
		{name: "a test failed by a sweep alone keeps its logs", sweepFails: true, wantKept: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			ft := &fakeCleanupT{failed: tt.bodyFails}
			var ran []string
			keep := func() { ran = append(ran, "keep") }
			sweep := func() {
				ran = append(ran, "sweep")
				if tt.sweepFails {
					ft.failed = true
				}
			}
			registerLogChecks(ft, keep, sweep, func() { ran = append(ran, "sweep") })

			// Act
			ft.finish()

			// Assert
			want := []string{"sweep", "sweep"}
			if tt.wantKept {
				want = append(want, "keep")
			}
			if !slices.Equal(ran, want) {
				t.Fatalf("cleanups ran %v, want %v", ran, want)
			}
		})
	}
}

func TestKeepFailureLogsKeepsTheLogsAndTheStderrUnderTheTestsName(t *testing.T) {
	// Arrange
	state := t.TempDir()
	if err := os.MkdirAll(filepath.Join(state, "logs"), 0o755); err != nil {
		t.Fatalf("mkdir logs: %v", err)
	}
	if err := os.WriteFile(filepath.Join(state, "logs", "daemon.run.log"), []byte("x\n"), 0o600); err != nil {
		t.Fatalf("write log: %v", err)
	}
	stderr := filepath.Join(t.TempDir(), "daemon.stderr")
	if err := os.WriteFile(stderr, []byte("panic: boom\n"), 0o600); err != nil {
		t.Fatalf("write stderr: %v", err)
	}
	root := t.TempDir()
	t.Setenv(ArtifactsEnv, root)

	// Act
	keepFailureLogs(NewRecordingTB(t, "TestFailedOne"), state, stderr)

	// Assert
	dest := filepath.Join(root, artifactDirName("TestFailedOne"))
	for _, name := range []string{"daemon.run.log", "daemon.stderr"} {
		if _, err := os.Stat(filepath.Join(dest, name)); err != nil {
			t.Errorf("kept %s: %v; want it under the failed test's name", name, err)
		}
	}
}

// TestEveryDaemonKeepsItsFailureLogsThroughOneHelper pins the extraction: an
// owned daemon and a view both keep their logs through keepFailureLogs under
// registerLogChecks, and no other harness file calls preserveFailureLogs by
// hand.
func TestEveryDaemonKeepsItsFailureLogsThroughOneHelper(t *testing.T) {
	// Arrange
	files, err := filepath.Glob("*.go")
	if err != nil {
		t.Fatalf("glob: %v", err)
	}

	// Act
	var handRolled, helperSites []string
	for _, f := range files {
		if strings.HasSuffix(f, "_test.go") {
			continue
		}
		body, err := os.ReadFile(f)
		if err != nil {
			t.Fatalf("read %s: %v", f, err)
		}
		src := string(body)
		if f != "artifacts.go" && strings.Contains(src, "preserveFailureLogs(") {
			handRolled = append(handRolled, f)
		}
		if strings.Contains(src, "registerLogChecks(t, func() { keepFailureLogs(t, ") {
			helperSites = append(helperSites, f)
		}
	}

	// Assert
	if len(handRolled) != 0 {
		t.Errorf("files calling preserveFailureLogs by hand: %v; keep failure logs through keepFailureLogs", handRolled)
	}
	if strings.Join(helperSites, ",") != "daemon.go,view.go" {
		t.Errorf("files keeping logs through the helper = %v, want daemon.go and view.go", helperSites)
	}
}
