package e2e

import (
	"bufio"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// THE SANDBOX BUILD GATE.
//
// Five simultaneous `e2e-sandbox.sh build` runs thrashed one buildkit cache,
// and a build from a stale checkout re-tagged the shared
// `agent-repl-e2e-sandbox:latest` over a good image -- after which a suite ran
// against sandbox sources nobody was looking at and reported about them as if
// they were the checkout's.
//
// The fix is two structural properties of the script, and this file tests both
// WITHOUT docker: a fake `docker` on PATH answers `info`, `version`, `build`,
// `image inspect` and `run`, and records every invocation. Real docker must
// never be required to prove a lock works.

const sandboxStampLabel = "org.agent-repl.sandbox-tree"

// fakeDocker is a stand-in container runtime.
//
// It records each invocation to a log file, answers `image inspect --format`
// from a label file the test controls, and can be told to block inside
// `build` until the test releases it -- which is how "the second build waits"
// is observed without a sleep standing in for synchronization.
type fakeDocker struct {
	dir      string // holds the fake binary, the log and the label file
	bin      string // the directory to put on PATH
	logPath  string
	labelPth string
}

func newFakeDocker(t *testing.T) *fakeDocker {
	t.Helper()
	dir := t.TempDir()
	bin := filepath.Join(dir, "bin")
	if err := os.MkdirAll(bin, 0o755); err != nil {
		t.Fatalf("mkdir bin: %v", err)
	}
	f := &fakeDocker{
		dir:      dir,
		bin:      bin,
		logPath:  filepath.Join(dir, "calls.log"),
		labelPth: filepath.Join(dir, "label"),
	}
	script := `#!/usr/bin/env bash
set -u
printf '%s\n' "$*" >> "` + f.logPath + `"
case "${1:-}" in
  info) exit 0 ;;
  version) printf 'arm64\n'; exit 0 ;;
  image)
    # image inspect [--format FMT] IMAGE
    if [[ ${3:-} == --format ]] || [[ ${2:-} == inspect && ${3:-} == --format ]]; then
      cat "` + f.labelPth + `" 2>/dev/null || true
      exit 0
    fi
    # A plain 'image inspect IMAGE' is the existence probe: the tag exists.
    exit 0 ;;
  build) exit 0 ;;
  run) exit 0 ;;
  *) exit 0 ;;
esac
`
	if err := os.WriteFile(filepath.Join(bin, "docker"), []byte(script), 0o755); err != nil {
		t.Fatalf("write fake docker: %v", err)
	}
	return f
}

func (f *fakeDocker) setLabel(t *testing.T, stamp string) {
	t.Helper()
	if err := os.WriteFile(f.labelPth, []byte(stamp+"\n"), 0o644); err != nil {
		t.Fatalf("write label: %v", err)
	}
}

func (f *fakeDocker) calls(t *testing.T) []string {
	t.Helper()
	b, err := os.ReadFile(f.logPath)
	if os.IsNotExist(err) {
		return nil
	}
	if err != nil {
		t.Fatalf("read calls: %v", err)
	}
	var out []string
	sc := bufio.NewScanner(strings.NewReader(string(b)))
	for sc.Scan() {
		if line := strings.TrimSpace(sc.Text()); line != "" {
			out = append(out, line)
		}
	}
	return out
}

func (f *fakeDocker) buildCount(t *testing.T) int {
	t.Helper()
	n := 0
	for _, c := range f.calls(t) {
		if strings.HasPrefix(c, "build ") {
			n++
		}
	}
	return n
}

// sandboxScript is the script under test, resolved from this package's dir.
func sandboxScript(t *testing.T) string {
	t.Helper()
	wd, err := os.Getwd()
	if err != nil {
		t.Fatalf("getwd: %v", err)
	}
	p := filepath.Join(wd, "sandbox", "bin", "e2e-sandbox.sh")
	if _, err := os.Stat(p); err != nil {
		t.Fatalf("script not found: %v", err)
	}
	return p
}

// currentSandboxStamp asks the script itself, so the test never re-derives the
// stamp rule and cannot drift from it.
func currentSandboxStamp(t *testing.T) string {
	t.Helper()
	script := sandboxScript(t)
	// The script's own stamp section is evaluated on its own -- sourcing the
	// whole file would run its dispatch -- so the rule under test is the
	// script's, never a copy of it kept here.
	cmd := exec.Command("bash", "-c",
		fmt.Sprintf(`set -euo pipefail; eval "$(sed -n '/^# --- the source stamp/,/^do_build() {$/p' %q | sed '$d')"; here=%q; sandbox_dir=$(cd -- "$here/.." && pwd); repo_root=$(cd -- "$sandbox_dir/../../../../.." && pwd); MODULE_REL=modules/app/agent-repl; IMAGE=x; log() { :; }; sandbox_stamp`,
			script, filepath.Dir(script)))
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("sandbox_stamp: %v\n%s", err, out)
	}
	return strings.TrimSpace(string(out))
}

func sandboxEnv(f *fakeDocker, lockDir string, extra ...string) []string {
	env := make([]string, 0, len(os.Environ())+3+len(extra))
	for _, entry := range os.Environ() {
		if !strings.HasPrefix(entry, "AGENT_REPL_SANDBOX_ALLOW_STALE=") {
			env = append(env, entry)
		}
	}
	env = append(env,
		"PATH="+f.bin+string(os.PathListSeparator)+os.Getenv("PATH"),
		"AGENT_REPL_SANDBOX_RUNTIME=docker",
		"AGENT_REPL_SANDBOX_BUILD_LOCK_DIR="+lockDir,
	)
	return append(env, extra...)
}

// TestSandboxBuildSkipsWhenImageAlreadyCarriesTheStamp: a second build that
// finds this checkout's stamp already on the tag must not rebuild.
func TestSandboxBuildSkipsWhenImageAlreadyCarriesTheStamp(t *testing.T) {
	t.Parallel()
	// Arrange.
	f := newFakeDocker(t)
	f.setLabel(t, currentSandboxStamp(t))
	lock := filepath.Join(t.TempDir(), "build.lock")

	// Act.
	cmd := exec.Command("bash", sandboxScript(t), "build")
	cmd.Env = sandboxEnv(f, lock)
	out, err := cmd.CombinedOutput()

	// Assert.
	if err != nil {
		t.Fatalf("build exited %v\n%s", err, out)
	}
	if n := f.buildCount(t); n != 0 {
		t.Fatalf("expected no rebuild, got %d build invocation(s)\n%s", n, out)
	}
	if !strings.Contains(string(out), "nothing to build") {
		t.Fatalf("expected the skip to say so, got:\n%s", out)
	}
}

// TestSandboxBuildForceRebuildsDespiteMatchingStamp: --force is the deliberate
// way past the skip, so the skip can never become a way to lose a rebuild.
func TestSandboxBuildForceRebuildsDespiteMatchingStamp(t *testing.T) {
	t.Parallel()
	// Arrange.
	f := newFakeDocker(t)
	f.setLabel(t, currentSandboxStamp(t))
	lock := filepath.Join(t.TempDir(), "build.lock")

	// Act.
	cmd := exec.Command("bash", sandboxScript(t), "build", "--force")
	cmd.Env = sandboxEnv(f, lock)
	out, err := cmd.CombinedOutput()

	// Assert.
	if err != nil {
		t.Fatalf("build --force exited %v\n%s", err, out)
	}
	if n := f.buildCount(t); n != 1 {
		t.Fatalf("expected exactly 1 build invocation, got %d\n%s", n, out)
	}
}

// TestSandboxBuildLabelsImageWithTheSandboxStamp: the built image must carry
// its sources, since that label is the only thing `run` can check.
func TestSandboxBuildLabelsImageWithTheSandboxStamp(t *testing.T) {
	t.Parallel()
	// Arrange.
	f := newFakeDocker(t)
	stamp := currentSandboxStamp(t)
	lock := filepath.Join(t.TempDir(), "build.lock")

	// Act.
	cmd := exec.Command("bash", sandboxScript(t), "build")
	cmd.Env = sandboxEnv(f, lock)
	out, err := cmd.CombinedOutput()

	// Assert.
	if err != nil {
		t.Fatalf("build exited %v\n%s", err, out)
	}
	want := "--label " + sandboxStampLabel + "=" + stamp
	found := false
	for _, c := range f.calls(t) {
		if strings.HasPrefix(c, "build ") && strings.Contains(c, want) {
			found = true
		}
	}
	if !found {
		t.Fatalf("build was not labelled %q; calls:\n%s", want, strings.Join(f.calls(t), "\n"))
	}
}

// TestSandboxBuildTakesOverALockHeldByADeadPid: a crashed build must not wedge
// every later build on the host.
func TestSandboxBuildTakesOverALockHeldByADeadPid(t *testing.T) {
	t.Parallel()
	// Arrange: a lock recording a pid that is certainly gone.
	f := newFakeDocker(t)
	f.setLabel(t, currentSandboxStamp(t))
	lock := filepath.Join(t.TempDir(), "build.lock")
	if err := os.MkdirAll(lock, 0o755); err != nil {
		t.Fatalf("mkdir lock: %v", err)
	}
	dead := reapedPid(t)
	if err := os.WriteFile(filepath.Join(lock, "pid"), []byte(fmt.Sprintf("%d\n", dead)), 0o644); err != nil {
		t.Fatalf("write pid: %v", err)
	}

	// Act.
	cmd := exec.Command("bash", sandboxScript(t), "build")
	cmd.Env = sandboxEnv(f, lock)
	out, err := cmd.CombinedOutput()

	// Assert.
	if err != nil {
		t.Fatalf("build exited %v\n%s", err, out)
	}
	if !strings.Contains(string(out), "reclaiming the build lock from dead pid") {
		t.Fatalf("expected a takeover, got:\n%s", out)
	}
}

// TestSandboxBuildWaitsForALockHeldByALivePid: the exclusion itself. The
// second build must not proceed while another holds the lock, and must
// proceed once it is released -- observed through the script's own output and
// the fake runtime's call log, never through a timed sleep.
func TestSandboxBuildWaitsForALockHeldByALivePid(t *testing.T) {
	t.Parallel()
	// Arrange: a live holder (this test process) owns the lock.
	f := newFakeDocker(t)
	lock := filepath.Join(t.TempDir(), "build.lock")
	if err := os.MkdirAll(lock, 0o755); err != nil {
		t.Fatalf("mkdir lock: %v", err)
	}
	if err := os.WriteFile(filepath.Join(lock, "pid"), []byte(fmt.Sprintf("%d\n", os.Getpid())), 0o644); err != nil {
		t.Fatalf("write pid: %v", err)
	}

	reader, writer, err := os.Pipe()
	if err != nil {
		t.Fatalf("create output pipe: %v", err)
	}
	defer reader.Close()

	cmd := exec.Command("bash", sandboxScript(t), "build")
	cmd.Env = sandboxEnv(f, lock)
	cmd.Stdout, cmd.Stderr = writer, writer
	announced := make(chan struct{}, 1)
	scanned := make(chan struct{})
	var output strings.Builder
	go func() {
		defer close(scanned)
		scanner := bufio.NewScanner(reader)
		for scanner.Scan() {
			line := scanner.Text()
			output.WriteString(line)
			output.WriteByte('\n')
			if strings.Contains(line, "WAITING: another sandbox build holds the host build lock") {
				select {
				case announced <- struct{}{}:
				default:
				}
			}
		}
	}()

	// Act.
	if err := cmd.Start(); err != nil {
		_ = writer.Close()
		<-scanned
		t.Fatalf("start: %v", err)
	}
	waitErr := make(chan error, 1)
	go func() {
		err := cmd.Wait()
		_ = writer.Close()
		waitErr <- err
	}()

	// Assert: it announces the wait, and it has NOT built anything.
	select {
	case <-announced:
	case <-time.After(20 * time.Second):
		_ = cmd.Process.Kill()
		<-waitErr
		<-scanned
		t.Fatalf("timed out waiting for the build to announce its wait:\n%s", output.String())
	}
	if n := f.buildCount(t); n != 0 {
		t.Fatalf("a blocked build ran %d build invocation(s)", n)
	}

	// Act: release the lock.
	if err := os.RemoveAll(lock); err != nil {
		t.Fatalf("release lock: %v", err)
	}

	// Assert: it then proceeds and builds.
	if err := <-waitErr; err != nil {
		<-scanned
		t.Fatalf("build exited %v\n%s", err, output.String())
	}
	<-scanned
	if n := f.buildCount(t); n != 1 {
		t.Fatalf("expected 1 build invocation after release, got %d\n%s", n, output.String())
	}
}

// TestSandboxRunRefusesAStaleImage: the silent-stale-image failure, ended.
func TestSandboxRunRefusesAStaleImage(t *testing.T) {
	t.Parallel()
	// Arrange.
	f := newFakeDocker(t)
	f.setLabel(t, "0000000000000000000000000000000000000000")
	lock := filepath.Join(t.TempDir(), "build.lock")

	// Act.
	cmd := exec.Command("bash", sandboxScript(t), "run", "true")
	cmd.Env = sandboxEnv(f, lock)
	out, err := cmd.CombinedOutput()

	// Assert.
	if err == nil {
		t.Fatalf("run accepted a stale image:\n%s", out)
	}
	if !strings.Contains(string(out), "REFUSING TO RUN") {
		t.Fatalf("expected a loud refusal, got:\n%s", out)
	}
}

// TestSandboxRunRefusesAnUnlabelledImage: an image built before the stamp
// existed is exactly the stale image this gate is for, so "no label" is a
// refusal and not a pass.
func TestSandboxRunRefusesAnUnlabelledImage(t *testing.T) {
	t.Parallel()
	// Arrange: the label file stays empty.
	f := newFakeDocker(t)
	lock := filepath.Join(t.TempDir(), "build.lock")

	// Act.
	cmd := exec.Command("bash", sandboxScript(t), "run", "true")
	cmd.Env = sandboxEnv(f, lock)
	out, err := cmd.CombinedOutput()

	// Assert.
	if err == nil {
		t.Fatalf("run accepted an unlabelled image:\n%s", out)
	}
	if !strings.Contains(string(out), "REFUSING TO RUN") {
		t.Fatalf("expected a loud refusal, got:\n%s", out)
	}
}

// TestSandboxRunAllowsAStaleImageWhenOverridden: the escape hatch exists, is
// deliberate, and says out loud what it is doing.
func TestSandboxRunAllowsAStaleImageWhenOverridden(t *testing.T) {
	t.Parallel()
	// Arrange.
	f := newFakeDocker(t)
	f.setLabel(t, "0000000000000000000000000000000000000000")
	lock := filepath.Join(t.TempDir(), "build.lock")

	// Act.
	cmd := exec.Command("bash", sandboxScript(t), "run", "true")
	cmd.Env = sandboxEnv(f, lock, "AGENT_REPL_SANDBOX_ALLOW_STALE=1")
	out, _ := cmd.CombinedOutput()

	// Assert: whatever happens later (this host has no preflight-able
	// runtime), the stamp gate did not refuse.
	if strings.Contains(string(out), "REFUSING TO RUN") {
		t.Fatalf("ALLOW_STALE=1 was ignored:\n%s", out)
	}
	if !strings.Contains(string(out), "AGENT_REPL_SANDBOX_ALLOW_STALE=1") {
		t.Fatalf("expected the override to announce itself:\n%s", out)
	}
}

// reapedPid returns a pid that has certainly exited and been reaped.
func reapedPid(t *testing.T) int {
	t.Helper()
	c := exec.Command("true")
	if err := c.Run(); err != nil {
		t.Fatalf("spawn: %v", err)
	}
	return c.Process.Pid
}
