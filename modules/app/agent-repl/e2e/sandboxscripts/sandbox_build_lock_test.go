package sandboxscripts

import (
	"bufio"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strconv"
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
//
// NOR REAL GIT (owner rule: no test runs real git). The script stamps its
// sources with git when the checkout is a repository; beside the fake docker
// sits bin/fake-git.sh as `git`, which finds no fake repository above the
// checkout and answers "not a git repository", so the script takes its own
// content-digest stamp -- the same rule for the stamp this file asks for and
// the stamp the script compares.

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
    # A plain 'image inspect IMAGE' is the existence probe: the tag exists
    # unless the test said it does not.
    [[ -e "` + f.dir + `/missing" ]] && exit 1
    exit 0 ;;
  build) exit 0 ;;
  # run keeps what arrived on its stdin: the streamed working copy.
  # and says it is staged, as the image's entrypoint does once it extracted it.
  run) cat > "` + f.dir + `/run-stdin"; printf 'sandbox: staged 1 files, 4.0K total\n' >&2; exit 0 ;;
  *) exit 0 ;;
esac
`
	if err := os.WriteFile(filepath.Join(bin, "docker"), []byte(script), 0o755); err != nil {
		t.Fatalf("write fake docker: %v", err)
	}
	if err := os.Symlink(fakeGitScript(t), filepath.Join(bin, "git")); err != nil {
		t.Fatalf("link the fake git: %v", err)
	}
	// The host's free disk is an external dependency like the runtime: the
	// preflight's `df -Pk` reads this fake, which reports plenty of room
	// unless the test set the free kilobytes itself (setFreeKB).
	df := `#!/usr/bin/env bash
free=$(cat "` + f.dir + `/free-kb" 2>/dev/null || echo 104857600)
printf 'Filesystem 1024-blocks Used Available Capacity Mounted on\n'
printf 'fake 209715200 0 %s 1%% /\n' "$free"
`
	if err := os.WriteFile(filepath.Join(bin, "df"), []byte(df), 0o755); err != nil {
		t.Fatalf("write fake df: %v", err)
	}
	return f
}

// setFreeKB makes the fake df report KB kilobytes available.
func (f *fakeDocker) setFreeKB(t *testing.T, kb int) {
	t.Helper()
	if err := os.WriteFile(filepath.Join(f.dir, "free-kb"), []byte(strconv.Itoa(kb)+"\n"), 0o644); err != nil {
		t.Fatalf("write free-kb: %v", err)
	}
}

// imageMissing makes every existence probe answer that no image is tagged.
func (f *fakeDocker) imageMissing(t *testing.T) {
	t.Helper()
	if err := os.WriteFile(filepath.Join(f.dir, "missing"), nil, 0o644); err != nil {
		t.Fatalf("write missing marker: %v", err)
	}
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

// fakeGitScript is bin/fake-git.sh, resolved from this package's dir.
func fakeGitScript(t *testing.T) string {
	t.Helper()
	wd, err := os.Getwd()
	if err != nil {
		t.Fatalf("getwd: %v", err)
	}
	p := filepath.Join(wd, "..", "..", "bin", "fake-git.sh")
	if _, err := os.Stat(p); err != nil {
		t.Fatalf("fake git not found: %v", err)
	}
	return p
}

// sandboxScript is the script under test, resolved from this package's dir.
func sandboxScript(t *testing.T) string {
	t.Helper()
	wd, err := os.Getwd()
	if err != nil {
		t.Fatalf("getwd: %v", err)
	}
	p := filepath.Join(wd, "..", "sandbox", "bin", "e2e-sandbox.sh")
	if _, err := os.Stat(p); err != nil {
		t.Fatalf("script not found: %v", err)
	}
	return p
}

// sandboxBuildUnstaged is `e2e-sandbox.sh build ARGS` with the build context's
// staging replaced by an empty one. It sources the script, which then defines
// its functions and dispatches nothing, and calls do_build itself.
//
// WHY: stage_context tars every .go file in the module into the context, and
// the tests that use this assert only the lock, the stamp and the docker argv,
// none of which reads the context. TestSandboxBuildLabelsImageWithTheSandboxStamp
// runs the real staging, so the step is still executed in this suite.
func sandboxBuildUnstaged(t *testing.T, args ...string) *exec.Cmd {
	t.Helper()
	// The script path rides as $1, never as bash's $0: a $0 equal to the
	// sourced file's own path is exactly what the guard reads as a direct run.
	return exec.Command("bash", append([]string{"-c",
		`set -euo pipefail; script=$1; shift; . "$script"; stage_context() { :; }; do_build "$@"`,
		"bash", sandboxScript(t)}, args...)...)
}

// currentSandboxStamp asks the scripts' own image library, so the test never
// re-derives the stamp rule and cannot drift from it.
func currentSandboxStamp(t *testing.T, f *fakeDocker) string {
	t.Helper()
	return sandboxLib(t, f, "sandbox_stamp")
}

// sandboxLib sources sandbox/bin/sandbox-image.sh -- the one place a
// checkout's sandbox stamp and image name are decided, shared by
// e2e-sandbox.sh and preflight.sh -- and runs CALL against it, with the fake
// git on PATH and EXTRA added to the environment.
func sandboxLib(t *testing.T, f *fakeDocker, call string, extra ...string) string {
	t.Helper()
	lib := filepath.Join(filepath.Dir(sandboxScript(t)), "sandbox-image.sh")
	cmd := exec.Command("bash", "-c", fmt.Sprintf(`set -euo pipefail; . %q; %s`, lib, call))
	var env []string
	for _, kv := range os.Environ() {
		if !strings.HasPrefix(kv, "AGENT_REPL_SANDBOX_IMAGE=") {
			env = append(env, kv)
		}
	}
	cmd.Env = append(append(env, "PATH="+f.bin+string(os.PathListSeparator)+os.Getenv("PATH")), extra...)
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("%s: %v\n%s", call, err, out)
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
	f.setLabel(t, currentSandboxStamp(t, f))
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
	f.setLabel(t, currentSandboxStamp(t, f))
	lock := filepath.Join(t.TempDir(), "build.lock")

	// Act.
	cmd := sandboxBuildUnstaged(t, "--force")
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
	stamp := currentSandboxStamp(t, f)
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
	f.setLabel(t, currentSandboxStamp(t, f))
	lock := filepath.Join(t.TempDir(), "build.lock")
	if err := os.MkdirAll(lock, 0o755); err != nil {
		t.Fatalf("mkdir lock: %v", err)
	}
	dead := reapedPid(t)
	if err := os.WriteFile(filepath.Join(lock, "pid"), []byte(fmt.Sprintf("%d\n", dead)), 0o644); err != nil {
		t.Fatalf("write pid: %v", err)
	}

	// Act.
	cmd := sandboxBuildUnstaged(t)
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

	cmd := sandboxBuildUnstaged(t)
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
	// ISOLATED FROM THE HOST past the gate: a private slot directory, so the
	// run never queues behind a real sandbox holding the host's shared slots,
	// and no host webapp build, which would rewrite the checkout's dist.
	cmd.Env = sandboxEnv(f, lock, "AGENT_REPL_SANDBOX_ALLOW_STALE=1",
		"AGENT_REPL_SANDBOX_SLOT_DIR="+t.TempDir(), "AGENT_REPL_SANDBOX_NO_WEBAPP_BUILD=1")
	out, _ := cmd.CombinedOutput()

	// Assert: the stamp gate did not refuse.
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

// containerCPUArgs runs the script's own container_cpu_args section on its
// own, the way currentSandboxStamp runs the stamp section: the rule under
// test is the script's, never a copy of it kept here.
func containerCPUArgs(t *testing.T, cpus string) (string, error) {
	t.Helper()
	script := sandboxScript(t)
	cmd := exec.Command("bash", "-c",
		fmt.Sprintf(`set -euo pipefail; eval "$(sed -n '/^# --- the container.s CPU cap/,/^# --- run/p' %q | sed '$d')"; container_cpu_args %q`,
			script, cpus))
	out, err := cmd.CombinedOutput()
	return strings.TrimSpace(string(out)), err
}

// TestSandboxContainerCPUArgs: the container is capped to exactly the CPUs it
// is given, both by the runtime and by every Go process inside it, and a
// width that is not a positive whole number is refused rather than dropped.
func TestSandboxContainerCPUArgs(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name    string
		cpus    string
		want    string
		wantErr bool
	}{
		{name: "four CPUs", cpus: "4", want: "--cpus 4 --env GOMAXPROCS=4"},
		{name: "one CPU", cpus: "1", want: "--cpus 1 --env GOMAXPROCS=1"},
		{name: "zero is refused", cpus: "0", want: "AGENT_REPL_SANDBOX_CPUS must be a positive whole number of CPUs, got '0'", wantErr: true},
		{name: "a fraction is refused", cpus: "1.5", want: "AGENT_REPL_SANDBOX_CPUS must be a positive whole number of CPUs, got '1.5'", wantErr: true},
		{name: "empty is refused", cpus: "", want: "AGENT_REPL_SANDBOX_CPUS must be a positive whole number of CPUs, got ''", wantErr: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			// Act.
			got, err := containerCPUArgs(t, tt.cpus)

			// Assert.
			if (err != nil) != tt.wantErr || got != tt.want {
				t.Fatalf("container_cpu_args %q = %q, %v; want %q (error %v)", tt.cpus, got, err, tt.want, tt.wantErr)
			}
		})
	}
}

// TestE2EEmacsScriptHandsItsSlotWidthToTheSandbox: under testrun the suite
// holds AGENT_REPL_UNIT_SLOTS core slots, and the container must be capped to
// exactly that many CPUs; a direct run leaves the sandbox's own default.
func TestE2EEmacsScriptHandsItsSlotWidthToTheSandbox(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name  string
		extra []string
		want  string
	}{
		{name: "the unit's width becomes the container's CPUs", extra: []string{"AGENT_REPL_UNIT_SLOTS=4"}, want: "cpus=4"},
		{name: "a direct run leaves the sandbox default", want: "cpus=unset"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			// Arrange: the suite script in a module of its own, beside a
			// sandbox runner that reports the CPU cap it was handed.
			script := sandboxSuiteModule(t, "test-e2e-emacs.sh", "[ \"$1\" = preflight ] && exit 0\n"+
				"printf 'cpus=%s\\n' \"${AGENT_REPL_SANDBOX_CPUS:-unset}\"\n")
			cmd := exec.Command("bash", script)
			var env []string
			for _, kv := range os.Environ() {
				if !strings.HasPrefix(kv, "AGENT_REPL_UNIT_SLOTS=") && !strings.HasPrefix(kv, "AGENT_REPL_SANDBOX_CPUS=") {
					env = append(env, kv)
				}
			}
			cmd.Env = append(append(env, "AGENT_REPL_BACKGROUND_PRIORITY=1"), tt.extra...)

			// Act.
			out, err := cmd.CombinedOutput()

			// Assert.
			if err != nil || !strings.Contains(string(out), tt.want+"\n") {
				t.Fatalf("test-e2e-emacs.sh = %v:\n%s\nwant a line %q", err, out, tt.want)
			}
		})
	}
}

// TestSandboxImageIsTaggedByTheSandboxStamp: every checkout's sandbox image is
// its own, named by the sandbox sources it is built from, so no build can
// evict another checkout's image.
func TestSandboxImageIsTaggedByTheSandboxStamp(t *testing.T) {
	t.Parallel()
	// Arrange.
	f := newFakeDocker(t)
	stamp := currentSandboxStamp(t, f)

	// Act.
	image := sandboxLib(t, f, "sandbox_image")

	// Assert.
	if want := "agent-repl-e2e-sandbox:" + stamp; image != want {
		t.Fatalf("sandbox_image = %q, want %q", image, want)
	}
}

// TestSandboxImageHonorsAnExplicitImage: AGENT_REPL_SANDBOX_IMAGE still names
// an image deliberately, over the stamp's.
func TestSandboxImageHonorsAnExplicitImage(t *testing.T) {
	t.Parallel()
	// Arrange.
	f := newFakeDocker(t)

	// Act.
	image := sandboxLib(t, f, "sandbox_image", "AGENT_REPL_SANDBOX_IMAGE=elsewhere:pinned")

	// Assert.
	if image != "elsewhere:pinned" {
		t.Fatalf("sandbox_image = %q, want the explicit elsewhere:pinned", image)
	}
}

// TestSandboxBuildTagsTheStampImage: a build tags the image the checkout's
// stamp names, never a shared tag another checkout also uses.
func TestSandboxBuildTagsTheStampImage(t *testing.T) {
	t.Parallel()
	// Arrange.
	f := newFakeDocker(t)
	want := "-t agent-repl-e2e-sandbox:" + currentSandboxStamp(t, f) + " "
	lock := filepath.Join(t.TempDir(), "build.lock")

	// Act.
	cmd := sandboxBuildUnstaged(t)
	cmd.Env = sandboxEnv(f, lock)
	out, err := cmd.CombinedOutput()

	// Assert.
	if err != nil {
		t.Fatalf("build exited %v\n%s", err, out)
	}
	for _, c := range f.calls(t) {
		if strings.HasPrefix(c, "build ") && strings.Contains(c, want) {
			return
		}
	}
	t.Fatalf("no build tagged %q; calls:\n%s", want, strings.Join(f.calls(t), "\n"))
}

// TestSandboxPreflightNamesTheStampImageWhenItIsNotBuilt: the preflight looks
// for this checkout's own image, and says exactly which one is missing.
func TestSandboxPreflightNamesTheStampImageWhenItIsNotBuilt(t *testing.T) {
	t.Parallel()
	// Arrange.
	f := newFakeDocker(t)
	f.imageMissing(t)
	image := "agent-repl-e2e-sandbox:" + currentSandboxStamp(t, f)
	lock := filepath.Join(t.TempDir(), "build.lock")

	// Act.
	cmd := exec.Command("bash", sandboxScript(t), "preflight")
	cmd.Env = sandboxEnv(f, lock)
	out, err := cmd.CombinedOutput()

	// Assert.
	var exit *exec.ExitError
	if !errors.As(err, &exit) || exit.ExitCode() != 12 {
		t.Fatalf("preflight = %v, want exit 12\n%s", err, out)
	}
	if !strings.Contains(string(out), "'"+image+"' is not built") {
		t.Fatalf("preflight did not name %s:\n%s", image, out)
	}
}

// TestSandboxPreflightReadsFreeDiskFromDf: the preflight's disk gate
// answers from the free space df reports, declining below the floor and
// passing at it.
func TestSandboxPreflightReadsFreeDiskFromDf(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name     string
		freeGiB  int
		wantExit int
		wantOut  string
	}{
		{"declines below the floor", 11, 13, "11GiB free"},
		{"passes at the floor", 12, 0, "READY"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			t.Parallel()
			// Arrange.
			f := newFakeDocker(t)
			f.setFreeKB(t, tt.freeGiB*1024*1024)
			cmd := exec.Command("bash", sandboxScript(t), "preflight")
			cmd.Env = sandboxEnv(f, filepath.Join(t.TempDir(), "build.lock"))

			// Act.
			out, err := cmd.CombinedOutput()

			// Assert.
			code := 0
			var exit *exec.ExitError
			if errors.As(err, &exit) {
				code = exit.ExitCode()
			} else if err != nil {
				t.Fatalf("preflight = %v\n%s", err, out)
			}
			if code != tt.wantExit || !strings.Contains(string(out), tt.wantOut) {
				t.Fatalf("preflight exit %d, want %d naming %q:\n%s", code, tt.wantExit, tt.wantOut, out)
			}
		})
	}
}

// e2eEmacsModule lays the suite script in a module of its own beside a fake
// sandbox runner whose behaviour is SANDBOX, and answers the script's path.
func e2eEmacsModule(t *testing.T, sandbox string) string {
	t.Helper()
	return sandboxSuiteModule(t, "test-e2e-emacs.sh", sandbox)
}

// sandboxSuiteModule lays the sandboxed suite script SCRIPT, with the
// bin/lib-sandbox-suite.sh it sources, in a module of its own beside a fake
// sandbox runner whose behaviour is SANDBOX, and answers the script's path.
func sandboxSuiteModule(t *testing.T, script, sandbox string) string {
	t.Helper()
	wd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	root := t.TempDir()
	layout := map[string]string{
		filepath.Join(root, "e2e", "sandbox", "bin", "e2e-sandbox.sh"): "#!/usr/bin/env bash\nset -u\nhere=$(cd -- \"$(dirname -- \"$0\")\" && pwd)\n" + sandbox,
	}
	for _, name := range []string{script, "lib-sandbox-suite.sh"} {
		body, err := os.ReadFile(filepath.Join(wd, "..", "..", "bin", name))
		if err != nil {
			t.Fatal(err)
		}
		layout[filepath.Join(root, "bin", name)] = string(body)
	}
	for path, body := range layout {
		if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(path, []byte(body), 0o755); err != nil {
			t.Fatal(err)
		}
	}
	return filepath.Join(root, "bin", script)
}

// runE2EEmacs runs the suite script at SCRIPT, answering its output and exit.
func runE2EEmacs(t *testing.T, script string) (string, int) {
	t.Helper()
	cmd := exec.Command("bash", script)
	cmd.Env = append(os.Environ(), "AGENT_REPL_BACKGROUND_PRIORITY=1")
	out, err := cmd.CombinedOutput()
	var exit *exec.ExitError
	switch {
	case err == nil:
		return string(out), 0
	case errors.As(err, &exit):
		return string(out), exit.ExitCode()
	default:
		t.Fatalf("run test-e2e-emacs.sh: %v\n%s", err, out)
		return "", 0
	}
}

// builtSandbox is a fake runner whose image does not exist until `build`.
const builtSandbox = `case $1 in
  preflight) [ -e "$here/built" ] && { echo READY; exit 0; }; echo "image not built"; exit 12 ;;
  build) touch "$here/built"; echo BUILT ;;
  run) echo RAN ;;
esac
`

func TestE2EEmacsBuildsThisCheckoutsImageWhenItIsNotBuilt(t *testing.T) {
	t.Parallel()
	// Arrange.
	script := e2eEmacsModule(t, builtSandbox)

	// Act.
	out, code := runE2EEmacs(t, script)

	// Assert.
	if code != 0 || !strings.Contains(out, "BUILT\n") || !strings.Contains(out, "RAN\n") {
		t.Fatalf("test-e2e-emacs.sh exit %d, want the image built and the layer run:\n%s", code, out)
	}
}

func TestE2EEmacsFailsWhenTheImageCannotBeBuilt(t *testing.T) {
	t.Parallel()
	// Arrange: the build fails.
	script := e2eEmacsModule(t, `case $1 in
  preflight) echo "image not built"; exit 12 ;;
  build) echo "BUILD BROKE"; exit 1 ;;
  run) echo RAN ;;
esac
`)

	// Act.
	out, code := runE2EEmacs(t, script)

	// Assert: a failure, never the decline (77) and never a run.
	if code == 0 || code == 77 || strings.Contains(out, "RAN\n") {
		t.Fatalf("test-e2e-emacs.sh exit %d, want a failure without a run:\n%s", code, out)
	}
}

func TestE2EEmacsDeclinesAnUnusableSandboxWithoutBuilding(t *testing.T) {
	t.Parallel()
	// Arrange: the runtime is down.
	script := e2eEmacsModule(t, `case $1 in
  preflight) echo "docker is not running"; exit 11 ;;
  build) echo BUILT ;;
  run) echo RAN ;;
esac
`)

	// Act.
	out, code := runE2EEmacs(t, script)

	// Assert.
	if code != 77 || strings.Contains(out, "BUILT\n") || strings.Contains(out, "RAN\n") {
		t.Fatalf("test-e2e-emacs.sh exit %d, want the decline (77) with no build and no run:\n%s", code, out)
	}
}

// TestSandboxRunStreamsTheWorkingCopyAndMountsNoHostPath pins the fix for the
// host's file-table exhaustion: Docker Desktop's file sharing opened every
// file of a bind-mounted checkout and never released them, ~4,500 host
// descriptors per run, until every e2e-emacs gate failed staging with "Too
// many open files in system". So `run` must mount NO host path at all: the
// working copy rides the container's stdin as a tar, and the failure
// artifacts live in a VM-side volume that `docker cp` copies out.
func TestSandboxRunStreamsTheWorkingCopyAndMountsNoHostPath(t *testing.T) {
	t.Parallel()
	// Arrange: an image carrying this checkout's stamp, a private slot dir, no
	// host webapp build, and artifacts requested.
	f := newFakeDocker(t)
	f.setLabel(t, currentSandboxStamp(t, f))
	artifacts := t.TempDir()
	cmd := exec.Command("bash", sandboxScript(t), "run", "true")
	cmd.Env = sandboxEnv(f, filepath.Join(t.TempDir(), "build.lock"),
		"AGENT_REPL_SANDBOX_SLOT_DIR="+t.TempDir(), "AGENT_REPL_SANDBOX_NO_WEBAPP_BUILD=1",
		"AGENT_REPL_E2E_ARTIFACTS="+artifacts)

	// Act
	out, err := cmd.CombinedOutput()

	// Assert: the run went through, with no bind mount and stdin open.
	if err != nil {
		t.Fatalf("run = %v\n%s", err, out)
	}
	var run string
	for _, c := range f.calls(t) {
		if strings.HasPrefix(c, "run ") {
			run = c
		}
	}
	if run == "" {
		t.Fatalf("no `run` invocation recorded:\n%s", strings.Join(f.calls(t), "\n"))
	}
	for _, banned := range []string{"type=bind", " -v ", "--volume", "/repo-src"} {
		if strings.Contains(run, banned) {
			t.Fatalf("docker run names %q, want no host path mounted:\n%s", banned, run)
		}
	}
	if !strings.Contains(run, "--interactive") || !strings.Contains(run, "type=volume,target=/artifacts") {
		t.Fatalf("docker run = %q, want --interactive and the artifacts on a VM-side volume", run)
	}
	// The container must outlive its exit, or the `docker cp` below finds
	// it gone and every failure's artifacts are lost.
	if strings.Contains(run, "--rm") {
		t.Fatalf("docker run = %q, want no --rm: the artifacts are copied out after the exit", run)
	}

	// Assert: what arrived on stdin is the staged working copy.
	list, err := exec.Command("tar", "-tf", filepath.Join(f.dir, "run-stdin")).Output()
	if err != nil {
		t.Fatalf("the run's stdin is not a tar: %v", err)
	}
	entries := strings.Split(strings.TrimSpace(string(list)), "\n")
	if !slices.Contains(entries, "e2e/go.mod") || !slices.Contains(entries, "metaprompt.md") {
		t.Fatalf("the streamed copy (%d entries) lacks e2e/go.mod or metaprompt.md", len(entries))
	}
	// Directories ride as entries of their own, so their mtimes survive: the
	// webapp dist's staleness rule counts directories, and one created only
	// on the way to a file is stamped with the extraction time.
	if !slices.Contains(entries, "webapp/src/") && !slices.Contains(entries, "webapp/src") {
		t.Fatalf("the streamed copy carries no entry for the directory webapp/src, so its mtime is lost")
	}
	for _, e := range entries {
		if strings.Contains("/"+e+"/", "/.git/") || strings.Contains("/"+e+"/", "/node_modules/") {
			t.Fatalf("the streamed copy carries %q, which staging excludes", e)
		}
	}

	// Assert: the artifacts left through the API, and the container was removed.
	var copied, removed bool
	for _, c := range f.calls(t) {
		copied = copied || (strings.HasPrefix(c, "cp ") && strings.Contains(c, ":/artifacts/."))
		removed = removed || strings.HasPrefix(c, "rm -f -v agent-repl-e2e-")
	}
	if !copied || !removed {
		t.Fatalf("calls = %q, want a `docker cp` of /artifacts and the container removed", f.calls(t))
	}
}

// TestSandboxShellExecsALoginShellOnceTheCopyIsStaged pins `shell` under the
// streamed working copy: stdin carries the tar, so the shell cannot be the
// container's own command. The container holds itself open, and the login
// shell is exec'd into it once the entrypoint's "staged" line arrives.
func TestSandboxShellExecsALoginShellOnceTheCopyIsStaged(t *testing.T) {
	t.Parallel()
	// Arrange
	f := newFakeDocker(t)
	f.setLabel(t, currentSandboxStamp(t, f))
	cmd := exec.Command("bash", sandboxScript(t), "shell")
	cmd.Env = sandboxEnv(f, filepath.Join(t.TempDir(), "build.lock"),
		"AGENT_REPL_SANDBOX_SLOT_DIR="+t.TempDir(), "AGENT_REPL_SANDBOX_NO_WEBAPP_BUILD=1")

	// Act
	out, err := cmd.CombinedOutput()

	// Assert: the container ran on the stream, then the shell was exec'd in.
	if err != nil {
		t.Fatalf("shell = %v\n%s", err, out)
	}
	calls := f.calls(t)
	ran := slices.IndexFunc(calls, func(c string) bool { return strings.HasPrefix(c, "run ") && strings.HasSuffix(c, " sleep infinity") })
	execd := slices.IndexFunc(calls, func(c string) bool {
		return strings.HasPrefix(c, "exec -it --workdir /work/repo/modules/app/agent-repl agent-repl-e2e-") && strings.HasSuffix(c, " bash -l")
	})
	if ran < 0 || execd < ran {
		t.Fatalf("calls = %q, want the held container run, then a login shell exec'd into it", calls)
	}
}

// TestSourcingTheSandboxScriptDispatchesNothing pins the guard
// sandboxBuildUnstaged depends on: sourced, e2e-sandbox.sh defines its
// functions and runs no subcommand, not even the help a bare run prints.
func TestSourcingTheSandboxScriptDispatchesNothing(t *testing.T) {
	t.Parallel()
	// Arrange
	cmd := exec.Command("bash", "-c", `set -euo pipefail; . "$1"; declare -F do_build do_run main >/dev/null; echo sourced`, "bash", sandboxScript(t))

	// Act
	out, err := cmd.CombinedOutput()

	// Assert
	if err != nil || strings.TrimSpace(string(out)) != "sourced" {
		t.Fatalf("sourcing the script = %v, %q; want its functions defined and nothing dispatched", err, out)
	}
}

// TestEachSandboxedSuiteRunsItsOwnTestsInTheSandbox: both sandboxed suites
// go through lib-sandbox-suite.sh, each running its own tests and declining
// (77) when the sandbox is unusable.
func TestEachSandboxedSuiteRunsItsOwnTestsInTheSandbox(t *testing.T) {
	t.Parallel()
	tests := []struct {
		name, script, pattern, label string
	}{
		{name: "the Emacs client layer and the Doom tier", script: "test-e2e-emacs.sh", pattern: "TestEmacs|TestDoom", label: "e2e-emacs"},
	}
	for _, tt := range tests {
		t.Run(tt.name+" runs its tests", func(t *testing.T) {
			t.Parallel()
			// Arrange
			script := sandboxSuiteModule(t, tt.script, "[ \"$1\" = preflight ] && exit 0\necho \"RAN $*\"\n")

			// Act
			out, code := runE2EEmacs(t, script)

			// Assert
			if code != 0 || !strings.Contains(out, "RAN run --dir e2e go test ./ -run "+tt.pattern+" -v\n") || !strings.Contains(out, "["+tt.label+"] running") {
				t.Fatalf("%s exit %d, want %s run in the sandbox under [%s]:\n%s", tt.script, code, tt.pattern, tt.label, out)
			}
		})
		t.Run(tt.name+" declines an unusable sandbox", func(t *testing.T) {
			t.Parallel()
			// Arrange
			script := sandboxSuiteModule(t, tt.script, "[ \"$1\" = preflight ] && { echo 'docker is not running'; exit 3; }\necho RAN\n")

			// Act
			out, code := runE2EEmacs(t, script)

			// Assert
			if code != 77 || strings.Contains(out, "RAN") || !strings.Contains(out, "docker is not running") {
				t.Fatalf("%s exit %d, want a 77 decline naming the preflight's reason and nothing run:\n%s", tt.script, code, out)
			}
		})
	}
}

// TestNoSuiteScriptRunsTheSandboxItself: a suite that ran the sandbox on its
// own would drift from the shared preflight, build and decline.
func TestNoSuiteScriptRunsTheSandboxItself(t *testing.T) {
	t.Parallel()
	wd, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
	}
	scripts, err := filepath.Glob(filepath.Join(wd, "..", "..", "bin", "test-*.sh"))
	if err != nil {
		t.Fatal(err)
	}
	for _, path := range scripts {
		body, err := os.ReadFile(path)
		if err != nil {
			t.Fatal(err)
		}
		if strings.Contains(string(body), `"$SANDBOX" run`) || strings.Contains(string(body), "e2e-sandbox.sh run") {
			t.Errorf("%s runs the sandbox itself; source bin/lib-sandbox-suite.sh and call run_sandbox_suite", filepath.Base(path))
		}
	}
}
