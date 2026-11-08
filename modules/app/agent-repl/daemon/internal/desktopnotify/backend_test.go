package desktopnotify

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/sourcescan"
)

// fakeRunner answers one scripted stdout and records the argv it was handed.
type fakeRunner struct {
	stdout string
	err    error
	bin    string
	args   []string
	// startErr, when set, is a program that never started: Run answers it
	// without calling started.
	startErr error
	// exited is set once Run has answered.
	exited bool
	// deadline is the deadline Run's context carried, zero when it had none.
	deadline time.Time
	// block, when set, is waited on before answering (a banner awaiting a
	// click), and ctx's end answers ctx.Err().
	block chan struct{}
}

func (r *fakeRunner) Run(ctx context.Context, bin string, args []string, started func()) (string, error) {
	r.bin, r.args = bin, args
	r.deadline, _ = ctx.Deadline()
	defer func() { r.exited = true }()
	if r.startErr != nil {
		return "", r.startErr
	}
	if started != nil {
		started()
	}
	if r.block != nil {
		select {
		case <-r.block:
		case <-ctx.Done():
			return "", ctx.Err()
		}
	}
	return r.stdout, r.err
}

func noLookPath(string) (string, error) { return "", errors.New("not on PATH") }

func onPath(name string) (string, error) { return "/usr/bin/" + name, nil }

// target is the fixture's banner target.
var target = Target{ID: "ws1", Dir: "/work/ws1"}

func mustPlatform(t *testing.T, goos string) Platform {
	t.Helper()
	p, err := PlatformFor(goos)
	if err != nil {
		t.Fatalf("PlatformFor(%s): %v", goos, err)
	}
	return p
}

func TestPlatformForNamesEachPlatformsProgram(t *testing.T) {
	cases := []struct {
		goos string
		want string
	}{
		{goos: "linux", want: "notify-send"},
	}
	for _, tc := range cases {
		t.Run(tc.goos, func(t *testing.T) {
			// Act
			p, err := PlatformFor(tc.goos)

			// Assert
			if err != nil {
				t.Fatalf("PlatformFor: %v", err)
			}
			if p.Program != tc.want {
				t.Fatalf("program = %q, want %q", p.Program, tc.want)
			}
		})
	}
}

func TestPlatformForRefusesAPlatformWithNoBannerProgram(t *testing.T) {
	for _, goos := range []string{"windows", "darwin"} {
		t.Run(goos, func(t *testing.T) {
			// Act
			_, err := PlatformFor(goos)

			// Assert
			if err == nil {
				t.Fatalf("%s was given a banner program; macOS banners are the Zettel app's and Windows has none", goos)
			}
		})
	}
}

func TestNewBackendTakesTheOverride(t *testing.T) {
	// Act
	b, err := NewBackend(mustPlatform(t, "linux"), "/tmp/fake-notifier", noLookPath, &fakeRunner{})

	// Assert
	if err != nil {
		t.Fatalf("NewBackend: %v", err)
	}
	if b.Program() != "/tmp/fake-notifier" {
		t.Fatalf("program = %q, want the override", b.Program())
	}
}

func TestNewBackendLooksTheProgramUpOnPath(t *testing.T) {
	// Act
	b, err := NewBackend(mustPlatform(t, "linux"), "", onPath, &fakeRunner{})

	// Assert
	if err != nil {
		t.Fatalf("NewBackend: %v", err)
	}
	if b.Program() != "/usr/bin/notify-send" {
		t.Fatalf("program = %q, want the PATH answer", b.Program())
	}
}

func TestNewBackendRefusesAMissingProgram(t *testing.T) {
	// Act
	_, err := NewBackend(mustPlatform(t, "linux"), "", noLookPath, &fakeRunner{})

	// Assert
	if err == nil {
		t.Fatal("a missing banner program was accepted")
	}
}

func TestNotifySendBackendSpeaksNotifySendsArgv(t *testing.T) {
	// Arrange
	runner := &fakeRunner{}
	b, _ := NewBackend(mustPlatform(t, "linux"), "", onPath, runner)

	// Act
	_, err := b.Post(context.Background(), target, Banner{Title: "T", Body: "B"}, func() {})

	// Assert
	if err != nil {
		t.Fatalf("Post: %v", err)
	}
	want := []string{"--app-name=Emacs", "--hint=boolean:suppress-sound:true", "--action=default=Open", "--wait", "--expire-time=10000", "T", "B"}
	if !reflect.DeepEqual(runner.args, want) {
		t.Fatalf("argv = %q, want %q", runner.args, want)
	}
}

func TestBackendReadsTheOutcome(t *testing.T) {
	cases := []struct {
		name   string
		goos   string
		stdout string
		want   Outcome
	}{
		{name: "notify-send action", goos: "linux", stdout: "default\n", want: Clicked},
		{name: "notify-send dismissed", goos: "linux", stdout: "", want: Unclicked},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			runner := &fakeRunner{}
			b, err := NewBackend(mustPlatform(t, tc.goos), "", onPath, runner)
			if err != nil {
				t.Fatalf("NewBackend: %v", err)
			}
			runner.stdout = tc.stdout

			// Act
			outcome, err := b.Post(context.Background(), target, Banner{Title: "T", Body: "B"}, func() {})

			// Assert
			if err != nil {
				t.Fatalf("Post: %v", err)
			}
			if outcome != tc.want {
				t.Fatalf("outcome = %v, want %v", outcome, tc.want)
			}
		})
	}
}

func TestBackendSurfacesAFailedProgram(t *testing.T) {
	// Arrange
	cause := errors.New("exit status 3: notifications are not authorized")
	runner := &fakeRunner{}
	b, err := NewBackend(mustPlatform(t, "linux"), "", onPath, runner)
	if err != nil {
		t.Fatalf("NewBackend: %v", err)
	}
	runner.err = cause

	// Act
	outcome, err := b.Post(context.Background(), target, Banner{Title: "T", Body: "B"}, func() {})

	// Assert
	if !errors.Is(err, cause) || outcome != Unclicked {
		t.Fatalf("Post = (%v, %v), want (Unclicked, the program's failure)", outcome, err)
	}
}

// TestBackendReportsTheBannerMadeExactlyWhenItIsMade pins the made edge the
// chime rings from: a read-back banner is made once its program is up showing
// it, and a banner whose program never started is never made.
func TestBackendReportsTheBannerMadeExactlyWhenItIsMade(t *testing.T) {
	cases := []struct {
		name      string
		goos      string
		err       error
		startErr  error
		wantMade  bool
		wantAfter bool
	}{
		{name: "a read-back banner is made once its program is up", goos: "linux", wantMade: true, wantAfter: false},
		{name: "a read-back banner whose program never started is not made", goos: "linux", startErr: errors.New("exec: no such file")},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			runner := &fakeRunner{}
			b, err := NewBackend(mustPlatform(t, tc.goos), "", onPath, runner)
			if err != nil {
				t.Fatalf("NewBackend: %v", err)
			}
			runner.stdout, runner.err, runner.startErr, runner.exited = "", tc.err, tc.startErr, false
			made, madeAfterExit := 0, false

			// Act
			_, _ = b.Post(context.Background(), target, Banner{Title: "T", Body: "B"}, func() {
				made++
				madeAfterExit = runner.exited
			})

			// Assert
			wantCount := 0
			if tc.wantMade {
				wantCount = 1
			}
			if made != wantCount {
				t.Fatalf("made called %d times, want %d", made, wantCount)
			}
			if tc.wantMade && madeAfterExit != tc.wantAfter {
				t.Fatalf("made after the program exited = %v, want %v", madeAfterExit, tc.wantAfter)
			}
		})
	}
}

func TestResolveProgram(t *testing.T) {
	cases := []struct {
		name     string
		override string
		lookPath func(string) (string, error)
		want     string
		wantErr  string
	}{
		{name: "override wins", override: "/tmp/fake", lookPath: noLookPath, want: "/tmp/fake"},
		{name: "found on PATH", lookPath: onPath, want: "/usr/bin/player"},
		{name: "missing names the role and program", lookPath: noLookPath,
			wantErr: `the chime program "player" is not installed: not on PATH`},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := resolveProgram("chime", "player", tc.override, tc.lookPath)

			// Assert
			if tc.wantErr != "" {
				if err == nil || err.Error() != tc.wantErr {
					t.Fatalf("err = %v, want %q", err, tc.wantErr)
				}
				return
			}
			if err != nil || got != tc.want {
				t.Fatalf("resolveProgram = (%q, %v), want %q", got, err, tc.want)
			}
		})
	}
}

// TestBannerAndChimeResolveThroughOneHelper pins that both programs share
// resolveProgram's rule: a missing one is refused with its role named.
func TestBannerAndChimeResolveThroughOneHelper(t *testing.T) {
	// Act
	_, bannerErr := NewBackend(mustPlatform(t, "linux"), "", noLookPath, &fakeRunner{})
	_, chimeErr := NewChime(mustChimePlatform(t, "darwin"), "", noLookPath, &fakeRunner{})

	// Assert
	if bannerErr == nil || bannerErr.Error() != `the banner program "terminal-notifier" is not installed: not on PATH` {
		t.Fatalf("banner err = %v", bannerErr)
	}
	if chimeErr == nil || chimeErr.Error() != `the chime program "afplay" is not installed: not on PATH` {
		t.Fatalf("chime err = %v", chimeErr)
	}
}

func mustChimePlatform(t *testing.T, goos string) ChimePlatform {
	t.Helper()
	p, err := ChimePlatformFor(goos)
	if err != nil {
		t.Fatalf("ChimePlatformFor(%s): %v", goos, err)
	}
	return p
}

func TestChimePlatformForNamesEachPlatformsPlayer(t *testing.T) {
	cases := []struct {
		goos     string
		wantProg string
		wantArgs []string
	}{
		{goos: "darwin", wantProg: "afplay", wantArgs: []string{"/System/Library/Sounds/Glass.aiff"}},
		{goos: "linux", wantProg: "canberra-gtk-play", wantArgs: []string{"--id=complete"}},
	}
	for _, tc := range cases {
		t.Run(tc.goos, func(t *testing.T) {
			// Act
			p, err := ChimePlatformFor(tc.goos)

			// Assert
			if err != nil {
				t.Fatalf("ChimePlatformFor: %v", err)
			}
			if p.Program != tc.wantProg || !reflect.DeepEqual(p.Args, tc.wantArgs) {
				t.Fatalf("platform = %+v, want %s %q", p, tc.wantProg, tc.wantArgs)
			}
		})
	}
}

func TestChimePlatformForRefusesAnUnsupportedPlatform(t *testing.T) {
	// Act
	_, err := ChimePlatformFor("windows")

	// Assert
	if err == nil {
		t.Fatal("an unsupported platform was given a chime")
	}
}

func TestNewChimeTakesTheOverride(t *testing.T) {
	// Act
	c, err := NewChime(mustChimePlatform(t, "darwin"), "/tmp/fake-chime", noLookPath, &fakeRunner{})

	// Assert
	if err != nil || c.Program() != "/tmp/fake-chime" {
		t.Fatalf("NewChime = (%v, %v), want the override", c, err)
	}
}

func TestChimePlaysThePlatformsSound(t *testing.T) {
	// Arrange
	runner := &fakeRunner{}
	c, _ := NewChime(mustChimePlatform(t, "darwin"), "", onPath, runner)

	// Act
	err := c.Play(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("Play: %v", err)
	}
	want := []string{"/System/Library/Sounds/Glass.aiff"}
	if runner.bin != "/usr/bin/afplay" || !reflect.DeepEqual(runner.args, want) {
		t.Fatalf("ran %s %q, want /usr/bin/afplay %q", runner.bin, runner.args, want)
	}
}

func TestChimeSurfacesAFailedPlayer(t *testing.T) {
	// Arrange
	cause := errors.New("exit status 1")
	c, _ := NewChime(mustChimePlatform(t, "linux"), "", onPath, &fakeRunner{err: cause})

	// Act
	err := c.Play(context.Background())

	// Assert
	if !errors.Is(err, cause) {
		t.Fatalf("Play = %v, want the player's failure", err)
	}
}

// TestExecRunnerHelperProcess is the program TestExecRunner runs: this test
// binary re-executed, so no real banner program or player is ever started.
func TestExecRunnerHelperProcess(t *testing.T) {
	switch os.Getenv("DESKTOPNOTIFY_HELPER") {
	case "":
		return
	case "ok":
		fmt.Print("default")
		os.Exit(0)
	default:
		fmt.Fprint(os.Stderr, "refused")
		os.Exit(3)
	}
}

func TestExecRunner(t *testing.T) {
	cases := []struct {
		name        string
		mode        string
		wantStdout  string
		wantErr     string
		wantStarted int
	}{
		{name: "a program that runs calls started once and answers its stdout", mode: "ok", wantStdout: "default", wantStarted: 1},
		{name: "a program that exits non-zero answers its stderr", mode: "fail", wantErr: "refused", wantStarted: 1},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			t.Setenv("DESKTOPNOTIFY_HELPER", tc.mode)
			started := 0

			// Act
			out, err := ExecRunner{}.Run(context.Background(), os.Args[0], []string{"-test.run=^TestExecRunnerHelperProcess$"}, func() { started++ })

			// Assert
			if started != tc.wantStarted {
				t.Fatalf("started called %d times, want %d", started, tc.wantStarted)
			}
			if tc.wantErr == "" && (err != nil || out != tc.wantStdout) {
				t.Fatalf("Run = (%q, %v), want (%q, nil)", out, err, tc.wantStdout)
			}
			if tc.wantErr != "" && (err == nil || !strings.Contains(err.Error(), tc.wantErr)) {
				t.Fatalf("Run err = %v, want one carrying %q", err, tc.wantErr)
			}
		})
	}
}

func TestExecRunnerCallsNoStartedForAProgramThatNeverStarted(t *testing.T) {
	// Arrange
	started := 0

	// Act
	_, err := ExecRunner{}.Run(context.Background(), filepath.Join(t.TempDir(), "absent"), nil, func() { started++ })

	// Assert
	if err == nil || started != 0 {
		t.Fatalf("Run = %v with started called %d times, want an error and no start", err, started)
	}
}

func TestRunWithinBoundsTheRun(t *testing.T) {
	cases := []struct {
		name    string
		block   bool
		wantErr error
	}{
		{name: "a run inside its bound answers the program's output", block: false, wantErr: nil},
		{name: "a run past its bound answers the deadline", block: true, wantErr: context.DeadlineExceeded},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			runner := &fakeRunner{stdout: "out"}
			if tc.block {
				runner.block = make(chan struct{})
			}
			started := false

			// Act
			out, err := runWithin(context.Background(), time.Millisecond, runner, "/bin/x", []string{"a"}, func() { started = true })

			// Assert
			if !errors.Is(err, tc.wantErr) {
				t.Fatalf("runWithin = %v, want %v", err, tc.wantErr)
			}
			if tc.wantErr == nil && out != "out" {
				t.Fatalf("runWithin answered %q, want the program's output", out)
			}
			if !started || runner.bin != "/bin/x" || !reflect.DeepEqual(runner.args, []string{"a"}) {
				t.Fatalf("ran %s %q (started=%v), want /bin/x [a] started", runner.bin, runner.args, started)
			}
		})
	}
}

// TestEveryProgramRunGoesThroughRunWithin pins that no backend, chime or
// speech bounds a run of its own: the one bounded run is runWithin, across
// every production source of the package.
func TestEveryProgramRunGoesThroughRunWithin(t *testing.T) {
	cases := []struct {
		substr string
		want   []string
	}{
		{substr: "context.WithTimeout(", want: []string{"backend.go"}},
		{substr: "runner.Run(", want: []string{"backend.go"}},
	}
	for _, tc := range cases {
		t.Run(tc.substr, func(t *testing.T) {
			// Act
			got := sourcescan.Sites(t, tc.substr)

			// Assert
			if !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("%q occurs in %v, want only runWithin's %v", tc.substr, got, tc.want)
			}
		})
	}
}

func TestExecRunnerRefusesAnOutputThatCannotBeCapturedWithoutStarting(t *testing.T) {
	// Arrange: no file for the program's output can be made.
	t.Setenv("DESKTOPNOTIFY_HELPER", "ok")
	t.Setenv("TMPDIR", filepath.Join(t.TempDir(), "absent"))
	started := 0

	// Act
	_, err := ExecRunner{}.Run(context.Background(), os.Args[0], []string{"-test.run=^TestExecRunnerHelperProcess$"}, func() { started++ })

	// Assert
	if err == nil || !strings.Contains(err.Error(), "make the child's stdout file") {
		t.Fatalf("Run err = %v, want a refusal naming the stdout file", err)
	}
	if started != 0 {
		t.Fatalf("started called %d times, want never", started)
	}
}
