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

	"agent-repld/internal/ids"
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

// clickCommand is the fixture's click action: it names the target so a test
// can see which workspace the stored command selects.
func clickCommand(target Target) string {
	return "select " + string(target.ID) + " in " + target.Dir
}

// target is the fixture's banner target.
var target = Target{ID: "ws1", Dir: "/work/ws1"}

// version3 is what terminal-notifier 3.1.0 answers -version with.
const version3 = "terminal-notifier 3.1.0.\n"

func mustPlatform(t *testing.T, goos string) Platform {
	t.Helper()
	p, err := PlatformFor(goos, clickCommand)
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
		{goos: "darwin", want: "terminal-notifier"},
		{goos: "linux", want: "notify-send"},
	}
	for _, tc := range cases {
		t.Run(tc.goos, func(t *testing.T) {
			// Act
			p, err := PlatformFor(tc.goos, clickCommand)

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

func TestPlatformForRefusesAnUnsupportedPlatform(t *testing.T) {
	// Act
	_, err := PlatformFor("windows", clickCommand)

	// Assert
	if err == nil {
		t.Fatal("an unsupported platform was given a banner program")
	}
}

func TestPlatformForRefusesAMacOSBannerWithNoClickAction(t *testing.T) {
	// Assert
	defer func() {
		if recover() == nil {
			t.Fatal("a macOS banner was built with no click action")
		}
	}()

	// Act
	_, _ = PlatformFor("darwin", nil)
}

func TestNewBackendTakesTheOverride(t *testing.T) {
	// Act
	b, err := NewBackend(context.Background(), mustPlatform(t, "darwin"), "/tmp/fake-notifier", noLookPath, &fakeRunner{stdout: version3}, DefaultDeliveredBound)

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
	b, err := NewBackend(context.Background(), mustPlatform(t, "linux"), "", onPath, &fakeRunner{}, DefaultDeliveredBound)

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
	_, err := NewBackend(context.Background(), mustPlatform(t, "darwin"), "", noLookPath, &fakeRunner{stdout: version3}, DefaultDeliveredBound)

	// Assert
	if err == nil {
		t.Fatal("a missing banner program was accepted")
	}
}

func TestNewBackendChecksTheMinimumVersion(t *testing.T) {
	cases := []struct {
		name    string
		stdout  string
		err     error
		wantErr string
	}{
		{name: "version 3 is accepted", stdout: version3},
		{name: "a later major is accepted", stdout: "terminal-notifier 4.0.2."},
		{name: "version 2 is refused", stdout: "terminal-notifier 2.0.0.",
			wantErr: `the banner program /usr/bin/terminal-notifier is version 2, older than the 3 this platform needs: "terminal-notifier 2.0.0."`},
		{name: "an answer with no version is refused", stdout: "usage: terminal-notifier",
			wantErr: `the banner program /usr/bin/terminal-notifier answered no version: "usage: terminal-notifier"`},
		{name: "a failed query is refused", err: errors.New("exit status 1"),
			wantErr: "ask the banner program /usr/bin/terminal-notifier its version: exit status 1"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			runner := &fakeRunner{stdout: tc.stdout, err: tc.err}

			// Act
			_, err := NewBackend(context.Background(), mustPlatform(t, "darwin"), "", onPath, runner, DefaultDeliveredBound)

			// Assert
			if !reflect.DeepEqual(runner.args, []string{"-version"}) {
				t.Fatalf("asked the version with %q, want -version", runner.args)
			}
			if tc.wantErr == "" {
				if err != nil {
					t.Fatalf("NewBackend: %v", err)
				}
				return
			}
			if err == nil || err.Error() != tc.wantErr {
				t.Fatalf("err = %v, want %q", err, tc.wantErr)
			}
		})
	}
}

func TestNewBackendAsksNoVersionOfAProgramWithNoMinimum(t *testing.T) {
	// Arrange
	runner := &fakeRunner{}

	// Act
	_, err := NewBackend(context.Background(), mustPlatform(t, "linux"), "", onPath, runner, DefaultDeliveredBound)

	// Assert
	if err != nil || runner.args != nil {
		t.Fatalf("NewBackend = %v after running %q, want no version query", err, runner.args)
	}
}

func TestTerminalNotifierBackendSpeaksTerminalNotifiersArgv(t *testing.T) {
	cases := []struct {
		name   string
		banner Banner
		want   []string
	}{
		{name: "a banner carries no sound of its own", banner: Banner{Title: "T", Body: "B"}, want: []string{
			"-title", `\T`, "-message", `\B`,
			"-group", `\agent-repl:ws1`, "-activate", EmacsBundleID, "-execute", `\select ws1 in /work/ws1`}},
		{name: "a value NSUserDefaults would parse stays text", banner: Banner{Title: "[ws] {x}", Body: `"quoted" (y)`}, want: []string{
			"-title", `\[ws] {x}`, "-message", `\"quoted" (y)`,
			"-group", `\agent-repl:ws1`, "-activate", EmacsBundleID, "-execute", `\select ws1 in /work/ws1`}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			runner := &fakeRunner{stdout: version3}
			b, err := NewBackend(context.Background(), mustPlatform(t, "darwin"), "", onPath, runner, DefaultDeliveredBound)
			if err != nil {
				t.Fatalf("NewBackend: %v", err)
			}

			// Act
			_, err = b.Post(context.Background(), target, tc.banner, func() {})

			// Assert
			if err != nil {
				t.Fatalf("Post: %v", err)
			}
			if runner.bin != "/usr/bin/terminal-notifier" || !reflect.DeepEqual(runner.args, tc.want) {
				t.Fatalf("ran %s %q, want /usr/bin/terminal-notifier %q", runner.bin, runner.args, tc.want)
			}
		})
	}
}

func TestNotifySendBackendSpeaksNotifySendsArgv(t *testing.T) {
	// Arrange
	runner := &fakeRunner{}
	b, _ := NewBackend(context.Background(), mustPlatform(t, "linux"), "", onPath, runner, DefaultDeliveredBound)

	// Act
	_, err := b.Post(context.Background(), target, Banner{Title: "T", Body: "B"}, func() {})

	// Assert
	if err != nil {
		t.Fatalf("Post: %v", err)
	}
	want := []string{"--app-name=Emacs", "--hint=boolean:suppress-sound:true", "--action=default=Open", "--wait", "--expire-time=60000", "T", "B"}
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
		{name: "a macOS banner is handed whatever the program prints", goos: "darwin", stdout: version3, want: Handed},
		{name: "a macOS banner is handed when the program prints nothing", goos: "darwin", stdout: "", want: Handed},
		{name: "notify-send action", goos: "linux", stdout: "default\n", want: Clicked},
		{name: "notify-send dismissed", goos: "linux", stdout: "", want: Unclicked},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			runner := &fakeRunner{stdout: version3}
			b, err := NewBackend(context.Background(), mustPlatform(t, tc.goos), "", onPath, runner, DefaultDeliveredBound)
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
	runner := &fakeRunner{stdout: version3}
	b, err := NewBackend(context.Background(), mustPlatform(t, "darwin"), "", onPath, runner, DefaultDeliveredBound)
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

// TestTerminalNotifierBackendListsTheBannersItStillHolds pins the macOS
// delivered query: terminal-notifier -list ALL, read as the workspaces whose
// group Notification Center still lists.
func TestTerminalNotifierBackendListsTheBannersItStillHolds(t *testing.T) {
	cases := []struct {
		name   string
		stdout string
		want   map[ids.WorkspaceID]bool
	}{
		{name: "a list naming no banner is empty output", stdout: "", want: map[ids.WorkspaceID]bool{}},
		{name: "the header row alone names no banner", stdout: "GroupID\tTitle\tSubtitle\tMessage\tDelivered At\n",
			want: map[ids.WorkspaceID]bool{}},
		{name: "each workspace group is a delivered banner",
			stdout: "GroupID\tTitle\tSubtitle\tMessage\tDelivered At\n" +
				"agent-repl:ws1\tT1\t\tB1\t2026-10-15 16:16:08 +0000\n" +
				"agent-repl:ws2\tT2\t\tB2\t2026-10-15 16:17:08 +0000\n",
			want: map[ids.WorkspaceID]bool{"ws1": true, "ws2": true}},
		{name: "a group that is no workspace's is skipped",
			stdout: "GroupID\tTitle\tSubtitle\tMessage\tDelivered At\n" +
				"agent-repl-docker-fd\tT\t\tB\t2026-10-15 05:21:33 +0000\n" +
				"agent-repl:\tT\t\tB\t2026-10-15 05:21:33 +0000\n" +
				"other-app\tT\t\tB\t2026-10-15 05:21:33 +0000\n" +
				"agent-repl:ws3\tT\t\tB\t2026-10-15 05:21:33 +0000",
			want: map[ids.WorkspaceID]bool{"ws3": true}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			runner := &fakeRunner{stdout: version3}
			b, err := NewBackend(context.Background(), mustPlatform(t, "darwin"), "", onPath, runner, DefaultDeliveredBound)
			if err != nil {
				t.Fatalf("NewBackend: %v", err)
			}
			runner.stdout = tc.stdout

			// Act
			got, err := b.Delivered(context.Background())

			// Assert
			if err != nil {
				t.Fatalf("Delivered: %v", err)
			}
			if !reflect.DeepEqual(runner.args, []string{"-list", "ALL"}) {
				t.Fatalf("ran %q, want -list ALL", runner.args)
			}
			if !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("Delivered = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestBackendDeliveredSurfacesAFailedProgram(t *testing.T) {
	// Arrange
	cause := errors.New("exit status 1: list refused")
	runner := &fakeRunner{stdout: version3}
	b, err := NewBackend(context.Background(), mustPlatform(t, "darwin"), "", onPath, runner, DefaultDeliveredBound)
	if err != nil {
		t.Fatalf("NewBackend: %v", err)
	}
	runner.err = cause

	// Act
	got, err := b.Delivered(context.Background())

	// Assert
	if !errors.Is(err, cause) || got != nil {
		t.Fatalf("Delivered = (%v, %v), want (nil, the program's failure)", got, err)
	}
}

func TestBackendDeliveredIsBoundedByTheDeliveredBound(t *testing.T) {
	// Arrange
	runner := &fakeRunner{stdout: version3}
	b, err := NewBackend(context.Background(), mustPlatform(t, "darwin"), "", onPath, runner, DefaultDeliveredBound)
	if err != nil {
		t.Fatalf("NewBackend: %v", err)
	}
	runner.block = make(chan struct{})

	// Act
	_, err = b.Delivered(context.Background())

	// Assert
	if !errors.Is(err, context.DeadlineExceeded) {
		t.Fatalf("Delivered = %v, want the bound's deadline", err)
	}
}

func TestBackendDeliveredIsBoundedByTheBoundItWasBuiltWith(t *testing.T) {
	// Arrange: a bound no default shares, so only the one handed in can match.
	const bound = 7 * time.Second
	runner := &fakeRunner{stdout: version3}
	b, err := NewBackend(context.Background(), mustPlatform(t, "darwin"), "", onPath, runner, bound)
	if err != nil {
		t.Fatalf("NewBackend: %v", err)
	}
	runner.stdout = ""
	before := time.Now()

	// Act
	if _, err := b.Delivered(context.Background()); err != nil {
		t.Fatalf("Delivered: %v", err)
	}
	after := time.Now()

	// Assert: the deadline was set inside the call, the bound out from then.
	if runner.deadline.Before(before.Add(bound)) || runner.deadline.After(after.Add(bound)) {
		t.Fatalf("Delivered ran with a deadline %s out, want the %s it was built with", runner.deadline.Sub(before), bound)
	}
}

func TestNewBackendPanicsOnANonPositiveDeliveredBound(t *testing.T) {
	// Arrange
	defer func() {
		// Assert
		got, _ := recover().(string)
		if !strings.Contains(got, "delivered bound of 0s") {
			t.Fatalf("panic = %q, want it to name the zero bound", got)
		}
	}()

	// Act
	_, _ = NewBackend(context.Background(), mustPlatform(t, "darwin"), "", onPath, &fakeRunner{stdout: version3}, 0)
	t.Fatal("NewBackend returned for a zero delivered bound, want a panic")
}

func TestReadBackBackendIsNeverAskedWhatIsDelivered(t *testing.T) {
	// Arrange
	b, err := NewBackend(context.Background(), mustPlatform(t, "linux"), "", onPath, &fakeRunner{}, DefaultDeliveredBound)
	if err != nil {
		t.Fatalf("NewBackend: %v", err)
	}
	defer func() {
		// Assert
		if recover() == nil {
			t.Fatal("Delivered on a read-back backend did not panic")
		}
	}()

	// Act
	_, _ = b.Delivered(context.Background())
}

// TestBackendReportsTheBannerMadeExactlyWhenItIsMade pins the made edge the
// chime rings from: a handed banner is made once its program exits cleanly, a
// read-back banner once its program is up showing it, and a banner whose
// program failed or never started is never made.
func TestBackendReportsTheBannerMadeExactlyWhenItIsMade(t *testing.T) {
	cases := []struct {
		name      string
		goos      string
		err       error
		startErr  error
		wantMade  bool
		wantAfter bool
	}{
		{name: "a handed banner is made once its program exits", goos: "darwin", wantMade: true, wantAfter: true},
		{name: "a handed banner whose program failed is not made", goos: "darwin", err: errors.New("exit status 1")},
		{name: "a read-back banner is made once its program is up", goos: "linux", wantMade: true, wantAfter: false},
		{name: "a read-back banner whose program never started is not made", goos: "linux", startErr: errors.New("exec: no such file")},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			runner := &fakeRunner{stdout: version3}
			b, err := NewBackend(context.Background(), mustPlatform(t, tc.goos), "", onPath, runner, DefaultDeliveredBound)
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

func TestFirstNumber(t *testing.T) {
	cases := []struct {
		in     string
		want   int
		wantOK bool
	}{
		{in: "terminal-notifier 3.1.0.", want: 3, wantOK: true},
		{in: "12", want: 12, wantOK: true},
		{in: "v10.2", want: 10, wantOK: true},
		{in: "no digits", wantOK: false},
		{in: "", wantOK: false},
	}
	for _, tc := range cases {
		t.Run(tc.in, func(t *testing.T) {
			// Act
			got, ok := firstNumber(tc.in)

			// Assert
			if got != tc.want || ok != tc.wantOK {
				t.Fatalf("firstNumber(%q) = (%d, %v), want (%d, %v)", tc.in, got, ok, tc.want, tc.wantOK)
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
	_, bannerErr := NewBackend(context.Background(), mustPlatform(t, "darwin"), "", noLookPath, &fakeRunner{}, DefaultDeliveredBound)
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

// TestEveryProgramRunGoesThroughRunWithin pins that no backend or chime
// bounds a run of its own: the one bounded run is runWithin.
func TestEveryProgramRunGoesThroughRunWithin(t *testing.T) {
	// Arrange
	source, err := os.ReadFile("backend.go")
	if err != nil {
		t.Fatalf("read backend.go: %v", err)
	}

	// Act
	timeouts := strings.Count(string(source), "context.WithTimeout(")
	runs := strings.Count(string(source), "runner.Run(")

	// Assert
	if timeouts != 1 || runs != 1 {
		t.Fatalf("backend.go bounds %d runs and calls runner.Run %d times, want both only in runWithin", timeouts, runs)
	}
}
