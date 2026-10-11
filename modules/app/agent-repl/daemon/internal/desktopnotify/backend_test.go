package desktopnotify

import (
	"context"
	"errors"
	"reflect"
	"testing"
)

// fakeRunner answers one scripted stdout and records the argv it was handed.
type fakeRunner struct {
	stdout string
	err    error
	bin    string
	args   []string
	// block, when set, is waited on before answering (a banner awaiting a
	// click), and ctx's end answers ctx.Err().
	block chan struct{}
}

func (r *fakeRunner) Run(ctx context.Context, bin string, args []string) (string, error) {
	r.bin, r.args = bin, args
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
	b, err := NewBackend(context.Background(), mustPlatform(t, "darwin"), "/tmp/fake-notifier", noLookPath, &fakeRunner{stdout: version3})

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
	b, err := NewBackend(context.Background(), mustPlatform(t, "linux"), "", onPath, &fakeRunner{})

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
	_, err := NewBackend(context.Background(), mustPlatform(t, "darwin"), "", noLookPath, &fakeRunner{stdout: version3})

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
			_, err := NewBackend(context.Background(), mustPlatform(t, "darwin"), "", onPath, runner)

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
	_, err := NewBackend(context.Background(), mustPlatform(t, "linux"), "", onPath, runner)

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
		{name: "a sounding banner", banner: Banner{Title: "T", Body: "B"}, want: []string{
			"-title", `\T`, "-message", `\B`, "-sound", "default",
			"-group", `\agent-repl:ws1`, "-activate", EmacsBundleID, "-execute", `\select ws1 in /work/ws1`}},
		{name: "a silent banner carries no sound", banner: Banner{Title: "T", Body: "B", Silent: true}, want: []string{
			"-title", `\T`, "-message", `\B`,
			"-group", `\agent-repl:ws1`, "-activate", EmacsBundleID, "-execute", `\select ws1 in /work/ws1`}},
		{name: "a value NSUserDefaults would parse stays text", banner: Banner{Title: "[ws] {x}", Body: `"quoted" (y)`, Silent: true}, want: []string{
			"-title", `\[ws] {x}`, "-message", `\"quoted" (y)`,
			"-group", `\agent-repl:ws1`, "-activate", EmacsBundleID, "-execute", `\select ws1 in /work/ws1`}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			runner := &fakeRunner{stdout: version3}
			b, err := NewBackend(context.Background(), mustPlatform(t, "darwin"), "", onPath, runner)
			if err != nil {
				t.Fatalf("NewBackend: %v", err)
			}

			// Act
			_, err = b.Post(context.Background(), target, tc.banner)

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
	b, _ := NewBackend(context.Background(), mustPlatform(t, "linux"), "", onPath, runner)

	// Act
	_, err := b.Post(context.Background(), target, Banner{Title: "T", Body: "B"})

	// Assert
	if err != nil {
		t.Fatalf("Post: %v", err)
	}
	want := []string{"--app-name=Emacs", "--action=default=Open", "--wait", "--expire-time=60000", "T", "B"}
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
			b, err := NewBackend(context.Background(), mustPlatform(t, tc.goos), "", onPath, runner)
			if err != nil {
				t.Fatalf("NewBackend: %v", err)
			}
			runner.stdout = tc.stdout

			// Act
			outcome, err := b.Post(context.Background(), target, Banner{Title: "T", Body: "B"})

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
	b, err := NewBackend(context.Background(), mustPlatform(t, "darwin"), "", onPath, runner)
	if err != nil {
		t.Fatalf("NewBackend: %v", err)
	}
	runner.err = cause

	// Act
	outcome, err := b.Post(context.Background(), target, Banner{Title: "T", Body: "B"})

	// Assert
	if !errors.Is(err, cause) || outcome != Unclicked {
		t.Fatalf("Post = (%v, %v), want (Unclicked, the program's failure)", outcome, err)
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
	_, bannerErr := NewBackend(context.Background(), mustPlatform(t, "darwin"), "", noLookPath, &fakeRunner{})
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
