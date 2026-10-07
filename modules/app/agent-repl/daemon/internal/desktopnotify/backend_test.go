package desktopnotify

import (
	"context"
	"errors"
	"reflect"
	"testing"

	"claude-repld/internal/ids"
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
		{goos: "darwin", want: "alerter"},
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

func TestPlatformForRefusesAnUnsupportedPlatform(t *testing.T) {
	// Act
	_, err := PlatformFor("windows")

	// Assert
	if err == nil {
		t.Fatal("an unsupported platform was given a banner program")
	}
}

func TestNewBackendTakesTheOverride(t *testing.T) {
	// Act
	b, err := NewBackend(mustPlatform(t, "darwin"), "/tmp/fake-notifier", noLookPath, &fakeRunner{})

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
	_, err := NewBackend(mustPlatform(t, "darwin"), "", noLookPath, &fakeRunner{})

	// Assert
	if err == nil {
		t.Fatal("a missing banner program was accepted")
	}
}

func TestAlerterBackendSpeaksAlertersArgv(t *testing.T) {
	// Arrange
	runner := &fakeRunner{stdout: "@TIMEOUT"}
	b, _ := NewBackend(mustPlatform(t, "darwin"), "", onPath, runner)

	// Act
	_, err := b.Post(context.Background(), ids.WorkspaceID("ws1"), Banner{Title: "T", Body: "B"})

	// Assert
	if err != nil {
		t.Fatalf("Post: %v", err)
	}
	want := []string{"--title", "T", "--message", "B", "--sound", "default", "--sender", EmacsBundleID,
		"--timeout", "60", "--group", "agent-repl:ws1"}
	if runner.bin != "/usr/bin/alerter" || !reflect.DeepEqual(runner.args, want) {
		t.Fatalf("ran %s %q, want /usr/bin/alerter %q", runner.bin, runner.args, want)
	}
}

func TestNotifySendBackendSpeaksNotifySendsArgv(t *testing.T) {
	// Arrange
	runner := &fakeRunner{}
	b, _ := NewBackend(mustPlatform(t, "linux"), "", onPath, runner)

	// Act
	_, err := b.Post(context.Background(), ids.WorkspaceID("ws1"), Banner{Title: "T", Body: "B"})

	// Assert
	if err != nil {
		t.Fatalf("Post: %v", err)
	}
	want := []string{"--app-name=Emacs", "--action=default=Open", "--wait", "--expire-time=60000", "T", "B"}
	if !reflect.DeepEqual(runner.args, want) {
		t.Fatalf("argv = %q, want %q", runner.args, want)
	}
}

func TestBackendReadsTheClick(t *testing.T) {
	cases := []struct {
		name   string
		goos   string
		stdout string
		want   bool
	}{
		{name: "alerter content click", goos: "darwin", stdout: "@CONTENTCLICKED\n", want: true},
		{name: "alerter action click", goos: "darwin", stdout: "@ACTIONCLICKED", want: true},
		{name: "alerter timeout", goos: "darwin", stdout: "@TIMEOUT", want: false},
		{name: "alerter closed", goos: "darwin", stdout: "@CLOSED", want: false},
		{name: "notify-send action", goos: "linux", stdout: "default\n", want: true},
		{name: "notify-send dismissed", goos: "linux", stdout: "", want: false},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			b, _ := NewBackend(mustPlatform(t, tc.goos), "", onPath, &fakeRunner{stdout: tc.stdout})

			// Act
			clicked, err := b.Post(context.Background(), ids.WorkspaceID("ws1"), Banner{Title: "T", Body: "B"})

			// Assert
			if err != nil {
				t.Fatalf("Post: %v", err)
			}
			if clicked != tc.want {
				t.Fatalf("clicked = %v, want %v", clicked, tc.want)
			}
		})
	}
}

func TestBackendSurfacesAFailedProgram(t *testing.T) {
	// Arrange
	cause := errors.New("exit status 64")
	b, _ := NewBackend(mustPlatform(t, "darwin"), "", onPath, &fakeRunner{err: cause})

	// Act
	clicked, err := b.Post(context.Background(), ids.WorkspaceID("ws1"), Banner{Title: "T", Body: "B"})

	// Assert
	if !errors.Is(err, cause) || clicked {
		t.Fatalf("Post = (%v, %v), want (false, the program's failure)", clicked, err)
	}
}

func TestAlerterBackendPostsASilentBannerWithoutTheSound(t *testing.T) {
	// Arrange
	runner := &fakeRunner{stdout: "@TIMEOUT"}
	b, _ := NewBackend(mustPlatform(t, "darwin"), "", onPath, runner)

	// Act
	_, err := b.Post(context.Background(), ids.WorkspaceID("ws1"), Banner{Title: "T", Body: "B", Silent: true})

	// Assert
	if err != nil {
		t.Fatalf("Post: %v", err)
	}
	want := []string{"--title", "T", "--message", "B", "--sender", EmacsBundleID,
		"--timeout", "60", "--group", "agent-repl:ws1"}
	if !reflect.DeepEqual(runner.args, want) {
		t.Fatalf("argv = %q, want %q", runner.args, want)
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
	_, bannerErr := NewBackend(mustPlatform(t, "darwin"), "", noLookPath, &fakeRunner{})
	_, chimeErr := NewChime(mustChimePlatform(t, "darwin"), "", noLookPath, &fakeRunner{})

	// Assert
	if bannerErr == nil || bannerErr.Error() != `the banner program "alerter" is not installed: not on PATH` {
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
