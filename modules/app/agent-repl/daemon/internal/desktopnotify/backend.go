package desktopnotify

import (
	"bytes"
	"context"
	"fmt"
	"os/exec"
	"strconv"
	"strings"
	"time"

	"claude-repld/internal/ids"
)

// EnvNotifierCmd overrides the banner program's binary. The platform's argv
// is unchanged — only the program it is handed to — so a test points it at a
// script that records the argv and answers a click or a dismissal, and an
// operator can point it at a build of the program outside PATH.
const EnvNotifierCmd = "AGENT_REPL_NOTIFIER_CMD"

// ClickTimeout is how long a banner stays clickable before the program
// dismisses it itself. The program blocks until a click, a dismissal or this
// timeout, which is what lets the daemon read the click back.
const ClickTimeout = 60 * time.Second

// postBound bounds one banner program's whole run: its own click timeout plus
// the program's start and exit. A run past it is a hung program.
const postBound = ClickTimeout + 10*time.Second

// EmacsBundleID is the bundle the macOS banner is attributed to. macOS
// foregrounds a banner's originating application when it is clicked, so
// attributing it to Emacs brings Emacs forward as Emacs selects the tab.
const EmacsBundleID = "org.gnu.Emacs"

// Banner is one desktop notification.
type Banner struct {
	// Title is the banner's first line.
	Title string
	// Body is the banner's text below the title.
	Body string
	// Silent posts the banner without the platform's sound. A turn end rings
	// its own chime (Notifier.Ring) whether or not its banner is posted, so
	// its banner is silent and the turn sounds exactly once.
	Silent bool
}

// Runner executes one banner program and answers its standard output. It is
// the package's one exec seam, so every backend is tested with no process.
type Runner interface {
	Run(ctx context.Context, bin string, args []string) (string, error)
}

// ExecRunner is the production Runner.
type ExecRunner struct{}

// Run executes bin with args and answers its standard output. A non-zero exit
// is an error carrying the program's standard error.
func (ExecRunner) Run(ctx context.Context, bin string, args []string) (string, error) {
	cmd := exec.CommandContext(ctx, bin, args...)
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		return stdout.String(), fmt.Errorf("%s: %w: %s", bin, err, strings.TrimSpace(stderr.String()))
	}
	return stdout.String(), nil
}

// Backend posts one banner and blocks until it is clicked, dismissed or timed
// out, reporting whether it was clicked.
type Backend interface {
	// Program names the banner program, for the record.
	Program() string
	// Post shows the banner for workspace ws.
	Post(ctx context.Context, ws ids.WorkspaceID, banner Banner) (clicked bool, err error)
}

// Platform names a platform's banner program and how it is spoken to.
type Platform struct {
	// Program is the program's default binary name, looked up on PATH.
	Program string
	// args composes the argv for one banner.
	args func(ws ids.WorkspaceID, banner Banner) []string
	// clicked reads the program's standard output for a click.
	clicked func(stdout string) bool
}

// PlatformFor answers the banner program for a GOOS: alerter on macOS (it
// posts through UNUserNotificationCenter and reports a click on stdout), and
// notify-send on Linux (libnotify, whose --wait prints the invoked action).
func PlatformFor(goos string) (Platform, error) {
	switch goos {
	case "darwin":
		return Platform{Program: "alerter", args: alerterArgs, clicked: alerterClicked}, nil
	case "linux":
		return Platform{Program: "notify-send", args: notifySendArgs, clicked: notifySendClicked}, nil
	default:
		return Platform{}, fmt.Errorf("desktop notifications are not supported on %s", goos)
	}
}

// alerterArgs is alerter's argv. Its flags are double-dash long options (the
// Swift ArgumentParser build refuses single-dash spellings and posts nothing),
// and the --group keyed to the workspace coalesces that workspace's banners.
func alerterArgs(ws ids.WorkspaceID, banner Banner) []string {
	args := []string{
		"--title", banner.Title,
		"--message", banner.Body,
	}
	if !banner.Silent {
		args = append(args, "--sound", "default")
	}
	return append(args,
		"--sender", EmacsBundleID,
		"--timeout", strconv.Itoa(int(ClickTimeout/time.Second)),
		"--group", "agent-repl:"+string(ws),
	)
}

// alerterClicked reads alerter's activation token: @CONTENTCLICKED for the
// banner itself, @ACTIONCLICKED for an action; @TIMEOUT and @CLOSED are not
// clicks.
func alerterClicked(stdout string) bool {
	token := strings.TrimSpace(stdout)
	return strings.HasPrefix(token, "@CONTENTCLICKED") || strings.HasPrefix(token, "@ACTIONCLICKED")
}

// notifySendAction is the action a Linux banner click invokes.
const notifySendAction = "default"

// notifySendArgs is notify-send's argv. The `default` action is what a click
// on the banner body invokes, and --wait keeps the program alive to print it.
func notifySendArgs(_ ids.WorkspaceID, banner Banner) []string {
	return []string{
		"--app-name=Emacs",
		"--action=" + notifySendAction + "=Open",
		"--wait",
		"--expire-time=" + strconv.Itoa(int(ClickTimeout/time.Millisecond)),
		banner.Title,
		banner.Body,
	}
}

// notifySendClicked reads notify-send's invoked action.
func notifySendClicked(stdout string) bool {
	return strings.TrimSpace(stdout) == notifySendAction
}

// programBackend is a Backend over one platform's banner program.
type programBackend struct {
	bin      string
	platform Platform
	runner   Runner
}

// NewBackend resolves the platform's banner program: $AGENT_REPL_NOTIFIER_CMD
// when set (override), else the platform's program on PATH. An error names a
// platform with no banner program or a program that is not installed.
func NewBackend(platform Platform, override string, lookPath func(string) (string, error), runner Runner) (Backend, error) {
	bin, err := resolveProgram("banner", platform.Program, override, lookPath)
	if err != nil {
		return nil, err
	}
	return &programBackend{bin: bin, platform: platform, runner: runner}, nil
}

// resolveProgram answers the binary one of the notifier's programs runs:
// override when set, else program on PATH. role names the program in the
// error a program that is not installed answers.
func resolveProgram(role, program, override string, lookPath func(string) (string, error)) (string, error) {
	if override != "" {
		return override, nil
	}
	found, err := lookPath(program)
	if err != nil {
		return "", fmt.Errorf("the %s program %q is not installed: %w", role, program, err)
	}
	return found, nil
}

// Program implements Backend.
func (b *programBackend) Program() string { return b.bin }

// Post implements Backend.
func (b *programBackend) Post(ctx context.Context, ws ids.WorkspaceID, banner Banner) (bool, error) {
	ctx, cancel := context.WithTimeout(ctx, postBound)
	defer cancel()
	stdout, err := b.runner.Run(ctx, b.bin, b.platform.args(ws, banner))
	if err != nil {
		return false, err
	}
	return b.platform.clicked(stdout), nil
}

// EnvChimeCmd overrides the chime program's binary, exactly as EnvNotifierCmd
// does the banner program's: the platform's argv is unchanged, so a test
// points it at a recorder and no test ever plays a real sound.
const EnvChimeCmd = "AGENT_REPL_CHIME_CMD"

// chimeBound bounds one chime program's run. The sound is a second or so; a
// run past this is a hung program.
const chimeBound = 10 * time.Second

// Chime plays the short sound a turn end rings.
type Chime interface {
	// Program names the chime program, for the record.
	Program() string
	// Play plays the sound once and returns when it has finished.
	Play(ctx context.Context) error
}

// ChimePlatform names a platform's sound player and the argv that plays the
// turn-end sound.
type ChimePlatform struct {
	// Program is the player's default binary name, looked up on PATH.
	Program string
	// Args is the player's argv for the sound.
	Args []string
}

// ChimePlatformFor answers the sound player for a GOOS: afplay with a system
// sound on macOS, and canberra-gtk-play with the freedesktop `complete` event
// on Linux.
func ChimePlatformFor(goos string) (ChimePlatform, error) {
	switch goos {
	case "darwin":
		return ChimePlatform{Program: "afplay", Args: []string{"/System/Library/Sounds/Glass.aiff"}}, nil
	case "linux":
		return ChimePlatform{Program: "canberra-gtk-play", Args: []string{"--id=complete"}}, nil
	default:
		return ChimePlatform{}, fmt.Errorf("a turn-end chime is not supported on %s", goos)
	}
}

// programChime is a Chime over one platform's sound player.
type programChime struct {
	bin      string
	platform ChimePlatform
	runner   Runner
}

// NewChime resolves the platform's sound player: $AGENT_REPL_CHIME_CMD when
// set (override), else the platform's player on PATH.
func NewChime(platform ChimePlatform, override string, lookPath func(string) (string, error), runner Runner) (Chime, error) {
	bin, err := resolveProgram("chime", platform.Program, override, lookPath)
	if err != nil {
		return nil, err
	}
	return &programChime{bin: bin, platform: platform, runner: runner}, nil
}

// Program implements Chime.
func (c *programChime) Program() string { return c.bin }

// Play implements Chime.
func (c *programChime) Play(ctx context.Context) error {
	ctx, cancel := context.WithTimeout(ctx, chimeBound)
	defer cancel()
	_, err := c.runner.Run(ctx, c.bin, c.platform.Args)
	return err
}
