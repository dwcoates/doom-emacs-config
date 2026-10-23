package desktopnotify

import (
	"context"
	"errors"
	"fmt"
	"strconv"
	"strings"
	"time"

	"agent-repld/internal/childproc"
	"agent-repld/internal/ids"
)

// EnvNotifierCmd overrides the banner program's binary. The platform's argv
// is unchanged — only the program it is handed to — so a test points it at a
// script that records the argv and answers a click or a dismissal, and an
// operator can point it at a build of the program outside PATH.
const EnvNotifierCmd = "AGENT_REPL_NOTIFIER_CMD"

// ClickTimeout is how long a banner that is READ BACK stays clickable before
// the program dismisses it itself: such a program (notify-send) blocks until a
// click, a dismissal or this timeout, which is what lets the daemon read the
// click back. A banner HANDED to the platform (terminal-notifier) has no such
// wait: macOS keeps it, and its click runs the action stored with it.
const ClickTimeout = 60 * time.Second

// postBound bounds one banner program's whole run: its own click timeout plus
// the program's start and exit. A run past it is a hung program.
const postBound = ClickTimeout + 10*time.Second

// versionBound bounds the version query a platform's minimum version is
// checked with at boot.
const versionBound = 10 * time.Second

// EmacsBundleID is the application a macOS banner's click brings forward. The
// banner program activates it from the click itself -- the one moment macOS
// lets an application hand activation to another -- so Emacs comes forward
// while the click's SelectWorkspace moves its frame to the workspace.
const EmacsBundleID = "org.gnu.Emacs"

// Banner is one desktop notification.
type Banner struct {
	// Title is the banner's first line.
	Title string
	// Body is the banner's text below the title.
	Body string
}

// Target is the workspace a banner is posted for: the id its group and its
// click name, and the directory the click's WorkspaceRef echoes.
type Target struct {
	ID  ids.WorkspaceID
	Dir string
}

// ClickAction answers the sh command a HANDED banner's click runs for its
// target. It is the daemon's own `call SelectWorkspace`, so the click reaches
// whichever daemon serves when it lands, however long after the post that is.
type ClickAction func(target Target) string

// Outcome is what posting one banner came to.
type Outcome int

const (
	// Unclicked is a read-back banner that was dismissed or timed out.
	Unclicked Outcome = iota
	// Clicked is a read-back banner that was clicked; the daemon relays it.
	Clicked
	// Handed is a banner given to the platform with its click action stored
	// on it: the click selects the workspace by itself, and nothing is read
	// back.
	Handed
)

// Runner executes one program and answers its standard output. It is the
// package's one exec seam, so every backend is tested with no process.
type Runner interface {
	// Run executes bin with args. started, when set, is called once the
	// process is running, before Run waits for it to exit.
	Run(ctx context.Context, bin string, args []string, started func()) (string, error)
}

// runWithin runs one program through runner under bound: EVERY run of a
// notifier program is bounded, so a hung program costs its bound and never
// holds a banner's goroutine forever.
func runWithin(ctx context.Context, bound time.Duration, runner Runner, bin string, args []string, started func()) (string, error) {
	ctx, cancel := context.WithTimeout(ctx, bound)
	defer cancel()
	return runner.Run(ctx, bin, args, started)
}

// ExecRunner is the production Runner.
type ExecRunner struct{}

// Run executes bin with args and answers its standard output. A non-zero exit
// is an error carrying the program's standard error.
func (ExecRunner) Run(ctx context.Context, bin string, args []string, started func()) (string, error) {
	cmd := childproc.CommandContext(ctx, childproc.Stable, bin, args...)
	capture, err := childproc.CaptureOutput(cmd)
	if err != nil {
		return "", fmt.Errorf("%s: %w", bin, err)
	}
	if err := cmd.Start(); err != nil {
		return "", errors.Join(fmt.Errorf("%s: %w", bin, err), capture.Close())
	}
	if started != nil {
		started()
	}
	waitErr := cmd.Wait()
	stdout, stderr, err := capture.Finish()
	if err != nil {
		return "", fmt.Errorf("%s: %w", bin, errors.Join(waitErr, err))
	}
	if waitErr != nil {
		return stdout, fmt.Errorf("%s: %w: %s", bin, waitErr, strings.TrimSpace(stderr))
	}
	return stdout, nil
}

// Backend posts one banner.
type Backend interface {
	// Program names the banner program, for the record.
	Program() string
	// Post shows the banner for target, answering what it came to: a
	// read-back program blocks until the banner is clicked, dismissed or timed
	// out; a handed one returns once the platform holds it.
	//
	// MADE IS CALLED EXACTLY WHEN THE BANNER IS MADE, and never for a banner
	// that was not: once the platform holds a handed banner (the program
	// exited cleanly), or once a read-back program is up showing its banner.
	// It is the one edge the notifier rings the banner's chime from.
	Post(ctx context.Context, target Target, banner Banner, made func()) (Outcome, error)
}

// Platform names a platform's banner program and how it is spoken to.
type Platform struct {
	// Program is the program's default binary name, looked up on PATH.
	Program string
	// args composes the argv for one banner.
	args func(target Target, banner Banner) []string
	// outcome reads the program's standard output.
	outcome func(stdout string) Outcome
	// readBack is a program that blocks, showing its banner, until the banner
	// is clicked, dismissed or timed out: its banner is made once it is
	// running. Any other program's banner is made once it exits cleanly.
	readBack bool
	// minMajor, when non-zero, is the lowest major version the program may be:
	// versionArgs asks it, and the first number in its answer is the major.
	minMajor    int
	versionArgs []string
}

// PlatformFor answers the banner program for a GOOS: terminal-notifier on
// macOS (version 3 or later, the UserNotifications rebuild: an older one posts
// through an API macOS 26 drops without a word), whose banner is HANDED to the
// platform with click's command stored on it; and notify-send on Linux
// (libnotify, whose --wait prints the invoked action), READ BACK.
func PlatformFor(goos string, click ClickAction) (Platform, error) {
	switch goos {
	case "darwin":
		if click == nil {
			panic("desktopnotify: the macOS banner needs its click action")
		}
		return Platform{
			Program:     "terminal-notifier",
			args:        func(target Target, banner Banner) []string { return terminalNotifierArgs(target, banner, click) },
			outcome:     func(string) Outcome { return Handed },
			minMajor:    3,
			versionArgs: []string{"-version"},
		}, nil
	case "linux":
		return Platform{Program: "notify-send", args: notifySendArgs, outcome: notifySendOutcome, readBack: true}, nil
	default:
		return Platform{}, fmt.Errorf("desktop notifications are not supported on %s", goos)
	}
}

// terminalNotifierArgs is terminal-notifier's argv. The -group keyed to the
// workspace replaces that workspace's previous banner; -activate brings Emacs
// forward on the click and -execute runs the click's SelectWorkspace. No
// -sound is passed: the banner's one sound is the notifier's chime, rung the
// instant the banner is made.
//
// EVERY FREE VALUE IS BACKSLASH-PREFIXED. terminal-notifier reads its options
// through NSUserDefaults, which parses a value as a property list first: one
// starting with `[`, `(`, `{` or a quote is misread and the banner is refused.
// The program strips one leading backslash from every value, so prefixing each
// one makes every value a plain string whatever it starts with.
func terminalNotifierArgs(target Target, banner Banner, click ClickAction) []string {
	return []string{
		"-title", literal(banner.Title),
		"-message", literal(banner.Body),
		"-group", literal(bannerGroup(target.ID)),
		"-activate", EmacsBundleID,
		"-execute", literal(click(target)),
	}
}

// literal escapes one terminal-notifier value so it is read as plain text.
func literal(value string) string { return `\` + value }

// bannerGroupPrefix opens every group a workspace's banners share.
const bannerGroupPrefix = "agent-repl:"

// bannerGroup is the group a workspace's banners share.
func bannerGroup(ws ids.WorkspaceID) string { return bannerGroupPrefix + string(ws) }

// notifySendAction is the action a Linux banner click invokes.
const notifySendAction = "default"

// notifySendArgs is notify-send's argv. The `default` action is what a click
// on the banner body invokes, and --wait keeps the program alive to print it.
// The suppress-sound hint keeps the notification server silent: the banner's
// one sound is the notifier's chime.
func notifySendArgs(_ Target, banner Banner) []string {
	return []string{
		"--app-name=Emacs",
		"--hint=boolean:suppress-sound:true",
		"--action=" + notifySendAction + "=Open",
		"--wait",
		"--expire-time=" + strconv.Itoa(int(ClickTimeout/time.Millisecond)),
		banner.Title,
		banner.Body,
	}
}

// notifySendOutcome reads notify-send's invoked action.
func notifySendOutcome(stdout string) Outcome {
	if strings.TrimSpace(stdout) == notifySendAction {
		return Clicked
	}
	return Unclicked
}

// programBackend is a Backend over one platform's banner program.
type programBackend struct {
	bin      string
	platform Platform
	runner   Runner
}

// NewBackend resolves the platform's banner program: $AGENT_REPL_NOTIFIER_CMD
// when set (override), else the platform's program on PATH. An error names a
// platform with no banner program, a program that is not installed, or one
// older than the platform's minimum version.
func NewBackend(ctx context.Context, platform Platform, override string, lookPath func(string) (string, error), runner Runner) (Backend, error) {
	bin, err := resolveProgram("banner", platform.Program, override, lookPath)
	if err != nil {
		return nil, err
	}
	if platform.minMajor > 0 {
		if err := checkMajor(ctx, runner, bin, platform); err != nil {
			return nil, err
		}
	}
	return &programBackend{bin: bin, platform: platform, runner: runner}, nil
}

// checkMajor refuses a program older than the platform's minimum major
// version, or one whose version cannot be read.
func checkMajor(ctx context.Context, runner Runner, bin string, platform Platform) error {
	out, err := runWithin(ctx, versionBound, runner, bin, platform.versionArgs, nil)
	if err != nil {
		return fmt.Errorf("ask the banner program %s its version: %w", bin, err)
	}
	major, ok := firstNumber(out)
	if !ok {
		return fmt.Errorf("the banner program %s answered no version: %q", bin, strings.TrimSpace(out))
	}
	if major < platform.minMajor {
		return fmt.Errorf("the banner program %s is version %d, older than the %d this platform needs: %q",
			bin, major, platform.minMajor, strings.TrimSpace(out))
	}
	return nil
}

// firstNumber answers the first run of digits in s.
func firstNumber(s string) (int, bool) {
	start := strings.IndexAny(s, "0123456789")
	if start < 0 {
		return 0, false
	}
	end := start
	for end < len(s) && s[end] >= '0' && s[end] <= '9' {
		end++
	}
	n, err := strconv.Atoi(s[start:end])
	return n, err == nil
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
func (b *programBackend) Post(ctx context.Context, target Target, banner Banner, made func()) (Outcome, error) {
	var started func()
	if b.platform.readBack {
		started = made
	}
	stdout, err := runWithin(ctx, postBound, b.runner, b.bin, b.platform.args(target, banner), started)
	if err != nil {
		return Unclicked, err
	}
	if !b.platform.readBack {
		made()
	}
	return b.platform.outcome(stdout), nil
}

// EnvChimeCmd overrides the chime program's binary, exactly as EnvNotifierCmd
// does the banner program's: the platform's argv is unchanged, so a test
// points it at a recorder and no test ever plays a real sound.
const EnvChimeCmd = "AGENT_REPL_CHIME_CMD"

// chimeBound bounds one chime program's run. The sound is a second or so; a
// run past this is a hung program.
const chimeBound = 10 * time.Second

// Chime plays the short sound every banner rings the instant it is made.
type Chime interface {
	// Program names the chime program, for the record.
	Program() string
	// Play plays the sound once and returns when it has finished.
	Play(ctx context.Context) error
}

// ChimePlatform names a platform's sound player and the argv that plays the
// banner's sound.
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
		return ChimePlatform{}, fmt.Errorf("a banner chime is not supported on %s", goos)
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
	_, err := runWithin(ctx, chimeBound, c.runner, c.bin, c.platform.Args, nil)
	return err
}
