package externalbrowser_test

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"syscall"
	"testing"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/externalbrowser"
	"claude-repld/internal/usersetup"
)

func TestValidate(t *testing.T) {
	tests := []struct {
		name    string
		url     string
		wantErr bool
	}{
		{name: "https", url: "https://example.com/a?b=c#d"},
		{name: "http", url: "http://example.com"},
		{name: "empty", url: "", wantErr: true},
		{name: "file scheme", url: "file:///etc/passwd", wantErr: true},
		{name: "javascript scheme", url: "javascript:alert(1)", wantErr: true},
		{name: "no scheme", url: "example.com", wantErr: true},
		{name: "embedded space", url: "https://example.com/a b", wantErr: true},
		{name: "embedded newline", url: "https://example.com/a\nb", wantErr: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			err := externalbrowser.Validate(tc.url)

			// Assert.
			if (err != nil) != tc.wantErr {
				t.Fatalf("Validate(%q) = %v, wantErr %v", tc.url, err, tc.wantErr)
			}
		})
	}
}

func TestLaunchArgv(t *testing.T) {
	tests := []struct {
		name    string
		profile string
		want    []string
	}{
		{name: "a profile rides as the profile flag", profile: "Profile 9", want: []string{"--profile-directory=Profile 9", "https://example.com"}},
		{name: "no profile passes no profile flag", profile: "", want: []string{"https://example.com"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := externalbrowser.LaunchArgv(tc.profile, "https://example.com")

			// Assert.
			if !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("LaunchArgv() = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestActivateArgvNamesTheApp(t *testing.T) {
	// Arrange, Act.
	got := externalbrowser.ActivateArgv("Google Chrome")

	// Assert.
	if len(got) != 2 || got[0] != "-e" || !strings.Contains(got[1], `"Google Chrome"`) {
		t.Fatalf("ActivateArgv() = %v, want an osascript activation naming the app", got)
	}
}

func TestOpenInvokesTheLauncherWithTheURL(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	record := filepath.Join(dir, "argv")
	script := writeScript(t, dir, "launcher", `printf '%s\n' "$@" > `+shellQuote(record)+`
exit 0`)
	o := newOpener(t, externalbrowser.Config{LauncherCmd: script})

	// Act.
	err := o.Open(context.Background(), "https://example.com/page", "")

	// Assert.
	if err != nil {
		t.Fatalf("Open() = %v, want nil", err)
	}
	if got := readFile(t, record); got != "https://example.com/page\n" {
		t.Fatalf("launcher argv = %q, want the url alone", got)
	}
}

func TestOpenSurfacesANonZeroLauncherExit(t *testing.T) {
	// Arrange: a link the user clicked that silently went nowhere is worse
	// than a loud failure.
	dir := t.TempDir()
	script := writeScript(t, dir, "failing", "exit 3")
	o := newOpener(t, externalbrowser.Config{LauncherCmd: script})

	// Act.
	err := o.Open(context.Background(), "https://example.com", "")

	// Assert.
	if err == nil {
		t.Fatal("Open() = nil error, want the launcher's non-zero exit surfaced")
	}
}

func TestOpenSurfacesAMissingLauncher(t *testing.T) {
	// Arrange.
	o := newOpener(t, externalbrowser.Config{LauncherCmd: filepath.Join(t.TempDir(), "not-installed")})

	// Act.
	err := o.Open(context.Background(), "https://example.com", "")

	// Assert.
	if err == nil {
		t.Fatal("Open() = nil error, want the missing launcher surfaced")
	}
}

func TestOpenSucceedsWhenTheLauncherBecomesTheBrowser(t *testing.T) {
	// Arrange: a cold browser start runs the browser IN the invoked process,
	// which never exits; the opener must not block on it.
	dir := t.TempDir()
	pidFile := filepath.Join(dir, "pid")
	script := writeScript(t, dir, "becomes-browser", `echo $$ > `+shellQuote(pidFile)+`
exec tail -f /dev/null`)
	t.Cleanup(func() { killRecordedPID(t, pidFile) })
	o := newOpener(t, externalbrowser.Config{LauncherCmd: script, LaunchWindow: 20 * time.Millisecond})

	// Act.
	err := o.Open(context.Background(), "https://example.com", "")

	// Assert.
	if err != nil {
		t.Fatalf("Open() = %v, want nil (a still-running launcher became the browser)", err)
	}
}

func TestOpenRefusesAnInvalidURLWithoutLaunching(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	marker := filepath.Join(dir, "ran")
	script := writeScript(t, dir, "launcher", "touch "+shellQuote(marker))
	o := newOpener(t, externalbrowser.Config{LauncherCmd: script})

	// Act.
	err := o.Open(context.Background(), "file:///etc/passwd", "")

	// Assert.
	if err == nil {
		t.Fatal("Open() = nil error, want the url refused")
	}
	if _, statErr := os.Stat(marker); statErr == nil {
		t.Fatal("the launcher ran for a refused url")
	}
}

func TestOpenUsesTheEnvironmentLauncherWhenTheConfigNamesNone(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	record := filepath.Join(dir, "argv")
	script := writeScript(t, dir, "env-launcher", `printf '%s\n' "$@" > `+shellQuote(record))
	t.Setenv(externalbrowser.EnvBrowserCmd, script)
	o := newOpener(t, externalbrowser.Config{})

	// Act.
	err := o.Open(context.Background(), "https://example.com/env", "")

	// Assert.
	if err != nil {
		t.Fatalf("Open() = %v, want nil", err)
	}
	if got := readFile(t, record); got != "https://example.com/env\n" {
		t.Fatalf("launcher argv = %q, want the url alone", got)
	}
}

func TestOpenHonorsACancelledContext(t *testing.T) {
	// Arrange.
	o := newOpener(t, externalbrowser.Config{LauncherCmd: "/bin/true"})
	ctx, cancel := context.WithCancel(context.Background())
	cancel()

	// Act.
	err := o.Open(ctx, "https://example.com", "")

	// Assert.
	if err == nil {
		t.Fatal("Open() = nil error, want the context's error")
	}
}

// newOpener builds an opener over cfg with a capturing logger.
func newOpener(t *testing.T, cfg externalbrowser.Config) externalbrowser.Opener {
	t.Helper()
	o, _ := newOpenerLogged(t, cfg)
	return o
}

// newOpenerLogged builds an opener over cfg and hands back the capturing logger
// too, for the branches whose whole point is the record they leave.
func newOpenerLogged(t *testing.T, cfg externalbrowser.Config) (externalbrowser.Opener, *dlog.TestLogger) {
	t.Helper()
	logger := dlog.NewTestLogger()
	cfg.Logger = logger
	o, err := externalbrowser.New(cfg)
	if err != nil {
		t.Fatalf("externalbrowser.New() = %v, want nil", err)
	}
	return o, logger
}

// writeLocalState plants a Chrome Local State document and answers its path.
func writeLocalState(t *testing.T, dir, body string) string {
	t.Helper()
	path := filepath.Join(dir, "Local State")
	if err := os.WriteFile(path, []byte(body), 0o600); err != nil {
		t.Fatalf("WriteFile() = %v", err)
	}
	return path
}

// recordingLauncher plants a raise that succeeds and a launcher that records
// its argv, and answers the opener config driving them plus the argv file.
func recordingLauncher(t *testing.T, localState string) (externalbrowser.Config, string) {
	t.Helper()
	dir := t.TempDir()
	record := filepath.Join(dir, "argv")
	activate := writeScript(t, dir, "activate", "exit 0")
	launch := writeScript(t, dir, "launch", `printf '%s\n' "$@" > `+shellQuote(record))
	return externalbrowser.Config{ActivateBin: activate, DefaultLauncherBin: launch, LocalStatePath: localState}, record
}

// TestOpenDefaultRoutesTheAccountToItsChromeProfile pins that the account's
// own profile, read from Chrome's Local State, is the one in Chrome's argv.
func TestOpenDefaultRoutesTheAccountToItsChromeProfile(t *testing.T) {
	tests := []struct {
		name        string
		email       string
		wantProfile string
	}{
		{name: "personal account", email: "personal@example.com", wantProfile: "Default"},
		{name: "work account", email: "work@example.com", wantProfile: "Profile 1"},
		{name: "email case is ignored", email: "Work@Example.com", wantProfile: "Profile 1"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			cfg, record := recordingLauncher(t, writeLocalState(t, t.TempDir(), twoProfiles))
			o := newOpener(t, cfg)

			// Act.
			if err := o.Open(context.Background(), "https://example.com/page", tc.email); err != nil {
				t.Fatalf("Open() = %v, want nil", err)
			}

			// Assert.
			want := "--profile-directory=" + tc.wantProfile + "\nhttps://example.com/page\n"
			if got := readFile(t, record); got != want {
				t.Fatalf("launch argv = %q, want %q", got, want)
			}
		})
	}
}

// TestOpenDefaultWithNoAccountPassesNoProfileFlag pins the logged-out state: an
// answer, not a fault, so the url goes to the browser with no profile flag and
// nothing is logged above DEBUG.
func TestOpenDefaultWithNoAccountPassesNoProfileFlag(t *testing.T) {
	// Arrange.
	cfg, record := recordingLauncher(t, writeLocalState(t, t.TempDir(), twoProfiles))
	o, logger := newOpenerLogged(t, cfg)

	// Act.
	if err := o.Open(context.Background(), "https://example.com/page", ""); err != nil {
		t.Fatalf("Open() = %v, want nil", err)
	}

	// Assert.
	if got := readFile(t, record); got != "https://example.com/page\n" {
		t.Fatalf("launch argv = %q, want the url alone", got)
	}
	for _, r := range logger.Records() {
		if r.Level != dlog.LevelDebug {
			t.Fatalf("logged-out open left a %s record: %+v", r.Level, r)
		}
	}
}

// TestOpenDefaultFailsAnAccountItCannotRoute pins that there is NO PINNED
// DEFAULT: an account whose Chrome profile cannot be found fails the open,
// launches nothing, points at the user guide, and records the email at ERROR.
func TestOpenDefaultFailsAnAccountItCannotRoute(t *testing.T) {
	tests := []struct {
		name       string
		localState func(t *testing.T) string
		wantBranch string
	}{
		{
			name:       "Local State is unreadable",
			localState: func(t *testing.T) string { return filepath.Join(t.TempDir(), "no-such-Local-State") },
			wantBranch: "local-state-unreadable",
		},
		{
			name:       "no profile is signed in as the account",
			localState: func(t *testing.T) string { return writeLocalState(t, t.TempDir(), twoProfiles) },
			wantBranch: "no-profile-match",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			cfg, record := recordingLauncher(t, tc.localState(t))
			o, logger := newOpenerLogged(t, cfg)

			// Act.
			err := o.Open(context.Background(), "https://example.com/page", "stranger@example.com")

			// Assert.
			if err == nil || !strings.Contains(err.Error(), usersetup.Doc) {
				t.Fatalf("Open() = %v, want an error pointing at %s", err, usersetup.Doc)
			}
			if _, statErr := os.Stat(record); statErr == nil {
				t.Fatal("the browser was launched for an account with no profile")
			}
			if !loggedError(logger.Records(), "daemon.externalbrowser.profile_for_account", tc.wantBranch, "stranger@example.com") {
				t.Fatalf("records = %+v, want an ERROR on branch %q naming the email", logger.Records(), tc.wantBranch)
			}
		})
	}
}

// TestOpenOverriddenSkipsTheProfileRouting pins that a launcher override names
// the whole launch: an account no profile matches is never looked up.
func TestOpenOverriddenSkipsTheProfileRouting(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	o := newOpener(t, externalbrowser.Config{
		LauncherCmd:    writeScript(t, dir, "launcher", "exit 0"),
		LocalStatePath: filepath.Join(dir, "no-such-Local-State"),
	})

	// Act.
	err := o.Open(context.Background(), "https://example.com/page", "stranger@example.com")

	// Assert.
	if err != nil {
		t.Fatalf("Open() = %v, want nil", err)
	}
}

// loggedError reports whether an ERROR record under op on branch names email.
func loggedError(records []dlog.Record, op, branch, email string) bool {
	for _, r := range records {
		if r.Level == dlog.LevelError && r.Operation == op && r.Context["branch"] == branch && r.Context["email"] == email {
			return true
		}
	}
	return false
}

// writeScript plants an executable shell script and answers its path.
func writeScript(t *testing.T, dir, name, body string) string {
	t.Helper()
	path := filepath.Join(dir, name)
	if err := os.WriteFile(path, []byte("#!/bin/sh\n"+body+"\n"), 0o700); err != nil {
		t.Fatalf("WriteFile() = %v", err)
	}
	return path
}

// readFile answers a file's whole content.
func readFile(t *testing.T, path string) string {
	t.Helper()
	body, err := os.ReadFile(path) //nolint:gosec // test-owned path
	if err != nil {
		t.Fatalf("ReadFile(%s) = %v", path, err)
	}
	return string(body)
}

// shellQuote wraps a path for the fake scripts' single-quoted shell.
func shellQuote(s string) string {
	return "'" + strings.ReplaceAll(s, "'", `'\''`) + "'"
}

// killRecordedPID reaps a fake launcher that deliberately never exits.
func killRecordedPID(t *testing.T, pidFile string) {
	t.Helper()
	body, err := os.ReadFile(pidFile) //nolint:gosec // test-owned path
	if err != nil {
		return
	}
	var pid int
	if _, err := fmtSscan(strings.TrimSpace(string(body)), &pid); err != nil || pid <= 0 {
		return
	}
	proc, err := os.FindProcess(pid)
	if err != nil {
		return
	}
	_ = proc.Kill()
}

// fmtSscan is fmt.Sscan, named here so the import list stays honest about why
// the test parses a pid at all.
func fmtSscan(s string, a ...any) (int, error) { return fmt.Sscan(s, a...) }

// TestOpenDefaultRaisesTheBrowserBeforeHandingOverTheURL pins the ORDER the
// default path exists for: Chrome raises the profile window it puts the tab
// in but does not bring itself to the front, so activating afterwards would
// restore whichever window was frontmost before — routinely the other
// profile's.
func TestOpenDefaultRaisesTheBrowserBeforeHandingOverTheURL(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	order := filepath.Join(dir, "order")
	activate := writeScript(t, dir, "activate", "echo activate >> "+shellQuote(order))
	launch := writeScript(t, dir, "launch", "echo launch >> "+shellQuote(order))
	o := newOpener(t, externalbrowser.Config{ActivateBin: activate, DefaultLauncherBin: launch})

	// Act.
	err := o.Open(context.Background(), "https://example.com/page", "")

	// Assert.
	if err != nil {
		t.Fatalf("Open() = %v, want nil", err)
	}
	if got := readFile(t, order); got != "activate\nlaunch\n" {
		t.Fatalf("order = %q, want the raise before the hand-off", got)
	}
}

// TestOpenDefaultSurfacesAFailedRaise pins that a raise that failed is its own
// loud error, distinct from a failed hand-off.
func TestOpenDefaultSurfacesAFailedRaise(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	activate := writeScript(t, dir, "activate", "exit 1")
	launch := writeScript(t, dir, "launch", "exit 0")
	o := newOpener(t, externalbrowser.Config{ActivateBin: activate, DefaultLauncherBin: launch})

	// Act.
	err := o.Open(context.Background(), "https://example.com", "")

	// Assert.
	if err == nil {
		t.Fatal("Open() = nil error, want the failed raise surfaced")
	}
	if !strings.Contains(err.Error(), externalbrowser.DefaultApp) {
		t.Fatalf("err = %v, want it to name the app it could not raise", err)
	}
}

// TestOpenDefaultDoesNotHandOverAfterAFailedRaise pins that a launch whose
// raise failed is abandoned rather than half-performed.
func TestOpenDefaultDoesNotHandOverAfterAFailedRaise(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	marker := filepath.Join(dir, "handed-over")
	activate := writeScript(t, dir, "activate", "exit 1")
	launch := writeScript(t, dir, "launch", "touch "+shellQuote(marker))
	o := newOpener(t, externalbrowser.Config{ActivateBin: activate, DefaultLauncherBin: launch})

	// Act.
	if err := o.Open(context.Background(), "https://example.com", ""); err == nil {
		t.Fatal("Open() = nil error, want the failed raise surfaced")
	}

	// Assert.
	if _, statErr := os.Stat(marker); statErr == nil {
		t.Fatal("the url was handed over after the raise failed")
	}
}

// TestOpenDefaultSurfacesAFailedHandOff pins the second half: the browser was
// raised but refused the url, which names the profile it was meant for.
func TestOpenDefaultSurfacesAFailedHandOff(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	activate := writeScript(t, dir, "activate", "exit 0")
	launch := writeScript(t, dir, "launch", "exit 4")
	o := newOpener(t, externalbrowser.Config{
		ActivateBin: activate, DefaultLauncherBin: launch,
		LocalStatePath: writeLocalState(t, dir, twoProfiles),
	})

	// Act.
	err := o.Open(context.Background(), "https://example.com", "work@example.com")

	// Assert.
	if err == nil {
		t.Fatal("Open() = nil error, want the failed hand-off surfaced")
	}
	if !strings.Contains(err.Error(), "Profile 1") {
		t.Fatalf("err = %v, want it to name the profile the url was meant for", err)
	}
}

// TestOpenDefaultSurfacesAnAbsentBrowser pins the host where the pinned binary
// is not installed: the launch fails synchronously and loudly.
func TestOpenDefaultSurfacesAnAbsentBrowser(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	activate := writeScript(t, dir, "activate", "exit 0")
	o := newOpener(t, externalbrowser.Config{
		ActivateBin: activate, DefaultLauncherBin: filepath.Join(dir, "not-installed"),
	})

	// Act.
	err := o.Open(context.Background(), "https://example.com", "")

	// Assert.
	if err == nil {
		t.Fatal("Open() = nil error, want the absent browser surfaced")
	}
}

// TestOpenAbandonsALaunchTheCallerCancels pins the third arm of the launch
// window: neither the launcher exiting nor the window elapsing, but the caller
// giving up while the launcher is still running.
func TestOpenAbandonsALaunchTheCallerCancels(t *testing.T) {
	// Arrange: the launcher announces itself down a fifo — a blocking write
	// the test's read completes — so the cancellation lands while the launch
	// is genuinely in flight, with nothing polled and nothing slept on.
	dir := t.TempDir()
	started := filepath.Join(dir, "started")
	if err := syscall.Mkfifo(started, 0o600); err != nil {
		t.Fatalf("Mkfifo() = %v", err)
	}
	pidFile := filepath.Join(dir, "pid")
	script := writeScript(t, dir, "slow-launcher", `echo $$ > `+shellQuote(pidFile)+`
echo up > `+shellQuote(started)+`
exec tail -f /dev/null`)
	t.Cleanup(func() { killRecordedPID(t, pidFile) })
	o := newOpener(t, externalbrowser.Config{LauncherCmd: script, LaunchWindow: time.Minute})
	ctx, cancel := context.WithCancel(context.Background())

	// Act.
	done := make(chan error, 1)
	go func() { done <- o.Open(ctx, "https://example.com", "") }()
	fifo, err := os.Open(started) //nolint:gosec // test-owned path
	if err != nil {
		t.Fatalf("open the fifo: %v", err)
	}
	if _, err := io.ReadAll(fifo); err != nil {
		t.Fatalf("read the fifo: %v", err)
	}
	if err := fifo.Close(); err != nil {
		t.Fatalf("close the fifo: %v", err)
	}
	cancel()

	// Assert.
	if err := <-done; !errors.Is(err, context.Canceled) {
		t.Fatalf("Open() = %v, want context.Canceled", err)
	}
}
