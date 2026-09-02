package externalbrowser_test

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/externalbrowser"
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

func TestLaunchArgvCarriesTheProfileAndTheURL(t *testing.T) {
	// Arrange, Act.
	got := externalbrowser.LaunchArgv("Profile 9", "https://example.com")

	// Assert.
	want := []string{"--profile-directory=Profile 9", "https://example.com"}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("LaunchArgv() = %v, want %v", got, want)
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
	err := o.Open(context.Background(), "https://example.com/page")

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
	err := o.Open(context.Background(), "https://example.com")

	// Assert.
	if err == nil {
		t.Fatal("Open() = nil error, want the launcher's non-zero exit surfaced")
	}
}

func TestOpenSurfacesAMissingLauncher(t *testing.T) {
	// Arrange.
	o := newOpener(t, externalbrowser.Config{LauncherCmd: filepath.Join(t.TempDir(), "not-installed")})

	// Act.
	err := o.Open(context.Background(), "https://example.com")

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
	err := o.Open(context.Background(), "https://example.com")

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
	err := o.Open(context.Background(), "file:///etc/passwd")

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
	err := o.Open(context.Background(), "https://example.com/env")

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
	err := o.Open(ctx, "https://example.com")

	// Assert.
	if err == nil {
		t.Fatal("Open() = nil error, want the context's error")
	}
}

// newOpener builds an opener over cfg with a capturing logger.
func newOpener(t *testing.T, cfg externalbrowser.Config) externalbrowser.Opener {
	t.Helper()
	cfg.Logger = dlog.NewTestLogger()
	o, err := externalbrowser.New(cfg)
	if err != nil {
		t.Fatalf("externalbrowser.New() = %v, want nil", err)
	}
	return o
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
