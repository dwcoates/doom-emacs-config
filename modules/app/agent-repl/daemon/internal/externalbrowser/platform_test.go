package externalbrowser_test

import (
	"context"
	"os"
	"path/filepath"
	"reflect"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/externalbrowser"
)

func TestPlatformFor(t *testing.T) {
	tests := []struct {
		name    string
		goos    string
		want    externalbrowser.Platform
		wantErr bool
	}{
		{name: "macOS raises Chrome with osascript", goos: "darwin", want: externalbrowser.Darwin},
		{name: "Linux hands the url to google-chrome and raises nothing", goos: "linux", want: externalbrowser.Linux},
		{name: "a platform agent-repl does not run on is refused", goos: "windows", wantErr: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got, err := externalbrowser.PlatformFor(tc.goos)

			// Assert.
			if (err != nil) != tc.wantErr || !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("PlatformFor(%q) = (%+v, %v), want %+v (error %v)", tc.goos, got, err, tc.want, tc.wantErr)
			}
		})
	}
}

func TestLinuxPlatformNamesFreedesktopPrograms(t *testing.T) {
	// Assert.
	l := externalbrowser.Linux
	if l.Binary != "google-chrome" || l.ActivateBin != "" || l.SlackOpen != "xdg-open" ||
		l.LocalState != filepath.Join(".config", "google-chrome", "Local State") {
		t.Fatalf("Linux = %+v", l)
	}
}

// TestOpenDefaultOnAPlatformThatRaisesNothingOnlyHandsTheURLOver pins Linux's
// default path: no raise runs, and the routed profile reaches the launcher.
func TestOpenDefaultOnAPlatformThatRaisesNothingOnlyHandsTheURLOver(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	record := filepath.Join(dir, "argv")
	launch := writeScript(t, dir, "launch", `printf '%s\n' "$@" > `+shellQuote(record))
	platform := externalbrowser.Platform{Binary: launch}
	o, logger := newOpenerLogged(t, externalbrowser.Config{Platform: &platform, LocalStatePath: writeLocalState(t, dir, twoProfiles)})

	// Act.
	err := o.Open(context.Background(), o.log, "https://example.com/page", "work@example.com")

	// Assert.
	if err != nil {
		t.Fatalf("Open() = %v, want nil", err)
	}
	if got := readFile(t, record); got != "--profile-directory=Profile 1\nhttps://example.com/page\n" {
		t.Fatalf("launch argv = %q, want the routed profile and the url", got)
	}
	if !loggedBranch(logger.Records(), dlog.LevelDebug, "no-activate") {
		t.Fatalf("records = %+v, want a record on branch no-activate", logger.Records())
	}
}

// TestEmptyFieldsTakeThePlatforms pins that every default the config leaves
// empty is the platform's: its binary, its raise and its argv, and its Local
// State under the home directory.
func TestEmptyFieldsTakeThePlatforms(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	order := filepath.Join(dir, "order")
	activate := writeScript(t, dir, "activate", `printf 'activate %s\n' "$*" >> `+shellQuote(order))
	launch := writeScript(t, dir, "launch", `printf 'launch %s\n' "$*" >> `+shellQuote(order))
	home := filepath.Join(dir, "home")
	if err := os.MkdirAll(filepath.Join(home, "chrome"), 0o755); err != nil {
		t.Fatal(err)
	}
	writeLocalState(t, filepath.Join(home, "chrome"), twoProfiles)
	t.Setenv("HOME", home)
	platform := externalbrowser.Platform{
		Binary: launch, ActivateBin: activate, ActivateArgv: []string{"-raise"},
		LocalState: filepath.Join("chrome", "Local State"),
	}
	o := newOpener(t, externalbrowser.Config{Platform: &platform})

	// Act.
	err := o.Open(context.Background(), o.log, "https://example.com/page", "personal@example.com")

	// Assert.
	if err != nil {
		t.Fatalf("Open() = %v, want nil", err)
	}
	want := "activate -raise\nlaunch --profile-directory=Default https://example.com/page\n"
	if got := readFile(t, order); got != want {
		t.Fatalf("order = %q, want %q", got, want)
	}
}
