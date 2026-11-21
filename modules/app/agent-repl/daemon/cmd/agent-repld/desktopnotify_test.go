package main

import (
	"context"
	"errors"
	"os"
	"reflect"
	"runtime"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/desktopnotify"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// namesDB answers one workspace record, or one error.
type namesDB struct {
	wsm.DB
	record wsm.Workspace
	err    error
}

func (d namesDB) Workspace(context.Context, ids.WorkspaceID) (wsm.Workspace, error) {
	return d.record, d.err
}

func TestBannerWorkspacesAnswersTheRegistryNameAndDirectory(t *testing.T) {
	// Arrange
	workspaces := bannerWorkspaces{db: namesDB{record: wsm.Workspace{Name: "fix-flaky-reconnect", Dir: "/work/fix-flaky-reconnect"}}}

	// Act
	got, err := workspaces.Workspace(context.Background(), "ws1")

	// Assert
	want := desktopnotify.Workspace{Name: "fix-flaky-reconnect", Dir: "/work/fix-flaky-reconnect"}
	if err != nil || got != want {
		t.Fatalf("Workspace = (%+v, %v), want %+v", got, err, want)
	}
}

func TestBannerWorkspacesSurfacesAFailedRead(t *testing.T) {
	// Arrange
	cause := errors.New("no such workspace")
	workspaces := bannerWorkspaces{db: namesDB{err: cause}}

	// Act
	_, err := workspaces.Workspace(context.Background(), "ws1")

	// Assert
	if !errors.Is(err, cause) || !strings.Contains(err.Error(), `"ws1"`) {
		t.Fatalf("Workspace error = %v, want the read's cause naming the workspace", err)
	}
}

// recordingClicks records the clicks relayed to it.
type recordingClicks struct{ got []ids.WorkspaceID }

func (c *recordingClicks) NotificationClicked(ws ids.WorkspaceID) { c.got = append(c.got, ws) }

func TestClickForwarderRelaysToTheBoundServer(t *testing.T) {
	// Arrange
	target := &recordingClicks{}
	f := &clickForwarder{}
	f.bind(target)

	// Act
	f.NotificationClicked("ws1")

	// Assert
	if len(target.got) != 1 || target.got[0] != "ws1" {
		t.Fatalf("relayed %v, want [ws1]", target.got)
	}
}

func TestClickForwarderRefusesAClickBeforeTheServerIsBound(t *testing.T) {
	// Arrange
	f := &clickForwarder{}

	// Assert
	defer func() {
		if recover() == nil {
			t.Fatal("an unbound forwarder accepted a click")
		}
	}()

	// Act
	f.NotificationClicked("ws1")
}

// TestResolveBannerBackendIsTheAppOnMacOS pins that on macOS every banner is
// the launcher's, whatever program the environment names, and that
// elsewhere the platform's program honors the override.
func TestResolveBannerBackendIsTheLauncherOnMacOS(t *testing.T) {
	// Arrange
	t.Setenv("AGENT_REPL_NOTIFIER_CMD", "/tmp/fake-banner")
	log := dlog.NewTestLogger()
	launcher := desktopnotify.NewLauncher("/sock/launcher.sock", desktopnotify.LauncherPostBound, log)

	// Act
	backend, err := resolveBannerBackend(log, launcher)

	// Assert
	if err != nil {
		t.Fatalf("resolveBannerBackend: %v", err)
	}
	want := "/tmp/fake-banner"
	if runtime.GOOS == "darwin" {
		want = desktopnotify.LauncherProgram
	}
	if backend.Program() != want {
		t.Fatalf("program = %q, want %q on %s", backend.Program(), want, runtime.GOOS)
	}
}

func TestLauncherDeployedOnlyOnMacOS(t *testing.T) {
	cases := []struct {
		goos string
		want bool
	}{{"darwin", true}, {"linux", false}}
	for _, tc := range cases {
		t.Run(tc.goos, func(t *testing.T) {
			// Act
			got := launcherDeployed(tc.goos)

			// Assert
			if got != tc.want {
				t.Fatalf("launcherDeployed(%s) = %v, want %v", tc.goos, got, tc.want)
			}
		})
	}
}

func TestResolveChimeTakesTheOverride(t *testing.T) {
	// Arrange
	t.Setenv("AGENT_REPL_CHIME_CMD", "/tmp/fake-chime")
	log := dlog.NewTestLogger()

	// Act
	chime, err := resolveChime(log)

	// Assert
	if err != nil {
		t.Fatalf("resolveChime: %v", err)
	}
	if chime.Program() != "/tmp/fake-chime" {
		t.Fatalf("program = %q, want the override", chime.Program())
	}
}

// fakeProgram is a resolved program for resolveNotifyProgram.
type fakeProgram struct{ bin string }

func (p fakeProgram) Program() string { return p.bin }

func TestResolveNotifyProgramRecordsTheResolvedProgram(t *testing.T) {
	// Arrange
	t.Setenv("AGENT_REPL_TEST_OVERRIDE", "/tmp/override")
	log := dlog.NewTestLogger()

	// Act
	got, err := resolveNotifyProgram(log, "chime", "AGENT_REPL_TEST_OVERRIDE", func(_, override string) (fakeProgram, error) {
		return fakeProgram{bin: override}, nil
	})

	// Assert
	if err != nil || got.bin != "/tmp/override" {
		t.Fatalf("resolveNotifyProgram = (%+v, %v), want the override", got, err)
	}
	r := findRecord(t, log.Records(), "resolved the desktop chime program")
	if r.Level != "info" || r.Context["role"] != "chime" || r.Context["program"] != "/tmp/override" {
		t.Fatalf("record = %+v, want the role and program at info", r)
	}
}

func TestResolveNotifyProgramRecordsAMissingProgram(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	cause := errors.New("not installed")

	// Act
	_, err := resolveNotifyProgram(log, "banner", "AGENT_REPL_TEST_UNSET", func(_, _ string) (fakeProgram, error) {
		return fakeProgram{}, cause
	})

	// Assert
	if !errors.Is(err, cause) {
		t.Fatalf("err = %v, want the build's cause", err)
	}
	r := findRecord(t, log.Records(), "no desktop banner program; it will not run")
	if r.Level != "error" || r.Context["role"] != "banner" || r.Context["cause"] != "not installed" || r.Context["override_env"] != "AGENT_REPL_TEST_UNSET" {
		t.Fatalf("record = %+v, want the role, cause and override env at error", r)
	}
}

// TestNotifyProgramsResolveThroughOneHelper fails a resolver that hand-rolls
// its boot record instead of asking resolveNotifyProgram.
func TestNotifyProgramsResolveThroughOneHelper(t *testing.T) {
	// Arrange
	src, err := os.ReadFile("desktopnotify.go")
	if err != nil {
		t.Fatalf("read desktopnotify.go: %v", err)
	}

	// Act
	asks := strings.Count(string(src), "return resolveNotifyProgram(")
	records := strings.Count(string(src), "log.Error(opDesktopNotify")

	// Assert
	if asks != 2 || records != 1 {
		t.Fatalf("resolveNotifyProgram( x%d (want 2), log.Error(opDesktopNotify x%d (want 1)", asks, records)
	}
}

func TestSpeechPlatformTakesTheOverrides(t *testing.T) {
	// Arrange
	t.Setenv(desktopnotify.EnvSayCmd, "/tmp/fake-say")
	t.Setenv(desktopnotify.EnvAudioOutputCmd, "/tmp/fake-audio")
	t.Setenv(desktopnotify.EnvBluetoothCmd, "/tmp/fake-bluetooth")
	want := desktopnotify.SpeechPlatformFor(runtime.GOOS, "/bin/agent-repld").WithOverrides("/tmp/fake-say", "/tmp/fake-audio", "/tmp/fake-bluetooth")

	// Act
	got := speechPlatform("/bin/agent-repld")

	// Assert
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("speechPlatform = %+v, want this platform's with the overrides %+v", got, want)
	}
}

// fixedStanding answers one standing.
type fixedStanding struct {
	state *agentreplv1.PersistentWifiState
}

func (s fixedStanding) Latest() (*agentreplv1.PersistentWifiState, bool) { return s.state, true }

func TestWifiStandingForwarderAnswersTheBoundStanding(t *testing.T) {
	// Arrange
	state := &agentreplv1.PersistentWifiState{Mode: &agentreplv1.PersistentWifiState_On{On: &agentreplv1.PersistentWifiModeOn{}}}
	f := &wifiStandingForwarder{}
	f.bind(fixedStanding{state: state})

	// Act
	got, ok := f.Latest()

	// Assert
	if !ok || got != state {
		t.Fatalf("Latest = %v, %v, want the bound standing", got, ok)
	}
}

func TestWifiStandingForwarderPanicsUnbound(t *testing.T) {
	// Assert
	defer func() {
		if recover() == nil {
			t.Fatal("an unbound forwarder answered a standing")
		}
	}()

	// Act
	(&wifiStandingForwarder{}).Latest()
}
