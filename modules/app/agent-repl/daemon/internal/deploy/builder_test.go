package deploy

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
)

func newScriptBuilder(t *testing.T, answers map[string]runAnswer) (*ScriptBuilder, *scriptedRunner, *dlog.TestLogger) {
	t.Helper()
	runner := &scriptedRunner{answers: answers}
	log := dlog.NewTestLogger()
	return &ScriptBuilder{
		ModuleRoot: "/checkout/modules/app/agent-repl",
		LogDir:     t.TempDir(),
		Runner:     runner,
		Clock:      newStepClock(),
		Log:        log,
	}, runner, log
}

func TestTheBuildRegeneratesTheProtosThenStagesEveryTargetInOrder(t *testing.T) {
	// Arrange
	b, runner, _ := newScriptBuilder(t, map[string]runAnswer{})

	// Act
	err := b.Build(context.Background(), "/staging/1")

	// Assert
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	calls := runner.Calls()
	want := []string{"make -C /checkout/modules/app/agent-repl/proto all"}
	for _, target := range Targets {
		want = append(want, "bash /checkout/modules/app/agent-repl/bin/build-frontend.sh --out /staging/1 "+target)
	}
	if len(calls) != len(want) {
		t.Fatalf("steps = %v, want %v", calls, want)
	}
	for i := range want {
		if got := strings.Join(calls[i], " "); got != want[i] {
			t.Fatalf("step %d = %q, want %q", i, got, want[i])
		}
	}
	for _, dir := range runner.dirs {
		if dir != b.ModuleRoot {
			t.Fatalf("a step ran in %q, want the module root", dir)
		}
	}
}

func TestAFailedStepNamesItselfAndStopsTheBuild(t *testing.T) {
	tests := []struct {
		name       string
		answer     runAnswer
		wantDetail string
	}{
		{name: "a step that exits non-zero", answer: runAnswer{out: "vite: 2 errors\nerror TS2322", code: 2}, wantDetail: "error TS2322"},
		{name: "a step that could not start", answer: runAnswer{err: errors.New("exec: bash: not found")}, wantDetail: "not found"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			step := "bash /checkout/modules/app/agent-repl/bin/build-frontend.sh --out /staging/1 webapp"
			b, runner, log := newScriptBuilder(t, map[string]runAnswer{step: tc.answer})

			// Act
			err := b.Build(context.Background(), "/staging/1")

			// Assert
			var failed *BuildFailed
			if !errors.As(err, &failed) || failed.Step != "webapp" || !strings.Contains(failed.Detail, tc.wantDetail) {
				t.Fatalf("Build = %v, want the webapp step named with %q", err, tc.wantDetail)
			}
			if last := runner.Calls()[len(runner.Calls())-1]; strings.Join(last, " ") != step {
				t.Fatalf("the build ran past the failed step: last %v", last)
			}
			archived, readErr := os.ReadFile(failed.Log)
			if readErr != nil || !strings.Contains(string(archived), "==== webapp") {
				t.Fatalf("archive = %q (%v), want the failed step's output kept", archived, readErr)
			}
			if !loggedTo(log, "error", "a build step failed") {
				t.Fatalf("records = %+v, want the failure at ERROR", log.Records())
			}
		})
	}
}

func TestABuildThatCannotMakeItsLogDirectoryBuildsNothing(t *testing.T) {
	// Arrange: the log directory's parent is a file.
	b, runner, _ := newScriptBuilder(t, map[string]runAnswer{})
	blocker := filepath.Join(t.TempDir(), "file")
	writeFile(t, blocker, "x")
	b.LogDir = filepath.Join(blocker, "logs")

	// Act
	err := b.Build(context.Background(), "/staging/1")

	// Assert
	var failed *BuildFailed
	if !errors.As(err, &failed) || failed.Step != "setup" {
		t.Fatalf("Build = %v, want the setup failure", err)
	}
	if len(runner.Calls()) != 0 {
		t.Fatalf("steps ran with no log to keep them in")
	}
}

func TestStagedNamesTheBuildFrontendLayout(t *testing.T) {
	// Arrange
	s := Staged{Dir: "/s"}

	// Act + Assert
	for got, want := range map[string]string{
		s.ShimMain():            "/s/agent-shim/claude/shim/dist/main.js",
		s.WebappDist():          "/s/webapp/dist",
		s.DaemonBin():           "/s/daemon/bin/agent-repld",
		s.CacheBin("shim-lock"): "/s/cache-bin/shim-lock",
		s.WifiApp():             "/s/wifi-helper/AgentReplWifi.app",
	} {
		if got != want {
			t.Fatalf("staged path = %q, want %q", got, want)
		}
	}
}

func TestAnOverrideReplacesEveryStepWithOneCommand(t *testing.T) {
	// Arrange
	b, runner, _ := newScriptBuilder(t, map[string]runAnswer{})
	b.Override = "/harness/fake-build"

	// Act
	err := b.Build(context.Background(), "/staging/1")

	// Assert
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	calls := runner.Calls()
	if len(calls) != 1 || strings.Join(calls[0], " ") != "/harness/fake-build --out /staging/1" {
		t.Fatalf("steps = %v, want the override alone, given the staging directory", calls)
	}
}

func TestAFailedOverrideIsTheBuildStep(t *testing.T) {
	// Arrange
	b, _, log := newScriptBuilder(t, map[string]runAnswer{"/harness/fake-build --out /staging/1": {out: "fake build refused", code: 1}})
	b.Override = "/harness/fake-build"

	// Act
	err := b.Build(context.Background(), "/staging/1")

	// Assert
	var failed *BuildFailed
	if !errors.As(err, &failed) || failed.Step != "build" || !strings.Contains(failed.Detail, "fake build refused") {
		t.Fatalf("Build = %v, want the override's failure named as the build step", err)
	}
	if !loggedTo(log, "error", "a build step failed") {
		t.Fatalf("records = %+v, want the failure at ERROR", log.Records())
	}
}

func TestTheWifiHelperIsBuiltLastByItsOwnBuildWhereThePlatformHasOne(t *testing.T) {
	tests := []struct {
		name       string
		wifiHelper bool
		want       string
	}{
		{name: "the platform with a helper builds it", wifiHelper: true,
			want: "bash /checkout/modules/app/agent-repl/wifi-helper/build.sh /staging/1/wifi-helper/AgentReplWifi.app"},
		{name: "a platform with none does not", wifiHelper: false,
			want: "bash /checkout/modules/app/agent-repl/bin/build-frontend.sh --out /staging/1 " + Targets[len(Targets)-1]},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			b, runner, _ := newScriptBuilder(t, map[string]runAnswer{})
			b.WifiHelper = tc.wifiHelper

			// Act
			if err := b.Build(context.Background(), "/staging/1"); err != nil {
				t.Fatalf("Build: %v", err)
			}

			// Assert
			calls := runner.Calls()
			if got := strings.Join(calls[len(calls)-1], " "); got != tc.want {
				t.Fatalf("last step = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestAFailedWifiHelperBuildNamesItsStep(t *testing.T) {
	// Arrange
	step := "bash /checkout/modules/app/agent-repl/wifi-helper/build.sh /staging/1/wifi-helper/AgentReplWifi.app"
	b, _, _ := newScriptBuilder(t, map[string]runAnswer{step: {out: "swiftc: error: no such module", code: 1}})
	b.WifiHelper = true

	// Act
	err := b.Build(context.Background(), "/staging/1")

	// Assert
	var failed *BuildFailed
	if !errors.As(err, &failed) || failed.Step != "wifi" || !strings.Contains(failed.Detail, "no such module") {
		t.Fatalf("Build = %v, want the wifi step named with swiftc's words", err)
	}
}

func TestWifiSourceHash(t *testing.T) {
	tests := []struct {
		name    string
		content *string
		want    string
		wantErr string
	}{
		{name: "the hash the build wrote, without its newline", content: ptr("abc123\n"), want: "abc123"},
		{name: "an app naming no hash", content: ptr("  \n"), wantErr: "names no source hash"},
		{name: "an app with no hash file", content: nil, wantErr: "read the Wi-Fi helper's source hash"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			app := filepath.Join(t.TempDir(), "AgentReplWifi.app")
			if tc.content != nil {
				writeFile(t, filepath.Join(app, "Contents", "Resources", "source-sha256"), *tc.content)
			}

			// Act
			got, err := wifiSourceHash(app)

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("wifiSourceHash = %q, %v; want an error saying %q", got, err, tc.wantErr)
				}
				return
			}
			if err != nil || got != tc.want {
				t.Fatalf("wifiSourceHash = %q, %v; want %q", got, err, tc.want)
			}
		})
	}
}

func ptr(s string) *string { return &s }
