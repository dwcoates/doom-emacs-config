package deploy

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"sort"
	"strings"
	"sync"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/restarttiming"
	"agent-repld/internal/restarttiming/restarttimingtest"
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
		Observer:   &stepObserver{},
		Log:        log,
		Restarts:   &restarttimingtest.Recorder{},
	}, runner, log
}

// stepObserver records each build step as "began:<step>" or
// "ended:<step>" ("ended:<step>:failed" for a failed one), in the order it is
// told; ended, when set, is sent each step that ended.
type stepObserver struct {
	mu    sync.Mutex
	told  []string
	ended chan string
}

func (o *stepObserver) BuildStepBegan(step string) {
	o.mu.Lock()
	defer o.mu.Unlock()
	o.told = append(o.told, "began:"+step)
}

func (o *stepObserver) BuildStepEnded(step string, err error) {
	o.mu.Lock()
	if err != nil {
		o.told = append(o.told, "ended:"+step+":failed")
	} else {
		o.told = append(o.told, "ended:"+step)
	}
	o.mu.Unlock()
	if o.ended != nil {
		o.ended <- step
	}
}

// Told is every step the observer was told, in order.
func (o *stepObserver) Told() []string {
	o.mu.Lock()
	defer o.mu.Unlock()
	return append([]string(nil), o.told...)
}

const (
	protoStep  = "bash /checkout/modules/app/agent-repl/bin/build-frontend.sh proto"
	targetStep = "bash /checkout/modules/app/agent-repl/bin/build-frontend.sh --out /staging/1 "
	wifiStep   = "bash /checkout/modules/app/agent-repl/wifi-helper/build.sh /staging/1/wifi-helper/AgentReplWifi.app --reuse /home/.cache/agent-repl/AgentReplWifi.app"
)

// sortedCopy is xs sorted, for comparing what concurrent steps did.
func sortedCopy(xs []string) []string {
	out := append([]string(nil), xs...)
	sort.Strings(out)
	return out
}

// joinedCalls is each call's argv, joined.
func joinedCalls(calls [][]string) []string {
	out := make([]string, len(calls))
	for i, c := range calls {
		out[i] = strings.Join(c, " ")
	}
	return out
}

func TestTheBuildTellsItsObserverEveryStepAsItBeginsAndEnds(t *testing.T) {
	// Arrange
	b, _, _ := newScriptBuilder(t, map[string]runAnswer{})

	// Act
	err := b.Build(context.Background(), "/staging/1")

	// Assert: the protos first, then every target begun before it ended.
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	got := b.Observer.(*stepObserver).Told()
	if len(got) < 2 || got[0] != "began:proto" || got[1] != "ended:proto" {
		t.Fatalf("observer told %v, want the protos built first", got)
	}
	var want []string
	for _, target := range Targets {
		want = append(want, "began:"+target, "ended:"+target)
	}
	if strings.Join(sortedCopy(got[2:]), ",") != strings.Join(sortedCopy(want), ",") {
		t.Fatalf("observer told %v, want every target begun and ended once", got[2:])
	}
	for _, target := range Targets {
		if slices.Index(got, "began:"+target) > slices.Index(got, "ended:"+target) {
			t.Fatalf("observer told %s ended before it began: %v", target, got)
		}
	}
}

func TestAFailedProtoStepIsTheLastStepTheObserverIsTold(t *testing.T) {
	// Arrange
	b, runner, _ := newScriptBuilder(t, map[string]runAnswer{protoStep: {code: 1}})

	// Act
	err := b.Build(context.Background(), "/staging/1")

	// Assert
	var failed *BuildFailed
	if !errors.As(err, &failed) || failed.Step != "proto" {
		t.Fatalf("Build = %v, want the proto step's failure", err)
	}
	if got := b.Observer.(*stepObserver).Told(); strings.Join(got, ",") != "began:proto,ended:proto:failed" {
		t.Fatalf("observer told %v, want the proto step alone", got)
	}
	if calls := joinedCalls(runner.Calls()); len(calls) != 1 || calls[0] != protoStep {
		t.Fatalf("steps = %v, want no target built over failed protos", calls)
	}
}

func TestAFailedTargetDoesNotStopItsSiblings(t *testing.T) {
	// Arrange
	b, runner, log := newScriptBuilder(t, map[string]runAnswer{targetStep + "shim": {out: "esbuild: 1 error", code: 1}})

	// Act
	err := b.Build(context.Background(), "/staging/1")

	// Assert: the build fails on the shim, and every other target still built.
	var failed *BuildFailed
	if !errors.As(err, &failed) || failed.Step != "shim" {
		t.Fatalf("Build = %v, want the shim step's failure", err)
	}
	if len(runner.Calls()) != 1+len(Targets) {
		t.Fatalf("steps = %v, want every target run beside the failed one", joinedCalls(runner.Calls()))
	}
	told := b.Observer.(*stepObserver).Told()
	for _, target := range Targets {
		want := "ended:" + target
		if target == "shim" {
			want += ":failed"
		}
		if !slices.Contains(told, want) {
			t.Fatalf("observer told %v, want %s", told, want)
		}
	}
	if !loggedTo(log, "error", "a build step failed") {
		t.Fatalf("records = %+v, want the failure at ERROR", log.Records())
	}
}

func TestTheBuildNamesTheFirstFailedStepInBuildOrder(t *testing.T) {
	// Arrange: two targets fail; the shim comes before the daemon.
	b, _, log := newScriptBuilder(t, map[string]runAnswer{
		targetStep + "daemon": {out: "go: compile error", code: 1},
		targetStep + "shim":   {out: "esbuild: 1 error", code: 1},
	})

	// Act
	err := b.Build(context.Background(), "/staging/1")

	// Assert: the shim is named, and both failures are logged.
	var failed *BuildFailed
	if !errors.As(err, &failed) || failed.Step != "shim" {
		t.Fatalf("Build = %v, want the shim, the first failed step in build order", err)
	}
	n := 0
	for _, r := range log.Records() {
		if r.Level == "error" && strings.Contains(r.Message, "a build step failed") {
			n++
		}
	}
	if n != 2 {
		t.Fatalf("logged %d failed steps, want both: %+v", n, log.Records())
	}
}

func TestTheBuildRegeneratesTheProtosThenStagesEveryTarget(t *testing.T) {
	// Arrange
	b, runner, _ := newScriptBuilder(t, map[string]runAnswer{})

	// Act
	err := b.Build(context.Background(), "/staging/1")

	// Assert
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	calls := joinedCalls(runner.Calls())
	var want []string
	for _, target := range Targets {
		want = append(want, targetStep+target)
	}
	if len(calls) == 0 || calls[0] != protoStep || strings.Join(sortedCopy(calls[1:]), ",") != strings.Join(sortedCopy(want), ",") {
		t.Fatalf("steps = %v, want the protos, then every target once", calls)
	}
	for _, dir := range runner.dirs {
		if dir != b.ModuleRoot {
			t.Fatalf("a step ran in %q, want the module root", dir)
		}
	}
}

func TestAFailedStepNamesItselfAndKeepsItsOutput(t *testing.T) {
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
			b, _, log := newScriptBuilder(t, map[string]runAnswer{targetStep + "webapp": tc.answer})

			// Act
			err := b.Build(context.Background(), "/staging/1")

			// Assert
			var failed *BuildFailed
			if !errors.As(err, &failed) || failed.Step != "webapp" || !strings.Contains(failed.Detail, tc.wantDetail) {
				t.Fatalf("Build = %v, want the webapp step named with %q", err, tc.wantDetail)
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

func TestTheArchiveKeepsEveryStepInBuildOrder(t *testing.T) {
	// Arrange
	answers := map[string]runAnswer{protoStep: {out: "proto out"}}
	for _, target := range Targets {
		answers[targetStep+target] = runAnswer{out: target + " out"}
	}
	b, _, _ := newScriptBuilder(t, answers)

	// Act
	if err := b.Build(context.Background(), "/staging/1"); err != nil {
		t.Fatalf("Build: %v", err)
	}

	// Assert
	logs, err := filepath.Glob(filepath.Join(b.LogDir, "build-*.log"))
	if err != nil || len(logs) != 1 {
		t.Fatalf("build logs = %v (%v), want one", logs, err)
	}
	archived, err := os.ReadFile(logs[0])
	if err != nil {
		t.Fatal(err)
	}
	at := -1
	for _, step := range append([]string{"proto"}, Targets...) {
		i := strings.Index(string(archived), "==== "+step+": ")
		if i <= at || !strings.Contains(string(archived), step+" out") {
			t.Fatalf("archive = %q, want %s's section with its output after the step before it", archived, step)
		}
		at = i
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

func TestTheWifiHelperIsBuiltByItsOwnBuildWhereThePlatformHasOne(t *testing.T) {
	tests := []struct {
		name      string
		installed string
		want      bool
	}{
		{name: "the platform with a helper builds it, reusing the installed app", installed: "/home/.cache/agent-repl/AgentReplWifi.app", want: true},
		{name: "a platform with none does not", installed: "", want: false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			b, runner, _ := newScriptBuilder(t, map[string]runAnswer{})
			b.WifiInstalled = tc.installed

			// Act
			if err := b.Build(context.Background(), "/staging/1"); err != nil {
				t.Fatalf("Build: %v", err)
			}

			// Assert
			if got := slices.Contains(joinedCalls(runner.Calls()), wifiStep); got != tc.want {
				t.Fatalf("steps = %v, want the wifi helper built %v", joinedCalls(runner.Calls()), tc.want)
			}
		})
	}
}

func TestAFailedWifiHelperBuildNamesItsStep(t *testing.T) {
	// Arrange
	b, _, _ := newScriptBuilder(t, map[string]runAnswer{wifiStep: {out: "swiftc: error: no such module", code: 1}})
	b.WifiInstalled = "/home/.cache/agent-repl/AgentReplWifi.app"

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

// gatedRunner is a Runner whose every step tells started when it runs, then
// waits for the test to release it, so a test orders the steps' ends on its
// clock.
type gatedRunner struct {
	inner   *scriptedRunner
	started chan string
	mu      sync.Mutex
	release map[string]chan struct{}
}

func (r *gatedRunner) gate(argv string) chan struct{} {
	r.mu.Lock()
	defer r.mu.Unlock()
	if r.release[argv] == nil {
		r.release[argv] = make(chan struct{})
	}
	return r.release[argv]
}

func (r *gatedRunner) Run(ctx context.Context, dir string, argv []string) (string, int, error) {
	key := strings.Join(argv, " ")
	r.started <- key
	<-r.gate(key)
	return r.inner.Run(ctx, dir, argv)
}

func TestEveryBuildStepIsTimedUnderItsOwnName(t *testing.T) {
	// Arrange: the protos take 2s; then every target starts at once, the
	// webapp ends 7s later and the rest 3s after it.
	b, runner, _ := newScriptBuilder(t, map[string]runAnswer{})
	clock := newStepClock()
	b.Clock = clock
	observer := &stepObserver{ended: make(chan string, 1+len(Targets))}
	b.Observer = observer
	gated := &gatedRunner{inner: runner, started: make(chan string, 1+len(Targets)), release: map[string]chan struct{}{}}
	b.Runner = gated
	done := make(chan error, 1)

	// Act
	go func() { done <- b.Build(context.Background(), "/staging/1") }()
	<-gated.started
	<-clock.After(2 * time.Second)
	close(gated.gate(protoStep))
	<-observer.ended
	for range Targets {
		<-gated.started
	}
	<-clock.After(7 * time.Second)
	close(gated.gate(targetStep + "webapp"))
	<-observer.ended
	<-clock.After(3 * time.Second)
	for _, target := range Targets {
		if target != "webapp" {
			close(gated.gate(targetStep + target))
		}
	}
	err := <-done

	// Assert
	if err != nil {
		t.Fatalf("Build: %v", err)
	}
	got := b.Restarts.(*restarttimingtest.Recorder).Of(restarttiming.DeployBuildStep)
	if len(got) != 1+len(Targets) {
		t.Fatalf("build steps timed = %+v, want one per step", got)
	}
	timed := map[string]float64{}
	for _, o := range got {
		if o.Outcome() != restarttiming.OK {
			t.Fatalf("step %s timed %s, want ok", o.Phase(), o.Outcome())
		}
		timed[o.Phase()] = o.Seconds()
	}
	for _, step := range append([]string{"proto"}, Targets...) {
		want := 10.0
		switch step {
		case "proto":
			want = 2
		case "webapp":
			want = 7
		}
		if timed[step] != want {
			t.Fatalf("step %s timed %vs, want %vs: %v", step, timed[step], want, timed)
		}
	}
}

func TestAFailedBuildStepIsTimedAsFailedAndItsSiblingsAsTheirOwn(t *testing.T) {
	tests := []struct {
		name   string
		answer runAnswer
	}{
		{name: "a step that exits non-zero", answer: runAnswer{out: "vite: 2 errors", code: 2}},
		{name: "a step that could not start", answer: runAnswer{err: errors.New("exec: bash: not found")}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			b, _, log := newScriptBuilder(t, map[string]runAnswer{targetStep + "webapp": tc.answer})

			// Act
			err := b.Build(context.Background(), "/staging/1")

			// Assert
			if err == nil {
				t.Fatal("Build succeeded, want the webapp step's failure")
			}
			got := b.Restarts.(*restarttimingtest.Recorder).Of(restarttiming.DeployBuildStep)
			if len(got) != 1+len(Targets) {
				t.Fatalf("build steps timed = %+v, want every step", got)
			}
			for _, o := range got {
				want := restarttiming.OK
				if o.Phase() == "webapp" {
					want = restarttiming.Failed
				}
				if o.Outcome() != want {
					t.Fatalf("step %s timed %s, want %s", o.Phase(), o.Outcome(), want)
				}
			}
			if !loggedTo(log, "error", "a build step failed") {
				t.Fatalf("no error record for the failed step: %v", log.Records())
			}
		})
	}
}
