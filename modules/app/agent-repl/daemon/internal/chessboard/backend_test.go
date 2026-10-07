package chessboard

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"

	"claude-repld/internal/dlog"
)

// readyBackend builds a backend over a fake checkout and plugin directory,
// recording every step it announces.
func readyBackend(t *testing.T, r *fakeRunner) (*backend, string, string, *dlog.TestLogger, func() []string) {
	t.Helper()
	checkout, _ := fakeCheckout(t)
	plugin := t.TempDir()
	log := dlog.NewTestLogger()
	var mu sync.Mutex
	var steps []string
	b := &backend{
		log:       log,
		run:       r,
		getenv:    envOf(map[string]string{EngineDirEnv: checkout}),
		pluginDir: plugin,
		onStep: func(s string) {
			mu.Lock()
			steps = append(steps, s)
			mu.Unlock()
		},
		life: context.Background(),
	}
	return b, checkout, plugin, log, func() []string {
		mu.Lock()
		defer mu.Unlock()
		return append([]string(nil), steps...)
	}
}

// fullyWorking installs a runner whose every backend command succeeds.
func fullyWorking(t *testing.T, r *fakeRunner, checkout string) {
	t.Helper()
	buildsTheWidget(r, filepath.Join(checkout, cliDir))
	buildsTheWebapp(r, "webapp-v1")
	servesAt(r, "http://127.0.0.1:4000/")
}

func TestEnsureServesTheSingletonsURLAndTheWidgetBuild(t *testing.T) {
	// Arrange.
	r := newFakeRunner()
	b, checkout, _, _, _ := readyBackend(t, r)
	fullyWorking(t, r, checkout)

	// Act.
	srv, f := b.ensure(context.Background())

	// Assert.
	if f != nil || srv.baseURL != "http://127.0.0.1:4000" || srv.widgetStamp == "" ||
		srv.widgetDist != widgetDist(filepath.Join(checkout, cliDir)) {
		t.Fatalf("ensure() = %+v, %v; want the trimmed URL and the widget build", srv, f)
	}
}

func TestEnsureAnnouncesItsStepsInOrder(t *testing.T) {
	// Arrange.
	r := newFakeRunner()
	b, checkout, _, _, steps := readyBackend(t, r)
	fullyWorking(t, r, checkout)

	// Act.
	b.ensure(context.Background())

	// Assert.
	want := []string{stepGettingReady, stepBuildingWidget, stepBuildingBackend, stepStartingBackend}
	if strings.Join(steps(), "|") != strings.Join(want, "|") {
		t.Fatalf("steps = %v, want %v", steps(), want)
	}
}

func TestEnsureStatesTheCheckoutToTheSingleton(t *testing.T) {
	// Arrange.
	r := newFakeRunner()
	b, checkout, _, _, _ := readyBackend(t, r)
	fullyWorking(t, r, checkout)

	// Act.
	b.ensure(context.Background())

	// Assert.
	want := "env " + EngineDirEnv + "=" + checkout + " gns cee debug webapp"
	for _, c := range r.commands() {
		if c == want {
			return
		}
	}
	t.Fatalf("commands %v never ran %q", r.commands(), want)
}

func TestEnsureReportsAMissingCheckout(t *testing.T) {
	// Arrange.
	r := newFakeRunner()
	b, _, _, log, _ := readyBackend(t, r)
	b.getenv = envOf(nil)

	// Act.
	_, f := b.ensure(context.Background())

	// Assert.
	if f == nil || !strings.HasPrefix(f.reason, "The explanation-engine checkout was not found") {
		t.Fatalf("failure = %v, want the missing checkout", f)
	}
	assertErrorRecord(t, log, "checkout")
}

func TestEnsureReportsAMissingPlugin(t *testing.T) {
	// Arrange.
	r := newFakeRunner()
	b, checkout, _, log, _ := readyBackend(t, r)
	fullyWorking(t, r, checkout)
	b.pluginDir = filepath.Join(t.TempDir(), "absent")

	// Act.
	_, f := b.ensure(context.Background())

	// Assert.
	if f == nil || f.reason != "The gns cee plugin is not installed, so the chess widget's backend cannot run." {
		t.Fatalf("failure = %v, want the missing plugin", f)
	}
	assertErrorRecord(t, log, "webapp_install")
}

func TestEnsureInstallsANewWebappBuildAndStopsTheOldSingleton(t *testing.T) {
	// Arrange.
	r := newFakeRunner()
	b, checkout, plugin, _, _ := readyBackend(t, r)
	fullyWorking(t, r, checkout)
	writeFile(t, filepath.Join(plugin, webappBinary), "webapp-v0")

	// Act.
	_, f := b.ensure(context.Background())

	// Assert.
	installed, _ := os.ReadFile(filepath.Join(plugin, webappBinary))
	stopped := false
	for _, c := range r.commands() {
		stopped = stopped || c == "pkill -f "+filepath.Join(plugin, webappBinary)
	}
	if f != nil || string(installed) != "webapp-v1" || !stopped {
		t.Fatalf("installed %q, stopped %t, failure %v; want v1 installed after a stop", installed, stopped, f)
	}
}

func TestEnsureKeepsAnInstalledWebappThatIsTheCheckoutsBuild(t *testing.T) {
	// Arrange.
	r := newFakeRunner()
	b, checkout, plugin, _, _ := readyBackend(t, r)
	fullyWorking(t, r, checkout)
	writeFile(t, filepath.Join(plugin, webappBinary), "webapp-v1")

	// Act.
	_, f := b.ensure(context.Background())

	// Assert.
	for _, c := range r.commands() {
		if strings.HasPrefix(c, "pkill") {
			t.Fatalf("an unchanged build stopped the singleton (%v)", r.commands())
		}
	}
	if _, err := os.Stat(filepath.Join(plugin, webappBuildName)); f != nil || !os.IsNotExist(err) {
		t.Fatalf("failure %v, leftover build %v; want the build removed", f, err)
	}
}

func TestEnsureReportsAStopThatFailed(t *testing.T) {
	// Arrange.
	r := newFakeRunner()
	b, checkout, _, log, _ := readyBackend(t, r)
	fullyWorking(t, r, checkout)
	r.on("pkill", func(string, []string) (string, int, error) { return "pkill: denied", 2, nil })

	// Act.
	_, f := b.ensure(context.Background())

	// Assert.
	if f == nil || f.reason != "The running chess widget backend could not be stopped for its rebuild." {
		t.Fatalf("failure = %v, want the failed stop", f)
	}
	assertErrorRecord(t, log, "webapp_install")
}

func TestEnsureReportsAFailedWebappBuild(t *testing.T) {
	// Arrange.
	r := newFakeRunner()
	b, checkout, _, log, _ := readyBackend(t, r)
	fullyWorking(t, r, checkout)
	r.on("go build", func(string, []string) (string, int, error) { return "main.go:1: syntax error", 1, nil })

	// Act.
	_, f := b.ensure(context.Background())

	// Assert.
	if f == nil || f.reason != "Building the chess widget's backend failed: main.go:1: syntax error" {
		t.Fatalf("failure = %v, want the build's last line", f)
	}
	assertErrorRecord(t, log, "webapp_build")
}

func TestEnsureReportsASingletonThatWouldNotStart(t *testing.T) {
	// Arrange.
	r := newFakeRunner()
	b, checkout, _, log, _ := readyBackend(t, r)
	fullyWorking(t, r, checkout)
	r.on("env", func(string, []string) (string, int, error) { return "cee-webapp binary missing", 1, nil })

	// Act.
	_, f := b.ensure(context.Background())

	// Assert.
	if f == nil || f.reason != "The chess widget's backend could not be started: cee-webapp binary missing" {
		t.Fatalf("failure = %v, want the start failure", f)
	}
	assertErrorRecord(t, log, "webapp_start")
}

func TestEnsureReportsASingletonThatNamedNoURL(t *testing.T) {
	// Arrange.
	r := newFakeRunner()
	b, checkout, _, log, _ := readyBackend(t, r)
	fullyWorking(t, r, checkout)
	r.on("env", func(string, []string) (string, int, error) { return `{"other":1}`, 0, nil })

	// Act.
	_, f := b.ensure(context.Background())

	// Assert.
	if f == nil || f.reason != "The chess widget's backend started, but did not say where it listens." {
		t.Fatalf("failure = %v, want the missing URL", f)
	}
	assertErrorRecord(t, log, "webapp_start")
}

func TestAJoinDuringARunSharesIt(t *testing.T) {
	// Arrange. The first run blocks in its widget install until released.
	r := newFakeRunner()
	b, checkout, _, _, _ := readyBackend(t, r)
	fullyWorking(t, r, checkout)
	release := make(chan struct{})
	r.on("npm ci", func(string, []string) (string, int, error) {
		<-release
		return "", 0, nil
	})
	first := b.join()

	// Act.
	second := b.join()
	close(release)
	<-first.done

	// Assert.
	if first != second {
		t.Fatal("a join while a run was in flight started a second run")
	}
}

func TestAJoinAfterARunStartsAFreshOne(t *testing.T) {
	// Arrange.
	r := newFakeRunner()
	b, checkout, _, _, _ := readyBackend(t, r)
	fullyWorking(t, r, checkout)
	first := b.join()
	<-first.done

	// Act.
	second := b.join()
	<-second.done

	// Assert.
	if first == second {
		t.Fatal("a join after the run ended reused the finished run")
	}
}
