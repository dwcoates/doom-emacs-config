package deploy

import (
	"context"
	"errors"
	"path/filepath"
	"reflect"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/dlog"
)

func TestTheLauncherIsInstalledAndRestartedWhenItsReportedBuildIsStale(t *testing.T) {
	cases := []struct {
		name      string
		running   func(t *testing.T) string
		wantCalls []string
		want      OutcomeKind
	}{
		{name: "the reported launcher runs the fresh build: nothing restarts",
			running: func(t *testing.T) string { return hashOf(t, "launcher-2") }, want: UpToDate},
		{name: "the reported launcher runs an older build: it alone restarts",
			running: func(t *testing.T) string { return hashOf(t, "launcher-1") }, wantCalls: []string{"launcher"}, want: Restarted},
		{name: "no launcher reports a build: the restart starts it",
			running: func(*testing.T) string { return "" }, wantCalls: []string{"launcher"}, want: Restarted},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newLauncherHarness(t, tc.running(t))

			// Act
			result, err := h.d.Deploy(context.Background(), false)

			// Assert
			if err != nil {
				t.Fatalf("Deploy: %v", err)
			}
			if got := h.services.Calls(); !reflect.DeepEqual(got, tc.wantCalls) {
				t.Fatalf("restarts = %v, want %v", got, tc.wantCalls)
			}
			if got := outcome(t, result, ComponentLauncher).Kind; got != tc.want {
				t.Fatalf("launcher = %s, want %s", got, tc.want)
			}
			if got := readFile(t, filepath.Join(h.live.LauncherApp(), LauncherExecutable)); got != "launcher-2" {
				t.Fatalf("installed launcher = %q, want the fresh build", got)
			}
			if got := readFile(t, filepath.Join(h.live.LauncherApp(), "Contents", "Info.plist")); got != "plist launcher-2" {
				t.Fatalf("installed bundle's plist = %q, want the fresh bundle whole", got)
			}
		})
	}
}

func TestTheLauncherTellsItsHotReload(t *testing.T) {
	cases := []struct {
		name    string
		running func(t *testing.T) string
		want    []string
	}{
		{name: "the reported launcher runs the fresh build",
			running: func(t *testing.T) string { return hashOf(t, "launcher-2") },
			want:    []string{"built:9", "current:test service", "current:launcher", "daemon_current"}},
		{name: "the reported launcher runs an older build",
			running: func(t *testing.T) string { return hashOf(t, "launcher-1") },
			want:    []string{"built:9", "current:test service", "restarting:launcher", "restarted:launcher", "daemon_current"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newLauncherHarness(t, tc.running(t))

			// Act
			if _, err := h.d.Deploy(context.Background(), false); err != nil {
				t.Fatalf("Deploy: %v", err)
			}

			// Assert
			if told := h.hotReload.told(); !toldAll(told, tc.want) {
				t.Fatalf("told %v, want %v in order", told, tc.want)
			}
		})
	}
}

func TestAFailedLauncherRestartIsTheDeploysAnswerAndItsRollbackRestartsItAgain(t *testing.T) {
	// Arrange
	h := newLauncherHarness(t, "")
	h.services.launcherErr = errors.New("kickstart refused")
	h.services.recovers = true

	// Act
	_, err := h.d.Deploy(context.Background(), false)

	// Assert
	var failed *ServiceRestartFailed
	if !errors.As(err, &failed) || failed.Component != ComponentLauncher {
		t.Fatalf("Deploy = %v, want the launcher's restart failure", err)
	}
	if got := h.services.Calls(); !reflect.DeepEqual(got, []string{"launcher", "launcher"}) {
		t.Fatalf("restarts = %v, want the failed restart and the rollback's", got)
	}
	if got := readFile(t, filepath.Join(h.live.LauncherApp(), LauncherExecutable)); got != "launcher-1" {
		t.Fatalf("installed launcher = %q, want the rollback's restored build", got)
	}
}

func TestAPlatformWithNoLauncherDeploysNone(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	result, err := h.d.Deploy(context.Background(), false)

	// Assert
	if err != nil {
		t.Fatalf("Deploy: %v", err)
	}
	for _, o := range result.Outcomes {
		if o.Component == ComponentLauncher {
			t.Fatalf("outcomes carry the launcher %+v on a platform with none", o)
		}
	}
}

func TestTheLauncherComponentHasItsWireArm(t *testing.T) {
	if got := ComponentLauncher.Arm(); got != agentreplv1.DeployComponent_DEPLOY_COMPONENT_LAUNCHER {
		t.Fatalf("Arm = %v, want DEPLOY_COMPONENT_LAUNCHER", got)
	}
}

func TestTheBuilderStagesTheLauncherOnlyWhenAsked(t *testing.T) {
	cases := []struct {
		launcher bool
		want     bool
	}{{launcher: false, want: false}, {launcher: true, want: true}}
	for _, tc := range cases {
		// Arrange
		b := &ScriptBuilder{ModuleRoot: "/m", Launcher: tc.launcher}

		// Act
		steps := b.steps("/staging")

		// Assert
		last := steps[len(steps)-1]
		got := last.name == LauncherTarget && reflect.DeepEqual(last.argv,
			[]string{"bash", "/m/bin/build-frontend.sh", "--out", "/staging", LauncherTarget})
		if got != tc.want {
			t.Fatalf("Launcher %v: last step %+v, want the launcher staged: %v", tc.launcher, last, tc.want)
		}
	}
}

// lsregisterRunner answers one scripted run and records its argv.
type lsregisterRunner struct {
	argv []string
	out  string
	code int
	err  error
}

func (r *lsregisterRunner) Run(_ context.Context, _ string, argv []string) (string, int, error) {
	r.argv = argv
	return r.out, r.code, r.err
}

func TestLSRegisterRegistersTheBundle(t *testing.T) {
	cases := []struct {
		name    string
		runner  *lsregisterRunner
		wantErr string
	}{
		{name: "registered", runner: &lsregisterRunner{}},
		{name: "a nonzero exit is its words", runner: &lsregisterRunner{out: "bad bundle\n", code: 3}, wantErr: "exited 3: bad bundle"},
		{name: "a run that could not start", runner: &lsregisterRunner{err: errors.New("no such file")}, wantErr: "no such file"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			err := LSRegister{Binary: "/bin/lsregister", Runner: tc.runner}.Register(context.Background(), "/Apps/Zettel.app")

			// Assert
			if !reflect.DeepEqual(tc.runner.argv, []string{"/bin/lsregister", "-f", "/Apps/Zettel.app"}) {
				t.Fatalf("argv = %v", tc.runner.argv)
			}
			if (tc.wantErr == "") != (err == nil) || (err != nil && !strings.Contains(err.Error(), tc.wantErr)) {
				t.Fatalf("Register = %v, want %q", err, tc.wantErr)
			}
		})
	}
}

func TestRestartLauncherRegistersThenRestartsTheService(t *testing.T) {
	cases := []struct {
		name              string
		noHolder          bool
		holderErr         error
		registerErr       error
		restartErr        error
		notLoaded         bool
		definitionWritten bool
		defineErr         error
		wantRegistered    bool
		wantRestart       bool
		wantStart         bool
		wantErr           string
		wantRecord        string
	}{
		{name: "a loaded service is restarted", wantRegistered: true, wantRestart: true},
		{name: "a socket no declared holder holds ensures no holder", noHolder: true, wantRegistered: true, wantRestart: true},
		{name: "a socket holder that cannot be ensured registers and restarts nothing", holderErr: errors.New("holder refused"),
			wantErr: "holder refused"},
		{name: "a failed registration restarts nothing", registerErr: errors.New("lsregister died"), wantRegistered: true,
			wantErr: "lsregister died", wantRecord: "registering the launcher with LaunchServices failed"},
		{name: "a failed restart is its error", restartErr: errors.New("no such label"), wantRegistered: true, wantRestart: true,
			wantErr: "no such label", wantRecord: "the launcher restart failed"},
		{name: "an unloaded service whose definition is installed is started, not restarted",
			notLoaded: true, definitionWritten: true, wantRegistered: true, wantStart: true},
		{name: "an unloaded service with no definition has it written, then is started", notLoaded: true, wantRegistered: true, wantStart: true},
		{name: "a definition that cannot be written is the error", notLoaded: true, defineErr: errors.New("no entry"), wantRegistered: true,
			wantErr: "no entry", wantRecord: "a service's definition could not be written from its requirements entry"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			log := dlog.NewTestLogger()
			dir := t.TempDir()
			if tc.definitionWritten {
				writeFile(t, filepath.Join(dir, LauncherLabel+".plist"), "<plist/>")
			}
			services := &restartRecorder{err: tc.restartErr, loaded: !tc.notLoaded, dir: dir}
			holder, wantEnsured := "socket-holder-launcher", []string{"socket-holder-launcher"}
			if tc.noHolder {
				holder, wantEnsured = "", nil
			}
			var registered, ensured []string
			r := &Restarter{Services: services, Log: log, LauncherApp: "/Apps/Zettel.app", LauncherHolder: holder,
				EnsureHolder: func(_ context.Context, id string) error {
					ensured = append(ensured, id)
					return tc.holderErr
				},
				// Define writes the definition from its requirements entry, as
				// bin/requirements.sh define-service does.
				Define: func(_ context.Context, label string) error {
					if tc.defineErr != nil {
						return tc.defineErr
					}
					writeFile(t, filepath.Join(dir, label+".plist"), "<plist/>")
					return nil
				},
				RegisterApp: func(_ context.Context, bundle string) error {
					registered = append(registered, bundle)
					return tc.registerErr
				}}

			// Act
			err := r.RestartLauncher(context.Background())

			// Assert
			if !reflect.DeepEqual(ensured, wantEnsured) {
				t.Fatalf("ensured holders %v, want %v", ensured, wantEnsured)
			}
			if got := len(registered) == 1 && registered[0] == "/Apps/Zettel.app"; got != tc.wantRegistered {
				t.Fatalf("registered %v, want registered: %v", registered, tc.wantRegistered)
			}
			if services.restarted != tc.wantRestart || services.started != tc.wantStart {
				t.Fatalf("restarted = %v, started = %v; want restarted %v, started %v", services.restarted, services.started, tc.wantRestart, tc.wantStart)
			}
			if (tc.wantErr == "") != (err == nil) || (err != nil && !strings.Contains(err.Error(), tc.wantErr)) {
				t.Fatalf("RestartLauncher = %v, want %q", err, tc.wantErr)
			}
			if tc.wantRecord != "" {
				if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Message: tc.wantRecord}); !ok {
					t.Fatalf("no error record %q", tc.wantRecord)
				}
			}
		})
	}
}

// restartRecorder is a ServiceManager holding the launcher's service loaded
// or not, with its definition under dir, that records a restart and a start.
type restartRecorder struct {
	ServiceManager
	loaded    bool
	dir       string
	restarted bool
	started   bool
	err       error
}

func (k *restartRecorder) Status(_ context.Context, label string) (bool, int, error) {
	if label != LauncherLabel {
		panic("restartRecorder: status of " + label)
	}
	return k.loaded, 0, nil
}

func (k *restartRecorder) Definition(label string) string {
	return filepath.Join(k.dir, label+".plist")
}

func (k *restartRecorder) Start(_ context.Context, label string) error {
	if label != LauncherLabel {
		panic("restartRecorder: start of " + label)
	}
	k.started = true
	return nil
}

func (k *restartRecorder) Restart(_ context.Context, label string) error {
	if label != LauncherLabel {
		panic("restartRecorder: restart of " + label)
	}
	k.restarted = true
	return k.err
}

func TestTheLauncherInstallPlacesItsStampsBesideTheInstalledApp(t *testing.T) {
	// Arrange
	h := newLauncherHarness(t, hashOf(t, "launcher-1"))
	writeFile(t, filepath.Join(filepath.Dir(h.live.LauncherApp()), ".Zettel.app.source-tree"), "tree-launcher-1")

	// Act
	_, err := h.d.Deploy(context.Background(), false)

	// Assert
	if err != nil {
		t.Fatalf("Deploy: %v", err)
	}
	stamp := filepath.Join(h.live.Applications, ".Zettel.app.source-tree")
	if got := readFile(t, stamp); got != "tree-launcher-2" {
		t.Fatalf("launcher source-tree stamp = %q, want the fresh build's, which a staging run reuses the app by", got)
	}
}
