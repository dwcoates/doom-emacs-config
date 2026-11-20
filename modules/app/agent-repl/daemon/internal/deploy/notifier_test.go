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

func TestTheNotifierAppIsInstalledAndRestartedWhenItsRunningBuildIsStale(t *testing.T) {
	cases := []struct {
		name      string
		running   func(t *testing.T) string
		wantCalls []string
		want      OutcomeKind
	}{
		{name: "the attached app runs the fresh build: nothing restarts",
			running: func(t *testing.T) string { return hashOf(t, "notifier-2") }, want: UpToDate},
		{name: "the attached app runs an older build: it alone restarts",
			running: func(t *testing.T) string { return hashOf(t, "notifier-1") }, wantCalls: []string{"notifier"}, want: Restarted},
		{name: "no app is attached: the restart starts it",
			running: func(*testing.T) string { return "" }, wantCalls: []string{"notifier"}, want: Restarted},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h, _ := newNotifierHarness(t, tc.running(t))

			// Act
			result, err := h.d.Deploy(context.Background(), false)

			// Assert
			if err != nil {
				t.Fatalf("Deploy: %v", err)
			}
			if got := h.services.Calls(); !reflect.DeepEqual(got, tc.wantCalls) {
				t.Fatalf("restarts = %v, want %v", got, tc.wantCalls)
			}
			if got := outcome(t, result, ComponentNotifier).Kind; got != tc.want {
				t.Fatalf("notifier = %s, want %s", got, tc.want)
			}
			if got := readFile(t, filepath.Join(h.live.NotifierApp(), NotifierExecutable)); got != "notifier-2" {
				t.Fatalf("installed notifier = %q, want the fresh build", got)
			}
			if got := readFile(t, filepath.Join(h.live.NotifierApp(), "Contents", "Info.plist")); got != "plist notifier-2" {
				t.Fatalf("installed bundle's plist = %q, want the fresh bundle whole", got)
			}
		})
	}
}

func TestTheNotifierTellsItsHotReload(t *testing.T) {
	cases := []struct {
		name    string
		running func(t *testing.T) string
		want    []string
	}{
		{name: "the attached app runs the fresh build",
			running: func(t *testing.T) string { return hashOf(t, "notifier-2") },
			want:    []string{"built:9", "current:test service", "current:notifier", "daemon_current"}},
		{name: "the attached app runs an older build",
			running: func(t *testing.T) string { return hashOf(t, "notifier-1") },
			want:    []string{"built:9", "current:test service", "restarting:notifier", "restarted:notifier", "daemon_current"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h, _ := newNotifierHarness(t, tc.running(t))

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

func TestAFailedNotifierRestartIsTheDeploysAnswerAndItsRollbackRestartsItAgain(t *testing.T) {
	// Arrange
	h, _ := newNotifierHarness(t, "")
	h.services.notifierErr = errors.New("kickstart refused")
	h.services.recovers = true

	// Act
	_, err := h.d.Deploy(context.Background(), false)

	// Assert
	var failed *ServiceRestartFailed
	if !errors.As(err, &failed) || failed.Component != ComponentNotifier {
		t.Fatalf("Deploy = %v, want the notifier's restart failure", err)
	}
	if got := h.services.Calls(); !reflect.DeepEqual(got, []string{"notifier", "notifier"}) {
		t.Fatalf("restarts = %v, want the failed restart and the rollback's", got)
	}
	if got := readFile(t, filepath.Join(h.live.NotifierApp(), NotifierExecutable)); got != "notifier-1" {
		t.Fatalf("installed notifier = %q, want the rollback's restored build", got)
	}
}

func TestAPlatformWithNoNotifierDeploysNone(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	result, err := h.d.Deploy(context.Background(), false)

	// Assert
	if err != nil {
		t.Fatalf("Deploy: %v", err)
	}
	for _, o := range result.Outcomes {
		if o.Component == ComponentNotifier {
			t.Fatalf("outcomes carry the notifier %+v on a platform with none", o)
		}
	}
}

func TestTheNotifierComponentHasItsWireArm(t *testing.T) {
	if got := ComponentNotifier.Arm(); got != agentreplv1.DeployComponent_DEPLOY_COMPONENT_NOTIFIER {
		t.Fatalf("Arm = %v, want DEPLOY_COMPONENT_NOTIFIER", got)
	}
}

func TestTheBuilderStagesTheNotifierOnlyWhenAsked(t *testing.T) {
	cases := []struct {
		notifier bool
		want     bool
	}{{notifier: false, want: false}, {notifier: true, want: true}}
	for _, tc := range cases {
		// Arrange
		b := &ScriptBuilder{ModuleRoot: "/m", Notifier: tc.notifier}

		// Act
		steps := b.steps("/staging")

		// Assert
		last := steps[len(steps)-1]
		got := last.name == NotifierTarget && reflect.DeepEqual(last.argv,
			[]string{"bash", "/m/bin/build-frontend.sh", "--out", "/staging", NotifierTarget})
		if got != tc.want {
			t.Fatalf("Notifier %v: last step %+v, want the notifier staged: %v", tc.notifier, last, tc.want)
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

func TestRestartNotifierRegistersThenRestartsTheService(t *testing.T) {
	cases := []struct {
		name              string
		registerErr       error
		restartErr        error
		notLoaded         bool
		definitionWritten bool
		defineErr         error
		wantRestart       bool
		wantStart         bool
		wantErr           string
		wantRecord        string
	}{
		{name: "a loaded service is restarted", wantRestart: true},
		{name: "a failed registration restarts nothing", registerErr: errors.New("lsregister died"),
			wantErr: "lsregister died", wantRecord: "registering the notifier app with LaunchServices failed"},
		{name: "a failed restart is its error", restartErr: errors.New("no such label"), wantRestart: true,
			wantErr: "no such label", wantRecord: "the notifier app restart failed"},
		{name: "an unloaded service whose definition is installed is started, not restarted",
			notLoaded: true, definitionWritten: true, wantStart: true},
		{name: "an unloaded service with no definition has it written, then is started", notLoaded: true, wantStart: true},
		{name: "a definition that cannot be written is the error", notLoaded: true, defineErr: errors.New("no entry"),
			wantErr: "no entry", wantRecord: "a service's definition could not be written from its requirements entry"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			log := dlog.NewTestLogger()
			dir := t.TempDir()
			if tc.definitionWritten {
				writeFile(t, filepath.Join(dir, NotifierLabel+".plist"), "<plist/>")
			}
			services := &restartRecorder{err: tc.restartErr, loaded: !tc.notLoaded, dir: dir}
			var registered []string
			r := &Restarter{Services: services, Log: log, NotifierApp: "/Apps/Zettel.app",
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
			err := r.RestartNotifier(context.Background())

			// Assert
			if !reflect.DeepEqual(registered, []string{"/Apps/Zettel.app"}) {
				t.Fatalf("registered %v", registered)
			}
			if services.restarted != tc.wantRestart || services.started != tc.wantStart {
				t.Fatalf("restarted = %v, started = %v; want restarted %v, started %v", services.restarted, services.started, tc.wantRestart, tc.wantStart)
			}
			if (tc.wantErr == "") != (err == nil) || (err != nil && !strings.Contains(err.Error(), tc.wantErr)) {
				t.Fatalf("RestartNotifier = %v, want %q", err, tc.wantErr)
			}
			if tc.wantRecord != "" {
				if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Message: tc.wantRecord}); !ok {
					t.Fatalf("no error record %q", tc.wantRecord)
				}
			}
		})
	}
}

// restartRecorder is a ServiceManager holding the notifier's service loaded
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
	if label != NotifierLabel {
		panic("restartRecorder: status of " + label)
	}
	return k.loaded, 0, nil
}

func (k *restartRecorder) Definition(label string) string {
	return filepath.Join(k.dir, label+".plist")
}

func (k *restartRecorder) Start(_ context.Context, label string) error {
	if label != NotifierLabel {
		panic("restartRecorder: start of " + label)
	}
	k.started = true
	return nil
}

func (k *restartRecorder) Restart(_ context.Context, label string) error {
	if label != NotifierLabel {
		panic("restartRecorder: restart of " + label)
	}
	k.restarted = true
	return k.err
}
