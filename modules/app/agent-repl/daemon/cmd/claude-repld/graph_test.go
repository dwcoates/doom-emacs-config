package main

import (
	"errors"
	"path/filepath"
	"reflect"
	"strings"

	"claude-repld/internal/dlog"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/resolve/topbar"
	"claude-repld/internal/wsm"

	"claude-repld/internal/merge"
	"claude-repld/internal/rollout"

	"claude-repld/internal/boot"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/workspace"
	"testing"
	"time"
)

func TestAnUnsetHoldoutWarnCadenceLeavesTheDefaultToTheController(t *testing.T) {
	// Arrange.
	t.Setenv(HoldoutWarnEnv, "")

	// Act.
	got, err := resolveHoldoutWarnEvery()

	// Assert.
	if err != nil || got != 0 {
		t.Fatalf("resolveHoldoutWarnEvery() = (%v, %v), want (0, nil) so the controller's default stands", got, err)
	}
}

func TestTheHoldoutWarnCadenceComesFromTheEnvironment(t *testing.T) {
	// Arrange.
	t.Setenv(HoldoutWarnEnv, "250ms")

	// Act.
	got, err := resolveHoldoutWarnEvery()

	// Assert.
	if err != nil || got != 250*time.Millisecond {
		t.Fatalf("resolveHoldoutWarnEvery() = (%v, %v), want 250ms", got, err)
	}
}

func TestAMalformedHoldoutWarnCadenceIsRefused(t *testing.T) {
	// Arrange.
	t.Setenv(HoldoutWarnEnv, "soon")

	// Act.
	_, err := resolveHoldoutWarnEvery()

	// Assert.
	if err == nil {
		t.Fatal("resolveHoldoutWarnEvery() accepted a malformed cadence; a silent test knob makes its suite lie")
	}
}

func TestANonPositiveHoldoutWarnCadenceIsRefused(t *testing.T) {
	// Arrange.
	t.Setenv(HoldoutWarnEnv, "0s")

	// Act.
	_, err := resolveHoldoutWarnEvery()

	// Assert.
	if err == nil {
		t.Fatal("resolveHoldoutWarnEvery() accepted a non-positive cadence")
	}
}

// TestTheShimOnlyFakeHookCanOnlyTurnFakeOn pins the hook's one-way nature: a
// production spawn is never made LESS fake than the contract says it is.
func TestTheShimOnlyFakeHookCanOnlyTurnFakeOn(t *testing.T) {
	// Arrange, Act, Assert.
	for _, tc := range []struct {
		name  string
		value string
		want  bool
	}{
		{name: "unset", value: "", want: false},
		{name: "zero", value: "0", want: false},
		{name: "false", value: "false", want: false},
		{name: "one", value: "1", want: true},
		{name: "anything else", value: "yes", want: true},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Setenv(FakeShimsEnv, tc.value)
			if got := fakeShims(); got != tc.want {
				t.Fatalf("fakeShims() with %s=%q = %v, want %v", FakeShimsEnv, tc.value, got, tc.want)
			}
		})
	}
}

// TestAdoptedDeathWitnessReportsAFreeLockAsDeath pins that a workspace lock
// which reads free is the evidence that stops the redial loop.
func TestAdoptedDeathWitnessReportsAFreeLockAsDeath(t *testing.T) {
	// Arrange.
	witness := adoptedDeathWitness(func(string) (sessionlock.State, error) {
		return sessionlock.StateFree, nil
	})

	// Act.
	free, err := witness("/w")

	// Assert.
	if err != nil || !free {
		t.Fatalf("witness() = %t, %v, want true and no error", free, err)
	}
}

// TestAdoptedDeathWitnessReportsAHeldLockAsAlive pins that a lock a shim still
// holds is never read as a death.
func TestAdoptedDeathWitnessReportsAHeldLockAsAlive(t *testing.T) {
	// Arrange.
	witness := adoptedDeathWitness(func(string) (sessionlock.State, error) {
		return sessionlock.StateHeld, nil
	})

	// Act.
	free, err := witness("/w")

	// Assert.
	if err != nil || free {
		t.Fatalf("witness() = %t, %v, want false and no error", free, err)
	}
}

// TestAdoptedDeathWitnessSurfacesACouldNotTellProbe pins the boot rule: a probe
// that could not tell is never read as free, and its error is surfaced rather
// than swallowed into a "not free" answer.
func TestAdoptedDeathWitnessSurfacesACouldNotTellProbe(t *testing.T) {
	// Arrange.
	witness := adoptedDeathWitness(func(string) (sessionlock.State, error) {
		return sessionlock.StateUnknown, errors.New("permission denied")
	})

	// Act.
	free, err := witness("/w")

	// Assert.
	if free || err == nil {
		t.Fatalf("witness() = %t, %v, want false and the probe error", free, err)
	}
}

// TestResolveAdoptBoundDefaultsToTheProductionBound pins that an unset knob
// leaves the production last resort in force.
func TestResolveAdoptBoundDefaultsToTheProductionBound(t *testing.T) {
	// Arrange, Act.
	got, err := resolveAdoptBound("")

	// Assert.
	if err != nil {
		t.Fatalf("resolveAdoptBound(\"\") = error %v", err)
	}
	if got != boot.DefaultAdoptBound {
		t.Fatalf("resolveAdoptBound(\"\") = %v, want %v", got, boot.DefaultAdoptBound)
	}
}

// TestResolveAdoptBoundRefusesAMalformedValue pins that a knob which cannot be
// read is a refusal: a run that silently ignored it would report a bound it
// never used.
func TestResolveAdoptBoundRefusesAMalformedValue(t *testing.T) {
	// Arrange, Act.
	_, err := resolveAdoptBound("soon")

	// Assert.
	if err == nil {
		t.Fatalf("resolveAdoptBound(\"soon\") = nil error, want a refusal")
	}
}

// TestResolveAdoptBoundRefusesANonPositiveValue pins the other refusal: a zero
// bound would make every adoption overrun before it began.
func TestResolveAdoptBoundRefusesANonPositiveValue(t *testing.T) {
	// Arrange, Act.
	_, err := resolveAdoptBound("0s")

	// Assert.
	if err == nil {
		t.Fatalf("resolveAdoptBound(\"0s\") = nil error, want a refusal")
	}
}

// TestResolveAdoptBoundReadsADuration pins the ordinary case.
func TestResolveAdoptBoundReadsADuration(t *testing.T) {
	// Arrange, Act.
	got, err := resolveAdoptBound("250ms")

	// Assert.
	if err != nil {
		t.Fatalf("resolveAdoptBound(\"250ms\") = error %v", err)
	}
	if got != 250*time.Millisecond {
		t.Fatalf("resolveAdoptBound(\"250ms\") = %v, want 250ms", got)
	}
}

// TestResolveStartBoundDefaultsToTheProductionWindow pins that an unset knob
// leaves the production window in force.
func TestResolveStartBoundDefaultsToTheProductionWindow(t *testing.T) {
	// Arrange, Act.
	got, err := resolveStartBound("")

	// Assert.
	if err != nil {
		t.Fatalf("resolveStartBound(\"\") = error %v", err)
	}
	if got != workspace.DefaultStartSessionBound {
		t.Fatalf("resolveStartBound(\"\") = %v, want %v", got, workspace.DefaultStartSessionBound)
	}
}

// TestResolveStartBoundRefusesAMalformedValue pins the same refusal the
// adoption bound makes: a knob that silently did nothing would make the run it
// was set for report a bound it never used.
func TestResolveStartBoundRefusesAMalformedValue(t *testing.T) {
	// Arrange, Act.
	_, err := resolveStartBound("presently")

	// Assert.
	if err == nil {
		t.Fatalf("resolveStartBound(\"presently\") = nil error, want a refusal")
	}
}

// TestResolveStartBoundRefusesANonPositiveValue pins the other refusal: a zero
// bound would expire every start before the shim was asked.
func TestResolveStartBoundRefusesANonPositiveValue(t *testing.T) {
	// Arrange, Act.
	_, err := resolveStartBound("0s")

	// Assert.
	if err == nil {
		t.Fatalf("resolveStartBound(\"0s\") = nil error, want a refusal")
	}
}

// TestResolveStartBoundReadsADuration pins the ordinary case.
func TestResolveStartBoundReadsADuration(t *testing.T) {
	// Arrange, Act.
	got, err := resolveStartBound("250ms")

	// Assert.
	if err != nil {
		t.Fatalf("resolveStartBound(\"250ms\") = error %v", err)
	}
	if got != 250*time.Millisecond {
		t.Fatalf("resolveStartBound(\"250ms\") = %v, want 250ms", got)
	}
}

func TestResolveSelfRepo(t *testing.T) {
	tests := []struct {
		name    string
		flag    string
		env     string
		root    string
		want    string
		wantErr string
	}{
		{name: "the flag wins over the environment", flag: "/flag", env: "/env", root: "/repo/modules/app/agent-repl", want: "/flag"},
		{name: "the environment wins over the checkout", env: "/env", root: "/repo/modules/app/agent-repl", want: "/env"},
		{name: "the default is the repository root, not the module root", root: "/repo/modules/app/agent-repl", want: "/repo"},
		{name: "an override lets an unmarked checkout boot", env: "/env", root: "/pinned", want: "/env"},
		{name: "an unmarked checkout with no override is refused", root: "/pinned", wantErr: "resolving the self repository"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			t.Setenv(envSelfRepo, tt.env)

			// Act.
			got, err := resolveSelfRepo(options{selfRepo: tt.flag}, tt.root)

			// Assert.
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("resolveSelfRepo() = (%q, %v), want an error containing %q", got, err, tt.wantErr)
				}
				return
			}
			if err != nil || got != tt.want {
				t.Fatalf("resolveSelfRepo() = (%q, %v), want %q", got, err, tt.want)
			}
		})
	}
}

// The merge gate's script is the checkout's own bin/test-all.sh: the default
// self repository joined by merge.TestCommandFor must land beneath the module
// root exactly once, never at modules/app/agent-repl/modules/app/agent-repl.
func TestTheDefaultSelfRepoRunsTheCheckoutsOwnTestAll(t *testing.T) {
	// Arrange.
	t.Setenv(envSelfRepo, "")
	t.Setenv("AGENT_REPL_TEST_ALL_SCRIPT", "")
	root := filepath.Join("/repo", "modules", "app", "agent-repl")
	selfRepo, err := resolveSelfRepo(options{}, root)
	if err != nil {
		t.Fatalf("resolveSelfRepo() error = %v", err)
	}

	// Act.
	argv := merge.TestCommandFor(selfRepo)

	// Assert.
	if want := filepath.Join(root, "bin", "test-all.sh"); argv[len(argv)-1] != want {
		t.Fatalf("TestCommandFor(%q) = %v, want the script at %q", selfRepo, argv, want)
	}
}

func TestResolveFactsBound(t *testing.T) {
	tests := []struct {
		name  string
		value string
		want  time.Duration
	}{
		{name: "blank is the rollout's default", value: "", want: rollout.DefaultFactsBound},
		{name: "a set value is taken", value: "200ms", want: 200 * time.Millisecond},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act
			got, err := resolveFactsBound(tc.value)

			// Assert
			if err != nil || got != tc.want {
				t.Fatalf("resolveFactsBound(%q) = (%v, %v), want %v", tc.value, got, err, tc.want)
			}
		})
	}
}

// recordingFooter records the faults the sink drew on the footer.
type recordingFooter struct {
	footer.Resolver
	opened []string
	closed []string
}

func (f *recordingFooter) OpenFault(_ ids.WorkspaceID, fault footer.Fault) {
	f.opened = append(f.opened, fault.ID)
}

func (f *recordingFooter) CloseFault(_ ids.WorkspaceID, id string) {
	f.closed = append(f.closed, id)
}

// recordingTopbar records the daemon-scoped warnings the sink raised.
type recordingTopbar struct {
	topbar.Resolver
	raised    map[string]string
	warnings  map[string]topbar.DaemonWarning
	retracted []string
}

func (t *recordingTopbar) RaiseDaemonWarning(key string, w topbar.DaemonWarning) {
	t.raised[key] = w.Line
	if t.warnings != nil {
		t.warnings[key] = w
	}
}

func (t *recordingTopbar) RetractDaemonWarning(key string) { t.retracted = append(t.retracted, key) }

func TestTheFaultSinkDrawsEachFaultWhereHealthSays(t *testing.T) {
	tests := []struct {
		name       string
		ws         ids.WorkspaceID
		line       health.FaultLine
		wantTopbar map[string]string
	}{
		{"a daemon-scoped fault with a topbar line stands on the footer and the topbar",
			"", health.FaultLine{ID: "f-1", Kind: health.KindDeployFailed, Topbar: "deploy failed: build: x"},
			map[string]string{"f-1": "deploy failed: build: x"}},
		{"a daemon-scoped fault with no topbar line stands on the footer alone",
			"", health.FaultLine{ID: "f-2", Kind: health.KindPromptsDirMissing}, map[string]string{}},
		{"a workspace fault never reaches the daemon's topbar set",
			"ws-1", health.FaultLine{ID: "f-3", Kind: health.KindDeployFailed, Topbar: "deploy failed: build: x"},
			map[string]string{}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := &recordingFooter{}
			tb := &recordingTopbar{raised: map[string]string{}}
			sink := newFaultSurfaces(f, tb, health.NewLoudFaults(dlog.NewTestLogger()))

			// Act
			sink.FaultOpened(tc.ws, tc.line)

			// Assert
			if len(f.opened) != 1 || f.opened[0] != string(tc.line.ID) {
				t.Fatalf("footer opened = %v, want the one fault", f.opened)
			}
			if !reflect.DeepEqual(tb.raised, tc.wantTopbar) {
				t.Fatalf("topbar raised = %v, want %v", tb.raised, tc.wantTopbar)
			}
		})
	}
}

func TestTheFaultSinkRetractsFromTheTopbarOnlyWhatItRaisedThere(t *testing.T) {
	tests := []struct {
		name          string
		line          health.FaultLine
		wantRetracted []string
	}{
		{"a fault raised on the topbar is retracted from it",
			health.FaultLine{ID: "f-1", Kind: health.KindDeployFailed, Topbar: "deploy failed: build: x"}, []string{"f-1"}},
		{"a fault the topbar never carried is retracted from the footer alone",
			health.FaultLine{ID: "f-2", Kind: health.KindPromptsDirMissing}, nil},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := &recordingFooter{}
			tb := &recordingTopbar{raised: map[string]string{}}
			sink := newFaultSurfaces(f, tb, health.NewLoudFaults(dlog.NewTestLogger()))
			sink.FaultOpened("", tc.line)

			// Act
			sink.FaultClosed("", tc.line.ID)

			// Assert
			if len(f.closed) != 1 || f.closed[0] != string(tc.line.ID) {
				t.Fatalf("footer closed = %v, want the one fault", f.closed)
			}
			if !reflect.DeepEqual(tb.retracted, tc.wantRetracted) {
				t.Fatalf("topbar retracted = %v, want %v", tb.retracted, tc.wantRetracted)
			}
		})
	}
}

func TestTheFaultSinkGivesAFailedDeploysTopbarRowItsOverlay(t *testing.T) {
	tests := []struct {
		name string
		line health.FaultLine
		want *topbar.DeployFailedOverlay
	}{
		{"a failed deploy's row opens what failed",
			health.FaultLine{ID: "f-1", Kind: health.KindDeployFailed, Topbar: "deploy failed: build webapp: tsc",
				Record: wsm.Fault{Kind: health.KindDeployFailed, Evidence: health.DeployFailure{
					Step: health.DeployStepBuild, BuildStep: "webapp", Detail: "tsc", Log: "/s/build.log"}.Evidence()}},
			&topbar.DeployFailedOverlay{Step: "build", Component: "webapp", Rollback: "nothing was installed", Detail: "tsc", Log: "/s/build.log"}},
		{"a failed deploy whose record names no step is its line alone",
			health.FaultLine{ID: "f-2", Kind: health.KindDeployFailed, Topbar: "deploy failed: prose",
				Record: wsm.Fault{Kind: health.KindDeployFailed, Detail: "prose"}},
			nil},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			tb := &recordingTopbar{raised: map[string]string{}, warnings: map[string]topbar.DaemonWarning{}}
			sink := newFaultSurfaces(&recordingFooter{}, tb, health.NewLoudFaults(dlog.NewTestLogger()))

			// Act
			sink.FaultOpened("", tc.line)

			// Assert
			if got := tb.warnings[string(tc.line.ID)].DeployFailed; !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("overlay = %+v, want %+v", got, tc.want)
			}
		})
	}
}

// loudIDs answers the standing loud faults' ids, nil when none was ever
// published.
func loudIDs(l *health.LoudFaults) []string {
	latest, ok := l.Topic().Latest()
	if !ok {
		return nil
	}
	out := []string{}
	for _, f := range latest.GetFaults() {
		out = append(out, f.GetFaultId())
	}
	return out
}

// failedBuildLine is a failed deploy's daemon-scoped fault line.
func failedBuildLine(id ids.FaultID) health.FaultLine {
	record := wsm.Fault{Kind: health.KindDeployFailed, Evidence: health.DeployFailure{
		Step: health.DeployStepBuild, BuildStep: "webapp", Detail: "tsc"}.Evidence()}
	return health.FaultLine{ID: id, Kind: health.KindDeployFailed, Record: record, Topbar: health.FaultTopbarLine(record, true)}
}

func TestTheFaultSinkTellsEmacsExactlyTheTopbarsFaults(t *testing.T) {
	tests := []struct {
		name string
		ws   ids.WorkspaceID
		line health.FaultLine
		want []string
	}{
		{"a failed deploy is told", "", failedBuildLine("f-1"), []string{"f-1"}},
		{"a daemon-scoped fault the topbar does not carry is not", "",
			health.FaultLine{ID: "f-2", Kind: health.KindPromptsDirMissing}, nil},
		{"a workspace fault is not", "ws-1", failedBuildLine("f-3"), nil},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			loud := health.NewLoudFaults(dlog.NewTestLogger())
			sink := newFaultSurfaces(&recordingFooter{}, &recordingTopbar{raised: map[string]string{}}, loud)

			// Act
			sink.FaultOpened(tc.ws, tc.line)

			// Assert
			if got := loudIDs(loud); !reflect.DeepEqual(got, tc.want) {
				t.Fatalf("loud faults = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestTheFaultSinkRetractsAClosedFaultFromEmacs(t *testing.T) {
	// Arrange
	loud := health.NewLoudFaults(dlog.NewTestLogger())
	sink := newFaultSurfaces(&recordingFooter{}, &recordingTopbar{raised: map[string]string{}}, loud)
	sink.FaultOpened("", failedBuildLine("f-1"))

	// Act
	sink.FaultClosed("", "f-1")

	// Assert
	if got := loudIDs(loud); got == nil || len(got) != 0 {
		t.Fatalf("loud faults = %v, want the empty set published", got)
	}
}
