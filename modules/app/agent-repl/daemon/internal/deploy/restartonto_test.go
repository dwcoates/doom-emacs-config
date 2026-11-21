package deploy

import (
	"context"
	"errors"
	"os"
	"reflect"
	"regexp"
	"testing"

	"agent-repld/internal/deployprogress"
	"agent-repld/internal/dlog"
)

func TestRestartOntoStatesThePhaseRecordsTheAskAndAnswersTheOutcome(t *testing.T) {
	cases := []struct {
		name       string
		restartErr error
		wantErr    bool
		wantRecord dlog.RecordQuery
		wantTold   []string
	}{
		{name: "a restart that succeeds", wantRecord: dlog.RecordQuery{Level: "info", Message: "restarted the widget onto the fresh build"},
			wantTold: []string{"restarting:zettel", "restarted:zettel"}},
		{name: "a restart that fails", restartErr: errors.New("kickstart refused"), wantErr: true,
			wantRecord: dlog.RecordQuery{Level: "error", Message: "the widget service restart failed"},
			wantTold:   []string{"restarting:zettel"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			asked := false

			// Act
			err := h.d.restartOnto(context.Background(), ComponentZettel, "the widget service", "the widget", deployprogress.Zettel,
				func() { asked = true }, func(context.Context) error { return tc.restartErr }, dlog.Context{"widget_build": "b"})

			// Assert
			if !asked {
				t.Fatal("the ask was not recorded, so a rollback would not restart the service")
			}
			var failed *ServiceRestartFailed
			if tc.wantErr != errors.As(err, &failed) || (failed != nil && failed.Component != ComponentZettel) {
				t.Fatalf("restartOnto = %v, want a failure: %v", err, tc.wantErr)
			}
			if !logged(h.log, tc.wantRecord.Level, opDecide, tc.wantRecord.Message) {
				t.Fatalf("no %s record %q", tc.wantRecord.Level, tc.wantRecord.Message)
			}
			if got := h.progress.lastStated(); got == nil || got.Phase != deployprogress.RestartingServices ||
				len(got.Components) != 1 || got.Components[0] != deployprogress.Zettel {
				t.Fatalf("progress = %+v, want the restarting phase naming the service", got)
			}
			if got := h.hotReload.told(); !reflect.DeepEqual(got, tc.wantTold) {
				t.Fatalf("hot reload told %v, want %v", got, tc.wantTold)
			}
		})
	}
}

func TestRollbackRestartRecordsAndHandsOnItsFailure(t *testing.T) {
	cases := []struct {
		name       string
		restartErr error
		wantFailed bool
		wantRecord dlog.RecordQuery
	}{
		{name: "a restart that succeeds", wantRecord: dlog.RecordQuery{Level: "info", Message: "restarted the widget onto the restored build"}},
		{name: "a restart that fails", restartErr: errors.New("no"), wantFailed: true,
			wantRecord: dlog.RecordQuery{Level: "error", Message: "could not restart the widget onto the restored build"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			var failed []string

			// Act
			h.d.rollbackRestart(context.Background(), ComponentZettel, "the widget",
				func(context.Context) error { return tc.restartErr },
				func(c Component, what string, _ error) { failed = append(failed, string(c)+": "+what) })

			// Assert
			if tc.wantFailed != (len(failed) == 1 && failed[0] == "zettel: restart the widget") {
				t.Fatalf("failures = %v, want one: %v", failed, tc.wantFailed)
			}
			if !logged(h.log, tc.wantRecord.Level, opRollback, tc.wantRecord.Message) {
				t.Fatalf("no %s record %q", tc.wantRecord.Level, tc.wantRecord.Message)
			}
		})
	}
}

// TestEverySingleServiceRestartGoesThroughTheHelpers pins the shared shape: in
// a deploy and in its rollback, the only service restart called directly is
// the store's (which restarts the sidecar with it and so has two outcomes);
// every other goes through restartOnto or rollbackRestart.
func TestEverySingleServiceRestartGoesThroughTheHelpers(t *testing.T) {
	direct := regexp.MustCompile(`Services\.Restart(\w+)\(ctx[,)]`)
	for _, file := range []string{"deploy.go", "rollback.go"} {
		// Arrange
		src, err := os.ReadFile(file)
		if err != nil {
			t.Fatal(err)
		}

		// Act
		calls := direct.FindAllStringSubmatch(string(src), -1)

		// Assert
		for _, call := range calls {
			if call[1] != "Store" {
				t.Errorf("%s calls Services.Restart%s directly; a single service restarts through restartOnto or rollbackRestart", file, call[1])
			}
		}
	}
}

// lastStated is the last phase the deploy stated, nil when none.
func (f *fakeProgress) lastStated() *deployprogress.Progress {
	f.mu.Lock()
	defer f.mu.Unlock()
	if len(f.stated) == 0 {
		return nil
	}
	return f.stated[len(f.stated)-1]
}

// TestRestartWithRestartsThatStandaloneThroughTheServices pins the adapter
// both helpers take a standalone service's restart through: it restarts that
// service, and answers the restart's own error.
func TestRestartWithRestartsThatStandaloneThroughTheServices(t *testing.T) {
	cases := []struct {
		name    string
		failErr error
	}{
		{name: "a restart that succeeds"},
		{name: "a restart that fails", failErr: errors.New("kickstart refused")},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			services := &fakeServices{standaloneErr: map[Component]error{ComponentTests: tc.failErr}}

			// Act
			err := Tests.restartWith(services)(context.Background())

			// Assert
			if !errors.Is(err, tc.failErr) || (tc.failErr == nil && err != nil) {
				t.Fatalf("restart = %v, want %v", err, tc.failErr)
			}
			if calls := services.Calls(); len(calls) != 1 || calls[0] != string(ComponentTests) {
				t.Fatalf("calls = %v, want one restart of the test service", calls)
			}
		})
	}
}
