package main

import (
	"context"
	"errors"
	"sync"
	"testing"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/workspace"
	"claude-repld/internal/wsm"
)

// ebFleet records the starts the editor's bring-up asks for and runs its
// detached work inline, so the test reads the outcome when the call returns.
type ebFleet struct {
	mu       sync.Mutex
	started  []ids.WorkspaceID
	startErr map[ids.WorkspaceID]error
}

func (f *ebFleet) Start(_ context.Context, ws ids.WorkspaceID) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.started = append(f.started, ws)
	return f.startErr[ws]
}

func (f *ebFleet) Detach(run func(context.Context)) { run(context.Background()) }

// ebRecords answers every workspace's record and no session record.
type ebRecords struct{}

func (ebRecords) Workspace(_ context.Context, ws ids.WorkspaceID) (wsm.Workspace, error) {
	return wsm.Workspace{ID: ws, Dir: "/tree/" + string(ws)}, nil
}

func (ebRecords) Session(context.Context, ids.WorkspaceID) (wsm.Session, bool, error) {
	return wsm.Session{}, false, nil
}

// ebOwnership answers a standing per workspace, Owned unless arranged.
type ebOwnership struct {
	mu        sync.Mutex
	standings map[ids.WorkspaceID]workspace.Standing
	errs      map[ids.WorkspaceID]error
	// after, when set, is the standing answered once a start has been asked.
	after map[ids.WorkspaceID]workspace.Standing
	fleet *ebFleet
}

func (o *ebOwnership) Standing(_ context.Context, ws ids.WorkspaceID) (workspace.Standing, error) {
	o.mu.Lock()
	defer o.mu.Unlock()
	if err := o.errs[ws]; err != nil {
		return 0, err
	}
	if standing, ok := o.after[ws]; ok && o.fleet != nil {
		o.fleet.mu.Lock()
		asked := len(o.fleet.started) > 0
		o.fleet.mu.Unlock()
		if asked {
			return standing, nil
		}
	}
	return o.standings[ws], nil
}

// ebLog is a concurrency-safe recording logger: bringup.Run starts each
// workspace on its own goroutine.
type ebLog struct {
	mu     sync.Mutex
	levels []string
}

func (l *ebLog) note(level string)                  { l.mu.Lock(); l.levels = append(l.levels, level); l.mu.Unlock() }
func (l *ebLog) Debug(string, string, dlog.Context) { l.note(dlog.LevelDebug) }
func (l *ebLog) Info(string, string, dlog.Context)  { l.note(dlog.LevelInfo) }
func (l *ebLog) Warn(string, string, dlog.Context)  { l.note(dlog.LevelWarn) }
func (l *ebLog) Error(string, string, dlog.Context) { l.note(dlog.LevelError) }
func (l *ebLog) With(dlog.Context) dlog.Logger      { return l }

func (l *ebLog) count(level string) int {
	l.mu.Lock()
	defer l.mu.Unlock()
	n := 0
	for _, got := range l.levels {
		if got == level {
			n++
		}
	}
	return n
}

// ebDone records each workspace's done report.
type ebDone struct {
	mu   sync.Mutex
	errs map[ids.WorkspaceID]error
}

func (d *ebDone) done(ws ids.WorkspaceID, err error) {
	d.mu.Lock()
	defer d.mu.Unlock()
	d.errs[ws] = err
}

func TestEditorBringUpStartsOnlyWorkspacesThisDaemonServes(t *testing.T) {
	tests := []struct {
		name        string
		standing    workspace.Standing
		standingErr error
		wantStarted bool
		wantDoneErr bool
		wantErrors  int
	}{
		{"an owned workspace is started", workspace.StandingOwned, nil, true, false, 0},
		{"a workspace transferring away is told done and not started", workspace.StandingTransferringAway, nil, false, false, 0},
		{"a workspace not yet adopted is told done and not started", workspace.StandingNotYetAdopted, nil, false, false, 0},
		{"an unreadable standing is told done with its error and not started", 0, errors.New("arranged: unreadable"), false, true, 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			fleet := &ebFleet{}
			own := &ebOwnership{
				standings: map[ids.WorkspaceID]workspace.Standing{"w1": tt.standing},
				errs:      map[ids.WorkspaceID]error{"w1": tt.standingErr},
			}
			log := &ebLog{}
			done := &ebDone{errs: map[ids.WorkspaceID]error{}}

			// Act.
			editorBringUp(fleet, ebRecords{}, own, func(ids.WorkspaceID, bool) {}, log, []ids.WorkspaceID{"w1"}, done.done)

			// Assert.
			if got := len(fleet.started) == 1; got != tt.wantStarted {
				t.Fatalf("started = %v, want started=%t", fleet.started, tt.wantStarted)
			}
			err, told := done.errs["w1"]
			if !told || (err != nil) != tt.wantDoneErr {
				t.Fatalf("done = (%v, told=%t), want told with error=%t", err, told, tt.wantDoneErr)
			}
			if got := log.count(dlog.LevelError); got != tt.wantErrors {
				t.Fatalf("ERROR records = %d, want %d", got, tt.wantErrors)
			}
		})
	}
}

func TestEditorBringUpStandsDownAStartAHandoverTook(t *testing.T) {
	// Arrange: owned when asked, transferred by the time its start fails.
	fleet := &ebFleet{startErr: map[ids.WorkspaceID]error{"w1": errors.New("arranged: already_started")}}
	own := &ebOwnership{
		standings: map[ids.WorkspaceID]workspace.Standing{"w1": workspace.StandingOwned},
		after:     map[ids.WorkspaceID]workspace.Standing{"w1": workspace.StandingTransferringAway},
		fleet:     fleet,
	}
	log := &ebLog{}
	done := &ebDone{errs: map[ids.WorkspaceID]error{}}

	// Act.
	editorBringUp(fleet, ebRecords{}, own, func(ids.WorkspaceID, bool) {}, log, []ids.WorkspaceID{"w1"}, done.done)

	// Assert.
	if done.errs["w1"] == nil || log.count(dlog.LevelError) != 0 {
		t.Fatalf("done = %v, ERROR records = %d; want the failure reported done and no ERROR", done.errs["w1"], log.count(dlog.LevelError))
	}
}

func TestEditorBringUpReportsAStartThatFailedWhileStillServed(t *testing.T) {
	// Arrange.
	fleet := &ebFleet{startErr: map[ids.WorkspaceID]error{"w1": errors.New("arranged: the shim never answered")}}
	own := &ebOwnership{standings: map[ids.WorkspaceID]workspace.Standing{"w1": workspace.StandingOwned}}
	log := &ebLog{}
	done := &ebDone{errs: map[ids.WorkspaceID]error{}}

	// Act.
	editorBringUp(fleet, ebRecords{}, own, func(ids.WorkspaceID, bool) {}, log, []ids.WorkspaceID{"w1"}, done.done)

	// Assert.
	if done.errs["w1"] == nil || log.count(dlog.LevelError) != 1 {
		t.Fatalf("done = %v, ERROR records = %d; want the failure reported and recorded at ERROR", done.errs["w1"], log.count(dlog.LevelError))
	}
}
