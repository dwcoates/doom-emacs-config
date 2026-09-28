package health

import (
	"context"
	"errors"
	"testing"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// errStub is what every unstubbed fake call answers with, so a test that
// reaches a method it did not arrange fails loudly instead of observing a zero
// value.
var errStub = errors.New("health test: unstubbed call")

// stubDB is a wsm.DB whose unused methods panic on use. Embedding the
// interface rather than spelling fifty methods keeps each test's arrangement
// to exactly the calls it cares about; a call outside that set nil-panics,
// which is the loud failure the test wants.
type stubDB struct {
	wsm.DB

	workspaces   map[ids.WorkspaceID]wsm.Workspace
	workspaceErr error

	faults    []wsm.Fault
	faultsErr error
	scopes    []wsm.FaultScope

	openedFault wsm.Fault
	openedID    ids.FaultID
	openErr     error

	closedID ids.FaultID
	closedAt time.Time
	closeErr error
}

func (d *stubDB) Workspace(_ context.Context, id ids.WorkspaceID) (wsm.Workspace, error) {
	if d.workspaceErr != nil {
		return wsm.Workspace{}, d.workspaceErr
	}
	w, ok := d.workspaces[id]
	if !ok {
		return wsm.Workspace{}, errors.New("no such workspace")
	}
	return w, nil
}

func (d *stubDB) OpenFaults(_ context.Context, scope wsm.FaultScope) ([]wsm.Fault, error) {
	d.scopes = append(d.scopes, scope)
	if d.faultsErr != nil {
		return nil, d.faultsErr
	}
	return d.faults, nil
}

func (d *stubDB) OpenFault(_ context.Context, f wsm.Fault) (ids.FaultID, error) {
	if d.openErr != nil {
		return "", d.openErr
	}
	d.openedFault = f
	return d.openedID, nil
}

func (d *stubDB) CloseFault(_ context.Context, id ids.FaultID, at time.Time) error {
	if d.closeErr != nil {
		return d.closeErr
	}
	d.closedID, d.closedAt = id, at
	return nil
}

// stubSurfaces is a dlog.Surfaces backed by one capturing TestLogger, so a
// test can assert both the answer and the record the branch left behind.
type stubSurfaces struct {
	logger *dlog.TestLogger
	// workspaceErr makes the per-workspace sink unresolvable, which is the
	// invariant violation the session answer surfaces rather than papers over.
	workspaceErr error
}

func newStubSurfaces() *stubSurfaces {
	return &stubSurfaces{logger: dlog.NewTestLogger()}
}

func (s *stubSurfaces) Global() dlog.Logger { return s.logger }

func (s *stubSurfaces) Workspace(dir string) (dlog.Logger, error) {
	if s.workspaceErr != nil {
		return nil, s.workspaceErr
	}
	return s.logger.With(dlog.Context{"dir": dir}), nil
}

// WorkspaceOrCentral implements dlog.Surfaces: the workspace's logger when it
// resolves, and the global one when it does not.
func (s *stubSurfaces) WorkspaceOrCentral(dir string) dlog.Logger {
	log, err := s.Workspace(dir)
	if err != nil {
		return s.Global()
	}
	return log
}

func (s *stubSurfaces) ShimSink(string) (dlog.Borrowed, error) { return nil, errStub }

// BindWorkspaceIDs implements dlog.Surfaces. This double answers its own
// workspace ids, so there is no lookup to install.
func (s *stubSurfaces) BindWorkspaceIDs(dlog.WorkspaceIDLookup) {}

func (s *stubSurfaces) ShimRollRequests() <-chan dlog.ShimRollRequest { return nil }

func (s *stubSurfaces) DetachDir(string) error { return nil }

func (s *stubSurfaces) AttachDir(string) error { return nil }

func (s *stubSurfaces) ClientLog(string, dlog.ClientRecord) error { return errStub }

func (s *stubSurfaces) Close() error { return nil }

// fixedNow is the instant every stamping test asserts against.
var fixedNow = time.Date(2026, 8, 29, 12, 0, 0, 0, time.UTC)

// newReporter builds a reporter over the supplied fakes, failing the test when
// construction refuses.
func newReporter(t *testing.T, db wsm.DB, live LiveFunc, log dlog.Surfaces) Reporter {
	t.Helper()
	r, err := New(Deps{
		DB: db, Live: live, Log: log, Now: func() time.Time { return fixedNow },
		Instance: "daemon-test", PID: 4242,
		BuildSHA: func() (string, error) { return "test-build", nil },
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	return r
}

// alwaysLive is the liveness probe of a workspace whose session is up and
// whose link serves.
func alwaysLive(ids.WorkspaceID) (bool, bool) { return true, true }

// memFaults is an in-memory fault record: the faults it holds stand until
// they are closed. It embeds wsm.DB so it can stand behind ObserveFaults;
// every method it does not spell nil-panics.
type memFaults struct {
	wsm.DB

	seq    int
	faults []wsm.Fault
	closed map[ids.FaultID]time.Time

	readErr  error
	closeErr error
}

func newMemFaults() *memFaults { return &memFaults{closed: map[ids.FaultID]time.Time{}} }

// seed records a standing fault directly and answers its id.
func (m *memFaults) seed(f wsm.Fault) ids.FaultID {
	id, _ := m.OpenFault(context.Background(), f)
	return id
}

func (m *memFaults) OpenFault(_ context.Context, f wsm.Fault) (ids.FaultID, error) {
	m.seq++
	f.ID = ids.FaultID("fault-" + string(rune('0'+m.seq)))
	m.faults = append(m.faults, f)
	return f.ID, nil
}

func (m *memFaults) CloseFault(_ context.Context, id ids.FaultID, at time.Time) error {
	if m.closeErr != nil {
		return m.closeErr
	}
	m.closed[id] = at
	return nil
}

func (m *memFaults) OpenFaults(_ context.Context, scope wsm.FaultScope) ([]wsm.Fault, error) {
	if m.readErr != nil {
		return nil, m.readErr
	}
	var out []wsm.Fault
	for _, f := range m.faults {
		if _, gone := m.closed[f.ID]; gone {
			continue
		}
		if scope.Workspace != nil && (f.Workspace == nil || *f.Workspace != *scope.Workspace) {
			continue
		}
		if scope.Kind != "" && f.Kind != scope.Kind {
			continue
		}
		out = append(out, f)
	}
	return out, nil
}

// standing answers whether a fault is still open.
func (m *memFaults) standing(id ids.FaultID) bool {
	_, gone := m.closed[id]
	return !gone
}
