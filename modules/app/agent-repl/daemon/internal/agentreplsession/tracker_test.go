package agentreplsession

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/wsm"
)

// t0 is the instant every test's events are relative to.
var t0 = time.Date(2026, 10, 6, 9, 0, 0, 0, time.UTC)

// fakeStore is the durable session in memory.
type fakeStore struct {
	session *wsm.AgentReplSession
	readErr error
	putErr  error
	puts    int
}

func (s *fakeStore) AgentReplSession(context.Context) (wsm.AgentReplSession, bool, error) {
	if s.readErr != nil {
		return wsm.AgentReplSession{}, false, s.readErr
	}
	if s.session == nil {
		return wsm.AgentReplSession{}, false, nil
	}
	return *s.session, true, nil
}

func (s *fakeStore) PutAgentReplSession(_ context.Context, session wsm.AgentReplSession) error {
	s.puts++
	if s.putErr != nil {
		return s.putErr
	}
	s.session = &session
	return nil
}

// fakePublisher records every session stated.
type fakePublisher struct {
	views []*frontendv1.TopbarAgentReplSession
}

func (p *fakePublisher) SetAgentReplSession(v *frontendv1.TopbarAgentReplSession) {
	p.views = append(p.views, v)
}

// last answers the most recent view, failing when none was stated.
func (p *fakePublisher) last(t *testing.T) *frontendv1.TopbarAgentReplSession {
	t.Helper()
	if len(p.views) == 0 {
		t.Fatal("no session was stated")
	}
	return p.views[len(p.views)-1]
}

// newTracker builds a tracker over the fakes.
func newTracker(t *testing.T, store *fakeStore) (*Tracker, *fakePublisher, *dlog.TestLogger) {
	t.Helper()
	pub := &fakePublisher{}
	log := dlog.NewTestLogger()
	tr, err := New(context.Background(), store, pub, log)
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	return tr, pub, log
}

// logged reports whether a record at level was written under operation.
func logged(log *dlog.TestLogger, level, operation string) bool {
	for _, r := range log.Records() {
		if r.Level == level && r.Operation == operation {
			return true
		}
	}
	return false
}

func TestNewRefusesAMissingCollaborator(t *testing.T) {
	cases := []struct {
		name  string
		store Store
		pub   Publisher
		log   dlog.Logger
		want  string
	}{
		{"store", nil, &fakePublisher{}, dlog.NewTestLogger(), "store is required"},
		{"publisher", &fakeStore{}, nil, dlog.NewTestLogger(), "publisher is required"},
		{"logger", &fakeStore{}, &fakePublisher{}, nil, "logger is required"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			_, err := New(context.Background(), tc.store, tc.pub, tc.log)

			// Assert.
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("New error = %v, want %q", err, tc.want)
			}
		})
	}
}

func TestNewStatesNothingBeforeAnySessionBegan(t *testing.T) {
	// Act.
	_, pub, _ := newTracker(t, &fakeStore{})

	// Assert.
	if len(pub.views) != 0 {
		t.Fatalf("stated %d sessions, want none", len(pub.views))
	}
}

func TestNewCarriesThePersistedSessionAcrossADaemonRestart(t *testing.T) {
	// Arrange: the session a daemon before this one persisted.
	store := &fakeStore{session: &wsm.AgentReplSession{StartedAt: t0, Began: wsm.SessionBeganLogin}}

	// Act.
	_, pub, _ := newTracker(t, store)

	// Assert.
	got := pub.last(t)
	if got.GetStartedAtMs() != t0.UnixMilli() || got.GetLogin() == nil {
		t.Fatalf("stated %v, want the persisted login session", got)
	}
}

func TestNewRefusesAStoreThatCannotBeRead(t *testing.T) {
	// Arrange.
	log := dlog.NewTestLogger()

	// Act.
	_, err := New(context.Background(), &fakeStore{readErr: errors.New("disk gone")}, &fakePublisher{}, log)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "disk gone") {
		t.Fatalf("New error = %v, want the read failure", err)
	}
	if !logged(log, "error", "daemon.agentreplsession.load") {
		t.Fatalf("the failure was not recorded at ERROR: %v", log.Records())
	}
}

func TestAnEditorStartBeginsASession(t *testing.T) {
	// Arrange.
	store := &fakeStore{}
	tr, pub, _ := newTracker(t, store)

	// Act.
	tr.EditorStarted(context.Background(), t0)

	// Assert.
	got := pub.last(t)
	if got.GetStartedAtMs() != t0.UnixMilli() || got.GetEditorStart() == nil {
		t.Fatalf("stated %v, want an editor-start session at t0", got)
	}
	if store.session == nil || store.session.Began != wsm.SessionBeganEditorStart {
		t.Fatalf("persisted %+v, want the editor-start session", store.session)
	}
}

func TestSessionStartIsTheLaterOfLoginAndEditorStart(t *testing.T) {
	cases := []struct {
		name      string
		first     func(*Tracker)
		second    func(*Tracker)
		wantStart time.Time
		wantLogin bool
	}{
		{
			name:      "a login after the editor started begins a new session",
			first:     func(tr *Tracker) { tr.EditorStarted(context.Background(), t0) },
			second:    func(tr *Tracker) { tr.LoginCompleted(context.Background(), t0.Add(time.Hour)) },
			wantStart: t0.Add(time.Hour),
			wantLogin: true,
		},
		{
			name:      "a login stamped before the editor started leaves the editor's session",
			first:     func(tr *Tracker) { tr.EditorStarted(context.Background(), t0) },
			second:    func(tr *Tracker) { tr.LoginCompleted(context.Background(), t0.Add(-time.Hour)) },
			wantStart: t0,
			wantLogin: false,
		},
		{
			name:      "a new Emacs after a login begins a new session",
			first:     func(tr *Tracker) { tr.LoginCompleted(context.Background(), t0) },
			second:    func(tr *Tracker) { tr.EditorStarted(context.Background(), t0.Add(time.Hour)) },
			wantStart: t0.Add(time.Hour),
			wantLogin: false,
		},
		{
			name:      "an event at the standing session's own instant leaves it",
			first:     func(tr *Tracker) { tr.LoginCompleted(context.Background(), t0) },
			second:    func(tr *Tracker) { tr.EditorStarted(context.Background(), t0) },
			wantStart: t0,
			wantLogin: true,
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			tr, pub, _ := newTracker(t, &fakeStore{})
			tc.first(tr)

			// Act.
			tc.second(tr)

			// Assert.
			got := pub.last(t)
			if got.GetStartedAtMs() != tc.wantStart.UnixMilli() || (got.GetLogin() != nil) != tc.wantLogin {
				t.Fatalf("stated %v, want a session at %s (login %v)", got, tc.wantStart, tc.wantLogin)
			}
		})
	}
}

func TestAnEventTheStandingSessionOutlivesIsRecorded(t *testing.T) {
	// Arrange.
	tr, pub, log := newTracker(t, &fakeStore{})
	tr.EditorStarted(context.Background(), t0)

	// Act.
	tr.LoginCompleted(context.Background(), t0.Add(-time.Minute))

	// Assert.
	if len(pub.views) != 1 {
		t.Fatalf("stated %d sessions, want only the editor's", len(pub.views))
	}
	for _, r := range log.Records() {
		if r.Operation == "daemon.agentreplsession.begin" && r.Context["event"] == "login" {
			return
		}
	}
	t.Fatalf("the outlived login was not recorded: %v", log.Records())
}

func TestASessionThatCannotBePersistedIsNotBegun(t *testing.T) {
	// Arrange.
	store := &fakeStore{putErr: errors.New("read-only")}
	tr, pub, log := newTracker(t, store)

	// Act.
	tr.EditorStarted(context.Background(), t0)

	// Assert.
	if !logged(log, "error", "daemon.agentreplsession.begin") {
		t.Fatalf("the failed write was not recorded at ERROR: %v", log.Records())
	}
	if len(pub.views) != 0 || store.session != nil {
		t.Fatalf("stated %v and stored %+v, want nothing begun", pub.views, store.session)
	}
}

func TestASessionIsNotBegunWhenTheStandingOneCannotBeRead(t *testing.T) {
	// Arrange.
	store := &fakeStore{}
	tr, pub, log := newTracker(t, store)
	store.readErr = errors.New("disk gone")

	// Act.
	tr.LoginCompleted(context.Background(), t0)

	// Assert.
	if !logged(log, "error", "daemon.agentreplsession.begin") {
		t.Fatalf("the failed read was not recorded at ERROR: %v", log.Records())
	}
	if len(pub.views) != 0 || store.puts != 0 {
		t.Fatalf("stated %d and wrote %d times, want nothing", len(pub.views), store.puts)
	}
}

func TestViewPanicsOnACauseWsmNeverHolds(t *testing.T) {
	// Arrange.
	defer func() {
		// Assert.
		if r := recover(); r == nil || !strings.Contains(r.(string), "reboot") {
			t.Fatalf("recovered %v, want a panic naming the cause", r)
		}
	}()

	// Act.
	view(wsm.AgentReplSession{StartedAt: t0, Began: "reboot"})
}

// TestRecordsCarryTheSessionsSharedContext asserts the tracker's records use
// the one context shape wsm defines, rather than a hand-rolled divergent one.
func TestRecordsCarryTheSessionsSharedContext(t *testing.T) {
	// Arrange.
	tr, _, log := newTracker(t, &fakeStore{})

	// Act.
	tr.EditorStarted(context.Background(), t0)

	// Assert.
	want := wsm.AgentReplSession{StartedAt: t0, Began: wsm.SessionBeganEditorStart}.LogContext()
	for _, r := range log.Records() {
		if r.Operation != "daemon.agentreplsession.begin" {
			continue
		}
		for k, v := range want {
			if r.Context[k] != v {
				t.Fatalf("record context %v, want the shared %v", r.Context, want)
			}
		}
		return
	}
	t.Fatalf("no begin record: %v", log.Records())
}

func TestASuccessorBeginsAgainstWhatTheIncumbentLastWrote(t *testing.T) {
	// Arrange: the successor is built from the store before the incumbent,
	// still serving, begins a later session.
	store := &fakeStore{}
	incumbent, _, _ := newTracker(t, store)
	successor, pub, _ := newTracker(t, store)
	incumbent.LoginCompleted(context.Background(), t0.Add(time.Hour))

	// Act.
	successor.EditorStarted(context.Background(), t0)

	// Assert.
	if len(pub.views) != 0 || store.session.Began != wsm.SessionBeganLogin {
		t.Fatalf("successor stated %v and the store holds %+v, want the incumbent's later login standing", pub.views, store.session)
	}
}
