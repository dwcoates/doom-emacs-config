package editorinstance

import (
	"context"
	"errors"
	"testing"
	"time"

	"claude-repld/internal/dlog"
)

// fakeStore answers scripted verdicts and records what it was told.
type fakeStore struct {
	isNew    bool
	err      error
	readOnly bool
	noted    []string
}

func (s *fakeStore) ReadOnly() bool { return s.readOnly }

func (s *fakeStore) NoteEditorInstance(_ context.Context, instance string, _ time.Time) (bool, error) {
	s.noted = append(s.noted, instance)
	return s.isNew, s.err
}

// fakeStarts records every session a new Emacs began.
type fakeStarts struct {
	began []time.Time
}

func (s *fakeStarts) EditorStarted(_ context.Context, at time.Time) {
	s.began = append(s.began, at)
}

func epoch() time.Time { return time.Unix(0, 0) }

func TestConnected(t *testing.T) {
	tests := []struct {
		name      string
		serves    bool
		readOnly  bool
		isNew     bool
		wantNew   bool
		wantNoted int
		wantLevel string
		wantBegan int
	}{
		{name: "a new process on the serving daemon is new", serves: true, isNew: true, wantNew: true, wantNoted: 1, wantLevel: "info", wantBegan: 1},
		{name: "a reconnect on the serving daemon is not new", serves: true, isNew: false, wantNew: false, wantNoted: 1, wantLevel: "debug"},
		{name: "a joining daemon judges nothing and writes nothing", serves: false, isNew: true, wantNew: false, wantNoted: 0, wantLevel: "debug"},
		{name: "a serving daemon whose handle is not promoted yet writes nothing", serves: true, readOnly: true, isNew: true, wantNew: false, wantNoted: 0, wantLevel: "debug"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			store := &fakeStore{isNew: tt.isNew, readOnly: tt.readOnly}
			starts := &fakeStarts{}
			logs := dlog.NewTestSurfaces()
			tracker, err := New(store, func() bool { return tt.serves }, epoch, logs.Global(), starts)
			if err != nil {
				t.Fatalf("New: %v", err)
			}

			// Act
			got, err := tracker.Connected(context.Background(), "e1")

			// Assert
			if err != nil || got != tt.wantNew {
				t.Fatalf("Connected = (%v, %v), want (%v, nil)", got, err, tt.wantNew)
			}
			if len(store.noted) != tt.wantNoted {
				t.Fatalf("noted %v, want %d writes", store.noted, tt.wantNoted)
			}
			if !logged(logs, tt.wantLevel) {
				t.Fatalf("records = %v, want one at %s", logs.Records(), tt.wantLevel)
			}
			if len(starts.began) != tt.wantBegan {
				t.Fatalf("began %d sessions, want %d", len(starts.began), tt.wantBegan)
			}
		})
	}
}

func TestANewEmacsBeginsTheSessionAtTheInstantItConnected(t *testing.T) {
	// Arrange
	at := time.Date(2026, 10, 6, 9, 0, 0, 0, time.UTC)
	starts := &fakeStarts{}
	tracker, err := New(&fakeStore{isNew: true}, func() bool { return true }, func() time.Time { return at }, dlog.NewTestSurfaces().Global(), starts)
	if err != nil {
		t.Fatalf("New: %v", err)
	}

	// Act
	if _, err := tracker.Connected(context.Background(), "e2"); err != nil {
		t.Fatalf("Connected: %v", err)
	}

	// Assert
	if len(starts.began) != 1 || !starts.began[0].Equal(at) {
		t.Fatalf("began %v, want one session at %s", starts.began, at)
	}
}

func TestConnectedReportsAFailedStore(t *testing.T) {
	// Arrange
	store := &fakeStore{err: errors.New("disk gone")}
	logs := dlog.NewTestSurfaces()
	starts := &fakeStarts{}
	tracker, err := New(store, func() bool { return true }, epoch, logs.Global(), starts)
	if err != nil {
		t.Fatalf("New: %v", err)
	}

	// Act
	got, err := tracker.Connected(context.Background(), "e1")

	// Assert
	if err == nil || got {
		t.Fatalf("Connected = (%v, %v), want the store's failure", got, err)
	}
	if !logged(logs, "error") {
		t.Fatalf("records = %v, want the failure at ERROR", logs.Records())
	}
	if len(starts.began) != 0 {
		t.Fatalf("a failed judgement began %d sessions, want none", len(starts.began))
	}
}

func TestNewRefusesAMissingCollaborator(t *testing.T) {
	logs := dlog.NewTestSurfaces()
	tests := []struct {
		name   string
		store  Store
		serves func() bool
		now    func() time.Time
		log    dlog.Logger
		starts Starts
	}{
		{name: "no store", serves: func() bool { return true }, now: epoch, log: logs.Global(), starts: &fakeStarts{}},
		{name: "no serving test", store: &fakeStore{}, now: epoch, log: logs.Global(), starts: &fakeStarts{}},
		{name: "no clock", store: &fakeStore{}, serves: func() bool { return true }, log: logs.Global(), starts: &fakeStarts{}},
		{name: "no logger", store: &fakeStore{}, serves: func() bool { return true }, now: epoch, starts: &fakeStarts{}},
		{name: "no session to begin", store: &fakeStore{}, serves: func() bool { return true }, now: epoch, log: logs.Global()},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, err := New(tt.store, tt.serves, tt.now, tt.log, tt.starts)

			// Assert
			if err == nil {
				t.Fatal("New = nil, want a refusal")
			}
		})
	}
}

// logged reports whether a record at level was written under op.
func logged(logs *dlog.TestSurfaces, level string) bool {
	for _, r := range logs.Records() {
		if r.Operation == op && r.Level == level {
			return true
		}
	}
	return false
}
