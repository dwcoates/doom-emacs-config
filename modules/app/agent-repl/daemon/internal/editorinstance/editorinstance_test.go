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
	isNew bool
	err   error
	noted []string
}

func (s *fakeStore) NoteEditorInstance(_ context.Context, instance string, _ time.Time) (bool, error) {
	s.noted = append(s.noted, instance)
	return s.isNew, s.err
}

func epoch() time.Time { return time.Unix(0, 0) }

func TestConnected(t *testing.T) {
	tests := []struct {
		name      string
		serves    bool
		isNew     bool
		wantNew   bool
		wantNoted int
		wantLevel string
	}{
		{name: "a new process on the serving daemon is new", serves: true, isNew: true, wantNew: true, wantNoted: 1, wantLevel: "info"},
		{name: "a reconnect on the serving daemon is not new", serves: true, isNew: false, wantNew: false, wantNoted: 1, wantLevel: "debug"},
		{name: "a joining daemon judges nothing and writes nothing", serves: false, isNew: true, wantNew: false, wantNoted: 0, wantLevel: "debug"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			store := &fakeStore{isNew: tt.isNew}
			logs := dlog.NewTestSurfaces()
			tracker, err := New(store, func() bool { return tt.serves }, epoch, logs.Global())
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
		})
	}
}

func TestConnectedReportsAFailedStore(t *testing.T) {
	// Arrange
	store := &fakeStore{err: errors.New("disk gone")}
	logs := dlog.NewTestSurfaces()
	tracker, err := New(store, func() bool { return true }, epoch, logs.Global())
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
}

func TestNewRefusesAMissingCollaborator(t *testing.T) {
	logs := dlog.NewTestSurfaces()
	tests := []struct {
		name   string
		store  Store
		serves func() bool
		now    func() time.Time
		log    dlog.Logger
	}{
		{name: "no store", serves: func() bool { return true }, now: epoch, log: logs.Global()},
		{name: "no serving test", store: &fakeStore{}, now: epoch, log: logs.Global()},
		{name: "no clock", store: &fakeStore{}, serves: func() bool { return true }, log: logs.Global()},
		{name: "no logger", store: &fakeStore{}, serves: func() bool { return true }, now: epoch},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, err := New(tt.store, tt.serves, tt.now, tt.log)

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
