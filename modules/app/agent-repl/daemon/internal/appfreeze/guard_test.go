package appfreeze

import (
	"errors"
	"io"
	"path/filepath"
	"reflect"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

func TestRunGuard(t *testing.T) {
	kept := func() *memStore {
		return &memStore{rec: &Record{Apps: []RecordedApp{{Name: "Slack", PIDs: []int{200, 201}}}}}
	}
	tests := []struct {
		name       string
		goneErr    error
		owned      bool
		ownErr     error
		store      *memStore
		wantEvents []string
		wantErr    bool
		wantErrors int
	}{
		{name: "no daemon owns the set: it is thawed, cleared, then released", owned: true, store: kept(),
			wantEvents: []string{"own", "cont:200", "cont:201", "clear", "release"}},
		{name: "a living daemon owns the set: nothing is thawed", owned: false, store: kept(),
			wantEvents: []string{"own"}},
		{name: "nothing kept: released without a signal", owned: true, store: &memStore{},
			wantEvents: []string{"own", "release"}},
		{name: "the wait failing thaws nothing", goneErr: errors.New("bad fd"), store: kept(),
			wantErr: true, wantErrors: 1},
		{name: "asking for the set failing thaws nothing", ownErr: errors.New("EBADF"), store: kept(),
			wantEvents: []string{"own"}, wantErr: true, wantErrors: 1},
		{name: "an unreadable set thaws nothing", owned: true, store: &memStore{loadErr: errors.New("corrupt")},
			wantEvents: []string{"own", "release"}, wantErr: true, wantErrors: 1},
		{name: "a set that cannot be cleared is recorded", owned: true,
			store:      func() *memStore { m := kept(); m.clearErr = errors.New("locked"); return m }(),
			wantEvents: []string{"own", "cont:200", "cont:201", "clear-failed", "release"}, wantErr: true, wantErrors: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			var events []string
			tt.store.events = &events
			log := dlog.NewTestLogger()

			// Act
			err := RunGuard(GuardDeps{
				DaemonGone: func() error { return tt.goneErr },
				TryOwn: func() (func(), bool, error) {
					events = append(events, "own")
					if tt.ownErr != nil || !tt.owned {
						return nil, false, tt.ownErr
					}
					return func() { events = append(events, "release") }, true, nil
				},
				Store:  tt.store,
				Signal: signalLog(&events, nil),
				Log:    log,
			})

			// Assert
			if (err != nil) != tt.wantErr {
				t.Errorf("RunGuard() = %v, want error %v", err, tt.wantErr)
			}
			if !reflect.DeepEqual(events, tt.wantEvents) {
				t.Errorf("events = %q, want %q", events, tt.wantEvents)
			}
			errs := dlog.FilterRecords(log.Records(), dlog.RecordQuery{Level: dlog.LevelError})
			if len(errs) != tt.wantErrors {
				t.Errorf("ERROR records = %+v, want %d", errs, tt.wantErrors)
			}
		})
	}
}

type scriptedReader struct {
	n   int
	err error
}

func (s scriptedReader) Read(p []byte) (int, error) { return s.n, s.err }

func TestPipeGone(t *testing.T) {
	tests := []struct {
		name    string
		reader  io.Reader
		wantErr string
	}{
		{name: "end of file is the daemon gone", reader: strings.NewReader("")},
		{name: "a byte is a breach", reader: strings.NewReader("x"), wantErr: "carried a byte"},
		{name: "a read error is surfaced", reader: scriptedReader{err: errors.New("EBADF")}, wantErr: "EBADF"},
		{name: "a read of nothing and no end is refused", reader: scriptedReader{}, wantErr: "neither"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			err := PipeGone(tt.reader)()

			// Assert
			if tt.wantErr == "" {
				if err != nil {
					t.Fatalf("PipeGone() = %v, want nil", err)
				}
				return
			}
			if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
				t.Fatalf("PipeGone() = %v, want an error naming %q", err, tt.wantErr)
			}
		})
	}
}

func TestOwnershipExcludesTheGuardWhileADaemonHoldsIt(t *testing.T) {
	// Arrange: flock on a temp file is kernel arbitration within this
	// process's own descriptors, no child process involved.
	path := filepath.Join(t.TempDir(), OwnersFile)
	held, err := HoldOwnership(path, GuardThawBound)
	if err != nil {
		t.Fatalf("HoldOwnership() = %v", err)
	}

	// Act
	_, ownedWhileHeld, errWhileHeld := FlockTryOwn(path)()
	_ = held.Close()
	release, ownedAfter, errAfter := FlockTryOwn(path)()

	// Assert
	if ownedWhileHeld || errWhileHeld != nil {
		t.Fatalf("while a daemon holds it: owned %v, %v; want refused", ownedWhileHeld, errWhileHeld)
	}
	if !ownedAfter || errAfter != nil {
		t.Fatalf("after the daemon let go: owned %v, %v; want granted", ownedAfter, errAfter)
	}
	release()
}

func TestHoldOwnershipWaitsNoLongerThanItsBound(t *testing.T) {
	// Arrange: a guard holds the file exclusive and never lets go.
	path := filepath.Join(t.TempDir(), OwnersFile)
	release, owned, err := FlockTryOwn(path)()
	if !owned || err != nil {
		t.Fatalf("FlockTryOwn() = %v, %v", owned, err)
	}
	defer release()

	// Act
	_, err = HoldOwnership(path, time.Millisecond)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "stuck") {
		t.Fatalf("HoldOwnership() = %v, want the bound to fire", err)
	}
}
