package appfreeze

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"syscall"

	"agent-repld/internal/atomicfile"
)

// Record is the frozen set: every process the quiet state may have stopped,
// by application, in the configured order.
type Record struct {
	Apps []RecordedApp `json:"apps"`
}

// RecordedApp is one application's frozen processes.
type RecordedApp struct {
	Name string `json:"name"`
	PIDs []int  `json:"pids"`
}

// empty reports a record that names no process.
func (r Record) empty() bool {
	for _, app := range r.Apps {
		if len(app.PIDs) > 0 {
			return false
		}
	}
	return true
}

// Store keeps the Record durably. FileStore is the production one.
type Store interface {
	// Load answers the kept record, and false when none is kept.
	Load() (Record, bool, error)
	// Save keeps r, replacing any kept record.
	Save(r Record) error
	// Clear forgets the kept record; clearing none is no error.
	Clear() error
}

// RecordFile and OwnersFile are the frozen set's two files under the state
// root's persistent-wifi directory.
const (
	RecordFile = "frozen-apps.json"
	OwnersFile = "frozen-apps.owners"
)

// FileStore keeps the record as one JSON file, replaced atomically and
// synced, so a crash leaves the old record or the new, never half of either.
// It is a file rather than a state-database row because the thaw guard reads
// it after every daemon is gone (guard.go).
type FileStore struct{ Path string }

// Load implements Store.
func (s FileStore) Load() (Record, bool, error) {
	body, err := os.ReadFile(s.Path)
	if errors.Is(err, fs.ErrNotExist) {
		return Record{}, false, nil
	}
	if err != nil {
		return Record{}, false, fmt.Errorf("read the frozen set %s: %w", s.Path, err)
	}
	var r Record
	if err := json.Unmarshal(body, &r); err != nil {
		return Record{}, false, fmt.Errorf("the frozen set %s is not a record: %w", s.Path, err)
	}
	return r, true, nil
}

// Save implements Store.
func (s FileStore) Save(r Record) error {
	body, err := json.Marshal(r)
	if err != nil {
		// A Record is ints and strings; one that does not marshal is a bug.
		panic(fmt.Sprintf("appfreeze: a frozen set did not marshal: %v", err))
	}
	if err := os.MkdirAll(filepath.Dir(s.Path), 0o700); err != nil {
		return fmt.Errorf("make the frozen set's directory: %w", err)
	}
	if err := atomicfile.Replace(s.Path, body, atomicfile.Options{Sync: true}); err != nil {
		return fmt.Errorf("keep the frozen set %s: %w", s.Path, err)
	}
	return nil
}

// Clear implements Store.
func (s FileStore) Clear() error {
	if err := os.Remove(s.Path); err != nil && !errors.Is(err, fs.ErrNotExist) {
		return fmt.Errorf("clear the frozen set %s: %w", s.Path, err)
	}
	return nil
}

// Signaler sends one signal to one process. KillSignaler is the production
// one; a test records the signals instead, so no test signals a real process.
type Signaler func(pid int, sig syscall.Signal) error

// KillSignaler is kill(2).
func KillSignaler(pid int, sig syscall.Signal) error { return syscall.Kill(pid, sig) }

// errnoOf names the errno a failed signal carries, for the record: "ESRCH",
// "EPERM", or the error itself when it carries none.
func errnoOf(err error) string {
	var errno syscall.Errno
	if errors.As(err, &errno) {
		switch errno {
		case syscall.ESRCH:
			return "ESRCH"
		case syscall.EPERM:
			return "EPERM"
		}
		return fmt.Sprintf("errno %d", int(errno))
	}
	return err.Error()
}

// gone reports a signal that failed because the process no longer exists:
// it exited between the table read and the signal, or while frozen.
func gone(err error) bool { return errors.Is(err, syscall.ESRCH) }
