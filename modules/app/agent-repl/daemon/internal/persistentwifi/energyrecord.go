package persistentwifi

import (
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"

	"agent-repld/internal/atomicfile"
)

// EnergyRecord is the energy mode the machine ran in when the lid-closed
// quiet state was entered: the user's, restored when the state is left.
type EnergyRecord struct {
	// Mode is the `pmset` powermode value, verbatim ("1" Low Power Mode, "2"
	// High Power Mode, "0" Automatic).
	Mode string `json:"mode"`
}

// EnergyStore keeps the EnergyRecord durably, so a daemon that adopts a
// standing quiet state across a restart, a handover or a crash restores what
// the first one recorded. EnergyFileStore is the production one.
type EnergyStore interface {
	// Load answers the kept record, and false when none is kept. A record
	// that cannot be read or names no powermode is an error.
	Load() (EnergyRecord, bool, error)
	// Save keeps r, replacing any kept record.
	Save(r EnergyRecord) error
	// Clear forgets the kept record; clearing none is no error.
	Clear() error
}

// EnergyRecordFile is the record's file under the state root's
// persistent-wifi directory, beside the frozen set (appfreeze.RecordFile).
const EnergyRecordFile = "energy-mode.json"

// powerModeValue is a well-formed `pmset` powermode value (energyModeLine's).
var powerModeValue = regexp.MustCompile(`^\d+$`)

// EnergyFileStore keeps the record as one JSON file, replaced atomically and
// synced, so a crash leaves the old record or the new, never half of either.
// It is a file rather than a state-database row because the thaw guard reads
// it after every daemon is gone (GuardRestoreEnergy), exactly as it reads the
// frozen set.
type EnergyFileStore struct{ Path string }

// Load implements EnergyStore.
func (s EnergyFileStore) Load() (EnergyRecord, bool, error) {
	body, err := os.ReadFile(s.Path)
	if errors.Is(err, fs.ErrNotExist) {
		return EnergyRecord{}, false, nil
	}
	if err != nil {
		return EnergyRecord{}, false, fmt.Errorf("read the recorded energy mode %s: %w", s.Path, err)
	}
	var r EnergyRecord
	if err := json.Unmarshal(body, &r); err != nil {
		return EnergyRecord{}, false, fmt.Errorf("the recorded energy mode %s is not a record: %w", s.Path, err)
	}
	if !powerModeValue.MatchString(r.Mode) {
		return EnergyRecord{}, false, fmt.Errorf("the recorded energy mode %s names no powermode (%q)", s.Path, r.Mode)
	}
	return r, true, nil
}

// Save implements EnergyStore.
func (s EnergyFileStore) Save(r EnergyRecord) error {
	body, err := json.Marshal(r)
	if err != nil {
		// An EnergyRecord is one string; one that does not marshal is a bug.
		panic(fmt.Sprintf("persistentwifi: an energy record did not marshal: %v", err))
	}
	if err := os.MkdirAll(filepath.Dir(s.Path), 0o700); err != nil {
		return fmt.Errorf("make the recorded energy mode's directory: %w", err)
	}
	if err := atomicfile.Replace(s.Path, body, atomicfile.Options{Sync: true}); err != nil {
		return fmt.Errorf("keep the recorded energy mode %s: %w", s.Path, err)
	}
	return nil
}

// Clear implements EnergyStore.
func (s EnergyFileStore) Clear() error {
	if err := os.Remove(s.Path); err != nil && !errors.Is(err, fs.ErrNotExist) {
		return fmt.Errorf("clear the recorded energy mode %s: %w", s.Path, err)
	}
	return nil
}
