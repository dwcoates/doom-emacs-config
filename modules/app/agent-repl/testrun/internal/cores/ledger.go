package cores

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"sync/atomic"
	"syscall"
	"time"

	"agentrepl/testrun/internal/atomicfile"
)

// THE LEDGER: what every governed unit on the host expects to cost. A unit
// states itself in the pool's directory before it asks for seats (a request:
// its rank, width and expected cores) and again once it holds them (its start),
// and its record goes when it gives them back. The governor reads the ledger
// to cost the next unit by its own history rather than by a flat cost a seat,
// and to count a just-started unit's cores until the host's reads show them.
//
// A DEAD RUN'S RECORDS ARE KNOWN BY THE KERNEL. Each run holds its run lock
// (core-run-<token>.lock) for as long as it lives, and every record carries
// its run's token. The governor reads a run's records only while their lock
// is held: a lock it can take is a run that died, and it removes the run's
// records and lock.

const (
	ledgerRunPrefix  = "core-run-"
	ledgerUnitPrefix = "core-unit-"
)

// UnitRecord is one unit's entry in the ledger.
type UnitRecord struct {
	Unit  string  `json:"unit"`
	Rank  int     `json:"rank"`
	Width int     `json:"width"`
	Cores float64 `json:"cores"`
	// Started is when the unit took its seats; zero while it waits for them.
	Started time.Time `json:"started,omitzero"`
}

// ledger is one run's hand in the pool's ledger.
type ledger struct {
	dir   string
	once  sync.Once
	token string
	lock  *os.File
	err   error
	seq   atomic.Int64
}

// open takes the run's lock, once, the first time a unit states itself.
func (l *ledger) open() error {
	l.once.Do(func() {
		if err := os.MkdirAll(l.dir, 0o755); err != nil {
			l.err = fmt.Errorf("make the core seats directory: %w", err)
			return
		}
		l.token = fmt.Sprintf("%d.%d", os.Getpid(), time.Now().UnixNano())
		path := filepath.Join(l.dir, ledgerRunPrefix+l.token+".lock")
		f, err := os.OpenFile(path, os.O_CREATE|os.O_RDWR, 0o644)
		if err != nil {
			l.err = fmt.Errorf("open the run's ledger lock %s: %w", path, err)
			return
		}
		if err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
			l.err = errors.Join(fmt.Errorf("lock the run's ledger lock %s: %w", path, err), f.Close())
			return
		}
		l.lock = f
	})
	return l.err
}

// post states a new unit, answering its record's path.
func (l *ledger) post(rec UnitRecord) (string, error) {
	if err := l.open(); err != nil {
		return "", err
	}
	path := filepath.Join(l.dir, fmt.Sprintf("%s%s-%d.json", ledgerUnitPrefix, l.token, l.seq.Add(1)))
	return path, writeRecord(path, rec)
}

// writeRecord replaces a unit's record whole, so a reader never sees half of
// it.
func writeRecord(path string, rec UnitRecord) error {
	data, err := json.Marshal(rec)
	if err != nil {
		return fmt.Errorf("encode the ledger record of unit %s: %w", rec.Unit, err)
	}
	if err := atomicfile.Replace(path, filepath.Base(path)+".*", data); err != nil {
		return fmt.Errorf("write the ledger record of unit %s: %w", rec.Unit, err)
	}
	return nil
}

// readLedger answers every live run's unit records under dir, and removes a
// dead run's.
func readLedger(dir string) ([]UnitRecord, error) {
	entries, err := os.ReadDir(dir)
	if errors.Is(err, os.ErrNotExist) {
		return nil, nil
	}
	if err != nil {
		return nil, fmt.Errorf("read the core ledger %s: %w", dir, err)
	}
	// Every run's lock is visited, so a run that ended with no unit left on
	// the ledger has its lock swept too.
	byRun := map[string][]string{}
	for _, e := range entries {
		name := e.Name()
		if token, ok := strings.CutPrefix(name, ledgerRunPrefix); ok && strings.HasSuffix(token, ".lock") {
			token = strings.TrimSuffix(token, ".lock")
			if _, ok := byRun[token]; !ok {
				byRun[token] = nil
			}
			continue
		}
		if !strings.HasPrefix(name, ledgerUnitPrefix) || !strings.HasSuffix(name, ".json") {
			continue
		}
		token, _, ok := strings.Cut(strings.TrimPrefix(name, ledgerUnitPrefix), "-")
		if !ok {
			return nil, fmt.Errorf("the core ledger record %s names no run", filepath.Join(dir, name))
		}
		byRun[token] = append(byRun[token], filepath.Join(dir, name))
	}
	var out []UnitRecord
	for token, paths := range byRun {
		alive, err := runAlive(dir, token, paths)
		if err != nil {
			return nil, err
		}
		if !alive {
			continue
		}
		for _, path := range paths {
			data, err := os.ReadFile(path)
			if errors.Is(err, os.ErrNotExist) {
				// Its unit gave its seats back since the directory was read.
				continue
			}
			if err != nil {
				return nil, fmt.Errorf("read the core ledger record %s: %w", path, err)
			}
			var rec UnitRecord
			if err := json.Unmarshal(data, &rec); err != nil {
				return nil, fmt.Errorf("the core ledger record %s does not read: %w", path, err)
			}
			out = append(out, rec)
		}
	}
	return out, nil
}

// runAlive answers whether the run token is alive: whether its lock is held.
// A run that is gone has its records and lock removed, under its lock.
func runAlive(dir, token string, records []string) (bool, error) {
	path := filepath.Join(dir, ledgerRunPrefix+token+".lock")
	f, err := os.OpenFile(path, os.O_RDWR, 0o644)
	if errors.Is(err, os.ErrNotExist) {
		// A run writes its lock before any record, so its records outlived
		// it: an earlier sweep removed the lock between the two.
		return false, removeAll(records)
	}
	if err != nil {
		return false, fmt.Errorf("open the ledger lock %s: %w", path, err)
	}
	defer f.Close()
	err = syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB)
	if errors.Is(err, syscall.EWOULDBLOCK) {
		return true, nil
	}
	if err != nil {
		return false, fmt.Errorf("probe the ledger lock %s: %w", path, err)
	}
	return false, removeAll(append(records, path))
}

func removeAll(paths []string) error {
	var errs []error
	for _, p := range paths {
		if err := os.Remove(p); err != nil && !errors.Is(err, os.ErrNotExist) {
			errs = append(errs, fmt.Errorf("remove a dead run's ledger file %s: %w", p, err))
		}
	}
	return errors.Join(errs...)
}
