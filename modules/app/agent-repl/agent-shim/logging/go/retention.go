package logging

// retention.go -- HOW LONG A LOG FILE IS KEPT, and the two ways a file that has
// outlived it is removed.
//
// THE RULE. A log file is kept until its LAST WRITE is DefaultRetention old,
// and is then removed. It is the same rule for every runtime and every log, so
// nothing on disk is older than a few days and the whole of agent-repl's
// logging is bounded by what a few days of writing produces.
//
// WHO REMOVES WHAT. Only a file's own writer, or a pruner that has proved no
// writer is left, ever renames or removes a log file:
//
//   - A FAMILY SOMEBODY HOLDS (a RotatingFile is open on its current file) is
//     aged by that holder: Expire rolls a current file whose last write is past
//     the retention and removes the generations past it, all under the
//     RotatingFile's own mutex, so it can never race the holder's own rolls.
//   - A FAMILY NOBODY HOLDS (a closed workspace's sinks, an exited daemon's
//     run log generations) is aged by PruneDirectory, which proves nobody holds
//     it by taking the exclusive flock every holder's shared lock excludes.
//
// THE AGE IS THE LAST WRITE, NOT THE OLDEST RECORD. A current generation that
// is written slowly can hold records older than the retention until it reaches
// the cap; its size is then bounded by the cap, and the generation it rolls
// into is removed one retention after that roll.

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"syscall"
	"time"
)

// DefaultRetention is how long after its last write a log file is kept.
const DefaultRetention = 72 * time.Hour

// DefaultExpireInterval is how often a long-lived holder ages the family it
// holds. Hourly keeps a file at most an hour past the retention.
const DefaultExpireInterval = time.Hour

// ExpireResult is what one Expire did.
type ExpireResult struct {
	// RolledCurrent says the current generation was past the retention and
	// was rolled away, so the next write starts a fresh file.
	RolledCurrent bool
	// Removed are the generation files removed, oldest slot last.
	Removed []string
}

// CurrentExpired answers whether the current generation's last write is older
// than the retention. An empty current file is never expired: there is
// nothing in it to age, and rolling it would only mint an empty generation.
//
// The answer reads the FILE's modification time, not this writer's memory of
// its own writes, so a descriptor another process shares (the shim's fd 3)
// keeps the file young by writing to it.
func (r *RotatingFile) CurrentExpired(now time.Time) (bool, error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.currentExpiredLocked(now)
}

func (r *RotatingFile) currentExpiredLocked(now time.Time) (bool, error) {
	if r.f == nil {
		return false, fmt.Errorf("rotating log %q is closed", r.path)
	}
	info, err := r.f.Stat()
	if err != nil {
		return false, fmt.Errorf("stat rotating log %q for its age: %w", r.path, err)
	}
	return info.Size() > 0 && info.ModTime().Before(now.Add(-r.retention)), nil
}

// ExpireGenerations removes every retained generation whose last write is
// older than the retention, and answers the paths it removed.
func (r *RotatingFile) ExpireGenerations(now time.Time) ([]string, error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	if r.f == nil {
		return nil, fmt.Errorf("rotating log %q is closed", r.path)
	}
	return r.expireGenerationsLocked(now)
}

func (r *RotatingFile) expireGenerationsLocked(now time.Time) ([]string, error) {
	cutoff := now.Add(-r.retention)
	var removed []string
	var errs []error
	for i := 1; i <= r.backups; i++ {
		path := generationPath(r.path, i)
		info, err := os.Lstat(path)
		if errors.Is(err, fs.ErrNotExist) {
			continue
		}
		if err != nil {
			errs = append(errs, fmt.Errorf("stat rotating log generation %q for its age: %w", path, err))
			continue
		}
		if !info.ModTime().Before(cutoff) {
			continue
		}
		if err := removeIfPresent(path); err != nil {
			errs = append(errs, err)
			continue
		}
		removed = append(removed, path)
	}
	return removed, errors.Join(errs...)
}

// Expire ages the whole family this writer holds: a current generation past
// the retention is rolled (and, being past it, removed with the generations),
// and every generation past it is removed. It is for a writer that owns its
// descriptor outright; a writer whose descriptor a child shares rolls only at
// the child's replacement, so it asks CurrentExpired and ExpireGenerations
// apart instead.
func (r *RotatingFile) Expire(now time.Time) (ExpireResult, error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	expired, err := r.currentExpiredLocked(now)
	if err != nil {
		return ExpireResult{}, err
	}
	var result ExpireResult
	if expired {
		if err := r.rollLocked(); err != nil {
			return ExpireResult{}, fmt.Errorf("roll the expired current generation: %w", err)
		}
		result.RolledCurrent = true
	}
	removed, err := r.expireGenerationsLocked(now)
	result.Removed = removed
	return result, err
}

// ExpireOn ages r once at once and again at every tick until ctx is done,
// handing every outcome to report, which is where the holder's own logger
// records it. It returns when ctx is done.
func ExpireOn(ctx context.Context, r *RotatingFile, ticks <-chan time.Time, now func() time.Time, report func(ExpireResult, error)) {
	report(r.Expire(now()))
	for {
		select {
		case <-ctx.Done():
			return
		case at := <-ticks:
			report(r.Expire(at))
		}
	}
}

// PruneReport is what one PruneDirectory pass did.
type PruneReport struct {
	// Families is how many owned families were examined.
	Families int
	// Held are the files skipped because a writer holds them.
	Held []string
	// Removed are the files removed.
	Removed []string
	// Kept is how many examined files are still inside the retention.
	Kept int
}

// PruneDirectory removes, from every log family in dir that owned names and no
// writer holds, each file whose last write is older than retention.
//
// A FAMILY is a current file `<base>` and its generations `<base>.<n>`. owned
// is asked about the base, and must name only families whose every writer is a
// RotatingFile: the shared lock a RotatingFile holds is the ONLY thing that
// tells this pruner a family is in use. A file written by anything else (a
// launchd-held stdout, an editor that opens per write) is never owned here; its
// writer ages it.
//
// THE PROOF THAT NOBODY HOLDS A FAMILY is the exclusive flock on its current
// file, held for as long as any of the family is being removed. A holder's
// shared lock refuses it, so a held family is skipped whole; a writer that
// opens the family meanwhile blocks on its own shared lock until the pruner is
// done, then sees the path no longer names what it opened and opens it afresh
// (openLocked). A family whose current file is absent has no writer to race,
// and each of its generations is locked alone before it is removed.
func PruneDirectory(dir string, now time.Time, retention time.Duration, owned func(base string) bool) (PruneReport, error) {
	if retention <= 0 {
		return PruneReport{}, fmt.Errorf("prune log directory %q: retention %v is not positive", dir, retention)
	}
	entries, err := os.ReadDir(dir)
	if err != nil {
		return PruneReport{}, fmt.Errorf("read log directory %q to prune it: %w", dir, err)
	}
	families := map[string][]int{}
	for _, entry := range entries {
		if !entry.Type().IsRegular() {
			continue
		}
		base, gen := splitGeneration(entry.Name())
		if !owned(base) {
			continue
		}
		families[base] = append(families[base], gen)
	}
	bases := make([]string, 0, len(families))
	for base := range families {
		bases = append(bases, base)
	}
	sort.Strings(bases)

	cutoff := now.Add(-retention)
	var report PruneReport
	var errs []error
	for _, base := range bases {
		report.Families++
		gens := families[base]
		sort.Ints(gens)
		if err := pruneFamily(filepath.Join(dir, base), gens, cutoff, &report); err != nil {
			errs = append(errs, err)
		}
	}
	return report, errors.Join(errs...)
}

// pruneFamily ages one family under the exclusive lock on its current file.
// gens lists the generation numbers present, 0 being the current file.
func pruneFamily(current string, gens []int, cutoff time.Time, report *PruneReport) error {
	var lock *os.File
	if len(gens) > 0 && gens[0] == 0 {
		f, outcome, err := lockExclusive(current)
		if err != nil {
			return err
		}
		switch outcome {
		case lockHeld:
			report.Held = append(report.Held, current)
			return nil
		case lockTaken:
			lock = f
		}
		gens = gens[1:]
	}
	var errs []error
	for _, gen := range gens {
		if err := pruneAlone(generationPath(current, gen), cutoff, report); err != nil {
			errs = append(errs, err)
		}
	}
	if lock != nil {
		if err := removeIfExpired(current, lock, cutoff, report); err != nil {
			errs = append(errs, err)
		}
		if err := lock.Close(); err != nil {
			errs = append(errs, fmt.Errorf("release the prune lock on %q: %w", current, err))
		}
	}
	return errors.Join(errs...)
}

// pruneAlone locks one file exclusively and removes it when it is expired. A
// held file is skipped: a retiring child may still write the generation its
// descriptor was renamed into.
func pruneAlone(path string, cutoff time.Time, report *PruneReport) error {
	f, outcome, err := lockExclusive(path)
	if err != nil {
		return err
	}
	switch outcome {
	case lockHeld:
		report.Held = append(report.Held, path)
		return nil
	case lockGone:
		return nil
	}
	err = removeIfExpired(path, f, cutoff, report)
	if closeErr := f.Close(); closeErr != nil {
		err = errors.Join(err, fmt.Errorf("release the prune lock on %q: %w", path, closeErr))
	}
	return err
}

// removeIfExpired removes path, whose exclusive lock f holds, when the file's
// last write is before cutoff.
func removeIfExpired(path string, f *os.File, cutoff time.Time, report *PruneReport) error {
	info, err := f.Stat()
	if err != nil {
		return fmt.Errorf("stat %q for its age: %w", path, err)
	}
	if !info.ModTime().Before(cutoff) {
		report.Kept++
		return nil
	}
	if err := removeIfPresent(path); err != nil {
		return err
	}
	report.Removed = append(report.Removed, path)
	return nil
}

// lockOutcome is what lockExclusive found.
type lockOutcome int

const (
	// lockTaken: the exclusive lock is held, on the returned file.
	lockTaken lockOutcome = iota
	// lockHeld: another descriptor holds a lock on the file.
	lockHeld
	// lockGone: the file vanished before it could be opened. Whoever removed
	// it owns that outcome, and nothing is left here to prune.
	lockGone
)

// lockExclusive opens path and takes the exclusive flock without waiting.
func lockExclusive(path string) (*os.File, lockOutcome, error) {
	f, err := os.Open(path)
	if errors.Is(err, fs.ErrNotExist) {
		return nil, lockGone, nil
	}
	if err != nil {
		return nil, lockTaken, fmt.Errorf("open %q to prune it: %w", path, err)
	}
	if err := syscall.Flock(int(f.Fd()), syscall.LOCK_EX|syscall.LOCK_NB); err != nil {
		if errors.Is(err, syscall.EWOULDBLOCK) {
			return nil, lockHeld, closeAfter(f, nil)
		}
		return nil, lockTaken, closeAfter(f, fmt.Errorf("lock %q to prune it: %w", path, err))
	}
	return f, lockTaken, nil
}

// splitGeneration splits `<base>.<n>` into base and n, and answers a name with
// no positive numeric suffix as its own base at generation 0.
func splitGeneration(name string) (string, int) {
	dot := strings.LastIndexByte(name, '.')
	if dot <= 0 || dot == len(name)-1 {
		return name, 0
	}
	n, err := strconv.Atoi(name[dot+1:])
	if err != nil || n < 1 || strconv.Itoa(n) != name[dot+1:] {
		return name, 0
	}
	return name[:dot], n
}
