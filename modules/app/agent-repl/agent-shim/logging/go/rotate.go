package logging

// rotate.go -- THE SIZE-CAPPED, N-GENERATION LOG FILE every long-lived Go
// runtime in this repo appends through.
//
// WHY IT LIVES HERE. Every long-lived Go runtime needs the same cap in bytes,
// the same fixed number of retained generations, and a roll that renames
// rather than truncates so the newest evidence is never the evidence that gets
// thrown away. The daemon, store and sidecar are separate Go modules and
// cannot own that answer for each other. The answer belongs with the other
// facts every runtime must answer identically, so it is hoisted here.
//
// Every consumer appends to what it finds and rolls ONLY at the cap or at
// expiry (retention.go). A deploy, crash, or ordinary process restart never
// consumes a history generation.
//
// THE CURRENT GENERATION IS HELD UNDER A SHARED flock FOR AS LONG AS IT IS
// OPEN. That lock is how a pruner in ANY process tells a family somebody still
// writes from one nobody does (PruneDirectory): it is attached to the open file
// description, so a child that inherits the descriptor (the shim's fd 3) holds
// it too, and the kernel drops it with the last holder. A roll never leaves the
// family unlocked: the current file is renamed into slot 1 while still open and
// locked, and the retiring descriptor is closed only after the fresh one is.

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strconv"
	"sync"
	"syscall"
	"time"
)

// DefaultCapBytes is how large one generation may grow before the writer
// rolls. It matches the daemon's own cap so a reader of any runtime's log
// knows the same number.
const DefaultCapBytes = 64 << 20

// DefaultBackups is how many previous generations are retained beside the
// current file, as `<path>.1` (the most recent) through `<path>.N` (the
// oldest).
const DefaultBackups = 5

// RotatingFile is an io.Writer over a log file that never exceeds
// (backups+1) * cap bytes on disk.
//
// It is safe for concurrent use: a logging package that serializes its own
// records still shares this writer with whatever else holds it.
type RotatingFile struct {
	path      string
	cap       int64
	backups   int
	retention time.Duration

	mu   sync.Mutex
	f    *os.File
	size int64
}

// File answers the current generation's descriptor. The RotatingFile retains
// ownership: callers may hand the descriptor to exec.Cmd.ExtraFiles, which
// duplicates it into the child, but must never close it themselves.
func (r *RotatingFile) File() *os.File {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.f
}

// OpenRotating opens (creating as needed) the log file at path, appending to
// whatever is already there. A cap or backup count of zero takes the default;
// a negative one is a programming error and is refused rather than clamped,
// because "no cap" is precisely the state this type exists to make
// unreachable.
func OpenRotating(path string, capBytes int64, backups int) (*RotatingFile, error) {
	if path == "" {
		return nil, fmt.Errorf("rotating log path is empty")
	}
	if capBytes < 0 {
		return nil, fmt.Errorf("rotating log %q: cap %d bytes is negative", path, capBytes)
	}
	if backups < 0 {
		return nil, fmt.Errorf("rotating log %q: backup count %d is negative", path, backups)
	}
	if capBytes == 0 {
		capBytes = DefaultCapBytes
	}
	if backups == 0 {
		backups = DefaultBackups
	}
	dir := filepath.Dir(path)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return nil, fmt.Errorf("create rotating log directory %q: %w", dir, err)
	}
	f, err := openLocked(path)
	if err != nil {
		return nil, err
	}
	info, err := f.Stat()
	if err != nil {
		statErr := fmt.Errorf("stat rotating log %q: %w", path, err)
		if closeErr := f.Close(); closeErr != nil {
			return nil, errors.Join(statErr, fmt.Errorf("close rotating log %q: %w", path, closeErr))
		}
		return nil, statErr
	}
	return &RotatingFile{path: path, cap: capBytes, backups: backups, retention: DefaultRetention, f: f, size: info.Size()}, nil
}

// openLocked opens path for append and takes the shared lock on it, then
// checks that the path STILL names the file it locked. A pruner that holds the
// exclusive lock may unlink the path between this open and this lock; the
// check catches that, and the second open creates a fresh file no pruner can
// take (a fresh file is not expired). A second miss is therefore not a race to
// wait out but a broken invariant, and it is refused.
func openLocked(path string) (*os.File, error) {
	for attempt := 1; attempt <= 2; attempt++ {
		f, err := os.OpenFile(path, os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0o644)
		if err != nil {
			return nil, fmt.Errorf("open rotating log %q: %w", path, err)
		}
		if err := syscall.Flock(int(f.Fd()), syscall.LOCK_SH); err != nil {
			return nil, closeAfter(f, fmt.Errorf("lock rotating log %q: %w", path, err))
		}
		same, err := namesFile(path, f)
		if err != nil {
			return nil, closeAfter(f, err)
		}
		if same {
			return f, nil
		}
		if err := f.Close(); err != nil {
			return nil, fmt.Errorf("close rotating log %q after a pruner replaced it: %w", path, err)
		}
	}
	return nil, fmt.Errorf("open rotating log %q: the path named a different file after each of two locked opens", path)
}

// namesFile answers whether path names the file f has open.
func namesFile(path string, f *os.File) (bool, error) {
	held, err := f.Stat()
	if err != nil {
		return false, fmt.Errorf("stat the open rotating log %q: %w", path, err)
	}
	named, err := os.Stat(path)
	if errors.Is(err, fs.ErrNotExist) {
		return false, nil
	}
	if err != nil {
		return false, fmt.Errorf("stat rotating log path %q: %w", path, err)
	}
	return os.SameFile(held, named), nil
}

// closeAfter closes f and answers cause joined with any close failure.
func closeAfter(f *os.File, cause error) error {
	if err := f.Close(); err != nil {
		return errors.Join(cause, fmt.Errorf("close %q: %w", f.Name(), err))
	}
	return cause
}

// Path answers the current generation's path.
func (r *RotatingFile) Path() string { return r.path }

// Write appends p, rolling FIRST when p would carry the file past the cap, so
// a record is never split across two generations. A record larger than the cap
// still lands whole in a file of its own rather than being refused: losing the
// record would be worse than exceeding the cap once.
func (r *RotatingFile) Write(p []byte) (int, error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	if r.f == nil {
		return 0, fmt.Errorf("rotating log %q is closed", r.path)
	}
	if r.size > 0 && r.size+int64(len(p)) > r.cap {
		if err := r.rollLocked(); err != nil {
			return 0, err
		}
	}
	n, err := r.f.Write(p)
	r.size += int64(n)
	return n, err
}

// Roll explicitly closes the current generation, shifts the retained
// generations, and opens a fresh descriptor. Size-triggered writers do not
// need it; descriptor-inheriting writers use it only at process replacement,
// so the retired child keeps its duplicated descriptor on the renamed inode
// while the replacement child inherits the fresh one.
func (r *RotatingFile) Roll() error {
	r.mu.Lock()
	defer r.mu.Unlock()
	if r.f == nil {
		return fmt.Errorf("rotating log %q is closed", r.path)
	}
	return r.rollLocked()
}

// rollLocked shifts the retained generations down one slot, moving the
// current file into slot 1 WHILE IT IS STILL OPEN AND LOCKED, opens and locks a
// fresh current file, and only then closes the retiring descriptor: at every
// instant some file of the family carries this writer's lock. A failed roll
// leaves the writer closed, so every later write is refused rather than lost.
// Caller holds mu.
func (r *RotatingFile) rollLocked() error {
	retiring := r.f
	r.f = nil
	if err := rotateGenerations(r.path, r.backups); err != nil {
		return closeAfter(retiring, err)
	}
	f, err := openLocked(r.path)
	if err != nil {
		return closeAfter(retiring, fmt.Errorf("reopen rotating log after the roll: %w", err))
	}
	r.f = f
	r.size = 0
	if err := retiring.Close(); err != nil {
		return fmt.Errorf("close the retired generation of rotating log %q: %w", r.path, err)
	}
	return nil
}

// Close releases the descriptor. A closed writer refuses further writes rather
// than silently dropping them.
func (r *RotatingFile) Close() error {
	r.mu.Lock()
	defer r.mu.Unlock()
	if r.f == nil {
		return nil
	}
	f := r.f
	r.f = nil
	if err := f.Close(); err != nil {
		return fmt.Errorf("close rotating log %q: %w", r.path, err)
	}
	return nil
}

// rotateGenerations shifts the retained backups down one slot and moves the
// current file into slot 1. The oldest slot is discarded. A missing file at
// any slot is not an error: the first roll on a fresh path has none.
func rotateGenerations(path string, backups int) error {
	if backups < 1 {
		return removeIfPresent(path)
	}
	if err := removeIfPresent(generationPath(path, backups)); err != nil {
		return err
	}
	for i := backups - 1; i >= 1; i-- {
		if err := renameIfPresent(generationPath(path, i), generationPath(path, i+1)); err != nil {
			return err
		}
	}
	return renameIfPresent(path, generationPath(path, 1))
}

// generationPath names the i-th retained generation.
func generationPath(path string, i int) string { return path + "." + strconv.Itoa(i) }

func removeIfPresent(path string) error {
	if err := os.Remove(path); err != nil && !os.IsNotExist(err) {
		return fmt.Errorf("remove rotating log generation %q: %w", path, err)
	}
	return nil
}

// renameIfPresent moves from onto to. An absent from is the rename's own
// answer rather than a separate stat's, so a generation a pruner removed an
// instant earlier is absent and not a failure.
func renameIfPresent(from, to string) error {
	if err := os.Rename(from, to); err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return nil
		}
		return fmt.Errorf("roll rotating log %q to %q: %w", from, to, err)
	}
	return nil
}
