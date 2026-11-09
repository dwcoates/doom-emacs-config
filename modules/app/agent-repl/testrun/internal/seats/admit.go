package seats

import (
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"syscall"
	"time"
)

// ADMITTED SEATS: a pool whose seats are granted only while a check of the
// host, beyond the count of seats, admits one more (the RAM disk seats, whose
// check is the host's available memory). Admit is Take with three additions:
//
//   - THE CHECK AND THE TAKE ARE ONE STEP. A taker reads who holds seats,
//     runs the admission check and takes its seat all while holding the
//     pool's turnstile, so two takers can never both be admitted on the same
//     reading of the host.
//   - EVERY HOLDER IS NAMED. A taker writes who it is (Holder) into its seat's
//     lock file before it lets the turnstile go, and a taker reads the held
//     seats' records only under the turnstile, so a record read is always the
//     current holder's; a record of a holder that died sits only in a seat the
//     kernel already freed, which no reader reads.
//   - RANK ORDER, as TakeN keeps it: a taker holds its rank's lane, shared,
//     while it waits, and a taker finding a better rank's lane held waits for
//     it to empty before it even checks admission.
//
// A REFUSED TAKER WAKES ON A SEAT LET GO, OR ON A RECHECK. Every holder also
// holds its seat's watch lock (<Prefix>-<i>.watch), exclusive, for as long as
// it holds the seat; a refused or full-pool taker blocks, one goroutine per
// held seat, on a shared lock of that watch, which the kernel grants the
// moment the holder lets the seat go or dies. A refused taker also wakes on
// Admission.Recheck, since what the check reads (the host's memory) changes
// with no seat let go. Neither is a poll of the seats.

// Holder is the record a seat's holder writes into the seat.
type Holder struct {
	// Pid is the holding run's process id.
	Pid int
	// Workspace is the name of the workspace whose run holds the seat, empty
	// for a run started outside agent-repl.
	Workspace string
	// Rank is the holder's rank (zero is the best).
	Rank int
	// Size is what the holder's seat costs the host, in the pool's unit (a
	// RAM disk's MiB).
	Size int
}

// Why is why an admitting taker waits.
type Why int

const (
	// Outranked: a taker of a better rank waits.
	Outranked Why = iota + 1
	// Full: every seat is held.
	Full
	// Refused: a seat is free, but the admission check refused it.
	Refused
)

// Wait is one wait of an admitting taker: why, and every held seat's holder.
type Wait struct {
	Why Why
	// Held is every held seat's holder, in seat order.
	Held []Holder
	// at is the held seats' indexes, Held's order: the seats a full or
	// refused taker watches.
	at []int
}

// Admission is what Admit takes a seat with.
type Admission struct {
	// Me is the taker's record, written into the seat it takes.
	Me Holder
	// Check decides, under the turnstile, whether the taker may take a free
	// seat while held hold theirs. It is asked only while a seat is free and
	// no better rank waits.
	Check func(held []Holder) (bool, error)
	// Waiting is told each time the taker begins to wait, why, and who holds
	// the seats; an error it answers ends the wait, and Admit answers it.
	Waiting func(Wait) error
	// Recheck answers a channel that fires when a refused taker should check
	// again with no seat let go.
	Recheck func() <-chan time.Time
}

// Seat is one admitted seat; Release gives it back.
type Seat struct {
	// Index is the seat's number in the pool.
	Index int
	// Held is every OTHER seat's holder when this one was taken.
	Held        []Holder
	lock, watch *os.File
}

// Release lets the seat go: its lock first, so a taker woken by the watch
// finds the seat free, then its watch.
func (s *Seat) Release() error {
	var errs []error
	if err := s.lock.Close(); err != nil {
		errs = append(errs, fmt.Errorf("close the seat %s: %w", s.lock.Name(), err))
	}
	if err := s.watch.Close(); err != nil {
		errs = append(errs, fmt.Errorf("close the seat's watch %s: %w", s.watch.Name(), err))
	}
	return errors.Join(errs...)
}

// watchPath is seat i's watch lock.
func (p Pool) watchPath(i int) string {
	return filepath.Join(p.Dir, fmt.Sprintf("%s-%d.watch", p.Prefix, i))
}

// Admit takes one of the pool's seats once a.Check admits it and no better
// rank waits, waiting, unbounded, until then.
func (p Pool) Admit(a Admission) (*Seat, error) {
	if p.N < 1 {
		return nil, fmt.Errorf("the %s seat pool %s has %d seats; it needs at least one", p.What, p.Dir, p.N)
	}
	if a.Check == nil || a.Waiting == nil || a.Recheck == nil {
		return nil, fmt.Errorf("take a %s seat: an admission needs its Check, Waiting and Recheck", p.What)
	}
	if a.Me.Rank < 0 {
		return nil, fmt.Errorf("take a %s seat at rank %d: a rank is zero or more", p.What, a.Me.Rank)
	}
	if err := os.MkdirAll(p.Dir, 0o755); err != nil {
		return nil, fmt.Errorf("make the %s seats directory: %w", p.What, err)
	}
	lane, err := p.lock(p.lanePath(a.Me.Rank), syscall.LOCK_SH)
	if err != nil {
		return nil, err
	}
	// Closing the lane's file drops the taker's share of it: the deferred
	// close is the one release, on every path.
	defer lane.Close()
	w := &watcher{p: p, watching: map[int]bool{}, woke: make(chan woken, p.N)}
	for {
		seat, wait, above, err := p.tryAdmit(a)
		if err != nil {
			return nil, err
		}
		if seat != nil {
			return seat, nil
		}
		if err := a.Waiting(wait); err != nil {
			return nil, err
		}
		switch wait.Why {
		case Outranked:
			empty, err := p.lock(p.lanePath(above), syscall.LOCK_EX)
			if err != nil {
				return nil, err
			}
			if err := empty.Close(); err != nil {
				return nil, fmt.Errorf("close the %s seats' lane %s: %w", p.What, p.lanePath(above), err)
			}
		case Full:
			if err := w.untilLetGo(wait.at, nil); err != nil {
				return nil, err
			}
		case Refused:
			if err := w.untilLetGo(wait.at, a.Recheck()); err != nil {
				return nil, err
			}
		}
	}
}

// tryAdmit is one turn under the turnstile: a seat taken, or the wait that
// follows (and, outranked, the better rank it waits for).
func (p Pool) tryAdmit(a Admission) (*Seat, Wait, int, error) {
	turnstile, err := p.lock(p.turnstilePath(), syscall.LOCK_EX)
	if err != nil {
		return nil, Wait{}, 0, err
	}
	free, held, err := p.survey()
	if err != nil {
		return nil, Wait{}, 0, errors.Join(err, turnstile.Close())
	}
	// let goes back, closing the free seat this turn holds (if any) and the
	// turnstile, joining their errors to err.
	let := func(err error) error {
		if free != nil {
			err = errors.Join(err, free.f.Close())
		}
		return errors.Join(err, turnstile.Close())
	}
	above, err := p.waiterAbove(a.Me.Rank)
	if err != nil {
		return nil, Wait{}, 0, let(err)
	}
	others, at := holders(held)
	if above >= 0 {
		return nil, Wait{Why: Outranked, Held: others, at: at}, above, let(nil)
	}
	if free == nil {
		return nil, Wait{Why: Full, Held: others, at: at}, 0, let(nil)
	}
	admitted, err := a.Check(others)
	if err != nil || !admitted {
		return nil, Wait{Why: Refused, Held: others, at: at}, 0, let(err)
	}
	// The watch is held before the turnstile goes, so every taker that sees
	// this seat held finds its watch held too. A waiter's shared hold of it is
	// only ever momentary, so this waits at most that.
	watch, err := p.lock(p.watchPath(free.i), syscall.LOCK_EX)
	if err != nil {
		return nil, Wait{}, 0, let(err)
	}
	if err := writeHolder(free.f, a.Me); err != nil {
		return nil, Wait{}, 0, errors.Join(let(err), watch.Close())
	}
	if err := turnstile.Close(); err != nil {
		return nil, Wait{}, 0, errors.Join(fmt.Errorf("let the %s seats' turnstile go: %w", p.What, err), free.f.Close(), watch.Close())
	}
	return &Seat{Index: free.i, Held: others, lock: free.f, watch: watch}, Wait{}, 0, nil
}

// freeSeat is the first free seat a survey found, held by the surveyor.
type freeSeat struct {
	i int
	f *os.File
}

// survey reads every seat under the turnstile: the first free one, which it
// holds, and every seat's holder, a free seat's entry the zero Holder. A held
// seat whose record does not read is an error: every holder writes its record
// before it lets the turnstile go.
func (p Pool) survey() (*freeSeat, []Holder, error) {
	var free *freeSeat
	held := make([]Holder, p.N)
	fail := func(err error) (*freeSeat, []Holder, error) {
		if free != nil {
			err = errors.Join(err, free.f.Close())
		}
		return nil, nil, err
	}
	for i := range p.N {
		f, err := p.TryTake(i)
		if err != nil {
			return fail(err)
		}
		if f != nil {
			if free == nil {
				free = &freeSeat{i: i, f: f}
			} else if err := f.Close(); err != nil {
				return fail(fmt.Errorf("let the probed %s seat %s go: %w", p.What, p.Path(i), err))
			}
			continue
		}
		raw, err := os.ReadFile(p.Path(i))
		if err != nil {
			return fail(fmt.Errorf("read the holder of the %s seat %s: %w", p.What, p.Path(i), err))
		}
		h, err := parseHolder(string(raw))
		if err != nil {
			return fail(fmt.Errorf("the %s seat %s is held, and its holder's record %q does not read: %w", p.What, p.Path(i), raw, err))
		}
		held[i] = h
	}
	return free, held, nil
}

// holders is a survey's held seats alone, in seat order: their records and
// their indexes.
func holders(held []Holder) ([]Holder, []int) {
	var out []Holder
	var at []int
	for i, h := range held {
		if h != (Holder{}) {
			out, at = append(out, h), append(at, i)
		}
	}
	return out, at
}

// writeHolder writes h into its seat's lock file, replacing the last
// holder's record.
func writeHolder(f *os.File, h Holder) error {
	body := fmt.Sprintf("pid=%d\nworkspace=%s\nrank=%d\nsize=%d\n", h.Pid, strconv.Quote(h.Workspace), h.Rank, h.Size)
	if err := f.Truncate(0); err != nil {
		return fmt.Errorf("empty the seat %s to name its holder: %w", f.Name(), err)
	}
	if _, err := f.WriteAt([]byte(body), 0); err != nil {
		return fmt.Errorf("name the holder in the seat %s: %w", f.Name(), err)
	}
	return nil
}

// parseHolder reads a seat's record; every field must be there and read.
func parseHolder(raw string) (Holder, error) {
	var h Holder
	seen := map[string]bool{}
	for line := range strings.SplitSeq(strings.TrimSpace(raw), "\n") {
		key, v, _ := strings.Cut(line, "=")
		var err error
		switch key {
		case "pid":
			h.Pid, err = strconv.Atoi(v)
		case "workspace":
			h.Workspace, err = strconv.Unquote(v)
		case "rank":
			h.Rank, err = strconv.Atoi(v)
		case "size":
			h.Size, err = strconv.Atoi(v)
		default:
			return Holder{}, fmt.Errorf("an unknown line %q", line)
		}
		if err != nil {
			return Holder{}, fmt.Errorf("%s=%q: %w", key, v, err)
		}
		seen[key] = true
	}
	if len(seen) != 4 || h.Pid <= 0 {
		return Holder{}, errors.New("it names no pid, workspace, rank and size")
	}
	return h, nil
}

// woken is a watched seat let go, or a watch that failed.
type woken struct {
	i   int
	err error
}

// watcher keeps at most one goroutine per seat blocked on that seat's watch,
// across every wait of one Admit.
type watcher struct {
	p        Pool
	watching map[int]bool
	woke     chan woken
}

// untilLetGo returns once any of the seats held is let go (or its holder
// dies), or recheck fires (a nil recheck never does). A seat already watched
// from an earlier wait keeps its one goroutine.
func (w *watcher) untilLetGo(held []int, recheck <-chan time.Time) error {
	for _, i := range held {
		if w.watching[i] {
			continue
		}
		w.watching[i] = true
		go func() {
			f, err := w.p.lock(w.p.watchPath(i), syscall.LOCK_SH)
			if err == nil {
				err = f.Close()
			}
			// woke holds one slot per seat, and a seat has one goroutine at a
			// time, so the send never blocks, even after Admit has returned.
			w.woke <- woken{i: i, err: err}
		}()
	}
	select {
	case got := <-w.woke:
		delete(w.watching, got.i)
		if got.err != nil {
			return fmt.Errorf("watch the %s seat %d: %w", w.p.What, got.i, got.err)
		}
	case <-recheck:
	}
	return nil
}
