package cores

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"math"
	"os"
	"path/filepath"
	"slices"
	"time"

	"agentrepl/logging/hostcpu"
	"agentrepl/testrun/internal/seats"
)

// THE CPU GOVERNOR (owner ruling, 2026-10-26): every test and build on the
// host together may only bring the host's TOTAL CPU use up to CapPercent.
// What the tests may take is what the rest of the host leaves under the cap:
// on an idle host they take up to the whole pool, and on a host already at
// the cap they take nothing, and every unit of every run waits for the CPU
// ("waiting for CPU") until the host's load drops.
//
// THE GOVERNOR ADMITS, IT NEVER PAUSES. A running unit always runs to its
// end: the governor only decides whether the next unit may start. So it acts
// at a unit's start or end, and a unit holding its cores while the rest of
// the host grows busier holds them until it ends.
//
// IT SPEAKS THROUGH THE SAME KERNEL LOCKS AS THE SEATS. A unit starts only
// once it holds its seats (Host.Hold), so the governor holds back seats the
// tests may not have: it takes free seats itself, and while the host is at
// the cap it holds the pool's turnstile, so no taker can even begin to
// gather. Every waiting unit waits on the kernel's lock and starts the
// moment the governor lets a seat or the turnstile go; nothing polls.
//
// ONE GOVERNOR ON THE HOST, HELD BY A KERNEL LOCK. Every run starts one
// (Governor.Run), and it governs only once it holds the pool's governor role
// (seats.Pool.Exclusive): the first run governs, and every later one waits on
// the role's lock. A governor's run that exits drops the role, its seats and
// its turnstile with its locks, and the next waiting run governs at once.
//
// IT ERRS LOW. The cap is the most the host may reach, never a target:
// busyness is the peak of the last PeakTicks reads; MarginCores of the cap are
// never handed out; and a seat is costed at SeatCost cores, both when the
// governor hands one out and for every seat the tests took in the last
// RampTicks, whose load the host's reads may not show yet (units were
// measured using up to 2.25 cores per seat, 2026-10-26). A governor that
// cannot read the host's load hands out nothing.

const (
	// CapPercent is the most of the host's CPU every process on the host
	// together may use while tests or builds add to it (owner ruling,
	// 2026-10-26).
	CapPercent = 65
	// MarginCores is how many of the cap's cores are never handed out.
	MarginCores = 1.0
	// SeatCost is how many cores a seat is assumed to cost.
	SeatCost = 2.0
	// Tick is how often the governor reads the host's load.
	Tick = time.Second
	// PeakTicks is how many of the last reads the governor's busyness is the
	// peak of.
	PeakTicks = 3
	// RampTicks is how long, in reads, a seat the tests took is costed by
	// SeatCost on top of the host's reads.
	RampTicks = 3
)

// Allowance answers how many more seats the tests may take: the cores the
// cap leaves once the host's busiest recent read, the margin and the seats
// taken too recently to show in the reads are counted, at SeatCost cores a
// seat.
func Allowance(numCPU int, peakBusyCores float64, rampSeats int) int {
	headroom := float64(numCPU)*CapPercent/100 - MarginCores - peakBusyCores - SeatCost*float64(rampSeats)
	if headroom <= 0 {
		return 0
	}
	return int(math.Floor(headroom / SeatCost))
}

// StateFile is the file the governor states the host's CPU in for every
// waiting unit to read (CPUState).
const StateFile = "core.cpu"

// CPUState is what the governor last stated about the host's CPU.
type CPUState struct {
	// WaitingForCPU is true while the host is too busy for any unit to start.
	WaitingForCPU bool `json:"waiting_for_cpu"`
	// BusyPercent is the host's busyness the governor last read, 0-100.
	BusyPercent float64 `json:"busy_percent"`
	// CapPercent is the cap the governor holds the host to.
	CapPercent int `json:"cap_percent"`
}

// ReadState reads what the governor last stated under dir. A missing file is
// a host no governor has stated anything on yet, which holds no unit back
// for the CPU: a unit waiting then waits for seats.
func ReadState(dir string) (CPUState, error) {
	data, err := os.ReadFile(filepath.Join(dir, StateFile))
	if errors.Is(err, os.ErrNotExist) {
		return CPUState{CapPercent: CapPercent}, nil
	}
	if err != nil {
		return CPUState{}, fmt.Errorf("read the CPU governor's state: %w", err)
	}
	var s CPUState
	if err := json.Unmarshal(data, &s); err != nil {
		return CPUState{}, fmt.Errorf("read the CPU governor's state %s: %w", filepath.Join(dir, StateFile), err)
	}
	return s, nil
}

// writeState replaces the state file whole, so a reader never sees half of it.
func writeState(dir string, s CPUState) error {
	data, err := json.Marshal(s)
	if err != nil {
		return fmt.Errorf("encode the CPU governor's state: %w", err)
	}
	tmp, err := os.CreateTemp(dir, StateFile+".*")
	if err != nil {
		return fmt.Errorf("write the CPU governor's state: %w", err)
	}
	_, werr := tmp.Write(data)
	cerr := tmp.Close()
	if err := errors.Join(werr, cerr); err != nil {
		return errors.Join(fmt.Errorf("write the CPU governor's state %s: %w", tmp.Name(), err), os.Remove(tmp.Name()))
	}
	if err := os.Rename(tmp.Name(), filepath.Join(dir, StateFile)); err != nil {
		return errors.Join(fmt.Errorf("replace the CPU governor's state: %w", err), os.Remove(tmp.Name()))
	}
	return nil
}

// GovernorLogger is what the governor says on.
type GovernorLogger interface {
	Infof(format string, args ...any)
	Errorf(format string, args ...any)
}

// Governor holds the host's tests to the CPU the cap leaves them.
type Governor struct {
	Pool   seats.Pool
	NumCPU int
	// Read reads the host's CPU times (hostcpu.Read).
	Read func() (hostcpu.Times, error)
	// Ticks paces the reads, one per Tick.
	Ticks <-chan time.Time
	Log   GovernorLogger
	// Stepped, when set, is told each read's decision once the governor has
	// acted on it, and the same decision again, Gated updated, once a
	// turnstile it waited for arrives.
	Stepped func(Decision)
}

// Decision is what one read decided.
type Decision struct {
	// Measured is false until two reads give the host's busyness, and while a
	// read fails.
	Measured bool
	// BusyCores is the peak busyness the decision counted.
	BusyCores float64
	// TestSeats is how many seats tests held.
	TestSeats int
	// RampSeats is how many of them they took too recently to show in the
	// reads.
	RampSeats int
	// Allow is how many more seats the tests may take; zero closes the gate.
	Allow int
	// Withheld is how many seats the governor holds back.
	Withheld int
	// Gated is whether the governor holds the turnstile.
	Gated bool
}

// governing is one governor's hold on the pool.
type governing struct {
	prev     hostcpu.Times
	havePrev bool
	busy     []float64
	held     []int
	withheld map[int]*os.File
	gate     *os.File
	// pending is the turnstile the governor waits for while a taker
	// gathers; a gather in progress cannot be interrupted.
	pending  chan turnstileResult
	waiting  bool
	stated   bool
	last     Decision
	lastErrs map[string]string
}

type turnstileResult struct {
	f   *os.File
	err error
}

// Run governs the pool once it holds the governor role, until ctx ends.
func (g *Governor) Run(ctx context.Context) {
	role := make(chan turnstileResult, 1)
	go func() {
		f, err := g.Pool.Exclusive("governor")
		role <- turnstileResult{f, err}
	}()
	var held turnstileResult
	select {
	case <-ctx.Done():
		// The role arrives, if ever, after this run stopped asking.
		go func() {
			if r := <-role; r.f != nil {
				_ = r.f.Close()
			}
		}()
		return
	case held = <-role:
	}
	if held.err != nil {
		g.Log.Errorf("CPU governor: this run cannot take the governor's role, so another run must govern the host's tests: %v", held.err)
		return
	}
	defer held.f.Close()
	g.Log.Infof("CPU governor: this run governs every test on the host, holding the host to %d%% of its %d cores", CapPercent, g.NumCPU)
	s := &governing{withheld: map[int]*os.File{}, lastErrs: map[string]string{}}
	defer g.release(s)
	g.step(s)
	for {
		select {
		case <-ctx.Done():
			return
		case r := <-s.pending:
			s.pending = nil
			g.arrived(s, r)
			s.last.Gated = s.gate != nil
			g.stepped(s.last)
		case <-g.Ticks:
			g.step(s)
		}
	}
}

// step reads the host once and acts on what it decides.
func (g *Governor) step(s *governing) {
	d := Decision{}
	if busy, ok := g.readBusy(s); ok {
		s.busy = lastN(append(s.busy, busy*float64(g.NumCPU)), PeakTicks)
	}
	d.Measured = s.havePrev && len(s.busy) > 0
	tests, ok := g.testSeats(s)
	if ok {
		s.held = lastN(append(s.held, tests), RampTicks+1)
	}
	d.TestSeats = tests
	if d.Measured && ok {
		d.BusyCores = slices.Max(s.busy)
		d.RampSeats = max(0, tests-slices.Min(s.held))
		d.Allow = Allowance(g.NumCPU, d.BusyCores, d.RampSeats)
	}
	if d.Allow == 0 {
		g.closeGate(s)
		g.withhold(s, g.Pool.N-tests)
	} else {
		g.openGate(s)
		g.withhold(s, g.Pool.N-tests-d.Allow)
	}
	d.Withheld, d.Gated = len(s.withheld), s.gate != nil
	g.state(s, d)
	s.last = d
	g.stepped(d)
}

func (g *Governor) stepped(d Decision) {
	if g.Stepped != nil {
		g.Stepped(d)
	}
}

// readBusy reads the host's CPU times and answers its busyness since the
// last read. A failed read forgets the last one, so the next busyness is
// measured over two good reads.
func (g *Governor) readBusy(s *governing) (float64, bool) {
	now, err := g.Read()
	if err != nil {
		s.havePrev = false
		g.failed(s, "read", "cannot read the host's CPU load, so no test unit starts until it can", err)
		return 0, false
	}
	prev, had := s.prev, s.havePrev
	s.prev, s.havePrev = now, true
	if !had {
		return 0, false
	}
	busy, err := now.Busy(prev)
	if err != nil {
		g.failed(s, "busy", "cannot tell how busy the host was, so no test unit starts until it can", err)
		return 0, false
	}
	g.recovered(s, "read")
	g.recovered(s, "busy")
	return busy, true
}

// testSeats counts the seats tests hold: every held seat but the governor's.
func (g *Governor) testSeats(s *governing) (int, bool) {
	n := 0
	for i := range g.Pool.N {
		if _, ours := s.withheld[i]; ours {
			continue
		}
		held, err := g.Pool.Held(i)
		if err != nil {
			g.failed(s, "probe", "cannot tell which seats the tests hold, so no test unit starts until it can", err)
			return n, false
		}
		if held {
			n++
		}
	}
	g.recovered(s, "probe")
	return n, true
}

// withhold makes the governor hold target seats: it lets go of what it holds
// beyond target, and takes free seats up to it. A seat a taker wins first is
// simply not taken; the next read counts it as the tests'.
func (g *Governor) withhold(s *governing, target int) {
	target = max(target, 0)
	for i, f := range s.withheld {
		if len(s.withheld) <= target {
			break
		}
		delete(s.withheld, i)
		if err := f.Close(); err != nil {
			g.failed(s, "release", "cannot let a held-back seat go", err)
		}
	}
	for i := 0; i < g.Pool.N && len(s.withheld) < target; i++ {
		if _, ours := s.withheld[i]; ours {
			continue
		}
		f, err := g.Pool.TryTake(i)
		if err != nil {
			g.failed(s, "take", "cannot hold a seat back from the tests", err)
			return
		}
		if f != nil {
			s.withheld[i] = f
		}
	}
}

// closeGate holds the turnstile, so no taker can begin to gather. While a
// taker is mid-gather the governor waits for its gather to end.
func (g *Governor) closeGate(s *governing) {
	if s.gate != nil || s.pending != nil {
		return
	}
	f, err := g.Pool.TryTurnstile()
	if err != nil {
		g.failed(s, "gate", "cannot hold the seats' turnstile", err)
		return
	}
	if f != nil {
		s.gate = f
		return
	}
	pending := make(chan turnstileResult, 1)
	s.pending = pending
	go func() {
		f, err := g.Pool.Turnstile()
		pending <- turnstileResult{f, err}
	}()
}

// openGate lets the turnstile go.
func (g *Governor) openGate(s *governing) {
	if s.gate == nil {
		return
	}
	if err := s.gate.Close(); err != nil {
		g.failed(s, "gate", "cannot let the seats' turnstile go", err)
	}
	s.gate = nil
}

// arrived takes the turnstile the governor waited for: it keeps it while the
// host is still at the cap, and lets it go at once otherwise.
func (g *Governor) arrived(s *governing, r turnstileResult) {
	if r.err != nil {
		g.failed(s, "gate", "cannot hold the seats' turnstile", r.err)
		return
	}
	s.gate = r.f
	if !s.waiting {
		g.openGate(s)
	}
}

// state says, on every change, whether units wait for the CPU: in the log,
// and in the state file every waiting unit reads.
func (g *Governor) state(s *governing, d Decision) {
	waiting := d.Allow == 0
	busy := 100 * d.BusyCores / float64(g.NumCPU)
	if s.stated && waiting == s.waiting {
		return
	}
	s.waiting, s.stated = waiting, true
	switch {
	case !d.Measured:
		g.Log.Infof("CPU governor: waiting for CPU until the host's load is read; no test unit starts")
	case waiting:
		g.Log.Infof("CPU governor: waiting for CPU; the host is %.0f%% busy (%d test seat(s) held, %d taken in the last %ds) against the %d%% cap, so no test unit starts", busy, d.TestSeats, d.RampSeats, RampTicks, CapPercent)
	default:
		g.Log.Infof("CPU governor: CPU available; the host is %.0f%% busy against the %d%% cap, so tests may take %d more seat(s)", busy, CapPercent, d.Allow)
	}
	if err := writeState(g.Pool.Dir, CPUState{WaitingForCPU: waiting, BusyPercent: busy, CapPercent: CapPercent}); err != nil {
		g.failed(s, "state", "cannot state the host's CPU for waiting units", err)
		return
	}
	g.recovered(s, "state")
}

// failed says a failure once per distinct cause, at ERROR.
func (g *Governor) failed(s *governing, what, consequence string, err error) {
	if s.lastErrs[what] == err.Error() {
		return
	}
	s.lastErrs[what] = err.Error()
	g.Log.Errorf("CPU governor: %s: %v", consequence, err)
}

// recovered forgets a failure, so its cause is said again if it recurs.
func (g *Governor) recovered(s *governing, what string) {
	delete(s.lastErrs, what)
}

// release lets every lock the governor holds go, and the turnstile it still
// waits for go the moment it arrives.
func (g *Governor) release(s *governing) {
	g.openGate(s)
	g.withhold(s, 0)
	if pending := s.pending; pending != nil {
		go func() {
			if r := <-pending; r.f != nil {
				_ = r.f.Close()
			}
		}()
	}
}

func lastN[T any](xs []T, n int) []T {
	if len(xs) > n {
		return xs[len(xs)-n:]
	}
	return xs
}
