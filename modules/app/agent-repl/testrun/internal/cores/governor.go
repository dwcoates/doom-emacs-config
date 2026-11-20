package cores

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"time"

	"agentrepl/logging/hostcpu"
	"agentrepl/logging/hostpower"
	"agentrepl/testrun/internal/atomicfile"
	"agentrepl/testrun/internal/seats"
)

// THE CPU GOVERNOR (owner ruling, 2026-11-13, replacing the 2026-10-26 and
// 2026-10-30 rulings that capped the whole host at one share): every test on
// the host together should average the limits' Target of the host's cores
// (35%, 5.6 of 16), its TARGET, and the tests must never bring the host's
// total CPU above the limits' Ceiling (50%, 8 of 16), its CEILING. The target
// is the tests' share: the owner's other work no longer eats it, it only
// lowers what the ceiling leaves.
//
// THE LIMITS FOLLOW THE HOST'S POWER (owner ruling, 2026-11-17). On AC power
// in High Power Mode the tests may average 50% and the host reach 55%
// (PoweredLimits); on battery, or in any other energy mode, the limits are
// 35% and 50% (BaseLimits). A closed lid under persistent Wi-Fi keeps High
// Power Mode on AC power, so it keeps the higher limits, and unplugging
// returns the base ones whether the lid is open or closed. The governor reads
// the power at every read, so a change governs the next unit admitted; a unit
// already running runs to its end.
//
// IT TELLS THE TESTS' CPU FROM THE REST OF THE HOST'S. Every read measures
// the host's busy cores (hostcpu) and the cores every test process used
// (TestsCPU: each unit's whole process tree, every run's alike), and the rest
// of the host is the difference. Both are the AVERAGE of the last AverageTicks
// one-second reads, never the busiest read (owner ruling, 2026-11-13).
//
// IT COSTS EACH UNIT BY ITS OWN HISTORY. A unit asks for seats with the cores
// its own passing runs used (plan.Build, from the runner's per-unit CPU
// accounting), or DefaultCoresPerSlot a slot when it has none, and states it
// in the ledger (ledger.go). A unit that just started counts at those cores
// for RampWindow, until the averaged reads hold its measured use; after that
// its measured use stands for it. A unit is admitted when
//
//	tests + its cores <= the target, and
//	rest of the host + tests + its cores <= the ceiling,
//
// with tests the tests' measured cores plus every just-started unit's.
//
// THE FIRST UNIT IS NEVER STARVED. While no test unit runs, the next unit
// starts whenever the rest of the host is under the ceiling, whatever its
// cost: one unit cannot overshoot by more than itself, and a unit costing
// more than the target (a daemon integration batch measured 6.1 cores) could
// otherwise never start. So a unit heavier than what the others leave waits
// for them to end, and then runs alone.
//
// THE GOVERNOR ADMITS, IT NEVER PAUSES. A running unit always runs to its
// end: the governor only decides whether the next unit may start, at the
// moment it asks (the ledger wakes the governor, wake.go), at every read,
// and whenever a unit ends. Pausing (SIGSTOP) a running unit would hold its
// time limit running and its sockets and child processes unanswered, so a
// test bound measured on an idle host would fail for the stop.
//
// IT SPEAKS THROUGH THE SAME KERNEL LOCKS AS THE SEATS. A unit starts only
// once it holds its seats (Host.Hold), so the governor holds back what the
// tests may not take: it takes free seats itself, and while the next unit may
// not start it holds the pool's turnstile, so no taker can even begin to
// gather. When the next unit may start, the governor lets the turnstile and
// that unit's width of seats go. Every waiting unit waits on the kernel's
// lock and starts the moment the governor lets it go; nothing polls.
//
// ONE GOVERNOR ON THE HOST, HELD BY A KERNEL LOCK. Every run starts one
// (Governor.Run), and it governs only once it holds the pool's governor role
// (seats.Pool.Exclusive): the first run governs, and every later one waits on
// the role's lock. A governor's run that exits drops the role, its seats and
// its turnstile with its locks, and the next waiting run governs at once.
//
// IT ERRS LOW. A unit that just started counts at its expected cores on top
// of whatever of its use the reads already show; held seats no ledger record
// accounts for (a run of an earlier testrun, or a unit in the instant between
// taking its seats and stating it) count DefaultCoresPerSlot each for
// RampWindow; a waiter seen on a rank's lane with no record is costed as the
// widest unit at DefaultCoresPerSlot a slot; and a governor that cannot read
// the host, the tests or the ledger hands out nothing.

// Limits are the tests' target and the host's ceiling, each a percent of the
// host's cores.
type Limits struct {
	// Target is the share of the host's cores every test on the host
	// together should average.
	Target int
	// Ceiling is the share of the host's cores the whole host may not pass
	// while tests add to it.
	Ceiling int
}

var (
	// BaseLimits are the limits on battery, or in any energy mode but High
	// Power Mode (owner rulings 2026-10-30, which set the target at 35, and
	// 2026-11-13, which made it the tests' own share under a 50 ceiling).
	BaseLimits = Limits{Target: 35, Ceiling: 50}
	// PoweredLimits are the limits on AC power in High Power Mode (owner
	// rulings, 2026-11-17 and 2026-11-20: 55 and 70 made the owner's own work
	// slow while a merge gate ran, so the ceiling sits 5 points above the
	// target).
	PoweredLimits = Limits{Target: 50, Ceiling: 55}
)

// TargetCores is the tests' target on a host of numCPU cores.
func (l Limits) TargetCores(numCPU int) float64 { return float64(numCPU) * float64(l.Target) / 100 }

// CeilingCores is the host's ceiling on a host of numCPU cores.
func (l Limits) CeilingCores(numCPU int) float64 {
	return float64(numCPU) * float64(l.Ceiling) / 100
}

// LimitsSaid names both limits for the log: the powered ones and the base.
func LimitsSaid() string {
	return fmt.Sprintf("the tests to an average of %d%% of the host and the host to %d%% on AC power in High Power Mode, and to %d%% and %d%% otherwise", PoweredLimits.Target, PoweredLimits.Ceiling, BaseLimits.Target, BaseLimits.Ceiling)
}

// HostPower is the power the host runs on, as the governor reads it.
type HostPower struct {
	// Source is AC power or the battery.
	Source hostpower.Power
	// EnergyMode is `pmset`'s powermode verbatim; empty when the host has
	// none.
	EnergyMode string
}

// Limits answers the limits the host's power allows.
func (p HostPower) Limits() Limits {
	if p.Source == hostpower.PowerAC && p.EnergyMode == hostpower.EnergyModeHighPower {
		return PoweredLimits
	}
	return BaseLimits
}

// String names the power for the log.
func (p HostPower) String() string {
	mode := p.EnergyMode
	switch mode {
	case hostpower.EnergyModeHighPower:
		mode = "High Power Mode"
	case hostpower.EnergyModeLowPower:
		mode = "Low Power Mode"
	case "":
		mode = "no energy mode"
	default:
		mode = "energy mode " + mode
	}
	return fmt.Sprintf("%s power in %s", p.Source, mode)
}

// ReadHostPower reads the source SOURCE says the host runs on and the
// energy mode ENERGYMODE reads.
func ReadHostPower(source hostpower.PowerSource, energyMode func() (string, error)) (HostPower, error) {
	power, err := source.Power()
	if err != nil {
		return HostPower{}, err
	}
	mode, err := energyMode()
	if err != nil {
		return HostPower{}, err
	}
	return HostPower{Source: power, EnergyMode: mode}, nil
}

const (
	// FirstUnitSeats is the widest unit's width (roster's Slots, asserted by
	// roster_test.go): the seats the governor lets go for a waiter whose
	// width it does not know.
	FirstUnitSeats = 2
	// DefaultCoresPerSlot is what a unit with no measured history is costed
	// a slot of its width. Measured 2026-11-13 over 93 merge-gate units of
	// over 5s: median 1.12 cores, p90 1.48, the heaviest 6.11 for two slots,
	// and units were earlier measured using up to 2.25 cores a slot; two a
	// slot is above nine units in ten, so an unmeasured unit is rarely
	// undercosted.
	DefaultCoresPerSlot = 2.0
	// UngovernedSeatCores is how many cores an ungoverned run's seat stands
	// for (PlanFor): a seat per core oversubscribed the host about 2.4 times
	// (2026-11-12).
	UngovernedSeatCores = 2.0
	// Tick is how often the governor reads the host's and the tests' CPU.
	Tick = time.Second
	// AverageTicks is how many of the last reads the governor's load is the
	// average of (owner ruling, 2026-11-13: the average of the last 3
	// seconds).
	AverageTicks = 3
	// RampWindow is how long a unit that started counts at its expected
	// cores: until every one of the AverageTicks reads averaged began after it
	// started, one read more for the read under way when it started.
	RampWindow = (AverageTicks + 1) * Tick
	// maxRank is the worst rank a run can have: niceness 20, macOS's floor.
	maxRank = 20
)

// ExpectedCores is what a unit of width is costed: what its own history
// measured, or DefaultCoresPerSlot a slot when it has none.
func ExpectedCores(width int, measured float64, known bool) float64 {
	if known && measured > 0 {
		return measured
	}
	return DefaultCoresPerSlot * float64(width)
}

// Load is the CPU the governor decides on, in cores.
type Load struct {
	// HostCores is the whole host's busy cores, averaged.
	HostCores float64
	// TestsCores is the cores the tests' processes used, averaged.
	TestsCores float64
	// RampCores is the cores of units started too recently for the averaged
	// reads to hold.
	RampCores float64
	// Running is whether any test unit runs: holds seats or just started.
	Running bool
}

// Tests is the tests' load: what they were measured using, and what the
// units that just started are expected to.
func (l Load) Tests() float64 { return l.TestsCores + l.RampCores }

// Other is the rest of the host's load.
func (l Load) Other() float64 { return max(0, l.HostCores-l.TestsCores) }

// Admits answers whether a unit expected to use cores may start on a host of
// numCPU cores under l: within limits' target and ceiling, or,
// while no unit runs, whenever the rest of the host is under the ceiling.
func Admits(numCPU int, limits Limits, l Load, cores float64) bool {
	ceiling := limits.CeilingCores(numCPU)
	if !l.Running {
		return l.Other() < ceiling
	}
	tests := l.Tests() + cores
	return tests <= limits.TargetCores(numCPU) && l.Other()+tests <= ceiling
}

// Request is the unit the governor decides on next.
type Request struct {
	Unit  string
	Rank  int
	Width int
	Cores float64
	// Recorded is false for a waiter seen only on its rank's lane.
	Recorded bool
}

// StateFile is the file the governor states the host's CPU in for every
// waiting unit to read (CPUState).
const StateFile = "core.cpu"

// CPUState is what the governor last stated about the host's CPU.
type CPUState struct {
	// WaitingForCPU is true while the governor holds the next unit back for
	// the CPU.
	WaitingForCPU bool `json:"waiting_for_cpu"`
	// BusyPercent is the host's busyness the governor last read, 0-100.
	BusyPercent float64 `json:"busy_percent"`
	// TestsPercent is the tests' load the governor last counted, 0-100.
	TestsPercent float64 `json:"tests_percent"`
	// CapPercent is the tests' target.
	CapPercent int `json:"cap_percent"`
	// CeilingPercent is the host's ceiling.
	CeilingPercent int `json:"ceiling_percent"`
}

// ReadState reads what the governor last stated under dir. A missing file is
// a host no governor has stated anything on yet, which holds no unit back
// for the CPU: a unit waiting then waits for seats.
func ReadState(dir string) (CPUState, error) {
	data, err := os.ReadFile(filepath.Join(dir, StateFile))
	if errors.Is(err, os.ErrNotExist) {
		return CPUState{CapPercent: BaseLimits.Target, CeilingPercent: BaseLimits.Ceiling}, nil
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
	if err := atomicfile.Replace(filepath.Join(dir, StateFile), StateFile+".*", data); err != nil {
		return fmt.Errorf("write the CPU governor's state: %w", err)
	}
	return nil
}

// GovernorLogger is what the governor says on.
type GovernorLogger interface {
	Infof(format string, args ...any)
	Errorf(format string, args ...any)
}

// Governor holds the host's tests to their target and the host to its
// ceiling.
type Governor struct {
	Pool   seats.Pool
	NumCPU int
	// Read reads the host's CPU times (hostcpu.Read).
	Read func() (hostcpu.Times, error)
	// Tests reads the CPU seconds the tests' processes used (TestsCPU.Read).
	Tests func() (float64, error)
	// Power reads the power the host runs on (ReadHostPower), which sets
	// the limits.
	Power func() (HostPower, error)
	// Now is the clock a ledger record's start is compared with.
	Now func() time.Time
	// Ticks paces the reads, one per Tick.
	Ticks <-chan time.Time
	Log   GovernorLogger
	// Stepped, when set, is told each decision once the governor has acted
	// on it, and the same decision again, Gated updated, once a turnstile it
	// waited for arrives.
	Stepped func(Decision)
}

// Decision is what one decision decided.
type Decision struct {
	// Measured is false until two reads give the host's and the tests'
	// load, and while a read fails.
	Measured bool
	// Load is the load the decision counted.
	Load Load
	// Limits are the limits the decision held the load to.
	Limits Limits
	// TestSeats is how many seats tests held.
	TestSeats int
	// Next is the unit decided on; Waiting is false when no unit waits.
	Next    Request
	Waiting bool
	// Admit is whether Next may start.
	Admit bool
	// Withheld is how many seats the governor holds back.
	Withheld int
	// Gated is whether the governor holds the turnstile.
	Gated bool
}

// governing is one governor's hold on the pool.
type governing struct {
	// power is the host's power last read; havePower is false until it
	// is read, and while a read fails.
	power     HostPower
	havePower bool
	prevHost  hostcpu.Times
	prevTests float64
	havePrev  bool
	host      []float64
	tests     []float64
	// unrecorded is the count of held seats no ledger record accounted for,
	// at each of the last reads.
	unrecorded []int
	withheld   map[int]*os.File
	gate       *os.File
	// pending is the turnstile the governor waits for while a taker
	// gathers; a gather in progress cannot be interrupted.
	pending  chan lockResult
	stated   bool
	waiting  bool
	last     Decision
	lastErrs map[string]string
}

// lockResult is a lock the governor waited for on another goroutine, since a
// flock that waits cannot be interrupted.
type lockResult struct {
	f   *os.File
	err error
}

// awaitLock holds the lock lock answers, on another goroutine, and hands it
// over on the channel it answers.
func awaitLock(lock func() (*os.File, error)) chan lockResult {
	ch := make(chan lockResult, 1)
	go func() {
		f, err := lock()
		ch <- lockResult{f, err}
	}()
	return ch
}

// dropOnArrival lets a lock the governor stopped waiting for go the moment it
// arrives.
func dropOnArrival(ch <-chan lockResult) {
	go func() {
		if r := <-ch; r.f != nil {
			_ = r.f.Close()
		}
	}()
}

// Run governs the pool once it holds the governor role, until ctx ends.
func (g *Governor) Run(ctx context.Context) {
	role := awaitLock(func() (*os.File, error) { return g.Pool.Exclusive("governor") })
	var held lockResult
	select {
	case <-ctx.Done():
		// The role arrives, if ever, after this run stopped asking.
		dropOnArrival(role)
		return
	case held = <-role:
	}
	if held.err != nil {
		g.Log.Errorf("CPU governor: this run cannot take the governor's role, so another run must govern the host's tests: %v", held.err)
		return
	}
	defer held.f.Close()
	g.Log.Infof("CPU governor: this run governs every test on the host's %d cores, holding %s", g.NumCPU, LimitsSaid())
	wake, stopWake, err := listenWake(g.Pool.Dir)
	if err != nil {
		// Units are still decided on at every read; each waits up to a Tick
		// longer for its decision.
		g.Log.Errorf("CPU governor: cannot be woken by units asking for seats, so each waits for the next read to be decided on: %v", err)
	} else {
		defer func() {
			if err := stopWake(); err != nil {
				g.Log.Errorf("CPU governor: cannot close the wake FIFO: %v", err)
			}
		}()
	}
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
		case <-wake:
			g.decide(s, false)
		case <-g.Ticks:
			g.step(s)
		}
	}
}

// step reads the host and the tests once and decides on what it read.
func (g *Governor) step(s *governing) {
	g.readPower(s)
	g.read(s)
	g.decide(s, true)
}

// readPower reads the power the host runs on, and says when the limits it
// allows change.
func (g *Governor) readPower(s *governing) {
	power, err := g.Power()
	if err != nil {
		s.havePower = false
		g.failed(s, "power", "cannot read the host's power, so no test unit starts until it can", err)
		return
	}
	g.recovered(s, "power")
	if !s.havePower || power.Limits() != s.power.Limits() {
		l := power.Limits()
		g.Log.Infof("CPU governor: the host runs on %s, so the tests average at most %d%% (%.1f cores) and the host reaches at most %d%% (%.1f cores)", power, l.Target, l.TargetCores(g.NumCPU), l.Ceiling, l.CeilingCores(g.NumCPU))
	}
	s.power, s.havePower = power, true
}

// read takes one read of the host's and the tests' CPU. A failed read
// forgets the last one, so the next load is measured over two good reads.
func (g *Governor) read(s *governing) {
	host, err := g.Read()
	if err != nil {
		s.havePrev = false
		g.failed(s, "read", "cannot read the host's CPU load, so no test unit starts until it can", err)
		return
	}
	g.recovered(s, "read")
	tests, err := g.Tests()
	if err != nil {
		s.havePrev = false
		g.failed(s, "tests", "cannot read the tests' own CPU, so no test unit starts until it can", err)
		return
	}
	g.recovered(s, "tests")
	prevHost, prevTests, had := s.prevHost, s.prevTests, s.havePrev
	s.prevHost, s.prevTests, s.havePrev = host, tests, true
	if !had {
		return
	}
	busy, err := host.Busy(prevHost)
	if err != nil {
		s.havePrev = false
		g.failed(s, "busy", "cannot tell how busy the host was, so no test unit starts until it can", err)
		return
	}
	g.recovered(s, "busy")
	// The interval's length, read off the host's own counters: every core
	// spends each second in one state.
	seconds := (host.Total() - prevHost.Total()) / float64(g.NumCPU)
	s.host = lastN(append(s.host, busy*float64(g.NumCPU)), AverageTicks)
	s.tests = lastN(append(s.tests, (tests-prevTests)/seconds), AverageTicks)
}

// decide decides on the next unit and acts on it. A decision at a read also
// remembers how many held seats the ledger did not account for.
func (g *Governor) decide(s *governing, atRead bool) {
	d := Decision{Measured: s.havePower && s.havePrev && len(s.host) > 0, Limits: s.power.Limits()}
	tests, seatsOK := g.testSeats(s)
	d.TestSeats = tests
	records, ledgerOK := g.ledger(s)
	next, waiting, lanesOK := g.next(s, records)
	d.Next, d.Waiting = next, waiting
	if seatsOK && ledgerOK {
		d.Load = g.load(s, records, tests, atRead)
	}
	if d.Measured && seatsOK && ledgerOK && lanesOK && waiting {
		d.Admit = Admits(g.NumCPU, d.Limits, d.Load, next.Cores)
	}
	if d.Admit {
		g.openGate(s)
		g.withhold(s, g.Pool.N-tests-next.Width)
	} else {
		g.closeGate(s)
		g.withhold(s, g.Pool.N-tests)
	}
	d.Withheld, d.Gated = len(s.withheld), s.gate != nil
	s.last = d
	g.state(s, d, seatsOK && ledgerOK && lanesOK)
	g.stepped(d)
}

// load is the load a decision counts: the averaged reads, every unit that
// started within RampWindow at its expected cores, and every held seat no
// record accounts for that was taken within RampWindow at
// DefaultCoresPerSlot.
func (g *Governor) load(s *governing, records []UnitRecord, testSeats int, atRead bool) Load {
	l := Load{HostCores: mean(s.host), TestsCores: max(0, mean(s.tests))}
	now := g.Now()
	recorded := 0
	for _, r := range records {
		if r.Started.IsZero() {
			continue
		}
		recorded += r.Width
		if now.Sub(r.Started) < RampWindow {
			l.RampCores += r.Cores
		}
	}
	unrecorded := max(0, testSeats-recorded)
	if atRead {
		s.unrecorded = lastN(append(s.unrecorded, unrecorded), AverageTicks+1)
	}
	settled := unrecorded
	if len(s.unrecorded) > 0 {
		settled = min(settled, slices.Min(s.unrecorded))
	}
	l.RampCores += DefaultCoresPerSlot * float64(unrecorded-settled)
	l.Running = testSeats > 0 || l.RampCores > 0
	return l
}

// next answers the unit the governor decides on: of the best rank waiting
// (no other rank can gather while it waits, seats.Pool.TakeN), the costliest,
// since whichever of them gathers first starts. A rank whose lane is held
// with no record waiting at it is a run that states nothing in the ledger,
// costed as the widest unit at DefaultCoresPerSlot a slot.
func (g *Governor) next(s *governing, records []UnitRecord) (Request, bool, bool) {
	best := -1
	var at []UnitRecord
	for _, r := range records {
		if !r.Started.IsZero() {
			continue
		}
		switch {
		case best < 0 || r.Rank < best:
			best, at = r.Rank, []UnitRecord{r}
		case r.Rank == best:
			at = append(at, r)
		}
	}
	top := maxRank
	if best >= 0 {
		top = best - 1
	}
	for rank := 0; rank <= top; rank++ {
		held, err := g.Pool.Waiting(rank)
		if err != nil {
			g.failed(s, "lanes", "cannot tell which runs wait for seats, so no test unit starts until it can", err)
			return Request{}, false, false
		}
		if held {
			g.recovered(s, "lanes")
			return Request{Rank: rank, Width: FirstUnitSeats, Cores: DefaultCoresPerSlot * FirstUnitSeats}, true, true
		}
	}
	g.recovered(s, "lanes")
	if best < 0 {
		return Request{}, false, true
	}
	r := slices.MaxFunc(at, func(a, b UnitRecord) int {
		if a.Cores != b.Cores {
			if a.Cores < b.Cores {
				return -1
			}
			return 1
		}
		return b.Width - a.Width
	})
	width := 0
	for _, a := range at {
		width = max(width, a.Width)
	}
	return Request{Unit: r.Unit, Rank: r.Rank, Width: width, Cores: r.Cores, Recorded: true}, true, true
}

func (g *Governor) ledger(s *governing) ([]UnitRecord, bool) {
	records, err := readLedger(g.Pool.Dir)
	if err != nil {
		g.failed(s, "ledger", "cannot read what the units on the host cost, so no test unit starts until it can", err)
		return nil, false
	}
	g.recovered(s, "ledger")
	return records, true
}

func (g *Governor) stepped(d Decision) {
	if g.Stepped != nil {
		g.Stepped(d)
	}
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
// simply not taken; the next decision counts it as the tests'.
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
	s.pending = awaitLock(g.Pool.Turnstile)
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
// last decision holds the next unit back, and lets it go at once otherwise.
func (g *Governor) arrived(s *governing, r lockResult) {
	if r.err != nil {
		g.failed(s, "gate", "cannot hold the seats' turnstile", r.err)
		return
	}
	s.gate = r.f
	if s.last.Admit {
		g.openGate(s)
	}
}

// state says, on every change, whether units wait for the CPU: in the log,
// and in the state file every waiting unit reads.
func (g *Governor) state(s *governing, d Decision, readable bool) {
	waiting := !d.Measured || !readable || (d.Waiting && !d.Admit)
	n := float64(g.NumCPU)
	var said string
	switch {
	case !d.Measured || !readable:
		said = "CPU governor: waiting for CPU until the host's power and the host's and the tests' load are read; no test unit starts"
	case waiting:
		said = fmt.Sprintf("CPU governor: waiting for CPU; the next unit %s costs %.1f cores, and the tests use %.1f of their %.1f (%d%%) while the host is %.1f cores busy against its %.1f (%d%%) ceiling, so no test unit starts",
			unitName(d.Next), d.Next.Cores, d.Load.Tests(), d.Limits.TargetCores(g.NumCPU), d.Limits.Target, d.Load.HostCores, d.Limits.CeilingCores(g.NumCPU), d.Limits.Ceiling)
	default:
		said = fmt.Sprintf("CPU governor: CPU available; the tests use %.1f of their %.1f cores (%d%%) and the host is %.1f cores busy against its %.1f (%d%%) ceiling",
			d.Load.Tests(), d.Limits.TargetCores(g.NumCPU), d.Limits.Target, d.Load.HostCores, d.Limits.CeilingCores(g.NumCPU), d.Limits.Ceiling)
	}
	if s.stated && waiting == s.waiting {
		return
	}
	s.stated, s.waiting = true, waiting
	g.Log.Infof("%s", said)
	st := CPUState{WaitingForCPU: waiting, BusyPercent: 100 * d.Load.HostCores / n, TestsPercent: 100 * d.Load.Tests() / n, CapPercent: d.Limits.Target, CeilingPercent: d.Limits.Ceiling}
	if err := writeState(g.Pool.Dir, st); err != nil {
		g.failed(s, "state", "cannot state the host's CPU for waiting units", err)
		return
	}
	g.recovered(s, "state")
}

// unitName names a decided unit for the log.
func unitName(r Request) string {
	if !r.Recorded {
		return fmt.Sprintf("(a rank-%d run's, unstated)", r.Rank)
	}
	return r.Unit
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
	if s.pending != nil {
		dropOnArrival(s.pending)
	}
}

func mean(xs []float64) float64 {
	if len(xs) == 0 {
		return 0
	}
	sum := 0.0
	for _, x := range xs {
		sum += x
	}
	return sum / float64(len(xs))
}

func lastN[T any](xs []T, n int) []T {
	if len(xs) > n {
		return xs[len(xs)-n:]
	}
	return xs
}
