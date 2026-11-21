package cores

import "agentrepl/testrun/internal/proctable"

// THE TESTS' OWN CPU (owner ruling, 2026-11-13: the cap is the tests' share,
// so the rest of the host's load must not eat it). Every process a test
// run's unit starts carries the run's mark (NestedEnv) in the environment it
// inherits, whatever group or session it moves to; TestsCPU sums the CPU of
// every marked process on the host, every run's alike (proctable.Marked).
//
// IT COUNTS EVERY CPU SECOND ONCE, ACROSS PROCESS CHURN. A scan sees only the
// processes alive at it, and a test run's processes are mostly short (a
// go build's compile and link steps live well under a second), so the sum of
// the live processes' use would miss most of them and fall whenever one
// exits. Instead each read adds what every live marked process used since the
// last read, its own CPU and its reaped children's, which the kernel adds to
// a parent when it waits for a child:
//
//   - a process new since the last read adds all it has used, its reaped
//     children's included, so a child that lived and was reaped entirely
//     between two reads is counted in its parent;
//   - a process seen at the last read and gone now was reaped, and the kernel
//     folded its whole use into its parent's children: when that parent is a
//     marked process still alive, what the gone process had already been
//     counted for is taken off again, so its parent counts it once;
//   - a process whose parent is not marked (a unit's own leader, which the
//     runner reaps) or is gone loses only its use since the last read.
//
// The very first read only takes the baseline. A total can therefore dip for
// a read (a child gone from the scan before its parent reaped it), never by
// more than that child's use, and the next read makes it up; the governor
// averages rates over AverageTicks reads.
type TestsCPU struct {
	// Scan answers every marked process alive now (proctable.Marked.Scan).
	Scan func() ([]proctable.Usage, error)

	primed bool
	total  float64
	seen   map[procKey]seenProc
}

// procKey is one process: its pid and its start, which tell it from a later
// process given the same pid.
type procKey struct {
	pid   int
	start int64
}

// seenProc is what the last read counted of a process.
type seenProc struct {
	ppid  int
	mark  string
	total float64
}

// Read answers the CPU seconds every marked process has used since the first
// read.
func (a *TestsCPU) Read() (float64, error) {
	growth, err := a.ReadByMark()
	if err != nil {
		return 0, err
	}
	for _, g := range growth {
		a.total += g
	}
	return a.total, nil
}

// ReadByMark answers the CPU seconds the marked processes used since the last
// read, by each process's mark value (proctable.Usage.Mark), counted as Read
// counts them: a reaped process's use is taken off its own mark when its
// living marked parent now holds it. The first read only takes the baseline
// and answers nothing.
func (a *TestsCPU) ReadByMark() (map[string]float64, error) {
	growth, err := a.ReadGrowthByMark()
	if err != nil {
		return nil, err
	}
	cpu := make(map[string]float64, len(growth))
	for mark, g := range growth {
		cpu[mark] = g.CPU
	}
	return cpu, nil
}

// MarkGrowth is what one mark's processes did since the last read.
type MarkGrowth struct {
	// CPU is the CPU seconds they used, counted as ReadByMark counts it.
	CPU float64
	// Started is how many processes carrying the mark this read saw that the
	// last did not. A process that started and was reaped between two reads
	// is never seen, so it is not counted here (its CPU still is, through its
	// parent).
	Started int
}

// ReadGrowthByMark is ReadByMark with each mark's started processes beside
// its CPU. The first read only takes the baseline and answers nothing.
func (a *TestsCPU) ReadGrowthByMark() (map[string]MarkGrowth, error) {
	procs, err := a.Scan()
	if err != nil {
		return nil, err
	}
	now := make(map[procKey]seenProc, len(procs))
	byPid := make(map[int]procKey, len(procs))
	growth := map[string]MarkGrowth{}
	add := func(mark string, cpu float64, started int) {
		g := growth[mark]
		g.CPU += cpu
		g.Started += started
		growth[mark] = g
	}
	for _, p := range procs {
		k := procKey{pid: p.Pid, start: p.Start.UnixNano()}
		total := p.Own + p.Children
		if prev, ok := a.seen[k]; ok {
			add(p.Mark, total-prev.total, 0)
		} else if a.primed {
			add(p.Mark, total, 1)
		}
		now[k] = seenProc{ppid: p.Ppid, mark: p.Mark, total: total}
		byPid[p.Pid] = k
	}
	for k, gone := range a.seen {
		if _, alive := now[k]; alive {
			continue
		}
		if _, parentAlive := byPid[gone.ppid]; parentAlive {
			add(gone.mark, -gone.total, 0)
		}
	}
	a.seen, a.primed = now, true
	return growth, nil
}
