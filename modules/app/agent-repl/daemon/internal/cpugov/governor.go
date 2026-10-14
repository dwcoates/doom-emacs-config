// Package cpugov is the merge queue's PROCESSOR GOVERNOR: it decides when a
// speculative test gate -- one run ahead of its turn, against a chained base --
// may start, by the whole machine's processor load.
//
// THE BAND IS THE ADMISSION RULE (owner ruling, 2026-10-11). The governor
// keeps the host between Low (50%) and High (70%) of its processors whenever
// enough gates are waiting: the next gate is admitted only while the SMOOTHED
// load, once every running gate has settled into it, stands below Low, and
// only when its own mean cost is projected to keep that smoothed load at or
// under High. Nothing is admitted above High. Brief bursts past High from gates
// already admitted are acceptable: what keeps the machine responsive is the
// speculative gates' low scheduling priority (merge's gate runs them niced),
// not processor share held free.
//
// WHAT IS MEASURED. The host's own processor ticks (host_statistics'
// HOST_CPU_LOAD_INFO on macOS, /proc/stat on Linux): every tick of every
// processor spent in user, nice or system time is busy, host-wide and not just
// this daemon's tree, and per-process %cpu is never summed (it undercounts the
// host by about a third, measured against top). Each SampleEvery interval's
// busy fraction is folded into an exponential moving average over Tau (30s),
// which rides over a test run's bursts. Its lag is modelled rather than
// ignored: a gate admitted t seconds ago has reached only 1-exp(-t/Tau) of its
// eventual share of the average, so the share still to come is counted
// against the band before anything else is admitted.
//
// WHAT A GATE COSTS. A gate's mean cost in processors is LEARNED from the
// gates that ran to an exit (their CPU time, user and system, over their wall
// time: the kernel's wait accounting of the script and every descendant),
// seeded with 2.2: the measured 147% average scaled by the third summed
// per-process %cpu was found to undercount.
//
// ON AC POWER THE BAND IS LIFTED (owner ruling, 2026-10-13). The merge behind
// the head races it to land (merge's race.go), so on a host that is not on
// battery a speculative gate is admitted at once, bounded only by MaxGates;
// on battery, and while the power source is unread, the band decides. The
// power source is read with every sample, and a failure to read it is said
// once per distinct cause, at ERROR.
//
// PRIORITY IS HEAD-FIRST. A gate of the merge at its queue's head is never
// governed: it is granted at once and only counted. Waiting speculative gates
// are considered closest-to-head first, and a gate is never admitted while one
// closer to its head is deferred.
package cpugov

import (
	"context"
	"errors"
	"fmt"
	"math"
	"sort"
	"sync"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

// The band and the measured cost model's seeds.
const (
	// DefaultLow is the band's floor: a gate is admitted only while the
	// settled load stands below it.
	DefaultLow = 0.50
	// DefaultHigh is the ceiling no admission is projected to exceed.
	DefaultHigh = 0.70
	// DefaultMeanCores is a merge test run's mean cost, the seed the learned
	// mean starts from: 147% of one processor over a 6m48s run as summed
	// per-process %cpu, which undercounts the host by about a third.
	DefaultMeanCores = 2.2
	// LoadTau is the smoothing's time constant.
	LoadTau = 30 * time.Second
	// DefaultLearnRate is how far one observed run moves the learned mean.
	DefaultLearnRate = 0.3
	// MinLearnWall is the shortest run the mean is learned from: a run that
	// short is a gate that found little to do, and says nothing about a run.
	MinLearnWall = 10 * time.Second
	// SampleEvery is the production sampling cadence.
	SampleEvery = 5 * time.Second
	// QueueWindow is THE ONE SETTING for the merge queue's working window
	// (owner rulings, 2026-10-11): the head and the merges right behind it, up
	// to this many in all. It bounds both how many merges rebase in a chain and
	// how many test gates run at once; an entry beyond it waits untouched. The
	// band still decides whether a gate behind the head starts; the head's
	// always does.
	QueueWindow = 2
)

// Sample is the host's SMOOTHED load: the fraction of its processors busy,
// and how many it has.
type Sample struct {
	Utilization float64
	CPUs        int
}

// Ticks are the host's processor ticks since boot, summed over every
// processor: those spent busy (user, nice and system) and in all.
type Ticks struct {
	Busy, Total uint64
	CPUs        int
}

// Sampler reads the host's processor ticks.
type Sampler interface {
	Ticks() (Ticks, error)
}

// Power is the source the host runs on.
type Power int

const (
	// PowerUnknown is a power source not yet read, or unreadable: the band
	// decides.
	PowerUnknown Power = iota
	// PowerAC is a host on external power: speculative gates run at once.
	PowerAC
	// PowerBattery is a host on its battery: the band decides.
	PowerBattery
)

func (p Power) String() string {
	switch p {
	case PowerAC:
		return "ac"
	case PowerBattery:
		return "battery"
	default:
		return "unknown"
	}
}

// PowerSource reads the source the host runs on.
type PowerSource interface {
	Power() (Power, error)
}

// Request is one gate asking to run.
type Request struct {
	Workspace ids.WorkspaceID
	Repo      string
	// Rank is the merge's distance from its queue's head: 0 is the head, 1
	// the merge right behind it.
	Rank int
	// Head reports a gate of the merge at its queue's head, which is never
	// governed.
	Head bool
}

// Permit is one admitted gate. Release it exactly once when the gate ends.
type Permit struct {
	g       *Governor
	req     Request
	granted time.Time
	once    sync.Once
}

// Config is the governor's band and cost model.
type Config struct {
	Low, High   float64
	MeanCores   float64
	LearnRate   float64
	Tau         time.Duration
	MinLearnRun time.Duration
	// MaxGates bounds the gates running at once, the head's included.
	MaxGates int
}

// DefaultConfig is the production band and the measured cost seeds.
func DefaultConfig() Config {
	return Config{
		Low: DefaultLow, High: DefaultHigh, MeanCores: DefaultMeanCores,
		LearnRate: DefaultLearnRate, Tau: LoadTau, MinLearnRun: MinLearnWall, MaxGates: QueueWindow,
	}
}

// waiter is one speculative gate waiting for admission.
type waiter struct {
	req     Request
	ready   chan struct{}
	permit  *Permit
	reason  string
	arrival uint64
}

// Governor admits gates by the host's load.
type Governor struct {
	cfg     Config
	sampler Sampler
	source  PowerSource
	now     func() time.Time
	log     dlog.Logger

	mu        sync.Mutex
	sample    Sample
	sampled   bool
	sampleErr string
	// power is the source the host last read as running on, and powerErr
	// the standing cause it could not be read.
	power    Power
	powerErr string
	// last is the previous tick reading and when it was taken; primed says
	// one stands.
	last     Ticks
	lastAt   time.Time
	primed   bool
	mean     float64
	active   map[*Permit]struct{}
	waiting  []*waiter
	arrivals uint64
}

// New builds a governor. Every collaborator is required.
func New(cfg Config, sampler Sampler, source PowerSource, now func() time.Time, log dlog.Logger) (*Governor, error) {
	if sampler == nil || source == nil || now == nil || log == nil {
		return nil, errors.New("cpugov: a sampler, a power source, a clock and a logger are required")
	}
	if !(cfg.Low > 0 && cfg.Low < cfg.High && cfg.High <= 1) || cfg.MeanCores <= 0 ||
		cfg.LearnRate <= 0 || cfg.LearnRate > 1 || cfg.Tau <= 0 || cfg.MaxGates < 1 {
		return nil, fmt.Errorf("cpugov: the configuration %+v is not a band and a cost model", cfg)
	}
	return &Governor{cfg: cfg, sampler: sampler, source: source, now: now, log: log, mean: cfg.MeanCores, active: map[*Permit]struct{}{}}, nil
}

// Run samples the host every SampleEvery until ctx ends, re-deciding the
// waiting gates on every sample. The ticker is the sampling cadence, the
// kernel's own; nothing waits on it for synchronization.
func (g *Governor) Run(ctx context.Context) {
	g.observe()
	tick := time.NewTicker(SampleEvery)
	defer tick.Stop()
	for {
		select {
		case <-ctx.Done():
			return
		case <-tick.C:
			g.observe()
		}
	}
}

// observe reads the host's ticks and power source, folds the interval since
// the last reading into the smoothed load, and re-decides the waiting gates.
func (g *Governor) observe() {
	t, err := g.sampler.Ticks()
	power, powerErr := g.source.Power()
	now := g.now()
	g.mu.Lock()
	defer g.mu.Unlock()
	g.notePowerLocked(power, powerErr)
	g.foldLocked(t, err, now)
	g.decideLocked()
}

// notePowerLocked records the source the host runs on. An unreadable source
// leaves the band deciding, said once per distinct cause at ERROR; a change of
// source is said at INFO.
func (g *Governor) notePowerLocked(power Power, err error) {
	if err != nil {
		if err.Error() != g.powerErr {
			g.powerErr = err.Error()
			g.log.Error("daemon.cpugov.power", "could not read the host's power source; speculative gates stay under the load band until it reads", dlog.Context{
				"error": err.Error(), "was": g.power.String()})
		}
		g.power = PowerUnknown
		return
	}
	g.powerErr = ""
	if power == g.power {
		return
	}
	g.log.Info("daemon.cpugov.power", "the host's power source changed", dlog.Context{
		"was": g.power.String(), "power": power.String(), "band_lifted": power == PowerAC})
	g.power = power
}

// ObservePower installs the power source and re-decides the waiting gates:
// the sampling loop's step, driven directly by a caller that reads it itself.
func (g *Governor) ObservePower(power Power) {
	g.mu.Lock()
	defer g.mu.Unlock()
	g.notePowerLocked(power, nil)
	g.decideLocked()
}

// foldLocked folds one tick reading into the smoothed load.
func (g *Governor) foldLocked(t Ticks, err error, now time.Time) {
	if err == nil && t.CPUs <= 0 {
		err = fmt.Errorf("cpugov: the host reports %d processors", t.CPUs)
	}
	if err == nil && g.primed && (t.Total < g.last.Total || t.Busy < g.last.Busy) {
		err = fmt.Errorf("cpugov: the host's processor ticks went backwards (busy %d->%d, total %d->%d)", g.last.Busy, t.Busy, g.last.Total, t.Total)
	}
	if err != nil {
		// NO SAMPLE, NO ADMISSION BY THE BAND. A speculative gate the band
		// admitted blind could push the host past the ceiling; the head's gate
		// is never governed, and on AC power the band is lifted, so the queue
		// still moves. The failure is said once per distinct cause, at ERROR.
		if err.Error() != g.sampleErr {
			g.sampleErr = err.Error()
			g.log.Error("daemon.cpugov.sample", "could not read the host's load; the band admits no speculative gate until it reads", dlog.Context{"error": err.Error()})
		}
		g.sampled, g.primed = false, false
		return
	}
	if g.sampleErr != "" {
		g.log.Info("daemon.cpugov.sample", "the host's load reads again", dlog.Context{"cpus": t.CPUs})
		g.sampleErr = ""
	}
	if !g.primed || t.Total == g.last.Total {
		// THE FIRST READING ONLY PRIMES: a load is a difference of two.
		g.last, g.lastAt, g.primed = t, now, true
		return
	}
	interval := float64(t.Busy-g.last.Busy) / float64(t.Total-g.last.Total)
	weight := 1.0
	if g.sampled {
		weight = 1 - math.Exp(-now.Sub(g.lastAt).Seconds()/g.cfg.Tau.Seconds())
	}
	g.sample = Sample{Utilization: g.sample.Utilization + weight*(interval-g.sample.Utilization), CPUs: t.CPUs}
	g.sampled = true
	g.last, g.lastAt = t, now
}

// Observe installs one sample and re-decides the waiting gates: the sampling
// loop's step, driven directly by a caller that samples itself.
func (g *Governor) Observe(s Sample) {
	g.mu.Lock()
	defer g.mu.Unlock()
	g.sample, g.sampled = s, true
	g.decideLocked()
}

// Acquire admits one gate, blocking until it may run or ctx ends. A head gate
// is granted at once.
func (g *Governor) Acquire(ctx context.Context, req Request) (*Permit, error) {
	w := g.enqueue(req)
	select {
	case <-w.ready:
		return w.permit, nil
	case <-ctx.Done():
		g.mu.Lock()
		defer g.mu.Unlock()
		// A GRANT THAT RACED THE CANCEL IS GIVEN BACK, never leaked: the
		// grant and this read are under the one lock.
		select {
		case <-w.ready:
			g.releaseLocked(w.permit)
		default:
			g.dropWaiterLocked(w)
		}
		g.log.Info("daemon.cpugov.withdraw", "a speculative gate stopped waiting for admission", dlog.Context{
			"workspace": string(req.Workspace), "repo": req.Repo, "rank": req.Rank, "cause": context.Cause(ctx).Error()})
		g.decideLocked()
		return nil, context.Cause(ctx)
	}
}

// enqueue puts one gate in line and decides at once: a head gate, and a
// speculative gate the band admits now, answer a waiter already ready.
func (g *Governor) enqueue(req Request) *waiter {
	g.mu.Lock()
	defer g.mu.Unlock()
	g.arrivals++
	w := &waiter{req: req, ready: make(chan struct{}), arrival: g.arrivals}
	if req.Head {
		var pr projection
		if g.sampled {
			pr = g.projectLocked()
		}
		w.permit = g.grantLocked(req)
		close(w.ready)
		g.log.Info("daemon.cpugov.admit", "the head's gate runs at once; it is never governed", g.fieldsLocked(req, "admit", "head", pr))
		return w
	}
	g.waiting = append(g.waiting, w)
	sort.SliceStable(g.waiting, func(i, j int) bool {
		if g.waiting[i].req.Rank != g.waiting[j].req.Rank {
			return g.waiting[i].req.Rank < g.waiting[j].req.Rank
		}
		return g.waiting[i].arrival < g.waiting[j].arrival
	})
	g.decideLocked()
	return w
}

// Release ends a gate. A run that went to an exit teaches the cost model its
// measured cost; a run that was cut (measured false) teaches nothing.
func (p *Permit) Release(cores float64, wall time.Duration, measured bool) {
	p.once.Do(func() {
		g := p.g
		g.mu.Lock()
		defer g.mu.Unlock()
		if measured && wall >= g.cfg.MinLearnRun && cores > 0 {
			before := g.mean
			g.mean = (1-g.cfg.LearnRate)*g.mean + g.cfg.LearnRate*cores
			g.log.Info("daemon.cpugov.learn", "learned a gate's processor cost from a finished run", dlog.Context{
				"workspace": string(p.req.Workspace), "observed_cores": cores, "wall_s": wall.Seconds(),
				"mean_cores_before": before, "mean_cores": g.mean})
		}
		g.releaseLocked(p)
		g.decideLocked()
	})
}

func (g *Governor) grantLocked(req Request) *Permit {
	p := &Permit{g: g, req: req, granted: g.now()}
	g.active[p] = struct{}{}
	return p
}

func (g *Governor) releaseLocked(p *Permit) {
	delete(g.active, p)
}

func (g *Governor) dropWaiterLocked(w *waiter) {
	for i, other := range g.waiting {
		if other == w {
			g.waiting = append(g.waiting[:i], g.waiting[i+1:]...)
			return
		}
	}
}

// projection is one admission decision's arithmetic, as fractions of the
// machine.
type projection struct {
	utilization float64
	background  float64
	settled     float64
	projected   float64
}

// project computes the decision's arithmetic for admitting one more gate.
func (g *Governor) projectLocked() projection {
	cpus := float64(g.sample.CPUs)
	if cpus <= 0 {
		cpus = 1
	}
	now := g.now()
	absorbed, settledShare := 0.0, 0.0
	for p := range g.active {
		age := now.Sub(p.granted)
		absorbed += g.mean * (1 - math.Exp(-age.Seconds()/g.cfg.Tau.Seconds())) / cpus
		settledShare += g.mean / cpus
	}
	u := math.Min(1, g.sample.Utilization)
	background := math.Max(0, u-absorbed)
	return projection{
		utilization: u,
		background:  background,
		settled:     background + settledShare,
		projected:   background + settledShare + g.mean/cpus,
	}
}

// decideLocked admits waiting gates, closest-to-head first, while each fits
// the band, and stops at the first that does not.
func (g *Governor) decideLocked() {
	for len(g.waiting) > 0 {
		w := g.waiting[0]
		reason, admitted := "", "below_band"
		var pr projection
		if g.sampled {
			pr = g.projectLocked()
		}
		switch {
		case len(g.active) >= g.cfg.MaxGates:
			reason = "gate_limit"
		case g.power == PowerAC:
			admitted = "on_ac_power"
		case !g.sampled:
			reason = "no_sample"
		default:
			switch {
			case pr.settled >= g.cfg.Low:
				reason = "load_in_or_above_band"
			case pr.projected > g.cfg.High:
				reason = "projected_above_ceiling"
			}
		}
		if reason != "" {
			// A DEFERRAL IS RECORDED WHEN ITS REASON CHANGES: the decision is
			// re-made on every sample, and an unchanged one is the same
			// decision standing.
			if w.reason != reason {
				w.reason = reason
				g.log.Info("daemon.cpugov.admit", "deferred a speculative gate", g.fieldsLocked(w.req, "defer", reason, pr))
			}
			return
		}
		g.waiting = g.waiting[1:]
		w.permit = g.grantLocked(w.req)
		g.log.Info("daemon.cpugov.admit", "admitted a speculative gate", g.fieldsLocked(w.req, "admit", admitted, pr))
		close(w.ready)
	}
}

func (g *Governor) fieldsLocked(req Request, decision, reason string, pr projection) dlog.Context {
	return dlog.Context{
		"workspace": string(req.Workspace), "repo": req.Repo, "rank": req.Rank, "decision": decision, "reason": reason,
		"utilization": pr.utilization, "background": pr.background, "settled": pr.settled, "projected": pr.projected,
		"band_low": g.cfg.Low, "band_high": g.cfg.High, "mean_cores": g.mean,
		"active": len(g.active), "waiting": len(g.waiting), "cpus": g.sample.CPUs, "power": g.power.String(),
	}
}
