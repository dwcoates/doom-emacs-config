// Package cpugov is the merge queue's GATE GOVERNOR: it counts the test gates
// the queue runs, bounded at MaxGates, and reads the host's power source,
// which decides how many merges at the front of a queue are worked on.
//
// THE POWER SOURCE DECIDES THE WINDOW (owner ruling, 2026-10-14). On AC power
// a queue works its top two merges: they race to land, taking turns at the
// tests (merge's testturn.go). On battery only the head is worked on -- no
// race and no turns -- and the same holds while the power source is unread,
// so a reader that fails costs battery nothing. Window answers it, and every
// subscriber (OnPowerChange) is told each change, so the queue starts the
// merge behind its head the moment the host is plugged in and suspends it the
// moment it is unplugged.
//
// THE POWER SOURCE IS READ every SampleEvery (Run), and a failure to read it
// is said once per distinct cause, at ERROR.
//
// PRIORITY IS HEAD-FIRST. A gate of the merge at its queue's head is never
// governed: it is granted at once and only counted. Waiting gates behind a
// head are admitted closest-to-head first, while fewer than MaxGates run.
package cpugov

import (
	"context"
	"errors"
	"fmt"
	"sort"
	"sync"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
)

const (
	// QueueWindow is the queue's window on AC power: the head and the merge
	// right behind it, which race to land. It is also the default bound on
	// the gates running at once.
	QueueWindow = 2
	// BatteryWindow is the queue's window on battery, and while the power
	// source is unread: the head alone.
	BatteryWindow = 1
	// SampleEvery is the production cadence the power source is read at.
	SampleEvery = 5 * time.Second
)

// Power is the source the host runs on.
type Power int

const (
	// PowerUnknown is a power source not yet read, or unreadable: the queue
	// works its head alone, as on battery.
	PowerUnknown Power = iota
	// PowerAC is a host on external power: the top two merges race.
	PowerAC
	// PowerBattery is a host on its battery: the head alone is worked on.
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
	g    *Governor
	req  Request
	once sync.Once
}

// Config is the governor's bound.
type Config struct {
	// MaxGates bounds the gates running at once, the head's included.
	MaxGates int
}

// DefaultConfig is the production bound.
func DefaultConfig() Config {
	return Config{MaxGates: QueueWindow}
}

// waiter is one gate behind a head waiting for admission.
type waiter struct {
	req     Request
	ready   chan struct{}
	permit  *Permit
	reason  string
	arrival uint64
}

// Governor counts the queue's gates and reads the host's power source.
type Governor struct {
	cfg    Config
	source PowerSource
	log    dlog.Logger

	mu sync.Mutex
	// power is the source the host last read as running on, and powerErr
	// the standing cause it could not be read.
	power       Power
	powerErr    string
	subscribers []func(Power)
	active      map[*Permit]struct{}
	waiting     []*waiter
	arrivals    uint64
}

// New builds a governor. Every collaborator is required.
func New(cfg Config, source PowerSource, log dlog.Logger) (*Governor, error) {
	if source == nil || log == nil {
		return nil, errors.New("cpugov: a power source and a logger are required")
	}
	if cfg.MaxGates < 1 {
		return nil, fmt.Errorf("cpugov: the configuration %+v bounds no gate", cfg)
	}
	return &Governor{cfg: cfg, source: source, log: log, active: map[*Permit]struct{}{}}, nil
}

// Run reads the power source every SampleEvery until ctx ends. The ticker is
// the reading cadence, the kernel's own; nothing waits on it for
// synchronization.
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

// observe reads the power source and installs it.
func (g *Governor) observe() {
	power, err := g.source.Power()
	g.install(power, err)
}

// ObservePower installs the power source: the reading loop's step, driven
// directly by a caller that reads it itself.
func (g *Governor) ObservePower(power Power) {
	g.install(power, nil)
}

// install records one reading and, when it changed the power source, tells
// every subscriber, outside the lock and in the order they subscribed.
func (g *Governor) install(power Power, err error) {
	g.mu.Lock()
	was := g.power
	g.notePowerLocked(power, err)
	now := g.power
	subscribers := append([]func(Power){}, g.subscribers...)
	g.mu.Unlock()
	if now == was {
		return
	}
	for _, tell := range subscribers {
		tell(now)
	}
}

// notePowerLocked records the source the host runs on. An unreadable source
// reads as unknown, said once per distinct cause at ERROR; a change of source
// is said at INFO.
func (g *Governor) notePowerLocked(power Power, err error) {
	if err != nil {
		if err.Error() != g.powerErr {
			g.powerErr = err.Error()
			g.log.Error("daemon.cpugov.power", "could not read the host's power source; the queue works only its head until it reads", dlog.Context{
				"error": err.Error(), "was": g.power.String()})
		}
		power = PowerUnknown
	} else {
		g.powerErr = ""
	}
	if power == g.power {
		return
	}
	g.log.Info("daemon.cpugov.power", "the host's power source changed", dlog.Context{
		"was": g.power.String(), "power": power.String(), "window": windowOf(power)})
	g.power = power
}

// OnPowerChange subscribes tell to every change of the power source.
func (g *Governor) OnPowerChange(tell func(Power)) {
	g.mu.Lock()
	defer g.mu.Unlock()
	g.subscribers = append(g.subscribers, tell)
}

// Power answers the source the host last read as running on.
func (g *Governor) Power() Power {
	g.mu.Lock()
	defer g.mu.Unlock()
	return g.power
}

// Window answers how many merges at the front of a queue are worked on.
func (g *Governor) Window() int {
	return windowOf(g.Power())
}

func windowOf(power Power) int {
	if power == PowerAC {
		return QueueWindow
	}
	return BatteryWindow
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
		g.log.Info("daemon.cpugov.withdraw", "a gate behind its head stopped waiting for admission", dlog.Context{
			"workspace": string(req.Workspace), "repo": req.Repo, "rank": req.Rank, "cause": context.Cause(ctx).Error()})
		g.decideLocked()
		return nil, context.Cause(ctx)
	}
}

// enqueue puts one gate in line and decides at once: a head gate, and a gate
// behind its head the bound has room for, answer a waiter already ready.
func (g *Governor) enqueue(req Request) *waiter {
	g.mu.Lock()
	defer g.mu.Unlock()
	g.arrivals++
	w := &waiter{req: req, ready: make(chan struct{}), arrival: g.arrivals}
	if req.Head {
		w.permit = g.grantLocked(req)
		close(w.ready)
		g.log.Info("daemon.cpugov.admit", "the head's gate runs at once; it is never governed", g.fieldsLocked(req, "admit", "head"))
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

// Release ends a gate.
func (p *Permit) Release() {
	p.once.Do(func() {
		g := p.g
		g.mu.Lock()
		defer g.mu.Unlock()
		g.releaseLocked(p)
		g.decideLocked()
	})
}

func (g *Governor) grantLocked(req Request) *Permit {
	p := &Permit{g: g, req: req}
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

// decideLocked admits waiting gates, closest-to-head first, while the bound
// has room, and stops at the first it has none for.
func (g *Governor) decideLocked() {
	for len(g.waiting) > 0 {
		w := g.waiting[0]
		if len(g.active) >= g.cfg.MaxGates {
			// A DEFERRAL IS RECORDED WHEN ITS REASON CHANGES: an unchanged
			// one is the same decision standing.
			if w.reason != "gate_limit" {
				w.reason = "gate_limit"
				g.log.Info("daemon.cpugov.admit", "deferred a gate behind its head", g.fieldsLocked(w.req, "defer", "gate_limit"))
			}
			return
		}
		g.waiting = g.waiting[1:]
		w.permit = g.grantLocked(w.req)
		g.log.Info("daemon.cpugov.admit", "admitted a gate behind its head", g.fieldsLocked(w.req, "admit", "below_gate_limit"))
		close(w.ready)
	}
}

func (g *Governor) fieldsLocked(req Request, decision, reason string) dlog.Context {
	return dlog.Context{
		"workspace": string(req.Workspace), "repo": req.Repo, "rank": req.Rank, "decision": decision, "reason": reason,
		"active": len(g.active), "waiting": len(g.waiting), "max_gates": g.cfg.MaxGates, "power": g.power.String(),
	}
}
