package wifirpc

import (
	"sort"
	"sync"

	"agent-repld/internal/changesignal"
)

// Peers is the set of processes connected to the service, by the pid the
// kernel records for each connection. The service protects each one's tree
// from the application freeze (appfreeze.Protection.Selves) for as long as
// its connection lives, so the fact is carried by the connection itself and
// is never configured.
type Peers struct {
	mu      sync.Mutex
	pids    map[int]int
	changed changesignal.Signal
}

// NewPeers answers an empty set.
func NewPeers() *Peers { return &Peers{pids: map[int]int{}} }

// add counts one more connection from pid.
func (p *Peers) add(pid int) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.pids[pid]++
	p.changed.Notify()
}

// remove counts one connection from pid gone.
func (p *Peers) remove(pid int) {
	p.mu.Lock()
	defer p.mu.Unlock()
	if p.pids[pid] <= 1 {
		delete(p.pids, pid)
	} else {
		p.pids[pid]--
	}
	p.changed.Notify()
}

// Changed answers a channel closed at the next change of the set, so a waiter
// learns of a connection coming or going by that event and never by polling.
func (p *Peers) Changed() <-chan struct{} { return p.changed.Changed() }

// PIDs answers every pid with at least one open connection, sorted.
func (p *Peers) PIDs() []int {
	p.mu.Lock()
	defer p.mu.Unlock()
	out := make([]int, 0, len(p.pids))
	for pid := range p.pids {
		out = append(out, pid)
	}
	sort.Ints(out)
	return out
}
