// Package netstate is the daemon's ONE owner of whether this machine can
// reach the network, and of the one signal that says it can again.
//
// THE EVIDENCE IS THE DAEMON'S OWN. Three sources say the network is down,
// in the order Classify asks them:
//
//   - an open network_unreachable fault: a shim's diagnostics reported the
//     outage (conversation.v1.SessionFaultNetworkUnreachable), and the fault
//     hook (health.FaultSink) tells this package as it tells the footer;
//   - the CLI's own words: a failure whose account names a socket code such
//     as ENOTFOUND or ECONNRESET, or says it could not reach the API;
//   - the daemon's reachability check, asked only when neither of the above
//     settles it: one unbilled HEAD of the vendor's API host.
//
// THE NETWORK IS DOWN while any network fault stands or while an outage the
// daemon itself observed stands. Its RESTORATION is the edge from down to up,
// and Restored answers a latch closed at that edge: a waiter awaits it and is
// never told to retry on a timer. A shim closes its fault when its own probe
// reaches the API (shim.engine.network_resume); an outage the daemon observed
// is ended by the daemon's own probe, on the shim's fixed five-second beat,
// which runs only while that outage stands.
//
// The probe beat is the detection, not the waiting: a hook-based detector
// (macOS path monitoring) replaces it by ending the observed outage itself,
// and nothing that awaits Restored changes.
//
// EVERY OUTAGE IS DATED. The outage begins at the first piece of evidence
// that took the network down, and the restore edge tells every listener
// (OnRestored) how long it stood, so a consumer that acts on a long outage
// reads its length from this one owner rather than timing it itself.
package netstate

import (
	"context"
	"regexp"
	"slices"
	"strings"
	"sync"
	"time"

	"agent-repld/internal/changesignal"
	"agent-repld/internal/dlog"
	"agent-repld/internal/health"
	"agent-repld/internal/ids"
	"agent-repld/internal/inflight"
)

// op is the operation every record of this package carries.
const op = "daemon.netstate"

// ProbeInterval is the beat the daemon's probe runs on while an outage it
// observed stands: the shim's own (NETWORK_RESUME_PROBE_INTERVAL_MS), fixed,
// no backoff.
const ProbeInterval = 5 * time.Second

// Evidence names what decided a failure was the network's.
type Evidence string

const (
	// EvidenceFaultOpen is a network_unreachable fault standing when the
	// failure was classified.
	EvidenceFaultOpen Evidence = "network_fault_open"
	// EvidenceCLIWords is the failure's own account naming a network failure.
	EvidenceCLIWords Evidence = "cli_words"
	// EvidenceReachabilityCheck is the daemon's own probe failing to reach
	// the API.
	EvidenceReachabilityCheck Evidence = "reachability_check"
)

// Verdict is Classify's answer.
type Verdict struct {
	// Network is true when the failure was the network's.
	Network bool
	// Evidence names what decided it; empty when Network is false.
	Evidence Evidence
	// Detail is the evidence's own account: the matched words, the probe's
	// answer, or why no evidence said network.
	Detail string
}

// ProbeAnswer is one reachability check's answer.
type ProbeAnswer struct {
	// Reachable is true when the API answered at all; any HTTP status counts.
	Reachable bool
	// Detail is what answered, or why nothing did.
	Detail string
}

// Prober asks whether the vendor's API host can be reached. Its error means
// the check could not be made at all (the vendor guard refused it), which is
// evidence of nothing.
type Prober interface {
	Probe(ctx context.Context) (ProbeAnswer, error)
}

// Deps is what a Monitor runs on.
type Deps struct {
	// Log records every classification and every edge.
	Log dlog.Logger
	// Probe is the daemon's reachability check.
	Probe Prober
	// After is the clock the probe beat waits on.
	After func(time.Duration) <-chan time.Time
	// Now is the clock every outage is dated by.
	Now func() time.Time
}

// Outage is one stretch of the network being down, from the first evidence
// that took it down to the edge that restored it.
type Outage struct {
	// Since is when the first evidence took the network down.
	Since time.Time
	// Restored is when the last evidence ended.
	Restored time.Time
}

// Duration is how long the outage stood.
func (o Outage) Duration() time.Duration { return o.Restored.Sub(o.Since) }

// Monitor holds the network's state. It is a health.FaultSink.
type Monitor struct {
	life context.Context
	deps Deps

	mu sync.Mutex
	// faults are the network_unreachable faults standing, by id.
	faults map[ids.FaultID]struct{}
	// observed is an outage the daemon itself observed, standing until the
	// daemon's probe reaches the API.
	observed bool
	// probing is true while the probe beat runs.
	probing bool
	// observations counts the outages the daemon observed, so a beat whose
	// probe answered while a newer observation arrived does not end it.
	observations uint64
	// restored announces the next edge from down to up.
	restored changesignal.Signal
	// downSince is when the standing outage began; zero while the network is
	// up.
	downSince time.Time
	// listeners are told of every restore edge (OnRestored).
	listeners []func(Outage)
}

// New answers a Monitor whose probe beat ends with LIFE.
func New(life context.Context, deps Deps) *Monitor {
	if deps.Log == nil || deps.Probe == nil || deps.After == nil || deps.Now == nil {
		panic("netstate: a Monitor needs its log, its probe and its clocks")
	}
	return &Monitor{life: life, deps: deps, faults: map[ids.FaultID]struct{}{}}
}

// downLocked reports whether the network is down. The caller holds m.mu.
func (m *Monitor) downLocked() bool { return len(m.faults) > 0 || m.observed }

// goDownLocked dates the outage that WASDOWN says just began: the caller
// holds m.mu and has just added one piece of evidence.
func (m *Monitor) goDownLocked(wasDown bool) {
	if !wasDown {
		m.downSince = m.deps.Now()
	}
}

// restoreIfUpLocked closes the restored latch and arms the next one when the
// network is no longer down, answering the outage that ended and the
// listeners to tell of it (told by the caller once it drops m.mu). The caller
// holds m.mu and has just ended one piece of evidence while the network was
// down.
func (m *Monitor) restoreIfUpLocked() (Outage, []func(Outage), bool) {
	if m.downLocked() {
		return Outage{}, nil, false
	}
	if m.downSince.IsZero() {
		panic("netstate: the network was restored from an outage that was never dated")
	}
	outage := Outage{Since: m.downSince, Restored: m.deps.Now()}
	m.downSince = time.Time{}
	m.restored.Notify()
	return outage, slices.Clone(m.listeners), true
}

// OnRestored tells FN of every restore edge from here on, with the outage that
// ended. FN runs on whatever ended the outage, outside the Monitor's lock, so
// it must not block.
func (m *Monitor) OnRestored(fn func(Outage)) {
	m.mu.Lock()
	defer m.mu.Unlock()
	m.listeners = append(m.listeners, fn)
}

// tell tells LISTENERS of OUTAGE.
func tell(listeners []func(Outage), outage Outage) {
	for _, fn := range listeners {
		fn(outage)
	}
}

// FaultOpened records a network_unreachable fault; every other kind is not
// this package's.
func (m *Monitor) FaultOpened(ws ids.WorkspaceID, line health.FaultLine) {
	if line.Kind != health.KindNetworkUnreachable {
		return
	}
	m.mu.Lock()
	m.goDownLocked(m.downLocked())
	m.faults[line.ID] = struct{}{}
	standing := len(m.faults)
	m.mu.Unlock()
	m.deps.Log.Debug(op, "a network fault opened; the network is down", dlog.Context{
		"fault": string(line.ID), "workspace": string(ws), "network_faults": standing,
	})
}

// FaultClosed retracts a network fault this Monitor recorded. A fault of any
// other kind closing is not this package's.
func (m *Monitor) FaultClosed(ws ids.WorkspaceID, id ids.FaultID) {
	m.mu.Lock()
	if _, ok := m.faults[id]; !ok {
		m.mu.Unlock()
		return
	}
	delete(m.faults, id)
	standing, observed := len(m.faults), m.observed
	outage, listeners, restored := m.restoreIfUpLocked()
	m.mu.Unlock()
	fields := dlog.Context{"fault": string(id), "workspace": string(ws), "network_faults": standing, "observed_outage": observed}
	if restored {
		m.deps.Log.Info(op, "the last network fault closed; the network is restored", dlog.Merge(fields, outageFields(outage)))
		tell(listeners, outage)
		return
	}
	m.deps.Log.Debug(op, "a network fault closed; the network is still down", fields)
}

// Restored answers the latch closed when the network is next restored: one
// already closed while the network is up.
func (m *Monitor) Restored() <-chan struct{} {
	m.mu.Lock()
	defer m.mu.Unlock()
	if !m.downLocked() {
		done := make(chan struct{})
		close(done)
		return done
	}
	return m.restored.Changed()
}

// Classify decides whether a failure whose own account is WORDS was the
// network's. A failure the daemon decides from its own words or its own probe
// starts an observed outage, which the probe beat ends.
func (m *Monitor) Classify(ctx context.Context, words string) Verdict {
	m.mu.Lock()
	faults := len(m.faults)
	m.mu.Unlock()
	if faults > 0 {
		return m.verdict(Verdict{Network: true, Evidence: EvidenceFaultOpen, Detail: "a network_unreachable fault stands"}, false)
	}
	if matched := NetworkWords(words); matched != "" {
		return m.verdict(Verdict{Network: true, Evidence: EvidenceCLIWords, Detail: matched}, true)
	}
	answer, err := m.deps.Probe.Probe(ctx)
	if err != nil {
		m.deps.Log.Info(op, "the reachability check could not run, so nothing says the network failed", dlog.Context{"cause": err.Error()})
		return Verdict{Detail: "the reachability check could not run: " + err.Error()}
	}
	if answer.Reachable {
		return m.verdict(Verdict{Detail: answer.Detail}, false)
	}
	return m.verdict(Verdict{Network: true, Evidence: EvidenceReachabilityCheck, Detail: answer.Detail}, true)
}

// verdict records V and, when OBSERVE, starts the outage the daemon observed.
func (m *Monitor) verdict(v Verdict, observe bool) Verdict {
	fields := dlog.Context{"network": v.Network, "evidence": string(v.Evidence), "detail": v.Detail}
	if !v.Network {
		m.deps.Log.Debug(op, "the failure was not the network's", fields)
		return v
	}
	m.deps.Log.Info(op, "the failure was the network's: this machine cannot reach it", fields)
	if observe {
		m.observe()
	}
	return v
}

// observe starts an observed outage, and its probe beat unless one runs.
func (m *Monitor) observe() {
	m.mu.Lock()
	m.goDownLocked(m.downLocked())
	m.observed = true
	m.observations++
	start := !m.probing
	m.probing = true
	m.mu.Unlock()
	if start {
		inflight.Standing(func() { m.probeUntilReachable() })
	}
}

// probeUntilReachable asks the probe on the beat until the API answers, then
// ends the observed outage. It ends with the daemon.
func (m *Monitor) probeUntilReachable() {
	for {
		select {
		case <-m.life.Done():
			m.mu.Lock()
			m.probing = false
			m.mu.Unlock()
			return
		case <-m.deps.After(ProbeInterval):
		}
		m.mu.Lock()
		asked := m.observations
		m.mu.Unlock()
		answer, err := m.deps.Probe.Probe(m.life)
		if err != nil {
			m.deps.Log.Error(op, "the reachability check could not run while an observed outage stands; the outage ends only when it can", dlog.Context{"cause": err.Error()})
			continue
		}
		if !answer.Reachable {
			m.deps.Log.Debug(op, "the API is still unreachable", dlog.Context{"detail": answer.Detail})
			continue
		}
		m.mu.Lock()
		if m.observations != asked {
			m.mu.Unlock()
			m.deps.Log.Debug(op, "the API answered, but a newer outage was observed while it was asked; the beat goes on", dlog.Context{"detail": answer.Detail})
			continue
		}
		m.observed, m.probing = false, false
		faults := len(m.faults)
		outage, listeners, restored := m.restoreIfUpLocked()
		m.mu.Unlock()
		fields := dlog.Context{"detail": answer.Detail, "network_faults": faults, "restored": restored}
		if restored {
			fields = dlog.Merge(fields, outageFields(outage))
		}
		m.deps.Log.Info(op, "the reachability check reached the API; the outage the daemon observed is over", fields)
		if restored {
			tell(listeners, outage)
		}
		return
	}
}

// outageFields records OUTAGE.
func outageFields(outage Outage) dlog.Context {
	return dlog.Context{"down_since": outage.Since, "down_for": outage.Duration().String()}
}

// networkCodes are the socket errors that mean the API host could not be
// reached at all: the shim's own list (engine/network-resume.ts).
var networkCodes = regexp.MustCompile(`(?i)\b(ENOTFOUND|EAI_AGAIN|ECONNREFUSED|ECONNRESET|ETIMEDOUT|ENETUNREACH|EHOSTUNREACH|ENETDOWN)\b`)

// networkProse are the CLI's own sentences for an unreachable API.
var networkProse = regexp.MustCompile(`(?i)(can['’]t reach the api server|\bconnection (?:error|refused|reset|closed)\b|\bfetch failed\b|\bsocket hang up\b)`)

// NetworkWords answers the words in TEXT that name a network failure (a
// socket code, upper-cased, or the CLI's sentence), or "" when none do.
func NetworkWords(text string) string {
	if code := networkCodes.FindString(text); code != "" {
		return strings.ToUpper(code)
	}
	return networkProse.FindString(text)
}
