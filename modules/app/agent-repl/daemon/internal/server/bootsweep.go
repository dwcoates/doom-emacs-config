// bootsweep.go reconciles the daemon with the shims that outlived its
// predecessor.
//
// WHAT WAS MISSING. A shim survives its daemon by design: it redials the one
// well-known daemon socket forever with backoff, and the new daemon's listener
// PARKS the connection under its session id before anything asks for it. But
// nothing ever asked. A parked connection sat there until some later act —
// a prompt, a perspective switch — happened to bring that workspace up, so a
// daemon restart left every surviving session unclaimed and every workspace
// blue, with a perfectly healthy shim connected to a daemon that had not
// noticed it.
//
// This is the boot half of that: walk the registry once the listener and the
// lock dir are up, and claim what is already there.
//
// THE TWO PROBES ARE THE SAME PAIR EnsureShim USES, and for the same reason
// neither is sufficient alone:
//
//   - CONNECTED answers for a shim that has already dialled in. It is O(1)
//     against the listener's parked map.
//   - THE SESSION LOCK answers for one that is alive but has not dialled in
//     yet. Right after boot that window is real — the survivor's reconnect
//     backoff can be seconds — and a session judged on the connection alone
//     during it reads as "no shim" when the truth is "not yet".
//
// A locked-but-unconnected session is NOT ensured on the spot: EnsureShim
// refuses to spawn against a held lock (rightly — that would be two writers on
// one transcript) and would fail loudly for what is an ordinary backoff. It is
// carried to a single RE-CHECK pass instead, by which time the redial has
// landed and the connection is claimable. That re-check is the only reason
// this needs a delay at all, and the delay is injected rather than slept so a
// test drives it as an event.
//
// A session that is NEITHER connected nor locked has no shim. It is left
// UNWIRED for the on-demand path (workspaceopen's ensure, or the first
// prompt) rather than spawned here: booting the daemon is not a reason to
// start every conversation the user has ever had.
//
// NOTHING HERE BLOCKS READINESS. The sweep runs after /healthz is already
// answering, because it is reconciliation rather than a boot dependency, and a
// slow probe over a dozen workspaces must never look like a daemon that failed
// to start.
package server

import (
	"context"
	"errors"
	"sync"
	"time"

	"claude-repld/internal/errclass"
	"claude-repld/internal/registry"
)

// DefaultBootSweepRecheck is how long the sweep waits before its one re-check
// pass. It is a REDIAL-WINDOW bound, not a tuned delay: the shimclient's
// reconnect backoff tops out at 5s, so a survivor mid-backoff at boot has
// dialled in by the time this elapses.
const DefaultBootSweepRecheck = 8 * time.Second

// DefaultBootSweepParallelism bounds how many workspaces are reattached at
// once. Each one is a lock probe plus a bring-up, and a boot with thirty
// registered workspaces should not open thirty at the same instant.
const DefaultBootSweepParallelism = 4

// BootSweeper reattaches the sessions whose shims outlived the previous
// daemon. Construct it with every field set; Run does the rest.
type BootSweeper struct {
	// Reg is the persistent session registry (required).
	Reg *registry.Registry
	// Connected reports whether a shim for a session has already dialled in
	// (the listener's parked map). Required.
	Connected func(sessionID string) (bool, error)
	// Held reports whether a live process holds a session's lock — a shim that
	// is alive but may not have dialled in yet. Required. An error means "I
	// could not tell", which is NEVER read as free.
	Held func(sessionID string) (bool, error)
	// Ensurer brings a workspace's session up (reattach-first). Required.
	Ensurer WorkspaceEnsurer
	// Logf is the loud logger (required): every classification and every
	// reattach is logged, because this runs unattended at boot and its
	// verdicts are the only account of why a workspace did or did not come up.
	Logf func(string, ...any)
	// Recheck fires the one re-check pass. Nil uses DefaultBootSweepRecheck.
	// Injected so a test drives the pass as an event rather than a sleep.
	Recheck <-chan time.Time
	// Parallelism bounds concurrent reattaches. Zero uses the default.
	Parallelism int
}

// Run performs the first pass, waits for the re-check, then performs the
// second pass over whatever the first left unclaimed. It returns when the
// second pass completes, or as soon as ctx ends.
func (s *BootSweeper) Run(ctx context.Context) {
	deferred := s.pass(ctx, "boot", nil)
	if ctx.Err() != nil {
		return
	}
	if len(deferred) == 0 {
		s.Logf("server: boot sweep complete; nothing deferred to the re-check pass")
		return
	}
	recheck := s.Recheck
	if recheck == nil {
		t := time.NewTimer(DefaultBootSweepRecheck)
		defer t.Stop()
		recheck = t.C
	}
	s.Logf("server: boot sweep deferred %d session(s) to a re-check pass (their shims hold a lock but have not redialled yet)",
		len(deferred))
	select {
	case <-ctx.Done():
		return
	case <-recheck:
	}
	s.pass(ctx, "re-check", deferred)
}

// pass classifies and reattaches one round. When only is nil every
// non-terminal record with a cwd is considered; otherwise only those sessions
// are. It returns the sessions whose shims hold a lock but have not dialled in
// — the set the re-check pass exists for.
func (s *BootSweeper) pass(ctx context.Context, label string, only []registry.Record) []registry.Record {
	records := only
	if records == nil {
		for _, rec := range s.Reg.All() {
			if rec.Terminal || rec.CWD == "" {
				continue
			}
			records = append(records, rec)
		}
	}
	parallelism := s.Parallelism
	if parallelism <= 0 {
		parallelism = DefaultBootSweepParallelism
	}

	var (
		mu       sync.Mutex
		deferred []registry.Record
		wg       sync.WaitGroup
	)
	sem := make(chan struct{}, parallelism)
	for _, rec := range records {
		if ctx.Err() != nil {
			break
		}
		wg.Add(1)
		go func(rec registry.Record) {
			defer wg.Done()
			sem <- struct{}{}
			defer func() { <-sem }()
			if again, ok := s.reconcile(label, rec); ok {
				mu.Lock()
				deferred = append(deferred, again)
				mu.Unlock()
			}
		}(rec)
	}
	wg.Wait()
	return deferred
}

// reconcile classifies one record and acts on it, reporting whether it should
// be revisited by the re-check pass.
func (s *BootSweeper) reconcile(label string, rec registry.Record) (registry.Record, bool) {
	connected, err := s.Connected(rec.SessionID)
	if err != nil {
		if label == "boot" {
			s.Logf("server: %s sweep: session %s (ws %s) parked-connection probe FAILED, so whether the shim is connected is UNKNOWN; deferring to the re-check pass: %v",
				label, rec.SessionID, rec.CWD, err)
			return rec, true
		}
		s.Logf("server: %s sweep: session %s (ws %s) parked-connection probe FAILED again, so whether the shim is connected remains UNKNOWN; leaving it unwired: %v",
			label, rec.SessionID, rec.CWD, err)
		return registry.Record{}, false
	}
	if connected {
		s.Logf("server: %s sweep: session %s (ws %s) has a PARKED shim connection; reattaching", label, rec.SessionID, rec.CWD)
		if err := s.Ensurer.Ensure(rec.CWD); err != nil {
			if errors.Is(err, errclass.ErrSessionHibernated) {
				// NOT A FAILURE, and saying so matters: the bring-up evaluated
				// the keep-alive policy and found this record long past its
				// threshold, so it hibernated the session rather than
				// reattaching a shim nobody has spoken to in that long. The
				// session now meets the revival gate, which is the outcome the
				// sweep would have reached a tick later anyway.
				s.Logf("server: %s sweep: session %s (ws %s) HIBERNATED instead of reattached — the record was found past the keep-alive policy's threshold at bring-up; the user chooses a revival mode from the gate: %v",
					label, rec.SessionID, rec.CWD, err)
				return registry.Record{}, false
			}
			s.Logf("server: %s sweep: session %s (ws %s) reattach FAILED — the workspace stays unwired until it is opened: %v",
				label, rec.SessionID, rec.CWD, err)
			return registry.Record{}, false
		}
		s.Logf("server: %s sweep: session %s (ws %s) REATTACHED", label, rec.SessionID, rec.CWD)
		return registry.Record{}, false
	}
	held, err := s.Held(rec.SessionID)
	if err != nil {
		// "I could not tell" is never read as free: reporting the shim as gone
		// would be a claim the probe did not make.
		s.Logf("server: %s sweep: session %s (ws %s) lock probe FAILED, so whether a shim is alive is UNKNOWN; leaving it unwired: %v",
			label, rec.SessionID, rec.CWD, err)
		return registry.Record{}, false
	}
	if held {
		if label == "boot" {
			s.Logf("server: %s sweep: session %s (ws %s) has a live shim holding its lock but no connection yet; deferring to the re-check pass",
				label, rec.SessionID, rec.CWD)
			return rec, true
		}
		// Still not talking after the redial window. This is the state
		// EnsureShim calls a bug to surface rather than paper over, so it is
		// said plainly instead of being retried forever.
		s.Logf("server: %s sweep: session %s (ws %s) STILL holds its lock without connecting after the redial window; NOT spawning a duplicate — the holder is alive and may be mid-turn",
			label, rec.SessionID, rec.CWD)
		return registry.Record{}, false
	}
	s.Logf("server: %s sweep: session %s (ws %s) has no live shim; leaving it UNWIRED for the on-demand bring-up",
		label, rec.SessionID, rec.CWD)
	return registry.Record{}, false
}
