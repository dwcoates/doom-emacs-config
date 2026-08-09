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
	// Unwired receives every session this sweep FINISHED WITH AND DID NOT WIRE,
	// named by the verdict that left it that way (the BootSweepUnwired*
	// constants).
	//
	// It exists because a verdict this sweeper reached is a conclusion about a
	// session the USER owns, and until it has somewhere to go the only account
	// of it is a log line at boot — while the durable connectivity the previous
	// daemon wrote is still standing, so the workspace goes on presenting as
	// operational with no shim behind it.
	//
	// A nil hook is the pre-classification behavior: the verdict is logged and
	// nothing else happens. It is OPTIONAL rather than required because the
	// user-visible record it feeds is not landed yet (see the note on
	// classifyUnwired), and a required field would be a promise this daemon
	// cannot yet keep.
	//
	// An error from it is NEVER swallowed: it is logged against the session and
	// the verdict it belonged to, since a classification that failed to reach
	// the user is exactly the silence this hook exists to end.
	Unwired func(workspace, sessionID, verdict string) error
}

// The verdicts that leave a session unwired. Each is one branch of reconcile,
// named so a classified record can say WHICH conclusion the sweep reached
// rather than only that it reached one.
const (
	// BootSweepUnwiredNoLiveShim — neither probe found anything: the shim is
	// genuinely gone and the session waits for the on-demand bring-up.
	BootSweepUnwiredNoLiveShim = "boot_sweep_no_live_shim"
	// BootSweepUnwiredLockHeldWithoutConnection — a live process still holds
	// the session lock and never dialled in within the redial window.
	BootSweepUnwiredLockHeldWithoutConnection = "boot_sweep_lock_held_without_connection"
	// BootSweepUnwiredProbeFailed — the parked-connection probe failed twice,
	// so whether a shim is connected remains unknown.
	BootSweepUnwiredProbeFailed = "boot_sweep_probe_failed"
	// BootSweepUnwiredLockProbeFailed — the session-lock probe could not say
	// whether a shim is alive. It is the fourth branch that leaves a session
	// unwired, and it is classified for the same reason the other three are:
	// the sweep reached a conclusion and the user owns the session it reached
	// it about.
	BootSweepUnwiredLockProbeFailed = "boot_sweep_lock_probe_failed"
)

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
		s.classifyUnwired(label, rec, BootSweepUnwiredProbeFailed)
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
		s.classifyUnwired(label, rec, BootSweepUnwiredLockProbeFailed)
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
		s.classifyUnwired(label, rec, BootSweepUnwiredLockHeldWithoutConnection)
		return registry.Record{}, false
	}
	s.Logf("server: %s sweep: session %s (ws %s) has no live shim; leaving it UNWIRED for the on-demand bring-up",
		label, rec.SessionID, rec.CWD)
	s.classifyUnwired(label, rec, BootSweepUnwiredNoLiveShim)
	return registry.Record{}, false
}

// classifyUnwired hands one finished-with, unwired session to the classifier.
//
// THE VERDICT IS THE PAYLOAD. All four branches leave the same situation — a
// session the user owns with no shim this daemon drives — and they are NOT the
// same conclusion: "the shim is gone", "a live holder never dialled in" and "I
// could not tell" call for different things from the person reading that
// workspace. So the branch travels with the session rather than being flattened
// into one anonymous "unwired".
//
// THE RESIDUE, stated where it is felt. What this hook must ultimately produce
// is a USER-VISIBLE classified record: a connectivity verdict that stops the
// row the PREVIOUS daemon wrote from going on presenting a shimless workspace
// as operational, and a host-side account naming the verdict. Neither is
// expressible yet:
//
//   - the connectivity axis refuses any edge for the retired generation these
//     sessions are left on (ssm/connectivity.go, validateConnectivityTransition
//     — a hibernated generation cannot become current again, and a replacement
//     session/generation must enter `connecting` first, which would be a
//     bring-up claim this sweep deliberately did not make); and
//   - the host action that would carry the verdict has no arm for it.
//
// Until both exist the hook is wired and unclaimed, and every verdict is still
// LOGGED exactly as it was — so nothing regressed, and closing the gap is one
// implementation rather than a search.
func (s *BootSweeper) classifyUnwired(label string, rec registry.Record, verdict string) {
	if s.Unwired == nil {
		s.Logf("server: %s sweep: session %s (ws %s) verdict=%s is NOT CLASSIFIED — no classifier is wired, so this conclusion reaches the user nowhere but this line and the connectivity its predecessor wrote still stands",
			label, rec.SessionID, rec.CWD, verdict)
		return
	}
	if err := s.Unwired(rec.CWD, rec.SessionID, verdict); err != nil {
		s.Logf("server: %s sweep: session %s (ws %s) verdict=%s could NOT be classified: %v — the conclusion stays a log line, which is the silence the classification exists to end",
			label, rec.SessionID, rec.CWD, verdict, err)
		return
	}
	s.Logf("server: %s sweep: session %s (ws %s) verdict=%s CLASSIFIED — the session this sweep did not wire has a user-visible record rather than only this log",
		label, rec.SessionID, rec.CWD, verdict)
}
