// sessioncontrollers.go is the live wiring for the session controller
// (internal/sessioncontroller): the daemon-side implementations of the session controller's
// injected seams — SessionLocator (workspace -> live session id), ShimSpawner
// (reattach-first UDS bring-up), and the late-bound push forwarder that lets
// the session controller push frontend frames to a frontend.Server that is constructed
// AFTER the session controller (WireAgentShim returns the Server, whose target is then set
// on the forwarder). Kept in the server package (not sessioncontroller) because these
// read the daemon's registry and own the reattach/spawn plumbing; the session controller
// stays IO-narrow and testable behind the interfaces (design §4.4, §9.1, §16).
package server

import (
	"context"
	"errors"
	"fmt"
	"net"
	"os"
	"sync"
	"sync/atomic"
	"syscall"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/frontend"
	"claude-repld/internal/registry"
	"claude-repld/internal/sessioncontroller"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/shim"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/shimlisten"
	"claude-repld/internal/statedb"
)

// SessionLocator resolves a workspace to the newest non-terminal session bound
// to it (the record whose CWD is the workspace). It implements
// sessioncontroller.SessionLocator over the persistent registry: the session controller asks
// "which live session drives this workspace?" on the first frontend command
// for it, and the answer is the freshest conversation rooted at that worktree.
type SessionLocator struct {
	// Reg is the persistent session registry (required).
	Reg *registry.Registry
}

// RegistryClientLogIdentityResolver validates browser session attribution
// against the newest live registry record for the command workspace.
type RegistryClientLogIdentityResolver struct {
	Reg *registry.Registry
}

func (r *RegistryClientLogIdentityResolver) ResolveClientLogIdentity(workspace string) (ClientLogSessionIdentity, bool) {
	if r == nil || r.Reg == nil {
		return ClientLogSessionIdentity{}, false
	}
	sessionID, ok := (&SessionLocator{Reg: r.Reg}).Locate(workspace)
	if !ok {
		return ClientLogSessionIdentity{}, false
	}
	record, ok := r.Reg.Get(sessionID)
	if !ok {
		return ClientLogSessionIdentity{}, false
	}
	return ClientLogSessionIdentity{AgentReplSessionID: record.SessionID, ClaudeSessionID: record.ClaudeSessionID}, true
}

// Locate returns the id of the newest non-terminal session whose CWD is
// workspace, and whether one exists. Newest is by CreatedAt (RFC3339); a record
// whose timestamp cannot be parsed is treated as the zero time so it never
// shadows a record with a real one. A terminal record never matches — its
// conversation has ended, so it cannot be controlled.
func (l *SessionLocator) Locate(workspace string) (string, bool) {
	if l.Reg == nil || workspace == "" {
		return "", false
	}
	var (
		bestID string
		bestAt time.Time
		found  bool
	)
	for _, rec := range l.Reg.All() {
		if rec.Terminal || rec.CWD != workspace {
			continue
		}
		at, err := time.Parse(time.RFC3339, rec.CreatedAt)
		if err != nil {
			at = time.Time{}
		}
		if !found || at.After(bestAt) {
			bestID, bestAt, found = rec.SessionID, at, true
		}
	}
	return bestID, found
}

// ShimConnSource adapts the shim listener to shimclient.ConnSource, splitting
// the listener's (conn, hello) pair into the two values the client takes.
type ShimConnSource struct {
	// Listener is the daemon's shim listener (required).
	Listener *shimlisten.Server
	// Deaths is the spawn watch for shims THIS daemon launched. Optional: with
	// it bound, the client waiting here for a connection also learns when the
	// process that owed it that connection died, and this adapter marks the
	// watch the moment a connection is taken. Nil leaves both facts unknown
	// and the wait bounded only by the caller's deadline, as before.
	Deaths *ShimSpawnWatch
}

var _ shimclient.ShimDeaths = (*ShimConnSource)(nil)
var _ shimclient.ShimExits = (*ShimConnSource)(nil)

// DiedBeforeConnect implements shimclient.ShimDeaths by delegating to the
// spawn watch, so the client picks the seam up from its ConnSource without
// every layer between having to carry it.
func (s *ShimConnSource) DiedBeforeConnect(sessionID string) <-chan struct{} {
	if s.Deaths == nil {
		return nil
	}
	return s.Deaths.DiedBeforeConnect(sessionID)
}

// SpawnState implements shimclient.ShimDeaths.
func (s *ShimConnSource) SpawnState(sessionID string) shimclient.ShimSpawnState {
	if s.Deaths == nil {
		return shimclient.ShimSpawnState{}
	}
	return s.Deaths.SpawnState(sessionID)
}

// DiedAfterConnect implements shimclient.ShimExits by delegating to the
// daemon-owned process watch. A surviving shim from another daemon has no
// local process handle and therefore no exit channel.
func (s *ShimConnSource) DiedAfterConnect(sessionID string) <-chan shimclient.ShimExit {
	if s.Deaths == nil {
		return nil
	}
	return s.Deaths.DiedAfterConnect(sessionID)
}

var _ shimclient.ConnSource = (*ShimConnSource)(nil)

// Next blocks until sessionID's shim connects, then yields its connection and
// the ShimHello that identified it.
func (s *ShimConnSource) Next(ctx context.Context, sessionID string) (net.Conn, *corev1.ShimHello, error) {
	if s.Listener == nil {
		return nil, nil, fmt.Errorf("server: ShimConnSource has no listener")
	}
	c, err := s.Listener.Next(ctx, sessionID)
	if err != nil {
		return nil, nil, err
	}
	// Taking the connection IS the proof the spawned process dialled in, and
	// the only proof the listener stops giving once the connection is claimed.
	// Recording it here is what keeps a later exit from being misreported as a
	// shim that never connected.
	if s.Deaths != nil {
		s.Deaths.Connected(sessionID)
	}
	return c.Net, c.Hello, nil
}

// ConnectedFunc reports whether a shim for sessionID has already dialled in.
// Backed by the shim listener; injected so ShimSpawner is unit-testable
// without a real socket.
type ConnectedFunc func(sessionID string) (bool, error)

// EvictParkedFunc removes the listener's unclaimed transport for one session
// after the process owning it has been proven stopped.
type EvictParkedFunc func(sessionID, reason string) bool

// ShimSpawnFunc execs a fresh shim for sessionID, told which daemon socket to
// dial and where to write its events. The exec itself stays in main (which
// owns node/shim/store paths); ShimSpawner only decides WHEN to call it. It
// returns a stop func that terminates the launched shim cleanly (SIGTERM) —
// the daemon uses it to hibernate the child — or nil when there is nothing to
// stop (a shim that outlived a prior daemon and dialled back in).
type ShimSpawnFunc func(sessionID string, opts CreateOpts) (handle ShimHandle, err error)

// ShimHandle is everything one spawn hands back about the process it started.
//
// Both fields are optional in exactly one direction: a spawner that started
// nothing (a shim that outlived a prior daemon and dialled back in) has neither
// a stop nor a reaper, and a spawner that does not reap its children has a stop
// but no rendezvous. A spawner that DOES reap must publish Reaped, because that
// channel is the only way a stop can know the exit it asked for has been
// accounted for rather than merely delivered.
type ShimHandle struct {
	// Stop terminates the launched shim cleanly (SIGTERM). Nil when there is
	// nothing to stop.
	Stop ShimStopFunc
	// Reaped closes when the spawner's reaper has finished with the process:
	// waited on, its exit recorded, and its hooks run. Nil when this spawn has
	// no reaper of its own.
	Reaped <-chan struct{}
}

// ShimStopFunc stops one spawned shim, told WHO commanded the stop and WHY.
// The attribution is a required argument rather than a convenience: it is what
// makes a commanded shim death distinguishable, from the daemon log alone,
// from a shim that died on its own.
type ShimStopFunc func(by ShimStop) error

// ShimStop is the shim package's stop attribution, re-exported so a caller
// wiring a ShimStopFunc names one package rather than two.
type ShimStop = shim.Stop

// ShimSpawner keeps exactly one shim alive per session: it leaves an existing
// one alone (whether it is connected or merely holding its session lock, §4.4)
// or spawns a fresh one, resolving the spawn's CreateOpts from the session's
// registry record. It tracks the stop func of every shim IT spawned so
// StopShim can SIGTERM it on hibernation. It implements sessioncontroller.Spawner.
type ShimSpawner struct {
	reg       *registry.Registry
	connected ConnectedFunc
	evict     EvictParkedFunc
	spawn     ShimSpawnFunc
	logf      func(string, ...any)
	// signal delivers a signal to a pid. Injected so the surviving-shim stop
	// is unit-testable without spawning a real process to kill.
	signal func(pid int, sig syscall.Signal) error
	// awaitStopped blocks until sessionID's shim is really gone, for at most
	// within. Injected so a unit harness, which has no processes and no locks,
	// is not made to wait for a condition it can never observe.
	//
	// It reports errStopWaitExpired, and ONLY that, for "the bound elapsed with
	// the lock still held" — the one outcome the stop escalates on. Every other
	// error is "I could not tell", which is a hard failure rather than grounds
	// to signal anything.
	awaitStopped func(sessionID string, within time.Duration) error
	// termGraceOverride and killGraceOverride override the escalation bounds
	// (stopTermGrace, stopKillGrace). Zero means the production constants; only
	// a test assigns them, so every rung of the escalation can be driven
	// without waiting out a real grace.
	termGraceOverride time.Duration
	killGraceOverride time.Duration
	// forceFake mirrors the daemon-wide -fake decision, which the create path
	// already consults. See ForceFake and EnsureShim's resume gate.
	forceFake bool

	mu      sync.Mutex
	handles map[string]ShimHandle // session id -> the shim WE spawned
}

// consumeRewindLineage clears sessionID's rewind lineage, in the Update that
// records the spawn that just announced it.
//
// THE CLEAR IS THE CONSUMPTION, and it is durable for the same reason the arm
// is. A lineage left standing would ride the NEXT unrelated respawn too,
// telling the shim it had just been rewound when it had not, and the shim would
// emit a second SessionRewound for a rewind that never happened.
//
// A FAILED CLEAR IS LOUD AND NOT SWALLOWED: the record still carries a lineage
// that has already been announced, which the next spawn would announce again.
func (s *ShimSpawner) consumeRewindLineage(sessionID string) error {
	if s.reg == nil {
		return fmt.Errorf("server: session %s: no registry; the announced rewind lineage cannot be consumed", sessionID)
	}
	found, err := s.reg.Update(sessionID, func(rec *registry.Record) {
		rec.Rewind = registry.RewindLineage{}
	})
	if err != nil {
		return fmt.Errorf("server: session %s: clearing the consumed rewind lineage: %w", sessionID, err)
	}
	if !found {
		return fmt.Errorf("server: session %s: clearing the consumed rewind lineage found no record", sessionID)
	}
	return nil
}

// NewShimSpawner builds a ShimSpawner. reg and spawn are required; a nil
// connected reports nothing connected, a nil logf discards.
func NewShimSpawner(reg *registry.Registry, connected ConnectedFunc, evict EvictParkedFunc, spawn ShimSpawnFunc, logf func(string, ...any)) *ShimSpawner {
	if logf == nil {
		logf = func(string, ...any) {}
	}
	return &ShimSpawner{
		reg: reg, connected: connected, evict: evict, spawn: spawn, logf: logf,
		signal:       signalPID,
		awaitStopped: awaitShimStopped,
		handles:      map[string]ShimHandle{},
	}
}

// ForceFake tells the spawner that every session it brings up runs the
// scripted offline SDK, mirroring the daemon-wide -fake flag.
//
// It exists so the resume-viability gate reaches the SAME verdict on a respawn
// as the create reached on the same session. The scripted SDK writes no vendor
// transcript, so without this a hibernated or bounced fake session hard-failed
// its way back up on a file only the real CLI ever creates — with the create
// that started it having waived exactly that check.
func (s *ShimSpawner) ForceFake(on bool) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.forceFake = on
}

// fakeForced reports the daemon-wide fake decision under the lock.
func (s *ShimSpawner) fakeForced() bool {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.forceFake
}

// EnsureShim makes sure exactly one shim is alive for sessionID, spawning one
// only when none is.
//
// "Is a shim alive?" is answered by two things, because neither alone is
// enough:
//
//   - the LISTENER, for a shim that has already dialled in; and
//   - the session LOCK, for one that is alive but has not dialled in yet.
//
// The lock is what covers a daemon restart. A surviving shim reconnects on its
// own schedule, so immediately after boot the listener legitimately answers
// "not connected" for a session whose shim is alive and mid-turn. Spawning
// then would put two shims on one conversation — two writers on one
// transcript. The lock is held by the shim process itself and released by the
// kernel when it dies, so it answers correctly during that window.
//
// A lock held with nothing dialled in is NOT a spawn: it is a shim that is
// alive but not talking, which is a bug to surface rather than a state to
// paper over. Spawning a second one is the exact duplicate this prevents, and
// killing the holder would destroy the in-flight turn §4.4 protects.
//
// VALIDATE BEFORE RESUME. A record can name a conversation that no longer
// exists — a uuid adopted before the vendor wrote anything, a transcript
// deleted underneath us, or a config dir that moved. Handing that to the CLI
// is an immediate exit 1. The shared resume-viability gate rejects that state
// before spawn while leaving the durable identity untouched, so restoration
// can never become a different fresh conversation.
func (s *ShimSpawner) EnsureShim(ctx context.Context, sessionID string) (sessioncontroller.SpawnResult, error) {
	var res sessioncontroller.SpawnResult
	if s.reg == nil {
		return res, fmt.Errorf("server: ShimSpawner has no registry; cannot resolve session %s", sessionID)
	}
	if s.spawn == nil {
		return res, fmt.Errorf("server: ShimSpawner has no spawn func; cannot bring up session %s", sessionID)
	}
	if s.connected != nil {
		connected, err := s.connected(sessionID)
		if err != nil {
			return res, fmt.Errorf("server: session %s: cannot determine whether a shim is connected: %w", sessionID, err)
		}
		if connected {
			s.logf("server: session %s: shim connection proved open (no spawn)", sessionID)
			return res, nil
		}
	}
	held, err := sessionlock.Held(sessionID)
	if err != nil {
		return res, fmt.Errorf("server: session %s: cannot determine whether a shim holds its lock: %w", sessionID, err)
	}
	if held {
		return res, fmt.Errorf("server: session %s: a shim holds the session lock but has not connected — refusing to spawn a duplicate; the holder is alive and may be mid-turn", sessionID)
	}
	rec, ok := s.reg.Get(sessionID)
	if !ok {
		return res, fmt.Errorf("server: session %s has no registry record; cannot spawn its UDS shim", sessionID)
	}
	model := registry.NormalizeModel(rec.Model)
	if model != rec.Model {
		s.logf("server: session %s: normalized legacy record model marker %q to empty for respawn (shim chooses)",
			sessionID, rec.Model)
	}
	opts := CreateOpts{
		CWD:            rec.CWD,
		Model:          model,
		PermissionMode: rec.PermissionMode,
		ConfigDir:      rec.ConfigDir,
		Resume:         rec.ClaudeSessionID,
	}
	// THE REWIND LINEAGE COMES OFF THE RECORD, and this read IS the promised
	// recovery. The lineage was written by the same Update that flipped the
	// uuid, so a daemon that died anywhere after that flip — before the
	// respawn, mid-respawn, or between daemons entirely — finds it here on the
	// very next bring-up of the session and announces the rewind it owes.
	//
	// It is CONSUMED, not merely read: the clear below is what stops it riding
	// the next unrelated respawn and announcing a rewind that never happened.
	lineage := rec.Rewind
	if lineage.Armed() {
		opts.RewoundFrom = lineage.PreviousVendorSessionID
		opts.RewindRetainedLeaf = lineage.RetainedLeafUUID
		opts.RewindDroppedTurns = lineage.DroppedTurnIDs
		s.logf("server: session %s: spawning with REWIND LINEAGE rewound_from=%s retained_leaf=%s dropped_turns=%s resume=%s",
			sessionID, lineage.PreviousVendorSessionID, lineage.RetainedLeafUUID, lineage.DroppedTurnIDs, opts.Resume)
	}
	// ONE VERDICT FOR ONE QUESTION. The create path waives the resume-viability
	// gate for a fake session because the scripted SDK writes no transcripts;
	// this path used to run the same gate with the waiver hard-coded off, so
	// the two disagreed about the identical session. A fake session that was
	// hibernated, stopped or bounced could therefore never be brought back:
	// every respawn hard-failed on a file only a real CLI ever creates.
	fake := s.fakeForced()
	opts.Fake = fake
	// freshProof is the ticket to the no-resume spawn below. It is nil here and
	// stays nil unless some rung of the ladder MINTS it from evidence; see
	// freshgate.go for why the permission is an object rather than a bool.
	var freshProof *freshEligibility
	if missing := validateResumeTarget(opts, fake); missing != nil {
		// THE RESTORE RUNG, above every other answer to a missing transcript.
		// The conversation may still exist on disk beside the workspace
		// (transcriptbackup.go); putting it back turns what used to be a lost
		// conversation into a resumed one, and it must be tried before any rung
		// that starts something blank.
		restored, restoreErr := attemptTranscriptRestore(s.logf, "automatic_restore", sessionID, opts)
		if restoreErr != nil {
			return res, restoreErr
		}
		// A successful restore is deliberately NOT taken as proof the target is
		// now viable. The gate below re-derives that from the disk, because the
		// gate is the sole authority on resume viability and a restore that
		// landed somewhere it could not be read back from is a fact only the
		// gate can report.
		_ = restored
	}
	if missing := validateResumeTarget(opts, fake); missing != nil {
		if !resumeTargetCarriesAConversation(rec) {
			// A HANDSHAKE THAT NEVER BECAME A CONVERSATION. The vendor mints a
			// uuid at system:init, before a single word has been exchanged, and
			// the daemon records it the moment it arrives. Bring a workspace up
			// and lose its shim before the first turn — which is exactly what a
			// daemon bounce during a create does — and the record names a uuid
			// with no transcript and no history, because there was never
			// anything to write down.
			//
			// The gate's refusal is right about every OTHER shape of this: a
			// transcript that was deleted, moved, or written under a different
			// account is a conversation the user still has, and starting a
			// blank one in its place silently destroys it. That is why the
			// waiver is not "the file is missing" but "the file is missing AND
			// this record proves no turn ever ran". LastTurnEndMs is the
			// registry's own durable answer to the second half; it survives the
			// bounce the transcript never existed across.
			//
			// The stale uuid is deliberately NOT cleared here. Automatic
			// restoration does not mutate durable identity, and the spawn's own
			// system:init overwrites the pointer with the uuid it mints. A
			// respawn that dies before that simply hits this same waiver again.
			s.logf("server: session %s: resume viability gate WAIVED for respawn resume=%q reason=handshake_only_no_turn_ever_ran cwd=%q config_dir=%q — the vendor minted this uuid at bring-up and no turn ever ran under it, so there is no conversation to lose and the held work would otherwise be dropped",
				sessionID, opts.Resume, opts.CWD, opts.ConfigDir)
			opts.Resume = ""
			// NO PROOF IS MINTED HERE. The waiver establishes only that THIS
			// record never spoke; the gate below asks the wider question the
			// ruling actually turns on — has this WORKSPACE ever had a
			// conversation — over the same evidence, through the same
			// function. A handshake-only record in a workspace that has been
			// talking for a week is not a blank slate, and letting the waiver
			// mint its own permission is how it would become one.
		} else {
			logResumeContinuityFailure(s.logf, "automatic_restore", sessionID, opts, missing)
			return res, missing
		}
	}
	// THE STRUCTURAL GATE. Every path that reaches a spawn with no --resume
	// passes through here, because EnsureShim is the daemon's only spawn site:
	// Server.CreateSession registers the record and hands bring-up to the
	// controller, which arrives back at this function. A record that names no
	// conversation is therefore either a workspace that never had one — which
	// the evidence must SAY, not merely fail to contradict — or a conversation
	// about to be silently replaced.
	//
	// Fake sessions are the one standing exception, on the grounds the resume
	// gate already waives them for: the scripted offline SDK has no
	// conversation plane at all, so there is nothing a blank start could
	// destroy.
	if opts.Resume == "" && freshProof == nil {
		if fake {
			s.logf("server: session %s: no-resume spawn permitted for a FAKE session — the scripted offline SDK has no conversation to lose (cwd=%q)",
				sessionID, opts.CWD)
		} else {
			proof, reason := proveFreshEligible(gatherConversationEvidence(s.reg, rec.ConfigDir, rec.CWD))
			if proof == nil {
				// THE RESTORE RUNG AGAIN, and this placement recovers the worst
				// case: the workspace HAS a conversation, nothing on the record
				// names a resumable one, and the refusal below is otherwise the
				// end of the road. With no uuid to aim at, the newest backup
				// this workspace holds names the conversation it should be
				// returned to.
				noTarget := opts
				noTarget.Resume = ""
				restored, restoreErr := attemptTranscriptRestore(s.logf, "automatic_restore", sessionID, noTarget)
				if restoreErr != nil {
					return res, restoreErr
				}
				if recovered, ok := newestBackupConversation(rec.CWD); restored && ok {
					// Resumed rather than refused, and the spawn below carries
					// it exactly as an ordinary resume would: the whole point of
					// the backup plane is that a recovered conversation is not
					// a special kind of session afterwards.
					opts.Resume = recovered
					s.logf("server: session %s: conversation %s RECOVERED from backup and will be RESUMED rather than refused (cwd=%q)",
						sessionID, recovered, rec.CWD)
				} else {
					s.logf("server: session %s: no-resume spawn REFUSED cwd=%q config_dir=%q — %s",
						sessionID, rec.CWD, rec.ConfigDir, reason)
					return res, unresumableConversation(rec.CWD, rec.ConfigDir, reason)
				}
			} else {
				freshProof = proof
				s.logf("server: session %s: no-resume spawn PERMITTED cwd=%q config_dir=%q — the registry proves this workspace has never run a conversation",
					sessionID, rec.CWD, rec.ConfigDir)
			}
		}
	}
	if opts.Fake && opts.Resume != "" {
		s.logf("server: session %s: resume viability gate WAIVED for respawn (this daemon forces the scripted offline SDK, which writes no transcript) resume=%q",
			sessionID, opts.Resume)
	}
	res.Resumed = opts.Resume
	s.logf("server: session %s: no live shim — spawning UDS shim (resume=%q fake=%t)", sessionID, opts.Resume, opts.Fake)
	handle, err := s.spawnShim(sessionID, opts, freshProof)
	if err != nil {
		// The lineage stays on the record: this spawn never announced it, so
		// the next bring-up still owes it.
		return res, err
	}
	if handle.Stop != nil {
		s.mu.Lock()
		s.handles[sessionID] = handle
		s.mu.Unlock()
	}
	if lineage.Armed() {
		if err := s.consumeRewindLineage(sessionID); err != nil {
			s.logf("server: session %s: rewind lineage CONSUMPTION FAILED after the spawn announced it: %v — the record still carries an announced lineage, and the next spawn would announce a rewind that never happened", sessionID, err)
			return res, err
		}
		s.logf("server: session %s: rewind lineage CONSUMED by the spawn that announced it rewound_from=%s dropped_turns=%s",
			sessionID, lineage.PreviousVendorSessionID, lineage.DroppedTurnIDs)
	}
	return res, nil
}

// resumeTargetCarriesAConversation reports whether this record's vendor uuid
// has ever named an actual exchange, as opposed to a handshake the vendor
// answered with an identity and nothing else.
//
// LastTurnEndMs IS THE AUTHORITY, and it is the registry's own: it is written
// by the turn-boundary sink when a turn ENDS, it is durable precisely so it
// survives the restarts an in-memory flag would not, and zero means no turn has
// ever ended under this record. It is deliberately not re-derived from the file
// plane or the event store here — a second definition of "has this conversation
// said anything" is how two subsystems come to disagree about one session.
//
// A record that HAS run a turn always carries a conversation, whether or not
// its transcript is currently readable; that is the case the resume gate exists
// to refuse, and this function reports true for it.
func resumeTargetCarriesAConversation(rec registry.Record) bool {
	return rec.LastTurnEndMs > 0
}

// DropResume clears the session's vendor conversation pointer so the next spawn
// starts fresh, reporting what it dropped.
//
// This is an explicit administrative operation. Automatic restoration never
// calls it: an invalid resume target hard-fails without mutating durable
// identity. A write failure is surfaced so callers cannot mistake an unchanged
// record for a successful administrative reset.
func (s *ShimSpawner) DropResume(sessionID string) (string, error) {
	if s.reg == nil {
		return "", fmt.Errorf("server: ShimSpawner has no registry; cannot drop session %s's resume pointer", sessionID)
	}
	var dropped string
	found, err := s.reg.Update(sessionID, func(rec *registry.Record) {
		dropped = rec.ClaudeSessionID
		rec.ClaudeSessionID = ""
	})
	if err != nil {
		return "", fmt.Errorf("server: session %s: dropping the resume pointer failed: %w", sessionID, err)
	}
	if !found {
		return "", fmt.Errorf("server: session %s: dropping the resume pointer found no record (never registered)", sessionID)
	}
	if dropped != "" {
		s.logf("server: session %s: vendor conversation pointer %s DROPPED from the record", sessionID, dropped)
	}
	return dropped, nil
}

// StopShim SIGTERMs the session's shim (hibernation's clean stop, and the stop
// half of a hard restart).
//
// TWO HANDLES, ONE PROCESS, in strict order of authority:
//
//  1. THE PROCESS HANDLE this spawner kept when it launched the shim. Exact,
//     and the ordinary case.
//  2. THE PID the shim announced on its ShimHello (hintPID), used only when
//     there is no handle — a shim that outlived a PREVIOUS daemon, which this
//     process never spawned. Without it StopShim was a permanent no-op for
//     exactly those shims, so a survivor could not be bounced onto a new
//     bundle and an explicit session restart silently did nothing to it.
//
// The pid is not a guess and not a pid-reuse hazard: the caller only has one
// while the connection that carried it is live, and a live connection is proof
// the process on the other end is the process that opened it.
//
// No handle and no pid has no process to signal, but it is not a successful
// stop until the session lock proves that no unreaped shim owns the session.
// THE ATTRIBUTION IS THE CALLER'S, and it is required.
//
// This used to substitute one coarse package-level constant for every stop the
// daemon issued, because the Spawner signature carried no reason: an idle sweep,
// a merged teardown and a hard restart all reached the shim as "commanded
// session stop (hibernation or restart)", and the log could not tell them
// apart at the record. `by` now travels from the closed cause vocabulary at the
// session controller's one stop funnel, and it is VALIDATED HERE at the
// boundary: an unattributed stop is refused outright rather than defaulted,
// because a default is exactly what made the previous log unable to answer the
// question these records exist for.
func (s *ShimSpawner) StopShim(sessionID string, hintPID int32, by shim.Stop) error {
	if err := by.Validate(); err != nil {
		s.logf("server: session %s: SHIM STOP REFUSED hint_pid=%d initiator=%q reason=%q: %v — nothing was signalled, because a stop this daemon cannot attribute is a stop its log cannot explain",
			sessionID, hintPID, by.Initiator, by.Reason, err)
		return fmt.Errorf("server: session %s: refusing an unattributed shim stop: %w", sessionID, err)
	}
	s.mu.Lock()
	handle, ok := s.handles[sessionID]
	if s.awaitStopped == nil {
		s.mu.Unlock()
		return fmt.Errorf("server: session %s: cannot prove shim lock release because the exit observer is nil", sessionID)
	}
	if ok {
		delete(s.handles, sessionID)
	}
	s.mu.Unlock()
	switch {
	case ok:
		s.logf("server: session %s: stopping shim (SIGTERM via our own process handle) initiator=%s reason=%s", sessionID, by.Initiator, by.Reason)
		if err := handle.Stop(by); err != nil {
			return err
		}
	case hintPID > 0:
		// THE SURVIVOR'S STOP IS ATTRIBUTED TOO. A signal sent by pid leaves no
		// stop record of its own — the process handle that would have written
		// one belongs to a daemon that is gone — so this line is the only place
		// the survivor's death is explained.
		if err := signallablePID(sessionID, hintPID); err != nil {
			s.logf("server: session %s: SHIM STOP REFUSED — %v; nothing was signalled", sessionID, err)
			return err
		}
		s.logf("server: session %s: no daemon-spawned shim; stopping the SURVIVING shim by its announced pid %d (SIGTERM) initiator=%s reason=%s",
			sessionID, hintPID, by.Initiator, by.Reason)
		if err := s.signal(int(hintPID), syscall.SIGTERM); err != nil {
			if errors.Is(err, os.ErrProcessDone) {
				s.logf("server: session %s: shim pid %d had already exited", sessionID, hintPID)
				break
			}
			return fmt.Errorf("server: session %s: stopping surviving shim pid %d: %w", sessionID, hintPID, err)
		}
	default:
		s.logf("server: session %s: StopShim has no process handle or announced pid; proving the session lock is absent before reporting success", sessionID)
	}
	// STOPPED MEANS GONE, which is what every caller already assumed and the
	// signal alone does not deliver.
	//
	// A SIGTERM only ASKS. EnsureShim refuses to spawn while the session lock
	// is held, so a restart that stopped a shim and immediately ensured raced
	// the kernel releasing that lock and reliably failed — a bounce onto a new
	// bundle simply never respawned. Returning before the session is actually
	// free makes this method a lie its own callers are then obliged to work
	// around.
	//
	// The wait is on the CONDITION the spawn is gated by — the session lock,
	// which the kernel releases when the holder dies, however it dies — not on
	// a duration chosen to be probably long enough.
	//
	// A SIGTERM THAT IS IGNORED ESCALATES. The bound elapsing with the lock
	// still held is the one outcome that licenses a harder signal, because it
	// is proof that a process is alive for this session; every other error from
	// the wait means the truth is unknown, and an unknown truth is a hard
	// failure rather than grounds to kill something.
	termGrace, killGrace := s.stopGraces()
	if err := s.awaitStopped(sessionID, termGrace); err != nil {
		if !errors.Is(err, errStopWaitExpired) {
			return err
		}
		if err := s.escalateStopToKill(sessionID, hintPID, by, termGrace, killGrace, err); err != nil {
			return err
		}
	}
	s.awaitReaped(sessionID, handle)
	s.evictStoppedParked(sessionID)
	return nil
}

// stopGraces resolves the escalation bounds, honoring the test overrides.
func (s *ShimSpawner) stopGraces() (term, kill time.Duration) {
	term, kill = stopTermGrace, stopKillGrace
	if s.termGraceOverride > 0 {
		term = s.termGraceOverride
	}
	if s.killGraceOverride > 0 {
		kill = s.killGraceOverride
	}
	return term, kill
}

// escalateStopToKill is the second rung of the stop: SIGKILL, then the same
// bounded wait on the same condition.
//
// # Why a second rung exists at all
//
// A SIGTERM only ASKS, and a shim that is wedged — blocked in the vendor SDK,
// stuck on a write, mid-uninterruptible-syscall — does not answer. Reporting
// the stop successful there hands the caller a workspace it believes is free
// while a live process still owns the transcript, which is how two shims end
// up writing one conversation. The escalation removes the "asked nicely and
// gave up" outcome; what remains is a dead process or a loud typed failure.
//
// # What licenses signalling this pid
//
// The pid is the one the shim announced on its own ShimHello, over its own
// connection to this daemon — so it was reported by the process that opened
// that connection, not inferred. That is the strongest identity claim
// available: a pid cannot be tied to a process portably from inside Go, and
// this deliberately does NOT pretend otherwise by reading /proc or shelling
// out to ps.
//
// What makes the signal justified rather than a guess is the session lock. The
// escalation is reached ONLY from an expired wait, which means the lock was
// still held at the deadline — so a process is provably alive for this session
// at that instant. The signal is aimed at the only pid that process ever
// claimed. If the pid has been recycled onto something else the kernel refuses
// or the wait still expires, and the typed failure below is the outcome; the
// stop never reports success it did not earn.
//
// A pid whose process is already gone is a SUCCESS, not a failure: the thing
// this wants dead is dead. The lock wait still runs, because the lock rather
// than the signal is the proof.
func (s *ShimSpawner) escalateStopToKill(sessionID string, hintPID int32, by shim.Stop, termGrace, killGrace time.Duration, termErr error) error {
	if hintPID <= 0 {
		s.logf("server: session %s: shim SURVIVED SIGTERM after %s and this daemon holds no pid to escalate to initiator=%s reason=%s — the stop cannot be completed and is reported FAILED rather than assumed",
			sessionID, termGrace, by.Initiator, by.Reason)
		return fmt.Errorf("%w: session %s ignored SIGTERM for %s and no announced pid is available to escalate to: %w",
			ErrShimSurvivedStop, sessionID, termGrace, termErr)
	}
	if err := signallablePID(sessionID, hintPID); err != nil {
		s.logf("server: session %s: shim SURVIVED SIGTERM after %s and the escalation is REFUSED — %v", sessionID, termGrace, err)
		return err
	}
	s.logf("server: session %s: shim SURVIVED SIGTERM after %s; escalating to SIGKILL at the announced pid %d initiator=%s reason=%s — the session lock was still held at the deadline, so a process is alive for this session, and this pid is the only one its shim ever announced",
		sessionID, termGrace, hintPID, by.Initiator, by.Reason)
	if err := s.signal(int(hintPID), syscall.SIGKILL); err != nil {
		if !errors.Is(err, os.ErrProcessDone) {
			return fmt.Errorf("server: session %s: SIGKILL of shim pid %d: %w", sessionID, hintPID, err)
		}
		s.logf("server: session %s: shim pid %d was already gone when the SIGKILL was delivered; the session lock is still the proof", sessionID, hintPID)
	}
	if err := s.awaitStopped(sessionID, killGrace); err != nil {
		if !errors.Is(err, errStopWaitExpired) {
			return err
		}
		s.logf("server: session %s: shim SURVIVED SIGKILL at pid %d after a further %s initiator=%s reason=%s — the stop FAILED and nothing may proceed as though this session were free",
			sessionID, hintPID, killGrace, by.Initiator, by.Reason)
		return fmt.Errorf("%w: session %s survived SIGTERM (%s) and SIGKILL (%s) at pid %d: %w",
			ErrShimSurvivedStop, sessionID, termGrace, killGrace, hintPID, err)
	}
	s.logf("server: session %s: shim pid %d is gone after SIGKILL; the stop completed at the second rung", sessionID, hintPID)
	return nil
}

// signallablePID refuses the pids that can never name a shim.
//
// Neither check is theoretical. A zero or negative pid is a process GROUP or
// "every process we may signal" to kill(2), and the daemon's own pid would have
// it kill itself while reporting a shim stopped. Both are refusals rather than
// no-ops: a stop that cannot name its target has not established anything.
func signallablePID(sessionID string, pid int32) error {
	switch {
	case pid <= 1:
		return fmt.Errorf("server: session %s: refusing to signal pid %d, which names no shim (kill(2) reads it as a process group or as init)", sessionID, pid)
	case int(pid) == os.Getpid():
		return fmt.Errorf("server: session %s: refusing to signal pid %d, which is this daemon's own process", sessionID, pid)
	}
	return nil
}

// awaitReaped completes the stop of a shim THIS daemon spawned: it waits for
// the reaper to finish with the process, after the lock wait has already proved
// the process gone.
//
// TWO OBSERVERS OF ONE DEATH, and they do not land together. The lock wait ends
// in the KERNEL, at the instant the dying process's lock is released; the
// reaper's Exited and Reaped hooks run afterwards, in this daemon, on the
// reaper goroutine. Returning on the first of those made "stopped" true before
// the exit had been accounted for, so a respawn issued immediately afterwards
// could be running — a second process, a second spawn record — while shim 1's
// exit was still being reported. The exit then read as the NEW shim's, which is
// exactly how a bounce ends up with a live shim that everything believes died.
//
// It is a RENDEZVOUS, not a bound: the reaper always runs, and the process is
// already known to be gone before the wait begins. A spawner with no reaper
// (a survivor stopped by pid, a harness that starts no process) has no channel
// and nothing to wait for.
func (s *ShimSpawner) awaitReaped(sessionID string, handle ShimHandle) {
	if handle.Reaped == nil {
		return
	}
	<-handle.Reaped
	s.logf("server: session %s: shim reaper completed; the stop is finished", sessionID)
}

func (s *ShimSpawner) evictStoppedParked(sessionID string) {
	if s.evict == nil {
		return
	}
	evicted := s.evict(sessionID, "shim_process_stop_completed")
	s.logf("server: session %s: parked transport cleanup after stop evicted=%t", sessionID, evicted)
}

// ErrShimSurvivedStop reports that a shim was signalled, escalated to SIGKILL,
// and STILL held its session lock — so the process this daemon asked to die is
// alive and owns the session.
//
// It is a sentinel because of what a caller must do differently. An ordinary
// stop failure says the teardown did not complete; this says a live shim owns
// the conversation, so anything that would put a SECOND shim on it — a
// supersede minting a replacement session, a bounce respawning onto a new
// bundle — must abort rather than continue. That decision cannot be made by
// matching message text.
var ErrShimSurvivedStop = errors.New("server: the shim survived its stop and still holds its session lock")

// errStopWaitExpired reports that a bounded wait for a signalled shim to
// release its session lock elapsed with the lock still held.
//
// It is distinct from every other error the wait can produce, and the
// distinction is load-bearing: this one means "a process is provably alive for
// this session", which is what licenses escalating to a harder signal, while
// any other error means the truth could not be read at all and nothing may be
// signalled on the strength of it.
var errStopWaitExpired = errors.New("server: the shim still holds its session lock")

// stopTermGrace bounds how long a SIGTERMed shim is given to exit before the
// stop escalates to SIGKILL. It is a FAILURE bound, not a delay: the wait ends
// the instant the session lock is free, and this only decides how long a clean
// stop is given before it stops being asked for politely. Generous, because a
// shim shutting down cleanly flushes its transcript and closes its store link.
const stopTermGrace = 10 * time.Second

// stopKillGrace bounds how long a SIGKILLed shim is given to disappear.
//
// Short, and for a structural reason rather than a tuned one: SIGKILL cannot be
// caught, blocked or ignored, so the only thing being waited on is the kernel
// tearing the process down and releasing its flock. A wait that expires here
// means the lock is held by something that is not the process that was killed,
// which is a fact to report loudly rather than to wait longer for.
const stopKillGrace = 2 * time.Second

// stopPoll is how often the session lock is re-probed while waiting.
//
// A POLL, and it is worth saying why rather than pretending otherwise: a
// kernel flock has no readiness channel to wait on. It is released by process
// death and the only way to observe that is to try to take it. The alternative
// — waiting on the child's exit — covers ONLY shims this daemon spawned, and
// the surviving shim is exactly the case that needs this. So the condition
// itself is the right one; the sampling is the part the kernel does not offer
// a better form of.
const stopPoll = 20 * time.Millisecond

// awaitShimStopped blocks until no process holds sessionID's lock, i.e. until
// the shim is really gone, for at most within.
//
// An unreadable lock ends the wait with that error rather than a guess: "I
// could not tell" must never be read as "it is free". The bound elapsing is
// reported as errStopWaitExpired specifically, because that is the one outcome
// the caller may escalate on.
func awaitShimStopped(sessionID string, within time.Duration) error {
	deadline := time.Now().Add(within)
	for {
		held, err := sessionlock.Held(sessionID)
		if err != nil {
			return fmt.Errorf("server: session %s: waiting for its shim to exit: %w", sessionID, err)
		}
		if !held {
			return nil
		}
		if time.Now().After(deadline) {
			return fmt.Errorf("%w: session %s is still holding it %s after the signal; it is not stopping", errStopWaitExpired, sessionID, within)
		}
		time.Sleep(stopPoll)
	}
}

// RegistryRegistrar binds the session controller's claude_session_id write-through
// (sessioncontroller.SessionRegistrar) to the persistent registry: when a session's
// SessionStarted reports its CLI session uuid, it lands on the durable record
// so --resume and rehydration survive a daemon restart. It replaces the old
// L2 session hub's registrar, which the deleted stdio Run loop used to drive.
type RegistryRegistrar struct {
	Reg  *registry.Registry
	Logf func(string, ...any)
	// ModelCatalogs retains query-owned menus and feeds frontend reconnect
	// snapshots through the same SessionView shaping as ordinary pushes.
	ModelCatalogs *SessionModelCatalogs
	// PushView re-pushes a session's SessionView after a record write, so a
	// CONNECTED frontend sees the change rather than waiting for whatever
	// unrelated event next happens to push one. Late-bound by main (the Server
	// does not exist when the registrar is built), which is why the registrar
	// is held by pointer. Nil-safe: the connect snapshot still carries the
	// record, so a nil pusher costs freshness, never correctness.
	PushView func(sessionID string)
	// Now stamps the instant a WINDOW-shaped death was resolved
	// (supersederesolve.go), in unix millis. Nil takes the wall clock; it is a
	// field so a harness can assert the exact stamp rather than a range.
	Now func() int64
	// Backups copies a workspace's vendor transcript aside at the two
	// boundaries this registrar is the first to hear about: a turn ending and
	// a vendor uuid rotating. Optional — a nil writer takes no copies, which
	// is only right in a unit harness, because a daemon with no backups can
	// only ever answer a lost transcript with the ladder's hard fault.
	Backups *TranscriptBackups
}

// SessionModelCatalogObserved accepts a shim-published menu, then re-pushes
// the session view. Model selection remains owned by the shim; this only
// preserves the SDK's offered choices for frontend rendering.
func (r *RegistryRegistrar) SessionModelCatalogObserved(sessionID string, models []*corev1.ModelOption) error {
	if r.ModelCatalogs == nil {
		return fmt.Errorf("server: session %s: model catalog received without a catalog store", sessionID)
	}
	if err := r.ModelCatalogs.Set(sessionID, models); err != nil {
		if r.Logf != nil {
			r.Logf("server: session %s: REFUSING malformed model catalog: %v", sessionID, err)
		}
		return err
	}
	if r.Logf != nil {
		r.Logf("server: session %s: model catalog updated models=%d", sessionID, len(models))
	}
	r.repush(sessionID)
	return nil
}

// repush delivers the post-write SessionView push, if one is wired.
func (r *RegistryRegistrar) repush(sessionID string) {
	if r.PushView != nil {
		r.PushView(sessionID)
	}
}

// TerminalAccountingPersisted republishes the session aggregate immediately
// after terminal accounting is durable and its terminal conversation item has
// been delivered.
func (r *RegistryRegistrar) TerminalAccountingPersisted(sessionID string) {
	if r.Logf != nil {
		r.Logf("server: terminal accounting persisted session=%s; republishing SessionView", sessionID)
	}
	r.repush(sessionID)
}

// HistoricalTokenUtilizationPersisted republishes the session aggregate after
// a normalized file-plane response becomes durable.
func (r *RegistryRegistrar) HistoricalTokenUtilizationPersisted(sessionID string) {
	if r.Logf != nil {
		r.Logf("server: historical token utilization persisted session=%s; republishing SessionView", sessionID)
	}
	r.repush(sessionID)
}

// BackfillStateChanged persists the never-blue backfill signal (F2) on
// sessionID's record and re-pushes its SessionView.
//
// This is the completion signal the switch-ensure keys its already-live skip
// on: without it a live session whose history never arrived is indistinguishable
// from one whose history is fully rendered, and the workspace stays blue. Same
// loud-on-failure contract as its siblings.
func (r *RegistryRegistrar) BackfillStateChanged(sessionID, state string) {
	if r.Reg == nil {
		return
	}
	found, err := r.Reg.Update(sessionID, func(rec *registry.Record) { rec.BackfillState = state })
	if err != nil && r.Logf != nil {
		r.Logf("server: session %s: registry backfill_state write FAILED — the workspace may read as blue after a restart: %v", sessionID, err)
		return
	}
	if !found && r.Logf != nil {
		r.Logf("server: session %s: backfill_state write found no record (never registered)", sessionID)
		return
	}
	r.repush(sessionID)
}

// SessionModelObserved persists the model a live session reports itself to be
// running, and re-pushes its SessionView so the frontend's model chip follows.
//
// This is the write that closes the model's round trip. rec.Model was set once
// from the create command and read back on every respawn, so a session that
// changed model mid-life was relaunched as the ORIGINAL model after each
// hibernation. The observed value is the only one a respawn should trust.
//
// Idempotent by value: the SDK re-announces its init on every submit, so this
// is called constantly with an unchanged model and must not write each time.
func (r *RegistryRegistrar) SessionModelObserved(sessionID, model string) {
	if r.Reg == nil || model == "" {
		return
	}
	changed := false
	found, err := r.Reg.Update(sessionID, func(rec *registry.Record) {
		if rec.Model == model {
			return
		}
		if r.Logf != nil {
			r.Logf("server: session %s: observed model %q replaces the record's %q", sessionID, model, rec.Model)
		}
		rec.Model = model
		changed = true
	})
	if err != nil && r.Logf != nil {
		r.Logf("server: session %s: registry model write FAILED — a respawn may re-pin the stale model: %v", sessionID, err)
		return
	}
	if !found && r.Logf != nil {
		r.Logf("server: session %s: model write found no record (never registered)", sessionID)
		return
	}
	if changed {
		r.repush(sessionID)
	}
}

// SessionDied marks sessionID's record terminal with the reason its death
// carried (F4) and re-pushes its SessionView.
//
// This is the write that never existed: a shim death resolved the workspace
// dead through the SSM while the record still claimed the session was alive,
// so the dead-state card had nothing to explain itself with. An ALREADY
// terminal record is left alone — the FIRST reason is the true one, and a
// later shim exit must not overwrite "the user deleted this" with "the
// process exited".
func (r *RegistryRegistrar) SessionDied(sessionID, reason string) {
	if r.Reg == nil {
		return
	}
	found, err := r.Reg.Update(sessionID, func(rec *registry.Record) {
		if rec.Terminal {
			return
		}
		rec.Terminal = true
		rec.DeathReason = reason
	})
	if err != nil && r.Logf != nil {
		r.Logf("server: session %s: registry death write FAILED — the session will read as alive after a restart: %v", sessionID, err)
		return
	}
	if !found && r.Logf != nil {
		r.Logf("server: session %s: death write found no record (never registered)", sessionID)
		return
	}
	r.repush(sessionID)
}

// ClaudeSessionIDChanged persists claudeSessionID on sessionID's record. A
// missing record or a write failure is loud-logged, never silently dropped
// (the session would not survive a restart).
// ADOPTION IS EAGER. A first adoption used to be REFUSED until a turn proved
// the vendor had written the conversation on disk, so that a later bring-up
// could not --resume into a transcript that never existed. That protection now
// lives at the point of USE instead: ConversationResolver stats the transcript
// when it resolves a resume target and skips any conversation the vendor never
// wrote. Checking at use is strictly stronger than refusing to write down —
// same authority, consulted when the answer actually matters — and it removes
// the window in which the registry knowingly held an empty uuid while the shim
// and the webapp both knew the real one. That disagreement was not free: it
// made every client log fail attribution and nack, forever, for any session
// that had not yet run a turn.
func (r *RegistryRegistrar) ClaudeSessionIDChanged(sessionID, claudeSessionID string) bool {
	if r.Reg == nil {
		return false
	}
	found, err := r.Reg.Update(sessionID, func(rec *registry.Record) {
		rec.ClaudeSessionID = claudeSessionID
	})
	if err != nil {
		if r.Logf != nil {
			r.Logf("server: session %s: registry claude_session_id write FAILED — resume may break after a restart: %v", sessionID, err)
		}
		return false
	}
	if !found {
		if r.Logf != nil {
			r.Logf("server: session %s: claude_session_id write found no record (never registered)", sessionID)
		}
		return false
	}
	return true
}

// AdoptVendorSessionID adopts claudeSessionID as sessionID's vendor uuid,
// reporting whether that ROTATED an already-adopted different one and what it
// replaced.
//
// ON A ROTATION THE CONVERSATION'S CURSORS ARE RESET IN THE SAME WRITE. Both
// of them count in the store seq space the retired uuid keyed, and the vendor
// has just started a fresh space at 1:
//
//   - LastSeq is where the daemon re-Subscribes from. Left standing it would
//     ask the new space for events past its end — nothing arrives — and then
//     read the space's own seq=1 as a terminal ErrSeqRegression.
//   - NewestClearOrCompactSeq is the frontend replay floor. Left standing it
//     would sit ABOVE every seq the new space will ever produce for a long
//     while, flooring away the whole post-rotation conversation, including the
//     very clear that caused the rotation.
//
// ONE WRITE, not three, and that is load-bearing rather than tidy: registry
// maintenance hydrates a record's cursors up from the conversation checkpoint
// filed under its CURRENT uuid on every mutation, so a reset landing while the
// old uuid still stood would be silently undone before the new uuid was
// recorded. Resetting under the NEW uuid instead files a fresh checkpoint at
// zero, which is the truth about a seq space that has just begun.
//
// A first adoption (no uuid yet) and a re-announcement of the SAME uuid both
// report rotated=false and reset nothing.
//
// A FIRST adoption is additionally REFUSED without durable evidence, reporting
// adopted=false and writing nothing — see the ADOPT LATE block on
// sessioncontroller.SessionRegistrar. A ROTATION is never refused: its `previous` is
// non-empty by definition, so the vendor demonstrably wrote the conversation
// being rotated away from and the reset that accompanies it must still land.
func (r *RegistryRegistrar) AdoptVendorSessionID(sessionID, claudeSessionID string) (bool, string, bool) {
	return r.adoptVendorSessionID(sessionID, claudeSessionID, registry.RewindLineage{})
}

// AdoptRewoundVendorSessionID is AdoptVendorSessionID with the rewind's lineage
// written by the SAME Update. It implements
// sessioncontroller.VendorSessionAdopter.
//
// THE FLIP AND THE LINEAGE ARE ONE DURABLE FACT. The flip is the rewind's only
// destructive act and the lineage is the only account of what it dropped;
// written separately, a daemon dying in between left a record naming a
// truncated conversation with nothing left to say it had been truncated, and
// the SessionRewound the frontends replay from was emitted by nobody. Written
// together, a crash in any window leaves either both or neither — and the next
// spawn of a record that carries one replays the argv.
//
// An INCOMPLETE lineage is refused and NOTHING is written, flip included. The
// shim rejects an empty dropped-turn list, so adopting the new uuid while
// arming a partial lineage would leave the session pointing at the truncated
// transcript AND unable to spawn on it.
func (r *RegistryRegistrar) AdoptRewoundVendorSessionID(sessionID, claudeSessionID string, lineage sessioncontroller.RewindLineage) (bool, string, bool) {
	durable := registry.RewindLineage{
		PreviousVendorSessionID: lineage.PreviousVendorSessionID,
		RetainedLeafUUID:        lineage.RetainedLeafUUID,
		DroppedTurnIDs:          lineage.DroppedTurnIDs,
	}
	if !durable.Armed() {
		if r.Logf != nil {
			r.Logf("server: session %s: REFUSING the rewind flip to %s — the lineage is incomplete (rewound_from=%q retained_leaf=%q dropped_turns=%q); the shim rejects an empty dropped-turn list, so adopting it would leave the session naming a transcript it cannot spawn on",
				sessionID, claudeSessionID, durable.PreviousVendorSessionID, durable.RetainedLeafUUID, durable.DroppedTurnIDs)
		}
		return false, "", false
	}
	return r.adoptVendorSessionID(sessionID, claudeSessionID, durable)
}

// adoptVendorSessionID is the one Update both adoption entry points take. An
// armed lineage is stored beside the uuid it belongs to; the zero value leaves
// whatever the record carries alone.
func (r *RegistryRegistrar) adoptVendorSessionID(sessionID, claudeSessionID string, lineage registry.RewindLineage) (bool, string, bool) {
	if r.Reg == nil || claudeSessionID == "" {
		return false, "", false
	}
	var (
		previous string
		rotated  bool
	)
	found, err := r.Reg.Update(sessionID, func(rec *registry.Record) {
		previous = rec.ClaudeSessionID
		rotated = previous != "" && previous != claudeSessionID
		rec.ClaudeSessionID = claudeSessionID
		if lineage.Armed() {
			rec.Rewind = lineage
		}
		if rotated {
			rec.LastSeq = 0
			rec.NewestClearOrCompactSeq = 0
		}
	})
	if err != nil {
		if r.Logf != nil {
			r.Logf("server: session %s: registry vendor session adoption (uuid=%s) FAILED — a rotation's store cursor reset did not land, so the resubscribe may read the new seq space as a regression: %v",
				sessionID, claudeSessionID, err)
		}
		return false, previous, false
	}
	if !found {
		if r.Logf != nil {
			r.Logf("server: session %s: vendor session adoption (uuid=%s) found no record (never registered)", sessionID, claudeSessionID)
		}
		return false, previous, false
	}
	if rotated {
		if r.Logf != nil {
			r.Logf("server: session %s: VENDOR SESSION ROTATED %s -> %s — last_seq and the replay floor reset to zero for the new store seq space",
				sessionID, previous, claudeSessionID)
		}
		// THE RETIRING CONVERSATION'S LAST CHANCE. A rotation is the one
		// moment a transcript stops being appended to forever, and the record
		// now points at its successor — so the retiring uuid is named
		// explicitly here rather than read back off the record, which would
		// copy the new empty transcript and leave the retired one uncovered.
		if r.Backups != nil {
			if rec, ok := r.Reg.Get(sessionID); ok {
				r.Backups.CaptureConversation(rec.CWD, rec.ConfigDir, previous, "vendor_session_rotated")
			}
		}
		// The session's conversation IDENTITY just changed, which is exactly
		// the kind of record change a connected frontend should not have to
		// wait for an unrelated event to learn.
		r.repush(sessionID)
	}
	return rotated, previous, true
}

// QueuedPromptsChanged persists the prompts the daemon is currently holding for
// sessionID (E4). Same loud-on-failure contract as above: these are things the
// user typed that the agent has not seen, so losing the record silently is the
// one outcome that must not happen.
func (r *RegistryRegistrar) QueuedPromptsChanged(sessionID string, queued []registry.QueuedPrompt) {
	if r.Reg == nil {
		return
	}
	found, err := r.Reg.Update(sessionID, func(rec *registry.Record) { rec.QueuedPrompts = queued })
	if err != nil && r.Logf != nil {
		r.Logf("server: session %s: registry queued_prompts write FAILED — held prompts will not survive a restart: %v", sessionID, err)
		return
	}
	if !found && r.Logf != nil {
		r.Logf("server: session %s: queued_prompts write found no record (never registered)", sessionID)
	}
}

// TurnEndObserved persists when sessionID's most recent turn ended. It is the
// ONE input to the cache keep-alive policy, and persisting it is what makes
// every decision a time-since check against a durable instant rather than a
// timer's guess (see registry.Record.LastTurnEndMs).
//
// The write is loud on failure and CHANGES NOTHING ELSE. A lost timestamp does
// not corrupt anything — the policy's own "every unknown answers none" rule
// leaves an undated session alone — but it does silently switch the keep-alive
// off for that session, which is exactly the kind of quiet degradation the log
// line exists to make findable.
func (r *RegistryRegistrar) TurnEndObserved(sessionID string, atMs int64) {
	if r.Reg == nil || atMs <= 0 {
		return
	}
	found, err := r.Reg.Update(sessionID, func(rec *registry.Record) {
		// NEVER BACKWARDS. A late-arriving end for an older turn must not
		// rewind the clock the policy reads, or a session would be pinged
		// against a turn boundary it has already moved past.
		if atMs > rec.LastTurnEndMs {
			rec.LastTurnEndMs = atMs
		}
	})
	if err != nil && r.Logf != nil {
		r.Logf("server: session %s: registry last_turn_end_ms write FAILED at_ms=%d — the cache keep-alive has no durable instant to measure this session from and will leave it alone: %v",
			sessionID, atMs, err)
		return
	}
	// A TURN THAT ENDED IS A TRANSCRIPT AT REST. The vendor has finished
	// appending, so this is the cheapest moment at which a copy is worth
	// exactly one whole exchange. It is deliberately AFTER the durable write
	// and it never reports upward: a backup that could not be taken is loud in
	// the log and changes nothing about the turn.
	if r.Backups != nil {
		r.Backups.Capture(sessionID)
	}
	if !found && r.Logf != nil {
		r.Logf("server: session %s: last_turn_end_ms write found no record (never registered) at_ms=%d", sessionID, atMs)
	}
}

// HibernationChanged persists a session's hibernation state and its typed
// account in ONE write, then re-pushes the SessionView so the revival gate
// appears without waiting for an unrelated event.
//
// THE FLAG AND ITS ACCOUNT ARE ONE ARGUMENT, not two calls. A zero detail
// clears hibernation; a detail with a cause sets it. There is deliberately no
// way to write one without the other, which is the same guarantee
// registry.maintain enforces on the way to disk — expressed here so a caller
// cannot even construct the illegal pair.
func (r *RegistryRegistrar) HibernationChanged(sessionID string, detail registry.HibernationDetail) error {
	if r.Reg == nil {
		return nil
	}
	if !registry.ValidHibernationCause(detail.Cause) {
		return fmt.Errorf("server: session %s: refusing hibernation write with unknown cause %q", sessionID, detail.Cause)
	}
	found, err := r.Reg.Update(sessionID, func(rec *registry.Record) {
		rec.Hibernated = detail.Cause != ""
		rec.Hibernation = detail
	})
	if err != nil {
		if r.Logf != nil {
			r.Logf("server: session %s: registry hibernation write FAILED cause=%q since_ms=%d — the sleep will not survive a restart and the session could be revived implicitly: %v",
				sessionID, detail.Cause, detail.SinceMs, err)
		}
		return fmt.Errorf("server: session %s: persist hibernation: %w", sessionID, err)
	}
	if !found {
		if r.Logf != nil {
			r.Logf("server: session %s: hibernation write found no record (never registered) cause=%q", sessionID, detail.Cause)
		}
		return fmt.Errorf("server: session %s: persist hibernation: no such record", sessionID)
	}
	if r.Logf != nil {
		r.Logf("server: session %s: hibernation state persisted cause=%q since_ms=%d cutoff_ms=%d elapsed_ms=%d ttl_ms=%d hibernated=%v",
			sessionID, detail.Cause, detail.SinceMs, detail.CutoffMs, detail.ElapsedMs, detail.TTLMs, detail.Cause != "")
	}
	r.repush(sessionID)
	return nil
}

// HibernationOf reports sessionID's persisted hibernation detail and whether a
// record was found. It is the rehydration read: a daemon that just booted has
// no live controller to ask, and the durable record is the only thing that
// knows the session was deliberately put to sleep.
func (r *RegistryRegistrar) HibernationOf(sessionID string) (registry.HibernationDetail, bool) {
	if r.Reg == nil {
		return registry.HibernationDetail{}, false
	}
	rec, ok := r.Reg.Get(sessionID)
	if !ok {
		return registry.HibernationDetail{}, false
	}
	return rec.Hibernation, true
}

// LastTurnEndOf reports sessionID's persisted last-turn-end instant.
func (r *RegistryRegistrar) LastTurnEndOf(sessionID string) (int64, bool) {
	if r.Reg == nil {
		return 0, false
	}
	rec, ok := r.Reg.Get(sessionID)
	if !ok {
		return 0, false
	}
	return rec.LastTurnEndMs, true
}

// KeepAliveWindowStore adapts *statedb.KeepAliveWindows to the session
// controller's ledger interface.
//
// The adapter exists so the controller does not depend on the store's row
// shape: the exclusion needs three facts and two questions — one by identity,
// one by instant — and stating exactly that keeps a harness able to supply them
// without a database.
type KeepAliveWindowStore struct{ Windows *statedb.KeepAliveWindows }

func (s KeepAliveWindowStore) Open(w sessioncontroller.KeepAliveWindowRecord) error {
	return s.Windows.Open(statedb.KeepAliveWindow{
		TurnID: w.TurnID, Workspace: w.Workspace, StartedAtMs: w.StartedAtMs,
	})
}

func (s KeepAliveWindowStore) Close(turnID string, endedAtMs int64) error {
	return s.Windows.Close(turnID, endedAtMs)
}

func (s KeepAliveWindowStore) Covers(workspace string, tsMs int64) (bool, error) {
	return s.Windows.Covers(workspace, tsMs)
}

func (s KeepAliveWindowStore) HasTurn(workspace, turnID string) (bool, error) {
	return s.Windows.HasTurn(workspace, turnID)
}

// PushForwarder is the late-bound bridge from the session controller's per-session sinks to
// the frontend.Server. The controller is constructed BEFORE the Server (the Server
// is what WireAgentShim returns, and the session controller is one of its command
// backends), so the session controller holds this forwarder and main sets its target once
// the Server exists. Every push before the target is set is loud-logged and
// dropped honestly (a dropped frame is recoverable via the frontend's connect
// snapshot / resync), never silently swallowed. It implements sessioncontroller.Pusher.
type PushForwarder struct {
	target atomic.Pointer[frontend.Server]
	// Logf reports a push that arrived before the target was set. Nil discards.
	Logf func(string, ...any)
}

// SetTarget binds the frontend.Server the forwarder pushes to. Called once,
// after WireAgentShim returns the Server.
func (f *PushForwarder) SetTarget(s *frontend.Server) { f.target.Store(s) }

func (f *PushForwarder) logMiss(kind string) {
	if f.Logf != nil {
		f.Logf("server: push forwarder has no frontend target yet; dropping %s frame (recoverable via connect snapshot/resync)", kind)
	}
}

func (f *PushForwarder) PushConversationDelta(c *frontendv1.ConversationDelta) {
	if s := f.target.Load(); s != nil {
		s.PushConversationDelta(c)
		return
	}
	f.logMiss("conversation-delta")
}

func (f *PushForwarder) PushAsyncBubbleDelta(d *frontendv1.AsyncBubbleDelta) {
	if s := f.target.Load(); s != nil {
		s.PushAsyncBubbleDelta(d)
		return
	}
	f.logMiss("async-bubble-delta")
}

func (f *PushForwarder) PushTypingDelta(t *frontendv1.TypingDelta) {
	if s := f.target.Load(); s != nil {
		s.PushTypingDelta(t)
		return
	}
	f.logMiss("typing-delta")
}

func (f *PushForwarder) PushTaskCatalog(c *frontendv1.TaskCatalog) {
	if s := f.target.Load(); s != nil {
		s.PushTaskCatalog(c)
		return
	}
	f.logMiss("task-catalog")
}

func (f *PushForwarder) PushWorkspaceState(w *frontendv1.WorkspaceState) {
	if s := f.target.Load(); s != nil {
		s.PushWorkspaceState(w)
		return
	}
	f.logMiss("workspace-state")
}

func (f *PushForwarder) PushSessionInitView(v *frontendv1.SessionInitView) {
	if s := f.target.Load(); s != nil {
		s.PushSessionInitView(v)
		return
	}
	f.logMiss("session-init-view")
}

func (f *PushForwarder) PushHeartbeatView(h *frontendv1.HeartbeatView) {
	if s := f.target.Load(); s != nil {
		s.PushHeartbeatView(h)
		return
	}
	f.logMiss("heartbeat-view")
}

func (f *PushForwarder) PushQueueView(q *frontendv1.QueueView) {
	if s := f.target.Load(); s != nil {
		s.PushQueueView(q)
		return
	}
	f.logMiss("queue-view")
}

func (f *PushForwarder) PushProgressView(v *frontendv1.ProgressView) {
	if s := f.target.Load(); s != nil {
		s.PushProgressView(v)
		return
	}
	f.logMiss("progress-view")
}

// signalPID is the production signal delivery: find the process and signal it.
// A pid that no longer exists reports os.ErrProcessDone, which the caller reads
// as "already stopped" rather than as a failure.
func signalPID(pid int, sig syscall.Signal) error {
	proc, err := os.FindProcess(pid)
	if err != nil {
		return err
	}
	return proc.Signal(sig)
}
