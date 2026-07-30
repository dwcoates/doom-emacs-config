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
	"claude-repld/internal/session"
	"claude-repld/internal/sessioncontroller"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/shimlisten"
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
	return c.Net, c.Hello, nil
}

// ConnectedFunc reports whether a shim for sessionID has already dialled in.
// Backed by the shim listener; injected so ShimSpawner is unit-testable
// without a real socket.
type ConnectedFunc func(sessionID string) bool

// ShimSpawnFunc execs a fresh shim for sessionID, told which daemon socket to
// dial and where to write its events. The exec itself stays in main (which
// owns node/shim/store paths); ShimSpawner only decides WHEN to call it. It
// returns a stop func that terminates the launched shim cleanly (SIGTERM) —
// the daemon uses it to hibernate the child — or nil when there is nothing to
// stop (a shim that outlived a prior daemon and dialled back in).
type ShimSpawnFunc func(sessionID string, opts CreateOpts) (stop func() error, err error)

// ShimSpawner keeps exactly one shim alive per session: it leaves an existing
// one alone (whether it is connected or merely holding its session lock, §4.4)
// or spawns a fresh one, resolving the spawn's CreateOpts from the session's
// registry record. It tracks the stop func of every shim IT spawned so
// StopShim can SIGTERM it on hibernation. It implements sessioncontroller.Spawner.
type ShimSpawner struct {
	reg       *registry.Registry
	connected ConnectedFunc
	spawn     ShimSpawnFunc
	logf      func(string, ...any)
	// signal delivers a signal to a pid. Injected so the surviving-shim stop
	// is unit-testable without spawning a real process to kill.
	signal func(pid int, sig syscall.Signal) error
	// awaitStopped blocks until sessionID's shim is really gone. Injected so a
	// unit harness, which has no processes and no locks, is not made to wait
	// for a condition it can never observe.
	awaitStopped func(sessionID string) error

	mu    sync.Mutex
	stops map[string]func() error // session id -> stop the shim WE spawned
}

// NewShimSpawner builds a ShimSpawner. reg and spawn are required; a nil
// connected reports nothing connected, a nil logf discards.
func NewShimSpawner(reg *registry.Registry, connected ConnectedFunc, spawn ShimSpawnFunc, logf func(string, ...any)) *ShimSpawner {
	if logf == nil {
		logf = func(string, ...any) {}
	}
	return &ShimSpawner{
		reg: reg, connected: connected, spawn: spawn, logf: logf,
		signal:       signalPID,
		awaitStopped: awaitShimStopped,
		stops:        map[string]func() error{},
	}
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
// VALIDATE BEFORE RESUME. A record can name a conversation that does not
// exist — a uuid adopted before the vendor wrote anything, a transcript
// deleted underneath us, a config dir that moved. Handing that to the CLI is
// an immediate exit 1, and the shim's death during bring-up used to leave the
// workspace in `starting` with nothing to explain it. So the pointer is
// checked against the disk here, and a missing transcript starts the session
// FRESH with the pointer dropped and the drop reported to the caller, which
// owes the user a feed note about the history that is not coming back.
func (s *ShimSpawner) EnsureShim(ctx context.Context, sessionID string) (sessioncontroller.SpawnResult, error) {
	var res sessioncontroller.SpawnResult
	if s.reg == nil {
		return res, fmt.Errorf("server: ShimSpawner has no registry; cannot resolve session %s", sessionID)
	}
	if s.spawn == nil {
		return res, fmt.Errorf("server: ShimSpawner has no spawn func; cannot bring up session %s", sessionID)
	}
	if s.connected != nil && s.connected(sessionID) {
		s.logf("server: session %s: shim already connected (no spawn)", sessionID)
		return res, nil
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
	resume := rec.ClaudeSessionID
	if resume != "" {
		if path, exists := session.TranscriptExists(rec.ConfigDir, rec.CWD, resume); !exists {
			s.logf("server: session %s: STALE RESUME POINTER — the record names vendor conversation %s but no transcript exists at %s; dropping the pointer and starting FRESH rather than handing the CLI a --resume it will exit 1 on",
				sessionID, resume, path)
			dropped, dropErr := s.DropResume(sessionID)
			if dropErr != nil {
				return res, dropErr
			}
			res.StaleResumeDropped = dropped
			resume = ""
		}
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
		Resume:         resume,
	}
	res.Resumed = resume
	s.logf("server: session %s: no live shim — spawning fresh UDS shim (resume=%q)", sessionID, resume)
	stop, err := s.spawn(sessionID, opts)
	if err != nil {
		return res, err
	}
	if stop != nil {
		s.mu.Lock()
		s.stops[sessionID] = stop
		s.mu.Unlock()
	}
	return res, nil
}

// DropResume clears the session's vendor conversation pointer so the next spawn
// starts fresh, reporting what it dropped.
//
// A write failure is SURFACED rather than swallowed: leaving a pointer standing
// that the daemon has already decided not to honour would make the next
// bring-up repeat the same failed resume, which is the loop this exists to end.
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
		return "", fmt.Errorf("server: session %s: dropping the stale resume pointer FAILED — the next bring-up would repeat the same failed resume: %w", sessionID, err)
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
// No handle and no pid is a genuine no-op — nothing is running that we know of
// — and it is logged rather than treated as a failure.
func (s *ShimSpawner) StopShim(sessionID string, hintPID int32) error {
	s.mu.Lock()
	stop, ok := s.stops[sessionID]
	if ok {
		delete(s.stops, sessionID)
	}
	s.mu.Unlock()
	switch {
	case ok:
		s.logf("server: session %s: stopping shim (SIGTERM via our own process handle)", sessionID)
		if err := stop(); err != nil {
			return err
		}
	case hintPID > 0:
		s.logf("server: session %s: no daemon-spawned shim; stopping the SURVIVING shim by its announced pid %d (SIGTERM)", sessionID, hintPID)
		if err := s.signal(int(hintPID), syscall.SIGTERM); err != nil {
			if errors.Is(err, os.ErrProcessDone) {
				s.logf("server: session %s: shim pid %d had already exited", sessionID, hintPID)
				return nil
			}
			return fmt.Errorf("server: session %s: stopping surviving shim pid %d: %w", sessionID, hintPID, err)
		}
	default:
		s.logf("server: session %s: StopShim no-op (no daemon-spawned shim and no announced pid; already stopped, or never seen)", sessionID)
		return nil
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
	if s.awaitStopped == nil {
		return nil
	}
	return s.awaitStopped(sessionID)
}

// stopGrace bounds how long StopShim waits for a signalled shim to actually
// exit. It is a FAILURE bound, not a delay: the wait ends the instant the
// session lock is free, and this only decides how long to wait before calling
// a shim that ignored SIGTERM what it is.
const stopGrace = 10 * time.Second

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
// the shim is really gone. An unreadable lock ends the wait with that error
// rather than a guess: "I could not tell" must never be read as "it is free".
func awaitShimStopped(sessionID string) error {
	deadline := time.Now().Add(stopGrace)
	for {
		held, err := sessionlock.Held(sessionID)
		if err != nil {
			return fmt.Errorf("server: session %s: waiting for its shim to exit: %w", sessionID, err)
		}
		if !held {
			return nil
		}
		if time.Now().After(deadline) {
			return fmt.Errorf("server: session %s: its shim still holds the session lock %s after SIGTERM; it is not stopping", sessionID, stopGrace)
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
