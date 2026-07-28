// drivers.go is the live wiring for the per-session shim-driver
// (internal/sessiondrv): the daemon-side implementations of the driver's
// injected seams — SessionLocator (workspace -> live session id), ShimSpawner
// (reattach-first UDS bring-up), and the late-bound push forwarder that lets
// the driver push frontend frames to a frontend.Server that is constructed
// AFTER the driver (WireAgentShim returns the Server, whose target is then set
// on the forwarder). Kept in the server package (not sessiondrv) because these
// read the daemon's registry and own the reattach/spawn plumbing; the driver
// stays IO-narrow and testable behind the interfaces (design §4.4, §9.1, §16).
package server

import (
	"context"
	"fmt"
	"net"
	"sync"
	"sync/atomic"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/frontend"
	"claude-repld/internal/registry"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/shimlisten"
)

// SessionLocator resolves a workspace to the newest non-terminal session bound
// to it (the record whose CWD is the workspace). It implements
// sessiondrv.SessionLocator over the persistent registry: the driver asks
// "which live session drives this workspace?" on the first frontend command
// for it, and the answer is the freshest conversation rooted at that worktree.
type SessionLocator struct {
	// Reg is the persistent session registry (required).
	Reg *registry.Registry
}

// Locate returns the id of the newest non-terminal session whose CWD is
// workspace, and whether one exists. Newest is by CreatedAt (RFC3339); a record
// whose timestamp cannot be parsed is treated as the zero time so it never
// shadows a record with a real one. A terminal record never matches — its
// conversation has ended, so it cannot be driven.
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
// StopShim can SIGTERM it on hibernation. It implements sessiondrv.Spawner.
type ShimSpawner struct {
	reg       *registry.Registry
	connected ConnectedFunc
	spawn     ShimSpawnFunc
	logf      func(string, ...any)

	mu    sync.Mutex
	stops map[string]func() error // session id -> stop the shim WE spawned
}

// NewShimSpawner builds a ShimSpawner. reg and spawn are required; a nil
// connected reports nothing connected, a nil logf discards.
func NewShimSpawner(reg *registry.Registry, connected ConnectedFunc, spawn ShimSpawnFunc, logf func(string, ...any)) *ShimSpawner {
	if logf == nil {
		logf = func(string, ...any) {}
	}
	return &ShimSpawner{reg: reg, connected: connected, spawn: spawn, logf: logf, stops: map[string]func() error{}}
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
func (s *ShimSpawner) EnsureShim(ctx context.Context, sessionID string) error {
	if s.reg == nil {
		return fmt.Errorf("server: ShimSpawner has no registry; cannot resolve session %s", sessionID)
	}
	if s.spawn == nil {
		return fmt.Errorf("server: ShimSpawner has no spawn func; cannot bring up session %s", sessionID)
	}
	if s.connected != nil && s.connected(sessionID) {
		s.logf("server: session %s: shim already connected (no spawn)", sessionID)
		return nil
	}
	held, err := sessionlock.Held(sessionID)
	if err != nil {
		return fmt.Errorf("server: session %s: cannot determine whether a shim holds its lock: %w", sessionID, err)
	}
	if held {
		return fmt.Errorf("server: session %s: a shim holds the session lock but has not connected — refusing to spawn a duplicate; the holder is alive and may be mid-turn", sessionID)
	}
	rec, ok := s.reg.Get(sessionID)
	if !ok {
		return fmt.Errorf("server: session %s has no registry record; cannot spawn its UDS shim", sessionID)
	}
	opts := CreateOpts{
		CWD:            rec.CWD,
		Model:          rec.Model,
		PermissionMode: rec.PermissionMode,
		ConfigDir:      rec.ConfigDir,
		Resume:         rec.ClaudeSessionID,
	}
	s.logf("server: session %s: no live shim — spawning fresh UDS shim (resume=%q)", sessionID, rec.ClaudeSessionID)
	stop, err := s.spawn(sessionID, opts)
	if err != nil {
		return err
	}
	if stop != nil {
		s.mu.Lock()
		s.stops[sessionID] = stop
		s.mu.Unlock()
	}
	return nil
}

// StopShim SIGTERMs the shim this spawner launched for sessionID (hibernation's
// clean stop). A session whose shim the daemon did not spawn — a reattached one
// that outlived a prior daemon — is a no-op: there is no child to signal.
func (s *ShimSpawner) StopShim(sessionID string) error {
	s.mu.Lock()
	stop, ok := s.stops[sessionID]
	if ok {
		delete(s.stops, sessionID)
	}
	s.mu.Unlock()
	if !ok {
		s.logf("server: session %s: StopShim no-op (no daemon-spawned shim; reattached or already stopped)", sessionID)
		return nil
	}
	s.logf("server: session %s: stopping shim (SIGTERM)", sessionID)
	return stop()
}

// RegistryRegistrar binds the driver's claude_session_id write-through
// (sessiondrv.SessionRegistrar) to the persistent registry: when a session's
// SessionStarted reports its CLI session uuid, it lands on the durable record
// so --resume and rehydration survive a daemon restart. It replaces the old
// L2 session hub's registrar, which the deleted stdio Run loop used to drive.
type RegistryRegistrar struct {
	Reg  *registry.Registry
	Logf func(string, ...any)
	// PushView re-pushes a session's SessionView after a record write, so a
	// CONNECTED frontend sees the change rather than waiting for whatever
	// unrelated event next happens to push one. Late-bound by main (the Server
	// does not exist when the registrar is built), which is why the registrar
	// is held by pointer. Nil-safe: the connect snapshot still carries the
	// record, so a nil pusher costs freshness, never correctness.
	PushView func(sessionID string)
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
func (r *RegistryRegistrar) ClaudeSessionIDChanged(sessionID, claudeSessionID string) {
	if r.Reg == nil {
		return
	}
	found, err := r.Reg.Update(sessionID, func(rec *registry.Record) { rec.ClaudeSessionID = claudeSessionID })
	if err != nil && r.Logf != nil {
		r.Logf("server: session %s: registry claude_session_id write FAILED — resume may break after a restart: %v", sessionID, err)
		return
	}
	if !found && r.Logf != nil {
		r.Logf("server: session %s: claude_session_id write found no record (never registered)", sessionID)
	}
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
func (r *RegistryRegistrar) AdoptVendorSessionID(sessionID, claudeSessionID string) (bool, string) {
	if r.Reg == nil || claudeSessionID == "" {
		return false, ""
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
		return false, previous
	}
	if !found {
		if r.Logf != nil {
			r.Logf("server: session %s: vendor session adoption (uuid=%s) found no record (never registered)", sessionID, claudeSessionID)
		}
		return false, previous
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
	return rotated, previous
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

// PushForwarder is the late-bound bridge from the driver's per-session sinks to
// the frontend.Server. The driver is constructed BEFORE the Server (the Server
// is what WireAgentShim returns, and the driver is one of its command
// backends), so the driver holds this forwarder and main sets its target once
// the Server exists. Every push before the target is set is loud-logged and
// dropped honestly (a dropped frame is recoverable via the frontend's connect
// snapshot / resync), never silently swallowed. It implements sessiondrv.Pusher.
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
