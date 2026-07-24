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
	"sync/atomic"
	"time"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/frontend"
	"claude-repld/internal/registry"
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

// ReattachProbe answers the spawn-vs-reattach question for a session socket
// (defaults to ReattachDecision). Injected so ShimSpawner is unit-testable
// without a real UDS listener.
type ReattachProbe func(ctx context.Context, socketPath string) (bool, error)

// ShimSpawnFunc execs a fresh UDS-mode shim for sessionID, told to listen on
// socketPath and to write its events to the shim-store. The exec itself stays
// in main (which owns node/shim paths and the store socket); ShimSpawner only
// decides WHEN to call it. It returns once the shim has been launched (the
// shimclient then dials socketPath and drives the handshake).
type ShimSpawnFunc func(sessionID string, opts CreateOpts, socketPath string) error

// ShimSpawner makes a session's UDS shim reachable at its socket: it reattaches
// to a live shim (the shim outlives a dead daemon, §4.4) or spawns a fresh one,
// resolving the spawn's CreateOpts from the session's registry record. It
// implements sessiondrv.Spawner.
type ShimSpawner struct {
	reg   *registry.Registry
	probe ReattachProbe
	spawn ShimSpawnFunc
	logf  func(string, ...any)
}

// NewShimSpawner builds a ShimSpawner. reg and spawn are required; a nil probe
// defaults to ReattachDecision, a nil logf discards.
func NewShimSpawner(reg *registry.Registry, probe ReattachProbe, spawn ShimSpawnFunc, logf func(string, ...any)) *ShimSpawner {
	if probe == nil {
		probe = ReattachDecision
	}
	if logf == nil {
		logf = func(string, ...any) {}
	}
	return &ShimSpawner{reg: reg, probe: probe, spawn: spawn, logf: logf}
}

// EnsureShim reattaches to a live shim at socketPath or spawns a fresh one.
// The reattach probe distinguishes "a live shim is listening" (reattach, no
// spawn) from "no listener" (spawn). A probe error (a socket answered by
// something that is not a healthy shim) is surfaced, never papered over into a
// silent respawn. A spawn with no registry record is a loud error: the driver
// has no CreateOpts to reconstruct the session from.
func (s *ShimSpawner) EnsureShim(ctx context.Context, sessionID, socketPath string) error {
	if s.reg == nil {
		return fmt.Errorf("server: ShimSpawner has no registry; cannot resolve session %s", sessionID)
	}
	if s.spawn == nil {
		return fmt.Errorf("server: ShimSpawner has no spawn func; cannot bring up session %s", sessionID)
	}
	reattach, err := s.probe(ctx, socketPath)
	if err != nil {
		return fmt.Errorf("server: reattach probe for session %s at %s: %w", sessionID, socketPath, err)
	}
	if reattach {
		s.logf("server: session %s: reattaching to live shim at %s (no spawn)", sessionID, socketPath)
		return nil
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
	s.logf("server: session %s: no live shim at %s — spawning fresh UDS shim (resume=%q)", sessionID, socketPath, rec.ClaudeSessionID)
	return s.spawn(sessionID, opts, socketPath)
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

func (f *PushForwarder) PushDegradedNotice(n *frontendv1.DegradedNotice) {
	if s := f.target.Load(); s != nil {
		s.PushDegradedNotice(n)
		return
	}
	f.logMiss("degraded-notice")
}

func (f *PushForwarder) PushWorkspaceState(w *frontendv1.WorkspaceState) {
	if s := f.target.Load(); s != nil {
		s.PushWorkspaceState(w)
		return
	}
	f.logMiss("workspace-state")
}
