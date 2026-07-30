// agentshim.go binds the merged Wave-1 agent-shim modules (shimclient, ssm,
// workspace/merge, frontend) into the daemon. This file holds the daemon-side
// GLUE: the spawn contract for the shim and the adapters that let the shimclient and SSM read/write the
// daemon's existing session registry (the SeqStore high-water mark and the
// session->workspace binding).
//
// It is deliberately IO-narrow and free of the daemon's HTTP surface so the
// bindings are unit-testable in isolation (design §14.2, §16). The frontend
// server construction/mount and the FrontendCommand handler live in
// frontendcmd.go; this file is the plumbing they and the spawn path depend on.
package server

import (
	"time"

	"claude-repld/internal/registry"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/ssm"
)

// ShimUDSArgv is the spawn contract for the shim (design §8, §4.4). It is the
// existing stdio ShimArgv PLUS `--daemon-socket <path>`: the socket the shim
// DIALS to reach the daemon.
//
// The shim used to be told where to LISTEN, and the daemon dialled it. That put
// the dialer ahead of the listener — for the few hundred ms before the node
// process called listen(), the daemon's dial failed with ENOENT — and it needed
// a probe to answer "is a shim alive?" by dialling a path and reading a frame.
// Inverting it removed the probe, the per-session socket files, and the race
// (design-shim-transport-inversion.md); a shim that outlives its daemon now
// reconnects to this same fixed path.
func ShimUDSArgv(node, script, sessionID string, forceFake bool, opts CreateOpts, daemonSocket string) []string {
	return append(ShimArgv(node, script, sessionID, forceFake, opts), "--daemon-socket", daemonSocket)
}

// RegistrySeqStore adapts the persistent session registry to
// shimclient.SeqStore, so a session's replay high-water mark survives daemon
// restarts (design §4.4, §9.1). The shimclient persists nothing itself; this
// is where last_seen_seq becomes durable.
//
// A seq write for an unregistered session is loud-logged, never silently
// dropped: it means the shimclient advanced a session the daemon never
// recorded, which would break replay on the next restart.
type RegistrySeqStore struct {
	reg  *registry.Registry
	logf func(string, ...any)
}

// NewRegistrySeqStore builds a SeqStore over reg. reg is required (a nil
// registry cannot persist a watermark).
func NewRegistrySeqStore(reg *registry.Registry, logf func(string, ...any)) *RegistrySeqStore {
	if logf == nil {
		logf = func(string, ...any) {}
	}
	return &RegistrySeqStore{reg: reg, logf: logf}
}

var _ shimclient.SeqStore = (*RegistrySeqStore)(nil)

// LastSeq returns the durable high-water mark for sessionID (0 if none).
//
// The mark belongs to the CONVERSATION, not to the daemon session id it is
// filed under. The store's seq space is keyed by the vendor uuid, and every
// restart mints a FRESH s_ id for the same conversation (the create path
// supersedes the old record). Reading only this record's own mark therefore
// returned 0 for a resumed conversation, and the shim re-subscribed from
// from_seq=0 — replaying its ENTIRE history.
//
// That is not merely wasteful: those thousands of replayed frames arrive on
// the same connection as control Acks, so the first prompt after a restart
// queued behind ~6000 events and blew its 10s ack timeout. The user saw
// "sending does nothing" while the daemon was busy re-reading the whole
// conversation.
//
// The registry's durable checkpoint index resolves the exact conversation
// identity (config_dir + cwd + vendor uuid). A resumed session continues where
// its predecessor stopped without borrowing a same-uuid mark from another
// account root or workspace.
func (s *RegistrySeqStore) LastSeq(sessionID string) uint64 {
	rec, ok := s.reg.Get(sessionID)
	if !ok {
		return 0
	}
	if rec.ClaudeSessionID == "" {
		// No conversation identity yet (a fresh session before system:init):
		// this record's own mark is the only thing that can apply.
		return rec.LastSeq
	}
	if cp, ok := s.reg.CheckpointForSession(sessionID); ok {
		return max(rec.LastSeq, cp.LastSeq)
	}
	return rec.LastSeq
}

// SetLastSeq records seq as the new high-water mark, write-through to disk.
func (s *RegistrySeqStore) SetLastSeq(sessionID string, seq uint64) {
	found, err := s.reg.Update(sessionID, func(rec *registry.Record) { rec.LastSeq = seq })
	if err != nil {
		s.logf("session %s: registry seq write (last_seq=%d) FAILED — replay may re-fetch or lose events after a restart: %v", sessionID, seq, err)
		return
	}
	if !found {
		s.logf("session %s: registry seq write (last_seq=%d) found no record — the session was never registered", sessionID, seq)
	}
}

// NewestClearOrCompactSeq returns the durable REPLAY FLOOR for sessionID: the
// store seq of the newest clear or compaction observed on its conversation, or
// 0 when neither has been seen.
//
// It resolves through the conversation checkpoint for exactly the reason
// LastSeq does, and the consequence of not doing so is sharper here: every
// restart mints a fresh s_ id for the same conversation, so a floor read only
// off this record would be 0 after every restart and the frontend would be
// served back the whole history the clear or compaction discarded.
func (s *RegistrySeqStore) NewestClearOrCompactSeq(sessionID string) uint64 {
	rec, ok := s.reg.Get(sessionID)
	if !ok {
		return 0
	}
	if rec.ClaudeSessionID == "" {
		// No conversation identity yet (a fresh session before system:init):
		// this record's own mark is the only thing that can apply.
		return rec.NewestClearOrCompactSeq
	}
	if cp, ok := s.reg.CheckpointForSession(sessionID); ok {
		return max(rec.NewestClearOrCompactSeq, cp.NewestClearOrCompactSeq)
	}
	return rec.NewestClearOrCompactSeq
}

// SetNewestClearOrCompactSeq records seq as the conversation's newest clear or
// compaction, write-through to disk.
//
// MONOTONIC: an older clear or compaction never lowers the floor. Events arrive
// in seq order on one connection, but a re-delivery after a reattach can
// present one the daemon already recorded, and lowering the floor there would
// replay history the newer one had already made irrelevant.
func (s *RegistrySeqStore) SetNewestClearOrCompactSeq(sessionID string, seq uint64) {
	found, err := s.reg.Update(sessionID, func(rec *registry.Record) {
		rec.NewestClearOrCompactSeq = max(rec.NewestClearOrCompactSeq, seq)
	})
	if err != nil {
		s.logf("session %s: registry replay-floor write (newest_clear_or_compact_seq=%d) FAILED — a resync after a restart may replay the history the clear or compaction discarded: %v", sessionID, seq, err)
		return
	}
	if !found {
		s.logf("session %s: registry replay-floor write (newest_clear_or_compact_seq=%d) found no record — the session was never registered", sessionID, seq)
	}
}

// RegistryModeStore adapts the session registry to shimclient.ModeStore: the
// session's stored PERMISSION POSTURE, which rides DaemonHello into the shim's
// bring-up gate (core.proto DaemonHello.permission_mode).
//
// It reports the record VERBATIM, empty included, and resolves nothing. The
// one resolution site is protocol.ResolvePermissionMode, consulted by the
// shimclient; a second default invented here is exactly the drift that let a
// daemon-created session run under "default" while its record said nothing.
type RegistryModeStore struct{ reg *registry.Registry }

// NewRegistryModeStore builds a ModeStore over reg (required).
func NewRegistryModeStore(reg *registry.Registry) *RegistryModeStore {
	return &RegistryModeStore{reg: reg}
}

var _ shimclient.ModeStore = (*RegistryModeStore)(nil)

// PermissionMode returns sessionID's recorded mode, or "" when the session has
// none or is unknown.
func (s *RegistryModeStore) PermissionMode(sessionID string) string {
	rec, ok := s.reg.Get(sessionID)
	if !ok {
		return ""
	}
	return rec.PermissionMode
}

// RegistryResolver adapts the session registry to ssm.Resolver: it answers
// "which workspace is this session bound to?" for the SSM's per-workspace
// state log (design §9.2). The workspace is the session's working directory
// (CWD) — the worktree/project dir Emacs keys a workspace by — so the binding
// falls straight out of the record the create path already persists, with no
// separate table.
//
// A live session's CWD is preferred over the unwired record's, but they are
// the same value in practice; consulting the registry alone keeps the resolver
// free of the live session map's lock.
type RegistryResolver struct{ reg *registry.Registry }

// NewRegistryResolver builds a resolver over reg (required).
func NewRegistryResolver(reg *registry.Registry) *RegistryResolver {
	return &RegistryResolver{reg: reg}
}

var _ ssm.Resolver = (*RegistryResolver)(nil)

// Workspace returns the workspace (CWD) bound to sessionID, and whether the
// session is known and carries a workspace. A known session with an empty CWD
// reports (", false): a workspace-less session has no per-workspace state to
// resolve, which the SSM surfaces as an explicit miss rather than binding
// state to the empty-string workspace.
//
// sessionID may be EITHER identity a session has:
//
//   - the daemon-minted s_<hex> id, which is the registry's own key; or
//   - the vendor session uuid (the CLI's, and its transcript filename).
//
// Both are needed because the SSM resolves the id carried on the EVENT, and
// events are keyed by the vendor uuid — the store files them under it, since
// the shim reads it off the SDK message and the sidecar derives it from
// `<uuid>.jsonl`. Resolving only the s_ id meant every lifecycle event the
// controller applied failed with "no workspace bound to session <uuid>", so no
// turn or task state ever reached a workspace.
//
// The uuid lookup is a scan because the registry is keyed by the other id, and
// several records can carry one uuid (a superseded resume, a rehydrated
// record). The newest by CreatedAt wins, matching SessionLocator; an
// unparseable timestamp sorts as the zero time so it never shadows a real one.
func (r *RegistryResolver) Workspace(sessionID string) (string, bool) {
	if sessionID == "" {
		return "", false
	}
	// The registry's own key first: an exact hit needs no scan.
	if rec, ok := r.reg.Get(sessionID); ok {
		if rec.CWD == "" {
			return "", false
		}
		return rec.CWD, true
	}

	var (
		bestCWD string
		bestAt  time.Time
		found   bool
	)
	for _, rec := range r.reg.All() {
		if rec.ClaudeSessionID != sessionID || rec.CWD == "" {
			continue
		}
		at, err := time.Parse(time.RFC3339, rec.CreatedAt)
		if err != nil {
			at = time.Time{}
		}
		if !found || at.After(bestAt) {
			bestCWD, bestAt, found = rec.CWD, at, true
		}
	}
	return bestCWD, found
}
