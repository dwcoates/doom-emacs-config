// agentshim.go binds the merged Wave-1 agent-shim modules (shimclient, ssm,
// workspace/merge, frontend) into the daemon. This file holds the daemon-side
// GLUE: the spawn contract for the UDS-mode shim, the reattach-first spawn
// decision, and the adapters that let the shimclient and SSM read/write the
// daemon's existing session registry (the SeqStore high-water mark and the
// session->workspace binding).
//
// It is deliberately IO-narrow and free of the daemon's HTTP surface so the
// bindings are unit-testable in isolation (design §14.2, §16). The frontend
// server construction/mount and the FrontendCommand handler live in
// frontendcmd.go; this file is the plumbing they and the spawn path depend on.
package server

import (
	"context"
	"fmt"
	"net"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	"agentrepl/wire"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/known/anypb"

	"claude-repld/internal/registry"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/ssm"
)

// SessionSocketPath is the UDS path a session's claude-shim listens on and the
// daemon's shimclient dials. It is the single source of truth shared by the
// spawn contract (ShimUDSArgv, which tells the shim WHERE to listen) and the
// shimclient (which dials the same path), so the two can never disagree.
// Delegates to shimclient.DefaultSocketPath so the convention lives in one
// place: ~/.cache/agent-repl/sock/session-<id>.sock (design §3).
func SessionSocketPath(sessionID string) string {
	return shimclient.DefaultSocketPath(sessionID)
}

// ShimUDSArgv is the spawn contract for the UDS-mode claude-shim (design §8,
// §4.4). It is the existing stdio ShimArgv PLUS `--uds-socket <path>`.
//
// WHY the extra flag, and why a separate function rather than folding it into
// ShimArgv:
//
//   - In UDS mode the shim replaces its stdio event stream with a UDS LISTENER
//     at `path` (session-<id>.sock). The daemon CONNECTS to that listener,
//     which is precisely what makes daemon-restart REATTACH possible: the shim
//     outlives a dead daemon, keeps its SDK turn running, and the restarted
//     daemon re-dials the same live socket and replays from last_seen_seq
//     (§4.4) rather than respawning with --resume. A shim told where to listen
//     is the whole basis of that survival contract.
//   - The shim-side rewiring that HONORS --uds-socket (src/main.ts,
//     src/session.ts: stdio->UDS, stdin-EOF no longer ends the turn) is a
//     SEPARATE task. Until it lands, the still-live stdio path (ShimArgv +
//     shim.Proc) must keep working, so this flag is added by a DISTINCT
//     function the UDS spawn path calls — appending it to the one stdio
//     ShimArgv the current daemon uses would feed an unknown flag to the
//     stdio shim and to the e2e harness. When the shim speaks UDS, main.go's
//     spawn closure switches from ShimArgv to ShimUDSArgv.
func ShimUDSArgv(node, script, sessionID string, forceFake bool, opts CreateOpts, socketPath string) []string {
	return append(ShimArgv(node, script, sessionID, forceFake, opts), "--uds-socket", socketPath)
}

// reattachProbeTimeout bounds the reattach handshake probe. A var so tests can
// shorten it; short in production because a live shim's listener answers with
// its ShimHello immediately (the listener speaks first, §5.2 handshake).
var reattachProbeTimeout = 2 * time.Second

// ReattachDecision reports whether the daemon should REATTACH to an existing
// live shim at socketPath (true) or SPAWN a fresh one (false), implementing
// the reattach-first policy (design §4.4): if session-<id>.sock exists AND
// answers the handshake, connect instead of spawning; otherwise the shim is
// gone and a --resume respawn is the path.
//
// The probe dials the socket and reads the first frame the listener sends: a
// live shim opens with a ShimHello (the listener speaks first). The probe
// connection is then closed — the real shimclient re-dials and drives the full
// handshake; this only answers the spawn-vs-reattach question. A dial failure
// (no socket, or a stale socket file with nothing listening) means SPAWN, not
// an error: it is the expected fresh-session / dead-shim case. A dial that
// succeeds but yields a non-ShimHello or a read error IS surfaced as an error
// (the socket is answered by something that is not a healthy shim — an anomaly
// the caller must see, never paper over into a silent respawn).
func ReattachDecision(ctx context.Context, socketPath string) (reattach bool, err error) {
	dialCtx, cancel := context.WithTimeout(ctx, reattachProbeTimeout)
	defer cancel()
	var d net.Dialer
	conn, derr := d.DialContext(dialCtx, "unix", socketPath)
	if derr != nil {
		// No live listener: expected fresh-session / dead-shim case -> SPAWN.
		return false, nil
	}
	defer conn.Close()
	if dl, ok := dialCtx.Deadline(); ok {
		_ = conn.SetReadDeadline(dl)
	}
	payload, rerr := wire.ReadFrame(conn)
	if rerr != nil {
		return false, fmt.Errorf("reattach probe %s: read hello frame: %w", socketPath, rerr)
	}
	var env anypb.Any
	if uerr := proto.Unmarshal(payload, &env); uerr != nil {
		return false, fmt.Errorf("reattach probe %s: unmarshal hello envelope: %w", socketPath, uerr)
	}
	msg, uerr := env.UnmarshalNew()
	if uerr != nil {
		return false, fmt.Errorf("reattach probe %s: resolve hello type %q: %w", socketPath, env.GetTypeUrl(), uerr)
	}
	if _, ok := msg.(*corev1.ShimHello); !ok {
		return false, fmt.Errorf("reattach probe %s: first frame was %T, expected ShimHello", socketPath, msg)
	}
	return true, nil
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
func (s *RegistrySeqStore) LastSeq(sessionID string) uint64 {
	rec, ok := s.reg.Get(sessionID)
	if !ok {
		return 0
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

// RegistryResolver adapts the session registry to ssm.Resolver: it answers
// "which workspace is this session bound to?" for the SSM's per-workspace
// state log (design §9.2). The workspace is the session's working directory
// (CWD) — the worktree/project dir Emacs keys a workspace by — so the binding
// falls straight out of the record the create path already persists, with no
// separate table.
//
// A live session's CWD is preferred over the dormant record's, but they are
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
// driver applied failed with "no workspace bound to session <uuid>", so no
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
