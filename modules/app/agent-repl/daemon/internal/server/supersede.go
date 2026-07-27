// Single-live-session and single-writer enforcement for session creation.
//
// Driver routing is keyed by cwd, so two non-terminal records for one cwd make
// the newest create appear live in the pushed roster while Ensure can retain
// the predecessor's driver. A transcript JSONL also has exactly one legitimate
// author: the CLI that owns the conversation.
//
// That is not a cosmetic overlap: each session's model reconciler once
// tail-read that shared transcript and treated whatever it found as
// truth, so two writers made the two mirrors flip against each other.
// The conflict is removed at its source rather than tolerated
// downstream: every create takes sole ownership of its workspace, a resume
// additionally takes sole ownership of its transcript, and older holders are
// stood down. The newest create is the one the user just requested.
//
// After the agent-shim consumption cutover there is no live-session map:
// the persistent registry is the source of truth for who holds which
// workspace/transcript, and the per-session driver owns the live shim. Supersede
// therefore works entirely off the registry — it marks every non-terminal
// record contending for either resource terminal and stops its shim —
// rather than reaching into a hub that no longer exists.

package server

import (
	"claude-repld/internal/errclass"
	"claude-repld/internal/registry"
	"claude-repld/internal/session"
)

// supersedeReason is the death reason a superseded record carries. It
// reads as a planned handover in the log and in GET /sessions rather
// than as a conversation that died on its own. The literal lives in
// errclass beside its classification so a producer cannot write a reason
// the classifier does not know.
const supersedeReason = errclass.DeathReasonSuperseded

// transcriptOwner is the identity of the file a session appends to. The
// resume id ALONE is not that identity: the same claude session uuid
// legitimately exists under two accounts (one uuid lives under both
// ~/.claude and ~/.claude-chesscom on this machine), and those are two
// different files with two different writers. Only a shared resolved
// path is a genuine conflict.
func transcriptOwner(configDir, cwd, claudeSessionID string) string {
	return session.TranscriptPath(session.ClaudeConfigDir(configDir), cwd, claudeSessionID)
}

// supersedeCreateConflicts stands down every non-terminal registry record
// that either occupies OPTS.CWD or already appends to the transcript OPTS
// resumes. The newest create owns both resources alone.
//
// A record with no claude_session_id has adopted no transcript, so it
// cannot be contending for this one. A record on a DIFFERENT resolved path
// (same uuid, different account root) is a distinct file, but it still
// conflicts when it occupies the same cwd because command routing and live
// drivers are workspace-keyed.
// For every genuine conflict the record is marked terminal (so its id stops
// resolving and the driver never brings it up again) and its shim is stopped
// best-effort — a shim that was never brought up is a no-op Hibernate.
func (s *Server) supersedeCreateConflicts(opts CreateOpts) {
	wantTranscript := ""
	if opts.Resume != "" {
		wantTranscript = transcriptOwner(opts.ConfigDir, opts.CWD, opts.Resume)
	}
	for _, rec := range s.registry.All() {
		if rec.Terminal {
			continue
		}
		sameWorkspace := opts.CWD != "" && rec.CWD == opts.CWD
		sameTranscript := wantTranscript != "" &&
			rec.ClaudeSessionID != "" &&
			transcriptOwner(rec.ConfigDir, rec.CWD, rec.ClaudeSessionID) == wantTranscript
		if !sameWorkspace && !sameTranscript {
			continue
		}
		s.logf("session %s: superseded by newer create {cwd=%s resume=%s same_workspace=%v same_transcript=%v} — enforcing one live session per workspace and one writer per transcript",
			rec.SessionID, opts.CWD, opts.Resume, sameWorkspace, sameTranscript)
		// Never swallowed: a supersede that fails to land leaves the very
		// double-writer this exists to prevent.
		s.updateRegistry(rec.SessionID, "supersede terminal", func(r *registry.Record) {
			r.Terminal = true
			r.DeathReason = supersedeReason
		})
		// Stop the OLD session's shim if the driver had brought one up —
		// session-scoped, so superseding a stale record can never SIGTERM a
		// newer session that already owns the same cwd. A workspace with no
		// live driver, or one driven by a different session, is an expected
		// no-op, not a failure.
		if err := s.driver.HibernateSession(rec.CWD, rec.SessionID); err != nil {
			s.logf("session %s: supersede shim stop (ws %s): %v (expected when no live shim, or another session drives it)", rec.SessionID, rec.CWD, err)
		}
		// Connected frontends own a pushed SessionView roster. Without this
		// transition they keep the predecessor as non-terminal and retry its
		// already-accepted deletion on every orphan-reaper tick.
		s.pushSessionView(rec.SessionID)
		s.logf("session %s: supersede terminal SessionView pushed {ws=%s resume=%s same_workspace=%v same_transcript=%v}",
			rec.SessionID, rec.CWD, opts.Resume, sameWorkspace, sameTranscript)
	}
}
