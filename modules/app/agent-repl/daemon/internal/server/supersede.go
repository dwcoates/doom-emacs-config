// Single-writer enforcement for a resumed transcript.
//
// A transcript JSONL is an append-only file with exactly one legitimate
// author: the CLI that owns the conversation. Nothing structurally
// stopped a second session from resuming an id an older session was
// still live on, and the result is two CLIs appending to one file.
//
// That is not a cosmetic overlap: each session's model reconciler once
// tail-read that shared transcript and treated whatever it found as
// truth, so two writers made the two mirrors flip against each other.
// The conflict is removed at its source rather than tolerated
// downstream: a resume takes sole ownership of its transcript, and any
// older session still holding it is stood down. The newest resume is the
// one the user just asked for, so it is the one that wins.
//
// After the agent-shim consumption cutover there is no live-session map:
// the persistent registry is the source of truth for who holds which
// transcript, and the per-session driver owns the live shim. Supersede
// therefore works entirely off the registry — it marks every non-terminal
// record contending for the same transcript terminal and stops its shim —
// rather than reaching into a hub that no longer exists.

package server

import (
	"claude-repld/internal/registry"
	"claude-repld/internal/session"
)

// supersedeReason is the death reason a superseded record carries. It
// reads as a planned handover in the log and in GET /sessions rather
// than as a conversation that died on its own.
const supersedeReason = "superseded"

// transcriptOwner is the identity of the file a session appends to. The
// resume id ALONE is not that identity: the same claude session uuid
// legitimately exists under two accounts (one uuid lives under both
// ~/.claude and ~/.claude-chesscom on this machine), and those are two
// different files with two different writers. Only a shared resolved
// path is a genuine conflict.
func transcriptOwner(configDir, cwd, claudeSessionID string) string {
	return session.TranscriptPath(session.ClaudeConfigDir(configDir), cwd, claudeSessionID)
}

// supersedeResumeConflicts stands down every non-terminal registry record
// already appending to the transcript OPTS resumes, so the newest resume
// owns it alone. A no-op for a fresh conversation, which has no transcript
// to contend for yet.
//
// A record with no claude_session_id has adopted no transcript, so it
// cannot be contending for this one. A record on a DIFFERENT resolved path
// (same uuid, different account root) is a distinct file and is left alone.
// For every genuine conflict the record is marked terminal (so its id stops
// resolving and the driver never brings it up again) and its shim is stopped
// best-effort — a shim that was never brought up is a no-op Hibernate.
func (s *Server) supersedeResumeConflicts(opts CreateOpts) {
	if opts.Resume == "" {
		return
	}
	want := transcriptOwner(opts.ConfigDir, opts.CWD, opts.Resume)

	for _, rec := range s.registry.All() {
		if rec.Terminal || rec.ClaudeSessionID == "" {
			continue
		}
		if transcriptOwner(rec.ConfigDir, rec.CWD, rec.ClaudeSessionID) != want {
			continue
		}
		s.logf("session %s: superseded by a newer resume of %s — standing it down so %s keeps a single writer",
			rec.SessionID, opts.Resume, want)
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
	}
}
