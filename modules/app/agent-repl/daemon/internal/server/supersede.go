// Single-writer enforcement for a resumed transcript.
//
// A transcript JSONL is an append-only file with exactly one legitimate
// author: the CLI that owns the conversation. Nothing structurally
// stopped a second session from resuming an id an older session was
// still live on, and the result is two CLIs appending to one file.
//
// That is not a cosmetic overlap. Each session's model reconciler
// (§2.12) tail-reads that shared transcript and treats whatever it finds
// as truth, so with two writers each reconciler reads the OTHER's model
// back, calls it drift, and adopts it — the two mirrors flip against
// each other on every tick, the topbar oscillates, and because a revive
// relaunches from the mirror the corrupted value eventually becomes the
// real CLI. The reconciler is not at fault; it is a mirror of a file
// that had stopped having a single meaning.
//
// So the conflict is removed at its source rather than tolerated
// downstream: a resume takes sole ownership of its transcript, and any
// older session still holding it is shut down. The newest resume is the
// one the user just asked for, so it is the one that wins.

package server

import (
	"claude-repld/internal/session"
)

// supersedeReason is the death reason a superseded session carries. It
// reads as a planned handover in the log and in `GET /sessions` rather
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

// supersedeResumeConflicts shuts down every non-terminal session already
// appending to the transcript OPTS resumes, so the newest resume owns it
// alone. A no-op for a fresh conversation, which has no transcript to
// contend for yet.
//
// A HIBERNATED holder is superseded too, even though it has no CLI at
// this instant: it spawns one the moment anyone acts on it, which would
// re-create exactly the conflict being removed here. Its history is not
// lost — the newer session resumes the same transcript and seeds its
// replay ring from it.
//
// Locking: the session map is copied under s.mu and every Info/Shutdown
// call is made after s.mu is released. Shutdown reaches back into the
// server (registrar -> isSwitching -> s.mu), so holding s.mu across it
// would deadlock on a non-reentrant mutex.
func (s *Server) supersedeResumeConflicts(opts CreateOpts) {
	if opts.Resume == "" {
		return
	}
	want := transcriptOwner(opts.ConfigDir, opts.CWD, opts.Resume)

	s.mu.Lock()
	held := make([]*session.Session, 0, len(s.sessions))
	for _, sess := range s.sessions {
		held = append(held, sess)
	}
	s.mu.Unlock()

	for _, sess := range held {
		info := sess.Info()
		// A session with no claude_session_id has not adopted a
		// transcript yet, so it cannot be contending for this one.
		if info.Terminal || info.ClaudeSessionID == "" {
			continue
		}
		if transcriptOwner(info.ConfigDir, info.CWD, info.ClaudeSessionID) != want {
			continue
		}
		s.logf("session %s: superseded by a newer resume of %s — shutting it down so %s keeps a single writer",
			sess.ID, opts.Resume, want)
		// Never swallowed: a supersede that fails to land leaves the very
		// double-writer this exists to prevent, and the surviving CLI will
		// go on corrupting the shared transcript's model signal.
		if err := sess.Shutdown(supersedeReason); err != nil {
			s.logf("session %s: supersede shutdown FAILED — two CLIs may now be appending to %s: %v",
				sess.ID, want, err)
		}
	}
}
