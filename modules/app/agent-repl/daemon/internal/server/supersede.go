// Single-session enforcement for a workspace, and single-writer
// enforcement for a resumed transcript.
//
// Controller routing is keyed by cwd, so two non-terminal records for one cwd make
// the newest create appear live in the pushed roster while Ensure can retain
// the predecessor's controller. A transcript JSONL also has exactly one legitimate
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
// The workspace itself has the same shape of conflict WITHOUT a resume:
// the session controller runs one session per cwd and the locator resolves a cwd to
// its newest non-terminal record, so the moment a create mints a newer
// record, every older record on that cwd stops being reachable — but its
// shim keeps running and its registry record keeps reading as live.
// Frontends then correlate the cwd to the WRONG (stale) session and the
// leaked shim lingers until a daemon restart. So a create stands down
// every older non-terminal record on its cwd, transcript or not. A
// same-transcript contender always shares the cwd (the transcript path
// embeds it), so the cwd sweep subsumes the transcript sweep whenever the
// create carries a cwd; the transcript predicate still runs for the
// degenerate workspace-less resume.
//
// After the agent-shim consumption cutover there is no live-session map:
// the persistent registry is the source of truth for who holds which
// workspace/transcript, and the per-session controller owns the live shim. Supersede
// therefore works entirely off the registry — it marks every non-terminal
// record contending for the same workspace or transcript terminal, stops
// its shim, and pushes the terminal SessionView — rather than reaching
// into a hub that no longer exists.

package server

import (
	"errors"
	"fmt"

	"claude-repld/internal/errclass"
	"claude-repld/internal/registry"
	"claude-repld/internal/session"
	"claude-repld/internal/sessioncontroller"
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
// the create displaces: records on the same cwd (the workspace takes one
// live session, and this create is the newest claim on it) and records
// already appending to the transcript OPTS resumes (single writer). A
// no-op for a workspace-less fresh conversation, which contends for
// nothing yet.
//
// For every conflict the record is marked terminal (so its id stops
// resolving and the session controller never brings it up again), its shim is stopped
// best-effort — a shim that was never brought up is a no-op Hibernate —
// and its terminal SessionView is pushed. The push is load-bearing:
// without it every connected frontend keeps the superseded session listed
// live in its roster, correlates the cwd to the stale id on the next
// create, and binds its workspace to a session that no longer exists
// (the observed every-restore mis-bind).
//
// ONE STOP FAILURE IS NOT LIKE THE OTHERS. A stop that could not be completed
// is logged and the sweep continues, because a record whose shim was never
// brought up, or whose teardown failed for a bookkeeping reason, still has to
// be stood down. But ErrShimSurvivedStop says something else: a LIVE shim owns
// that conversation and refused to die. Continuing there produces exactly the
// double writer this file exists to prevent, so it is returned and the create
// that asked for it does not proceed. Every surviving stop is collected rather
// than the first one only, so the caller's error names each shim still holding
// a transcript.
func (s *Server) supersedeCreateConflicts(opts CreateOpts) error {
	var survived error
	var wantTranscript string
	if opts.Resume != "" {
		wantTranscript = transcriptOwner(opts.ConfigDir, opts.CWD, opts.Resume)
	}
	for _, rec := range s.registry.All() {
		if rec.Terminal {
			continue
		}
		workspaceConflict := opts.CWD != "" && rec.CWD == opts.CWD
		transcriptConflict := wantTranscript != "" && rec.ClaudeSessionID != "" &&
			transcriptOwner(rec.ConfigDir, rec.CWD, rec.ClaudeSessionID) == wantTranscript
		if !workspaceConflict && !transcriptConflict {
			continue
		}
		if transcriptConflict {
			s.logf("session %s: superseded by a newer resume of %s — standing it down so %s keeps a single writer",
				rec.SessionID, opts.Resume, wantTranscript)
		} else {
			s.logf("session %s: superseded by a newer session for workspace %s — standing it down so the workspace keeps a single live session",
				rec.SessionID, opts.CWD)
		}
		// Never swallowed: a supersede that fails to land leaves the very
		// double-writer this exists to prevent.
		s.updateRegistry(rec.SessionID, "supersede terminal", func(r *registry.Record) {
			r.Terminal = true
			r.DeathReason = supersedeReason
		})
		// Stop the OLD session's shim if the session controller had brought one up —
		// session-scoped, so superseding a stale record can never SIGTERM a
		// newer session that already owns the same cwd.
		if err := s.controller.HibernateSession(rec.CWD, rec.SessionID, sessioncontroller.StopCauseSessionSuperseded()); err != nil {
			s.logf("session %s: supersede exact shim stop FAILED (ws %s): %v", rec.SessionID, rec.CWD, err)
			if errors.Is(err, ErrShimSurvivedStop) {
				s.logf("session %s: supersede shim SURVIVED its stop (ws %s) — the superseding create is refused rather than started beside a live writer of the same transcript",
					rec.SessionID, rec.CWD)
				survived = errors.Join(survived, fmt.Errorf("superseded session %s (ws %s): %w", rec.SessionID, rec.CWD, err))
			}
		}
		// Deliver the stand-down to every connected frontend so their rosters
		// mark this session terminal NOW, not at their next full resync.
		s.pushSessionView(rec.SessionID)
		s.logf("session %s: supersede terminal SessionView pushed {ws=%s resume=%s same_workspace=%v same_transcript=%v}",
			rec.SessionID, rec.CWD, opts.Resume, workspaceConflict, transcriptConflict)
	}
	return survived
}
