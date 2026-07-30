package server

import (
	"sort"
	"time"

	"claude-repld/internal/errclass"
	"claude-repld/internal/registry"
	"claude-repld/internal/session"
)

// ConversationResolver answers "which conversation belongs to this workspace?"
// from the daemon's own records, so no frontend has to remember.
//
// THE FRONTEND USED TO ANSWER THIS. Every create carried a vendor uuid the
// caller had persisted from a previous run, which made each frontend a second
// authority on a question the daemon can answer exactly — and a weaker one,
// because a frontend holds one remembered value while the daemon holds the
// registry, the checkpoints, and the transcripts on disk. When a frontend's
// copy went missing, five workspaces started fresh conversations on top of
// fully intact transcripts and nothing noticed, because "I have no pointer"
// and "start me fresh" were indistinguishable on the wire.
//
// The resolution rule is: the NEWEST conversation at this (config_dir, cwd)
// whose transcript actually exists on disk, excluding conversations the user
// deliberately deleted.
type ConversationResolver struct {
	// Reg is the persistent session registry (required).
	Reg *registry.Registry
	// Logf receives the resolution account. Optional.
	Logf func(string, ...any)
	// transcriptExists is session.TranscriptExists, injected for tests.
	transcriptExists func(configDir, cwd, claudeSessionID string) (string, bool)
}

func (r *ConversationResolver) logf(format string, args ...any) {
	if r != nil && r.Logf != nil {
		r.Logf(format, args...)
	}
}

func (r *ConversationResolver) exists(configDir, cwd, csid string) (string, bool) {
	if r.transcriptExists != nil {
		return r.transcriptExists(configDir, cwd, csid)
	}
	return session.TranscriptExists(configDir, cwd, csid)
}

// ObservedClaudeSessionID reports the vendor uuid currently on sessionID's
// record. FOR OBSERVABILITY ONLY — it rides the create ack so a client can
// attribute its logs from its first line, and it is a point-in-time reading
// rather than a durable fact: the uuid rotates on a /clear or a compact, and
// the pushed SessionView is what tracks that.
//
// This can answer at all only because adoption is now EAGER. Under the old
// hold the record deliberately carried "" until a turn ran, which is exactly
// the disagreement that made every client log nack.
func (r *ConversationResolver) ObservedClaudeSessionID(sessionID string) string {
	if r == nil || r.Reg == nil || sessionID == "" {
		return ""
	}
	rec, ok := r.Reg.Get(sessionID)
	if !ok {
		return ""
	}
	return rec.ClaudeSessionID
}

// resumeCandidate is one conversation the resolver is considering, carrying
// the ordering key and the provenance the log line reports.
type resumeCandidate struct {
	claudeSessionID string
	createdAt       time.Time
	source          string
}

// ResolveResume returns the vendor conversation uuid a create for this
// (configDir, cwd) should resume, and whether one was found. A false return is
// the honest "this workspace has no conversation yet" — the caller starts
// fresh, which is correct for a brand-new workspace and for one whose every
// transcript has been deleted out from under it.
//
// THE ON-DISK CHECK IS THE LOAD-BEARING PART, and it is why this can be
// resolved late rather than guarded early. The CLI hard-exits when asked to
// --resume a transcript that does not exist, so the danger has always been
// pointing at a conversation the vendor never actually wrote. The daemon used
// to defend against that by REFUSING TO WRITE DOWN a uuid until a turn proved
// the vendor had written the file (the "adopt late" hold). Checking the disk
// here is strictly stronger: it consults the same authority, but at the moment
// the answer is used rather than at the moment it was first guessed, so a
// stale or never-written uuid is skipped instead of poisoning the record.
func (r *ConversationResolver) ResolveResume(configDir, cwd string) (string, bool) {
	if r == nil || r.Reg == nil || cwd == "" {
		return "", false
	}

	candidates := r.candidates(configDir, cwd)
	if len(candidates) == 0 {
		r.logf("resume-resolve: NO conversation on record for config_dir=%q cwd=%q — this create starts fresh", configDir, cwd)
		return "", false
	}

	// Newest first. A candidate with an unparseable timestamp sorts oldest
	// rather than shadowing one with a real time.
	sort.SliceStable(candidates, func(i, j int) bool {
		return candidates[i].createdAt.After(candidates[j].createdAt)
	})

	for _, c := range candidates {
		path, ok := r.exists(configDir, cwd, c.claudeSessionID)
		if !ok {
			r.logf("resume-resolve: SKIPPING uuid=%s (%s) for cwd=%q — no transcript at %s; the vendor never wrote this conversation, so --resume would hard-exit",
				c.claudeSessionID, c.source, cwd, path)
			continue
		}
		r.logf("resume-resolve: cwd=%q config_dir=%q RESUMES uuid=%s (%s, created_at=%s) — transcript present at %s",
			cwd, configDir, c.claudeSessionID, c.source, c.createdAt.Format(time.RFC3339), path)
		return c.claudeSessionID, true
	}

	r.logf("resume-resolve: %d conversation(s) on record for cwd=%q but NO transcript survives on disk — this create starts fresh", len(candidates), cwd)
	return "", false
}

// candidates gathers every conversation the registry knows for this location,
// from both the session records and the conversation checkpoints. The
// checkpoints matter on their own: a checkpoint outlives the session record
// that produced it, so a conversation whose record has been pruned is still
// resumable.
//
// ONE CONVERSATION IS JUDGED BY ITS NEWEST RECORD, never by whichever record
// happened to be visited first. A workspace accumulates many records per uuid
// — every restore supersedes the last — so the same conversation is typically
// named by a whole run of them, and they do not agree: an old `delete session`
// tombstone sits beside newer `superseded` records for the uuid the user has
// been talking to all along.
//
// This is a fix, not a refinement. The first version excluded on the uuid the
// moment it saw ANY deleted record, and Reg.All() iterates sorted by session
// id rather than by time, so a stale tombstone could shadow a live record on
// an alphabetical accident. It did: two workspaces had their real
// conversations excluded that way, fell through to unrelated candidates, and
// one landed on a transcript another live process was writing — which the
// shim then replayed into, saw a turn end for a turn it never saw begin, and
// died with a protocol error.
func (r *ConversationResolver) candidates(configDir, cwd string) []resumeCandidate {
	// newest[uuid] is the most recent record naming that conversation, which
	// is the only one whose death reason gets a vote.
	newest := make(map[string]registry.Record)
	newestAt := make(map[string]time.Time)
	var order []string

	for _, rec := range r.Reg.All() {
		if rec.CWD != cwd || rec.ConfigDir != configDir || rec.ClaudeSessionID == "" {
			continue
		}
		at, err := time.Parse(time.RFC3339, rec.CreatedAt)
		if err != nil {
			// Unparseable sorts oldest so it never shadows a record with a
			// real time — the same rule SessionLocator applies.
			at = time.Time{}
		}
		prior, ok := newest[rec.ClaudeSessionID]
		if !ok {
			order = append(order, rec.ClaudeSessionID)
		}
		// Ties break on session id so the choice is deterministic rather than
		// dependent on map or slice order.
		if !ok || at.After(newestAt[rec.ClaudeSessionID]) ||
			(at.Equal(newestAt[rec.ClaudeSessionID]) && rec.SessionID > prior.SessionID) {
			newest[rec.ClaudeSessionID] = rec
			newestAt[rec.ClaudeSessionID] = at
		}
	}

	seen := make(map[string]bool)
	var out []resumeCandidate
	for _, uuid := range order {
		rec := newest[uuid]
		seen[uuid] = true
		// A conversation whose LATEST record is a user delete stays deleted.
		// Superseded and shim-died records are fair game: those ended for
		// mechanical reasons (a newer session claimed the workspace, a shim
		// crashed) and their conversation is exactly what a restore wants
		// back. A uuid deleted once and later revived is resumable again,
		// because its newest record is the revival.
		if rec.DeathReason == errclass.DeathReasonDeleted {
			r.logf("resume-resolve: EXCLUDING uuid=%s for cwd=%q — its newest record is a user delete (session=%s created_at=%s)",
				uuid, cwd, rec.SessionID, rec.CreatedAt)
			continue
		}
		out = append(out, resumeCandidate{
			claudeSessionID: uuid,
			createdAt:       newestAt[uuid],
			source:          "session record " + rec.SessionID,
		})
	}

	for _, cp := range r.Reg.AllCheckpoints() {
		if cp.CWD != cwd || cp.ConfigDir != configDir || cp.ClaudeSessionID == "" {
			continue
		}
		if seen[cp.ClaudeSessionID] {
			continue
		}
		seen[cp.ClaudeSessionID] = true
		// A checkpoint carries no creation time. It sorts oldest, which is the
		// conservative placement: a checkpoint without a record is a survivor
		// of pruning, and should never outrank a conversation the registry
		// still remembers in full.
		out = append(out, resumeCandidate{
			claudeSessionID: cp.ClaudeSessionID,
			source:          "conversation checkpoint",
		})
	}

	return out
}
