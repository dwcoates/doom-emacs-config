package ssm

import (
	"database/sql"
	"fmt"
)

// compactiongate.go — A COMPACTION IS NEVER THE SECOND ONE OF AN UNCHANGED
// CONVERSATION.
//
// A compaction reads the ENTIRE conversation and writes back a summary of it.
// Run twice with nothing said in between, the second run reads the first run's
// summary and summarizes THAT: it costs a full-history model call, it degrades
// the conversation a second time, and it can only ever produce a worse summary
// of the same material. There is no state of the world in which it is the right
// thing to do.
//
// IT IS REACHABLE TODAY, and not rarely. The daemon initiates compactions from
// two places that know nothing about each other: the pre-expiry WARM compaction
// (sessioncontroller/warmcompact.go), which fires on the cache clock, and the
// compact-first REVIVAL (sessioncontroller/revive.go), which fires when a user
// picks it off the hibernation gate. The ordinary sequence — warm-compact,
// hibernate, revive-with-compaction — runs both back to back with no prompt
// between them. The warm path's own anchor cannot see this: it makes the warm
// compaction exactly-once per CACHE WINDOW, which says nothing about a
// compaction some other path ran.
//
// SO THE FACT LIVES HERE, WHERE BOTH PATHS CAN SEE IT. The SSM already owns the
// durable per-workspace record, and this is one more thing that is true about a
// workspace: when its conversation was last compacted, and when it last got new
// material to compact.
//
// TWO TIMESTAMPS, AND THE COMPARISON IS THE WHOLE POLICY. A compaction is
// REDUNDANT when the conversation was compacted and nothing has been said to it
// since. Everything else about the two writers follows from what each timestamp
// has to mean for that comparison to be honest:
//
//   - COMPACTED_AT is written from the first-class ContextCompacted and from
//     nothing else. That event is the only report that a compaction actually
//     FINISHED — the compacting axis has four closing edges, three of which
//     (turn end, session rotation, the vendor's status ticker going quiet) also
//     fire for a compaction that DIED — and suppressing a later compaction on
//     one of those would be suppressing it on no evidence at all. The same event
//     is what a compact-first revival's own completion gate waits on, so the two
//     read one signal rather than two that can disagree.
//
//   - PROMPT_AT is written for every prompt EXCEPT the daemon's own idle
//     machinery. A keep-alive ping is the daemon refreshing a prompt cache, not
//     the user saying something, and counting it would re-open the gate on every
//     ping — which is to say the gate would never close at all, since pings are
//     exactly what a session does while it is idle enough to be compacted. The
//     warm compaction's own submission is idle machinery for the same reason.
//     The distinction is already a parameter of the accepted edge
//     (PromptAdmission), so this needs no new plumbing and cannot drift from it.
//
// THE ABSENT ANSWER IS "GO AHEAD". A workspace with no compaction on record is
// not redundant, and neither is one whose compaction never reported completion.
// The gate exists to stop a provable duplicate, so anything it cannot prove
// resolves in favor of the caller doing its ordinary job.

// CompactionGate is what the log knows about one workspace's compaction
// history: when a compaction last COMPLETED, and when the conversation last
// received material a compaction would have something to do with.
//
// A zero in either field means the event has never been observed for this
// workspace, which is UNKNOWN rather than "at the epoch" — see Redundant.
type CompactionGate struct {
	// CompactedAtMs is when the workspace's last ContextCompacted landed.
	CompactedAtMs int64
	// PromptAtMs is when the workspace last accepted a prompt that was not the
	// daemon's own idle machinery.
	PromptAtMs int64
}

// Redundant reports whether a compaction submitted right now would be the
// SECOND compaction of a conversation nothing has been added to.
//
// A workspace that has never compacted is never redundant, which is why the
// zero CompactedAtMs is tested rather than compared: an unknown compaction
// history read as "compacted at the epoch" would suppress nothing, but reading
// an unknown PROMPT history as "prompted at the epoch" would suppress the first
// legitimate compaction of every workspace whose prompts predate this gate.
func (g CompactionGate) Redundant() bool {
	return g.CompactedAtMs != 0 && g.PromptAtMs <= g.CompactedAtMs
}

// NoteCompactionCompleted records that workspace's conversation was compacted,
// from the first-class ContextCompacted and from nothing else.
//
// It is a MAX rather than an assignment. Events arrive from a replayed stream
// as readily as from a live one, and a re-delivered older ContextCompacted must
// not move the gate backwards behind a prompt that has genuinely been accepted
// since.
func (m *Manager) NoteCompactionCompleted(workspace string) error {
	if workspace == "" {
		return fmt.Errorf("ssm: NoteCompactionCompleted got an empty workspace")
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	at := m.nextAt()
	if err := noteCompactionGateEdge(m.db, workspace, "compacted_at", at); err != nil {
		return err
	}
	m.logf("ssm: compaction gate CLOSED ws=%s compacted_at=%d — the conversation has been compacted, and a daemon-initiated compaction is declined until something is said to it; compacting a summary of a summary costs a whole-history model call and can only produce a worse summary of the same material",
		workspace, at)
	return nil
}

// noteCompactionGatePromptLocked records that workspace accepted a prompt whose
// material a later compaction would have something to do with. Caller holds mu.
//
// A FAILURE IS LOGGED, NOT RETURNED, and this is the one place in this file
// where that is right. The caller is MarkPromptAccepted, whose every failure
// path retracts the accepted edge and fails the user's prompt; losing a real
// prompt because a bookkeeping row could not be written would be a far larger
// harm than the gate's own worst case, which is one declined compaction. The
// error is never swallowed — it is reported at warn, and the gate stays where
// it was, which is the direction that declines rather than the one that
// duplicates.
func (m *Manager) noteCompactionGatePromptLocked(workspace string) {
	at := m.nextAt()
	if err := noteCompactionGateEdge(m.db, workspace, "prompt_at", at); err != nil {
		m.warn("ssm: recording the compaction gate's prompt edge FAILED ws=%s prompt_at=%d: %v — the prompt itself is unaffected, but a daemon-initiated compaction may be declined as redundant until the next accepted prompt records one",
			workspace, at)
		return
	}
}

// noteCompactionGateEdge advances ONE of the gate's two columns to at, never
// backwards.
//
// The column is a caller-supplied identifier rather than a bind parameter
// because SQLite cannot bind one; the two call sites above are the only
// callers and both pass a literal.
func noteCompactionGateEdge(db rowExecer, workspace, column string, at int64) error {
	_, err := db.Exec(fmt.Sprintf(`INSERT INTO compaction_gate(workspace, %s) VALUES (?, ?)
		ON CONFLICT(workspace) DO UPDATE SET %s = MAX(%s, excluded.%s)`,
		column, column, column, column), workspace, at)
	if err != nil {
		return fmt.Errorf("ssm: record compaction gate %s for workspace %q: %w", column, workspace, err)
	}
	return nil
}

// CompactionGateOf reads workspace's gate. A workspace with no row has never
// compacted and never accepted a prompt under this gate, which is the zero
// value and is never redundant.
func (m *Manager) CompactionGateOf(workspace string) (CompactionGate, error) {
	if workspace == "" {
		return CompactionGate{}, fmt.Errorf("ssm: CompactionGateOf got an empty workspace")
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	return readCompactionGate(m.db, workspace)
}

// readCompactionGate is CompactionGateOf's body, without the lock.
func readCompactionGate(db *sql.DB, workspace string) (CompactionGate, error) {
	var g CompactionGate
	err := db.QueryRow(
		`SELECT compacted_at, prompt_at FROM compaction_gate WHERE workspace = ?`, workspace,
	).Scan(&g.CompactedAtMs, &g.PromptAtMs)
	if err == sql.ErrNoRows {
		return CompactionGate{}, nil
	}
	if err != nil {
		return CompactionGate{}, fmt.Errorf("ssm: read compaction gate for workspace %q: %w", workspace, err)
	}
	return g, nil
}
