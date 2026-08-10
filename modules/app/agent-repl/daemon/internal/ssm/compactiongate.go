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
// THE CUT INSTANTS AND THE PROMPT INSTANT, AND THE COMPARISON IS THE WHOLE
// POLICY. A compaction is REDUNDANT when the conversation was CUT — compacted
// or cleared — and nothing has been said to it since. Everything else about the
// writers follows from what each timestamp has to mean for that comparison to
// be honest:
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
//   - CLEARED_AT is written from the first-class ContextCleared and from
//     nothing else, for the identical reason: the clearing axis's other closing
//     edges (the rotation, the watchdog's expiry) also close for a `/clear` the
//     vendor never carried out. It is a SEPARATE column from compacted_at
//     because a decline has to name the cut it was taken from, and reporting a
//     cleared conversation as "already compacted" is a false account of the
//     user's own workspace. Nothing that reads the gate has to distinguish
//     them; everything that REPORTS it does.
//
// THE GATE IS ALSO WHAT HOLDS AN AUTOMATIC SLEEP OFF. A cut conversation with
// nothing said to it since is not hibernated by the two clock-driven causes
// either (sessioncontroller/hibernation.go). A cut is the daemon's own most
// recent action on that conversation, and putting the workspace to sleep on top
// of it stands the session down behind a revival gate whose compacting choices
// this very gate already declines — a sleep taken over material nobody has
// added to, offering a revival that has nothing to do.
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
	// ClearedAtMs is when the workspace's last COMPLETED `/clear` landed.
	ClearedAtMs int64
	// PromptAtMs is when the workspace last accepted a prompt that was not the
	// daemon's own idle machinery.
	PromptAtMs int64
}

// CutAtMs is when the conversation was last CUT — compacted or cleared,
// whichever happened later — and 0 when neither has ever been observed.
//
// The two cuts are one fact for every predicate that asks "is there anything
// here worth acting on": a compaction leaves a summary, a clear leaves nothing,
// and in both cases the conversation now holds only what a cut left behind. The
// two are kept apart in the record — and in every log line taken from it —
// because a decline that names the wrong one misreports the user's own
// workspace.
func (g CompactionGate) CutAtMs() int64 {
	if g.ClearedAtMs > g.CompactedAtMs {
		return g.ClearedAtMs
	}
	return g.CompactedAtMs
}

// Redundant reports whether the conversation has been CUT — compacted or
// cleared — with nothing said to it since.
//
// A CLEAR COUNTS FOR THE COMPACTION'S REASON, ONLY MORE SO. Compacting a
// summary reads the whole history to produce a worse summary of the same
// material; compacting a conversation that was DISCARDED reads whatever the
// clear left — nothing, or a fresh session's opening lines — to summarize
// material nobody has added to yet. Neither is work worth a whole-history model
// call, and both re-open the instant the user says something.
//
// A workspace that has never been cut is never redundant, which is why the zero
// cut instant is tested rather than compared: an unknown cut history read as
// "cut at the epoch" would suppress nothing, but reading an unknown PROMPT
// history as "prompted at the epoch" would suppress the first legitimate
// compaction of every workspace whose prompts predate this gate.
func (g CompactionGate) Redundant() bool {
	cut := g.CutAtMs()
	return cut != 0 && g.PromptAtMs <= cut
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

// NoteConversationCleared records that workspace's conversation was DISCARDED,
// from the completed `/clear` and from nothing else.
//
// IT FOLLOWS COMPACTED_AT'S DISCIPLINE EXACTLY, because the same trap is here.
// The clearing axis has closing edges that also fire for a clear that DIED —
// the watchdog's expiry closes an axis whose clear the vendor may have refused
// outright, and a session rotation is a clear TAKING EFFECT but is appended by
// a daemon-local edge that no first-class report has confirmed. Only the
// first-class ContextCleared is the vendor reporting that the conversation is
// gone, so it is the only writer here, exactly as ContextCompacted is the only
// writer of compacted_at.
//
// It is a MAX for NoteCompactionCompleted's reason: a replayed older
// ContextCleared must not move the gate backwards behind a prompt that has
// genuinely been accepted since.
func (m *Manager) NoteConversationCleared(workspace string) error {
	if workspace == "" {
		return fmt.Errorf("ssm: NoteConversationCleared got an empty workspace")
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	at := m.nextAt()
	if err := noteCompactionGateEdge(m.db, workspace, "cleared_at", at); err != nil {
		return err
	}
	m.logf("ssm: compaction gate CLOSED ws=%s cleared_at=%d — the conversation has been discarded, so there is nothing left for a daemon-initiated compaction to summarize and nothing worth holding a session awake for until something is said to it",
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
// because SQLite cannot bind one; the call sites above are the only callers
// and each passes a literal.
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
		`SELECT compacted_at, cleared_at, prompt_at FROM compaction_gate WHERE workspace = ?`, workspace,
	).Scan(&g.CompactedAtMs, &g.ClearedAtMs, &g.PromptAtMs)
	if err == sql.ErrNoRows {
		return CompactionGate{}, nil
	}
	if err != nil {
		return CompactionGate{}, fmt.Errorf("ssm: read compaction gate for workspace %q: %w", workspace, err)
	}
	return g, nil
}
