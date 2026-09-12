// held.go decides what to do with a spool nobody has claimed yet.
//
// A spool materializes before — sometimes long before — the transcript line
// naming the call that spawned it is read. Until that line arrives the spool's
// owner is unknown, and tailing it would mean either inventing an owner or
// reading the spool path's runtime id as an identity. So it is HELD: discovered,
// re-checked every rescan, and not tailed.
//
// AN AGED UNOWNED SPOOL IS NEVER DROPPED. The launch line naming an owner is
// written when the task starts, so a hold normally clears within a rescan tick;
// one that outlives the bounded wait means the mapping is genuinely missing —
// which is a REASON TO INGEST THE BYTES AS RESIDUE, not a reason to discard
// them. After the window the spool is tailed as KindResidueSpool: its bytes land
// whole as StoreUnparsed naming the spool as their source, with a warning, and
// it KEEPS BEING TAILED so nothing appended later is lost either.
package main

import (
	"fmt"
	"time"

	"agentrepl/shim-claude-sidecar/internal/discover"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

type workspaceAttribution struct {
	dir string
	id  string
}

// catchupTally accumulates, during a startup catch-up rescan, the pre-existing
// items of one stale class so the pass states ONE summary instead of one
// warning per item. The clock it keeps is each item's own last-write mtime,
// never our read time, so the oldest age it reports is a fact about the file
// rather than about when we got round to it.
type catchupTally struct {
	count    int
	oldestMs int64 // 0 = nothing accumulated yet
}

func (c *catchupTally) add(itemMs int64) {
	c.count++
	if c.oldestMs == 0 || itemMs < c.oldestMs {
		c.oldestMs = itemMs
	}
}

func (c *catchupTally) reset() { *c = catchupTally{} }

// isBacklog reports whether a file whose relevant activity is at itemMs was
// already present before the sidecar started producing, and so belongs to the
// startup catch-up rather than to steady state. A zero process start (no
// boundary set yet) means nothing is catch-up: every item is stated per item,
// exactly as before this policy existed.
func (s *sidecar) isBacklog(itemMs int64) bool {
	return s.processStartMs != 0 && itemMs < s.processStartMs
}

// flushCatchupSummaries states, at the end of a rescan pass, ONE INFORMATIONAL
// summary per class the pass caught up on, naming the count and the oldest
// item's age. A class with nothing accumulated states nothing. Every tally is
// reset, so the summary is a per-pass edge rather than a running total.
//
// THE SUMMARIES ARE INFO, NOT WARN: each is an account of PRE-EXISTING backlog a
// restart re-derived — transcripts without workspace attribution, unclaimed
// spools ingested as residue, and records the store already holds under a
// different book — not a fault the owner must act on, so a strict harvest must
// not trip on them. The newly-arising per-item conditions these summarize away
// keep their own WARN level.
func (s *sidecar) flushCatchupSummaries(nowMs int64) {
	if s.catchupWorkspaces.count > 0 {
		age := time.Duration(nowMs-s.catchupWorkspaces.oldestMs) * time.Millisecond
		s.log.With(logging.Context{
			Operation: "catchup-summary", Level: "info",
			Reason: "workspace_unattributed", Repeat: logging.Repeat(s.catchupWorkspaces.count),
		}).Log("startup catch-up held %d pre-existing transcript(s) without workspace attribution; the oldest was last written %s ago — these predate this sidecar and are summarized here, not stated one by one",
			s.catchupWorkspaces.count, age)
		s.catchupWorkspaces.reset()
	}
	if s.catchupSpools.count > 0 {
		age := time.Duration(nowMs-s.catchupSpools.oldestMs) * time.Millisecond
		s.log.With(logging.Context{
			Operation: "catchup-summary", Level: "info",
			Reason: "spool_unclaimed", Repeat: logging.Repeat(s.catchupSpools.count),
		}).Log("startup catch-up ingested %d pre-existing unclaimed spool(s) as residue; the oldest was last written %s ago — these predate this sidecar and are summarized here, not stated one by one",
			s.catchupSpools.count, age)
		s.catchupSpools.reset()
	}
	if s.catchupBookConflicts.count > 0 {
		age := time.Duration(nowMs-s.catchupBookConflicts.oldestMs) * time.Millisecond
		s.log.With(logging.Context{
			Operation: "catchup-summary", Level: "info",
			Reason: "legacy_book_conflict", Repeat: logging.Repeat(s.catchupBookConflicts.count),
		}).Log("startup catch-up skipped %d pre-existing record(s) the store already holds under a different book; the oldest was last written %s ago — re-ingesting already-stored content is idempotent, so these are kept as-is and summarized here, not stated one by one",
			s.catchupBookConflicts.count, age)
		s.catchupBookConflicts.reset()
	}
}

// UnownedSpoolWindow is how long a spool may sit unclaimed before its bytes are
// ingested as residue rather than waited on any longer. It is the DEFAULT:
// --unowned-spool-window replaces it, so the residue path can be exercised in
// milliseconds instead of waited out.
const UnownedSpoolWindow = 60 * time.Second

// heldSpools remembers when each unclaimed spool was first seen.
type heldSpools struct {
	firstSeen map[string]time.Time // by resolved path
	demoted   map[string]bool      // paths already ingested as residue
	window    time.Duration
	log       *logging.Bound
}

func newHeldSpools(window time.Duration, log *logging.Bound) *heldSpools {
	if window == 0 {
		// Zero is how the caller says "unset", exactly as it is for the LOST
		// windows; the default is filled here so there is one place that knows it.
		window = UnownedSpoolWindow
	}
	return &heldSpools{
		firstSeen: map[string]time.Time{},
		demoted:   map[string]bool{},
		window:    window,
		log:       log,
	}
}

// hold records an unclaimed spool and reports whether its wait has expired.
func (h *heldSpools) hold(path string, now time.Time) (expired bool) {
	first, seen := h.firstSeen[path]
	if !seen {
		h.firstSeen[path] = now
		h.log.With(logging.Context{Operation: "hold-spool", Path: path}).
			Log("spool held: no spawning call has claimed it yet, so it is re-checked every rescan and not tailed")
		return false
	}
	if now.Sub(first) < h.window {
		h.log.With(logging.Context{Operation: "hold-spool", Path: path}).
			LogVerbose("spool still held after %s", now.Sub(first))
		return false
	}
	return true
}

// release stops holding a spool whose owner arrived.
func (h *heldSpools) release(path string) {
	if _, held := h.firstSeen[path]; !held {
		return
	}
	delete(h.firstSeen, path)
	h.log.With(logging.Context{Operation: "hold-spool", Path: path}).Log("spool released: its spawning call was observed")
}

// demote records that a spool's bytes are being ingested as residue, and
// reports whether that is the first time it is being said.
func (h *heldSpools) demote(path string) bool {
	if h.demoted[path] {
		return false
	}
	h.demoted[path] = true
	return true
}

// resolveTarget decides whether a discovered target may be tailed, and as what.
//
// A CONFIG-ROOT PATH NAMES ITS OWN SESSION — the transcript IS that session's
// record — so it answers immediately. A spool does not, and is held until its
// spawning call is observed or the bounded wait expires.
func (s *sidecar) resolveTarget(target discover.Target, now time.Time) (discover.Target, bool) {
	if target.SessionID != "" {
		return s.resolveTranscriptWorkspace(target)
	}
	if target.Kind == tail.KindResidueSpool {
		// Its task-id prefix already failed classification, so no owner would
		// change what happens to it: the bytes go to residue either way.
		return target, true
	}
	if obs, ok := s.owners.resolve(target); ok {
		s.held.release(target.Path)
		// AN a* SPOOL IS AN AGENT'S OWN TRANSCRIPT, so its book is that agent —
		// which IS the spawning call under the cross-plane minting rule. Every
		// other spool carries a RUN rather than an agent, and its frames are
		// attributed to the book the spawn happened in.
		if target.Kind == tail.KindAgentTranscript {
			target.AgentID = obs.activityID
		} else {
			target.AgentID = obs.agentID
		}
		target.WorkspaceDir = obs.workspaceDir
		target.WorkspaceID = obs.workspaceID
		target.ClaudeSessionID = obs.claudeSessionID
		if target.WorkspaceDir == "" || target.WorkspaceID == "" || target.ClaudeSessionID == "" {
			s.log.With(logging.Context{
				Operation: "resolve-spool-workspace", Path: target.Path, TaskID: target.TaskID, Level: "error",
			}).Log("spool owner carries no complete workspace/session attribution; the spool is not watched")
			return discover.Target{}, false
		}
		return target, true
	}
	if !s.held.hold(target.Path, now) {
		return discover.Target{}, false
	}
	// The wait expired. The bytes are ingested as residue rather than waited on
	// forever, and the file keeps being tailed.
	if s.held.demote(target.Path) {
		mtimeMs := fileActivityMs(target.Path, now.UnixMilli())
		if s.isBacklog(mtimeMs) {
			// A spool that already existed unclaimed before this sidecar started
			// is backlog: its spawning session is long gone and it will never be
			// claimed. A restart re-derives hundreds of these at once, so it is
			// accumulated and summarized by flushCatchupSummaries rather than
			// warned per file. The demotion itself still happens; only its record
			// is leveled to debug.
			s.catchupSpools.add(mtimeMs)
			s.log.With(logging.Context{Operation: "hold-expired", Path: target.Path, TaskID: target.TaskID, Reason: "spool_unclaimed", Level: "debug"}).
				LogVerbose("spool unclaimed after %s during startup catch-up: its bytes are ingested as unparsed residue and it keeps being tailed; it is summarized rather than stated on its own", s.held.window)
		} else {
			// A spool that appeared while the sidecar was already running and then
			// aged out unclaimed is a newly-arising condition, stated per file.
			s.log.With(logging.Context{Operation: "hold-expired", Path: target.Path, TaskID: target.TaskID, Level: "warn"}).
				Log("spool unclaimed after %s: its bytes are ingested as unparsed residue naming the spool as their source, and it keeps being tailed", s.held.window)
		}
	}
	target.Kind = tail.KindResidueSpool
	target.Raw = true
	return target, true
}

func (s *sidecar) resolveTranscriptWorkspace(target discover.Target) (discover.Target, bool) {
	key := target.ConfigRoot + "\x00" + target.ProjectKey + "\x00" + target.SessionID
	workspace, ok := s.workspaceBySession[key]
	if !ok {
		dir, id, err := discover.ResolveWorkspace(target)
		if err != nil {
			detail := err.Error()
			ctx := logging.Context{
				Operation: "resolve-transcript-workspace", Path: target.Path,
				ClaudeSessionID: target.SessionID, Level: "warn",
			}
			if s.workspaceFailures[key] == detail {
				// The same failure was already stated or counted; a rescan
				// re-checks the transcript every pass, so restating it here is
				// exactly the per-pass flood this leveling exists to prevent.
				ctx.Level = "debug"
				s.log.With(ctx).LogVerbose("transcript still held without workspace attribution: %v", err)
				return discover.Target{}, false
			}
			firstFailure := s.workspaceFailures[key] == ""
			s.workspaceFailures[key] = detail
			mtimeMs := fileActivityMs(target.Path, s.now().UnixMilli())
			if firstFailure && s.isBacklog(mtimeMs) {
				// A pre-existing transcript whose workspace cannot be resolved is
				// backlog the restart is catching up on. A restart re-derives
				// hundreds at once, so it is accumulated and summarized rather
				// than warned per file; only a transcript whose session appears
				// while the sidecar runs steady-state warns per item.
				s.catchupWorkspaces.add(mtimeMs)
				ctx.Level = "debug"
				s.log.With(ctx).LogVerbose("transcript held without workspace attribution during startup catch-up; it is summarized rather than stated on its own: %v", err)
				return discover.Target{}, false
			}
			s.log.With(ctx).Log("transcript held: workspace attribution is required before any bytes are read: %v", err)
			return discover.Target{}, false
		}
		workspace = workspaceAttribution{dir: dir, id: id}
		s.workspaceBySession[key] = workspace
		delete(s.workspaceFailures, key)
		s.log.With(logging.Context{
			Operation: "resolve-transcript-workspace", Path: target.Path,
			WorkspaceDir: dir, WorkspaceID: id, ClaudeSessionID: target.SessionID,
		}).LogVerbose("workspace attribution resolved from the main transcript")
	}
	if workspace.dir == "" || workspace.id == "" {
		panic(fmt.Sprintf("sidecar: cached workspace attribution for %q is incomplete", key))
	}
	target.WorkspaceDir = workspace.dir
	target.WorkspaceID = workspace.id
	target.ClaudeSessionID = target.SessionID
	return target, true
}
