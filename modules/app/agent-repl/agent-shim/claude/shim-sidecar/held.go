// held.go decides whether a spool is READ at all, and what to do with one
// nobody has claimed yet.
//
// A spool materializes before — sometimes long before — the transcript line
// naming the call that spawned it is read. Until that line arrives the spool's
// owner is unknown, and tailing it would mean either inventing an owner or
// reading the spool path's runtime id as an identity. So it is HELD: discovered,
// re-checked every rescan, and not tailed.
//
// ONLY WHAT IS RENDERED IS READ (owner rule, 2026-09-23: "anything not needed
// for rendering isn't needed at all"). A spool is read for the rows a claimed
// run renders from it — a shell run's `bash:<run>` deltas and terminal, a
// claimed workflow run's terminal — and for nothing else:
//
//   - AN UNCLAIMED SPOOL IS NEVER READ. Past the hold window its lapse is stated
//     once, and it stays held: nothing renders a spool no call claimed, and its
//     bytes used to be read whole, tailed forever and dropped at the write path
//     as residue — large, growing test logs costing a read and a cursor write
//     per poll for rows no one ever stored. It is still re-checked every
//     rescan, so a launch read later (a restart catching up on a backlog)
//     CLAIMS it and it is read from its start like any other claimed spool.
//   - AN a* SPOOL THAT IS A SYMLINK IS NEVER READ THROUGH THE SPOOL. It points at
//     the subagent's own `subagents/agent-<id>.jsonl`, which discovery ingests as
//     that agent's transcript; reading the link as well copies the transcript
//     twice.
//   - A SPOOL WHOSE TASK ID HAS NO a/b/w PREFIX IS NEVER READ. No conversion can
//     be selected for it, so nothing could render it; discovery states the
//     unrecognized prefix loudly, once.
package main

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
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
// spools left unread, and records the store already holds under a
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
			Reason: reasonSpoolUnclaimed, Repeat: logging.Repeat(s.catchupSpools.count),
		}).Log("startup catch-up left %d pre-existing unclaimed spool(s) unread; the oldest was last written %s ago — these predate this sidecar and are summarized here, not stated one by one",
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

// UnownedSpoolWindow is how long a spool may sit unclaimed before its lapse is
// stated. It is the DEFAULT: --unowned-spool-window replaces it, so the lapse
// can be exercised in milliseconds instead of waited out.
//
// THE WINDOW DECIDES WHAT IS SAID, NEVER WHAT IS READ. A spool is read only once
// a spawning call claims it, before the window lapses or after.
const UnownedSpoolWindow = 60 * time.Second

// The `reason` a spool decision is stated under. THE REASON IS THE DECISION'S
// IDENTITY, so a reader filters for one kind of skip without reading prose.
const (
	// reasonSpoolUnclaimed — no spawning call has claimed the spool within the
	// hold window, so nothing renders it and it is not read.
	reasonSpoolUnclaimed = "spool_unclaimed"
	// reasonTranscriptSymlink — an a* spool that is a link to the subagent's own
	// transcript, which is ingested through its config-root path.
	reasonTranscriptSymlink = "transcript_symlink"
	// reasonUnrecognizedPrefix — the task id carries no a/b/w kind prefix, so no
	// conversion, and no renderer, could be selected for it.
	reasonUnrecognizedPrefix = "unrecognized_prefix"
	// reasonSpoolVanished — the spool was gone before it could be examined.
	reasonSpoolVanished = "spool_vanished"
)

// heldSpools remembers when each unclaimed spool was first seen, and which
// spool decisions have already been stated.
//
// A DECISION IS A CONDITION, NOT AN EVENT. Every rescan re-resolves every
// unwatched spool, so each map below is what keeps a decision to one record per
// path rather than one per pass.
type heldSpools struct {
	firstSeen map[string]time.Time // by resolved path
	lapsed    map[string]bool      // paths whose hold window lapse was stated
	skipped   map[string]bool      // paths whose never-read decision was stated
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
		lapsed:    map[string]bool{},
		skipped:   map[string]bool{},
		window:    window,
		log:       log,
	}
}

// hold records an unclaimed spool and reports whether its hold window lapsed
// on THIS call — true exactly once per path, so the lapse is stated once.
func (h *heldSpools) hold(path string, now time.Time) (lapsedNow bool) {
	first, seen := h.firstSeen[path]
	if !seen {
		h.firstSeen[path] = now
		h.log.With(logging.Context{Operation: "hold-spool", Path: path}).
			Log("spool held: no spawning call has claimed it yet, so it is re-checked every rescan and not read")
		return false
	}
	if now.Sub(first) < h.window || h.lapsed[path] {
		h.log.With(logging.Context{Operation: "hold-spool", Path: path}).
			LogVerbose("spool still held after %s", now.Sub(first))
		return false
	}
	h.lapsed[path] = true
	return true
}

// release stops holding a spool whose owner arrived.
func (h *heldSpools) release(path string) {
	if _, held := h.firstSeen[path]; !held {
		return
	}
	delete(h.firstSeen, path)
	delete(h.lapsed, path)
	h.log.With(logging.Context{Operation: "hold-spool", Path: path}).Log("spool released: its spawning call was observed")
}

// skip states, once per path, that a spool is never read and why.
func (h *heldSpools) skip(target discover.Target, reason, why string) {
	bound := h.log.With(logging.Context{Operation: "spool-skip", Path: target.Path, TaskID: target.TaskID, Reason: reason})
	if h.skipped[target.Path] {
		bound.LogVerbose("spool still not read; the decision was already stated for this path")
		return
	}
	h.skipped[target.Path] = true
	bound.Log("spool not read: %s", why)
}

// unrenderedSpool answers whether a spool is never read whoever claims it, and
// why. ok is false when the answer could not be established this pass; the
// failure is already stated and the spool is examined again next rescan.
func (s *sidecar) unrenderedSpool(target discover.Target) (reason, why string, skip, ok bool) {
	switch target.Kind {
	case tail.KindResidueSpool:
		return reasonUnrecognizedPrefix, "its task id carries no a/b/w kind prefix, so no conversion could be selected and nothing renders it", true, true
	case tail.KindAgentTranscript:
		info, err := os.Lstat(target.Path)
		if err != nil {
			if errors.Is(err, fs.ErrNotExist) {
				return reasonSpoolVanished, "it was gone before it could be examined; nothing was skipped because nothing was read", true, true
			}
			s.log.With(logging.Context{
				Operation: "spool-skip", Path: target.Path, TaskID: target.TaskID, Level: "warn",
			}).Log("not reading this agent spool yet: whether it is a link to the agent's own transcript could not be established, and reading a link would copy that transcript twice: %v", err)
			return "", "", false, false
		}
		if info.Mode()&os.ModeSymlink != 0 {
			return reasonTranscriptSymlink, "it is a link to the subagent's own transcript, which is ingested through its config-root path; reading the link would copy it twice", true, true
		}
	}
	return "", "", false, true
}

// resolveTarget decides whether a discovered target may be tailed, and as what.
//
// A CONFIG-ROOT PATH NAMES ITS OWN SESSION — the transcript IS that session's
// record — so it answers immediately. A spool does not: it is read only for a
// claimed run, and only when what it holds is rendered from nowhere else.
func (s *sidecar) resolveTarget(target discover.Target, now time.Time) (discover.Target, bool) {
	if target.SessionID != "" {
		return s.resolveTranscriptWorkspace(target)
	}
	reason, why, skip, ok := s.unrenderedSpool(target)
	if !ok {
		return discover.Target{}, false
	}
	if skip {
		s.held.skip(target, reason, why)
		return discover.Target{}, false
	}
	s.followRename(target)
	if obs, ok := s.owners.resolve(target); ok {
		s.held.release(target.Path)
		delete(s.unclaimedShells, target.TaskID)
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
		s.log.With(logging.Context{
			Operation: "spool-claim", Path: target.Path, TaskID: target.TaskID, ActivityID: obs.activityID,
			WorkspaceDir: target.WorkspaceDir, WorkspaceID: target.WorkspaceID, ClaudeSessionID: target.ClaudeSessionID,
		}).Log("spool claimed by its spawning call: it is read as %s for the rows that call's run renders", target.Kind)
		return target, true
	}
	s.noteUnclaimedShell(target)
	if !s.held.hold(target.Path, now) {
		return discover.Target{}, false
	}
	// The window lapsed with no claim. The spool stays held and UNREAD: it is
	// re-checked every rescan and read the moment a launch claims it.
	mtimeMs := fileActivityMs(target.Path, now.UnixMilli())
	if s.isBacklog(mtimeMs) {
		// A spool that already existed unclaimed before this sidecar started
		// is backlog: its spawning session is usually long gone and it will
		// never be claimed. A restart re-derives hundreds of these at once, so
		// it is accumulated and summarized by flushCatchupSummaries rather than
		// stated per file.
		s.catchupSpools.add(mtimeMs)
		s.log.With(logging.Context{Operation: "hold-expired", Path: target.Path, TaskID: target.TaskID, Reason: reasonSpoolUnclaimed, Level: "debug"}).
			LogVerbose("spool unclaimed after %s during startup catch-up: nothing renders it, so it is not read; it is summarized rather than stated on its own", s.held.window)
		return discover.Target{}, false
	}
	// A spool that appeared while the sidecar was already running and then
	// aged out unclaimed is a newly-arising condition, stated per file. It is
	// INFO, not WARN: not reading what nothing renders is the rule working.
	s.log.With(logging.Context{Operation: "hold-expired", Path: target.Path, TaskID: target.TaskID, Reason: reasonSpoolUnclaimed, Level: "info"}).
		Log("spool unclaimed after %s: nothing renders it, so it is not read; it stays held and is read from its start if a spawning call claims it later", s.held.window)
	return discover.Target{}, false
}

// reasonTranscriptVanished is the `resolve-transcript-workspace` record's
// discriminator: attribution did not FAIL, the file it had to read is no longer
// there. It separates an ordinary end from a transcript that is present and
// cannot be attributed, which is the one an operator must look at.
const reasonTranscriptVanished = "transcript_vanished"

// reasonFirstCWDFallback is the `resolve-transcript-workspace` record's
// discriminator for a transcript attributed to its FIRST cwd because none of
// its cwds encodes to the project folder the file lives in.
const reasonFirstCWDFallback = "first_cwd_fallback"

func (s *sidecar) resolveTranscriptWorkspace(target discover.Target) (discover.Target, bool) {
	key := target.ConfigRoot + "\x00" + target.ProjectKey + "\x00" + target.SessionID
	workspace, ok := s.workspaceBySession[key]
	if !ok {
		attribution, err := discover.ResolveWorkspace(target)
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
			if errors.Is(err, fs.ErrNotExist) {
				// THE TRANSCRIPT IS GONE, SO THERE IS NOTHING TO HOLD FOR. The
				// attribution read is the FIRST thing done to a discovered
				// transcript, and a vendor session directory deleted between the
				// scan and that read leaves nothing to attribute and nothing to
				// wait on: no bytes were skipped, because none were ever read.
				// That is an ordinary end, not an attribution failure, so it is
				// STATED rather than warned — once per file, by the same
				// dedupe above, and summarized when it is startup backlog.
				//
				// Every one of the ten of these in the 2026-09-13 15:28 gap scan
				// was exactly this: ten `.claude-chesscom` transcripts, a main
				// session and its `subagents/` subtree, whose whole session
				// directory was removed at 14:37.
				ctx.Level = "info"
				ctx.Reason = reasonTranscriptVanished
				s.log.With(ctx).Log("the transcript was gone before its first byte could be read, so there is no workspace to attribute and nothing was skipped: %v", err)
				return discover.Target{}, false
			}
			s.log.With(ctx).Log("transcript held: workspace attribution is required before any bytes are read: %v", err)
			return discover.Target{}, false
		}
		dir, id := attribution.Dir, attribution.ID
		workspace = workspaceAttribution{dir: dir, id: id}
		s.workspaceBySession[key] = workspace
		delete(s.workspaceFailures, key)
		if attribution.FirstCWDFallback {
			// Stated ONCE PER FILE: the attribution is cached under the
			// session key just above, so no rescan resolves this file again.
			s.log.With(logging.Context{
				Operation: "resolve-transcript-workspace", Path: target.Path,
				WorkspaceDir: dir, WorkspaceID: id, ClaudeSessionID: target.SessionID,
				Reason: reasonFirstCWDFallback, Level: "info",
			}).Log("no cwd in the transcript encodes to the project folder it lives in, so it is attributed to its first cwd")
		} else {
			s.log.With(logging.Context{
				Operation: "resolve-transcript-workspace", Path: target.Path,
				WorkspaceDir: dir, WorkspaceID: id, ClaudeSessionID: target.SessionID,
			}).LogVerbose("workspace attribution resolved from the main transcript")
		}
	}
	if workspace.dir == "" || workspace.id == "" {
		panic(fmt.Sprintf("sidecar: cached workspace attribution for %q is incomplete", key))
	}
	target.WorkspaceDir = workspace.dir
	target.WorkspaceID = workspace.id
	target.ClaudeSessionID = target.SessionID
	return target, true
}
