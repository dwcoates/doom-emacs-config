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
	"time"

	"agentrepl/shim-claude-sidecar/internal/discover"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

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
		return target, true
	}
	if target.Kind == tail.KindResidueSpool {
		// Its task-id prefix already failed classification, so no owner would
		// change what happens to it: the bytes go to residue either way.
		return target, true
	}
	if obs, ok := s.owners.resolve(target); ok {
		s.held.release(target.Path)
		target.AgentID = obs.agentID
		return target, true
	}
	if !s.held.hold(target.Path, now) {
		return discover.Target{}, false
	}
	// The wait expired. The bytes are ingested as residue rather than waited on
	// forever, and the file keeps being tailed.
	if s.held.demote(target.Path) {
		s.log.With(logging.Context{Operation: "hold-expired", Path: target.Path, TaskID: target.TaskID, Level: "warn"}).
			Log("spool unclaimed after %s: its bytes are ingested as unparsed residue naming the spool as their source, and it keeps being tailed", s.held.window)
	}
	target.Kind = tail.KindResidueSpool
	target.Raw = true
	return target, true
}
