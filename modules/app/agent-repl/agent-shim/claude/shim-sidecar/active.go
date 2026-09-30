package main

// active.go — ONLY FILES THAT BELONG TO AN ACTIVE WORKSPACE ARE WATCHED.
//
// Owner ruling, 2026-09-24: "we can only check files that belong to an active
// workspace." Until then this process watched EVERY transcript under both
// config roots — 2,085 files on the owner's machine, of which about 19 had
// changed in the last hour — and the per-watcher tail poll (`pollAll`, one stat
// per watcher per tick) and `rekeyRotations` scaled with that whole corpus.
//
// THIS IS AN OPTIMIZATION, and every decision below is shaped by what it costs:
// the watched set is the files of the workspaces whose shim session is live
// (65 on the owner's machine as measured on 2026-09-24, against 2,110
// discovered), so the per-tick poll is that many stats rather than the whole
// corpus's.
//
// WHAT "ACTIVE" MEANS, AND WHO SAYS SO. A workspace is active while its shim
// holds `<lock dir>/workspace-<key>.lock` (internal/livelock): the kernel lock a
// shim takes inside StartSession and holds until it dies, which the daemon
// probes before spawning. `<key>` is also the directory the shim writes its
// identity records under, and `agent-id.json` there names the conversation's
// ORIGINAL vendor session id. So the active set is a set of CONVERSATIONS, read
// off two things the shim already writes, with no coupling to the daemon or its
// database. A lock that cannot be probed is read as HELD: "could not tell" is
// never "free", and reading more than necessary loses nothing.
//
// WHICH FILES BELONG TO A CONVERSATION. A file belongs to the conversation its
// vendor session id resolves to through the identity records (identity.Lookup):
//
//   - a main transcript by its own id — the original, or a rotation or fork the
//     shim linked to it;
//   - a subagent transcript or workflow journal by the session directory it
//     sits under;
//   - a task spool by the transcript session whose launch claimed it (the owner
//     index), because a spool's path names no session at all. A spool no launch
//     has claimed yet belongs to nobody and goes on to the hold, which never
//     reads it either.
//
// A session run outside agent-repl has no identity record, resolves to itself,
// and is never an active conversation, so it is never watched.
//
// ACTIVATION CATCHES UP FROM THE CURSOR. A file gated out is kept, unread, in
// `dormant`. When a conversation becomes active — opened, revived, resumed,
// rebound, or a rotation linked into it — every dormant file that now belongs
// to it is offered to `watchTargets`, which builds its tailer from the cursor
// the store holds. Nothing written while the workspace was inactive is lost: it
// is read from the committed offset exactly as a restart reads it.
//
// DEACTIVATION DRAINS. A conversation whose lock is released DRAINS rather than
// vanishing: its files stay watched until nothing is owed, by the LOST policy's
// own bounds (drained, below). Only then are they dropped back to dormant.
//
// A DRAINING CONVERSATION OUTLIVES ITS UNDISCOVERED FILES. A session can end
// before discovery has found the transcript it wrote (a fast turn, then the
// idle sweep's stand-down, inside one change-probe tick). Retiring it for
// having no watched file would gate that transcript out when it is found, and
// its answer would go unread until the workspace is next active. So it is
// retired only once a full scan that began after it ended has completed: every
// file it owned is then either watched, and drains by the bounds above, or
// does not exist.
//
// DISCOVERY IS NOT PER-FILE. The probe is one readdir of `<state>/shim`, two
// stats and one lock query per workspace, a constant per tick; files are found
// by the directory-level discovery that already exists (Scan and ScanChanged).

import (
	"errors"
	"fmt"
	"io/fs"
	"os"
	"sort"
	"time"

	"agentrepl/shim-claude-sidecar/internal/discover"
	"agentrepl/shim-claude-sidecar/internal/livelock"
	"agentrepl/shim-claude-sidecar/internal/logging"
	"agentrepl/shim-claude-sidecar/internal/tail"
)

// drainingConversation is a conversation whose lock was released: its
// workspace key, and the full-scan count when it ended.
type drainingConversation struct {
	key        string
	scansAtEnd uint64
}

// liveProbe answers, for each workspace lock path, whether a live shim holds
// it; a path it could not answer is in the error map instead. It is
// livelock.Held in production and a seam for nothing else: the suites take
// real flocks.
type liveProbe func(paths []string) (map[string]bool, map[string]error)

// conversationOf answers the ORIGINAL vendor session id of the conversation a
// target belongs to, and false when the target names none yet (a spool no
// launch has claimed).
func (s *sidecar) conversationOf(target discover.Target) (string, bool) {
	session := target.SessionID
	if session == "" {
		if s.owners.conflicts[target.TaskID] {
			return "", false
		}
		obs, ok := s.owners.byTask[target.TaskID]
		if !ok {
			return "", false
		}
		session = obs.claudeSessionID
	}
	if session == "" {
		return "", false
	}
	return s.identity.Lookup(session).Original, true
}

// admits reports whether a discovered target may be watched: its conversation
// is active or draining, or it names no conversation yet and is left to the
// hold.
func (s *sidecar) admits(target discover.Target) bool {
	original, known := s.conversationOf(target)
	if !known {
		return true
	}
	return s.member(original)
}

// member reports whether a conversation's files are watched.
func (s *sidecar) member(original string) bool {
	if _, ok := s.active[original]; ok {
		return true
	}
	_, ok := s.draining[original]
	return ok
}

// goDormant keeps a gated-out target, unread, for the activation that would
// admit it, and states the decision once per path.
func (s *sidecar) goDormant(target discover.Target) {
	// A ROTATION HOLD IS RE-EXAMINED ON EVERY POLL, which reads the file's
	// opening; a file no active workspace owns must not keep paying that.
	delete(s.rotationHeld, target.Path)
	if _, known := s.dormant[target.Path]; !known {
		s.log.With(logging.Context{
			Operation: "watch-dormant", Path: target.Path, TaskID: target.TaskID, VendorSessionID: target.SessionID,
		}).LogVerbose("not watched: no active workspace's conversation owns this file; it is read from its cursor if one becomes active")
	}
	s.dormant[target.Path] = target
}

// refreshActive re-reads which conversations are active, admits the dormant
// files a change made members, and drops the watched files that have drained.
// It runs on every poll tick and inside every rescan.
func (s *sidecar) refreshActive(now time.Time) {
	s.requireCursors("refreshActive")
	s.applyActive(now, s.identity.RefreshIfMoved())
}

// refreshActiveAfterRescanRefresh is refreshActive for the rescan, which has
// just refreshed the identity records itself.
//
// OPTIMIZATION: the record fingerprint is not re-walked (one glob and two stats
// per workspace saved per rescan), and the dormant set is not re-offered for
// the refresh — the scan right after offers every discovered file anyway.
func (s *sidecar) refreshActiveAfterRescanRefresh(now time.Time) {
	s.requireCursors("refreshActive")
	s.applyActive(now, false)
}

// applyActive is the body of both: probe the locks, then reconcile the watched
// set. recordsMoved says an identity record changed, which can change which
// conversation a dormant file belongs to without any lock changing.
func (s *sidecar) applyActive(now time.Time, recordsMoved bool) {
	refreshed := recordsMoved
	next := s.probeActive()
	var started, ended int
	for original, key := range s.active {
		if _, still := next[original]; !still {
			ended++
			s.draining[original] = drainingConversation{key: key, scansAtEnd: s.fullScans}
		}
	}
	for original := range next {
		if _, was := s.active[original]; !was {
			started++
		}
		delete(s.draining, original)
	}
	s.active = next
	admitted := 0
	if refreshed || started > 0 || ended > 0 {
		var ok bool
		admitted, ok = s.admitDormant(now)
		if !ok {
			// The store could not answer for an admitted file's cursor, so
			// production is suspended and every watcher is gone with it.
			return
		}
	}
	dropped := s.dropDrained(now)
	if started+ended+admitted+dropped == 0 {
		return
	}
	s.log.With(logging.Context{Operation: "watched-set"}).Log(
		"the watched set changed: %d workspace conversation(s) active (%d started, %d ended), %d draining; %d file(s) watched (%d admitted from dormant, %d dropped as drained), %d discovered file(s) dormant",
		len(s.active), started, ended, len(s.draining), len(s.watchers), admitted, dropped, len(s.dormant))
}

// probeActive answers every conversation whose workspace lock a live shim
// holds, mapped to its workspace key.
func (s *sidecar) probeActive() map[string]string {
	workspaces := s.identity.Workspaces()
	keys := make([]string, 0, len(workspaces))
	paths := make([]string, 0, len(workspaces))
	for key := range workspaces {
		keys = append(keys, key)
	}
	sort.Strings(keys)
	for _, key := range keys {
		paths = append(paths, livelock.Path(s.options.LockDir, key))
	}
	held, errs := s.lockHeld(paths)
	next := make(map[string]string, len(keys))
	for i, key := range keys {
		path := paths[i]
		original := workspaces[key]
		if err := errs[path]; err != nil {
			// COULD NOT TELL IS NEVER READ AS FREE. Watching a conversation whose
			// shim may be gone costs a few stats a tick; not watching one whose
			// shim is alive would leave its answer unread.
			next[original] = key
			s.stateLockFailure(path, key, err)
			continue
		}
		if _, failed := s.lockFailures[path]; failed {
			delete(s.lockFailures, path)
			s.log.With(logging.Context{Operation: "workspace-lock-probe", Path: path}).Log(
				"workspace %s's lock answers again (held=%t)", key, held[path])
		}
		if held[path] {
			next[original] = key
		}
	}
	return next
}

// stateLockFailure states a lock that could not be probed once per path per
// distinct error; a repeat is verbose.
func (s *sidecar) stateLockFailure(path, key string, err error) {
	detail := err.Error()
	bound := s.log.With(logging.Context{Operation: "workspace-lock-probe", Path: path, Level: "warn"})
	if s.lockFailures[path] == detail {
		bound.With(logging.Context{Level: "debug"}).LogVerbose("workspace %s's lock still cannot be probed; it is still treated as held: %v", key, err)
		return
	}
	s.lockFailures[path] = detail
	bound.Log("workspace %s's lock cannot be probed, so its conversation is treated as ACTIVE and its files stay watched: %v", key, err)
}

// admitDormant offers every dormant file that now belongs to an active or
// draining conversation to watchTargets, which reads it from its stored cursor.
// It answers how many it started watching, and false when the store could not
// answer for one (production is then suspended).
func (s *sidecar) admitDormant(now time.Time) (int, bool) {
	var ready []discover.Target
	for _, target := range s.dormant {
		if s.admits(target) {
			ready = append(ready, target)
		}
	}
	if len(ready) == 0 {
		return 0, true
	}
	sort.Slice(ready, func(i, j int) bool { return ready[i].Path < ready[j].Path })
	for _, target := range ready {
		s.log.With(logging.Context{
			Operation: "watch-admit", Path: target.Path, TaskID: target.TaskID, VendorSessionID: target.SessionID,
		}).LogVerbose("its workspace's conversation is active; it is read from the cursor the store holds")
	}
	return s.watchTargets(ready, now)
}

// dropDrained drops every watched file whose conversation is no longer active
// and which the file plane owes nothing more, and retires a draining
// conversation once none of its files is still watched and a full scan has
// completed since it ended. It answers how many files it dropped.
func (s *sidecar) dropDrained(now time.Time) int {
	owed := map[string]bool{}
	dropped := 0
	paths := make([]string, 0, len(s.watchers))
	for path := range s.watchers {
		paths = append(paths, path)
	}
	sort.Strings(paths)
	for _, path := range paths {
		w := s.watchers[path]
		original, known := s.conversationOf(w.target)
		if !known {
			continue
		}
		if _, active := s.active[original]; active {
			continue
		}
		if !s.drained(path, w, now) {
			owed[original] = true
			continue
		}
		s.drop(path, w)
		dropped++
	}
	for original, ended := range s.draining {
		if !owed[original] && s.fullScans > ended.scansAtEnd {
			delete(s.draining, original)
		}
	}
	return dropped
}

// drained reports whether a watched file of a conversation that is no longer
// active is owed nothing more.
//
// THE BOUND IS THE LOST POLICY'S, REUSED RATHER THAN DUPLICATED:
//
//   - a detached run the tracker still holds OPEN is owed a conclusion — its
//     own terminal, or LOST once its kind's silence window passes — so it stays
//     watched until the policy has one;
//   - durable bytes past the committed cursor are owed a read;
//   - a transcript is owed its tail until it has been silent for the tracker's
//     agent-silence window, the same "an agent has stopped working on this"
//     bound the boot rewind's at-rest test uses: a vendor process that outlived
//     its shim may still be writing.
//
// A FILE THAT IS GONE IS OWED NOTHING. A file whose size cannot be read for any
// other reason is kept, and the failure is stated once.
func (s *sidecar) drained(path string, w *watched, now time.Time) bool {
	if s.tracker.Open(path) {
		return false
	}
	if w.heal != nil {
		// A RE-DERIVATION IN PROGRESS IS OWED. Dropping it would leave the rows
		// the older conversion made wrong standing until the workspace is next
		// active; the stored cursor would resume it then, but nothing says when.
		return false
	}
	info, err := os.Stat(path)
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			return true
		}
		s.stateDrainFailure(path, err)
		return false
	}
	delete(s.drainFailures, path)
	if w.tailer.Offset() < info.Size() {
		return false
	}
	switch w.target.Kind {
	case tail.KindSessionTranscript, tail.KindAgentTranscript, tail.KindWorkflowJournal:
		return now.Sub(info.ModTime()) > s.tracker.Windows().AgentSilence
	default:
		return true
	}
}

// stateDrainFailure states once per path per distinct error that a file could
// not be tested for having drained; it stays watched.
func (s *sidecar) stateDrainFailure(path string, err error) {
	detail := err.Error()
	bound := s.log.With(logging.Context{Operation: "watch-drop", Path: path, Level: "warn"})
	if s.drainFailures[path] == detail {
		bound.With(logging.Context{Level: "debug"}).LogVerbose("still cannot tell whether this file has drained; it stays watched: %v", err)
		return
	}
	s.drainFailures[path] = detail
	bound.Log("cannot tell whether this file of an ended workspace has drained, so it stays watched: %v", err)
}

// drop stops watching one drained file and keeps it dormant, so a later
// activation reads it again from the cursor the store holds.
func (s *sidecar) drop(path string, w *watched) {
	delete(s.watchers, path)
	// THE BOOT REWIND IS ONCE PER WATCH. A re-watched file gets its own at-rest
	// decision, because the tailer and the converter's in-memory joins it would
	// re-warm are new.
	delete(s.rewound, w.tailer.FileID())
	s.dormant[path] = w.target
	s.log.With(logging.Context{
		Operation: "watch-drop", Path: path, TaskID: w.target.TaskID, FileID: w.tailer.FileID(),
		Offset: logging.Off(w.tailer.Offset()),
	}).LogVerbose("dropped: its workspace's session ended and nothing more is owed from it (%s)", w.target.Kind)
}

// pruneDormant forgets every dormant file a full scan no longer found. It is
// the only thing that removes a vanished file from the dormant set.
func (s *sidecar) pruneDormant(scanned []discover.Target) {
	present := make(map[string]bool, len(scanned))
	for _, target := range scanned {
		present[target.Path] = true
	}
	for path := range s.dormant {
		if !present[path] {
			delete(s.dormant, path)
		}
	}
}

// requireLockDir asserts the one option the active-workspace probe cannot run
// without. An empty lock dir would probe relative paths that exist nowhere and
// read every workspace as closed, which is a sidecar that silently reads
// nothing.
func requireLockDir(options Options) {
	if options.LockDir == "" {
		panic(fmt.Sprintf("sidecar: Options.LockDir is empty; the active-workspace probe cannot find a shim's workspace lock (%+v)", options.StateDir))
	}
}
