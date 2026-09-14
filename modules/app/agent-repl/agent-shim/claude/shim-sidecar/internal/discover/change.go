package discover

import (
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"agentrepl/shim-claude-sidecar/internal/logging"
)

// A NEW FILE IS FOUND WITHIN ONE POLL, NOT ONE RESCAN.
//
// Scan is the full enumeration and it runs on RescanInterval (30s by default),
// so until this file existed a transcript the vendor created one instant after
// a scan waited out the whole interval before anything read a byte of it. That
// is what a realtest measured on 2026-09-13: a fresh workspace's first turn
// concluded at 23:22:44, the vendor wrote its transcript at 23:22:44, and the
// sidecar's tail-pickup came at 23:23:14 — exactly one rescan later. The
// daemon, the shim and the webapp had already drawn the prompt, the turn end
// and the final-answer mark; only the assistant's ANSWER TEXT, which only this
// process reads, was thirty seconds late.
//
// THE FIX IS NOT A NARROWER SCAN (owner's standing rule: narrowing discovery
// "only serves to obfuscate inefficiency") AND NOT A SHORTER BLANKET RESCAN.
// The full glob costs what it costs — on the owner's machine 102 project
// directories, ~1800 transcripts, and a spool root (/private/tmp) with 23539
// children — and running it every second would spend that cost sixty times a
// minute to notice a handful of new files a year.
//
// SO THE PROBE ASKS THE DIRECTORY, NOT THE FILES. A file that appears changes
// the mtime of the directory it appears in, so one `stat` per candidate
// directory answers "is there anything new under here" for every file under it
// at once. Only a directory whose mtime MOVED is re-enumerated (one ReadDir),
// and only its own level is classified.
//
// MEASURED ON THE OWNER'S MACHINE, against both live config roots and
// /private/tmp: the candidate set is 189 directories and an idle probe costs
// 0.22-0.32ms, so a 1s poll spends 189 stats/second and no readdirs at all in
// steady state — inside the "few hundred stats per second" the ruling budgeted.
// The full Scan beside it costs 820ms for 2928 targets, which is 27ms/s
// amortized over its 30s interval: the probe is two orders of magnitude cheaper
// per second than the enumeration it front-runs, which is the whole reason it
// can run sixty times more often.
//
// NO FILE DESCRIPTOR IS HELD OPEN. A per-file kqueue/fsnotify watcher over the
// same corpus is REFUSED: the file-table exhaustion incident of 2026-09-13 (38
// ENFILE meta reads in one afternoon, from a box with no descriptors to spare)
// is exactly what thousands of held descriptors buy. A stat holds nothing.
//
// SCAN REMAINS THE BACKSTOP, unchanged, at its own interval. The probe is an
// accelerator and is allowed to miss:
//
//   - a directory registered by Scan is registered with the mtime it has AFTER
//     that scan globbed it, so a file created in the microseconds between the
//     glob and the stat is not seen as a change;
//   - a filesystem with coarse mtime granularity can land a write inside the
//     same timestamp the previous probe recorded;
//   - a directory nothing has ever put a discoverable file in, and whose parent
//     has not changed since boot, is not a candidate yet;
//   - the spool ROOT is deliberately not a candidate (see registerSpool).
//
// Every one of those is found by the next full Scan, so the worst case is the
// latency we had before this file existed, and the ordinary case is one poll.

// maxProbeDepth bounds how far below ONE changed directory a re-enumeration
// walks. The deepest shape the globs describe is
// projects/<slug>/<session>/subagents/workflows/wf_<id>/, five levels under a
// projects root, so five is the whole tree and never a truncation of it. The
// bound exists because a re-enumeration follows directories it has not seen
// before, and an unbounded walk under a directory the vendor nested something
// unexpected in would be a full recursive crawl on one mtime bump.
const maxProbeDepth = 5

// ChangedDir is one directory whose mtime moved since the previous probe,
// together with every target its re-enumeration classified. Targets are ALL of
// them, not only the new ones: which files are already being read is the
// reader's bookkeeping, not discovery's.
type ChangedDir struct {
	Dir     string
	Targets []Target
}

// probeItem is one directory queued for this pass. fresh marks a directory
// discovered by the pass itself, which is enumerated on the spot rather than
// compared against an mtime it has never had one of.
type probeItem struct {
	dir   string
	depth int
	fresh bool
}

// ScanChanged stats every candidate directory and re-enumerates only the ones
// whose mtime moved. It is the sidecar's SECOND discovery path, run on
// PollInterval beside the reads; Scan remains the first and the backstop.
//
// A DIRECTORY THAT VANISHED IS AN ORDINARY END, NOT A FAULT. Vendor session
// directories, workflow directories and task spools are deleted all the time,
// and a candidate that is gone between one stat and the next simply stops being
// a candidate: it is dropped from the set and states nothing. A probe that
// fails for ANY OTHER reason is a fault an operator has to see — the directory
// is real and unreadable, and every file under it is now found only by the full
// rescan — so it is stated, ONCE per directory per condition (see
// stateProbeFailure).
func (d *Discoverer) ScanChanged() []ChangedDir {
	queue := make([]probeItem, 0, len(d.probed))
	for _, dir := range sortedDirs(d.probed) {
		queue = append(queue, probeItem{dir: dir})
	}
	var out []ChangedDir
	stats, changed, found := 0, 0, 0
	// The queue GROWS as directories discovered by this pass are appended, so it
	// is walked by index rather than ranged over.
	for i := 0; i < len(queue); i++ {
		item := queue[i]
		stats++
		info, err := os.Stat(item.dir)
		if err != nil {
			if errors.Is(err, fs.ErrNotExist) {
				d.forgetDir(item.dir)
				continue
			}
			d.stateProbeFailure(item.dir, err)
			continue
		}
		if !info.IsDir() {
			// The name is a file now. It holds no candidates and never will
			// under this name; the full rescan owns whatever replaced it.
			d.forgetDir(item.dir)
			continue
		}
		if !item.fresh && info.ModTime().Equal(d.probed[item.dir]) {
			continue
		}
		entries, err := os.ReadDir(item.dir)
		if err != nil {
			if errors.Is(err, fs.ErrNotExist) {
				// Deleted between the stat and the read. Ordinary, and stated as
				// nothing: the ruling is explicit that a vanishing directory is
				// not an error record.
				d.forgetDir(item.dir)
				continue
			}
			d.stateProbeFailure(item.dir, err)
			continue
		}
		changed++
		// THE MTIME RECORDED IS THE ONE READ BEFORE THE ENUMERATION, so a file
		// written while the ReadDir was in flight leaves a directory whose mtime
		// no longer matches and is re-enumerated next tick rather than lost.
		d.probed[item.dir] = info.ModTime()
		delete(d.probeFailures, item.dir)
		var targets []Target
		for _, entry := range entries {
			path := filepath.Join(item.dir, entry.Name())
			if entry.IsDir() {
				if item.depth >= maxProbeDepth {
					continue
				}
				if _, known := d.probed[path]; known {
					continue
				}
				// A directory this pass has never seen is enumerated on the spot:
				// the file that made its parent's mtime move may well be inside
				// it, one level down.
				d.probed[path] = time.Time{}
				queue = append(queue, probeItem{dir: path, depth: item.depth + 1, fresh: true})
				continue
			}
			if target, ok := d.Classify(path); ok {
				targets = append(targets, target)
			}
		}
		if len(targets) == 0 {
			continue
		}
		found += len(targets)
		out = append(out, ChangedDir{Dir: item.dir, Targets: targets})
	}
	d.log.With(logging.Context{Operation: "discover-change"}).LogVerbose(
		"change probe: %d candidate director(ies) stat'd, %d changed, %d target(s) re-enumerated", stats, changed, found)
	return out
}

// forgetDir drops a directory from the candidate set. Anything that appears
// under that name again is re-registered by the next full Scan.
func (d *Discoverer) forgetDir(dir string) {
	delete(d.probed, dir)
	delete(d.probeFailures, dir)
}

// stateProbeFailure states a probe failure that is NOT a vanished directory.
//
// ONCE PER DIRECTORY PER CONDITION. The probe runs every poll, so a directory
// the process cannot read — a permission it does not have, an I/O error — would
// otherwise write one record per second for as long as the condition lasts.
// That is the same inverted-pyramid flood the discover-meta holds already
// leveled, so the same shape answers it: the first statement of a condition is
// the loud one, a repeat of the SAME condition is verbose, and a CHANGED
// condition is stated again.
func (d *Discoverer) stateProbeFailure(dir string, err error) {
	detail := err.Error()
	bound := d.log.With(logging.Context{Operation: "discover-change", Path: dir})
	if d.probeFailures[dir] == detail {
		bound.LogVerbose("the change probe still cannot read this directory for the same reason: %v", err)
		return
	}
	d.probeFailures[dir] = detail
	bound.With(logging.Context{Level: "warn"}).Log(
		"the change probe could not read this directory, so a file created under it is found only by the next full rescan rather than on the next poll: %v", err)
}

// registerProjectsRoot seeds the candidate set with one config root's projects
// directory AND every project directory under it.
//
// THE ROOT ITSELF IS A CANDIDATE because a workspace that has never run before
// gets a project directory that does not exist yet, and the realtest that
// produced this whole file was exactly that case: a FRESH workspace's first
// turn. The root's mtime moves when the vendor creates that directory, the
// probe enumerates it on the spot, and the transcript written into it a moment
// later moves the new directory's own mtime.
//
// ITS CHILDREN ARE REGISTERED EAGERLY, by one ReadDir per root per scan, rather
// than waiting for a match to name them. A project directory that holds no
// discoverable file today — the vendor created it and has not written the
// transcript yet — has no match to be registered from, and it is the single
// most likely directory in the tree to gain one.
func (d *Discoverer) registerProjectsRoot(root string) {
	projects := filepath.Join(root, "projects")
	d.registerDir(projects)
	entries, err := os.ReadDir(projects)
	if err != nil {
		// A config root with no projects directory yet is ordinary (a second
		// account that has never been used), and any other failure is stated by
		// the probe itself the moment it stats the root.
		return
	}
	for _, entry := range entries {
		if entry.IsDir() {
			d.registerDir(filepath.Join(projects, entry.Name()))
		}
	}
}

// registerAncestors registers the directory a discovered file lives in and
// every directory between it and boundary, EXCLUSIVE of boundary itself.
func (d *Discoverer) registerAncestors(dir, boundary string) {
	prefix := boundary + string(filepath.Separator)
	for current := dir; strings.HasPrefix(current, prefix); current = filepath.Dir(current) {
		d.registerDir(current)
	}
}

// registerSpool registers a discovered spool's own tasks directory and its
// ancestors up to — but never including — the spool root.
//
// THE SPOOL ROOT IS NOT A CANDIDATE, and that is a measurement rather than a
// preference. In production it is /private/tmp, which on the owner's machine
// holds 23539 entries and whose mtime moves whenever anything on the box writes
// a temp file. Making it a candidate would run a full recursive re-enumeration
// of /private/tmp on most poll ticks to find the handful of spool trees under
// it — which is the blanket rescan this design exists to avoid, once a second.
// A brand-new spool tree is therefore found by the next full Scan, exactly as
// it was before; every spool under a tree that has already been scanned once is
// found on the next poll.
func (d *Discoverer) registerSpool(path string) {
	d.registerAncestors(filepath.Dir(path), d.spoolRoot)
}

// registerDir adds one directory to the candidate set at the mtime it has now.
//
// IT NEVER RE-STAMPS A DIRECTORY IT ALREADY KNOWS. The stored mtime is the
// probe's memory of what it has already enumerated; overwriting it here — from
// a Scan that ran after a file appeared but before the probe saw it — would
// erase the very change the probe exists to notice.
func (d *Discoverer) registerDir(dir string) {
	if _, known := d.probed[dir]; known {
		return
	}
	info, err := os.Stat(dir)
	if err != nil || !info.IsDir() {
		// Nothing to probe. A directory that appears later is registered by the
		// scan that first globs something under it, or by its parent's probe.
		return
	}
	d.probed[dir] = info.ModTime()
}

// sortedDirs answers a stable iteration order for the candidate set, so one
// pass's records and one pass's discovery order do not depend on map ordering.
func sortedDirs(dirs map[string]time.Time) []string {
	out := make([]string, 0, len(dirs))
	for dir := range dirs {
		out = append(out, dir)
	}
	sort.Strings(out)
	return out
}
