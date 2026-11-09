package heldingress

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"sync"

	"agent-repld/internal/bounce"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/intakegate"
	"agent-repld/internal/kernelevent"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/promptqueue"
	"agent-repld/internal/recordlock"
	"agent-repld/internal/resolve/holds"
	"agent-repld/internal/wsm"
)

// The operation names this package's records carry.
const (
	opRun        = "daemon.heldingress.run"
	opIngest     = "daemon.heldingress.ingest"
	opDedupe     = "daemon.heldingress.dedupe"
	opDefer      = "daemon.heldingress.defer"
	opQuarantine = "daemon.heldingress.quarantine"
	opRemove     = "daemon.heldingress.remove"
	opGate       = "daemon.heldingress.gate"
	opLock       = "daemon.heldingress.lock"
)

// retry is one refused entry's standing: how often it was refused and the
// refusal kinds already recorded at their own level. A refused entry is
// submitted again at the next sweep a change wakes (Run), never on a delay.
type retry struct {
	attempts int
	// recorded names every kind of refusal this entry was already recorded
	// under. A kind is stated once at its own level; its repeats are DEBUG.
	recorded map[string]bool
	// wait is what the entry's last refusal says it waits on, drawn on its
	// held-section card until the next attempt answers otherwise.
	wait holds.IngressWait
}

// The kinds of deferral an entry can meet. Each is recorded at its own level
// ONCE per entry, so a condition that changes (a merge, then a cold gate) is
// still stated, and one that stands is not repeated on every retry.
const (
	kindUnregistered = "unregistered"
	kindLookupFailed = "lookup_failed"
	kindMerging      = "merging"
	kindColdGate     = "cold_gate"
	kindNoSession    = "no_session"
	kindMovedAway    = "moved_away"
	kindFault        = "fault"
)

// ingress is the held-prompt ingress.
type ingress struct {
	deps Deps

	// mu serializes sweeps: Run is the one caller in production, but a sweep
	// must never interleave with another over the same files.
	mu sync.Mutex
	// retries is keyed by file path and forgotten when the file is gone. It is
	// in memory on purpose: a restart retries every entry at once, which is
	// exactly what a restart is for.
	retries map[string]retry
	// gate answers whether this daemon takes the intake at all.
	gate *intakegate.Gate
	// stated names the workspaces whose unaccepted entries the last sweep
	// stated to the held section, so a sweep that finds none of a workspace's
	// entries left states the empty list once.
	stated map[ids.WorkspaceID]bool
}

func removeFile(path string) error { return os.Remove(path) }

// Run sweeps until ctx is cancelled: once at start, so everything written
// while no daemon was serving is ingested as soon as this one starts, then at
// every change that can let an entry in.
//
// NO SWEEP WAITS ON A TIMER. Three changes wake one:
//   - an entry written into (or removed from) the directory, or the directory
//     made, seen on a kernel watch of the directory and its parent;
//   - this daemon starting or stopping to take the intake (IntakeChanged);
//   - a change of the fleet while an entry waits (FleetChanges), which is
//     what clears every condition a refused entry waits on.
//
// EVERY REGISTRATION IS TAKEN BEFORE THE SWEEP IT FOLLOWS, so a change that
// lands during a sweep is already pending and wakes the next one at once.
func (i *ingress) Run(ctx context.Context) error {
	log := i.deps.Log.Global().With(dlog.Context{"dir": i.deps.Dir})
	// The directory's parent is followed too: its change is the directory
	// being made, which is never done here (see Sweep).
	changes, stop := kernelevent.Follow(ctx, filepath.Dir(i.deps.Dir), i.deps.Dir)
	defer stop()
	fleet := i.deps.FleetChanges(ctx)
	log.Info(opRun, "watching the held-prompt ingress", dlog.Context{"glob": Glob})
	for {
		intake := i.deps.IntakeChanged()
		if err := i.Sweep(ctx); err != nil && ctx.Err() == nil {
			// A sweep that fails removes nothing, so the next change meets
			// the same entries.
			log.Error(opRun, "a sweep of the held-prompt ingress failed", dlog.Context{"cause": err.Error()})
		}
		woke, err := i.awaitWake(ctx, changes, intake, fleet)
		if ctx.Err() != nil {
			log.Info(opRun, "stopped watching the held-prompt ingress", nil)
			return ctx.Err()
		}
		if err != nil {
			log.Error(opRun, "could not watch the held-prompt ingress; it takes no entry until the daemon restarts", dlog.Context{"cause": err.Error()})
			return err
		}
		log.Debug(opRun, "a change woke the held-prompt ingress", dlog.Context{"woke": woke})
	}
}

// The changes awaitWake answers as having woken a sweep.
const (
	wokeByDir    = "directory"
	wokeByIntake = "intake"
	wokeByFleet  = "fleet"
)

// awaitWake waits for the next change that can let an entry in, answering
// which one woke it, the directory watch's error, or nothing once ctx ends. A
// fleet change wakes a sweep only while an entry waits; one with nothing
// waiting is taken and waited past.
func (i *ingress) awaitWake(ctx context.Context, dir <-chan error, intake, fleet <-chan struct{}) (string, error) {
	for {
		select {
		case <-ctx.Done():
			return "", nil
		case err := <-dir:
			return wokeByDir, err
		case <-intake:
			return wokeByIntake, nil
		case _, ok := <-fleet:
			if !ok {
				return "", nil
			}
			if i.waiting() {
				return wokeByFleet, nil
			}
		}
	}
}

// waiting reports whether an entry the queue refused is still on disk.
func (i *ingress) waiting() bool {
	i.mu.Lock()
	defer i.mu.Unlock()
	return len(i.retries) > 0
}

// Sweep ingests every entry once, in name order.
//
// ONLY THE DAEMON THAT SERVES SWEEPS (intakegate): a joining successor and an
// incumbent whose handover has begun take nothing.
//
// A SWEEP IS EXCLUSIVE ACROSS DAEMONS, under a kernel lock held for its whole
// run. The gate alone cannot make it so: a sweep the incumbent began before
// its handover can still be submitting when the successor's gate opens, and
// an entry submitted by both -- the incumbent's claim of its key not yet
// accepted when the successor re-drives it -- would be delivered twice. The
// lock dies with its holder, so a daemon that crashed mid-sweep leaves nothing
// behind but its entries, which the next sweep takes under their keys.
//
// ORDER IS KEPT PER WORKSPACE: once one workspace's entry is left for a later
// sweep, every later entry for that workspace is left too, so a prompt is
// never delivered ahead of one its writer wrote before it.
func (i *ingress) Sweep(ctx context.Context) error {
	i.mu.Lock()
	defer i.mu.Unlock()
	if !i.gate.Admits() {
		return nil
	}
	// THE LOCK NEVER CREATES THE DIRECTORY: the ingress is a directory in the
	// shared state root, which a client creates when it writes an entry, and
	// recreating it would put back a state root that was deleted under this
	// daemon (its own root watch would then read the recreation as another
	// directory). No directory is no entry, so there is nothing to sweep.
	lock, held, err := recordlock.HoldIn(i.deps.LockPath)
	if errors.Is(err, os.ErrNotExist) {
		i.deps.Log.Global().Debug(opLock, "the ingress directory does not exist; nothing has been written to sweep", dlog.Context{"dir": i.deps.Dir})
		return nil
	}
	if err != nil {
		return fmt.Errorf("take the sweep lock %q: %w", i.deps.LockPath, err)
	}
	if !held {
		i.deps.Log.Global().Debug(opLock, "another daemon is sweeping the ingress; this sweep takes nothing", dlog.Context{"lock": i.deps.LockPath})
		return nil
	}
	defer func() {
		if err := lock.Release(); err != nil {
			i.deps.Log.Global().Error(opLock, "could not release the sweep lock", dlog.Context{"lock": i.deps.LockPath, "cause": err.Error()})
		}
	}()
	matches, err := filepath.Glob(filepath.Join(i.deps.Dir, Glob))
	if err != nil {
		return fmt.Errorf("glob %q: %w", Glob, err)
	}
	sort.Strings(matches)
	present := make(map[string]bool, len(matches))
	sw := &sweep{waiting: map[string]bool{}, lookups: map[string]lookup{}, pending: map[ids.WorkspaceID][]holds.IngressEntry{}}
	for _, path := range matches {
		if err := ctx.Err(); err != nil {
			return err
		}
		present[path] = true
		i.ingest(ctx, path, sw)
	}
	for path := range i.retries {
		if !present[path] {
			delete(i.retries, path)
		}
	}
	i.statePending(sw.pending)
	return nil
}

// sweep is one sweep's working state.
type sweep struct {
	// waiting names the project directories with an entry left for a later
	// sweep in this one.
	waiting map[string]bool
	// lookups are this sweep's workspace lookups, by project directory, so a
	// workspace's entries are resolved once per sweep.
	lookups map[string]lookup
	// pending are the entries this sweep left unaccepted, by workspace, in
	// name order.
	pending map[ids.WorkspaceID][]holds.IngressEntry
}

// lookup is one project directory's workspace lookup.
type lookup struct {
	record wsm.Workspace
	err    error
}

// workspaceOf resolves DIR's workspace once per sweep.
func (i *ingress) workspaceOf(ctx context.Context, sw *sweep, dir string) (wsm.Workspace, error) {
	if got, ok := sw.lookups[dir]; ok {
		return got.record, got.err
	}
	record, err := i.deps.WorkspaceByDir(ctx, dir)
	sw.lookups[dir] = lookup{record: record, err: err}
	return record, err
}

// statePending tells the held section every workspace's unaccepted entries:
// each workspace this sweep left an entry of, and once more, empty, each one
// the previous sweep stated and this one left none of.
func (i *ingress) statePending(pending map[ids.WorkspaceID][]holds.IngressEntry) {
	for ws, entries := range pending {
		i.deps.Pending.SetIngressPending(ws, entries)
	}
	for ws := range i.stated {
		if _, still := pending[ws]; !still {
			i.deps.Pending.SetIngressPending(ws, nil)
		}
	}
	i.stated = make(map[ids.WorkspaceID]bool, len(pending))
	for ws := range pending {
		i.stated[ws] = true
	}
}

// leavePending records that ENTRY of workspace WS stays for a later sweep,
// waiting on WAIT.
func leavePending(sw *sweep, ws ids.WorkspaceID, entry decoded, wait holds.IngressWait) {
	sw.pending[ws] = append(sw.pending[ws], holds.IngressEntry{
		Key: entry.IdempotencyKey, Said: entry.said, QueuedAt: entry.queuedAt, Wait: wait,
	})
}

// behindEarlier is the wait of an entry left because an earlier entry of its
// workspace is still waiting.
var behindEarlier = holds.IngressWait{Kind: holds.IngressAwaitingIntake, Detail: "waits behind an earlier prompt of this workspace"}

// ingest handles one entry file. An entry of a registered workspace that the
// queue does not accept in this sweep is left in SW's pending entries.
func (i *ingress) ingest(ctx context.Context, path string, sw *sweep) {
	log := i.deps.Log.Global().With(dlog.Context{"path": path})
	data, err := os.ReadFile(path)
	switch {
	case errors.Is(err, os.ErrNotExist):
		// Another daemon on this state root (a handover's successor) took
		// it between the glob and the read.
		log.Debug(opIngest, "the entry was gone before it was read", nil)
		return
	case err != nil:
		log.Error(opIngest, "could not read a held-prompt entry; it stays for the next sweep", dlog.Context{"cause": err.Error()})
		return
	}
	entry, err := parse(data)
	if err != nil {
		i.quarantine(log, path, err)
		return
	}
	dir := entry.ProjectDir
	log = log.With(dlog.Context{"project_dir": dir, "idempotency_key": entry.IdempotencyKey})
	record, lookupErr := i.workspaceOf(ctx, sw, dir)
	if sw.waiting[dir] {
		log.Debug(opDefer, "an earlier entry for this workspace is still waiting, so this one waits behind it", nil)
		if lookupErr == nil {
			leavePending(sw, record.ID, entry, behindEarlier)
		}
		return
	}
	if lookupErr != nil {
		sw.waiting[dir] = true
		level, kind := log.Error, kindLookupFailed
		if errors.Is(lookupErr, wsm.ErrNotFound) {
			level, kind = log.Warn, kindUnregistered
		}
		i.deferEntry(log, level, path, kind, "no workspace is registered at the entry's directory", lookupErr)
		return
	}
	wlog := i.deps.Log.WorkspaceOrCentral(record.Dir).With(dlog.Context{
		"workspace": string(record.ID), "path": path, "idempotency_key": entry.IdempotencyKey,
	})

	// The submission RE-DRIVES the attempt the client gave up on, so an
	// already-accepted key is the expected answer, not an anomaly.
	submitCtx, cancel := context.WithTimeout(ctx, i.deps.SubmitBound)
	outcome, err := i.deps.Prompts.Submit(prompthandler.WithRedrive(submitCtx), record.ID, entry.said, entry.IdempotencyKey, entry.origin, nil, entry.delivery, nil)
	cancel()
	// THE ANSWER IS RECORDED ONCE THE ENTRY IS GONE (announce, below): a
	// reader that waits on the record finds the ingress already without it.
	var announce func()
	switch {
	case errors.Is(err, prompthandler.ErrDuplicateSubmission):
		announce = func() {
			wlog.Info(opDedupe, "the held prompt was already accepted under its idempotency key, so it is not delivered twice", nil)
		}
	case err != nil:
		sw.waiting[dir] = true
		kind := refusalKind(err)
		level := wlog.Error
		if kind != kindFault {
			level = wlog.Info
		}
		i.deferEntry(wlog, level, path, kind, "the queue did not accept the held prompt; it stays for a later sweep", err)
		leavePending(sw, record.ID, entry, i.retries[path].wait)
		return
	default:
		announce = func() {
			wlog.Info(opIngest, "ingested a held prompt into the queue", dlog.Context{
				"turn":        string(outcome.Turn),
				"recognition": int(outcome.Recognition),
				"delivered":   outcome.Disposition.Delivered,
				"held":        outcome.Disposition.Parked(),
				"origin":      entry.origin.String(),
				"delivery":    entry.delivery.String(),
				"queued_at":   entry.QueuedAt,
			})
		}
	}
	delete(i.retries, path)
	// The queue answered the entry, so the held section stops drawing it now
	// rather than at the end of the sweep.
	i.deps.Pending.DropIngress(record.ID, entry.IdempotencyKey)
	if err := i.deps.Remove(path); err != nil && !errors.Is(err, os.ErrNotExist) {
		// The prompt IS in the queue. The file left behind is resubmitted by
		// the next sweep under the same key and answered as a duplicate, so
		// the failure costs a record, never a second delivery.
		wlog.Error(opRemove, "could not remove an ingested held-prompt entry", dlog.Context{"cause": err.Error()})
	}
	announce()
	i.deps.PublishHost(record.ID)
}

// waitOf is what an entry the queue refused under KIND waits on, as its card
// draws it.
func waitOf(kind string) holds.IngressWait {
	switch kind {
	case kindNoSession:
		return holds.IngressWait{Kind: holds.IngressNoSession}
	case kindMerging:
		return holds.IngressWait{Kind: holds.IngressMerge}
	case kindColdGate:
		return holds.IngressWait{Kind: holds.IngressColdGate}
	case kindMovedAway:
		return holds.IngressWait{Kind: holds.IngressMovedAway}
	case kindFault:
		return holds.IngressWait{Kind: holds.IngressRefused, Detail: "the daemon could not queue this prompt and keeps trying"}
	}
	// The lookup kinds name no workspace, so no card is drawn for them.
	return holds.IngressWait{Kind: holds.IngressAwaitingIntake}
}

// refusalKind names the queue's ANSWER about a standing condition — a merge
// in flight, a cold gate, no session, the workspace's move sealed toward
// another daemon — or kindFault for anything that is not one. Each standing
// condition clears on its own, so the entry simply waits it out.
func refusalKind(err error) string {
	switch {
	case errors.Is(err, promptqueue.ErrMerging):
		return kindMerging
	case errors.Is(err, promptqueue.ErrColdGate):
		return kindColdGate
	case errors.Is(err, promptqueue.ErrNoSession):
		return kindNoSession
	case errors.Is(err, bounce.ErrMovedAway):
		return kindMovedAway
	default:
		return kindFault
	}
}

// deferEntry records that an entry was left for a later sweep, which the next
// change of the fleet wakes (Run). The first deferral of an entry under each
// KIND is recorded at the given level; a repeat of a kind already stated is
// DEBUG, because the standing condition was already said once and a record
// per retry would only repeat it.
func (i *ingress) deferEntry(log dlog.Logger, level func(string, string, dlog.Context), path, kind, message string, cause error) {
	standing := i.retries[path]
	standing.attempts++
	standing.wait = waitOf(kind)
	first := !standing.recorded[kind]
	if first {
		if standing.recorded == nil {
			standing.recorded = map[string]bool{}
		}
		standing.recorded[kind] = true
	}
	i.retries[path] = standing
	fields := dlog.Context{"cause": cause.Error(), "kind": kind, "attempts": standing.attempts}
	if first {
		level(opDefer, message, fields)
		return
	}
	log.Debug(opDefer, message, fields)
}

// quarantine moves a malformed entry aside, where a person can still read what
// was written. It is the ONE way an entry leaves the ingress without its
// prompt reaching the queue, so it is a WARNING naming why.
//
// THE WARNING IS WRITTEN ONLY ONCE THE ENTRY HAS MOVED. It states a completed
// quarantine, so a reader who sees it finds the file under quarantine/; a move
// that fails is an ERROR instead, with the malformation named beside it.
func (i *ingress) quarantine(log dlog.Logger, path string, cause error) {
	fields := dlog.Context{"cause": cause.Error()}
	if err := os.MkdirAll(i.deps.QuarantineDir, 0o755); err != nil {
		log.Error(opQuarantine, "could not create the quarantine directory for a malformed held-prompt entry; it stays for the next sweep",
			dlog.Context{"cause": cause.Error(), "error": err.Error()})
		return
	}
	if err := os.Rename(path, filepath.Join(i.deps.QuarantineDir, filepath.Base(path))); err != nil {
		log.Error(opQuarantine, "could not quarantine a malformed held-prompt entry; it stays for the next sweep",
			dlog.Context{"cause": cause.Error(), "error": err.Error()})
		return
	}
	log.Warn(opQuarantine, "quarantined a malformed held-prompt entry", fields)
}

// Compile-time proof that ingress is an Ingress.
var _ Ingress = (*ingress)(nil)
