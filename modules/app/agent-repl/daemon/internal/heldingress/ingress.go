package heldingress

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"sync"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/prompthandler"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/wsm"
)

// The operation names this package's records carry.
const (
	opRun        = "daemon.heldingress.run"
	opIngest     = "daemon.heldingress.ingest"
	opDedupe     = "daemon.heldingress.dedupe"
	opDefer      = "daemon.heldingress.defer"
	opQuarantine = "daemon.heldingress.quarantine"
	opRemove     = "daemon.heldingress.remove"
)

// retry is one refused entry's standing: how often it was refused, and the
// earliest instant it is submitted again.
type retry struct {
	attempts int
	next     time.Time
}

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
}

func removeFile(path string) error { return os.Remove(path) }

// Run sweeps until ctx is cancelled. The first sweep runs before the first
// tick, so everything written while no daemon was serving is ingested as soon
// as this one starts.
func (i *ingress) Run(ctx context.Context) error {
	log := i.deps.Log.Global().With(dlog.Context{"dir": i.deps.Dir})
	ticker := time.NewTicker(i.deps.Interval)
	defer ticker.Stop()
	log.Info(opRun, "watching the held-prompt ingress", dlog.Context{
		"glob": Glob, "interval_ms": i.deps.Interval.Milliseconds(),
	})
	for {
		if err := i.Sweep(ctx); err != nil && ctx.Err() == nil {
			// A sweep that fails is logged and retried: nothing is removed
			// on a failed sweep, so the next one meets the same entries.
			log.Error(opRun, "a sweep of the held-prompt ingress failed", dlog.Context{"cause": err.Error()})
		}
		select {
		case <-ctx.Done():
			log.Info(opRun, "stopped watching the held-prompt ingress", nil)
			return ctx.Err()
		case <-ticker.C:
		}
	}
}

// Sweep ingests every entry once, in name order.
//
// ORDER IS KEPT PER WORKSPACE: once one workspace's entry is left for a later
// sweep, every later entry for that workspace is left too, so a prompt is
// never delivered ahead of one its writer wrote before it.
func (i *ingress) Sweep(ctx context.Context) error {
	i.mu.Lock()
	defer i.mu.Unlock()
	matches, err := filepath.Glob(filepath.Join(i.deps.Dir, Glob))
	if err != nil {
		return fmt.Errorf("glob %q: %w", Glob, err)
	}
	sort.Strings(matches)
	present := make(map[string]bool, len(matches))
	waiting := map[string]bool{}
	for _, path := range matches {
		if err := ctx.Err(); err != nil {
			return err
		}
		present[path] = true
		i.ingest(ctx, path, waiting)
	}
	for path := range i.retries {
		if !present[path] {
			delete(i.retries, path)
		}
	}
	return nil
}

// ingest handles one entry file. waiting names the project directories with an
// entry left for a later sweep in this one.
func (i *ingress) ingest(ctx context.Context, path string, waiting map[string]bool) {
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
	if waiting[dir] {
		log.Debug(opDefer, "an earlier entry for this workspace is still waiting, so this one waits behind it", nil)
		return
	}
	if standing, ok := i.retries[path]; ok && i.deps.Now().Before(standing.next) {
		waiting[dir] = true
		log.Debug(opDefer, "the entry's retry is not due yet", dlog.Context{"next": standing.next.Format(time.RFC3339Nano)})
		return
	}

	record, err := i.deps.WorkspaceByDir(ctx, dir)
	if err != nil {
		waiting[dir] = true
		level := log.Error
		if errors.Is(err, wsm.ErrNotFound) {
			level = log.Warn
		}
		i.deferEntry(log, level, path, "no workspace is registered at the entry's directory", err)
		return
	}
	wlog := i.deps.Log.WorkspaceOrCentral(record.Dir).With(dlog.Context{
		"workspace": string(record.ID), "path": path, "idempotency_key": entry.IdempotencyKey,
	})

	// The submission RE-DRIVES the attempt the client gave up on, so an
	// already-accepted key is the expected answer, not an anomaly.
	outcome, err := i.deps.Prompts.Submit(prompthandler.WithRedrive(ctx), record.ID, entry.said, entry.IdempotencyKey, entry.origin, nil)
	switch {
	case errors.Is(err, prompthandler.ErrDuplicateSubmission):
		wlog.Info(opDedupe, "the held prompt was already accepted under its idempotency key, so it is not delivered twice", nil)
	case err != nil:
		waiting[dir] = true
		level := wlog.Error
		if refusal(err) {
			level = wlog.Info
		}
		i.deferEntry(wlog, level, path, "the queue did not accept the held prompt; it stays for a later sweep", err)
		return
	default:
		wlog.Info(opIngest, "ingested a held prompt into the queue", dlog.Context{
			"turn":        string(outcome.Turn),
			"recognition": int(outcome.Recognition),
			"delivered":   outcome.Disposition.Delivered,
			"held":        outcome.Disposition.Parked(),
			"origin":      entry.origin.String(),
			"queued_at":   entry.QueuedAt,
		})
	}
	delete(i.retries, path)
	if err := i.deps.Remove(path); err != nil && !errors.Is(err, os.ErrNotExist) {
		// The prompt IS in the queue. The file left behind is resubmitted by
		// the next sweep under the same key and answered as a duplicate, so
		// the failure costs a record, never a second delivery.
		wlog.Error(opRemove, "could not remove an ingested held-prompt entry", dlog.Context{"cause": err.Error()})
	}
	i.deps.PublishHost(record.ID)
}

// refusal reports whether err is one of the queue's ANSWERS about a standing
// condition — a merge in flight, a cold gate, no session — rather than a
// fault. Each clears on its own, so the entry simply waits it out.
func refusal(err error) bool {
	return errors.Is(err, promptqueue.ErrMerging) ||
		errors.Is(err, promptqueue.ErrColdGate) ||
		errors.Is(err, promptqueue.ErrNoSession)
}

// deferEntry records that an entry was left for a later sweep and schedules
// its retry, doubling the delay up to the ceiling. The first deferral of an
// entry is recorded at the given level; a repeat is DEBUG, because the
// standing condition was already stated once and a record per retry would
// only repeat it.
func (i *ingress) deferEntry(log dlog.Logger, level func(string, string, dlog.Context), path, message string, cause error) {
	standing := i.retries[path]
	standing.attempts++
	delay := i.deps.Interval
	for n := 1; n < standing.attempts && delay < i.deps.RetryCeiling; n++ {
		delay *= 2
	}
	if delay > i.deps.RetryCeiling {
		delay = i.deps.RetryCeiling
	}
	standing.next = i.deps.Now().Add(delay)
	i.retries[path] = standing
	fields := dlog.Context{"cause": cause.Error(), "attempts": standing.attempts, "retry_in_ms": delay.Milliseconds()}
	if standing.attempts == 1 {
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
