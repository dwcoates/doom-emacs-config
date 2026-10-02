package newsdigest

import (
	"context"
	"errors"
	"fmt"
	"sync"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	"google.golang.org/protobuf/proto"

	"claude-repld/internal/dlog"
	"claude-repld/internal/flock"
	"claude-repld/internal/wsm"
)

// The triggers a run records.
const (
	triggerScheduled = "scheduled"
	triggerRefresh   = "refresh"
)

// errNotDue is a scheduled run that found, under the run lock, that the
// cadence is not due: another run (this daemon's refresh, or another
// daemon's) ended since the schedule looked.
var errNotDue = errors.New("newsdigest: no run is due")

// outcome is what a completed run produced.
type outcome struct {
	// items is how many items the digest it made holds; zero when it made
	// none (nothing was new, or nothing new was worth reporting).
	items int
}

// sourceResult is one source's part of a run.
type sourceResult struct {
	src  Source
	news news
	// failure is why the source could not be read; empty when it was.
	failure string
}

// Refresh answers RefreshNewsDigest: a run now, whatever the cadence. An
// error is a failure outside the contract's arms (the store, the lock).
func (d *Digester) Refresh(ctx context.Context) (*agentreplv1.RefreshNewsDigestResponse, error) {
	out, err := d.run(ctx, triggerRefresh, false)
	var modelFailed *ModelFailedError
	switch {
	case err == nil && out.items > 0:
		return refreshSuccess(&agentreplv1.RefreshNewsDigestSuccess{Produced: &agentreplv1.RefreshNewsDigestSuccess_Shown{
			Shown: &agentreplv1.RefreshNewsDigestShown{Items: uint32(out.items)},
		}}), nil
	case err == nil:
		return refreshSuccess(&agentreplv1.RefreshNewsDigestSuccess{Produced: &agentreplv1.RefreshNewsDigestSuccess_NothingNew{
			NothingNew: &agentreplv1.RefreshNewsDigestNothingNew{},
		}}), nil
	case errors.Is(err, ErrAlreadyRunning):
		return refreshError(&agentreplv1.RefreshNewsDigestError{Cause: &agentreplv1.RefreshNewsDigestError_AlreadyRunning{
			AlreadyRunning: &agentreplv1.RefreshNewsDigestAlreadyRunning{},
		}}), nil
	case errors.As(err, &modelFailed):
		return refreshError(&agentreplv1.RefreshNewsDigestError{Cause: &agentreplv1.RefreshNewsDigestError_ModelFailed{
			ModelFailed: &agentreplv1.RefreshNewsDigestModelFailed{Reason: modelFailed.Reason},
		}}), nil
	case errors.Is(err, ErrNoSourceRead):
		return refreshError(&agentreplv1.RefreshNewsDigestError{Cause: &agentreplv1.RefreshNewsDigestError_NoSourceRead{
			NoSourceRead: &agentreplv1.RefreshNewsDigestNoSourceRead{},
		}}), nil
	default:
		return nil, err
	}
}

func refreshSuccess(s *agentreplv1.RefreshNewsDigestSuccess) *agentreplv1.RefreshNewsDigestResponse {
	return &agentreplv1.RefreshNewsDigestResponse{Result: &agentreplv1.RefreshNewsDigestResponse_Success{Success: s}}
}

func refreshError(e *agentreplv1.RefreshNewsDigestError) *agentreplv1.RefreshNewsDigestResponse {
	return &agentreplv1.RefreshNewsDigestResponse{Result: &agentreplv1.RefreshNewsDigestResponse_Error{Error: e}}
}

// run makes one digest: read every source, keep what is new, condense it,
// record the run, and stand the digest. onlyIfDue makes it a scheduled run,
// which re-reads the cadence under the run lock and answers errNotDue when
// another run ended since the schedule looked.
func (d *Digester) run(ctx context.Context, trigger string, onlyIfDue bool) (outcome, error) {
	log := d.deps.Log.With(dlog.Context{"trigger": trigger})
	if !d.running.TryLock() {
		log.Info(opRun, "a news digest run is already in progress in this daemon", nil)
		return outcome{}, ErrAlreadyRunning
	}
	defer d.running.Unlock()
	lock, held, err := flock.TryExclusive(d.deps.LockPath)
	if err != nil {
		log.Error(opRun, "the news digest run lock could not be taken", dlog.Context{"lock": d.deps.LockPath, "cause": err.Error()})
		return outcome{}, fmt.Errorf("newsdigest: %w", err)
	}
	if !held {
		log.Info(opRun, "another daemon is running a news digest", dlog.Context{"lock": d.deps.LockPath})
		return outcome{}, ErrAlreadyRunning
	}
	defer func() {
		if err := lock.Release(); err != nil {
			log.Error(opRun, "the news digest run lock could not be released", dlog.Context{"lock": d.deps.LockPath, "cause": err.Error()})
		}
	}()

	state, err := d.deps.Store.NewsDigestState(ctx)
	if err != nil {
		return outcome{}, fmt.Errorf("newsdigest: read the digest state: %w", err)
	}
	started := d.deps.Clock.Now()
	if onlyIfDue && !state.LastRunEnd.IsZero() && started.Before(state.LastRunEnd.Add(d.deps.Every)) {
		log.Info(opRun, "a news digest run ended since the schedule looked; this one is not due", dlog.Context{
			"last_run_end": state.LastRunEnd.Format(time.RFC3339),
		})
		return outcome{}, errNotDue
	}
	since := state.Baseline
	if since.IsZero() {
		since = started.Add(-d.deps.Every)
	}
	log.Info(opRun, "a news digest run started", dlog.Context{
		"since": since.Format(time.RFC3339), "sources": len(d.deps.Sources),
	})

	results := d.readAll(ctx, state, since, log)
	if err := ctx.Err(); err != nil {
		log.Info(opRun, "the news digest run ended with the daemon's stand-down", dlog.Context{"cause": err.Error()})
		return outcome{}, err
	}
	sources, snapshots, material, newEntries, failed := tally(results)
	if failed == len(results) {
		log.Error(opRun, "no news digest source could be read", dlog.Context{"sources": len(results)})
		if err := d.recordLocked(ctx, wsm.NewsDigestRun{EndedAt: d.deps.Clock.Now()}, nil); err != nil {
			return outcome{}, err
		}
		return outcome{}, ErrNoSourceRead
	}
	if newEntries == 0 {
		if err := d.recordLocked(ctx, wsm.NewsDigestRun{EndedAt: d.deps.Clock.Now(), Recorded: true, Snapshots: snapshots}, nil); err != nil {
			return outcome{}, err
		}
		log.Info(opRun, "the news digest run found nothing new", dlog.Context{"sources_failed": failed})
		return outcome{}, nil
	}

	carried, err := decodeStanding(state.Standing)
	if err != nil {
		log.Error(opRun, "the standing news digest did not decode", dlog.Context{"digest": state.LatestID, "cause": err.Error()})
		return outcome{}, err
	}
	period := since.Local().Format(time.RFC1123) + " to " + started.Local().Format(time.RFC1123)
	sections, err := d.condenser.condense(ctx, period, material, carried.GetSections())
	if err != nil {
		if ctxErr := ctx.Err(); ctxErr != nil {
			log.Info(opRun, "the news digest run ended with the daemon's stand-down", dlog.Context{"cause": ctxErr.Error()})
			return outcome{}, ctxErr
		}
		log.Error(opModel, "the model could not condense the news digest", dlog.Context{
			"model": Model, "new_entries": newEntries, "cause": err.Error(),
		})
		if recErr := d.recordLocked(ctx, wsm.NewsDigestRun{EndedAt: d.deps.Clock.Now()}, nil); recErr != nil {
			return outcome{}, recErr
		}
		return outcome{}, err
	}
	ended := d.deps.Clock.Now()
	if len(sections) == 0 {
		if err := d.recordLocked(ctx, wsm.NewsDigestRun{EndedAt: ended, Recorded: true, Snapshots: snapshots}, nil); err != nil {
			return outcome{}, err
		}
		log.Info(opRun, "the model found nothing new worth reporting", dlog.Context{"new_entries": newEntries})
		return outcome{}, nil
	}

	id := d.deps.MintID()
	from := since
	if carried != nil {
		from = time.UnixMilli(carried.GetHeader().GetPeriod().GetFromMs())
	}
	overlay := &frontendv1.NewsDigestOverlay{
		Id: &frontendv1.NewsDigestId{Value: id},
		Header: &frontendv1.NewsDigestHeader{
			Title:  &frontendv1.NewsDigestTitle{Text: "Claude news · " + ended.Local().Format("Jan 2")},
			Period: &frontendv1.NewsDigestPeriod{FromMs: from.UnixMilli(), ToMs: ended.UnixMilli()},
		},
		Sections: sections,
		Sources:  &frontendv1.NewsDigestSources{Sources: sources},
	}
	encoded, err := proto.Marshal(overlay)
	if err != nil {
		return outcome{}, fmt.Errorf("newsdigest: encode the overlay: %w", err)
	}
	items := 0
	for _, s := range sections {
		items += len(s.GetItems())
	}
	run := wsm.NewsDigestRun{EndedAt: ended, Recorded: true, Snapshots: snapshots,
		Digest: &wsm.NewsDigestMinted{ID: id, Overlay: encoded}}
	if err := d.recordLocked(ctx, run, overlay); err != nil {
		return outcome{}, err
	}
	log.Info(opRun, "made a news digest; it stands in every webview", dlog.Context{
		"digest": id, "sections": len(sections), "items": items, "new_entries": newEntries,
		"sources_failed": failed, "carried": carried != nil, "duration_ms": ended.Sub(started).Milliseconds(),
	})
	return outcome{items: items}, nil
}

// recordLocked records run and, when it made a digest, publishes it, both
// under standingMu so a dismiss cannot land between them.
func (d *Digester) recordLocked(ctx context.Context, run wsm.NewsDigestRun, overlay *frontendv1.NewsDigestOverlay) error {
	d.standingMu.Lock()
	defer d.standingMu.Unlock()
	if err := d.deps.Store.RecordNewsDigestRun(ctx, run); err != nil {
		return fmt.Errorf("newsdigest: record the run: %w", err)
	}
	if overlay != nil {
		d.publishLocked(overlay)
	}
	return nil
}

// readAll reads every source at once and answers each one's result in the
// sources' order.
func (d *Digester) readAll(ctx context.Context, state wsm.NewsDigestState, since time.Time, log dlog.Logger) []sourceResult {
	results := make([]sourceResult, len(d.deps.Sources))
	var wg sync.WaitGroup
	for i, src := range d.deps.Sources {
		wg.Add(1)
		go func() {
			defer wg.Done()
			results[i] = d.readOne(ctx, src, state, since, log)
		}()
	}
	wg.Wait()
	return results
}

// readOne fetches, parses and diffs one source. A failure is the source's
// alone: it is recorded and the digest goes on without it.
func (d *Digester) readOne(ctx context.Context, src Source, state wsm.NewsDigestState, since time.Time, log dlog.Logger) sourceResult {
	log = log.With(dlog.Context{"source": src.Key, "url": src.URL, "format": src.Format.String()})
	fail := func(step string, err error) sourceResult {
		reason := step + ": " + err.Error()
		log.Info(opSource, "a news digest source could not be read; the digest goes on without it",
			dlog.Context{"cause": reason})
		return sourceResult{src: src, failure: reason}
	}
	body, err := d.deps.Fetcher.Fetch(ctx, src.URL)
	if err != nil {
		return fail("fetch", err)
	}
	r, err := parse(src, body)
	if err != nil {
		return fail("parse", err)
	}
	prior, had := state.Snapshots[src.Key]
	n, err := diff(src, r, prior, had, since)
	if err != nil {
		return fail("compare", err)
	}
	log.Debug(opSource, "read a news digest source", dlog.Context{"new_entries": n.count, "first_reading": !had})
	return sourceResult{src: src, news: n}
}

// tally folds the per-source results into the overlay's source rows, the
// snapshots to record, the material to condense, the new-entry total and the
// failed-source count.
func tally(results []sourceResult) ([]*frontendv1.NewsDigestSource, map[string]string, []sourceNews, int, int) {
	var (
		rows      []*frontendv1.NewsDigestSource
		snapshots = map[string]string{}
		material  []sourceNews
		total     int
		failed    int
	)
	for _, r := range results {
		row := &frontendv1.NewsDigestSource{Name: r.src.Name, Url: r.src.Home}
		if r.failure != "" {
			failed++
			row.Outcome = &frontendv1.NewsDigestSource_Failed{Failed: &frontendv1.NewsDigestSourceFailed{Reason: r.failure}}
			rows = append(rows, row)
			continue
		}
		row.Outcome = &frontendv1.NewsDigestSource_Read{Read: &frontendv1.NewsDigestSourceRead{NewEntries: uint32(r.news.count)}}
		rows = append(rows, row)
		snapshots[r.src.Key] = r.news.snapshot
		total += r.news.count
		if len(r.news.entries) > 0 {
			material = append(material, sourceNews{src: r.src, entries: r.news.entries})
		}
	}
	return rows, snapshots, material, total, failed
}
