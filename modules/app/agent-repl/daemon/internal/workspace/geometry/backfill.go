package geometry

import (
	"context"
	"fmt"

	"claude-repld/internal/dlog"
)

// WorkspaceLister enumerates the workspaces the daemon knows about at boot.
// The session registry satisfies it (its records ARE the workspace roster);
// declaring it here keeps this package free of a registry dependency.
type WorkspaceLister interface {
	// GeometryBackfillCandidates returns every workspace key the daemon still
	// serves. Terminal sessions are the lister's business to exclude.
	GeometryBackfillCandidates() []string
}

// Backfiller gives pre-cutover workspaces a geometry record at boot.
//
// A workspace created before the daemon owned the map has no record, and every
// merge of one would otherwise be refused forever. Deriving the coordinates
// from git once, at boot, is a repair of missing state — NOT a fallback for the
// recording path: nothing here runs when a record already exists, and a
// workspace whose geometry cannot be derived stays unrecorded so its merge is
// refused loudly rather than run against a guess.
type Backfiller struct {
	store   *Store
	deriver *Deriver
	lister  WorkspaceLister
	logf    dlog.Logf
}

// BackfillConfig collects the backfiller's collaborators. All are required.
type BackfillConfig struct {
	Store   *Store
	Deriver *Deriver
	Lister  WorkspaceLister
	Logf    dlog.Logf
}

// NewBackfiller validates its dependencies and returns the backfiller.
func NewBackfiller(cfg BackfillConfig) (*Backfiller, error) {
	switch {
	case cfg.Store == nil:
		return nil, fmt.Errorf("geometry: Backfiller needs a Store")
	case cfg.Deriver == nil:
		return nil, fmt.Errorf("geometry: Backfiller needs a Deriver")
	case cfg.Lister == nil:
		return nil, fmt.Errorf("geometry: Backfiller needs a WorkspaceLister")
	case cfg.Logf == nil:
		return nil, fmt.Errorf("geometry: Backfiller needs a Logf")
	}
	return &Backfiller{store: cfg.Store, deriver: cfg.Deriver, lister: cfg.Lister, logf: cfg.Logf}, nil
}

// BackfillReport is what one boot pass did, for the log and for tests.
type BackfillReport struct {
	// Recorded is the number of workspaces that gained a derived record.
	Recorded int
	// AlreadyRecorded is the number that already had one and were left alone.
	AlreadyRecorded int
	// Underivable is the number whose geometry git could not answer for. Each
	// one is logged; each one's merge will be refused with an explanation.
	Underivable int
}

// Run performs one backfill pass.
//
// A workspace whose derivation fails is CONTAINED: it is logged loudly and the
// pass continues, because one detached-HEAD or deleted worktree must not stop
// the daemon from repairing every other workspace. A STORE failure is
// structural and aborts the pass — a store that cannot be written is not a
// condition the next workspace will do better against.
func (b *Backfiller) Run(ctx context.Context) (BackfillReport, error) {
	candidates := b.lister.GeometryBackfillCandidates()
	report := BackfillReport{}
	b.logf("geometry: backfill START candidates=%d", len(candidates))
	for _, workspace := range candidates {
		key := Key(workspace)
		if key == "" {
			b.logf("geometry: backfill SKIPPED an empty workspace key")
			report.Underivable++
			continue
		}
		_, found, err := b.store.Lookup(ctx, key)
		if err != nil {
			return report, fmt.Errorf("geometry: backfill lookup %s: %w", key, err)
		}
		if found {
			report.AlreadyRecorded++
			continue
		}
		rec, err := b.deriver.Derive(ctx, key)
		if err != nil {
			b.logf("geometry: backfill UNDERIVABLE {workspace=%s} — the workspace keeps NO geometry record and its merge will be refused: %v", key, err)
			report.Underivable++
			continue
		}
		if err := b.store.Record(ctx, rec); err != nil {
			return report, fmt.Errorf("geometry: backfill record %s: %w", key, err)
		}
		report.Recorded++
	}
	b.logf("geometry: backfill DONE candidates=%d recorded=%d already=%d underivable=%d",
		len(candidates), report.Recorded, report.AlreadyRecorded, report.Underivable)
	return report, nil
}
