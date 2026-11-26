package logsites

import (
	"context"
	"encoding/json"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"testing"
	"time"

	"agentrepl/logging/logsite"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// t0 is the instant every fixture's clock starts at.
var t0 = time.Date(2026, 11, 21, 10, 0, 0, 0, time.UTC)

// fakeStore is the real in-memory store with any method replaced.
type fakeStore struct {
	Store
	snapshot func(ctx context.Context, now func() time.Time) (wsm.LogSiteBoundaries, time.Time, error)
	record   func(ctx context.Context, now func() time.Time) (time.Time, error)
	tally    func(ctx context.Context, c wsm.LogSiteChunk) (bool, error)
}

func (f *fakeStore) LogSiteSnapshot(ctx context.Context, now func() time.Time) (wsm.LogSiteBoundaries, time.Time, error) {
	if f.snapshot != nil {
		return f.snapshot(ctx, now)
	}
	return f.Store.LogSiteSnapshot(ctx, now)
}

func (f *fakeStore) RecordLogSiteDeploy(ctx context.Context, now func() time.Time) (time.Time, error) {
	if f.record != nil {
		return f.record(ctx, now)
	}
	return f.Store.RecordLogSiteDeploy(ctx, now)
}

// RecordLogSiteMark records a mark in the real store, as the orchestrator
// and the merge ledger do; the tally itself records none.
func (f *fakeStore) RecordLogSiteMark(ctx context.Context, now func() time.Time) (time.Time, error) {
	return f.Store.(wsm.DB).RecordLogSiteMark(ctx, now)
}

func (f *fakeStore) TallyLogSites(ctx context.Context, c wsm.LogSiteChunk) (bool, error) {
	if f.tally != nil {
		return f.tally(ctx, c)
	}
	return f.Store.TallyLogSites(ctx, c)
}

// fixture is a Tallier over one state logs directory and a real store.
type fixture struct {
	dir   string
	store *fakeStore
	log   *dlog.TestLogger
	now   time.Time
	t     *Tallier
}

// newFixture builds a fixture whose clock answers f.now.
func newFixture(t *testing.T) *fixture {
	t.Helper()
	db, err := wsm.OpenInMemory(context.Background(), wsm.WithLogger(dlog.Nowhere()))
	if err != nil {
		t.Fatalf("OpenInMemory: %v", err)
	}
	t.Cleanup(func() { db.Close() })
	f := &fixture{dir: t.TempDir(), store: &fakeStore{Store: db}, log: dlog.NewTestLogger(), now: t0.Add(time.Hour)}
	f.t = New(Options{Store: f.store, Log: f.log, Dirs: []Dir{StateLogs(f.dir)}, Now: func() time.Time { return f.now }, PassBound: time.Minute})
	return f
}

// path is the fixture's file NAME.
func (f *fixture) path(name string) string { return filepath.Join(f.dir, name) }

// write appends TEXT to the fixture's file NAME.
func (f *fixture) write(t *testing.T, name, text string) {
	t.Helper()
	file, err := os.OpenFile(f.path(name), os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0o644)
	if err != nil {
		t.Fatal(err)
	}
	defer file.Close()
	if _, err := file.WriteString(text); err != nil {
		t.Fatal(err)
	}
}

// pass runs one pass, failing the test on an error.
func (f *fixture) pass(t *testing.T) Report {
	t.Helper()
	r, err := f.t.Pass(context.Background(), "test")
	if err != nil {
		t.Fatalf("Pass: %v", err)
	}
	return r
}

// rows answers every kept row.
func (f *fixture) rows(t *testing.T) []wsm.LogSiteRow {
	t.Helper()
	rows, err := f.store.LogSiteRows(context.Background(), time.Unix(0, 0), t0.Add(1000*time.Hour), wsm.LogSiteLevels)
	if err != nil {
		t.Fatalf("LogSiteRows: %v", err)
	}
	return rows
}

// keyed is one keyed record's JSONL line, written at AT by the function F
// with TEMPLATE rendered as MESSAGE.
func keyed(at time.Time, level, function, template, message string) string {
	rec := map[string]any{
		"timestamp": at.Format(time.RFC3339Nano), "runtime": "daemon", "level": level, "verbosity": "normal",
		"operation": "daemon.test", "message": message, "site": logsite.Key("daemon", function, template),
		"template": template, "context": map[string]any{"what": "x"},
	}
	raw, err := json.Marshal(rec)
	if err != nil {
		panic(err)
	}
	return string(raw) + "\n"
}

// unkeyed is one record's JSONL line that names no site.
func unkeyed(at time.Time) string {
	return `{"timestamp":"` + at.Format(time.RFC3339Nano) + `","runtime":"daemon","level":"info","operation":"daemon.old","message":"said"}` + "\n"
}

// counts answers each row's site and count, as "site:count" in row order.
func counts(rows []wsm.LogSiteRow) []string {
	var out []string
	for _, r := range rows {
		out = append(out, r.Site+":"+strconv.FormatInt(r.Count, 10))
	}
	return out
}

// sorted answers ROWS sorted, as counts answers rows in key order.
func sorted(rows ...string) []string {
	sort.Strings(rows)
	return rows
}

// recorded reports whether LOG holds a record at LEVEL with MESSAGE.
func recorded(log *dlog.TestLogger, level, message string) bool {
	_, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: level, Operation: opPass, Message: message})
	return ok
}
