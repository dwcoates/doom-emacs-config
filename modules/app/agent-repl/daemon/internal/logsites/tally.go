// Package logsites tallies every log record of every runtime's sinks into
// the daemon's per-deploy record of the log sites that fired, and answers a
// window of time from it (logging-contract.md "Log sites",
// agentrepl.v1.ListLogSites).
//
// ONE PASS READS EVERY SINK FROM ITS OWN CURSOR, to the end of its last
// whole line, and adds what it read to the (deploy, site, level, bucket) rows
// (wsm logsites.go): a bucket is an hour, split at every mark inside it. It
// runs:
//
//   - at the start of the daemon's hourly retention pass, BEFORE anything is
//     pruned, so every file the daemon removes was read to its end first
//     (dlog.Surfaces.BeforeRetention); the hourly cadence is the retention
//     pass's own, and also bounds how far the files Emacs, the store and the
//     sidecar age themselves (three days, logging.DefaultRetention) can get
//     ahead of it;
//   - at each deploy's start, so the deploy before it is read whole soon;
//   - before every window is answered, so an answer is current to the
//     moment it is asked.
//
// A RECORD IS CREDITED TO THE DEPLOY SERVING AT ITS TIMESTAMP, in the bucket
// that holds it. A pass snapshots the deploys, the marks and an instant in
// one transaction, and tallies only the records written before that instant;
// a deploy or mark recorded later is later (wsm.DB.LogSiteSnapshot), so no
// record is credited to a deploy that did not serve it or to a bucket on the
// wrong side of a mark. A record at or after the instant is left for the next
// pass, its file's cursor standing before it.
package logsites

import (
	"bufio"
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"sort"
	"sync"
	"time"

	"agentrepl/logging/logsite"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

const opPass = "daemon.logsites.pass"

// Store is the part of the daemon's store the tally keeps its rows in.
type Store interface {
	LogSiteSnapshot(ctx context.Context, now func() time.Time) (wsm.LogSiteBoundaries, time.Time, error)
	RecordLogSiteDeploy(ctx context.Context, now func() time.Time) (time.Time, error)
	PruneLogSiteDeploys(ctx context.Context, keep int) (int64, error)
	LogSiteCursor(ctx context.Context, file string) (int64, bool, error)
	TallyLogSites(ctx context.Context, c wsm.LogSiteChunk) (bool, error)
	ForgetLogSiteCursors(ctx context.Context, live []string) (int64, error)
	LogSiteRows(ctx context.Context, from, to time.Time, levels []string) ([]wsm.LogSiteRow, error)
	OldestLogSiteHour(ctx context.Context) (time.Time, bool, error)
	LogSiteEdgeAt(ctx context.Context, at time.Time) (wsm.LogSiteEdge, error)
}

const (
	// KeepDeploys is how many deploys of rows the store keeps: the deploy
	// serving now and the two before it, so a check can compare what fired
	// since the last deploy with the deploy before (ListLogSites keeps at
	// least three). Rows an undecided production span needs are never
	// pruned, however many deploys ago its span began
	// (wsm.DB.PruneLogSiteDeploys).
	KeepDeploys = 3
	// ChunkBytes bounds how much of one file one transaction tallies, so a
	// first read of a large file never holds the store's one connection for
	// the whole of it, and never holds more than one chunk's rows in memory.
	ChunkBytes = 8 << 20
	// PassBound bounds one pass: 2.7 times the measured worst case, a first
	// pass decoding every line of three days of logs (7.7 GB in 1450 files,
	// 37.1 s on the owner's host under its usual load, 2026-11-21). A pass
	// after it reads only what was written since (30 ms on the same host).
	PassBound = 100 * time.Second
	// SampleContextBytes bounds a row's sample context: a record's context
	// is whatever its call had in hand, and a row keeps only the latest one.
	// A larger context is kept as how large it was.
	SampleContextBytes = 8 << 10
)

// Tallier runs the passes and answers windows.
type Tallier struct {
	store Store
	log   dlog.Logger
	dirs  []Dir
	now   func() time.Time
	// passBound bounds one pass (Options.PassBound).
	passBound time.Duration

	// mu serializes this daemon's passes: a window's pass waits for the
	// hourly one rather than reading the same files beside it. Two daemons'
	// passes are kept apart by each chunk's cursor check instead.
	mu sync.Mutex
}

// Options are a Tallier's collaborators.
type Options struct {
	Store Store
	Log   dlog.Logger
	// Dirs are the directories whose log files are read.
	Dirs []Dir
	// Now is the clock; nil means time.Now.
	Now func() time.Time
	// PassBound bounds one pass, which a pass that runs past it ends with a
	// failure. Required.
	PassBound time.Duration
}

// New answers a Tallier.
func New(o Options) *Tallier {
	if o.Store == nil || o.Log == nil {
		panic("logsites: New needs a store and a logger")
	}
	if o.PassBound <= 0 {
		panic("logsites: New needs a pass bound")
	}
	now := o.Now
	if now == nil {
		now = time.Now
	}
	return &Tallier{store: o.Store, log: o.Log, dirs: o.Dirs, now: now, passBound: o.PassBound}
}

// Report is what one pass did.
type Report struct {
	Files int
	// Bytes is how many bytes were tallied.
	Bytes int64
	// Records is how many records were credited to a site.
	Records int64
	// Unkeyed is how many records named no site: a record written before its
	// runtime keyed its sites, or by a runtime outside agent-repl.
	Unkeyed int64
	// Earlier is how many records predate every kept deploy.
	Earlier int64
	// Malformed is how many lines naming a site were not a record of the
	// contract's shape. A line that names no site is counted Unkeyed, whatever
	// its shape: it has no site to credit.
	Malformed int64
	// Moved is how many files another pass had tallied first.
	Moved int
	// Failed is how many files could not be read or tallied.
	Failed int
	// PrunedRows is how many rows of deploys no longer kept were dropped.
	PrunedRows int64
}

// Pass reads every sink from its cursor and tallies what it read. REASON
// names why the pass runs, for its record. A file that cannot be read or
// tallied is recorded at ERROR and fails the pass once every other file was
// read.
func (t *Tallier) Pass(ctx context.Context, reason string) (Report, error) {
	t.mu.Lock()
	defer t.mu.Unlock()
	ctx, cancel := context.WithTimeout(ctx, t.passBound)
	defer cancel()
	started := t.now()
	report, err := t.pass(ctx, reason)
	fields := dlog.Context{
		"reason": reason, "files": report.Files, "bytes": report.Bytes, "records": report.Records,
		"unkeyed": report.Unkeyed, "earlier": report.Earlier, "malformed": report.Malformed,
		"moved": report.Moved, "failed": report.Failed, "pruned_rows": report.PrunedRows,
		"elapsed_ms": t.now().Sub(started).Milliseconds(),
	}
	if errors.Is(err, context.DeadlineExceeded) && ctx.Err() != nil {
		t.log.With(fields).Error(opPass, "the log site pass ran past its bound of {bound}", dlog.Context{
			"bound": t.passBound.String(), "cause": err.Error(),
		})
		return report, err
	}
	if err != nil {
		t.log.With(fields).Error(opPass, "the log site pass could not tally every sink", dlog.Context{"cause": err.Error()})
		return report, err
	}
	t.log.Info(opPass, "tallied the log sites of every sink", fields)
	return report, nil
}

func (t *Tallier) pass(ctx context.Context, reason string) (Report, error) {
	var report Report
	bounds, until, err := t.store.LogSiteSnapshot(ctx, t.now)
	if err != nil {
		return report, fmt.Errorf("logsites: snapshot the deploys and marks: %w", err)
	}
	sources, err := listSources(t.dirs)
	if err != nil {
		return report, err
	}
	report.Files = len(sources)
	var (
		live     []string
		failures []error
	)
	for _, src := range sources {
		if err := ctx.Err(); err != nil {
			return report, err
		}
		id, err := identityOf(src.path, src.info)
		if err != nil {
			failures = append(failures, t.fileFailed(src.path, reason, err))
			report.Failed++
			continue
		}
		live = append(live, id)
		if err := t.tallyFile(ctx, src, id, bounds, until, &report); err != nil {
			if ctx.Err() != nil {
				return report, err
			}
			failures = append(failures, t.fileFailed(src.path, reason, err))
			report.Failed++
		}
	}
	if _, err := t.store.ForgetLogSiteCursors(ctx, live); err != nil {
		failures = append(failures, fmt.Errorf("logsites: forget the cursors of gone files: %w", err))
	}
	pruned, err := t.store.PruneLogSiteDeploys(ctx, KeepDeploys)
	if err != nil {
		failures = append(failures, fmt.Errorf("logsites: prune deploys no longer kept: %w", err))
	}
	report.PrunedRows = pruned
	return report, errors.Join(failures...)
}

// fileFailed records one file the pass could not tally, and answers the
// error it fails the pass with.
func (t *Tallier) fileFailed(path, reason string, err error) error {
	t.log.Error(opPass, "could not tally the log file {path}", dlog.Context{"path": path, "reason": reason, "cause": err.Error()})
	return fmt.Errorf("logsites: %s: %w", path, err)
}

// tallyFile tallies SRC from its cursor, one chunk per transaction, to the
// end of its last whole line or its first record at or after UNTIL.
func (t *Tallier) tallyFile(ctx context.Context, src source, id string, bounds wsm.LogSiteBoundaries, until time.Time, report *Report) error {
	offset, _, err := t.store.LogSiteCursor(ctx, id)
	if err != nil {
		return err
	}
	if size := src.info.Size(); size < offset {
		// A log file only grows: no runtime truncates one (logging-contract.md
		// "Persistence layout"). One that shrank under its cursor is a defect
		// of its writer.
		return fmt.Errorf("the file is %d bytes, shorter than its cursor at %d: a log file only grows", size, offset)
	}
	if src.info.Size() == offset {
		return nil
	}
	f, err := os.Open(src.path)
	if errors.Is(err, fs.ErrNotExist) {
		// Pruned since it was listed: every byte of it was tallied by the
		// pass before its pruner removed it, or it was aged by its own
		// runtime past the hourly pass's reach.
		return nil
	}
	if err != nil {
		return err
	}
	defer f.Close()
	if _, err := f.Seek(offset, io.SeekStart); err != nil {
		return err
	}
	r := bufio.NewReaderSize(f, readBufferBytes)
	malformedAt := int64(-1)
	var malformed int64
	for {
		chunk, done, err := readChunk(ctx, r, offset, bounds, until, report, &malformed, &malformedAt)
		if err != nil {
			return err
		}
		if chunk.to > offset {
			applied, err := t.store.TallyLogSites(ctx, wsm.LogSiteChunk{
				File: id, Path: src.path, From: offset, To: chunk.to, Counts: chunk.counts(), At: t.now(),
			})
			if err != nil {
				return err
			}
			if !applied {
				// Another daemon's pass tallied these bytes first; its count
				// stands, and this pass reads the file again next time from
				// where that one left it.
				report.Moved++
				return nil
			}
			report.Bytes += chunk.to - offset
			report.Records += chunk.records
			offset = chunk.to
		}
		if done {
			break
		}
	}
	report.Malformed += malformed
	if malformed > 0 {
		t.log.Error(opPass, "the log file {path} holds lines that are no record of the logging contract", dlog.Context{
			"path": src.path, "lines": malformed, "first_offset": malformedAt,
		})
	}
	return nil
}

// countKey is one row a chunk adds to.
type countKey struct {
	deploy time.Time
	site   string
	level  string
	bucket time.Time
}

// chunkTally is what one chunk read.
type chunkTally struct {
	to      int64
	records int64
	rows    map[countKey]*wsm.LogSiteCount
}

// counts answers the chunk's rows in a stable order.
func (c chunkTally) counts() []wsm.LogSiteCount {
	out := make([]wsm.LogSiteCount, 0, len(c.rows))
	for _, n := range c.rows {
		out = append(out, *n)
	}
	sort.Slice(out, func(i, j int) bool {
		a, b := out[i], out[j]
		switch {
		case !a.Deploy.Equal(b.Deploy):
			return a.Deploy.Before(b.Deploy)
		case a.Site != b.Site:
			return a.Site < b.Site
		case a.Level != b.Level:
			return a.Level < b.Level
		default:
			return a.Bucket.Before(b.Bucket)
		}
	})
	return out
}

// readBufferBytes is the read buffer of a file's pass; a longer record is
// read whole past it (readChunk).
const readBufferBytes = 64 << 10

// readChunk reads whole lines from R, which stands at FROM, until a chunk's
// worth, the end of its last whole line, or a record at or after UNTIL; DONE
// reports that the file holds nothing more for this pass.
func readChunk(ctx context.Context, r *bufio.Reader, from int64, bounds wsm.LogSiteBoundaries, until time.Time,
	report *Report, malformed, malformedAt *int64) (chunkTally, bool, error) {
	c := chunkTally{to: from, rows: map[countKey]*wsm.LogSiteCount{}}
	for c.to-from < ChunkBytes {
		if err := ctx.Err(); err != nil {
			return c, true, err
		}
		line, err := r.ReadSlice('\n')
		if errors.Is(err, bufio.ErrBufferFull) {
			// A record longer than the buffer: read it whole. ReadSlice's
			// bytes are the buffer's own, which the next read overwrites, so
			// its head is copied before the rest is read.
			head := append([]byte(nil), line...)
			rest, err2 := r.ReadBytes('\n')
			line = append(head, rest...)
			err = err2
		}
		if errors.Is(err, io.EOF) {
			// A partial last line is a record still being written; the next
			// pass reads it whole.
			return c, true, nil
		}
		if err != nil {
			return c, true, err
		}
		if !bytes.Contains(line, siteField) {
			// No site field: a record written before its runtime keyed its
			// sites, or by a runtime outside agent-repl. It is credited to no
			// site whenever it was written, so it is counted without being
			// decoded (a first pass over three days of such records read 7.7
			// GB, 2026-11-21).
			report.Unkeyed++
			c.to += int64(len(line))
			continue
		}
		rec, ok := decode(line)
		switch {
		case !ok:
			*malformed++
			if *malformedAt < 0 {
				*malformedAt = c.to
			}
		case !rec.at.Before(until):
			// Written at or after the pass's snapshot: the next pass credits
			// it, to a deploy it then knows.
			return c, true, nil
		case rec.site == "":
			report.Unkeyed++
		default:
			deploy, ok := latestAtOrBefore(bounds.Deploys, rec.at)
			if !ok {
				report.Earlier++
				break
			}
			c.add(deploy, bucketOf(bounds.Marks, rec.at), rec)
		}
		c.to += int64(len(line))
	}
	return c, false, nil
}

// add counts REC in the chunk's row for DEPLOY and BUCKET.
func (c *chunkTally) add(deploy, bucket time.Time, rec record) {
	key := countKey{deploy: deploy, site: rec.site, level: rec.level, bucket: bucket}
	n, ok := c.rows[key]
	if !ok {
		n = &wsm.LogSiteCount{Deploy: deploy, Site: rec.site, Level: rec.level, Bucket: bucket, FirstSeen: rec.at}
		c.rows[key] = n
	}
	n.Count++
	if rec.at.Before(n.FirstSeen) {
		n.FirstSeen = rec.at
	}
	if !rec.at.Before(n.LastSeen) {
		n.LastSeen = rec.at
		n.Runtime, n.Template, n.Operation = rec.runtime, rec.template, rec.operation
		n.SampleMessage, n.SampleWorkspace, n.SampleContext = rec.message, rec.workspace, rec.context
	}
	c.records++
}

// latestAtOrBefore answers the latest of INSTANTS (oldest first) at or
// before AT, and false when AT predates them all: of the deploys, the one
// serving at AT; of the marks, the last one AT is past.
func latestAtOrBefore(instants []time.Time, at time.Time) (time.Time, bool) {
	i := sort.Search(len(instants), func(i int) bool { return instants[i].After(at) })
	if i == 0 {
		return time.Time{}, false
	}
	return instants[i-1], true
}

// bucketOf answers the start of the bucket a record at AT is counted in: the
// later of AT's hour and the latest of MARKS (oldest first) at or before it.
func bucketOf(marks []time.Time, at time.Time) time.Time {
	bucket := at.Truncate(time.Hour)
	if mark, ok := latestAtOrBefore(marks, at); ok && mark.After(bucket) {
		return mark
	}
	return bucket
}

// record is the part of a log record the tally reads.
type record struct {
	at        time.Time
	site      string
	template  string
	runtime   string
	level     string
	operation string
	message   string
	workspace string
	context   string
}

// line is a log record's JSONL shape, as far as the tally reads it
// (logging-contract.md "JSONL schema").
type line struct {
	Timestamp   string          `json:"timestamp"`
	Runtime     string          `json:"runtime"`
	Level       string          `json:"level"`
	Operation   string          `json:"operation"`
	Message     string          `json:"message"`
	Site        string          `json:"site"`
	Template    string          `json:"template"`
	WorkspaceID string          `json:"workspace_id"`
	Context     json.RawMessage `json:"context"`
}

// siteField is the key a keyed record's JSONL names its site under; a
// line without it holds no site, and one with it is decoded to tell.
var siteField = []byte(`"site"`)

// levels are the record levels the contract allows.
var levels = map[string]bool{"debug": true, "info": true, "warn": true, "error": true}

// decode answers the record LINE holds, and false for a line that is no
// record of the contract's shape. A record with no site decodes with its
// site empty.
func decode(raw []byte) (record, bool) {
	var l line
	if err := json.Unmarshal(bytes.TrimSpace(raw), &l); err != nil {
		return record{}, false
	}
	at, err := time.Parse(time.RFC3339Nano, l.Timestamp)
	if err != nil || l.Runtime == "" || !levels[l.Level] {
		return record{}, false
	}
	rec := record{at: at, level: l.Level}
	if l.Site == "" {
		return rec, true
	}
	if !logsite.ValidKey(l.Site) || l.Template == "" {
		return record{}, false
	}
	ctx := "{}"
	if len(l.Context) > 0 && !bytes.Equal(l.Context, []byte("null")) {
		ctx = string(l.Context)
	}
	if len(ctx) > SampleContextBytes {
		ctx = fmt.Sprintf(`{"sample_context_bytes":%d}`, len(ctx))
	}
	rec.site, rec.template, rec.runtime, rec.operation = l.Site, l.Template, l.Runtime, l.Operation
	rec.message, rec.workspace, rec.context = l.Message, l.WorkspaceID, ctx
	return rec, true
}

// DeployBegan records that a deploy began now, prunes the deploys no longer
// kept, and runs a pass off the caller's goroutine so the deploy before is
// read whole soon. It answers the instant the deploy was recorded at, and
// true; a deploy that cannot be recorded is recorded at ERROR and answers
// false.
func (t *Tallier) DeployBegan(ctx context.Context, spawn func(func())) (time.Time, bool) {
	const op = "daemon.logsites.deploy_began"
	at, err := t.store.RecordLogSiteDeploy(ctx, t.now)
	if err != nil {
		t.log.Error(op, "could not record the deploy that began, so its log sites are credited to the deploy before", dlog.Context{"cause": err.Error()})
		return time.Time{}, false
	}
	t.log.Info(op, "a deploy began; its log sites are tallied apart from the deploy before", dlog.Context{"deploy_at": at})
	spawn(func() {
		// A failed pass is recorded by Pass.
		_, _ = t.Pass(ctx, "deploy_began")
	})
	return at, true
}
