package wsm

import (
	"context"
	"database/sql"
	"reflect"
	"testing"
	"time"

	"agent-repld/internal/dlog"
)

// logSiteHour is the hour every tally row of these tests falls in.
var logSiteHour = time.Date(2026, 11, 21, 10, 0, 0, 0, time.UTC)

// fixedNow is a clock that always answers AT.
func fixedNow(at time.Time) func() time.Time { return func() time.Time { return at } }

// logSiteCount is one count of site KEY at LEVEL credited to DEPLOY, seen once
// at SEEN with MESSAGE as its sample.
func logSiteCount(deploy time.Time, key, level string, seen time.Time, message string) LogSiteCount {
	return LogSiteCount{
		Deploy: deploy, Site: key, Level: level, Hour: seen.Truncate(time.Hour),
		Runtime: "daemon", Template: "said {what}", Operation: "daemon.test", Count: 1,
		FirstSeen: seen, LastSeen: seen, SampleMessage: message, SampleContext: `{"what":"x"}`,
	}
}

// recordedError reports whether LOG holds an ERROR record of OPERATION.
func recordedError(log *dlog.TestLogger, operation string) bool {
	_, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: operation})
	return ok
}

// tally adds one chunk to S, failing the test on an error, and answers
// whether it applied.
func tally(t *testing.T, s *store, c LogSiteChunk) bool {
	t.Helper()
	applied, err := s.TallyLogSites(context.Background(), c)
	if err != nil {
		t.Fatalf("TallyLogSites: %v", err)
	}
	return applied
}

// allLogSiteRows answers every row S keeps.
func allLogSiteRows(t *testing.T, s *store) []LogSiteRow {
	t.Helper()
	rows, err := s.LogSiteRows(context.Background(), time.Unix(0, 0), logSiteHour.Add(1000*time.Hour), LogSiteLevels)
	if err != nil {
		t.Fatalf("LogSiteRows: %v", err)
	}
	return rows
}

func TestTheLogSiteDeploysStartWithTheDeployBeforeEveryRecordedOne(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	deploys, err := s.LogSiteDeploys(context.Background())

	// Assert
	if err != nil || !reflect.DeepEqual(deploys, []time.Time{fromNanos(0)}) {
		t.Fatalf("LogSiteDeploys = (%v, %v), want only the instant-0 deploy", deploys, err)
	}
}

func TestRecordLogSiteDeployRecordsTheInstantTakenInItsTransaction(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	at, err := s.RecordLogSiteDeploy(context.Background(), fixedNow(logSiteHour))

	// Assert
	deploys, _ := s.LogSiteDeploys(context.Background())
	if err != nil || !at.Equal(logSiteHour) || len(deploys) != 2 || !deploys[1].Equal(logSiteHour) {
		t.Fatalf("RecordLogSiteDeploy = (%v, %v), deploys %v; want the deploy at %v recorded", at, err, deploys, logSiteHour)
	}
}

func TestRecordLogSiteDeployRefusesAnInstantNotAfterTheLatest(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	if _, err := s.RecordLogSiteDeploy(context.Background(), fixedNow(logSiteHour)); err != nil {
		t.Fatalf("RecordLogSiteDeploy: %v", err)
	}

	// Act
	_, err := s.RecordLogSiteDeploy(context.Background(), fixedNow(logSiteHour))

	// Assert
	deploys, _ := s.LogSiteDeploys(context.Background())
	if err == nil || len(deploys) != 2 {
		t.Fatalf("a second deploy at the same instant = %v, deploys %v; want it refused and nothing added", err, deploys)
	}
	if !recordedError(log, "daemon.wsm.record_log_site_deploy") {
		t.Fatalf("records = %+v, want the refusal at ERROR", log.Records())
	}
}

func TestLogSiteSnapshotAnswersTheDeploysAndItsInstant(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	if _, err := s.RecordLogSiteDeploy(context.Background(), fixedNow(logSiteHour)); err != nil {
		t.Fatalf("RecordLogSiteDeploy: %v", err)
	}
	at := logSiteHour.Add(time.Minute)

	// Act
	deploys, snapshot, err := s.LogSiteSnapshot(context.Background(), fixedNow(at))

	// Assert
	if err != nil || !snapshot.Equal(at) || !reflect.DeepEqual(deploys, []time.Time{fromNanos(0), fromNanos(nanos(logSiteHour))}) {
		t.Fatalf("LogSiteSnapshot = (%v, %v, %v), want both deploys at %v", deploys, snapshot, err, at)
	}
}

func TestTallyLogSitesAddsAChunkAndMovesItsCursor(t *testing.T) {
	deploy := fromNanos(0)
	early, late := logSiteHour.Add(time.Minute), logSiteHour.Add(2*time.Minute)
	tests := []struct {
		name        string
		chunks      []LogSiteChunk
		wantApplied []bool
		wantRows    []LogSiteRow
		wantOffset  int64
	}{
		{
			name:        "a first chunk adds its row and moves the cursor to its end",
			chunks:      []LogSiteChunk{{File: "f", Path: "/l/a.log", From: 0, To: 10, Counts: []LogSiteCount{logSiteCount(deploy, "aaaaaaaaaaaa", "info", early, "said x")}}},
			wantApplied: []bool{true},
			wantRows:    []LogSiteRow{{logSiteCount(deploy, "aaaaaaaaaaaa", "info", early, "said x")}},
			wantOffset:  10,
		},
		{
			name: "a later chunk adds its count, keeps the first sighting and takes the latest sample",
			chunks: []LogSiteChunk{
				{File: "f", Path: "/l/a.log", From: 0, To: 10, Counts: []LogSiteCount{logSiteCount(deploy, "aaaaaaaaaaaa", "info", early, "said x")}},
				{File: "f", Path: "/l/a.log", From: 10, To: 20, Counts: []LogSiteCount{logSiteCount(deploy, "aaaaaaaaaaaa", "info", late, "said y")}},
			},
			wantApplied: []bool{true, true},
			wantRows: func() []LogSiteRow {
				n := logSiteCount(deploy, "aaaaaaaaaaaa", "info", late, "said y")
				n.Count, n.FirstSeen = 2, early
				return []LogSiteRow{{n}}
			}(),
			wantOffset: 20,
		},
		{
			name: "an older record keeps the newer sample",
			chunks: []LogSiteChunk{
				{File: "f", Path: "/l/a.log", From: 0, To: 10, Counts: []LogSiteCount{logSiteCount(deploy, "aaaaaaaaaaaa", "info", late, "said y")}},
				{File: "g", Path: "/l/b.log", From: 0, To: 10, Counts: []LogSiteCount{logSiteCount(deploy, "aaaaaaaaaaaa", "info", early, "said x")}},
			},
			wantApplied: []bool{true, true},
			wantRows: func() []LogSiteRow {
				n := logSiteCount(deploy, "aaaaaaaaaaaa", "info", late, "said y")
				n.Count, n.FirstSeen = 2, early
				return []LogSiteRow{{n}}
			}(),
			wantOffset: 10,
		},
		{
			name: "a chunk read from elsewhere than the cursor changes nothing",
			chunks: []LogSiteChunk{
				{File: "f", Path: "/l/a.log", From: 0, To: 10, Counts: []LogSiteCount{logSiteCount(deploy, "aaaaaaaaaaaa", "info", early, "said x")}},
				{File: "f", Path: "/l/a.log", From: 0, To: 10, Counts: []LogSiteCount{logSiteCount(deploy, "aaaaaaaaaaaa", "info", early, "said x")}},
			},
			wantApplied: []bool{true, false},
			wantRows:    []LogSiteRow{{logSiteCount(deploy, "aaaaaaaaaaaa", "info", early, "said x")}},
			wantOffset:  10,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)

			// Act
			var applied []bool
			for _, c := range tt.chunks {
				applied = append(applied, tally(t, s, c))
			}

			// Assert
			if !reflect.DeepEqual(applied, tt.wantApplied) {
				t.Errorf("applied = %v, want %v", applied, tt.wantApplied)
			}
			if rows := allLogSiteRows(t, s); !reflect.DeepEqual(rows, tt.wantRows) {
				t.Errorf("rows = %+v, want %+v", rows, tt.wantRows)
			}
			if offset, found, err := s.LogSiteCursor(context.Background(), "f"); err != nil || !found || offset != tt.wantOffset {
				t.Errorf("cursor = (%d, %v, %v), want %d", offset, found, err, tt.wantOffset)
			}
		})
	}
}

func TestTallyLogSitesRefusesAChunkWithNoFileOrSpan(t *testing.T) {
	tests := []struct {
		name  string
		chunk LogSiteChunk
	}{
		{name: "no file", chunk: LogSiteChunk{From: 0, To: 1}},
		{name: "a negative start", chunk: LogSiteChunk{File: "f", From: -1, To: 1}},
		{name: "an end before its start", chunk: LogSiteChunk{File: "f", From: 5, To: 1}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)

			// Act
			applied, err := s.TallyLogSites(context.Background(), tt.chunk)

			// Assert
			if err == nil || applied {
				t.Fatalf("TallyLogSites = (%v, %v), want it refused", applied, err)
			}
			if !recordedError(log, "daemon.wsm.tally_log_sites") {
				t.Fatalf("records = %+v, want the refusal at ERROR", log.Records())
			}
			if _, found, _ := s.LogSiteCursor(context.Background(), tt.chunk.File); found {
				t.Fatal("a refused chunk left a cursor")
			}
		})
	}
}

func TestLogSiteCursorIsAbsentForAFileNeverTallied(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	offset, found, err := s.LogSiteCursor(context.Background(), "never")

	// Assert
	if err != nil || found || offset != 0 {
		t.Fatalf("LogSiteCursor = (%d, %v, %v), want none", offset, found, err)
	}
}

func TestPruneLogSiteDeploysKeepsTheMostRecent(t *testing.T) {
	a, b, c := logSiteHour, logSiteHour.Add(time.Hour), logSiteHour.Add(2*time.Hour)
	tests := []struct {
		name        string
		record      []time.Time
		keep        int
		wantDeploys []time.Time
		wantRemoved int64
	}{
		{name: "fewer deploys than kept prunes nothing", record: []time.Time{a}, keep: 3,
			wantDeploys: []time.Time{fromNanos(0), fromNanos(nanos(a))}, wantRemoved: 0},
		{name: "a fourth deploy prunes the oldest and its rows", record: []time.Time{a, b, c}, keep: 3,
			wantDeploys: []time.Time{fromNanos(nanos(a)), fromNanos(nanos(b)), fromNanos(nanos(c))}, wantRemoved: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			tally(t, s, LogSiteChunk{File: "f0", From: 0, To: 1, Counts: []LogSiteCount{logSiteCount(fromNanos(0), "aaaaaaaaaaaa", "info", a.Add(-time.Minute), "m")}})
			for i, at := range tt.record {
				if _, err := s.RecordLogSiteDeploy(context.Background(), fixedNow(at)); err != nil {
					t.Fatalf("RecordLogSiteDeploy: %v", err)
				}
				tally(t, s, LogSiteChunk{File: "f" + string(rune('1'+i)), From: 0, To: 1, Counts: []LogSiteCount{logSiteCount(at, "bbbbbbbbbbbb", "info", at, "m")}})
			}

			// Act
			removed, err := s.PruneLogSiteDeploys(context.Background(), tt.keep)

			// Assert
			deploys, _ := s.LogSiteDeploys(context.Background())
			if err != nil || removed != tt.wantRemoved || !reflect.DeepEqual(deploys, tt.wantDeploys) {
				t.Fatalf("PruneLogSiteDeploys = (%d, %v), deploys %v; want %d removed, deploys %v", removed, err, deploys, tt.wantRemoved, tt.wantDeploys)
			}
			for _, r := range allLogSiteRows(t, s) {
				if r.Deploy.Before(deploys[0]) {
					t.Fatalf("row %+v outlived its deploy", r)
				}
			}
		})
	}
}

func TestPruneLogSiteDeploysKeepsWhatAnUndecidedSpanNeeds(t *testing.T) {
	a, b, c, d := logSiteHour, logSiteHour.Add(time.Hour), logSiteHour.Add(2*time.Hour), logSiteHour.Add(3*time.Hour)
	spanSince := a.Add(30 * time.Minute)
	at := func(t time.Time) time.Time { return fromNanos(nanos(t)) }
	tests := []struct {
		name string
		// span is whether a production span from spanSince is recorded, and
		// decided whether it is decided.
		span        bool
		decided     bool
		wantDeploys []time.Time
		wantRemoved int64
	}{
		{name: "an undecided span keeps the deploy running at its start and every later one", span: true,
			wantDeploys: []time.Time{at(a), at(b), at(c), at(d)}, wantRemoved: 1},
		{name: "a decided span keeps nothing past the kept deploys", span: true, decided: true,
			wantDeploys: []time.Time{at(b), at(c), at(d)}, wantRemoved: 2},
		{name: "no span prunes to the kept deploys",
			wantDeploys: []time.Time{at(b), at(c), at(d)}, wantRemoved: 2},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			ctx := context.Background()
			s, _ := testStore(t)
			tally(t, s, LogSiteChunk{File: "f0", From: 0, To: 1, Counts: []LogSiteCount{logSiteCount(fromNanos(0), "aaaaaaaaaaaa", "info", a.Add(-time.Minute), "m")}})
			for i, at := range []time.Time{a, b, c, d} {
				if _, err := s.RecordLogSiteDeploy(ctx, fixedNow(at)); err != nil {
					t.Fatalf("RecordLogSiteDeploy: %v", err)
				}
				tally(t, s, LogSiteChunk{File: "f" + string(rune('1'+i)), From: 0, To: 1, Counts: []LogSiteCount{logSiteCount(at, "bbbbbbbbbbbb", "info", at, "m")}})
			}
			if tt.span {
				var decidedAt any
				decision := ""
				if tt.decided {
					decidedAt, decision = nanos(d), string(ProductionNothingToDo)
				}
				err := s.write(ctx, "test.production_span", dlog.Context{}, func(ctx context.Context, tx *sql.Tx) error {
					_, err := tx.ExecContext(ctx, `INSERT INTO production_checks (repo_key, span_since, span_until, deploy_at, decided_at, decision) VALUES ('repo', ?, ?, ?, ?, ?)`,
						nanos(spanSince), nanos(b), nanos(b), decidedAt, decision)
					return err
				})
				if err != nil {
					t.Fatalf("recording the span: %v", err)
				}
			}

			// Act
			removed, err := s.PruneLogSiteDeploys(ctx, 3)

			// Assert
			deploys, _ := s.LogSiteDeploys(ctx)
			if err != nil || removed != tt.wantRemoved || !reflect.DeepEqual(deploys, tt.wantDeploys) {
				t.Fatalf("PruneLogSiteDeploys = (%d, %v), deploys %v; want %d removed, deploys %v", removed, err, deploys, tt.wantRemoved, tt.wantDeploys)
			}
			for _, r := range allLogSiteRows(t, s) {
				if r.Deploy.Before(deploys[0]) {
					t.Fatalf("row %+v outlived its deploy", r)
				}
			}
		})
	}
}

func TestPruneLogSiteDeploysRefusesToKeepNone(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	_, err := s.PruneLogSiteDeploys(context.Background(), 0)

	// Assert
	deploys, _ := s.LogSiteDeploys(context.Background())
	if err == nil || len(deploys) != 1 {
		t.Fatalf("PruneLogSiteDeploys(0) = %v, deploys %v; want it refused and nothing pruned", err, deploys)
	}
	if !recordedError(log, "daemon.wsm.prune_log_site_deploys") {
		t.Fatalf("records = %+v, want the refusal at ERROR", log.Records())
	}
}

func TestForgetLogSiteCursorsDropsEveryFileNotLive(t *testing.T) {
	tests := []struct {
		name        string
		live        []string
		wantRemoved int64
		wantKept    []string
	}{
		{name: "a gone file's cursor goes", live: []string{"f1"}, wantRemoved: 1, wantKept: []string{"f1"}},
		{name: "no live file drops every cursor", live: nil, wantRemoved: 2},
		{name: "every file live drops nothing", live: []string{"f1", "f2"}, wantRemoved: 0, wantKept: []string{"f1", "f2"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			for _, f := range []string{"f1", "f2"} {
				tally(t, s, LogSiteChunk{File: f, From: 0, To: 1})
			}

			// Act
			removed, err := s.ForgetLogSiteCursors(context.Background(), tt.live)

			// Assert
			var kept []string
			for _, f := range []string{"f1", "f2"} {
				if _, found, _ := s.LogSiteCursor(context.Background(), f); found {
					kept = append(kept, f)
				}
			}
			if err != nil || removed != tt.wantRemoved || !reflect.DeepEqual(kept, tt.wantKept) {
				t.Fatalf("ForgetLogSiteCursors = (%d, %v), kept %v; want %d removed, kept %v", removed, err, kept, tt.wantRemoved, tt.wantKept)
			}
		})
	}
}

func TestLogSiteRowsAnswersTheHoursAndLevelsAsked(t *testing.T) {
	deploy := fromNanos(0)
	before, inside, after := logSiteHour.Add(-time.Minute), logSiteHour.Add(time.Minute), logSiteHour.Add(time.Hour+time.Minute)
	tests := []struct {
		name     string
		levels   []string
		wantKeys []string
	}{
		{name: "every level answers every row of the hour", levels: LogSiteLevels, wantKeys: []string{"bbbbbbbbbbbb", "cccccccccccc"}},
		{name: "a level leaves out the others", levels: []string{"error"}, wantKeys: []string{"cccccccccccc"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			tally(t, s, LogSiteChunk{File: "f", From: 0, To: 1, Counts: []LogSiteCount{
				logSiteCount(deploy, "aaaaaaaaaaaa", "info", before, "m"),
				logSiteCount(deploy, "bbbbbbbbbbbb", "info", inside, "m"),
				logSiteCount(deploy, "cccccccccccc", "error", inside, "m"),
				logSiteCount(deploy, "dddddddddddd", "info", after, "m"),
			}})

			// Act
			rows, err := s.LogSiteRows(context.Background(), logSiteHour, logSiteHour.Add(time.Hour), tt.levels)

			// Assert
			var keys []string
			for _, r := range rows {
				keys = append(keys, r.Site)
			}
			if err != nil || !reflect.DeepEqual(keys, tt.wantKeys) {
				t.Fatalf("LogSiteRows = (%v, %v), want %v", keys, err, tt.wantKeys)
			}
		})
	}
}

func TestLogSiteRowsRefusesAReadNamingNoLevel(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	_, err := s.LogSiteRows(context.Background(), logSiteHour, logSiteHour.Add(time.Hour), nil)

	// Assert
	if err == nil || !recordedError(log, "daemon.wsm.log_site_rows") {
		t.Fatalf("LogSiteRows with no level = %v, records %+v; want it refused at ERROR", err, log.Records())
	}
}

func TestOldestLogSiteHourAnswersTheEarliestKeptRow(t *testing.T) {
	tests := []struct {
		name      string
		counts    []LogSiteCount
		wantFound bool
		wantHour  time.Time
	}{
		{name: "no row is no hour"},
		{name: "the earliest row's hour", counts: []LogSiteCount{
			logSiteCount(fromNanos(0), "aaaaaaaaaaaa", "info", logSiteHour.Add(90*time.Minute), "m"),
			logSiteCount(fromNanos(0), "bbbbbbbbbbbb", "info", logSiteHour.Add(10*time.Minute), "m"),
		}, wantFound: true, wantHour: logSiteHour},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			tally(t, s, LogSiteChunk{File: "f", From: 0, To: 1, Counts: tt.counts})

			// Act
			hour, found, err := s.OldestLogSiteHour(context.Background())

			// Assert
			if err != nil || found != tt.wantFound || (found && !hour.Equal(tt.wantHour)) {
				t.Fatalf("OldestLogSiteHour = (%v, %v, %v), want (%v, %v)", hour, found, err, tt.wantHour, tt.wantFound)
			}
		})
	}
}
