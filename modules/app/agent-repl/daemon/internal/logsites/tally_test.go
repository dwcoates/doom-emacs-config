package logsites

import (
	"context"
	"errors"
	"os"
	"reflect"
	"strings"
	"testing"
	"time"

	"agentrepl/logging/logsite"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// The sites the fixtures write, by the function and template that key them.
var (
	siteA = logsite.Key("daemon", "pkg.A", "said {what}")
	siteB = logsite.Key("daemon", "pkg.B", "failed {what}")
)

func TestDecodeReadsTheRecordsTheTallyCounts(t *testing.T) {
	big := `{"timestamp":"2026-11-21T10:00:00Z","runtime":"daemon","level":"info","message":"m","site":"` + siteA +
		`","template":"t","context":{"x":"` + strings.Repeat("y", SampleContextBytes) + `"}}`
	tests := []struct {
		name    string
		line    string
		wantOK  bool
		wantRec record
	}{
		{name: "a keyed record", line: keyed(t0, "warn", "pkg.A", "said {what}", "said x"), wantOK: true,
			wantRec: record{at: t0, site: siteA, template: "said {what}", runtime: "daemon", level: "warn", operation: "daemon.test", message: "said x", context: `{"what":"x"}`}},
		{name: "a record that names no site", line: unkeyed(t0), wantOK: true, wantRec: record{at: t0, level: "info"}},
		{name: "a keyed record with no context samples an empty one", wantOK: true,
			line:    `{"timestamp":"2026-11-21T10:00:00Z","runtime":"daemon","level":"info","message":"m","site":"` + siteA + `","template":"t","context":null}`,
			wantRec: record{at: t0, site: siteA, template: "t", runtime: "daemon", level: "info", message: "m", context: "{}"}},
		{name: "a context past its bound samples its size", line: big, wantOK: true,
			wantRec: record{at: t0, site: siteA, template: "t", runtime: "daemon", level: "info", message: "m", context: `{"sample_context_bytes":8200}`}},
		{name: "a line that is no JSON", line: "not json\n"},
		{name: "a timestamp that does not parse", line: `{"timestamp":"yesterday","runtime":"daemon","level":"info"}`},
		{name: "a level the contract does not have", line: `{"timestamp":"2026-11-21T10:00:00Z","runtime":"daemon","level":"fatal"}`},
		{name: "no runtime", line: `{"timestamp":"2026-11-21T10:00:00Z","level":"info"}`},
		{name: "a site that is no key", line: `{"timestamp":"2026-11-21T10:00:00Z","runtime":"daemon","level":"info","site":"XYZ","template":"t"}`},
		{name: "a site with no template", line: `{"timestamp":"2026-11-21T10:00:00Z","runtime":"daemon","level":"info","site":"` + siteA + `"}`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			rec, ok := decode([]byte(tt.line))

			// Assert
			if ok != tt.wantOK || (ok && !reflect.DeepEqual(rec, tt.wantRec)) {
				t.Fatalf("decode = (%+v, %v), want (%+v, %v)", rec, ok, tt.wantRec, tt.wantOK)
			}
		})
	}
}

func TestDeployAtAnswersTheDeployServingAtAnInstant(t *testing.T) {
	deploys := []time.Time{t0, t0.Add(time.Hour)}
	tests := []struct {
		name   string
		at     time.Time
		want   time.Time
		wantOK bool
	}{
		{name: "before every deploy", at: t0.Add(-time.Second)},
		{name: "at a deploy's start", at: t0, want: t0, wantOK: true},
		{name: "between deploys", at: t0.Add(time.Minute), want: t0, wantOK: true},
		{name: "after the latest", at: t0.Add(2 * time.Hour), want: t0.Add(time.Hour), wantOK: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, ok := deployAt(deploys, tt.at)

			// Assert
			if ok != tt.wantOK || !got.Equal(tt.want) {
				t.Fatalf("deployAt(%v) = (%v, %v), want (%v, %v)", tt.at, got, ok, tt.want, tt.wantOK)
			}
		})
	}
}

func TestAPassTalliesEveryKeyedRecordAndCountsTheRest(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.write(t, "daemon.run.log",
		keyed(t0.Add(time.Minute), "info", "pkg.A", "said {what}", "said x")+
			keyed(t0.Add(2*time.Minute), "info", "pkg.A", "said {what}", "said y")+
			keyed(t0.Add(3*time.Minute), "error", "pkg.B", "failed {what}", "failed x")+
			unkeyed(t0.Add(4*time.Minute))+
			`{"site": no json`+"\n")

	// Act
	report := f.pass(t)

	// Assert
	if report.Records != 3 || report.Unkeyed != 1 || report.Malformed != 1 || report.Files != 1 {
		t.Errorf("report = %+v, want 3 records, 1 unkeyed, 1 malformed, 1 file", report)
	}
	rows := f.rows(t)
	if got, want := counts(rows), sorted(siteA+":2", siteB+":1"); !reflect.DeepEqual(got, want) {
		t.Errorf("rows = %v, want %v", got, want)
	}
	for _, r := range rows {
		if r.Site == siteA && (r.SampleMessage != "said y" || !r.FirstSeen.Equal(t0.Add(time.Minute)) || !r.Hour.Equal(t0)) {
			t.Errorf("row %+v, want its latest sample, its first sighting and its hour", r)
		}
	}
	if !recorded(f.log, dlog.LevelError, "the log file "+f.path("daemon.run.log")+" holds lines that are no record of the logging contract") {
		t.Errorf("records = %+v, want the malformed line recorded at ERROR", f.log.Records())
	}
	if !recorded(f.log, dlog.LevelInfo, "tallied the log sites of every sink") {
		t.Errorf("records = %+v, want the pass recorded at INFO", f.log.Records())
	}
}

func TestAPassReadsEachFileFromItsCursor(t *testing.T) {
	tests := []struct {
		name      string
		between   func(t *testing.T, f *fixture)
		wantCount []string
	}{
		{
			name: "a second pass reads only what was appended",
			between: func(t *testing.T, f *fixture) {
				f.write(t, "daemon.run.log", keyed(t0.Add(5*time.Minute), "info", "pkg.A", "said {what}", "said z"))
			},
			wantCount: []string{siteA + ":2"},
		},
		{
			name: "a rotation keeps the rolled file's cursor",
			between: func(t *testing.T, f *fixture) {
				if err := os.Rename(f.path("daemon.run.log"), f.path("daemon.run.log.1")); err != nil {
					t.Fatal(err)
				}
				f.write(t, "daemon.run.log", keyed(t0.Add(5*time.Minute), "info", "pkg.A", "said {what}", "said z"))
			},
			wantCount: []string{siteA + ":2"},
		},
		{
			name:      "nothing appended adds nothing",
			between:   func(*testing.T, *fixture) {},
			wantCount: []string{siteA + ":1"},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.write(t, "daemon.run.log", keyed(t0.Add(time.Minute), "info", "pkg.A", "said {what}", "said x"))
			f.pass(t)
			tt.between(t, f)

			// Act
			f.pass(t)

			// Assert
			if got := counts(f.rows(t)); !reflect.DeepEqual(got, tt.wantCount) {
				t.Fatalf("rows = %v, want %v", got, tt.wantCount)
			}
		})
	}
}

func TestAPassLeavesWhatIsNotYetWholeOrNotYetCredited(t *testing.T) {
	tests := []struct {
		name       string
		text       string
		wantFirst  []string
		wantSecond []string
	}{
		{
			name:       "a partial last line is read once it is whole",
			text:       keyed(t0.Add(time.Minute), "info", "pkg.A", "said {what}", "said x") + strings.TrimSuffix(keyed(t0.Add(2*time.Minute), "info", "pkg.A", "said {what}", "said y"), "\n"),
			wantFirst:  []string{siteA + ":1"},
			wantSecond: []string{siteA + ":2"},
		},
		{
			name:       "a record at or after the pass's snapshot waits for the next pass",
			text:       keyed(t0.Add(time.Minute), "info", "pkg.A", "said {what}", "said x") + keyed(t0.Add(time.Hour), "info", "pkg.A", "said {what}", "said y"),
			wantFirst:  []string{siteA + ":1"},
			wantSecond: []string{siteA + ":1", siteA + ":1"},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			f.write(t, "daemon.run.log", tt.text)

			// Act
			f.pass(t)
			first := counts(f.rows(t))
			if !strings.HasSuffix(tt.text, "\n") {
				f.write(t, "daemon.run.log", "\n")
			}
			f.now = f.now.Add(time.Hour)
			f.pass(t)

			// Assert
			if !reflect.DeepEqual(first, tt.wantFirst) {
				t.Errorf("after the first pass rows = %v, want %v", first, tt.wantFirst)
			}
			if got := counts(f.rows(t)); !reflect.DeepEqual(got, tt.wantSecond) {
				t.Errorf("after the second pass rows = %v, want %v", got, tt.wantSecond)
			}
		})
	}
}

func TestAPassCreditsEachRecordToTheDeployServingAtIt(t *testing.T) {
	// Arrange
	f := newFixture(t)
	deploy := t0.Add(30 * time.Minute)
	if _, err := f.store.RecordLogSiteDeploy(context.Background(), func() time.Time { return deploy }); err != nil {
		t.Fatalf("RecordLogSiteDeploy: %v", err)
	}
	f.write(t, "daemon.run.log",
		keyed(t0.Add(time.Minute), "info", "pkg.A", "said {what}", "said x")+
			keyed(t0.Add(40*time.Minute), "info", "pkg.A", "said {what}", "said y"))

	// Act
	f.pass(t)

	// Assert
	var deploys []time.Time
	for _, r := range f.rows(t) {
		deploys = append(deploys, r.Deploy)
	}
	if len(deploys) != 2 || !deploys[0].Equal(time.Unix(0, 0)) || !deploys[1].Equal(deploy) {
		t.Fatalf("rows credited to %v, want the instant-0 deploy then the deploy at %v", deploys, deploy)
	}
}

func TestAPassCountsARecordBeforeEveryKeptDeployApart(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.store.snapshot = func(context.Context, func() time.Time) ([]time.Time, time.Time, error) {
		return []time.Time{t0.Add(30 * time.Minute)}, f.now, nil
	}
	f.write(t, "daemon.run.log", keyed(t0.Add(time.Minute), "info", "pkg.A", "said {what}", "said x"))

	// Act
	report := f.pass(t)

	// Assert
	if report.Earlier != 1 || report.Records != 0 || len(f.rows(t)) != 0 {
		t.Fatalf("report = %+v, rows %v; want the record counted earlier and credited nowhere", report, counts(f.rows(t)))
	}
}

func TestAPassThatFindsTheCursorMovedLeavesTheOtherPassesCount(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.store.tally = func(context.Context, wsm.LogSiteChunk) (bool, error) { return false, nil }
	f.write(t, "daemon.run.log", keyed(t0.Add(time.Minute), "info", "pkg.A", "said {what}", "said x"))

	// Act
	report := f.pass(t)

	// Assert
	if report.Moved != 1 || report.Records != 0 {
		t.Fatalf("report = %+v, want the file counted moved and nothing credited", report)
	}
	if _, ok := dlog.FindRecord(f.log.Records(), dlog.RecordQuery{Level: dlog.LevelError}); ok {
		t.Fatalf("records = %+v, want no ERROR: a moved cursor is no failure", f.log.Records())
	}
}

func TestAPassFailsOnAFileItCannotTallyAndReadsTheRest(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, f *fixture)
		cause   string
	}{
		{
			name: "a file shorter than its cursor",
			arrange: func(t *testing.T, f *fixture) {
				f.write(t, "daemon.run.log", keyed(t0.Add(time.Minute), "info", "pkg.A", "said {what}", "said x"))
				f.pass(t)
				if err := os.Truncate(f.path("daemon.run.log"), 1); err != nil {
					t.Fatal(err)
				}
			},
			cause: "a log file only grows",
		},
		{
			name: "a chunk the store cannot keep",
			arrange: func(t *testing.T, f *fixture) {
				f.write(t, "daemon.run.log", keyed(t0.Add(time.Minute), "info", "pkg.A", "said {what}", "said x"))
				f.store.tally = func(_ context.Context, c wsm.LogSiteChunk) (bool, error) {
					if strings.HasSuffix(c.Path, "daemon.run.log") {
						return false, errors.New("disk full")
					}
					return f.store.Store.TallyLogSites(context.Background(), c)
				}
			},
			cause: "disk full",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			tt.arrange(t, f)
			f.write(t, "emacs.central.log", keyed(t0.Add(2*time.Minute), "info", "pkg.B", "failed {what}", "failed x"))

			// Act
			report, err := f.t.Pass(context.Background(), "test")

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.cause) || report.Failed != 1 {
				t.Fatalf("Pass = (%+v, %v), want one failed file naming %q", report, err, tt.cause)
			}
			rec, ok := dlog.FindRecord(f.log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opPass, Message: "could not tally the log file " + f.path("daemon.run.log")})
			if !ok || !strings.Contains(rec.Context["cause"].(string), tt.cause) {
				t.Fatalf("records = %+v, want the file's failure at ERROR with its cause", f.log.Records())
			}
			if !recorded(f.log, dlog.LevelError, "the log site pass could not tally every sink") {
				t.Fatalf("records = %+v, want the failed pass at ERROR", f.log.Records())
			}
			found := false
			for _, r := range f.rows(t) {
				found = found || r.Site == siteB
			}
			if !found {
				t.Fatal("the other file was not tallied")
			}
		})
	}
}

func TestAPassThatCannotSnapshotTheDeploysTalliesNothing(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.store.snapshot = func(context.Context, func() time.Time) ([]time.Time, time.Time, error) {
		return nil, time.Time{}, errors.New("database is locked")
	}
	f.write(t, "daemon.run.log", keyed(t0.Add(time.Minute), "info", "pkg.A", "said {what}", "said x"))

	// Act
	_, err := f.t.Pass(context.Background(), "test")

	// Assert
	if err == nil || len(f.rows(t)) != 0 {
		t.Fatalf("Pass = %v with rows %v, want a failure and nothing tallied", err, counts(f.rows(t)))
	}
	if !recorded(f.log, dlog.LevelError, "the log site pass could not tally every sink") {
		t.Fatalf("records = %+v, want the failed pass at ERROR", f.log.Records())
	}
}

func TestAPassPastItsBoundFailsNamingIt(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.t.passBound = time.Nanosecond
	f.write(t, "daemon.run.log", keyed(t0.Add(time.Minute), "info", "pkg.A", "said {what}", "said x"))

	// Act
	_, err := f.t.Pass(context.Background(), "test")

	// Assert
	if !errors.Is(err, context.DeadlineExceeded) {
		t.Fatalf("Pass = %v, want the bound's deadline", err)
	}
	if !recorded(f.log, dlog.LevelError, "the log site pass ran past its bound of 1ns") {
		t.Fatalf("records = %+v, want the bound named at ERROR", f.log.Records())
	}
}

func TestDeployBeganRecordsTheDeployAndRunsAPass(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.write(t, "daemon.run.log", keyed(t0.Add(time.Minute), "info", "pkg.A", "said {what}", "said x"))
	spawned := 0

	// Act
	f.t.DeployBegan(context.Background(), func(run func()) { spawned++; run() })

	// Assert
	deploys, _, err := f.store.LogSiteSnapshot(context.Background(), time.Now)
	if err != nil || len(deploys) != 2 || !deploys[1].Equal(f.now) {
		t.Fatalf("deploys = (%v, %v), want the deploy at %v recorded", deploys, err, f.now)
	}
	if spawned != 1 || len(f.rows(t)) != 1 {
		t.Fatalf("spawned %d passes, rows %v; want one pass that tallied the file", spawned, counts(f.rows(t)))
	}
	if _, ok := dlog.FindRecord(f.log.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: "daemon.logsites.deploy_began"}); !ok {
		t.Fatalf("records = %+v, want the deploy recorded at INFO", f.log.Records())
	}
}

func TestDeployBeganThatCannotRecordTheDeployRunsNoPass(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.store.record = func(context.Context, func() time.Time) (time.Time, error) {
		return time.Time{}, errors.New("database is locked")
	}
	spawned := 0

	// Act
	f.t.DeployBegan(context.Background(), func(func()) { spawned++ })

	// Assert
	rec, ok := dlog.FindRecord(f.log.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.logsites.deploy_began"})
	if !ok || rec.Context["cause"] != "database is locked" || spawned != 0 {
		t.Fatalf("records = %+v, spawned %d; want the failure at ERROR with its cause and no pass", f.log.Records(), spawned)
	}
}

func TestNewRefusesAMissingCollaborator(t *testing.T) {
	tests := []struct {
		name string
		opts Options
	}{
		{name: "no store", opts: Options{Log: dlog.Nowhere(), PassBound: time.Second}},
		{name: "no logger", opts: Options{Store: &fakeStore{}, PassBound: time.Second}},
		{name: "no pass bound", opts: Options{Store: &fakeStore{}, Log: dlog.Nowhere()}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Assert
			defer func() {
				if recover() == nil {
					t.Fatal("New returned, want a panic")
				}
			}()

			// Act
			New(tt.opts)
		})
	}
}
