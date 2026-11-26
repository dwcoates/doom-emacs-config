package logsites

import (
	"context"
	"errors"
	"reflect"
	"strconv"
	"testing"
	"time"

	"agentrepl/logging/logsite"

	"agent-repld/internal/wsm"
)

// windowFixture is a fixture whose log holds, in the hour after t0, siteA at
// info three times, siteB at error once and siteC at warn once, and siteA
// once more in the hour after.
func windowFixture(t *testing.T) *fixture {
	t.Helper()
	f := newFixture(t)
	f.now = t0.Add(3 * time.Hour)
	f.write(t, "daemon.run.log",
		keyed(t0.Add(time.Minute), "info", "pkg.A", "said {what}", "said 1")+
			keyed(t0.Add(2*time.Minute), "info", "pkg.A", "said {what}", "said 2")+
			keyed(t0.Add(3*time.Minute), "info", "pkg.A", "said {what}", "said 3")+
			keyed(t0.Add(4*time.Minute), "error", "pkg.B", "failed {what}", "failed 1")+
			keyed(t0.Add(5*time.Minute), "warn", "pkg.C", "slow {what}", "slow 1")+
			keyed(t0.Add(70*time.Minute), "info", "pkg.A", "said {what}", "said 4"))
	return f
}

var siteC = logsite.Key("daemon", "pkg.C", "slow {what}")

// keysOf answers each site's key, level and count, in the answer's order.
func keysOf(sites []Site) []string {
	var out []string
	for _, s := range sites {
		out = append(out, s.Key+"/"+s.Level+"/"+strconv.FormatInt(s.Count, 10))
	}
	return out
}

func TestWindowAnswersTheSitesThatFiredInItsHours(t *testing.T) {
	tests := []struct {
		name        string
		since       time.Time
		until       time.Time
		minimum     string
		wantSites   []string
		wantFrom    time.Time
		wantTo      time.Time
		wantPartial bool
		wantWidened bool
	}{
		{
			name: "errors first, then the most often fired", since: t0, until: t0.Add(30 * time.Minute),
			wantSites: []string{siteB + "/error/1", siteA + "/info/3", siteC + "/warn/1"},
			wantFrom:  t0, wantTo: t0.Add(time.Hour), wantWidened: true,
		},
		{
			name: "a widened edge leaves out the rows wholly past it", since: t0.Add(30 * time.Minute), until: t0.Add(80 * time.Minute),
			wantSites: []string{siteA + "/info/1"},
			wantFrom:  t0, wantTo: t0.Add(2 * time.Hour), wantWidened: true,
		},
		{
			name: "a minimum level leaves out the levels below it", since: t0, until: t0.Add(30 * time.Minute), minimum: "warn",
			wantSites: []string{siteB + "/error/1", siteC + "/warn/1"},
			wantFrom:  t0, wantTo: t0.Add(time.Hour), wantWidened: true,
		},
		{
			name: "a window older than the kept rows is answered from their oldest hour", since: t0.Add(-5 * time.Hour), until: t0.Add(30 * time.Minute),
			wantSites: []string{siteB + "/error/1", siteA + "/info/3", siteC + "/warn/1"},
			wantFrom:  t0, wantTo: t0.Add(time.Hour), wantPartial: true, wantWidened: true,
		},
		{
			name: "a window in which nothing fired answers no site", since: t0.Add(2 * time.Hour), until: t0.Add(150 * time.Minute),
			wantFrom: t0.Add(2 * time.Hour), wantTo: t0.Add(3 * time.Hour), wantWidened: true,
		},
		{
			name: "a window of whole hours is read exactly", since: t0, until: t0.Add(time.Hour),
			wantSites: []string{siteB + "/error/1", siteA + "/info/3", siteC + "/warn/1"},
			wantFrom:  t0, wantTo: t0.Add(time.Hour),
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := windowFixture(t)

			// Act
			answer, err := f.t.Window(context.Background(), tt.since, tt.until, tt.minimum)

			// Assert
			if err != nil {
				t.Fatalf("Window: %v", err)
			}
			if got := keysOf(answer.Sites); !reflect.DeepEqual(got, tt.wantSites) {
				t.Errorf("sites = %v, want %v", got, tt.wantSites)
			}
			if !answer.From.Equal(tt.wantFrom) || !answer.To.Equal(tt.wantTo) || answer.Partial != tt.wantPartial || answer.Widened != tt.wantWidened {
				t.Errorf("covered [%v, %v) partial %v widened %v, want [%v, %v) partial %v widened %v", answer.From, answer.To, answer.Partial, answer.Widened,
					tt.wantFrom, tt.wantTo, tt.wantPartial, tt.wantWidened)
			}
		})
	}
}

// TestWindowReadsAnEdgeTheTallySplitsAtExactly pins that a window from or to
// a mark or a deploy's start holds no record from past it, inside one hour.
func TestWindowReadsAnEdgeTheTallySplitsAtExactly(t *testing.T) {
	edge := t0.Add(3*time.Minute + 30*time.Second)
	tests := []struct {
		name      string
		record    func(f *fixture) error
		since     time.Time
		until     time.Time
		wantSites []string
	}{
		{name: "to a mark", since: t0, until: edge, wantSites: []string{siteA + "/info/3"},
			record: func(f *fixture) error {
				_, err := f.store.RecordLogSiteMark(context.Background(), func() time.Time { return edge })
				return err
			}},
		{name: "from a mark", since: edge, until: t0.Add(2 * time.Hour), wantSites: []string{siteB + "/error/1", siteC + "/warn/1", siteA + "/info/1"},
			record: func(f *fixture) error {
				_, err := f.store.RecordLogSiteMark(context.Background(), func() time.Time { return edge })
				return err
			}},
		{name: "to a deploy's start", since: t0, until: edge, wantSites: []string{siteA + "/info/3"},
			record: func(f *fixture) error {
				_, err := f.store.RecordLogSiteDeploy(context.Background(), func() time.Time { return edge })
				return err
			}},
		{name: "from a deploy's start", since: edge, until: t0.Add(2 * time.Hour), wantSites: []string{siteB + "/error/1", siteC + "/warn/1", siteA + "/info/1"},
			record: func(f *fixture) error {
				_, err := f.store.RecordLogSiteDeploy(context.Background(), func() time.Time { return edge })
				return err
			}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := windowFixture(t)
			if err := tt.record(f); err != nil {
				t.Fatalf("record the edge: %v", err)
			}

			// Act
			answer, err := f.t.Window(context.Background(), tt.since, tt.until, "")

			// Assert
			if err != nil {
				t.Fatalf("Window: %v", err)
			}
			if got := keysOf(answer.Sites); !reflect.DeepEqual(got, tt.wantSites) {
				t.Errorf("sites = %v, want %v", got, tt.wantSites)
			}
			if !answer.From.Equal(tt.since) || !answer.To.Equal(tt.until) || answer.Widened {
				t.Errorf("covered [%v, %v) widened %v, want exactly [%v, %v)", answer.From, answer.To, answer.Widened, tt.since, tt.until)
			}
		})
	}
}

// TestWindowOfTheSpanBeforeHoldsNoSiteThatFiredAfterIt pins the 2026-11-25
// production finding: two spans inside one hour, a site first firing between
// them. Read from marks, the span before holds no record of the site, so the
// production check sees it as new.
func TestWindowOfTheSpanBeforeHoldsNoSiteThatFiredAfterIt(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.now = t0.Add(2 * time.Hour)
	marks := []time.Time{t0.Add(5 * time.Minute), t0.Add(15 * time.Minute), t0.Add(25 * time.Minute), t0.Add(45 * time.Minute)}
	for _, m := range marks {
		if _, err := f.store.RecordLogSiteMark(context.Background(), func() time.Time { return m }); err != nil {
			t.Fatalf("RecordLogSiteMark: %v", err)
		}
	}
	f.write(t, "daemon.run.log",
		keyed(t0.Add(10*time.Minute), "warn", "pkg.C", "slow {what}", "slow 1")+
			keyed(t0.Add(20*time.Minute), "error", "pkg.B", "failed {what}", "failed 1")+
			keyed(t0.Add(30*time.Minute), "error", "pkg.B", "failed {what}", "failed 2")+
			keyed(t0.Add(50*time.Minute), "warn", "pkg.C", "slow {what}", "slow 2"))

	// Act
	before, errBefore := f.t.Window(context.Background(), marks[0], marks[1], "warn")
	span, errSpan := f.t.Window(context.Background(), marks[2], marks[3], "warn")

	// Assert
	if errBefore != nil || errSpan != nil {
		t.Fatalf("Window = (%v, %v)", errBefore, errSpan)
	}
	if got := keysOf(before.Sites); !reflect.DeepEqual(got, []string{siteC + "/warn/1"}) {
		t.Errorf("the span before's sites = %v, want only siteC's record inside it", got)
	}
	if got := keysOf(span.Sites); !reflect.DeepEqual(got, []string{siteB + "/error/1"}) {
		t.Errorf("the span's sites = %v, want only siteB's record inside it", got)
	}
}

func TestWindowAnswersASitesLatestRecordAsItsSample(t *testing.T) {
	// Arrange
	f := windowFixture(t)

	// Act
	answer, err := f.t.Window(context.Background(), t0, t0.Add(80*time.Minute), "info")

	// Assert
	if err != nil {
		t.Fatalf("Window: %v", err)
	}
	for _, s := range answer.Sites {
		if s.Key != siteA {
			continue
		}
		want := Site{
			Key: siteA, Template: "said {what}", Runtime: "daemon", Level: "info", Operation: "daemon.test", Count: 4,
			FirstSeen: t0.Add(time.Minute), LastSeen: t0.Add(70 * time.Minute),
			SampleAt: t0.Add(70 * time.Minute), SampleMessage: "said 4", SampleContext: `{"what":"x"}`,
		}
		if !reflect.DeepEqual(s, want) {
			t.Fatalf("site = %+v, want %+v", s, want)
		}
		return
	}
	t.Fatalf("sites = %v, want siteA among them", keysOf(answer.Sites))
}

func TestWindowRefusesAWindowItCannotAnswer(t *testing.T) {
	tests := []struct {
		name    string
		since   time.Time
		until   time.Time
		minimum string
		check   func(t *testing.T, err error)
	}{
		{
			name: "an end before its start", since: t0.Add(time.Hour), until: t0,
			check: func(t *testing.T, err error) {
				var malformed *MalformedWindowError
				if !errors.As(err, &malformed) {
					t.Fatalf("err = %v, want a malformed window", err)
				}
			},
		},
		{
			name: "a level the contract does not have", since: t0, until: t0.Add(time.Hour), minimum: "fatal",
			check: func(t *testing.T, err error) {
				var malformed *MalformedWindowError
				if !errors.As(err, &malformed) {
					t.Fatalf("err = %v, want a malformed window", err)
				}
			},
		},
		{
			name: "a window wholly before the kept rows", since: t0.Add(-5 * time.Hour), until: t0.Add(-2 * time.Hour),
			check: func(t *testing.T, err error) {
				var unrecorded *UnrecordedWindowError
				if !errors.As(err, &unrecorded) || !unrecorded.Oldest.Equal(t0) {
					t.Fatalf("err = %v, want an unrecorded window whose oldest hour is %v", err, t0)
				}
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := windowFixture(t)

			// Act
			_, err := f.t.Window(context.Background(), tt.since, tt.until, tt.minimum)

			// Assert
			tt.check(t, err)
		})
	}
}

func TestWindowFailsWhenItsPassFails(t *testing.T) {
	// Arrange
	f := windowFixture(t)
	f.store.tally = func(context.Context, wsm.LogSiteChunk) (bool, error) { return false, errors.New("disk full") }

	// Act
	_, err := f.t.Window(context.Background(), t0, t0.Add(time.Hour), "")

	// Assert
	if err == nil {
		t.Fatal("Window answered over a pass that failed, want its failure")
	}
}
