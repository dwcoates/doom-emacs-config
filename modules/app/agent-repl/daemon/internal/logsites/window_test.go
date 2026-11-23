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
	}{
		{
			name: "errors first, then the most often fired", since: t0, until: t0.Add(30 * time.Minute),
			wantSites: []string{siteB + "/error/1", siteA + "/info/3", siteC + "/warn/1"},
			wantFrom:  t0, wantTo: t0.Add(time.Hour),
		},
		{
			name: "a window widens to its whole hours and sums them", since: t0.Add(30 * time.Minute), until: t0.Add(80 * time.Minute),
			wantSites: []string{siteB + "/error/1", siteA + "/info/4", siteC + "/warn/1"},
			wantFrom:  t0, wantTo: t0.Add(2 * time.Hour),
		},
		{
			name: "a minimum level leaves out the levels below it", since: t0, until: t0.Add(30 * time.Minute), minimum: "warn",
			wantSites: []string{siteB + "/error/1", siteC + "/warn/1"},
			wantFrom:  t0, wantTo: t0.Add(time.Hour),
		},
		{
			name: "a window older than the kept rows is answered from their oldest hour", since: t0.Add(-5 * time.Hour), until: t0.Add(30 * time.Minute),
			wantSites: []string{siteB + "/error/1", siteA + "/info/3", siteC + "/warn/1"},
			wantFrom:  t0, wantTo: t0.Add(time.Hour), wantPartial: true,
		},
		{
			name: "a window in which nothing fired answers no site", since: t0.Add(2 * time.Hour), until: t0.Add(150 * time.Minute),
			wantFrom: t0.Add(2 * time.Hour), wantTo: t0.Add(3 * time.Hour),
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
			if !answer.From.Equal(tt.wantFrom) || !answer.To.Equal(tt.wantTo) || answer.Partial != tt.wantPartial {
				t.Errorf("covered [%v, %v) partial %v, want [%v, %v) partial %v", answer.From, answer.To, answer.Partial, tt.wantFrom, tt.wantTo, tt.wantPartial)
			}
		})
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
