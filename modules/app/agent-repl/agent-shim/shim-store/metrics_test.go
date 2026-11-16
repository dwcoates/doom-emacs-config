package main

import (
	"bytes"
	"context"
	"errors"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"sync"
	"testing"
	"time"

	"agentrepl/logging/metrics"

	"github.com/prometheus/client_golang/prometheus"
)

func TestOpenMetricsSurface(t *testing.T) {
	tests := []struct {
		name      string
		root      func(t *testing.T) string
		wantLevel string
		wantText  string
		wantErr   bool
		wantOpen  bool
	}{
		{name: "off without a root", root: func(*testing.T) string { return "" }, wantLevel: "debug", wantText: "the metrics surface is off"},
		{name: "listening and advertised", root: func(t *testing.T) string { return t.TempDir() }, wantLevel: "info", wantText: "the metrics surface is listening", wantOpen: true},
		{
			name: "a root that cannot hold targets is a boot error",
			root: func(t *testing.T) string {
				root := filepath.Join(t.TempDir(), "root")
				if err := os.WriteFile(root, nil, 0o644); err != nil {
					t.Fatal(err)
				}
				return root
			},
			wantLevel: "error", wantText: "the metrics surface could not open", wantErr: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			log, durable := storePprofLogger()
			root := tt.root(t)

			// Act
			surface, err := openMetricsSurface(root, filepath.Join(t.TempDir(), "events.db"), log)
			t.Cleanup(func() { closeMetricsSurface(surface, log) })

			// Assert
			if tt.wantErr != (err != nil) {
				t.Fatalf("err = %v, want error = %v", err, tt.wantErr)
			}
			if tt.wantOpen != (surface != nil) {
				t.Fatalf("surface open = %v, want %v", surface != nil, tt.wantOpen)
			}
			if tt.wantOpen {
				if _, statErr := os.Stat(metrics.TargetPath(root, metrics.ProgramStore)); statErr != nil {
					t.Fatalf("no store target advertised: %v", statErr)
				}
			}
			var found bool
			for _, r := range decodeStoreRecords(t, durable) {
				if r.Operation == "store.metrics" && r.Level == tt.wantLevel && strings.Contains(r.Message, tt.wantText) {
					found = true
				}
			}
			if !found {
				t.Fatalf("records = %+v, want a store.metrics %s record containing %q", decodeStoreRecords(t, durable), tt.wantLevel, tt.wantText)
			}
		})
	}
}

func TestTheStoreSurfaceServesItsDatabaseSize(t *testing.T) {
	// Arrange
	log, _ := storePprofLogger()
	dbPath := filepath.Join(t.TempDir(), "events.db")
	if err := os.WriteFile(dbPath, make([]byte, 2048), 0o644); err != nil {
		t.Fatal(err)
	}
	surface, err := openMetricsSurface(t.TempDir(), dbPath, log)
	if err != nil {
		t.Fatalf("openMetricsSurface: %v", err)
	}
	t.Cleanup(func() { closeMetricsSurface(surface, log) })

	// Act
	resp, err := http.Get("http://" + surface.Addr() + metrics.Path)
	if err != nil {
		t.Fatalf("scrape: %v", err)
	}
	defer resp.Body.Close()
	body, _ := io.ReadAll(resp.Body)

	// Assert
	if !strings.Contains(string(body), `agentrepl_db_bytes{db="events",file="db"} 2048`) {
		t.Fatalf("scrape lacks the database size:\n%s", body)
	}
}

// fakeEnds answers each PeerMessageEnds call from its script in turn, the
// last answer standing once the script runs out. A nil answer with a nil
// error waits for the call's deadline and fails with it.
type fakeEnds struct {
	mu      sync.Mutex
	answers []endsAnswer
	// deadlines records whether each call's context carried a deadline.
	deadlines []bool
}

type endsAnswer struct {
	ends map[string]int64
	err  error
}

func (f *fakeEnds) PeerMessageEnds(ctx context.Context) (map[string]int64, error) {
	f.mu.Lock()
	_, bounded := ctx.Deadline()
	f.deadlines = append(f.deadlines, bounded)
	answer := f.answers[0]
	if len(f.answers) > 1 {
		f.answers = f.answers[1:]
	}
	f.mu.Unlock()
	if answer.ends == nil && answer.err == nil {
		<-ctx.Done()
		return nil, ctx.Err()
	}
	return answer.ends, answer.err
}

// gatherPeerMessages scrapes the collector once and answers its series by end.
func gatherPeerMessages(t *testing.T, c prometheus.Collector) map[string]float64 {
	t.Helper()
	reg := prometheus.NewPedanticRegistry()
	reg.MustRegister(c)
	families, err := reg.Gather()
	if err != nil {
		t.Fatalf("Gather: %v", err)
	}
	got := map[string]float64{}
	for _, family := range families {
		if family.GetName() != peerMessagesName {
			continue
		}
		for _, m := range family.GetMetric() {
			got[m.GetLabel()[0].GetValue()] = m.GetGauge().GetValue()
		}
	}
	return got
}

// peerMessageRecords answers the store.metrics.peer-messages records at LEVEL.
func peerMessageRecords(t *testing.T, durable *bytes.Buffer, level string) []storeLogRecord {
	t.Helper()
	var out []storeLogRecord
	for _, r := range decodeStoreRecords(t, durable) {
		if r.Operation == "store.metrics.peer-messages" && r.Level == level {
			out = append(out, r)
		}
	}
	return out
}

func TestThePeerMessageCountIsServedByEnd(t *testing.T) {
	// Arrange
	log, _ := storePprofLogger()
	ends := &fakeEnds{answers: []endsAnswer{{ends: map[string]int64{"sender": 3, "recipient": 2}}}}

	// Act
	got := gatherPeerMessages(t, newPeerMessages(ends, log, peerMessagesTimeout))

	// Assert
	if want := map[string]float64{"sender": 3, "recipient": 2}; !reflect.DeepEqual(got, want) {
		t.Fatalf("series = %v, want %v", got, want)
	}
	if len(ends.deadlines) != 1 || !ends.deadlines[0] {
		t.Fatalf("deadlines = %v, want the one count bounded", ends.deadlines)
	}
}

func TestAFailedPeerMessageCountOmitsOnlyItsSeriesAndIsRecorded(t *testing.T) {
	tests := []struct {
		name   string
		answer endsAnswer
		cause  string
	}{
		{name: "a storage failure", answer: endsAnswer{err: errors.New("disk I/O error")}, cause: "disk I/O error"},
		{name: "a count past its bound", answer: endsAnswer{}, cause: context.DeadlineExceeded.Error()},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			log, durable := storePprofLogger()
			c := newPeerMessages(&fakeEnds{answers: []endsAnswer{tt.answer}}, log, time.Millisecond)

			// Act
			got := gatherPeerMessages(t, c)

			// Assert
			if len(got) != 0 {
				t.Fatalf("series = %v, want none while the count fails", got)
			}
			records := peerMessageRecords(t, durable, "error")
			if len(records) != 1 || !strings.Contains(records[0].Message, "cause="+tt.cause) || !strings.Contains(records[0].Message, "timeout_ms=1") {
				t.Fatalf("records = %+v, want one error naming the cause %q and the bound", records, tt.cause)
			}
		})
	}
}

func TestAPeerMessageCountFailureIsRecordedOncePerCause(t *testing.T) {
	// Arrange: the same failure twice, then another.
	log, durable := storePprofLogger()
	first := endsAnswer{err: errors.New("disk I/O error")}
	c := newPeerMessages(&fakeEnds{answers: []endsAnswer{first, first, {err: errors.New("database is locked")}}}, log, peerMessagesTimeout)

	// Act
	for range 3 {
		gatherPeerMessages(t, c)
	}

	// Assert
	records := peerMessageRecords(t, durable, "error")
	if len(records) != 2 || !strings.Contains(records[0].Message, "disk I/O error") || !strings.Contains(records[1].Message, "database is locked") {
		t.Fatalf("records = %+v, want one per cause", records)
	}
}

func TestAPeerMessageCountThatAnswersAgainIsServedAndRecorded(t *testing.T) {
	// Arrange
	log, durable := storePprofLogger()
	fail := endsAnswer{err: errors.New("disk I/O error")}
	c := newPeerMessages(&fakeEnds{answers: []endsAnswer{fail, {ends: map[string]int64{"sender": 1, "recipient": 0}}, fail}}, log, peerMessagesTimeout)
	gatherPeerMessages(t, c)

	// Act
	got := gatherPeerMessages(t, c)
	gatherPeerMessages(t, c)

	// Assert
	if want := map[string]float64{"sender": 1, "recipient": 0}; !reflect.DeepEqual(got, want) {
		t.Fatalf("series = %v, want %v once the count answers", got, want)
	}
	if recovered := peerMessageRecords(t, durable, "info"); len(recovered) != 1 {
		t.Fatalf("info records = %+v, want the recovery recorded once", recovered)
	}
	if failures := peerMessageRecords(t, durable, "error"); len(failures) != 2 {
		t.Fatalf("error records = %+v, want the failure after the recovery recorded again", failures)
	}
}

func TestRegisterPeerMessages(t *testing.T) {
	ends := &fakeEnds{answers: []endsAnswer{{ends: map[string]int64{"sender": 4, "recipient": 0}}}}
	t.Run("the off surface serves nothing", func(t *testing.T) {
		// Arrange
		log, _ := storePprofLogger()

		// Act
		err := registerPeerMessages(nil, ends, log)

		// Assert
		if err != nil {
			t.Fatalf("registerPeerMessages(nil) = %v, want nil", err)
		}
	})
	t.Run("an open surface serves the count", func(t *testing.T) {
		// Arrange
		log, _ := storePprofLogger()
		surface, err := openMetricsSurface(t.TempDir(), filepath.Join(t.TempDir(), "events.db"), log)
		if err != nil {
			t.Fatalf("openMetricsSurface: %v", err)
		}
		t.Cleanup(func() { closeMetricsSurface(surface, log) })

		// Act
		if err := registerPeerMessages(surface, ends, log); err != nil {
			t.Fatalf("registerPeerMessages: %v", err)
		}
		resp, err := http.Get("http://" + surface.Addr() + metrics.Path)
		if err != nil {
			t.Fatalf("scrape: %v", err)
		}
		defer resp.Body.Close()
		body, _ := io.ReadAll(resp.Body)

		// Assert
		if !strings.Contains(string(body), `agentrepl_peer_messages{end="sender"} 4`) {
			t.Fatalf("scrape lacks the peer message count:\n%s", body)
		}
	})
	t.Run("a second registration is refused and recorded", func(t *testing.T) {
		// Arrange
		log, durable := storePprofLogger()
		surface, err := openMetricsSurface(t.TempDir(), filepath.Join(t.TempDir(), "events.db"), log)
		if err != nil {
			t.Fatalf("openMetricsSurface: %v", err)
		}
		t.Cleanup(func() { closeMetricsSurface(surface, log) })
		if err := registerPeerMessages(surface, ends, log); err != nil {
			t.Fatalf("registerPeerMessages: %v", err)
		}

		// Act
		err = registerPeerMessages(surface, ends, log)

		// Assert
		if err == nil {
			t.Fatal("a second registration must be refused")
		}
		var found bool
		for _, r := range decodeStoreRecords(t, durable) {
			if r.Operation == "store.metrics" && r.Level == "error" && strings.Contains(r.Message, "could not register agentrepl_peer_messages") {
				found = true
			}
		}
		if !found {
			t.Fatalf("records = %+v, want the refused registration recorded at error", decodeStoreRecords(t, durable))
		}
	})
}

// TestThePeerMessageCountBoundFitsTheScrape pins the count's bound against the
// scraper's own scrape_timeout (metrics/scrape.yml): a bound sized to the
// scrape's measured stalls must still leave the scrape room to answer.
func TestThePeerMessageCountBoundFitsTheScrape(t *testing.T) {
	// Arrange
	raw, err := os.ReadFile(filepath.Join("..", "..", "metrics", "scrape.yml"))
	if err != nil {
		t.Fatalf("read scrape.yml: %v", err)
	}
	var scrapeTimeout time.Duration
	for _, line := range strings.Split(string(raw), "\n") {
		if v, ok := strings.CutPrefix(strings.TrimSpace(line), "scrape_timeout:"); ok {
			if scrapeTimeout, err = time.ParseDuration(strings.TrimSpace(v)); err != nil {
				t.Fatalf("parse scrape_timeout %q: %v", v, err)
			}
		}
	}
	if scrapeTimeout == 0 {
		t.Fatal("scrape.yml names no scrape_timeout")
	}

	// Act
	share := float64(peerMessagesTimeout) / float64(scrapeTimeout)

	// Assert
	if share > 0.1 {
		t.Fatalf("peerMessagesTimeout %v is %.0f%% of the %v scrape_timeout, want at most 10%%", peerMessagesTimeout, share*100, scrapeTimeout)
	}
}
