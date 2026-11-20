package stallbeat

import (
	"fmt"
	"sort"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/webfootprint"
)

// base is a moment on phase 0 of the cycle.
var base = time.Unix(1_800_000_000, 0)

// The webhost's two documents, each its own web content process.
const (
	host    = "host"
	sidebar = "sidebar"
)

// onBeat answers N moments one Period apart from START, each shifted by OFF.
func onBeat(start time.Time, n int, off time.Duration) []time.Time {
	out := make([]time.Time, n)
	for i := range out {
		out[i] = start.Add(time.Duration(i)*Period + off)
	}
	return out
}

// fakeFootprint answers a fixed reading, or none.
type fakeFootprint struct {
	reading webfootprint.Reading
	read    bool
}

func (f fakeFootprint) Latest() (webfootprint.Reading, bool) { return f.reading, f.read }

func newDetector(t *testing.T) (*Detector, *dlog.TestLogger) {
	t.Helper()
	return newDetectorWith(t, fakeFootprint{})
}

func newDetectorWith(t *testing.T, footprint Footprint) (*Detector, *dlog.TestLogger) {
	t.Helper()
	log := dlog.NewTestLogger()
	d, err := New(Deps{Log: log, Footprint: footprint})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	return d, log
}

// raiseBeat observes Beat stalls of DOCUMENT on one phase, the last at
// lastBeat, and answers the error they raised.
func raiseBeat(t *testing.T, d *Detector, log *dlog.TestLogger, document string) dlog.Record {
	t.Helper()
	for _, at := range onBeat(base, Beat, 0) {
		d.Observe(document, at, 300)
	}
	raised := raisedErrors(log)
	if len(raised) != 1 {
		t.Fatalf("raised %d errors, want 1: %+v", len(raised), log.Records())
	}
	return raised[0]
}

// lastBeat is the moment raiseBeat's error is raised at.
var lastBeat = base.Add(time.Duration(Beat-1) * Period)

// documentNames answers the kind/name of every document an error names, in order.
func documentNames(t *testing.T, r dlog.Record) []string {
	t.Helper()
	docs, ok := r.Context["documents"].([]map[string]any)
	if !ok {
		t.Fatalf("documents = %#v, want the named documents", r.Context["documents"])
	}
	var out []string
	for _, doc := range docs {
		out = append(out, fmt.Sprintf("%s/%s", doc["kind"], doc["name"]))
	}
	return out
}

func raisedErrors(log *dlog.TestLogger) []dlog.Record {
	return dlog.FilterRecords(log.Records(), dlog.RecordQuery{Level: "error"})
}

func TestObserveRaisesOnlyAtTheBeatThreshold(t *testing.T) {
	tests := []struct {
		name   string
		stalls []time.Time
		raised int
	}{
		{name: "raises when Beat stalls fall on one phase", stalls: onBeat(base, Beat, 0), raised: 1},
		{name: "stays quiet one stall short of Beat", stalls: onBeat(base, Beat-1, 0), raised: 0},
		{
			name: "stays quiet for stalls spread across the cycle",
			stalls: func() []time.Time {
				out := make([]time.Time, 20)
				for i := range out {
					out[i] = base.Add(time.Duration(i) * 29 * time.Second)
				}
				return out
			}(),
			raised: 0,
		},
		{name: "counts a stall just inside Tolerance", stalls: append(onBeat(base, Beat-1, 0), base.Add(time.Duration(Beat)*Period+Tolerance-time.Millisecond)), raised: 1},
		{name: "does not count a stall just outside Tolerance", stalls: append(onBeat(base, Beat-1, 0), base.Add(time.Duration(Beat)*Period+Tolerance+time.Millisecond)), raised: 0},
		{name: "counts stalls either side of the cycle's wrap as one phase", stalls: append(onBeat(base, Beat/2, 100*time.Millisecond), onBeat(base.Add(time.Duration(Beat/2)*Period), Beat/2, -100*time.Millisecond)...), raised: 1},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			d, log := newDetector(t)

			// Act
			for _, at := range tt.stalls {
				d.Observe(host, at, 300)
			}

			// Assert
			if got := len(raisedErrors(log)); got != tt.raised {
				t.Fatalf("raised %d errors, want %d: %+v", got, tt.raised, log.Records())
			}
		})
	}
}

func TestObserveForgetsStallsOlderThanTheWindow(t *testing.T) {
	// Arrange: Beat-1 stalls, then one on the same phase long after the window.
	d, log := newDetector(t)
	for _, at := range onBeat(base, Beat-1, 0) {
		d.Observe(host, at, 300)
	}

	// Act
	d.Observe(host, base.Add(Window+time.Duration(Beat)*Period), 300)

	// Assert
	if got := len(raisedErrors(log)); got != 0 {
		t.Fatalf("raised %d errors for stalls a window apart, want 0", got)
	}
}

func TestObserveRaisesOncePerWindowWhileThePatternHolds(t *testing.T) {
	// Arrange
	d, log := newDetector(t)

	// Act: a beat on every cycle for one window and a half.
	for _, at := range onBeat(base, int((Window+Window/2)/Period), 0) {
		d.Observe(host, at, 300)
	}

	// Assert
	if got := len(raisedErrors(log)); got != 2 {
		t.Fatalf("raised %d errors over a window and a half of beats, want 2", got)
	}
}

func TestObserveRecordsTheBeatsDiagnosis(t *testing.T) {
	// Arrange
	d, log := newDetector(t)

	// Act
	for _, at := range onBeat(base, Beat, 2*time.Second) {
		d.Observe(host, at, 250)
	}

	// Assert
	raised := raisedErrors(log)
	if len(raised) != 1 {
		t.Fatalf("raised %d errors, want 1", len(raised))
	}
	got := raised[0]
	want := dlog.Context{
		"document": host, "stalls_on_beat": Beat, "stalls_in_window": Beat, "stalls_by_chance": chanceBound(Beat) - 1, "blocked_ms_on_beat": int64(250 * Beat),
		"phase_ms": int64(2000), "window_s": int(Window.Seconds()), "period_s": int(Period.Seconds()),
	}
	if got.Operation != op {
		t.Fatalf("operation = %q, want %q", got.Operation, op)
	}
	for k, v := range want {
		if got.Context[k] != v {
			t.Fatalf("context[%q] = %#v, want %#v (whole: %+v)", k, got.Context[k], v, got.Context)
		}
	}
}

func TestNewRequiresEveryCollaborator(t *testing.T) {
	tests := []struct {
		name string
		deps Deps
	}{
		{name: "no logger", deps: Deps{Footprint: fakeFootprint{}}},
		{name: "no footprint", deps: Deps{Log: dlog.NewTestLogger()}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, err := New(tt.deps)

			// Assert
			if err == nil {
				t.Fatalf("New with %s succeeded, want an error", tt.name)
			}
		})
	}
}

func TestTheErrorNamesTheBeatingProcesssDocumentsLargestFirst(t *testing.T) {
	tests := []struct {
		name     string
		document string
		want     []string
	}{
		{name: "the host's process holds the host and every page", document: host,
			want: []string{"page/ws-big", "webhost/host", "page/ws-small"}},
		{name: "the sidebar's process holds the sidebar alone", document: sidebar,
			want: []string{"webhost/sidebar"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			d, log := newDetector(t)
			three := int64(3)
			d.Census(Document{Kind: KindPage, Name: "ws-small"}, lastBeat.Add(-time.Minute), Census{DomNodes: 900, FeedRows: 20, Animations: &three, Visibility: "hidden"})
			d.Census(Document{Kind: KindWebhost, Name: "sidebar"}, lastBeat.Add(-30*time.Second), Census{DomNodes: 8911})
			d.Census(Document{Kind: KindWebhost, Name: "host"}, lastBeat.Add(-30*time.Second), Census{DomNodes: 1200})
			d.Census(Document{Kind: KindPage, Name: "ws-big"}, lastBeat.Add(-10*time.Second), Census{DomNodes: 41000, FeedRows: 1200, Animations: &three, Visibility: "visible"})

			// Act
			got := raiseBeat(t, d, log, tt.document)

			// Assert
			if names := documentNames(t, got); fmt.Sprint(names) != fmt.Sprint(tt.want) {
				t.Fatalf("documents = %v, want %v", names, tt.want)
			}
		})
	}
}

func TestTheErrorStatesAPagesCensusWhole(t *testing.T) {
	// Arrange
	d, log := newDetector(t)
	three := int64(3)
	d.Census(Document{Kind: KindPage, Name: "ws-a"}, lastBeat.Add(-10*time.Second), Census{DomNodes: 900, FeedRows: 20, Animations: &three, Visibility: "hidden"})

	// Act
	got := raiseBeat(t, d, log, host)

	// Assert
	doc := got.Context["documents"].([]map[string]any)[0]
	want := map[string]any{"kind": "page", "name": "ws-a", "dom_nodes": int64(900), "feed_rows": int64(20), "animations": int64(3), "visibility": "hidden", "census_age_s": int64(10)}
	if fmt.Sprint(doc) != fmt.Sprint(want) {
		t.Fatalf("document = %v, want %v", doc, want)
	}
}

func TestTheErrorLeavesOutWhatAWebhostCensusDoesNotCount(t *testing.T) {
	// Arrange
	d, log := newDetector(t)
	d.Census(Document{Kind: KindWebhost, Name: "host"}, lastBeat, Census{DomNodes: 1200, FeedRows: 4})

	// Act
	got := raiseBeat(t, d, log, host)

	// Assert
	doc := got.Context["documents"].([]map[string]any)[0]
	if _, ok := doc["animations"]; ok {
		t.Fatalf("document = %v, want no animations for a webhost document", doc)
	}
	if _, ok := doc["visibility"]; ok {
		t.Fatalf("document = %v, want no visibility for a webhost document", doc)
	}
}

func TestTheErrorLeavesOutADocumentSilentForCensusFresh(t *testing.T) {
	// Arrange
	d, log := newDetector(t)
	d.Census(Document{Kind: KindPage, Name: "ws-gone"}, lastBeat.Add(-CensusFresh), Census{DomNodes: 5000})
	d.Census(Document{Kind: KindPage, Name: "ws-here"}, lastBeat.Add(-CensusFresh+time.Second), Census{DomNodes: 10})

	// Act
	got := raiseBeat(t, d, log, host)

	// Assert
	if names := documentNames(t, got); fmt.Sprint(names) != "[page/ws-here]" {
		t.Fatalf("documents = %v, want only the census still standing", names)
	}
}

func TestALaterCensusReplacesItsDocumentsEarlierOne(t *testing.T) {
	// Arrange
	d, log := newDetector(t)
	doc := Document{Kind: KindPage, Name: "ws-a"}
	d.Census(doc, lastBeat.Add(-time.Minute), Census{DomNodes: 100})
	d.Census(doc, lastBeat.Add(-10*time.Second), Census{DomNodes: 700})

	// Act
	got := raiseBeat(t, d, log, host)

	// Assert
	docs := got.Context["documents"].([]map[string]any)
	if len(docs) != 1 || docs[0]["dom_nodes"] != int64(700) {
		t.Fatalf("documents = %v, want the one document at its later census", docs)
	}
}

func TestTheErrorMessageNamesTheLargestDocument(t *testing.T) {
	tests := []struct {
		name string
		docs bool
		want string
	}{
		{name: "with documents", docs: true, want: "1 documents are loaded in it, the largest document is page ws-a at 900 elements"},
		{name: "with none", want: "0 documents are loaded in it, no document has stated its size"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			d, log := newDetector(t)
			if tt.docs {
				d.Census(Document{Kind: KindPage, Name: "ws-a"}, lastBeat, Census{DomNodes: 900})
			}

			// Act
			got := raiseBeat(t, d, log, host)

			// Assert
			if !strings.Contains(got.Message, tt.want) {
				t.Fatalf("message = %q, want it to contain %q", got.Message, tt.want)
			}
		})
	}
}

func TestTheErrorStatesTheWebFootprint(t *testing.T) {
	tests := []struct {
		name      string
		footprint fakeFootprint
		want      map[string]any
	}{
		{name: "a sampled footprint", footprint: fakeFootprint{read: true, reading: webfootprint.Reading{At: lastBeat.Add(-20 * time.Second), Processes: 3, LargestBytes: 2 << 30}},
			want: map[string]any{"largest_web_content_footprint_mb": int64(2048), "web_content_processes": 3, "footprint_age_s": int64(20)}},
		{name: "no footprint sampled yet", footprint: fakeFootprint{}, want: map[string]any{"footprint_sampled": false}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			d, log := newDetectorWith(t, tt.footprint)

			// Act
			got := raiseBeat(t, d, log, host)

			// Assert
			for k, v := range tt.want {
				if got.Context[k] != v {
					t.Fatalf("context[%q] = %#v, want %#v (whole: %+v)", k, got.Context[k], v, got.Context)
				}
			}
		})
	}
}

func TestRecentCountsEveryDocumentsStallsAndTheirBlockedTime(t *testing.T) {
	// Arrange
	d, _ := newDetector(t)
	start := time.Unix(1_800_000_000, 0)
	d.Observe(host, start, 100)
	d.Observe(sidebar, start.Add(time.Minute), 250)

	// Act
	count, blocked := d.Recent(start.Add(2 * time.Minute))

	// Assert
	if count != 2 || blocked != 350 {
		t.Fatalf("Recent = (%d, %d), want (2, 350)", count, blocked)
	}
}

func TestRecentLeavesOutAStallOlderThanTheWindow(t *testing.T) {
	// Arrange
	d, _ := newDetector(t)
	start := time.Unix(1_800_000_000, 0)
	d.Observe(host, start, 100)
	d.Observe(host, start.Add(9*time.Minute), 250)

	// Act
	count, blocked := d.Recent(start.Add(Window + time.Second))

	// Assert
	if count != 1 || blocked != 250 {
		t.Fatalf("Recent = (%d, %d), want only the stall inside the window", count, blocked)
	}
}

// cyclesInWindow is how many whole cycles one window holds.
const cyclesInWindow = int(Window / Period)

// spread answers N moments across one window whose phases lie evenly across
// 2s to 28s of the cycle, clear of phase 0, a few to any one phase, and
// whose cycles take turns, so every stretch of the window is as busy.
func spread(n int) []time.Time {
	out := make([]time.Time, n)
	for j := range out {
		cycle := time.Duration(j%cyclesInWindow) * Period
		phase := 2*time.Second + time.Duration(j)*26*time.Second/time.Duration(n)
		out[j] = base.Add(cycle + phase)
	}
	return out
}

// beatsAcross answers N moments on phase 0 in cycles spaced evenly across one
// window, at most cyclesInWindow of them, so the beat builds as the window
// fills rather than ahead of it.
func beatsAcross(n int) []time.Time {
	out := make([]time.Time, n)
	for i := range out {
		out[i] = base.Add(time.Duration(i*cyclesInWindow/n) * Period)
	}
	return out
}

func TestChanceBoundIsWhatUniformStallsReachOnOnePhaseOnlyRarely(t *testing.T) {
	tests := []struct {
		stalls int
		want   int
	}{
		{stalls: 0, want: 1},
		{stalls: 20, want: 7},
		{stalls: 67, want: 11},
		{stalls: 92, want: 13},
		{stalls: 368, want: 30},
	}
	for _, tt := range tests {
		t.Run(fmt.Sprintf("%d stalls", tt.stalls), func(t *testing.T) {
			// Act
			got := chanceBound(tt.stalls)

			// Assert
			if got != tt.want {
				t.Fatalf("chanceBound(%d) = %d, want %d", tt.stalls, got, tt.want)
			}
		})
	}
}

func TestObserveRaisesABusyWindowOnlyAboveChance(t *testing.T) {
	tests := []struct {
		name   string
		onBeat int
		spread int
		raised int
	}{
		{name: "stays quiet for Beat on one phase of a busy window, as chance puts there", onBeat: Beat, spread: 84, raised: 0},
		{name: "stays quiet one stall short of chance's bound", onBeat: chanceBound(92) - 1, spread: 92 - (chanceBound(92) - 1), raised: 0},
		{name: "raises at chance's bound", onBeat: chanceBound(92), spread: 92 - chanceBound(92), raised: 1},
		{name: "stays quiet for 19 on one phase of 364, as a busy window put there on 2026-11-16", onBeat: 19, spread: 364 - 19, raised: 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: the beat and the busy stalls across one window, in
			// the order the webhost reports them.
			d, log := newDetector(t)
			stalls := append(spread(tt.spread), beatsAcross(tt.onBeat)...)
			sort.Slice(stalls, func(a, b int) bool { return stalls[a].Before(stalls[b]) })

			// Act
			for _, at := range stalls {
				d.Observe(host, at, 300)
			}

			// Assert
			if got := len(raisedErrors(log)); got != tt.raised {
				t.Fatalf("raised %d errors, want %d: %+v", got, tt.raised, log.Records())
			}
		})
	}
}

func TestObserveCountsEachDocumentsStallsAgainstItsOwnBeat(t *testing.T) {
	tests := []struct {
		name      string
		documents []string
		perDoc    int
		raised    []string
	}{
		{name: "two documents' stalls on one phase never add up to a beat", documents: []string{host, sidebar}, perDoc: Beat / 2, raised: nil},
		{name: "each document's own beat raises once, naming it", documents: []string{host, sidebar}, perDoc: Beat, raised: []string{host, sidebar}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			d, log := newDetector(t)

			// Act
			for _, at := range onBeat(base, tt.perDoc, 0) {
				for _, document := range tt.documents {
					d.Observe(document, at, 300)
				}
			}

			// Assert
			raised := raisedErrors(log)
			if len(raised) != len(tt.raised) {
				t.Fatalf("raised %d errors, want %d: %+v", len(raised), len(tt.raised), raised)
			}
			for i, document := range tt.raised {
				if raised[i].Context["document"] != document || raised[i].Context["stalls_in_window"] != tt.perDoc {
					t.Fatalf("error %d context = %+v, want document %q with %d stalls", i, raised[i].Context, document, tt.perDoc)
				}
			}
		})
	}
}
