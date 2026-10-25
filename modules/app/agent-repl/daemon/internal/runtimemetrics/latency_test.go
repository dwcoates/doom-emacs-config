package runtimemetrics

import (
	"context"
	"errors"
	"math"
	"strings"
	"testing"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"github.com/prometheus/client_golang/prometheus"
	dto "github.com/prometheus/client_model/go"
)

func openSurface(t *testing.T) (*Surface, *dlog.TestLogger) {
	t.Helper()
	log := dlog.NewTestLogger()
	s, err := Open(log, t.TempDir())
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(s.Close)
	return s, log
}

func mustContain(t *testing.T, body string, want ...string) {
	t.Helper()
	for _, w := range want {
		if !strings.Contains(body, w) {
			t.Fatalf("scrape lacks %s:\n%s", w, body)
		}
	}
}

func TestValidateReportedLatency(t *testing.T) {
	switchRepeat := agentreplv1.ReportedLatencyKind_REPORTED_LATENCY_KIND_SWITCH_REPEAT
	tests := []struct {
		name    string
		latency *agentreplv1.ReportedLatency
		want    string
	}{
		{name: "a known kind and a finite duration", latency: &agentreplv1.ReportedLatency{Kind: switchRepeat, Seconds: 0.08}, want: "switch_repeat"},
		{name: "an unspecified kind", latency: &agentreplv1.ReportedLatency{Seconds: 0.08}},
		{name: "a negative duration", latency: &agentreplv1.ReportedLatency{Kind: switchRepeat, Seconds: -1}},
		{name: "a NaN duration", latency: &agentreplv1.ReportedLatency{Kind: switchRepeat, Seconds: math.NaN()}},
		{name: "an infinite duration", latency: &agentreplv1.ReportedLatency{Kind: switchRepeat, Seconds: math.Inf(1)}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			kind, err := ValidateReportedLatency(tt.latency)

			// Assert
			if tt.want == "" {
				if !errors.Is(err, ErrInvalidLatency) {
					t.Fatalf("err = %v, want ErrInvalidLatency", err)
				}
				return
			}
			if err != nil || kind.Name != tt.want {
				t.Fatalf("ValidateReportedLatency = (%v, %v), want %s", kind, err, tt.want)
			}
		})
	}
}

func TestAHistogramIsServedAsOneCumulativeSeries(t *testing.T) {
	// Arrange
	h := newHistogram([]float64{1, 2})
	h.observe(0.5)
	h.observe(1.5)
	h.observe(3)
	desc := prometheus.NewDesc("x_seconds", "x", []string{"label"}, nil)

	// Act
	var got dto.Metric
	if err := h.metric(desc, "a").Write(&got); err != nil {
		t.Fatalf("Write: %v", err)
	}

	// Assert
	hist := got.GetHistogram()
	if hist.GetSampleCount() != 3 || hist.GetSampleSum() != 5 {
		t.Fatalf("count %d sum %v, want 3 and 5", hist.GetSampleCount(), hist.GetSampleSum())
	}
	want := map[float64]uint64{1: 1, 2: 2}
	for _, b := range hist.GetBucket() {
		if want[b.GetUpperBound()] != b.GetCumulativeCount() {
			t.Fatalf("bucket %v holds %d, want %d", b.GetUpperBound(), b.GetCumulativeCount(), want[b.GetUpperBound()])
		}
	}
	if l := got.GetLabel(); len(l) != 1 || l[0].GetValue() != "a" {
		t.Fatalf("labels = %v, want label=a", l)
	}
}

func TestAnObservedLatencyIsServedInItsBuckets(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)
	kind := ReportedKinds[agentreplv1.ReportedLatencyKind_REPORTED_LATENCY_KIND_SWITCH_REPEAT]

	// Act
	s.ObserveLatency("ws1", kind, 0.04)
	s.ObserveLatency("ws1", kind, 0.15)
	body := scrape(t, s)

	// Assert
	mustContain(t, body,
		`agentrepl_latency_seconds_bucket{latency="switch_repeat",workspace="ws1",le="0.033"} 0`,
		`agentrepl_latency_seconds_bucket{latency="switch_repeat",workspace="ws1",le="0.05"} 1`,
		`agentrepl_latency_seconds_bucket{latency="switch_repeat",workspace="ws1",le="0.2"} 2`,
		`agentrepl_latency_seconds_count{latency="switch_repeat",workspace="ws1"} 2`,
		`agentrepl_latency_seconds_sum{latency="switch_repeat",workspace="ws1"} 0.19`,
	)
}

func TestValidateEditorLatency(t *testing.T) {
	stall := agentreplv1.EditorLatencyKind_EDITOR_LATENCY_KIND_STALL
	tests := []struct {
		name    string
		latency *agentreplv1.ReportedEditorLatency
		want    string
	}{
		{name: "a stall", latency: &agentreplv1.ReportedEditorLatency{Kind: stall, Seconds: 0.2}, want: "editor_stall"},
		{name: "an elisp reload", latency: &agentreplv1.ReportedEditorLatency{Kind: agentreplv1.EditorLatencyKind_EDITOR_LATENCY_KIND_ELISP_RELOAD, Seconds: 0.2}, want: "elisp_reload"},
		{name: "an input wait", latency: &agentreplv1.ReportedEditorLatency{Kind: agentreplv1.EditorLatencyKind_EDITOR_LATENCY_KIND_INPUT_WAIT, Seconds: 0.2}, want: "editor_input_wait"},
		{name: "an unspecified kind", latency: &agentreplv1.ReportedEditorLatency{Seconds: 0.2}},
		{name: "a negative duration", latency: &agentreplv1.ReportedEditorLatency{Kind: stall, Seconds: -1}},
		{name: "a NaN duration", latency: &agentreplv1.ReportedEditorLatency{Kind: stall, Seconds: math.NaN()}},
		{name: "an infinite duration", latency: &agentreplv1.ReportedEditorLatency{Kind: stall, Seconds: math.Inf(1)}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			kind, err := ValidateEditorLatency(tt.latency)

			// Assert
			if tt.want == "" {
				if !errors.Is(err, ErrInvalidLatency) {
					t.Fatalf("err = %v, want ErrInvalidLatency", err)
				}
				return
			}
			if err != nil || kind.Name != tt.want {
				t.Fatalf("ValidateEditorLatency = (%v, %v), want %s", kind, err, tt.want)
			}
		})
	}
}

func TestAnEditorLatencyIsServedUnderNoWorkspace(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)

	// Act
	s.ObserveEditorLatency(EditorKinds[agentreplv1.EditorLatencyKind_EDITOR_LATENCY_KIND_STALL], 0.15)
	body := scrape(t, s)

	// Assert
	mustContain(t, body,
		`agentrepl_latency_seconds_bucket{latency="editor_stall",workspace="",le="0.1"} 0`,
		`agentrepl_latency_seconds_bucket{latency="editor_stall",workspace="",le="0.2"} 1`,
		`agentrepl_latency_seconds_count{latency="editor_stall",workspace=""} 1`,
	)
}

func TestAWorkspaceForgetKeepsTheEditorsLatencies(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)
	s.ObserveEditorLatency(EditorKinds[agentreplv1.EditorLatencyKind_EDITOR_LATENCY_KIND_STALL], 0.15)

	// Act
	s.ForgetWorkspace("ws1")

	// Assert
	mustContain(t, scrape(t, s), `agentrepl_latency_seconds_count{latency="editor_stall",workspace=""} 1`)
}

// frame builds a shim frame for counting run since with traffic and, when
// counts is set, a first-output histogram whose ten cumulative bucket counts
// are counts.
func frame(since int64, sent, received uint64, counts []uint64, total uint64, sum float64) *shimv1.ShimMeasurements {
	m := &shimv1.ShimMeasurements{
		CountedSinceUnixMs: since,
		VendorTraffic:      &shimv1.ShimVendorTraffic{BytesSent: sent, BytesReceived: received},
	}
	if counts != nil {
		h := &shimv1.ShimLatencyHistogram{Kind: shimv1.ShimLatencyKind_SHIM_LATENCY_KIND_FIRST_OUTPUT, Count: total, SumSeconds: sum}
		for i, b := range vendorBuckets {
			h.Buckets = append(h.Buckets, &shimv1.ShimLatencyBucket{UpperBoundSeconds: b, CumulativeCount: counts[i]})
		}
		m.Latencies = append(m.Latencies, h)
	}
	return m
}

func ones(n uint64) []uint64 {
	out := make([]uint64, len(vendorBuckets))
	for i := range out {
		out[i] = n
	}
	return out
}

func TestShimFramesFold(t *testing.T) {
	tests := []struct {
		name   string
		frames []*shimv1.ShimMeasurements
		want   []string
	}{
		{
			name:   "a first frame adds its figures whole",
			frames: []*shimv1.ShimMeasurements{frame(100, 500, 9000, ones(1), 1, 1.5)},
			want: []string{
				`agentrepl_vendor_bytes_sent_total{workspace="ws1"} 500`,
				`agentrepl_vendor_bytes_received_total{workspace="ws1"} 9000`,
				`agentrepl_latency_seconds_count{latency="vendor_first_output",workspace="ws1"} 1`,
			},
		},
		{
			name:   "a later frame of the same run adds only its growth",
			frames: []*shimv1.ShimMeasurements{frame(100, 500, 9000, ones(1), 1, 1.5), frame(100, 800, 9500, ones(3), 3, 4.5)},
			want: []string{
				`agentrepl_vendor_bytes_sent_total{workspace="ws1"} 800`,
				`agentrepl_vendor_bytes_received_total{workspace="ws1"} 9500`,
				`agentrepl_latency_seconds_count{latency="vendor_first_output",workspace="ws1"} 3`,
				`agentrepl_latency_seconds_sum{latency="vendor_first_output",workspace="ws1"} 4.5`,
			},
		},
		{
			name:   "a new counting run adds its figures whole, never going backwards",
			frames: []*shimv1.ShimMeasurements{frame(100, 500, 9000, ones(2), 2, 3), frame(200, 100, 1000, ones(1), 1, 1)},
			want: []string{
				`agentrepl_vendor_bytes_sent_total{workspace="ws1"} 600`,
				`agentrepl_vendor_bytes_received_total{workspace="ws1"} 10000`,
				`agentrepl_latency_seconds_count{latency="vendor_first_output",workspace="ws1"} 3`,
			},
		},
		{
			name:   "a frame that leaves a kind out keeps that kind's figures",
			frames: []*shimv1.ShimMeasurements{frame(100, 500, 9000, ones(2), 2, 3), frame(100, 600, 9000, nil, 0, 0), frame(100, 600, 9000, ones(4), 4, 6)},
			want: []string{
				`agentrepl_latency_seconds_count{latency="vendor_first_output",workspace="ws1"} 4`,
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := openSurface(t)

			// Act
			for _, f := range tt.frames {
				s.FoldShimMeasurements("ws1", f)
			}

			// Assert
			mustContain(t, scrape(t, s), tt.want...)
			for _, r := range log.Records() {
				if r.Level == "error" {
					t.Fatalf("a valid sequence logged an error: %+v", r)
				}
			}
		})
	}
}

func TestAShimThatCountsNoTrafficPublishesNoTrafficSeries(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)
	m := frame(100, 0, 0, ones(1), 1, 1)
	m.VendorTraffic = nil

	// Act
	s.FoldShimMeasurements("ws1", m)
	body := scrape(t, s)

	// Assert
	if strings.Contains(body, VendorBytesSentName+"{") || strings.Contains(body, VendorBytesReceivedName+"{") {
		t.Fatalf("an uncounted traffic was published:\n%s", body)
	}
	mustContain(t, body, `agentrepl_latency_seconds_count{latency="vendor_first_output",workspace="ws1"} 1`)
}

func TestAnInvalidShimFrameIsRefusedOncePerCause(t *testing.T) {
	badBound := frame(100, 1, 1, ones(1), 1, 1)
	badBound.Latencies[0].Buckets[0].UpperBoundSeconds = 0.25
	duplicate := frame(100, 1, 1, ones(1), 1, 1)
	duplicate.Latencies = append(duplicate.Latencies, duplicate.Latencies[0])
	unknown := frame(100, 1, 1, ones(1), 1, 1)
	unknown.Latencies[0].Kind = shimv1.ShimLatencyKind_SHIM_LATENCY_KIND_UNSPECIFIED
	shrinking := frame(100, 1, 1, []uint64{2, 1, 1, 1, 1, 1, 1, 1, 1, 1}, 2, 1)
	tests := []struct {
		name  string
		frame *shimv1.ShimMeasurements
		cause string
	}{
		{name: "a bucket bound that is not the vocabulary's", frame: badBound, cause: "bucket 0 ends at 0.25"},
		{name: "a kind sent twice", frame: duplicate, cause: "appears twice"},
		{name: "an unknown kind", frame: unknown, cause: "is not a shim latency"},
		{name: "buckets that shrink", frame: shrinking, cause: "counts fewer than the bucket before it"},
		{name: "no counting instant", frame: frame(0, 1, 1, nil, 0, 0), cause: "counted_since_unix_ms is 0"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := openSurface(t)

			// Act
			s.FoldShimMeasurements("ws1", tt.frame)
			s.FoldShimMeasurements("ws1", tt.frame)

			// Assert
			if strings.Contains(scrape(t, s), `workspace="ws1"`) {
				t.Fatalf("a refused frame was folded")
			}
			var errs []dlog.Record
			for _, r := range log.Records() {
				if r.Level == "error" {
					errs = append(errs, r)
				}
			}
			if len(errs) != 1 || errs[0].Context["workspace_id"] != "ws1" || !strings.Contains(errs[0].Context["error"].(string), tt.cause) {
				t.Fatalf("error records = %+v, want one naming %q", errs, tt.cause)
			}
		})
	}
}

func TestAShimFigureGoingBackwardsWithinARunIsRefused(t *testing.T) {
	// Arrange
	s, log := openSurface(t)
	s.FoldShimMeasurements("ws1", frame(100, 500, 9000, ones(2), 2, 3))

	// Act
	s.FoldShimMeasurements("ws1", frame(100, 400, 9000, ones(2), 2, 3))

	// Assert
	mustContain(t, scrape(t, s), `agentrepl_vendor_bytes_sent_total{workspace="ws1"} 500`)
	var refused bool
	for _, r := range log.Records() {
		if r.Level == "error" && strings.Contains(r.Context["error"].(string), "vendor traffic went from 500/9000 to 400/9000") {
			refused = true
		}
	}
	if !refused {
		t.Fatalf("no refusal record in %+v", log.Records())
	}
}

func TestAValidFrameAfterARefusalIsRecordedAsRecovered(t *testing.T) {
	// Arrange
	s, log := openSurface(t)
	s.FoldShimMeasurements("ws1", frame(0, 1, 1, nil, 0, 0))

	// Act
	s.FoldShimMeasurements("ws1", frame(100, 5, 5, nil, 0, 0))

	// Assert
	mustContain(t, scrape(t, s), `agentrepl_vendor_bytes_sent_total{workspace="ws1"} 5`)
	var recovered bool
	for _, r := range log.Records() {
		if r.Level == "info" && r.Message == "a shim's measurements are folded again" {
			recovered = true
		}
	}
	if !recovered {
		t.Fatalf("no recovery record in %+v", log.Records())
	}
}

// forgettingDB is a registry whose Forget succeeds or fails on demand.
type forgettingDB struct {
	wsm.DB
	err error
}

func (d forgettingDB) Forget(context.Context, wsm.WorkspaceID) (wsm.ForgetReport, error) {
	return wsm.ForgetReport{}, d.err
}

func TestObserveForgets(t *testing.T) {
	tests := []struct {
		name      string
		forgetErr error
		wantGone  bool
	}{
		{name: "a forgotten workspace's series are deleted", wantGone: true},
		{name: "a failed forget keeps them", forgetErr: errors.New("database is locked")},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := openSurface(t)
			s.ObserveLatency("ws1", ReportedKinds[agentreplv1.ReportedLatencyKind_REPORTED_LATENCY_KIND_PAGE_BOOT], 0.2)
			s.FoldShimMeasurements("ws1", frame(100, 5, 5, nil, 0, 0))
			s.ObserveLatency("ws2", ReportedKinds[agentreplv1.ReportedLatencyKind_REPORTED_LATENCY_KIND_PAGE_BOOT], 0.2)
			db := s.ObserveForgets(forgettingDB{err: tt.forgetErr})

			// Act
			_, err := db.Forget(context.Background(), "ws1")

			// Assert
			if !errors.Is(err, tt.forgetErr) {
				t.Fatalf("Forget = %v, want %v passed through", err, tt.forgetErr)
			}
			body := scrape(t, s)
			if gone := !strings.Contains(body, `workspace="ws1"`); gone != tt.wantGone {
				t.Fatalf("ws1 series gone = %v, want %v:\n%s", gone, tt.wantGone, body)
			}
			mustContain(t, body, `agentrepl_latency_seconds_count{latency="page_boot",workspace="ws2"} 1`)
		})
	}
}

func TestTheOffSurfaceObservesNothingAndWrapsNothing(t *testing.T) {
	// Arrange
	var s *Surface
	db := forgettingDB{}

	// Act
	wrapped := s.ObserveForgets(db)
	s.ObserveLatency("ws1", LatencyKind{}, 1)
	s.ObserveEditorLatency(LatencyKind{}, 1)
	s.FoldShimMeasurements("ws1", frame(1, 1, 1, nil, 0, 0))
	s.ForgetWorkspace("ws1")

	// Assert
	if wrapped != wsm.DB(db) || s.Enabled() {
		t.Fatalf("the off surface wrapped the registry or reports itself enabled")
	}
}

func TestAShimsHeapIsServedAsItsLastSnapshot(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)
	first := frame(100, 0, 0, nil, 0, 0)
	first.Heap = &shimv1.ShimHeap{UsedBytes: 10, TotalBytes: 20, ExternalBytes: 3}
	second := frame(100, 0, 0, nil, 0, 0)
	second.Heap = &shimv1.ShimHeap{UsedBytes: 15, TotalBytes: 20, ExternalBytes: 4}

	// Act
	s.FoldShimMeasurements("ws1", first)
	s.FoldShimMeasurements("ws1", second)
	body := scrape(t, s)

	// Assert
	mustContain(t, body,
		`agentrepl_shim_heap_bytes{kind="used",workspace="ws1"} 15`,
		`agentrepl_shim_heap_bytes{kind="total",workspace="ws1"} 20`,
		`agentrepl_shim_heap_bytes{kind="external",workspace="ws1"} 4`,
	)
}

func TestANewShimRunWithNoHeapYetDropsTheOldHeap(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)
	old := frame(100, 0, 0, nil, 0, 0)
	old.Heap = &shimv1.ShimHeap{UsedBytes: 10, TotalBytes: 20}
	s.FoldShimMeasurements("ws1", old)

	// Act: a restarted shim's first frame, no heap read yet.
	s.FoldShimMeasurements("ws1", frame(200, 0, 0, nil, 0, 0))
	body := scrape(t, s)

	// Assert
	if strings.Contains(body, "agentrepl_shim_heap_bytes{") {
		t.Fatalf("the old process's heap outlived its run:\n%s", body)
	}
}

func TestAForgottenWorkspaceLosesItsShimHeap(t *testing.T) {
	// Arrange
	s, _ := openSurface(t)
	f := frame(100, 0, 0, nil, 0, 0)
	f.Heap = &shimv1.ShimHeap{UsedBytes: 10}
	s.FoldShimMeasurements("ws1", f)

	// Act
	s.ForgetWorkspace("ws1")
	body := scrape(t, s)

	// Assert
	if strings.Contains(body, "agentrepl_shim_heap_bytes{") {
		t.Fatalf("a forgotten workspace's heap was still served:\n%s", body)
	}
}

func TestTheSurfaceAnswersTheRootItWasOpenedUnder(t *testing.T) {
	// Arrange
	log := dlog.NewTestLogger()
	root := t.TempDir()
	s, err := Open(log, root)
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(s.Close)
	var off *Surface

	// Act / Assert
	if s.Root() != root || off.Root() != "" {
		t.Fatalf("Root() = %q and off %q, want %q and empty", s.Root(), off.Root(), root)
	}
}
