package runtimemetrics

import (
	"errors"
	"fmt"
	"math"
	"sort"
	"sync"

	"agent-repld/internal/causelog"
	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"github.com/prometheus/client_golang/prometheus"
)

// LatencyName is the histogram every reported latency lands in, labeled by
// `latency` (its proto/vocab/latencies.json name) and `workspace`.
const LatencyName = "agentrepl_latency_seconds"

// The vendor traffic counters, folded from each shim's measurements.
const (
	VendorBytesSentName     = "agentrepl_vendor_bytes_sent_total"
	VendorBytesReceivedName = "agentrepl_vendor_bytes_received_total"
	// ShimHeapName is each workspace's shim's JavaScript heap at its last
	// measurements frame, by kind (used, total, external).
	ShimHeapName = "agentrepl_shim_heap_bytes"
)

// LatencyKind is one live-reported latency: its vocabulary name and its
// histogram buckets. proto/vocab/latencies.json is the record both tables
// are tested against.
type LatencyKind struct {
	Name    string
	Buckets []float64
}

var (
	navigationBuckets = []float64{0.016, 0.033, 0.05, 0.1, 0.2, 0.3, 0.5, 1, 2, 5}
	pageBootBuckets   = []float64{0.05, 0.1, 0.2, 0.3, 0.5, 1, 2, 5, 10, 30}
	vendorBuckets     = []float64{0.5, 1, 2, 3, 5, 8, 13, 21, 34, 60}
	eventLoopBuckets  = []float64{0.001, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5}
)

// ReportedKinds are the latencies Emacs and the webapp report.
var ReportedKinds = map[agentreplv1.ReportedLatencyKind]LatencyKind{
	agentreplv1.ReportedLatencyKind_REPORTED_LATENCY_KIND_SWITCH_REPEAT: {Name: "switch_repeat", Buckets: navigationBuckets},
	agentreplv1.ReportedLatencyKind_REPORTED_LATENCY_KIND_SWITCH_FIRST:  {Name: "switch_first", Buckets: navigationBuckets},
	agentreplv1.ReportedLatencyKind_REPORTED_LATENCY_KIND_PAGE_BOOT:     {Name: "page_boot", Buckets: pageBootBuckets},
}

// WorkspaceOpen is the daemon-measured latency from an OpenWorkspace's
// arrival to its success, when the workspace's session is up.
var WorkspaceOpen = LatencyKind{Name: "workspace_open", Buckets: []float64{0.1, 0.25, 0.5, 1, 2, 3, 5, 8, 13, 30}}

// DaemonKinds are the latencies the daemon measures itself, keyed by the
// vocabulary's reported_as spelling.
var DaemonKinds = map[string]LatencyKind{
	"DAEMON_WORKSPACE_OPEN": WorkspaceOpen,
}

// ShimKinds are the latencies the shim measures.
var ShimKinds = map[shimv1.ShimLatencyKind]LatencyKind{
	shimv1.ShimLatencyKind_SHIM_LATENCY_KIND_FIRST_OUTPUT:     {Name: "vendor_first_output", Buckets: vendorBuckets},
	shimv1.ShimLatencyKind_SHIM_LATENCY_KIND_EVENT_LOOP_DELAY: {Name: "shim_event_loop_delay", Buckets: eventLoopBuckets},
}

// histogram is one series' running distribution.
type histogram struct {
	buckets []float64 // upper bounds
	counts  []uint64  // cumulative per bound
	count   uint64
	sum     float64
}

func newHistogram(bounds []float64) *histogram {
	return &histogram{buckets: bounds, counts: make([]uint64, len(bounds))}
}

func (h *histogram) observe(seconds float64) {
	for i, b := range h.buckets {
		if seconds <= b {
			h.counts[i]++
		}
	}
	h.count++
	h.sum += seconds
}

// metric answers the histogram as one constant series of desc.
func (h *histogram) metric(desc *prometheus.Desc, labels ...string) prometheus.Metric {
	buckets := make(map[float64]uint64, len(h.buckets))
	for i, b := range h.buckets {
		buckets[b] = h.counts[i]
	}
	return prometheus.MustNewConstHistogram(desc, h.count, h.sum, buckets, labels...)
}

// seriesKey names one series.
type seriesKey struct {
	latency   string
	workspace ids.WorkspaceID
}

// shimRun is the last measurements frame folded for one workspace's shim,
// within one counting run.
type shimRun struct {
	since int64
	// traffic is the run's last traffic figures, nil while the shim has not
	// counted any.
	traffic *shimv1.ShimVendorTraffic
	hists   map[shimv1.ShimLatencyKind]*shimv1.ShimLatencyHistogram
}

// traffic is one workspace's folded vendor traffic.
type traffic struct{ sent, received float64 }

// latencies holds every latency histogram and the vendor traffic counters
// the daemon exposes: client observations add to them directly, shim
// measurements are folded in from their cumulative figures. It is a
// prometheus.Collector reading only its own state, so a scrape never waits
// on anything else.
type latencies struct {
	log    dlog.Logger
	causes causelog.Tracker

	mu      sync.Mutex
	series  map[seriesKey]*histogram
	traffic map[ids.WorkspaceID]*traffic
	runs    map[ids.WorkspaceID]*shimRun
	// heaps is each workspace's shim's heap at its last frame that carried
	// one, within its current counting run.
	heaps map[ids.WorkspaceID]*shimv1.ShimHeap

	latencyDesc, sentDesc, receivedDesc, heapDesc *prometheus.Desc
}

func newLatencies(log dlog.Logger) *latencies {
	return &latencies{
		log:          log,
		series:       map[seriesKey]*histogram{},
		traffic:      map[ids.WorkspaceID]*traffic{},
		runs:         map[ids.WorkspaceID]*shimRun{},
		heaps:        map[ids.WorkspaceID]*shimv1.ShimHeap{},
		heapDesc:     prometheus.NewDesc(ShimHeapName, "A workspace's shim's JavaScript heap at its last measurements frame, by kind (used, total, external).", []string{"workspace", "kind"}, nil),
		latencyDesc:  prometheus.NewDesc(LatencyName, "How long a user-facing interaction took, by latency (proto/vocab/latencies.json) and workspace.", []string{"latency", "workspace"}, nil),
		sentDesc:     prometheus.NewDesc(VendorBytesSentName, "Bytes a workspace's shim sent to the vendor API since the daemon started.", []string{"workspace"}, nil),
		receivedDesc: prometheus.NewDesc(VendorBytesReceivedName, "Bytes a workspace's shim received from the vendor API since the daemon started.", []string{"workspace"}, nil),
	}
}

// ErrInvalidLatency is a latency a client should never have sent.
var ErrInvalidLatency = errors.New("invalid latency")

// validateReported checks one client latency, answering its kind.
func validateReported(l *agentreplv1.ReportedLatency) (LatencyKind, error) {
	kind, ok := ReportedKinds[l.GetKind()]
	if !ok {
		return LatencyKind{}, fmt.Errorf("%w: kind %v is not a reported latency", ErrInvalidLatency, l.GetKind())
	}
	if s := l.GetSeconds(); math.IsNaN(s) || math.IsInf(s, 0) || s < 0 {
		return LatencyKind{}, fmt.Errorf("%w: %s took %v seconds", ErrInvalidLatency, kind.Name, s)
	}
	return kind, nil
}

// observe adds one client latency.
func (l *latencies) observe(ws ids.WorkspaceID, kind LatencyKind, seconds float64) {
	l.mu.Lock()
	defer l.mu.Unlock()
	key := seriesKey{latency: kind.Name, workspace: ws}
	h := l.series[key]
	if h == nil {
		h = newHistogram(kind.Buckets)
		l.series[key] = h
	}
	h.observe(seconds)
}

// foldShim folds one shim measurements frame for ws. Within a counting run
// it adds each figure's growth since the run's last frame; a frame from a new
// run (a different counting instant) adds its figures whole. A frame that
// breaks the shim's own contract (an unknown kind, buckets that differ from
// the vocabulary's, a figure that went down within one run) is refused whole,
// recorded once per cause, and leaves the folded state as it was.
func (l *latencies) foldShim(ws ids.WorkspaceID, m *shimv1.ShimMeasurements) {
	if err := validateShim(m); err != nil {
		if l.causes.Failed("shim:"+string(ws), err.Error()) {
			l.log.Error(Operation, "a shim's measurements were refused; its figures are not folded until it sends valid ones", dlog.Context{
				"workspace_id": string(ws), "error": err.Error(),
			})
		}
		return
	}
	l.mu.Lock()
	defer l.mu.Unlock()
	prev := l.runs[ws]
	sameRun := prev != nil && prev.since == m.GetCountedSinceUnixMs()
	if sameRun {
		if err := regressed(prev, m); err != nil {
			if l.causes.Failed("shim:"+string(ws), err.Error()) {
				l.log.Error(Operation, "a shim's measurements went backwards within one counting run; the frame is refused", dlog.Context{
					"workspace_id": string(ws), "error": err.Error(),
				})
			}
			return
		}
	}
	if l.causes.Succeeded("shim:" + string(ws)) {
		l.log.Info(Operation, "a shim's measurements are folded again", dlog.Context{"workspace_id": string(ws)})
	}
	run := &shimRun{since: m.GetCountedSinceUnixMs(), hists: map[shimv1.ShimLatencyKind]*shimv1.ShimLatencyHistogram{}}
	if vt := m.GetVendorTraffic(); vt != nil {
		// A shim that counts no traffic leaves it unset, and no series is
		// published for it rather than a zero.
		t := l.traffic[ws]
		if t == nil {
			t = &traffic{}
			l.traffic[ws] = t
		}
		if sameRun && prev.traffic != nil {
			t.sent += float64(vt.GetBytesSent() - prev.traffic.GetBytesSent())
			t.received += float64(vt.GetBytesReceived() - prev.traffic.GetBytesReceived())
		} else {
			t.sent += float64(vt.GetBytesSent())
			t.received += float64(vt.GetBytesReceived())
		}
		run.traffic = vt
	} else if sameRun {
		run.traffic = prev.traffic
	}
	// THE HEAP IS A SNAPSHOT: the frame's own replaces the last, and a new
	// counting run with none yet drops the old process's.
	switch {
	case m.GetHeap() != nil:
		l.heaps[ws] = m.GetHeap()
	case !sameRun:
		delete(l.heaps, ws)
	}
	for _, sh := range m.GetLatencies() {
		kind := ShimKinds[sh.GetKind()]
		key := seriesKey{latency: kind.Name, workspace: ws}
		h := l.series[key]
		if h == nil {
			h = newHistogram(kind.Buckets)
			l.series[key] = h
		}
		var before *shimv1.ShimLatencyHistogram
		if sameRun {
			before = prev.hists[sh.GetKind()]
		}
		for i, b := range sh.GetBuckets() {
			grew := b.GetCumulativeCount()
			if before != nil {
				grew -= before.GetBuckets()[i].GetCumulativeCount()
			}
			h.counts[i] += grew
		}
		grewCount, grewSum := sh.GetCount(), sh.GetSumSeconds()
		if before != nil {
			grewCount -= before.GetCount()
			grewSum -= before.GetSumSeconds()
		}
		h.count += grewCount
		h.sum += grewSum
		run.hists[sh.GetKind()] = sh
	}
	if sameRun {
		// A kind the frame left out keeps the run's last figures for it.
		for k, v := range prev.hists {
			if _, ok := run.hists[k]; !ok {
				run.hists[k] = v
			}
		}
	}
	l.runs[ws] = run
}

// validateShim checks a frame against the shim contract and the vocabulary.
func validateShim(m *shimv1.ShimMeasurements) error {
	if m.GetCountedSinceUnixMs() <= 0 {
		return fmt.Errorf("counted_since_unix_ms is %d", m.GetCountedSinceUnixMs())
	}
	seen := map[shimv1.ShimLatencyKind]bool{}
	for _, h := range m.GetLatencies() {
		kind, ok := ShimKinds[h.GetKind()]
		if !ok {
			return fmt.Errorf("latency kind %v is not a shim latency", h.GetKind())
		}
		if seen[h.GetKind()] {
			return fmt.Errorf("latency %s appears twice", kind.Name)
		}
		seen[h.GetKind()] = true
		if len(h.GetBuckets()) != len(kind.Buckets) {
			return fmt.Errorf("latency %s has %d buckets, the vocabulary %d", kind.Name, len(h.GetBuckets()), len(kind.Buckets))
		}
		var last uint64
		for i, b := range h.GetBuckets() {
			if b.GetUpperBoundSeconds() != kind.Buckets[i] {
				return fmt.Errorf("latency %s bucket %d ends at %v, the vocabulary's at %v", kind.Name, i, b.GetUpperBoundSeconds(), kind.Buckets[i])
			}
			if b.GetCumulativeCount() < last {
				return fmt.Errorf("latency %s bucket %d counts fewer than the bucket before it", kind.Name, i)
			}
			last = b.GetCumulativeCount()
		}
		if h.GetCount() < last {
			return fmt.Errorf("latency %s counts %d in total but %d at or below its last bound", kind.Name, h.GetCount(), last)
		}
	}
	return nil
}

// regressed answers an error when any figure of m is below the same run's
// last frame.
func regressed(prev *shimRun, m *shimv1.ShimMeasurements) error {
	if vt, was := m.GetVendorTraffic(), prev.traffic; vt != nil && was != nil &&
		(vt.GetBytesSent() < was.GetBytesSent() || vt.GetBytesReceived() < was.GetBytesReceived()) {
		return fmt.Errorf("vendor traffic went from %d/%d to %d/%d bytes", was.GetBytesSent(), was.GetBytesReceived(), vt.GetBytesSent(), vt.GetBytesReceived())
	}
	for _, h := range m.GetLatencies() {
		before := prev.hists[h.GetKind()]
		if before == nil {
			continue
		}
		if h.GetCount() < before.GetCount() || h.GetSumSeconds() < before.GetSumSeconds() {
			return fmt.Errorf("latency %s count went from %d to %d", ShimKinds[h.GetKind()].Name, before.GetCount(), h.GetCount())
		}
		for i, b := range h.GetBuckets() {
			if b.GetCumulativeCount() < before.GetBuckets()[i].GetCumulativeCount() {
				return fmt.Errorf("latency %s bucket %d went down", ShimKinds[h.GetKind()].Name, i)
			}
		}
	}
	return nil
}

// forget deletes every series of ws: the workspace left the registry.
func (l *latencies) forget(ws ids.WorkspaceID) {
	l.mu.Lock()
	defer l.mu.Unlock()
	for key := range l.series {
		if key.workspace == ws {
			delete(l.series, key)
		}
	}
	delete(l.traffic, ws)
	delete(l.runs, ws)
	delete(l.heaps, ws)
}

// Describe implements prometheus.Collector.
func (l *latencies) Describe(ch chan<- *prometheus.Desc) {
	ch <- l.latencyDesc
	ch <- l.sentDesc
	ch <- l.receivedDesc
	ch <- l.heapDesc
}

// Collect implements prometheus.Collector.
func (l *latencies) Collect(ch chan<- prometheus.Metric) {
	l.mu.Lock()
	defer l.mu.Unlock()
	keys := make([]seriesKey, 0, len(l.series))
	for k := range l.series {
		keys = append(keys, k)
	}
	sort.Slice(keys, func(i, j int) bool {
		if keys[i].latency != keys[j].latency {
			return keys[i].latency < keys[j].latency
		}
		return keys[i].workspace < keys[j].workspace
	})
	for _, k := range keys {
		ch <- l.series[k].metric(l.latencyDesc, k.latency, string(k.workspace))
	}
	workspaces := make([]ids.WorkspaceID, 0, len(l.traffic))
	for ws := range l.traffic {
		workspaces = append(workspaces, ws)
	}
	sort.Slice(workspaces, func(i, j int) bool { return workspaces[i] < workspaces[j] })
	for _, ws := range workspaces {
		t := l.traffic[ws]
		ch <- prometheus.MustNewConstMetric(l.sentDesc, prometheus.CounterValue, t.sent, string(ws))
		ch <- prometheus.MustNewConstMetric(l.receivedDesc, prometheus.CounterValue, t.received, string(ws))
	}
	for ws, h := range l.heaps {
		ch <- prometheus.MustNewConstMetric(l.heapDesc, prometheus.GaugeValue, float64(h.GetUsedBytes()), string(ws), "used")
		ch <- prometheus.MustNewConstMetric(l.heapDesc, prometheus.GaugeValue, float64(h.GetTotalBytes()), string(ws), "total")
		ch <- prometheus.MustNewConstMetric(l.heapDesc, prometheus.GaugeValue, float64(h.GetExternalBytes()), string(ws), "external")
	}
}
