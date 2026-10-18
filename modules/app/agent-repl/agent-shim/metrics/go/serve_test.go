package metrics

import (
	"encoding/json"
	"errors"
	"io"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"

	"github.com/prometheus/client_golang/prometheus"
)

// errSink records every error the surface hands its caller.
type errSink struct {
	mu   sync.Mutex
	ops  []string
	errs []error
}

func (s *errSink) record(op string, err error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.ops = append(s.ops, op)
	s.errs = append(s.errs, err)
}

func (s *errSink) snapshot() ([]string, []error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]string(nil), s.ops...), append([]error(nil), s.errs...)
}

func newTestRegistry(t *testing.T, program string) *prometheus.Registry {
	t.Helper()
	reg, err := NewRegistry(program, "deadbeef")
	if err != nil {
		t.Fatalf("NewRegistry: %v", err)
	}
	return reg
}

func openTestServer(t *testing.T, program string, reg *prometheus.Registry, sink *errSink) *Server {
	t.Helper()
	s, err := Open(Config{Program: program, Root: t.TempDir(), Registry: reg, OnError: sink.record})
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() { _ = s.Close() })
	return s
}

func get(t *testing.T, url string) (int, string) {
	t.Helper()
	resp, err := http.Get(url)
	if err != nil {
		t.Fatalf("GET %s: %v", url, err)
	}
	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		t.Fatalf("read %s: %v", url, err)
	}
	return resp.StatusCode, string(body)
}

func TestOpenRefusals(t *testing.T) {
	reg := newTestRegistry(t, ProgramDaemon)
	sink := &errSink{}
	tests := []struct {
		name    string
		cfg     Config
		wantErr string
	}{
		{name: "unknown program", cfg: Config{Program: "webapp", Root: "/r", Registry: reg, OnError: sink.record}, wantErr: `unknown program "webapp"`},
		{name: "no registry", cfg: Config{Program: ProgramDaemon, Root: "/r", OnError: sink.record}, wantErr: "without a registry or an error sink"},
		{name: "no error sink", cfg: Config{Program: ProgramDaemon, Root: "/r", Registry: reg}, wantErr: "without a registry or an error sink"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			s, err := Open(tt.cfg)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
				t.Fatalf("err = %v, want it to contain %q", err, tt.wantErr)
			}
			if s != nil {
				t.Fatalf("a refused surface was still answered")
			}
		})
	}
}

func TestOpenWithoutRootIsOff(t *testing.T) {
	// Act
	s, err := Open(Config{Program: ProgramDaemon})

	// Assert
	if err != nil || s != nil {
		t.Fatalf("Open with no root = (%v, %v), want (nil, nil)", s, err)
	}
}

func TestServeAnswersMetricsOnLoopbackOnly(t *testing.T) {
	// Arrange
	sink := &errSink{}
	s := openTestServer(t, ProgramSidecar, newTestRegistry(t, ProgramSidecar), sink)

	// Act
	status, body := get(t, "http://"+s.Addr()+Path)
	otherStatus, _ := get(t, "http://"+s.Addr()+"/debug/pprof/")

	// Assert
	host, _, err := net.SplitHostPort(s.Addr())
	if err != nil || host != "127.0.0.1" {
		t.Fatalf("Addr = %q, want a 127.0.0.1 host", s.Addr())
	}
	if status != http.StatusOK || !strings.Contains(body, `agentrepl_build_info{build="deadbeef",program="sidecar"} 1`) {
		t.Fatalf("GET /metrics = %d:\n%s", status, body)
	}
	if otherStatus != http.StatusNotFound {
		t.Fatalf("GET /debug/pprof/ = %d, want 404: the metrics surface serves nothing else", otherStatus)
	}
	if ops, _ := sink.snapshot(); len(ops) != 0 {
		t.Fatalf("errors recorded on a healthy scrape: %v", ops)
	}
}

// failingCollector fails every collection, as a broken collector would.
type failingCollector struct{ desc *prometheus.Desc }

func (c failingCollector) Describe(ch chan<- *prometheus.Desc) { ch <- c.desc }
func (c failingCollector) Collect(ch chan<- prometheus.Metric) {
	ch <- prometheus.NewInvalidMetric(c.desc, errors.New("the process table could not be read"))
}

func TestScrapeFailureReachesTheErrorSink(t *testing.T) {
	// Arrange
	reg := newTestRegistry(t, ProgramStore)
	reg.MustRegister(failingCollector{desc: prometheus.NewDesc("agentrepl_test_broken", "broken", nil, nil)})
	sink := &errSink{}
	s := openTestServer(t, ProgramStore, reg, sink)

	// Act
	status, _ := get(t, "http://"+s.Addr()+Path)

	// Assert
	if status != http.StatusInternalServerError {
		t.Fatalf("GET /metrics = %d, want 500 for a failed collection", status)
	}
	ops, errs := sink.snapshot()
	if len(ops) == 0 || ops[0] != "metrics.scrape" {
		t.Fatalf("ops = %v, want metrics.scrape", ops)
	}
	if !strings.Contains(errs[0].Error(), `"store"`) || !strings.Contains(errs[0].Error(), "the process table could not be read") {
		t.Fatalf("err = %v, want the program and the cause", errs[0])
	}
}

func TestAdvertiseWritesTheTargetFile(t *testing.T) {
	// Arrange
	sink := &errSink{}
	s := openTestServer(t, ProgramDaemon, newTestRegistry(t, ProgramDaemon), sink)
	if err := os.MkdirAll(filepath.Dir(s.TargetPath()), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(s.TargetPath(), []byte(`[{"targets":["127.0.0.1:1"]}]`), 0o644); err != nil {
		t.Fatal(err)
	}

	// Act
	err := s.Advertise()

	// Assert
	if err != nil {
		t.Fatalf("Advertise: %v", err)
	}
	raw, err := os.ReadFile(s.TargetPath())
	if err != nil {
		t.Fatalf("read the target: %v", err)
	}
	var got []target
	if err := json.Unmarshal(raw, &got); err != nil {
		t.Fatalf("decode %s: %v", raw, err)
	}
	if len(got) != 1 || len(got[0].Targets) != 1 || got[0].Targets[0] != s.Addr() {
		t.Fatalf("targets = %+v, want only %s (an earlier instance's target replaced)", got, s.Addr())
	}
	if got[0].Labels["instance"] != "daemon" || got[0].Labels["program"] != "daemon" {
		t.Fatalf("labels = %v, want instance and program both daemon", got[0].Labels)
	}
	leftovers, _ := filepath.Glob(filepath.Join(filepath.Dir(s.TargetPath()), ".*.tmp"))
	if len(leftovers) != 0 {
		t.Fatalf("staged files left behind: %v", leftovers)
	}
}

func TestAdvertiseFailsWhenTheTargetsDirectoryCannotExist(t *testing.T) {
	// Arrange: the root is a regular file, so no directory can live under it.
	root := filepath.Join(t.TempDir(), "root")
	if err := os.WriteFile(root, nil, 0o644); err != nil {
		t.Fatal(err)
	}
	sink := &errSink{}
	s, err := Open(Config{Program: ProgramStore, Root: root, Registry: newTestRegistry(t, ProgramStore), OnError: sink.record})
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	t.Cleanup(func() { _ = s.Close() })

	// Act
	err = s.Advertise()

	// Assert
	if err == nil || !strings.Contains(err.Error(), `create the targets directory for "store"`) {
		t.Fatalf("err = %v, want the directory refusal", err)
	}
}

func TestCloseStopsTheListenerAndKeepsTheTarget(t *testing.T) {
	// Arrange
	sink := &errSink{}
	s, err := Open(Config{Program: ProgramStore, Root: t.TempDir(), Registry: newTestRegistry(t, ProgramStore), OnError: sink.record})
	if err != nil {
		t.Fatalf("Open: %v", err)
	}
	if err := s.Advertise(); err != nil {
		t.Fatalf("Advertise: %v", err)
	}

	// Act
	err = s.Close()

	// Assert
	if err != nil {
		t.Fatalf("Close: %v", err)
	}
	if _, err := http.Get("http://" + s.Addr() + Path); err == nil {
		t.Fatalf("the listener still answers after Close")
	}
	if _, err := os.Stat(s.TargetPath()); err != nil {
		t.Fatalf("the target file was removed on Close: %v", err)
	}
	if ops, _ := sink.snapshot(); len(ops) != 0 {
		t.Fatalf("a clean Close reported errors: %v", ops)
	}
}
