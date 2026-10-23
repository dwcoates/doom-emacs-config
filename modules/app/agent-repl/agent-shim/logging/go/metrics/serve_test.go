package metrics

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"testing"

	"github.com/prometheus/client_golang/prometheus"

	"agentrepl/logging/metrics/storeimport"
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
	if len(got[0].Labels) != 2 || got[0].Labels["instance"] != "daemon" || got[0].Labels["run"] != Run {
		t.Fatalf("labels = %v, want instance=daemon and run=%s", got[0].Labels, Run)
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
	root := t.TempDir()
	newFakeStore(t, root, http.StatusNoContent)
	s, err := Open(Config{Program: ProgramStore, Root: root, Registry: newTestRegistry(t, ProgramStore), OnError: sink.record})
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

func TestRunNamesThisProcess(t *testing.T) {
	// Arrange
	pid, started, ok := strings.Cut(Run, "-")

	// Assert
	if !ok || pid != strconv.Itoa(os.Getpid()) || started == "" {
		t.Fatalf("Run = %q, want <pid %d>-<start nanoseconds>", Run, os.Getpid())
	}
	if _, err := strconv.ParseInt(started, 10, 64); err != nil {
		t.Fatalf("Run's start %q is not nanoseconds: %v", started, err)
	}
}

// fakeStore is a metrics store's import endpoint that keeps every push.
type fakeStore struct {
	mu     sync.Mutex
	pushes []*http.Request
	bodies []string
}

// newFakeStore stands an import endpoint answering status and names it in
// root's StoreFile.
func newFakeStore(t *testing.T, root string, status int) *fakeStore {
	t.Helper()
	fs := &fakeStore{}
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		body, _ := io.ReadAll(r.Body)
		fs.mu.Lock()
		fs.pushes = append(fs.pushes, r)
		fs.bodies = append(fs.bodies, string(body))
		fs.mu.Unlock()
		w.WriteHeader(status)
	}))
	t.Cleanup(srv.Close)
	writeStoreFile(t, root, fmt.Sprintf(`{"import_url":%q,"job":"agent-repl"}`, srv.URL+"/api/v1/import/prometheus"))
	return fs
}

func writeStoreFile(t *testing.T, root, body string) {
	t.Helper()
	if err := os.WriteFile(filepath.Join(root, StoreFile), []byte(body), 0o644); err != nil {
		t.Fatal(err)
	}
}

func TestCloseImportsTheFinalValuesLabeledAsTheScrapesLabelThem(t *testing.T) {
	// Arrange
	sink := &errSink{}
	root := t.TempDir()
	store := newFakeStore(t, root, http.StatusNoContent)
	s, err := Open(Config{Program: ProgramDaemon, Root: root, Registry: newTestRegistry(t, ProgramDaemon), OnError: sink.record})
	if err != nil {
		t.Fatalf("Open: %v", err)
	}

	// Act
	err = s.Close()

	// Assert
	if err != nil {
		t.Fatalf("Close: %v", err)
	}
	store.mu.Lock()
	defer store.mu.Unlock()
	if len(store.pushes) != 1 || store.pushes[0].Method != http.MethodPost || store.pushes[0].URL.Path != "/api/v1/import/prometheus" {
		t.Fatalf("pushes = %d, want one POST to the import endpoint", len(store.pushes))
	}
	labels := store.pushes[0].URL.Query()["extra_label"]
	want := []string{"job=agent-repl", "instance=daemon", "run=" + Run}
	if strings.Join(labels, ",") != strings.Join(want, ",") {
		t.Fatalf("extra labels = %v, want %v", labels, want)
	}
	if !strings.Contains(store.bodies[0], "agentrepl_build_info{") {
		t.Fatalf("the push carries no build info:\n%s", store.bodies[0])
	}
	if ops, _ := sink.snapshot(); len(ops) != 0 {
		t.Fatalf("a successful push reported errors: %v", ops)
	}
}

func TestAPushThatCannotHappenIsRecordedAndTheListenerStillStops(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, root string)
		want    string
	}{
		{name: "no store file", arrange: func(*testing.T, string) {}, want: "read the store's address"},
		{name: "a store file without an address", arrange: func(t *testing.T, root string) { writeStoreFile(t, root, `{"job":"agent-repl"}`) }, want: "names no import_url and job"},
		{name: "a store that refuses", arrange: func(t *testing.T, root string) { newFakeStore(t, root, http.StatusBadRequest) }, want: "were refused"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			sink := &errSink{}
			root := t.TempDir()
			tt.arrange(t, root)
			s, err := Open(Config{Program: ProgramStore, Root: root, Registry: newTestRegistry(t, ProgramStore), OnError: sink.record})
			if err != nil {
				t.Fatalf("Open: %v", err)
			}

			// Act
			err = s.Close()

			// Assert
			if err != nil {
				t.Fatalf("Close = %v, want the listener stopped all the same", err)
			}
			ops, errs := sink.snapshot()
			if len(ops) != 1 || ops[0] != "metrics.push" || !strings.Contains(errs[0].Error(), tt.want) {
				t.Fatalf("recorded %v %v, want one metrics.push failure saying %q", ops, errs, tt.want)
			}
			if _, err := http.Get("http://" + s.Addr() + Path); err == nil {
				t.Fatalf("the listener still answers after Close")
			}
		})
	}
}

// TestTheSurfacePushesThroughTheSharedStoreImport pins the one store file
// every pusher reads: a runtime's final values and the test runner's timings
// name the same file through storeimport.
func TestTheSurfacePushesThroughTheSharedStoreImport(t *testing.T) {
	if StoreFile != storeimport.File {
		t.Fatalf("StoreFile = %q, storeimport.File = %q", StoreFile, storeimport.File)
	}
}
