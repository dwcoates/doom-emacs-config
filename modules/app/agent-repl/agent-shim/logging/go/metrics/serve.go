package metrics

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net"
	"net/http"
	"net/url"
	"os"
	"path/filepath"
	"time"

	"github.com/prometheus/client_golang/prometheus"
	"github.com/prometheus/client_golang/prometheus/promhttp"
	"github.com/prometheus/common/expfmt"
)

// Path is the one route a metrics surface serves.
const Path = "/metrics"

// TargetsDir is the directory under the metrics root holding one target file
// per program, the directory VictoriaMetrics' file_sd config reads.
const TargetsDir = "targets"

// shutdownGrace bounds Close: a scrape in flight, or the final push, must not
// hold up a runtime's exit.
const shutdownGrace = 2 * time.Second

// StoreFile is the file under the metrics root that names the metrics
// store's import endpoint and the scrape job its series belong to. The
// installer writes it beside scrape.yml (metrics/store.json).
const StoreFile = "store.json"

// store is StoreFile's content.
type store struct {
	// ImportURL takes Prometheus text exposition.
	ImportURL string `json:"import_url"`
	// Job is the scrape config's job_name, so a pushed sample lands on the
	// series its scrapes write.
	Job string `json:"job"`
}

// Config opens a metrics surface.
type Config struct {
	// Program is one of Programs.
	Program string
	// Root is the metrics root (production: ~/.cache/agent-repl/metrics).
	// Empty is OFF.
	Root string
	// Registry is what /metrics serves.
	Registry *prometheus.Registry
	// OnError receives every failure after Open returned: the listener's
	// serve loop ending, and an error encoding a scrape. The caller records it
	// through its own canonical logging helper. Required.
	OnError func(op string, err error)
}

// Server is an open metrics surface.
type Server struct {
	program  string
	root     string
	addr     string
	registry *prometheus.Registry
	srv      *http.Server
	done     chan struct{}
	onError  func(op string, err error)
	client   *http.Client
}

// Open listens on an ephemeral loopback port and serves Path. It does NOT
// advertise: the caller calls Advertise once it owns its role. An empty
// Config.Root answers (nil, nil): the surface is off.
func Open(cfg Config) (*Server, error) {
	if cfg.Root == "" {
		return nil, nil
	}
	if !ValidProgram(cfg.Program) {
		return nil, fmt.Errorf("metrics: unknown program %q (want one of %v)", cfg.Program, Programs)
	}
	if cfg.Registry == nil || cfg.OnError == nil {
		return nil, fmt.Errorf("metrics: program %q opened without a registry or an error sink", cfg.Program)
	}
	ln, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		return nil, fmt.Errorf("metrics: listen on loopback for %q: %w", cfg.Program, err)
	}
	mux := http.NewServeMux()
	mux.Handle(Path, promhttp.HandlerFor(cfg.Registry, promhttp.HandlerOpts{
		ErrorLog:      errorLog{program: cfg.Program, sink: cfg.OnError},
		ErrorHandling: promhttp.HTTPErrorOnError,
	}))
	s := &Server{
		program:  cfg.Program,
		root:     cfg.Root,
		addr:     ln.Addr().String(),
		registry: cfg.Registry,
		srv:      &http.Server{Handler: mux, ReadHeaderTimeout: 5 * time.Second},
		done:     make(chan struct{}),
		onError:  cfg.OnError,
		client:   &http.Client{},
	}
	go func() {
		defer close(s.done)
		if err := s.srv.Serve(ln); err != nil && !errors.Is(err, http.ErrServerClosed) {
			cfg.OnError("metrics.serve", fmt.Errorf("the metrics listener for %q at %s stopped: %w", cfg.Program, s.addr, err))
		}
	}()
	return s, nil
}

// Registry answers the registry the surface serves, for collectors a runtime
// registers after opening.
func (s *Server) Registry() *prometheus.Registry { return s.registry }

// Addr answers the loopback host:port the surface listens on.
func (s *Server) Addr() string { return s.addr }

// TargetPath answers the target file this surface advertises through.
func (s *Server) TargetPath() string { return TargetPath(s.root, s.program) }

// TargetPath answers program's target file under root.
func TargetPath(root, program string) string {
	return filepath.Join(root, TargetsDir, program+".json")
}

// Run is this process's run: its pid and the instant the package
// initialized, in nanoseconds. Every target this process advertises carries
// it as the `run` label, so each process run is its own set of series.
//
// A RUN IS ITS OWN SERIES BECAUSE EVERY COUNTER STARTS FROM ZERO WITH ITS
// PROCESS. Continuing one series across processes made a restart visible only
// when a counter went down, and a new process often counts as much as the old
// one by its first scrape: a handover's successor that booted once and started
// 17 workspaces read as no boot and a counter dropping from 18 to 17, which no
// query counts (docs/METRICS-JUDGEMENT-CALLS.md, call 12). Two processes cannot
// share a run: one pid is never live twice at the same nanosecond.
var Run = fmt.Sprintf("%d-%d", os.Getpid(), time.Now().UnixNano())

// target is one file_sd entry.
type target struct {
	Targets []string          `json:"targets"`
	Labels  map[string]string `json:"labels"`
}

// Advertise atomically writes the target file naming this surface, replacing
// whatever an earlier instance wrote. Its labels are `instance`, the program
// rather than the port, and `run` (Run), so each process run is its own set
// of series. There is no `program` target label: it would collide with the
// build info's own and push it aside as `exported_program`.
func (s *Server) Advertise() error {
	body, err := json.Marshal([]target{{
		Targets: []string{s.addr},
		Labels:  map[string]string{"instance": s.program, "run": Run},
	}})
	if err != nil {
		return fmt.Errorf("metrics: encode the target for %q: %w", s.program, err)
	}
	path := s.TargetPath()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return fmt.Errorf("metrics: create the targets directory for %q: %w", s.program, err)
	}
	tmp, err := os.CreateTemp(filepath.Dir(path), "."+s.program+".*.tmp")
	if err != nil {
		return fmt.Errorf("metrics: stage the target for %q: %w", s.program, err)
	}
	if _, err := tmp.Write(body); err != nil {
		tmp.Close()
		os.Remove(tmp.Name())
		return fmt.Errorf("metrics: write the target for %q: %w", s.program, err)
	}
	if err := tmp.Close(); err != nil {
		os.Remove(tmp.Name())
		return fmt.Errorf("metrics: close the staged target for %q: %w", s.program, err)
	}
	if err := os.Rename(tmp.Name(), path); err != nil {
		os.Remove(tmp.Name())
		return fmt.Errorf("metrics: publish the target for %q at %s: %w", s.program, path, err)
	}
	return nil
}

// Close pushes the surface's final values to the metrics store, then stops
// the listener. The target file stays: see the package comment.
//
// THE LAST VALUES ARE PUSHED, NOT LEFT FOR A SCRAPE THAT NEVER COMES. The
// store scrapes every 15 seconds, and whatever a process records after its
// last scrape was lost with it: an outgoing daemon times its handover's
// transfers and its deploy's decision after its target already names the
// successor, so no scrape ever read them. A push that fails is recorded
// through OnError and does not hold the exit.
func (s *Server) Close() error {
	ctx, cancel := context.WithTimeout(context.Background(), shutdownGrace)
	defer cancel()
	if err := s.push(ctx); err != nil {
		s.onError("metrics.push", err)
	}
	err := s.srv.Shutdown(ctx)
	<-s.done
	if err != nil {
		return fmt.Errorf("metrics: stop the listener for %q at %s: %w", s.program, s.addr, err)
	}
	return nil
}

// push sends every series the registry gathers to the store's import
// endpoint (StoreFile), labeled as this surface's scrapes label them.
func (s *Server) push(ctx context.Context) error {
	raw, err := os.ReadFile(filepath.Join(s.root, StoreFile))
	if err != nil {
		return fmt.Errorf("the final values of %q were not pushed: read the store's address: %w", s.program, err)
	}
	var st store
	if err := json.Unmarshal(raw, &st); err != nil || st.ImportURL == "" || st.Job == "" {
		return fmt.Errorf("the final values of %q were not pushed: %s names no import_url and job (%v)", s.program, StoreFile, err)
	}
	families, err := s.registry.Gather()
	if err != nil {
		return fmt.Errorf("the final values of %q were not pushed: gather: %w", s.program, err)
	}
	var body bytes.Buffer
	enc := expfmt.NewEncoder(&body, expfmt.NewFormat(expfmt.TypeTextPlain))
	for _, f := range families {
		if err := enc.Encode(f); err != nil {
			return fmt.Errorf("the final values of %q were not pushed: encode %s: %w", s.program, f.GetName(), err)
		}
	}
	target, err := url.Parse(st.ImportURL)
	if err != nil {
		return fmt.Errorf("the final values of %q were not pushed: import_url %q: %w", s.program, st.ImportURL, err)
	}
	q := target.Query()
	for _, l := range []string{"job=" + st.Job, "instance=" + s.program, "run=" + Run} {
		q.Add("extra_label", l)
	}
	target.RawQuery = q.Encode()
	req, err := http.NewRequestWithContext(ctx, http.MethodPost, target.String(), &body)
	if err != nil {
		return fmt.Errorf("the final values of %q were not pushed: %w", s.program, err)
	}
	resp, err := s.client.Do(req)
	if err != nil {
		return fmt.Errorf("the final values of %q were not pushed to %s: %w", s.program, st.ImportURL, err)
	}
	defer resp.Body.Close()
	if resp.StatusCode/100 != 2 {
		detail, _ := io.ReadAll(io.LimitReader(resp.Body, 512))
		return fmt.Errorf("the final values of %q were refused by %s: %s %s", s.program, st.ImportURL, resp.Status, bytes.TrimSpace(detail))
	}
	return nil
}

// errorLog adapts promhttp's error log onto the caller's sink.
type errorLog struct {
	program string
	sink    func(op string, err error)
}

func (l errorLog) Println(v ...interface{}) {
	l.sink("metrics.scrape", fmt.Errorf("a scrape of %q failed: %s", l.program, fmt.Sprint(v...)))
}
