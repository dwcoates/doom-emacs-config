package runtimemetrics

import (
	"context"
	"fmt"
	"os"
	"strings"
	"time"

	"agent-repld/internal/causelog"
	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"

	"agentrepl/logging/metrics"

	"github.com/prometheus/client_golang/prometheus"
)

// The state series: what agent-repl keeps on disk and how many workspaces
// it holds, read when scraped.
const (
	LogBytesName   = "agentrepl_log_bytes"
	LogFilesName   = "agentrepl_log_files"
	WorkspacesName = "agentrepl_workspaces"
)

// logRuntimes are the runtimes a log file's name can carry
// (agent-repl-<id>-<runtime>-<n>.log); any other file in the logs
// directory is counted under "other".
var logRuntimes = map[string]bool{"daemon": true, "shim": true, "webapp": true, "sidecar": true, "emacs": true}

// stateReadBound bounds the workspace read a scrape makes.
const stateReadBound = 2 * time.Second

// WorkspaceLister reads the registry's workspaces and repositories. wsm.DB
// is the production one.
type WorkspaceLister interface {
	ListWorkspaces(ctx context.Context) ([]wsm.Workspace, error)
	ListRepositories(ctx context.Context) ([]wsm.Repository, error)
}

// stateCollector serves the logs directory's size and file count per
// runtime, and the workspaces per repository and state.
type stateCollector struct {
	logsDir string
	lister  WorkspaceLister
	log     dlog.Logger
	causes  causelog.Tracker

	logBytes, logFiles, workspaces *prometheus.Desc
}

// logRuntime answers the runtime a log file's name carries.
func logRuntime(name string) string {
	if name == "daemon.run.log" {
		return "daemon"
	}
	rest, ok := strings.CutPrefix(name, "agent-repl-")
	if !ok {
		return "other"
	}
	parts := strings.SplitN(rest, "-", 3)
	if len(parts) < 2 || !logRuntimes[parts[1]] {
		return "other"
	}
	return parts[1]
}

// Describe implements prometheus.Collector.
func (c *stateCollector) Describe(ch chan<- *prometheus.Desc) {
	ch <- c.logBytes
	ch <- c.logFiles
	ch <- c.workspaces
}

// Collect implements prometheus.Collector. A read that fails is recorded once
// per cause and omits only what it would have served.
func (c *stateCollector) Collect(ch chan<- prometheus.Metric) {
	c.collectLogs(ch)
	c.collectWorkspaces(ch)
}

func (c *stateCollector) note(step string, err error) bool {
	return c.causes.Note(step, err, func(cause string) {
		c.log.Error(Operation, "a state metrics read failed; what it measures is omitted until it succeeds", dlog.Context{"step": step, "error": cause})
	}, func() {
		c.log.Info(Operation, "a state metrics read succeeds again", dlog.Context{"step": step})
	})
}

func (c *stateCollector) collectLogs(ch chan<- prometheus.Metric) {
	entries, err := os.ReadDir(c.logsDir)
	if !c.note("logs", err) {
		return
	}
	bytes, files := map[string]float64{}, map[string]float64{}
	var statErr error
	for _, e := range entries {
		if !e.Type().IsRegular() {
			continue
		}
		info, err := e.Info()
		if err != nil {
			if !os.IsNotExist(err) {
				statErr = fmt.Errorf("stat %s: %w", e.Name(), err)
			}
			continue
		}
		rt := logRuntime(e.Name())
		bytes[rt] += float64(info.Size())
		files[rt]++
	}
	c.note("log_stat", statErr)
	for rt, n := range files {
		ch <- prometheus.MustNewConstMetric(c.logBytes, prometheus.GaugeValue, bytes[rt], rt)
		ch <- prometheus.MustNewConstMetric(c.logFiles, prometheus.GaugeValue, n, rt)
	}
}

func (c *stateCollector) collectWorkspaces(ch chan<- prometheus.Metric) {
	ctx, cancel := context.WithTimeout(context.Background(), stateReadBound)
	defer cancel()
	repos, err := c.lister.ListRepositories(ctx)
	if !c.note("repositories", err) {
		return
	}
	workspaces, err := c.lister.ListWorkspaces(ctx)
	if !c.note("workspaces", err) {
		return
	}
	names := make(map[wsm.RepoID]string, len(repos))
	for _, r := range repos {
		names[r.ID] = r.Name
	}
	type key struct{ repo, state string }
	counts := map[key]float64{}
	for _, w := range workspaces {
		state := "open"
		switch {
		case w.Closed:
			state = "closed"
		case w.Hidden:
			state = "hidden"
		}
		counts[key{names[w.Repo], state}]++
	}
	for k, n := range counts {
		ch <- prometheus.MustNewConstMetric(c.workspaces, prometheus.GaugeValue, n, k.repo, k.state)
	}
}

// RegisterState registers the state collector (the logs under logsDir, the
// workspaces lister answers) and the sizes of the state database at dbPath
// (metrics.NewDBFiles, db="wsm"). The off surface registers nothing.
func (s *Surface) RegisterState(logsDir, dbPath string, lister WorkspaceLister) error {
	if s == nil {
		return nil
	}
	c := &stateCollector{
		logsDir: logsDir, lister: lister, log: s.log,
		logBytes:   prometheus.NewDesc(LogBytesName, "Bytes of agent-repl's log files, by the runtime that wrote them.", []string{"runtime"}, nil),
		logFiles:   prometheus.NewDesc(LogFilesName, "agent-repl's log files, by the runtime that wrote them.", []string{"runtime"}, nil),
		workspaces: prometheus.NewDesc(WorkspacesName, "Workspaces in the registry, by repository and state (open, hidden, closed).", []string{"repository", "state"}, nil),
	}
	if err := s.srv.Registry().Register(c); err != nil {
		return fmt.Errorf("runtimemetrics: register the state collector: %w", err)
	}
	if err := s.srv.Registry().Register(metrics.NewDBFiles("wsm", dbPath)); err != nil {
		return fmt.Errorf("runtimemetrics: register the state database's sizes: %w", err)
	}
	return nil
}
