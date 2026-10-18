package runtimemetrics

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"sync"

	"agent-repld/internal/dlog"

	"github.com/prometheus/client_golang/prometheus"
)

// DeployedVersionFile is the file, under the metrics root, the build writes
// the deployed checkout's version into on every run
// (docs/METRICS-JUDGEMENT-CALLS.md, call 3).
const DeployedVersionFile = "deployed-version"

// DeployedVersionName is the series naming the deployed version, value 1.
const DeployedVersionName = "agentrepl_deployed_version_info"

// DeployedVersionPath answers the deployed-version file under root.
func DeployedVersionPath(root string) string { return filepath.Join(root, DeployedVersionFile) }

// versionPattern is `git describe --tags --match '[0-9]*.[0-9]*.[0-9]*'
// --dirty`'s output: a release tag, optionally the commits past it and the
// abbreviated commit, optionally dirty.
var versionPattern = regexp.MustCompile(`^[0-9]+\.[0-9]+\.[0-9]+(-[0-9]+-g[0-9a-f]+)?(-dirty)?$`)

// deployedVersionCollector reads the deployed-version file at every scrape,
// so a deploy that changed only the webapp or the shim still moves the
// version without restarting the daemon.
//
// A file it cannot read or that holds no version is an expected condition (a
// build that predates the file, a hand-edited root), so it does not fail the
// scrape: the version series is omitted and the failure is recorded once
// per change of cause, never once per scrape.
type deployedVersionCollector struct {
	log  dlog.Logger
	path string
	desc *prometheus.Desc

	mu        sync.Mutex
	lastCause string
}

func newDeployedVersionCollector(log dlog.Logger, path string) *deployedVersionCollector {
	return &deployedVersionCollector{
		log:  log,
		path: path,
		desc: prometheus.NewDesc(DeployedVersionName, "Always 1. The version label is the version of the deployed checkout.", []string{"version"}, nil),
	}
}

func (c *deployedVersionCollector) Describe(ch chan<- *prometheus.Desc) { ch <- c.desc }

func (c *deployedVersionCollector) Collect(ch chan<- prometheus.Metric) {
	version, err := readDeployedVersion(c.path)
	c.note(version, err)
	if err != nil {
		return
	}
	ch <- prometheus.MustNewConstMetric(c.desc, prometheus.GaugeValue, 1, version)
}

// note records a change of outcome: a new cause of failure at ERROR, a
// recovery at INFO.
func (c *deployedVersionCollector) note(version string, err error) {
	cause := ""
	if err != nil {
		cause = err.Error()
	}
	c.mu.Lock()
	changed := cause != c.lastCause
	recovered := changed && cause == "" && c.lastCause != ""
	c.lastCause = cause
	c.mu.Unlock()
	switch {
	case changed && err != nil:
		c.log.Error(Operation, "the deployed version could not be read; the version series is omitted until it can", dlog.Context{"path": c.path, "error": cause})
	case recovered:
		c.log.Info(Operation, "the deployed version is readable again", dlog.Context{"path": c.path, "version": version})
	}
}

// readDeployedVersion reads and validates the version file.
func readDeployedVersion(path string) (string, error) {
	raw, err := os.ReadFile(path)
	if err != nil {
		return "", fmt.Errorf("read the deployed version: %w", err)
	}
	version := strings.TrimSpace(string(raw))
	if !versionPattern.MatchString(version) {
		return "", fmt.Errorf("the deployed version file holds %q, which is not a `git describe` version", version)
	}
	return version, nil
}
