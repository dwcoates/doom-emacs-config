package runtimemetrics

import (
	"context"
	"errors"
	"fmt"
	"sync"
	"sync/atomic"

	"agent-repld/internal/dlog"
	"agent-repld/internal/semver"

	"github.com/prometheus/client_golang/prometheus"
)

// DeployedVersionName is the series naming the deployed version, value 1.
const DeployedVersionName = "agentrepl_deployed_version_info"

// VersionGit is the git the deployed version is read through.
type VersionGit interface {
	ResolveRef(ctx context.Context, repoDir, ref string) (string, error)
	TagsAt(ctx context.Context, dir, commit string) ([]string, error)
	TagsMerged(ctx context.Context, dir, commit string) ([]string, error)
}

// deployedVersion is the version the daemon last resolved for its checkout,
// exposed as DeployedVersionName (docs/METRICS-JUDGEMENT-CALLS.md, call 3).
// It is resolved off every hot path, by DeployedVersionLoop, and only read
// here, so a scrape runs no git.
type deployedVersion struct {
	desc    *prometheus.Desc
	current atomic.Pointer[string]
}

func newDeployedVersion() *deployedVersion {
	return &deployedVersion{
		desc: prometheus.NewDesc(DeployedVersionName, "Always 1. The version label is the version of the checkout the daemon deploys from.", []string{"version"}, nil),
	}
}

func (d *deployedVersion) Describe(ch chan<- *prometheus.Desc) { ch <- d.desc }

func (d *deployedVersion) Collect(ch chan<- prometheus.Metric) {
	if v := d.current.Load(); v != nil {
		ch <- prometheus.MustNewConstMetric(d.desc, prometheus.GaugeValue, 1, *v)
	}
}

// ResolveDeployedVersion answers dir's version: the greatest version tag on
// HEAD, or, when HEAD carries none, the greatest one reachable from it with
// HEAD's commit as semver build metadata (`0.129.0+<12 hex>`), so a build
// between landings is never mistaken for the landing.
func ResolveDeployedVersion(ctx context.Context, git VersionGit, dir string) (string, error) {
	head, err := git.ResolveRef(ctx, dir, "HEAD")
	if err != nil {
		return "", fmt.Errorf("resolve HEAD of %s: %w", dir, err)
	}
	at, err := git.TagsAt(ctx, dir, head)
	if err != nil {
		return "", fmt.Errorf("read the tags on %s's HEAD %s: %w", dir, head, err)
	}
	if v, ok := semver.Highest(at); ok {
		return v.String(), nil
	}
	reachable, err := git.TagsMerged(ctx, dir, head)
	if err != nil {
		return "", fmt.Errorf("read the tags reachable from %s's HEAD %s: %w", dir, head, err)
	}
	v, ok := semver.Highest(reachable)
	if !ok {
		return "", errors.New("no version tag is reachable from HEAD of " + dir)
	}
	short := head
	if len(short) > 12 {
		short = short[:12]
	}
	return v.String() + "+" + short, nil
}

// VersionLoop resolves the deployed version once when it starts and again
// on every Kick, until its context ends.
type VersionLoop struct {
	version *deployedVersion
	git     VersionGit
	dir     string
	log     dlog.Logger
	kick    chan struct{}

	mu        sync.Mutex
	lastCause string
	lastValue string
}

// DeployedVersionLoop answers the loop that keeps the deployed version
// current for dir. On the off surface the loop resolves nothing and only
// waits for its context.
func (s *Surface) DeployedVersionLoop(git VersionGit, dir string) *VersionLoop {
	l := &VersionLoop{git: git, dir: dir, kick: make(chan struct{}, 1)}
	if s != nil {
		l.version, l.log = s.version, s.log
	}
	return l
}

// Kick asks for a fresh resolution: a deploy finished. It never blocks; kicks
// that arrive while one is pending collapse into it.
func (l *VersionLoop) Kick() {
	select {
	case l.kick <- struct{}{}:
	default:
	}
}

// Run drives the loop until ctx ends.
func (l *VersionLoop) Run(ctx context.Context) error {
	if l.version == nil {
		<-ctx.Done()
		return nil
	}
	l.resolve(ctx)
	for {
		select {
		case <-ctx.Done():
			return nil
		case <-l.kick:
			l.resolve(ctx)
		}
	}
}

// resolve reads the version and records a change of outcome: a new cause of
// failure at ERROR (the series is withdrawn, because a stale version would
// misattribute everything measured after it), a new version at INFO.
func (l *VersionLoop) resolve(ctx context.Context) {
	v, err := ResolveDeployedVersion(ctx, l.git, l.dir)
	if err != nil && ctx.Err() != nil {
		return
	}
	l.mu.Lock()
	defer l.mu.Unlock()
	if err != nil {
		l.version.current.Store(nil)
		l.lastValue = ""
		if cause := err.Error(); cause != l.lastCause {
			l.lastCause = cause
			l.log.Error(Operation, "the deployed version could not be resolved; the version series is withdrawn until it can", dlog.Context{"checkout": l.dir, "error": cause})
		}
		return
	}
	l.lastCause = ""
	l.version.current.Store(&v)
	if v != l.lastValue {
		l.lastValue = v
		l.log.Info(Operation, "the deployed version is resolved", dlog.Context{"checkout": l.dir, "version": v})
	}
}
