// Package deployedversion resolves the agent-repl version the daemon deploys
// from, and keeps it current for everything that states it: the metrics
// surface's version series (runtimemetrics) and the rail's header
// (frontend.v1.WorkspaceRoster.version).
//
// It is resolved off every hot path, by one Loop, once when the daemon starts
// and again after every deploy, so no reader ever runs git.
package deployedversion

import (
	"context"
	"errors"
	"fmt"

	"agent-repld/internal/causelog"
	"agent-repld/internal/dlog"
	"agent-repld/internal/semver"
)

// Operation names every record the loop writes.
const Operation = "daemon.deployed_version"

// Git is the git the version is read through.
type Git interface {
	ResolveRef(ctx context.Context, repoDir, ref string) (string, error)
	TagsAt(ctx context.Context, dir, commit string) ([]string, error)
	TagsMerged(ctx context.Context, dir, commit string) ([]string, error)
}

// Version is a checkout's version: the release it is at, and the commit it is
// past that release by when it is between landings.
type Version struct {
	// Release is the highest version tag reachable from HEAD: HEAD's own on a
	// landing, the last landing's on a commit after it.
	Release semver.Version
	// Past is HEAD's commit, abbreviated to 12 hex digits, when HEAD carries
	// no version tag. Empty on a landing.
	Past string
}

// String spells the version with Past as semver build metadata
// (`0.129.0+<12 hex>`), so a build between landings is never mistaken for
// the landing.
func (v Version) String() string {
	if v.Past == "" {
		return v.Release.String()
	}
	return v.Release.String() + "+" + v.Past
}

// Resolve answers dir's version: the greatest version tag on HEAD, or, when
// HEAD carries none, the greatest one reachable from it, past by HEAD.
func Resolve(ctx context.Context, git Git, dir string) (Version, error) {
	head, err := git.ResolveRef(ctx, dir, "HEAD")
	if err != nil {
		return Version{}, fmt.Errorf("resolve HEAD of %s: %w", dir, err)
	}
	at, err := git.TagsAt(ctx, dir, head)
	if err != nil {
		return Version{}, fmt.Errorf("read the tags on %s's HEAD %s: %w", dir, head, err)
	}
	if v, ok := semver.Highest(at); ok {
		return Version{Release: v}, nil
	}
	reachable, err := git.TagsMerged(ctx, dir, head)
	if err != nil {
		return Version{}, fmt.Errorf("read the tags reachable from %s's HEAD %s: %w", dir, head, err)
	}
	v, ok := semver.Highest(reachable)
	if !ok {
		return Version{}, errors.New("no version tag is reachable from HEAD of " + dir)
	}
	short := head
	if len(short) > 12 {
		short = short[:12]
	}
	return Version{Release: v, Past: short}, nil
}

// Sink states the deployed version. It is told each CHANGE of what the loop
// resolved: the version, or nil when it can no longer be resolved, which
// withdraws whatever the sink states (a stale version would misattribute
// everything after it). A sink starts out stating none.
type Sink interface {
	DeployedVersion(v *Version)
}

// Loop resolves the deployed version on Prime, once before the daemon serves,
// and again on every Kick while it Runs, and tells every sink each change.
type Loop struct {
	git   Git
	dir   string
	log   dlog.Logger
	sinks []Sink
	kick  chan struct{}

	// causes and last are touched only by the goroutine running the loop.
	causes causelog.Tracker
	last   *Version
}

// NewLoop answers the loop that keeps dir's version current in sinks.
func NewLoop(git Git, dir string, log dlog.Logger, sinks ...Sink) *Loop {
	return &Loop{git: git, dir: dir, log: log, sinks: sinks, kick: make(chan struct{}, 1)}
}

// Kick asks for a fresh resolution: a deploy finished. It never blocks; kicks
// that arrive while one is pending collapse into it.
func (l *Loop) Kick() {
	select {
	case l.kick <- struct{}{}:
	default:
	}
}

// Prime resolves the version once, on the caller's goroutine, before Run
// starts: the daemon primes it before it serves, so the first view any sink
// publishes already states it, and no client ever sees it arrive as a push of
// its own. A version that cannot be resolved is logged, never fatal: the
// views state none. Prime and Run never run at once.
func (l *Loop) Prime(ctx context.Context) { l.resolve(ctx) }

// Run re-resolves on every Kick until ctx ends.
func (l *Loop) Run(ctx context.Context) error {
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
// failure at ERROR, a new version at INFO. The sinks are told only a change.
func (l *Loop) resolve(ctx context.Context) {
	v, err := Resolve(ctx, l.git, l.dir)
	if err != nil && ctx.Err() != nil {
		return
	}
	if err != nil {
		if l.causes.Failed("resolve", err.Error()) {
			l.log.Error(Operation, "the deployed version could not be resolved; it is withdrawn until it can", dlog.Context{"checkout": l.dir, "error": err.Error()})
		}
		if l.last != nil {
			l.last = nil
			l.tell(nil)
		}
		return
	}
	l.causes.Succeeded("resolve")
	if l.last != nil && *l.last == v {
		return
	}
	l.last = &v
	l.log.Info(Operation, "the deployed version is resolved", dlog.Context{"checkout": l.dir, "version": v.String()})
	l.tell(&v)
}

// tell hands every sink the version, each its own copy.
func (l *Loop) tell(v *Version) {
	for _, sink := range l.sinks {
		if v == nil {
			sink.DeployedVersion(nil)
			continue
		}
		own := *v
		sink.DeployedVersion(&own)
	}
}
