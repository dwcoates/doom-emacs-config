// Package newsdigest is the daemon's DAILY CLAUDE NEWS DIGEST: once a day it
// reads the watched sources (DefaultSources), keeps only what is new since the
// previous digest, has Sonnet condense it into a frontend.v1.NewsDigestOverlay,
// and stands that overlay over the feed in every webview until one of them
// dismisses it.
//
// THE CADENCE IS DURABLE. One run every Every (24h), measured from the END of
// the previous run as recorded in the state store, so a daemon that restarts
// or crashes keeps the same cadence; a daemon that starts with a run overdue
// runs ONE, never a burst of the days it missed. A RefreshNewsDigest run
// moves the cadence exactly as a scheduled one does.
//
// ONE RUN AT A TIME, ACROSS PROCESSES. In process a TryLock refuses a second
// run (ErrAlreadyRunning); across processes a run holds an flock for its
// whole length, so an incumbent and its handover successor never run
// together. A scheduled run re-reads the cadence UNDER that lock, so a run the
// other daemon just finished is seen and not repeated. And only the daemon
// that SERVES (rollout's ServesIntake: not a joining successor, not an
// incumbent mid-handover) starts a scheduled run.
//
// THE STANDING DIGEST IS DURABLE TOO, and replayed: Republish reads it from
// the store into the topic every webview's WatchDaemon stream subscribes to,
// so a daemon that inherits a standing digest draws it, and a late webview is
// handed it. A dismiss in any webview publishes none to all of them.
//
// See docs/protobuf-design/news-digest.md and daemon/AGENTS.md "The news
// digest".
package newsdigest

import (
	"context"
	"errors"
	"fmt"
	"sync"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/clock"
	"claude-repld/internal/dlog"
	"claude-repld/internal/headless"
	"claude-repld/internal/publish"
	"claude-repld/internal/wsm"
)

// The records this package writes.
const (
	opRun      = "daemon.newsdigest.run"
	opSource   = "daemon.newsdigest.source"
	opModel    = "daemon.newsdigest.model"
	opSchedule = "daemon.newsdigest.schedule"
	opDismiss  = "daemon.newsdigest.dismiss"
	opStanding = "daemon.newsdigest.standing"
)

// Production windows.
const (
	// DefaultEvery is the cadence: one run a day, from the previous run's end.
	DefaultEvery = 24 * time.Hour
	// DefaultStartDelay is how long after the daemon starts the schedule is
	// first consulted: long enough for the boot's adoptions and the editor's
	// first opens to settle before a run fetches anything.
	DefaultStartDelay = 2 * time.Minute
	// DefaultRecheck is the longest the schedule waits between looks at the
	// cadence. A wait is measured on the monotonic clock, which a sleeping
	// laptop does not advance; re-reading the wall clock at least this often
	// keeps the cadence a wall-clock one. It is also how soon a schedule that
	// found the run held by another daemon, or itself not serving, looks again.
	DefaultRecheck = 15 * time.Minute
)

// ErrAlreadyRunning is a run refused because one is in flight, in this
// process or in another daemon on the same state root.
var ErrAlreadyRunning = errors.New("newsdigest: a digest run is already in progress")

// ErrNoSourceRead is a run in which every source failed to read.
var ErrNoSourceRead = errors.New("newsdigest: no source could be read")

// Store is the part of the state client the digest uses.
type Store interface {
	NewsDigestState(ctx context.Context) (wsm.NewsDigestState, error)
	RecordNewsDigestRun(ctx context.Context, run wsm.NewsDigestRun) error
	DismissNewsDigest(ctx context.Context, id string) (bool, error)
}

// Deps are the digest's collaborators and windows.
type Deps struct {
	// Sources are the watched sources, in display order. REQUIRED.
	Sources []Source
	// Fetcher reads a source. REQUIRED.
	Fetcher Fetcher
	// Headless runs the condensing call. REQUIRED.
	Headless headless.Runner
	// PromptsDir holds the brief, read at use time. REQUIRED.
	PromptsDir string
	// ConfigDir is the account the condensing call bills; empty leaves the
	// environment's own.
	ConfigDir string
	// Store is the state client. REQUIRED.
	Store Store
	// Clock is the digest's view of time. REQUIRED.
	Clock clock.Clock
	// LockPath is the kernel lock one run holds for its whole length.
	// REQUIRED.
	LockPath string
	// Serves answers whether this daemon is the one that serves, and so the
	// one that starts scheduled runs. REQUIRED.
	Serves func() bool
	// MintID mints a digest's opaque id (wsm.NewNewsDigestID). REQUIRED.
	MintID func() string
	// Every, StartDelay and Recheck are the windows; see the defaults.
	Every, StartDelay, Recheck time.Duration
	// ModelTimeout bounds the condensing call; zero is DefaultModelTimeout.
	ModelTimeout time.Duration
	// Log is the global logger: the digest is no workspace's. REQUIRED.
	Log dlog.Logger
}

// Digester makes, stands and dismisses the news digest.
type Digester struct {
	deps      Deps
	condenser condenser
	// running is held for one run's whole length; TryLock is the refusal.
	running sync.Mutex
	// standingMu serializes every change of the standing digest with its
	// publication, so the topic never shows an order the store does not.
	standingMu sync.Mutex
	topic      publish.Topic[*agentreplv1.NewsDigestStanding]
}

// New builds the digester, refusing a missing collaborator or a
// non-positive window rather than running on a zero value.
func New(deps Deps) (*Digester, error) {
	switch {
	case len(deps.Sources) == 0:
		return nil, errors.New("newsdigest: Sources are required")
	case deps.Fetcher == nil:
		return nil, errors.New("newsdigest: Fetcher is required")
	case deps.Headless == nil:
		return nil, errors.New("newsdigest: Headless is required")
	case deps.PromptsDir == "":
		return nil, errors.New("newsdigest: PromptsDir is required")
	case deps.Store == nil:
		return nil, errors.New("newsdigest: Store is required")
	case deps.Clock == nil:
		return nil, errors.New("newsdigest: Clock is required")
	case deps.LockPath == "":
		return nil, errors.New("newsdigest: LockPath is required")
	case deps.Serves == nil:
		return nil, errors.New("newsdigest: Serves is required")
	case deps.MintID == nil:
		return nil, errors.New("newsdigest: MintID is required")
	case deps.Log == nil:
		return nil, errors.New("newsdigest: Log is required")
	case deps.Every <= 0, deps.StartDelay <= 0, deps.Recheck <= 0:
		return nil, fmt.Errorf("newsdigest: the windows must be positive (every %v, start delay %v, recheck %v)",
			deps.Every, deps.StartDelay, deps.Recheck)
	}
	if err := validateSources(deps.Sources); err != nil {
		return nil, err
	}
	timeout := deps.ModelTimeout
	if timeout == 0 {
		timeout = DefaultModelTimeout
	}
	return &Digester{
		deps: deps,
		condenser: condenser{
			headless: deps.Headless, promptsDir: deps.PromptsDir,
			configDir: deps.ConfigDir, timeout: timeout,
		},
	}, nil
}

// validateSources refuses a source list with a blank or repeated key, a
// missing name, URL or home, or no format.
func validateSources(sources []Source) error {
	seen := map[string]bool{}
	for i, s := range sources {
		switch {
		case s.Key == "":
			return fmt.Errorf("newsdigest: source %d has no key", i)
		case seen[s.Key]:
			return fmt.Errorf("newsdigest: the source key %q is repeated", s.Key)
		case s.Name == "", s.URL == "", s.Home == "":
			return fmt.Errorf("newsdigest: source %q needs a name, a url and a home", s.Key)
		case s.Format <= formatUnset || s.Format > FormatPage:
			return fmt.Errorf("newsdigest: source %q has no format", s.Key)
		}
		seen[s.Key] = true
	}
	return nil
}

// Topic is the standing, pushed on every webview's WatchDaemon stream.
func (d *Digester) Topic() *publish.Topic[*agentreplv1.NewsDigestStanding] { return &d.topic }
