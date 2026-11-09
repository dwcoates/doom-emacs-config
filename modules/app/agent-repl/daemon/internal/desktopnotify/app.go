package desktopnotify

import (
	"context"
	"errors"
	"fmt"
	"sync"
	"time"

	"agent-repld/internal/dlog"
)

// opApp is the operation the notifier app's records carry: its stream
// attaching, being superseded and ending, and each post's answer.
const opApp = "daemon.desktopnotify.app"

// AppProgram names the macOS notifier app, for the record.
const AppProgram = "Zettel.app"

// AppAnswerBound bounds the wait for the app's answer to one post: the app
// hands the banner to macOS's notification center and answers at once, which
// takes milliseconds; a wait past this is an app that took the banner and
// went silent.
const AppAnswerBound = 5 * time.Second

// ErrNoApp is a banner wanted while no notifier app holds a
// WatchDesktopBanners stream: nobody can post it, and it is not queued.
var ErrNoApp = errors.New("the Zettel notifier app holds no WatchDesktopBanners stream; is com.agentrepl.notifier running?")

// ErrNotAllowed is a banner macOS refused because the user has not allowed
// Zettel to post notifications.
var ErrNotAllowed = errors.New("macOS refused the banner: Zettel is not allowed to post notifications (System Settings → Notifications → Zettel)")

// ErrUnknownPost is an answer naming no post that awaits one: already
// answered, past its wait, or handed by another daemon.
var ErrUnknownPost = errors.New("no post with that id awaits an answer")

// AppBanner is one banner handed to the app: the post it answers by, the text
// it shows, and the target its group and click token name.
type AppBanner struct {
	// PostID is the post's identity, echoed in the app's answer.
	PostID uint64
	// Banner is the banner's text.
	Banner Banner
	// Target is the workspace the banner is about.
	Target Target
	// Group is the banner's replacement group.
	Group string
}

// PostOutcome is what the app reported the platform did with one banner.
type PostOutcome struct {
	// Posted is a banner the platform took.
	Posted bool
	// NotAllowed is a banner refused because Zettel may not notify.
	NotAllowed bool
	// PlatformError is the platform's own description of any other refusal.
	// Exactly one of Posted, NotAllowed, and a non-empty PlatformError holds.
	PlatformError string
}

// err answers the outcome as the error a refused post records, or nil.
func (o PostOutcome) err() error {
	switch {
	case o.Posted:
		return nil
	case o.NotAllowed:
		return ErrNotAllowed
	case o.PlatformError != "":
		return fmt.Errorf("macOS refused the banner: %s", o.PlatformError)
	default:
		panic("desktopnotify: a post outcome that is none of posted, not allowed, or failed")
	}
}

// AppStream is one notifier app's standing WatchDesktopBanners stream, as the
// server's handler holds it.
type AppStream struct {
	generation uint64
	pid        int
	build      string
	banners    chan AppBanner
	superseded chan struct{}
	gone       chan struct{}
}

// Banners delivers each banner to post, in order.
func (s *AppStream) Banners() <-chan AppBanner { return s.banners }

// Superseded is closed when a later stream takes this one's place.
func (s *AppStream) Superseded() <-chan struct{} { return s.superseded }

// App is the macOS notifier app (Zettel.app) as the daemon sees it: the one
// WatchDesktopBanners stream standing, and the posts awaiting its answers. It
// is the macOS Backend: Post hands a banner over the stream and returns once
// the app reports the platform took it.
//
// ONE STREAM STANDS. A later Attach supersedes the earlier stream, whose
// handler sends its last frame and ends; the superseded stream's release then
// leaves its successor standing.
type App struct {
	log   dlog.Logger
	bound time.Duration

	mu         sync.Mutex
	generation uint64
	stream     *AppStream
	nextPost   uint64
	waiting    map[uint64]chan PostOutcome
}

// NewApp builds the notifier app's side with no stream standing. bound is
// how long a post waits for the app's answer (AppAnswerBound in production).
func NewApp(log dlog.Logger, bound time.Duration) *App {
	if log == nil || bound <= 0 {
		panic("desktopnotify: NewApp requires a logger and a positive answer bound")
	}
	return &App{log: log, bound: bound, waiting: map[uint64]chan PostOutcome{}}
}

// Attach stands the stream of the app pid running build, superseding any
// earlier one, and answers it with the release its handler calls when the
// stream ends.
func (a *App) Attach(pid int, build string) (*AppStream, func()) {
	a.mu.Lock()
	a.generation++
	stream := &AppStream{
		generation: a.generation,
		pid:        pid,
		build:      build,
		banners:    make(chan AppBanner),
		superseded: make(chan struct{}),
		gone:       make(chan struct{}),
	}
	previous := a.stream
	a.stream = stream
	if previous != nil {
		close(previous.superseded)
	}
	a.mu.Unlock()
	ctx := dlog.Context{"pid": pid, "build": build, "generation": stream.generation}
	if previous != nil {
		ctx["superseded_pid"] = previous.pid
		ctx["superseded_generation"] = previous.generation
	}
	a.log.Info(opApp, "the notifier app attached its banner stream", ctx)
	return stream, func() { a.release(stream) }
}

// release ends one stream: its pending posts fail at once, and the slot
// empties unless a later stream already took it.
func (a *App) release(stream *AppStream) {
	a.mu.Lock()
	current := a.stream == stream
	if current {
		a.stream = nil
	}
	close(stream.gone)
	a.mu.Unlock()
	if !current {
		a.log.Debug(opApp, "a superseded notifier app stream ended; its successor stands", dlog.Context{
			"pid": stream.pid, "generation": stream.generation,
		})
		return
	}
	a.log.Info(opApp, "the notifier app's banner stream ended; no app posts banners until one attaches", dlog.Context{
		"pid": stream.pid, "generation": stream.generation,
	})
}

// RunningBuild is the build the standing stream's app reported; false when
// no app holds the stream. A deploy judges the app stale on it.
func (a *App) RunningBuild() (string, bool) {
	a.mu.Lock()
	defer a.mu.Unlock()
	if a.stream == nil {
		return "", false
	}
	return a.stream.build, true
}

// Program implements Backend.
func (a *App) Program() string { return AppProgram }

// Post implements Backend: it hands the banner to the standing stream and
// waits, bounded, for the app's answer. A banner the platform took is made
// and answered Handed; its click arrives later through Notifier.Clicked.
func (a *App) Post(ctx context.Context, target Target, banner Banner, made func()) (Outcome, error) {
	a.mu.Lock()
	stream := a.stream
	if stream == nil {
		a.mu.Unlock()
		return Unclicked, ErrNoApp
	}
	a.nextPost++
	id := a.nextPost
	answer := make(chan PostOutcome, 1)
	a.waiting[id] = answer
	a.mu.Unlock()
	defer a.forget(id)

	timer := time.NewTimer(a.bound)
	defer timer.Stop()
	handed := AppBanner{PostID: id, Banner: banner, Target: target, Group: bannerGroup(target.ID)}
	select {
	case stream.banners <- handed:
	case <-stream.gone:
		return Unclicked, fmt.Errorf("the notifier app's stream ended before post %d was handed: %w", id, ErrNoApp)
	case <-timer.C:
		return Unclicked, fmt.Errorf("the notifier app's stream took no banner within %s (post %d)", a.bound, id)
	case <-ctx.Done():
		return Unclicked, ctx.Err()
	}
	select {
	case outcome := <-answer:
		if err := outcome.err(); err != nil {
			return Unclicked, err
		}
		made()
		return Handed, nil
	case <-stream.gone:
		return Unclicked, fmt.Errorf("the notifier app's stream ended before it answered post %d: %w", id, ErrNoApp)
	case <-timer.C:
		return Unclicked, fmt.Errorf("the notifier app did not answer post %d within %s", id, a.bound)
	case <-ctx.Done():
		return Unclicked, ctx.Err()
	}
}

// forget drops a post's wait, answered or not.
func (a *App) forget(id uint64) {
	a.mu.Lock()
	delete(a.waiting, id)
	a.mu.Unlock()
}

// Answer settles one post with the app's report. ErrUnknownPost when no post
// with that id awaits an answer.
func (a *App) Answer(postID uint64, outcome PostOutcome) error {
	a.mu.Lock()
	answer, ok := a.waiting[postID]
	delete(a.waiting, postID)
	a.mu.Unlock()
	if !ok {
		a.log.Info(opApp, "the notifier app answered a post nothing awaits", dlog.Context{
			"post": postID, "posted": outcome.Posted,
		})
		return ErrUnknownPost
	}
	answer <- outcome
	return nil
}
