package desktopnotify

import (
	"context"
	"sync"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
)

// opPost is the operation a banner's records carry.
const opPost = "daemon.desktopnotify.post"

// opChime is the operation a chime's records carry.
const opChime = "daemon.desktopnotify.chime"

// ClickSink is told a banner was clicked: the server pushes the host stream's
// notification_clicked, and Emacs raises its frame and selects the tab.
type ClickSink interface {
	NotificationClicked(ws ids.WorkspaceID)
}

// Names answers the name a banner titles a workspace by: the roster row's
// name, which is the name the workspace's tab carries.
type Names interface {
	WorkspaceName(ctx context.Context, ws ids.WorkspaceID) (string, error)
}

// Compose builds one banner from the workspace's name. It runs on the banner's
// own goroutine, so it may take its time (a model call summarizing the turn).
type Compose func(ctx context.Context, name string) Banner

// Deps are the notifier's collaborators.
type Deps struct {
	// Focus is Emacs's focus, which decides whether a banner is posted.
	Focus *Focus
	// Backend is the platform's banner program. Nil when none could be
	// resolved, in which case BackendErr says why and every banner records it.
	Backend Backend
	// BackendErr is why no Backend was resolved.
	BackendErr error
	// Chime is the platform's sound player. Nil when none could be resolved,
	// in which case ChimeErr says why and every chime records it.
	Chime Chime
	// ChimeErr is why no Chime was resolved.
	ChimeErr error
	// Clicks is told each clicked banner's workspace.
	Clicks ClickSink
	// Names titles each banner.
	Names Names
	// Log is the notifier's canonical logger.
	Log dlog.Logger
}

// Notifier posts desktop banners and rings chimes. Every banner and chime runs
// on a goroutine of its own, because the banner program blocks until the
// banner is clicked or dismissed and the player until the sound ends; Close
// cancels and joins them all.
type Notifier struct {
	deps   Deps
	ctx    context.Context
	cancel context.CancelFunc
	wg     sync.WaitGroup

	// mu orders every banner's and chime's start against Close: each is
	// either started before Close (and joined by it) or refused after it.
	mu     sync.Mutex
	closed bool
}

// New builds a notifier. It panics on a missing collaborator, because a
// half-wired notifier is a boot defect, not a runtime condition.
func New(deps Deps) *Notifier {
	if deps.Focus == nil || deps.Clicks == nil || deps.Names == nil || deps.Log == nil {
		panic("desktopnotify: New requires Focus, Clicks, Names and Log")
	}
	if (deps.Backend == nil) == (deps.BackendErr == nil) {
		panic("desktopnotify: New requires exactly one of Backend and BackendErr")
	}
	if (deps.Chime == nil) == (deps.ChimeErr == nil) {
		panic("desktopnotify: New requires exactly one of Chime and ChimeErr")
	}
	ctx, cancel := context.WithCancel(context.Background())
	return &Notifier{deps: deps, ctx: ctx, cancel: cancel}
}

// Post raises one banner for ws, unless Emacs is focused. The focus is read
// twice: here, so a focused Emacs costs no composition at all, and again once
// the banner is composed, so focus gained meanwhile still suppresses it.
func (n *Notifier) Post(ws ids.WorkspaceID, kind string, compose Compose) {
	log := n.deps.Log.With(dlog.Context{"workspace": string(ws), "kind": kind})
	if n.deps.Focus.Focused() {
		log.Info(opPost, "Emacs is focused; no desktop banner", nil)
		return
	}
	if !n.spawn(func() { n.show(ws, log, compose) }) {
		log.Info(opPost, "the daemon is standing down; no desktop banner", nil)
	}
}

// Ring plays the chime for ws, WHETHER OR NOT EMACS IS FOCUSED: a focused
// Emacs may be showing another workspace, and the sound is what tells the
// user this one is done.
func (n *Notifier) Ring(ws ids.WorkspaceID, kind string) {
	log := n.deps.Log.With(dlog.Context{"workspace": string(ws), "kind": kind})
	if !n.spawn(func() { n.play(log) }) {
		log.Info(opChime, "the daemon is standing down; no chime", nil)
	}
}

// spawn runs work on a goroutine Close joins, answering false (and running
// nothing) once Close has begun.
func (n *Notifier) spawn(work func()) bool {
	n.mu.Lock()
	defer n.mu.Unlock()
	if n.closed {
		return false
	}
	n.wg.Add(1)
	go func() {
		defer n.wg.Done()
		work()
	}()
	return true
}

// play plays one chime.
func (n *Notifier) play(log dlog.Logger) {
	if n.deps.Chime == nil {
		log.Error(opChime, "no chime program; the chime was not played", dlog.Context{
			"cause": n.deps.ChimeErr.Error(),
		})
		return
	}
	log.Info(opChime, "playing the chime", dlog.Context{"program": n.deps.Chime.Program()})
	err := n.deps.Chime.Play(n.ctx)
	switch {
	case stoodDown(n.ctx, err):
		log.Info(opChime, "the daemon stood down while the chime played", dlog.Context{"cause": err.Error()})
	case err != nil:
		log.Error(opChime, "the chime program failed", dlog.Context{
			"program": n.deps.Chime.Program(), "cause": err.Error(),
		})
	default:
		log.Debug(opChime, "the chime played", nil)
	}
}

// Raise posts an agent notification's banner: a permission ask, a question,
// or the agent addressing the user. It is titled by the workspace's name, with
// the notification's own line below it.
func (n *Notifier) Raise(ws ids.WorkspaceID, kind, text string) {
	n.Post(ws, kind, func(_ context.Context, name string) Banner {
		return Banner{Title: name, Body: text}
	})
}

// show composes and posts one banner, and relays its click.
func (n *Notifier) show(ws ids.WorkspaceID, log dlog.Logger, compose Compose) {
	name, err := n.deps.Names.WorkspaceName(n.ctx, ws)
	switch {
	case stoodDown(n.ctx, err):
		log.Info(opPost, "the daemon stood down while the workspace was named; no desktop banner", dlog.Context{"cause": err.Error()})
		return
	case err != nil:
		log.Error(opPost, "could not name the workspace; no desktop banner", dlog.Context{"cause": err.Error()})
		return
	}
	banner := compose(n.ctx, name)
	if n.deps.Focus.Focused() {
		log.Info(opPost, "Emacs was focused while the banner was composed; no desktop banner", nil)
		return
	}
	if n.deps.Backend == nil {
		log.Error(opPost, "no desktop banner program; the banner was not posted", dlog.Context{
			"cause": n.deps.BackendErr.Error(), "title": banner.Title,
		})
		return
	}
	log.Info(opPost, "posting a desktop banner", dlog.Context{
		"program": n.deps.Backend.Program(), "title": banner.Title,
	})
	clicked, err := n.deps.Backend.Post(n.ctx, ws, banner)
	switch {
	case stoodDown(n.ctx, err):
		log.Info(opPost, "the daemon stood down while a banner awaited its click", dlog.Context{"cause": err.Error()})
	case err != nil:
		log.Error(opPost, "the desktop banner program failed", dlog.Context{
			"program": n.deps.Backend.Program(), "title": banner.Title, "cause": err.Error(),
		})
	case clicked:
		log.Info(opPost, "the banner was clicked; asking Emacs to select the workspace", nil)
		n.deps.Clicks.NotificationClicked(ws)
	default:
		log.Debug(opPost, "the banner was dismissed or timed out", nil)
	}
}

// Close cancels every banner still awaiting its click and every chime still
// playing, and joins them.
func (n *Notifier) Close() {
	n.mu.Lock()
	n.closed = true
	n.mu.Unlock()
	n.cancel()
	n.wg.Wait()
}

// stoodDown reports whether err is a call the daemon's stand-down cut short:
// the call failed under a ctx (the notifier's lifetime) that has ended.
func stoodDown(ctx context.Context, err error) bool {
	return err != nil && ctx.Err() != nil
}
