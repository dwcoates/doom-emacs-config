// Package bringup starts the sessions of open workspaces that a daemon has
// come to serve with no session behind them.
//
// AN OPEN WORKSPACE IS NEVER SESSION-LESS (owner rulings, 2026-09-13 and
// 2026-10-02). A daemon comes to serve a workspace in more than one way: a
// fresh boot reconciles the registry and finds shims that did not survive, and
// a joining successor takes workspaces over from the daemon it replaces, some
// of them with no shim at all. Every one of those ways ends here, so a
// workspace's session is started the same way whichever one brought it to
// this daemon, and its feed draws its history without waiting for a prompt.
//
// The feed's history reaches the daemon ONLY through a live session's watch
// (sessionwatcher replays the opening history page), so a served workspace
// with no session draws an empty feed. That is why this is a structural
// requirement and not a nicety.
package bringup

import (
	"context"
	"errors"
	"fmt"
	"sync"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/wsm"
)

// StartFunc brings ONE workspace's session up. workspace.Fleet.Start is the
// production one: the same path OpenWorkspace takes, so a session started here
// and a session a user started are the same session in every respect.
type StartFunc func(ctx context.Context, ws ids.WorkspaceID) error

// SessionReader reads a workspace's durable session record. wsm.DB is the
// production one.
type SessionReader interface {
	Session(ctx context.Context, ws ids.WorkspaceID) (wsm.Session, bool, error)
}

// Deps are a bring-up's collaborators.
type Deps struct {
	// DB reads each workspace's session record, to tell a hibernated
	// workspace's wake apart from an ordinary start.
	DB SessionReader
	// StartSession starts one workspace's session.
	StartSession StartFunc
	// BringingUp lowers (false) the roster's bring-up marker
	// (sidebar.Resolver.SetBringingUp) for each workspace on every path. THE
	// CALLER RAISES IT, at the moment it decides the workspace is pending and
	// before that workspace's views can be drawn: a boot raises it before the
	// daemon serves, a takeover before it publishes. A workspace a bring-up
	// names is so never drawn idle and usable while nothing is behind it yet.
	BringingUp func(ws ids.WorkspaceID, underWay bool)
	// Log is where the per-workspace records and the one summary go.
	Log dlog.Logger
	// Operation names the records, so a boot's bring-up and a takeover's
	// are told apart in the log.
	Operation string
	// Done, when set, is told how each workspace's start ended (nil for a
	// session that came up or a workspace already being served), the moment
	// it ends rather than when the whole bring-up does. The editor's startup
	// (internal/startup) is the one caller that waits on single workspaces.
	Done func(ws ids.WorkspaceID, err error)
}

// Outcome is what one workspace's bring-up came to.
type Outcome int

const (
	// NotBegun is a workspace whose start was never begun because the
	// daemon was already leaving.
	NotBegun Outcome = iota
	// Started is a workspace whose session was started.
	Started
	// Woken is a hibernated workspace whose session was started.
	Woken
	// StoodDown is a workspace whose start ended because THIS daemon stood
	// its shim down under it.
	StoodDown
	// Failed is a workspace whose start could not complete.
	Failed
)

// Report is what one bring-up did, each slice in the order the workspaces
// were pending.
type Report struct {
	// Started are the workspaces whose session was started.
	Started []ids.WorkspaceID
	// Woken are the hibernated workspaces whose session was started. They
	// are counted apart from Started because waking one spends back the
	// memory the idle sweep reclaimed.
	Woken []ids.WorkspaceID
	// StoodDown are the workspaces whose start ended in a stand-down this
	// daemon ordered. Nothing about them is broken.
	StoodDown []ids.WorkspaceID
	// Failed are the workspaces whose start could not complete. Each start
	// already raised the workspace's own start-failed fault.
	Failed []ids.WorkspaceID
}

// Validate refuses a Deps that could not run a bring-up. Every field is
// required: a bring-up that silently skipped its starts, its marker or its
// records would leave a workspace session-less with nothing saying so.
func (d Deps) Validate() error {
	switch {
	case d.DB == nil:
		return errors.New("bringup: a session reader is required")
	case d.StartSession == nil:
		return errors.New("bringup: a session starter is required")
	case d.BringingUp == nil:
		return errors.New("bringup: the bring-up marker is required")
	case d.Log == nil:
		return errors.New("bringup: a logger is required")
	case d.Operation == "":
		return errors.New("bringup: an operation name is required")
	}
	return nil
}

// Run starts the session of every pending workspace and reports what came of
// each one.
//
// THE STARTS RUN CONCURRENTLY. Each one is a shim spawn plus a vendor child
// that has to prove itself live, and nothing one workspace's start does waits
// on another's: the start serializes per workspace (its start gate), not
// across them. MEASURED: the boot of 2026-09-27 20:52:08 (pid 2495) took 64.9s
// to bring four sessions up one after another, 11.1s to 22.4s each.
//
// EVERY FAILURE IS PER WORKSPACE. One workspace's start failing does not stop
// the next: the start already raises the workspace's own start-failed fault
// and states the dead link on every surface, and the summary counts it.
func Run(ctx context.Context, deps Deps, pending []wsm.Workspace) Report {
	outcomes := make([]Outcome, len(pending))
	var wg sync.WaitGroup
	for i, ws := range pending {
		wg.Add(1)
		go func() {
			defer wg.Done()
			var err error
			outcomes[i], err = one(ctx, deps, ws)
			if deps.Done != nil {
				deps.Done(ws.ID, err)
			}
		}()
	}
	wg.Wait()
	report := Report{}
	for i, ws := range pending {
		switch outcomes[i] {
		case Started:
			report.Started = append(report.Started, ws.ID)
		case Woken:
			report.Woken = append(report.Woken, ws.ID)
		case StoodDown:
			report.StoodDown = append(report.StoodDown, ws.ID)
		case Failed:
			report.Failed = append(report.Failed, ws.ID)
		case NotBegun:
		}
	}
	// ONE SUMMARY, AT INFO. The per-workspace records are DEBUG because they
	// are a loop body; what a person asks about after a bounce is how many
	// sessions this daemon brought back, so that count is stated once at the
	// level a person reads.
	deps.Log.Info(deps.Operation, "the bring-up started the open workspaces' sessions", dlog.Context{
		"pending":    len(pending),
		"started":    len(report.Started),
		"woken":      len(report.Woken),
		"stood_down": len(report.StoodDown),
		"failed":     len(report.Failed),
	})
	return report
}

// one brings one pending workspace's session up and says what came of it,
// logging the per-workspace record Run's summary counts.
func one(ctx context.Context, deps Deps, ws wsm.Workspace) (Outcome, error) {
	log := deps.Log
	// Raised by the caller when it named the workspace pending; lowered here
	// whatever this comes to, so a workspace whose start never began is not
	// held `pending` forever.
	defer deps.BringingUp(ws.ID, false)
	// A START THAT HAS BEGUN IS FINISHED, NEVER ABANDONED MID-WRITE, and a
	// start not yet begun is simply not begun once the daemon is leaving. A
	// start cancelled halfway is not a session that failed: it is a
	// half-written session record, a shim stopped between spawn and attach,
	// and a fault the daemon then could not record because the same
	// cancellation refused its transaction.
	if err := ctx.Err(); err != nil {
		log.Info(deps.Operation, "the daemon is leaving; this workspace is not started", dlog.Context{
			dlog.KeyWorkspaceID: string(ws.ID),
			"error":             err.Error(),
		})
		return NotBegun, fmt.Errorf("bringup: %q not started, the daemon is leaving: %w", ws.ID, err)
	}
	startCtx := context.WithoutCancel(ctx)
	fields := dlog.Context{
		dlog.KeyWorkspaceID:  string(ws.ID),
		dlog.KeyWorkspaceDir: ws.Dir,
	}
	session, exists, err := deps.DB.Session(startCtx, ws.ID)
	if err != nil {
		fields["error"] = err.Error()
		log.Error(deps.Operation, "a workspace's session record could not be read; it is not brought up", fields)
		return Failed, fmt.Errorf("bringup: read the session record of %q: %w", ws.ID, err)
	}
	hibernated := exists && session.Hibernated()
	if err := deps.StartSession(startCtx, ws.ID); err != nil {
		fields["error"] = err.Error()
		// A START THIS DAEMON STOOD THE SHIM DOWN UNDER IS NOT A FAILED
		// BRING-UP. An exit's drain force-stops every workspace session, and
		// a start still in flight when it does comes back from a shim the
		// same process just killed; a start that reaches the spawn after the
		// supervisor began standing down is refused one for the same reason. MEASURED: realtest run
		// 2026-09-13T16:20:34 recorded it as an ERROR on three consecutive
		// daemon generations.
		if errors.Is(err, shimclient.ErrStandDownOrdered) || errors.Is(err, shimclient.ErrStandingDown) {
			log.Debug(deps.Operation, "an open workspace's start ended in a stand-down this daemon ordered", fields)
			return StoodDown, err
		}
		log.Error(deps.Operation, "an open workspace's session did not come up; the bring-up goes on", fields)
		return Failed, err
	}
	if hibernated {
		log.Debug(deps.Operation, "a hibernated workspace was woken", fields)
		return Woken, nil
	}
	log.Debug(deps.Operation, "an open workspace's session was started", fields)
	return Started, nil
}
