package main

import (
	"context"
	"errors"
	"fmt"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/drain"
	"claude-repld/internal/ids"
	"claude-repld/internal/shimclient"
)

// standDownReason is why the serving lifetime ended. It is TYPED so the one
// teardown that depends on it -- stopping the shims -- is decided by a switch
// over values, never by reading an error's text.
type standDownReason int

const (
	// standDownOrderly is the drain's deadline, or the process's signal
	// context: the shims are left running for the next daemon to adopt.
	standDownOrderly standDownReason = iota
	// standDownHandover is the rollout's last transfer: the successor adopts
	// the shims.
	standDownHandover
	// standDownStateRootLost is the root watch finding what this daemon owns
	// on disk gone. Adoption goes through the store and the state under that
	// root, so nothing can ever adopt a shim left running: they are stopped.
	standDownStateRootLost
)

func (r standDownReason) String() string {
	switch r {
	case standDownOrderly:
		return "orderly"
	case standDownHandover:
		return "handover"
	case standDownStateRootLost:
		return "state_root_lost"
	default:
		return fmt.Sprintf("standDownReason(%d)", int(r))
	}
}

// stopsShims answers whether a stand-down for this reason must stop every
// shim before the process exits.
func (r standDownReason) stopsShims() bool {
	return r == standDownStateRootLost
}

// rootLossShimStopBound bounds EACH forced stop the state-root-loss stand-down
// makes -- one per held session and one for the supervisor's sweep of spawns
// still in flight. It is drain.DefaultStandBound, the budget the immediate
// shutdown gives the same forced stops, for the same reason: the exit must
// happen whatever any one shim does.
const rootLossShimStopBound = drain.DefaultStandBound

// shimStopFleet is the fleet surface stopEveryShim walks.
type shimStopFleet interface {
	Workspaces() []ids.WorkspaceID
	KillSession(ctx context.Context, ws ids.WorkspaceID, force bool) error
}

// stopEveryShim force-stops every shim this daemon supervises: the held
// sessions through the fleet, then whatever the supervisor started that never
// reached the fleet. It latches the supervisor's stand-down FIRST, so no
// bring-up starts a shim behind the walk and no client reads this daemon's
// own stops as deaths. Every failure is returned joined, never dropped.
func stopEveryShim(fleet shimStopFleet, supervisor shimclient.Supervisor, bound time.Duration) func(context.Context) (int, error) {
	return func(ctx context.Context) (int, error) {
		supervisor.BeginStandDown()
		var stopped int
		var errs []error
		for _, ws := range fleet.Workspaces() {
			stop, cancel := context.WithTimeout(context.WithoutCancel(ctx), bound)
			err := fleet.KillSession(stop, ws, true)
			cancel()
			if err != nil {
				errs = append(errs, fmt.Errorf("stop the shim of %q: %w", ws, err))
				continue
			}
			stopped++
		}
		sweep, cancel := context.WithTimeout(context.WithoutCancel(ctx), bound)
		defer cancel()
		if err := supervisor.StandDownEverySpawn(sweep, "the daemon's state root was lost"); err != nil {
			errs = append(errs, err)
		}
		return stopped, errors.Join(errs...)
	}
}

// standDownShims stops every shim when, and only when, the reason owes it.
// An ordinary exit and a handover return at once and leave the shims for
// adoption. The outcome is recorded at INFO, or at ERROR naming every shim
// that would not stop.
func standDownShims(ctx context.Context, reason standDownReason, stop func(context.Context) (int, error), log dlog.Logger) {
	if !reason.stopsShims() {
		return
	}
	if stop == nil {
		log.Error("daemon.cmd.state_root", "the state root was lost and nothing can stop the shims; they will outlive this daemon with nothing left to adopt them", dlog.Context{
			"reason": reason.String(),
		})
		return
	}
	stopped, err := stop(ctx)
	if err != nil {
		log.Error("daemon.cmd.state_root", "the state root was lost and some shims would not stop; they will outlive this daemon with nothing left to adopt them", dlog.Context{
			"reason":  reason.String(),
			"stopped": stopped,
			"error":   err.Error(),
		})
		return
	}
	log.Info("daemon.cmd.state_root", "the state root was lost; stopped every shim, since nothing can adopt them", dlog.Context{
		"reason":  reason.String(),
		"stopped": stopped,
	})
}
