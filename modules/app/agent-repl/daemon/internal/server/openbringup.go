// openbringup.go holds the OPEN COMMAND'S ACK CONTRACT and the in-flight
// coalescing that contract needs to be safe.
//
// THE DEFECT IT CLOSES: a self-sustaining congestion collapse. `Open' used to
// return only once the workspace's FULL bring-up had landed — shim spawn,
// vendor CLI boot, conversation resume, handshake — which is five to seventeen
// seconds per workspace. The editor's reattach sweep re-sends `openWorkspace'
// for every workspace whose open has not been acked within its deadline, so
// with tens of live workspaces the arrival rate of opens exceeded the rate at
// which they could be served. The command queue grew without bound, EVERY open
// timed out unacked, every timeout produced another open, and the daemon never
// caught up. Nothing was broken; the pipeline was simply asked to carry more
// than a serialized bring-up per workspace could deliver.
//
// THE ACK NOW MEANS ACCEPTED. Validated, the workspace's session located or
// created, its transcript bound, and its bring-up started or already running.
// It no longer means finished. That is the whole structural change, and it is
// what makes an open cost milliseconds instead of seconds.
//
// WHY THAT IS SAFE TO SAY. Bring-up PROGRESS never rode the ack in the first
// place: the SSM's workspace state and the pushed SessionView carry every phase
// the clients render, and they are pushed as they happen. The Emacs ensure path
// reads a successful ack as ACCEPTED and only gives up on a workspace when its
// open is REFUSED — which the synchronous half below still does, loudly, for
// every refusal it ever made.
//
// WHAT STAYS SYNCHRONOUS, AND WHY EXACTLY THOSE THINGS. Everything that can
// REFUSE the request: a missing ensurer or failure sink, session creation with
// its permission-mode validation and its ungated-consent refusal, and the
// record's existence after binding. Those are answers to "may this workspace be
// opened at all", the caller can act on them, and none of them waits on a shim.
// Only the wait for a driveable session moved.
//
// WHERE A LATE FAILURE GOES. Into the pushed surface, never nowhere: the
// bring-up ladder publishes its own resolved failures (a start-failed card and
// a closed connectivity axis), and anything the opener classifies after the
// ladder returns is published through the failure sink
// (sessioncontroller.RecordOpenFailure). A nil sink is refused at the top of
// Open rather than tolerated, because an early ack with no failure surface is
// exactly the silent failure this file must not create.
//
// COALESCING. A second open for a workspace whose bring-up is already in flight
// is acked AGAINST that bring-up rather than starting a second one. It is not
// an optimization: two concurrent bring-ups for one workspace would supersede
// each other mid-spawn, and the retry storm above is precisely a generator of
// duplicate opens. Run preferences are not compared before coalescing, because
// they are only ever read for a session an open STARTS — a workspace already
// mid-bring-up has one, and an open never re-postures a live session
// (WorkspaceOpenOpts).
package server

import (
	"context"
	"errors"
	"time"

	"claude-repld/internal/errclass"
	"claude-repld/internal/registry"
)

// openBringUpTimeout is a BACKSTOP on the detached bring-up, not a tuning knob
// and not the bound anyone waits out. Every bring-up path already bounds itself
// (the ladder resolves on its own timeout), so this only guarantees that no
// open can leave a goroutine parked forever if a future path forgets to. It is
// deliberately far above any real bring-up so it can never pre-empt one.
const openBringUpTimeout = 2 * time.Minute

// OpenFailureSink publishes a bring-up failure that arrived AFTER its
// open_workspace command was acked. Satisfied by *sessioncontroller.Manager.
//
// It exists because the ack is no longer the failure surface for the bring-up
// half of an open, and a failure with no surface is a swallowed failure.
type OpenFailureSink interface {
	RecordOpenFailure(workspace string, err error)
}

// openBringUp is one workspace's in-flight open. Callers that coalesce onto it
// do not wait for it — the ack is immediate either way — so it carries only
// what a later reader needs: the completion edge and the outcome, written
// before done is closed and read only after.
type openBringUp struct {
	done chan struct{}
	err  error
}

// claimOpen takes the workspace's open slot. It reports the round to drive and
// whether THIS caller is the one that must drive it; a caller that does not
// claim has coalesced onto the round already in flight.
//
// The slot is claimed BEFORE the synchronous half runs, not after, because the
// synchronous half creates and binds: two connections opening one workspace at
// the same instant have their own command lanes, so the lanes' per-workspace
// ordering does not cover them and only this claim does.
func (o *WorkspaceOpener) claimOpen(workspace string) (*openBringUp, bool) {
	o.openMu.Lock()
	defer o.openMu.Unlock()
	if o.opening == nil {
		// Lazy under the lock rather than in a constructor: focused harnesses
		// build a WorkspaceOpener literal, and an opener that panics depending
		// on how it was constructed is a worse contract than one check.
		o.opening = map[string]*openBringUp{}
	}
	if inflight, busy := o.opening[workspace]; busy {
		return inflight, false
	}
	round := &openBringUp{done: make(chan struct{})}
	o.opening[workspace] = round
	return round, true
}

// releaseOpen frees the workspace's slot and closes the round. It releases only
// the round it was given, so a slot already re-claimed by a later open is never
// stolen from it.
func (o *WorkspaceOpener) releaseOpen(workspace string, round *openBringUp) {
	o.openMu.Lock()
	if o.opening[workspace] == round {
		delete(o.opening, workspace)
	}
	o.openMu.Unlock()
	close(round.done)
}

// onOpenSettled, when set, is called with the workspace each time a DETACHED
// bring-up has finished and released the workspace's slot. It is a TEST SEAM
// and nothing reads it in production: the ack is now written before the
// bring-up ends, so "this open's bring-up is over" is no longer observable
// from outside the opener, and the alternative is a timing guess — which is
// exactly what this change exists to eliminate.
var onOpenSettled func(workspace string)

// openInFlight reports whether a bring-up is running for workspace. It is the
// coalescing claim made observable; nothing in production reads it.
func (o *WorkspaceOpener) openInFlight(workspace string) bool {
	o.openMu.Lock()
	defer o.openMu.Unlock()
	_, busy := o.opening[workspace]
	return busy
}

// driveOpen is the detached half: wait for the workspace to become driveable,
// classify the outcome, and publish a failure the ack can no longer carry.
//
// It runs under its OWN context rather than the command's. The command has been
// answered and its caller may be long gone; letting that caller's cancellation
// end the bring-up would abandon the very work the ack promised had started.
func (o *WorkspaceOpener) driveOpen(round *openBringUp, rec registry.Record, workspace string) {
	// LIFO: the slot is released first, then the settlement is announced, so a
	// reader woken by the seam observes a workspace with no open in flight.
	defer func() {
		if onOpenSettled != nil {
			onOpenSettled(workspace)
		}
	}()
	defer o.releaseOpen(workspace, round)
	ctx, cancel := context.WithTimeout(context.Background(), openBringUpTimeout)
	defer cancel()
	err := o.establish(rec, workspace, o.Ensurer.EnsureDriveable(ctx, workspace))
	round.err = err
	switch {
	case err == nil:
		o.Logf("server: open-workspace %q session %s is DRIVEABLE; the bring-up the open accepted has completed",
			workspace, rec.SessionID)
	case errors.Is(err, errclass.ErrSessionHibernated):
		// establish already said this loudly, and the revival gate is rendered
		// from the pushed SessionView. Carding it would show a continuity error
		// for a session that is simply asleep.
	default:
		o.Logf("server: open-workspace %q session %s bring-up FAILED after the open was acked: %v",
			workspace, rec.SessionID, err)
		o.Failures.RecordOpenFailure(workspace, err)
	}
}
