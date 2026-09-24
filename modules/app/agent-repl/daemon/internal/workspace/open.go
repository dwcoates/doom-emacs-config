package workspace

import (
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/footer"
	"claude-repld/internal/shimclient"
)

// Open brings a registered workspace's session up. MOUNTING A PARKED
// WORKSPACE'S FRONTEND IS AN IMPLICIT REVIVAL: there is no shim-less read path,
// so opening spawns rather than serving history from anywhere else.
//
// It is idempotent: opening a workspace whose session is already live clears
// the closed flag and returns, because the mount it answers has already
// happened.
// reportOpenStage relays one stage to an Open's progress reporter, if the
// caller set one. An open with no reporter (every non-client caller) emits
// nothing.
func reportOpenStage(progress OpenProgress, stage OpenStage) {
	if progress != nil {
		progress.Stage(stage)
	}
}

func (v *verbs) Open(ctx context.Context, ws ids.WorkspaceID, progress OpenProgress) error {
	record, log, err := v.owned(ctx, "OpenWorkspace", ws)
	if err != nil {
		return err
	}

	// A WORKSPACE WHOSE DIRECTORY IS GONE CANNOT BE OPENED. Boot already
	// CLOSES such a row (internal/boot's closeMissingDirs) precisely because a
	// registry row whose directory no longer exists names nothing a user can
	// work in; re-opening it would spawn a shim with no working tree and put
	// the unopenable tab straight back. So the re-open is REFUSED by name,
	// with the missing directory as the refusal's evidence, instead of failing
	// as an internal error the client cannot read.
	//
	// A STAT THAT DOES NOT SAY "NOT EXIST" IS NEVER READ AS GONE, the same
	// discipline boot applies: "could not tell" is not an answer, and refusing
	// the open on it would strand a workspace that is merely unreachable this
	// instant.
	reportOpenStage(progress, OpenStageCheckingWorktree)
	if _, statErr := os.Stat(record.Dir); errors.Is(statErr, fs.ErrNotExist) {
		return refuse(log, "OpenWorkspace", ArmSpawnFailed,
			fmt.Sprintf("the workspace's directory no longer exists: %s", record.Dir), false)
	}

	// A PARKED WORKSPACE IS REVIVED THROUGH THE SAME PATH, and the park is
	// LIFTED by it. The start below would have spawned the session either way
	// — that is the implicit revival this verb has always performed — but
	// nothing retracted the sweep's park from the session-scoped views, so the
	// topbar went on drawing the hibernated strip over a live session until
	// the shim's first link state happened to arrive.
	asleep, err := v.parked(ctx, ws)
	if err != nil {
		log.Error(opOpen, "could not tell whether the workspace was hibernated", dlog.Context{"cause": err.Error()})
		return fmt.Errorf("open %q: %w", ws, err)
	}
	if v.deps.Sessions.Live(ws) {
		log.Debug(opOpen, "the session is already live", nil)
		// AN OPEN RECONCILES; IT DOES NOT TRUST THE LIVENESS IT READ. A live
		// session is the reason this verb starts nothing — and it is also the
		// reason the record is never revisited on this path, which is how the
		// open came to answer in under two milliseconds and leave the user
		// with nothing: the workspace's session record still read `killed`
		// from a KillWorkspace weeks earlier, the roster RECEDES a killed
		// session's row, and Emacs gives a tab only to a row that is not
		// receded. The open now leaves the record saying what the fleet says.
		if err := retireTerminalRecord(ctx, log, v.deps.DB, opOpen, ws); err != nil {
			return fmt.Errorf("open %q: %w", ws, err)
		}
	} else {
		// THE SLOW STAGE. Reported only when a bring-up actually runs: a
		// session already live waited for nothing, and a stage announcing
		// work that is not happening is worse than no stage at all.
		reportOpenStage(progress, OpenStageStartingSession)
		if err := v.deps.Sessions.Start(ctx, ws); err != nil {
			// A DEPARTING DAEMON IS NOT A SESSION THAT FAILED TO COME UP. The
			// bring-up refused before it spawned because nothing would be left
			// to own the shim, and the successor opens the workspace from the
			// same record; the caller still gets the refusal, which is what it
			// acts on.
			if errors.Is(err, shimclient.ErrStandingDown) {
				log.Info(opOpen, "the session was not brought up: this daemon is standing down", nil)
			} else {
				log.Error(opOpen, "the session did not come up", dlog.Context{"cause": err.Error()})
			}
			return fmt.Errorf("open %q: start the session: %w", ws, err)
		}
	}
	if asleep {
		reportOpenStage(progress, OpenStageReviving)
		v.unpark(ws)
		log.Info(opOpen, "revived the hibernated workspace", nil)
	}

	if record.Closed {
		reportOpenStage(progress, OpenStageClearingClosed)
		if err := v.deps.DB.SetClosed(ctx, ws, false); err != nil {
			log.Error(opOpen, "could not clear the closed flag", dlog.Context{"cause": err.Error()})
			return fmt.Errorf("open %q: clear closed: %w", ws, err)
		}
		log.Debug(opOpen, "cleared the closed flag", nil)
	}

	// A close refusal is drawn in the footer; re-opening retires it, because
	// the state it described is gone.
	v.deps.Footer.SetClosing(ws, nil)

	// The build-staleness check belongs to the mount: a workspace coming up
	// against a shim older than the installed build goes to the bounce
	// registry now — bounced at once when free, when its work ends otherwise —
	// rather than discovering the mismatch mid-turn. The mount never waits on
	// the bounce.
	reportOpenStage(progress, OpenStageCheckingBuild)
	check, err := v.deps.Rollout.CheckStaleness(ctx, ws, false)
	switch {
	case err != nil:
		// NOT A FAILED MOUNT: the session is up and usable on the build it
		// has. The judgement that could not be made is the rollout's own loud
		// record too.
		log.Error(opOpen, "the build-staleness check could not judge the shim", dlog.Context{"cause": err.Error()})
	case check.Stale:
		log.Info(opOpen, "the shim runs an older build; it went to the bounce registry", dlog.Context{
			"reported_build": check.Reported, "installed_build": check.Installed,
			"bounce_now": check.Bounce.Now, "skipped": check.Skipped,
		})
	default:
		log.Debug(opOpen, "the shim runs the installed build", nil)
	}

	log.Info(opOpen, "opened the workspace", dlog.Context{"dir": record.Dir})
	v.republishRegistry(ctx, log, opOpen)
	return nil
}

// closeBlocker names why a close is refused, or nil when the workspace is
// quiet. The four blockers are the ruled ones; a standing cold gate and a
// parked session are deliberately NOT among them.
//
// EVERY BLOCKER IS COMPUTED, not just the first one that fires: the refusal
// carries all four counts as evidence (CloseWorkspaceBlocked, landing 7) and
// the footer draws the same composed sentence, so the check answers the whole
// picture and the ORDER below only decides which one the sentence leads with.
func (v *verbs) closeBlocker(ctx context.Context, ws ids.WorkspaceID) (*footer.CloseBlocked, error) {
	blocked := footer.CloseBlocked{}
	var liveDetail string
	if running, live := v.deps.Freeness(ws); live {
		blocked.TurnInFlight = running.Turn != nil
		agents, shells := len(running.LiveWork.Agents), len(running.LiveWork.Shells)
		blocked.LiveWork = uint32(agents + shells)
		liveDetail = fmt.Sprintf("%d detached agents and %d detached shells are still live", agents, shells)
	}
	held, err := v.deps.DB.HeldPrompts(ctx, ws)
	if err != nil {
		return nil, fmt.Errorf("read the held prompts: %w", err)
	}
	blocked.HeldPrompts = uint32(len(held))
	mergeState := ""
	if facts, ok := v.deps.Merge.Facts(ws); ok && mergeIsPending(facts.State) {
		blocked.MergeQueued = true
		mergeState = facts.State
	}

	switch {
	case blocked.TurnInFlight:
		blocked.Reason = "turn_in_flight"
		blocked.Detail = "a turn is still running; interrupt it or wait for it to end"
	case blocked.LiveWork > 0:
		blocked.Reason = "live_work"
		blocked.Detail = liveDetail
	case blocked.HeldPrompts > 0:
		blocked.Reason = "held_prompts"
		blocked.Detail = fmt.Sprintf("%d held prompts have not been delivered; release or drop them first", len(held))
	case blocked.MergeQueued:
		blocked.Reason = "merge_queued"
		blocked.Detail = fmt.Sprintf("a merge is %s; evict it from the queue first", mergeState)
	default:
		return nil, nil
	}
	return &blocked, nil
}

// mergeIsPending reports whether a merge state still owes the workspace work.
// A merge that landed or failed is finished and blocks nothing.
func mergeIsPending(state string) bool {
	switch state {
	case "enqueuing", "queued", "merging", "conflict":
		return true
	default:
		return false
	}
}
