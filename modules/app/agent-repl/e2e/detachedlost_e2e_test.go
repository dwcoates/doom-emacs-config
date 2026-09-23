// detachedlost_e2e_test.go — THE THREE LOST ARMS, end to end.
//
// SUBJECT — `DetachedLost {file_vanished | went_silent | swept_up}`
// (proto/src/conversation/v1/agent_activity.proto), the sidecar's statement
// that it STOPPED BEING ABLE TO SEE a detached run. The proto's own words:
// "WE STOPPED BEING ABLE TO SEE THE WORK — not known to have failed, not
// known to have finished. THE ARM IS HOW WE CONCLUDED IT". It rides
// `AgentBashInterrupted.cause.lost` for a shell, and the daemon draws it as
// feed.proto's `FeedShellSettled.outcome.lost` (FeedShellLost) — "We stopped
// being able to see it — spool gone or silent past the shim's ruling; not
// known to have failed."
//
// WHY THIS FILE EXISTS. detachedbash_e2e_test.go's
// TestBashDetachedLiveNeverSettles recorded the gap in as many words: every
// one of these arms needs the SIDECAR'S OWN staleness ruling to elapse or its
// boot sweep to run, and the sidecar's production windows (30s grace, 30m
// shell silence) cannot be reached inside this suite's budget. They are
// flags, so this file buys short ones through
// NewWorldWithSidecarStaleness (world_test.go) and drives each arm for real.
// NOTHING IS SIMULATED: the real fake-SDK shim writes the real spool, the
// real sidecar rules on it, the real store carries the terminal, and the
// assertion is on the daemon's own FeedRow.
//
// WHERE EACH ARM IS ASSERTED, and why it takes two surfaces.
//
//   - THE FEED says LOST AND WHICH LOST. Landing 11 gave `FeedShellLost` an
//     `oneof how {file_vanished | went_silent | swept_up}` mirroring
//     DetachedLost one-to-one, so the frontend surface now states the arm
//     itself and every test here pins it there. Before Landing 11 the
//     message was empty and this suite could only pin "lost, not cancelled,
//     not completed" at the feed.
//   - THE SIDECAR'S OWN RECORD says which arm it CONCLUDED, on its dedicated
//     `reason` key in the daemon-owned workspace sidecar sink — the key the
//     sidecar's own integration suite joins a terminal to its sweep on
//     (`lost-terminal`, seam.go).
//
// Both are asserted for every arm, and they remain distinct claims: the
// sidecar's reason is the conclusion it reached, the feed's `how` is the arm
// the daemon relayed onward. Asserting only the feed would pass while the
// sidecar concluded one arm and the relay named another; asserting only the
// reason would pass while the daemon dropped the arm on the floor.
//
// EVERY WAIT IS BOUNDED AND STATED, and there is no sleep in this file. The
// bounds are derived from the windows the tests themselves buy, and their
// derivation is spelled at each constant below.
package e2e

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/integration/harness"
)

// ===========================================================================
// The windows these tests buy, and the bounds derived from them.
// ===========================================================================

// dlSweepInterval is how often the sidecar's LOST sweep runs: cycle.go arms
// its sweep ticker at the RESCAN interval, which world_test.go's startSidecar
// sets to 200ms for every world in this suite. A conclusion is therefore
// owed no earlier than its window and no later than one sweep tick past it,
// which is what makes the bounds below a multiple of window+tick rather than
// of the window alone.
const dlSweepInterval = 200 * time.Millisecond

// dlSilenceWindow is the shell-silence window TestDetachedLostWentSilent
// buys. `!bash-detach-live` appends its one line and then never writes
// again, so the spool falls silent immediately and the only thing the window
// buys is the sidecar's own patience — it need only outlast the jitter
// between the fake's append and the sidecar's tail of it, which one 200ms
// rescan tick plus a 50ms poll already bounds. 400ms is ~1.6x that, small
// enough that the test is fast and large enough that a slow tail cannot
// conclude the run LOST while it is still being ingested.
const dlSilenceWindow = 400 * time.Millisecond

// dlGraceWindow is the grace TestDetachedLostFileVanished buys: how long a
// file that disappeared is given before the disappearance is concluded
// rather than absorbed as a rename race. Same sizing as dlSilenceWindow, for
// the same reason — the deletion is a single observable act and the window
// only has to outlast the poll that notices it.
const dlGraceWindow = 400 * time.Millisecond

// dlLostBound is how long a test waits for a bought window's conclusion to
// travel all the way to the feed: the window elapses, the next sweep (up to
// dlSweepInterval away) concludes it, the sidecar writes the terminal to the
// real store, the store fans it out over WatchBashRun, the real shim relays
// it, and the daemon redraws the row. 3x (window + one sweep tick) — the
// multiple the dispatcher fixed for this file, applied to the full
// window-plus-tick a conclusion actually owes rather than to the window
// alone.
//
// MEASURED, NOT GUESSED (5 consecutive runs of this file, sidecar log
// timestamps): a bought 400ms window concludes 404ms and 436ms after the
// triggering fact for went_silent and file_vanished respectively — the
// window plus a fraction of one sweep tick — and the LOST terminal reaches
// the store within ~1ms of the conclusion, the run's WatchBashRun stream
// ending on it in the same millisecond. The observed healthy maximum is
// therefore ~440ms against this 1800ms bound, a 4x margin; every one of the
// 15 test runs passed.
const dlLostBound = 3 * (dlSilenceWindow + dlSweepInterval)

// dlSpoolBound is how long a test waits for the fake SDK's spool file to
// appear under the world's spool root. It is an ordinary "a real process did
// the thing" wait, not a policy window, so it takes the suite's ordinary
// budget.
const dlSpoolBound = DefaultTimeout

// ===========================================================================
// Helpers.
// ===========================================================================

// dlAwaitOneSpool answers the single spool file the world's shim has written
// under the world's spool root, waiting (bounded) for it to appear.
//
// ONE FILE IS THE INVARIANT, not a convenience: each of these tests drives
// exactly one detached-shell scenario in a world of its own, so a second
// spool would mean the test is ruling on a file it did not intend, and the
// arm it then asserts would be about something else. It is failed rather
// than picked between.
func dlAwaitOneSpool(t *testing.T, ctx context.Context, w *World) string {
	t.Helper()
	tick := time.NewTicker(pollInterval)
	defer tick.Stop()
	for {
		var found []string
		err := filepath.WalkDir(w.Sidecar.SpoolRoot, func(path string, d os.DirEntry, err error) error {
			if err != nil {
				return err
			}
			if !d.IsDir() {
				found = append(found, path)
			}
			return nil
		})
		if err != nil {
			t.Fatalf("e2e: walking the world's spool root %s: %v", w.Sidecar.SpoolRoot, err)
		}
		switch {
		case len(found) == 1:
			// RESOLVED HERE, WHILE THE FILE STILL EXISTS. The sidecar
			// resolves every path it discovers, so its records name the
			// /private/var/... form of this suite's /var/folders/...
			// t.TempDir. Resolving later would be too late for the vanished
			// arm: EvalSymlinks on a deleted path fails, and the assertion
			// would then be comparing two spellings of the same file and
			// matching neither.
			resolved, err := filepath.EvalSymlinks(found[0])
			if err != nil {
				t.Fatalf("e2e: resolving the spool %s: %v", found[0], err)
			}
			return resolved
		case len(found) > 1:
			t.Fatalf("e2e: the world's spool root holds %d spools (%v); this test rules on exactly one", len(found), found)
		}
		select {
		case <-ctx.Done():
			t.Fatalf("e2e: no spool appeared under %s within %s; the scenario's detached shell never wrote one",
				w.Sidecar.SpoolRoot, dlSpoolBound)
		case <-tick.C:
		}
	}
}

// dlAwaitLostReason waits for the sidecar's own statement that it concluded
// a file LOST for a NAMED reason, and answers the record.
//
// THE REASON IS MATCHED ON ITS OWN KEY, never in prose. `lost-terminal`
// (seam.go) carries a dedicated `reason` key holding the same word the
// wire's DetachedLost arm carries, so this assertion goes on failing if the
// arm regresses — a substring match against the sentence would not.
func dlAwaitLostReason(t *testing.T, ctx context.Context, w *World, workspaceDir, spool, reason string) harness.LogRecord {
	t.Helper()
	tick := time.NewTicker(pollInterval)
	defer tick.Stop()
	for {
		var seen []string
		for _, r := range w.Daemon.WorkspaceLog(workspaceDir, "sidecar") {
			if !strings.HasPrefix(r.Operation, "lost-terminal") {
				continue
			}
			path, _ := r.Context["path"].(string)
			got, _ := r.Context["reason"].(string)
			if !dlSamePath(path, spool) {
				continue
			}
			if got == reason {
				return r
			}
			seen = append(seen, fmt.Sprintf("%s(reason=%q)", r.Operation, got))
		}
		select {
		case <-ctx.Done():
			t.Fatalf("e2e: the sidecar never concluded %s LOST with reason=%q within the bound; "+
				"the conclusions it did state for that file were %v", spool, reason, seen)
		case <-tick.C:
		}
	}
}

// dlSamePath compares a path the sidecar wrote against one this test built.
// THE COMPARISON IS SYMLINK-RESOLVED, because the two are the same file
// under two names: the world's spool root is a t.TempDir under
// /var/folders/... on darwin, and the sidecar resolves every path it
// discovers, so its records name /private/var/folders/... A raw string
// compare would silently match nothing and the arm assertion would time out
// having tested nothing. Same device, same reason, as the sidecar's own
// integration suite (`samePath`, helpers_test.go).
func dlSamePath(a, b string) bool {
	if a == b {
		return true
	}
	ra, err := filepath.EvalSymlinks(a)
	if err != nil {
		ra = a
	}
	rb, err := filepath.EvalSymlinks(b)
	if err != nil {
		rb = b
	}
	return ra == rb
}

// dlAwaitLostShell drives the feed until the named detached shell settles,
// and asserts the settled outcome is the LOST arm rather than completed or
// cancelled. The shell is answered so a caller can assert on the rest of it.
func dlAwaitLostShell(
	t *testing.T,
	ctx context.Context,
	w *World,
	ws *workspacev1.WorkspaceRef,
	initial []*frontendv1.FeedRow,
	stream *harness.Stream[*frontendv1.FeedRow],
	commandSubstring string,
	wantHow string,
) (shell *frontendv1.FeedShell, spool string) {
	t.Helper()
	// EVERY LOST RUN HERE WROTE ONE LINE before it was lost, and a LOST
	// conclusion never drops what was observed: the body is awaited carrying
	// it, which is also what makes the answered spool the one the settled
	// bubble draws.
	shell, spool = dbAwaitSettledShell(t, ctx, w, ws, initial, stream, commandSubstring,
		func(s *frontendv1.FeedShellSettled) bool { return s.GetOutcome() != nil },
		"partial output with no terminator")
	lost := shell.GetSettled().GetLost()
	if lost == nil {
		t.Fatalf("settled outcome = %v, want the LOST arm (feed.proto, FeedShellSettled.outcome.lost): "+
			"the sidecar stopped being able to see the run, which is neither a completion nor a cancel",
			shell.GetSettled().GetOutcome())
	}
	if got := dlShellLostHow(lost); got != wantHow {
		t.Fatalf("the LOST shell's how = %q, want %q (feed.proto, FeedShellLost.how): "+
			"the daemon relays the sidecar's DetachedLost arm by name, so a different word here "+
			"means the arm was dropped or renamed on the way to the frontend", got, wantHow)
	}
	return shell, spool
}

// dlShellLostHow names the feed's own lost arm, in the DetachedLost
// vocabulary, so a mismatch reads as the word rather than a wrapper type.
func dlShellLostHow(lost *frontendv1.FeedShellLost) string {
	switch lost.GetHow().(type) {
	case *frontendv1.FeedShellLost_FileVanished:
		return "file_vanished"
	case *frontendv1.FeedShellLost_WentSilent:
		return "went_silent"
	case *frontendv1.FeedShellLost_SweptUp:
		return "swept_up"
	}
	return "unset"
}

// dlDriveDetachedLive submits `!bash-detach-live` — the one scenario whose
// spool NEVER carries an `EXIT=` terminator and whose vendor sends no
// terminal notification (shell.ts's BASH_DETACH_LIVE), which is precisely
// the precondition every LOST arm needs: a run the sidecar can only ever
// stop seeing, never watch finish.
//
// The health fault is DECLARED, not tolerated: work that outlives its turn
// opens one by design, exactly as the detached-bash family's own tests
// declare it.
func dlDriveDetachedLive(t *testing.T, w *World) ([]*frontendv1.FeedRow, *harness.Stream[*frontendv1.FeedRow], *workspacev1.WorkspaceRef) {
	t.Helper()
	w.ExpectWarnings("daemon.health.open_fault")
	ws := dbWorkspace(t, w)
	initial, stream := dbOpenRootFeed(t, w, ws)
	turn := SubmitPrompt(t, w, ws, "!bash-detach-live")
	AwaitTurnEnded(t, w, ws, turn)
	return initial, stream, ws
}

// ===========================================================================
// (a) went_silent — the spool is still there and stopped growing.
// ===========================================================================

// TestDetachedLostWentSilent drives `!bash-detach-live`, whose spool gets one
// line and then nothing forever, against a sidecar whose SHELL-SILENCE
// window is short. The proto's arm for this is `went_silent` — "The run
// produced nothing past the reader's silence ruling" — and it is the arm
// this scenario has always been the lever for.
//
// ONLY THE SHELL-SILENCE WINDOW IS SHORTENED. The grace and the agent and
// workflow silences stay at the sidecar's production defaults, so no OTHER
// arm can reach a conclusion first and steal the subject.
func TestDetachedLostWentSilent(t *testing.T) {
	t.Parallel()
	// Arrange.
	w := NewWorldWithSidecarStaleness(t, WorldOpts{}, SidecarStaleness{ShellSilence: dlSilenceWindow})
	initial, stream, ws := dlDriveDetachedLive(t, w)
	defer stream.Close()

	ctx, cancel := context.WithTimeout(w.Ctx(), dlLostBound)
	defer cancel()

	// Act: nothing. The scenario's own silence IS the act — the spool exists,
	// carries its one line, and will never grow again.
	spool := dlAwaitOneSpool(t, ctx, w)

	// Assert: the arm, on the sidecar's own `reason` key.
	dlAwaitLostReason(t, ctx, w, ws.GetDir(), spool, "went_silent")

	// Assert: the feed draws it LOST, and the spool it managed to write is
	// still carried — a LOST conclusion never drops what was observed.
	shell, got := dlAwaitLostShell(t, ctx, w, ws, initial, stream, "sleep 100000", "went_silent")
	if !strings.Contains(got, "partial output with no terminator") {
		t.Errorf("the LOST shell's spool = %q, want the one line the run managed to write before it went silent", got)
	}
	if shell.GetSettled().GetExit() != nil {
		t.Errorf("the LOST shell carries exit %v; a run we merely stopped seeing reported no status, "+
			"and an exit chip would state one it never gave", shell.GetSettled().GetExit())
	}
}

// ===========================================================================
// (b) file_vanished — the spool disappeared under the reader.
// ===========================================================================

// TestDetachedLostFileVanished deletes the spool the fake wrote, under the
// sidecar, and asserts the `file_vanished` arm — "The spool or transcript
// file disappeared from disk", concluded only once a GRACE window has
// absorbed the ordinary rename/replace race.
//
// DELETING THE FILE IS THIS TEST'S OWN ACT, not a reach into a subject: the
// spool root is the world's own t.TempDir, and the file is one this test's
// scenario wrote inside it. Nothing production-owned is touched.
func TestDetachedLostFileVanished(t *testing.T) {
	t.Parallel()
	// Arrange: only the GRACE window is bought; the silence windows stay at
	// production length so `went_silent` cannot conclude the run first.
	w := NewWorldWithSidecarStaleness(t, WorldOpts{}, SidecarStaleness{Grace: dlGraceWindow})
	initial, stream, ws := dlDriveDetachedLive(t, w)
	defer stream.Close()

	ctx, cancel := context.WithTimeout(w.Ctx(), dlLostBound)
	defer cancel()

	spool := dlAwaitOneSpool(t, ctx, w)

	// The bytes must be INGESTED before the file goes, or the test would be
	// racing the tail rather than exercising the policy: the feed's own live
	// spool text is the evidence the sidecar read the file, and it is a
	// bounded wait on a real signal.
	live := dbAwaitLiveDetachedShell(t, ctx, w, ws, initial, stream, "sleep 100000", "partial output with no terminator")
	if live.GetLive() == nil {
		t.Fatalf("detached shell state = %v, want live before the spool is removed", live.GetState())
	}

	// Act.
	if err := os.Remove(spool); err != nil {
		t.Fatalf("e2e: removing this test's own spool %s: %v", spool, err)
	}

	// Assert: the arm, then the feed's LOST draw.
	dlAwaitLostReason(t, ctx, w, ws.GetDir(), spool, "file_vanished")
	_, got := dlAwaitLostShell(t, ctx, w, ws, initial, stream, "sleep 100000", "file_vanished")
	if !strings.Contains(got, "partial output with no terminator") {
		t.Errorf("the LOST shell's spool = %q, want the bytes read before the file vanished — "+
			"the file going away is not a licence to drop what was already observed", got)
	}
}

// ===========================================================================
// (c) swept_up — a leftover spool that predates the machine's boot.
// ===========================================================================

// dlPreBootStamp is a time no machine's boot can be older than, which is what
// makes a file "not touched since before the reboot" without this test
// knowing when this machine actually booted. Same device, same reason, as the
// sidecar's own integration suite (lost_policy_test.go's preBootStamp).
var dlPreBootStamp = time.Date(2001, 1, 1, 0, 0, 0, 0, time.UTC)

// TestDetachedLostSweptUp drives the third arm — "A boot sweep found the run
// open with no living producer".
//
// HOW THE ARM IS REACHED, per the sidecar's own policy (internal/stale, and
// cycle.go's `bootSwept` gate): `Tracker.BootSweep` runs ONCE PER SIDECAR
// PROCESS, on its first production cycle, and concludes every tracked run
// whose file has not been written since the machine booted. The activity
// clock is the file's MTIME, never the instant the sidecar read it —
// deliberately, so that "a spool full of pre-reboot bytes" cannot be made to
// look alive by being read. So this test stamps the leftover spool
// pre-boot and RESTARTS the sidecar over it: the new process re-discovers the
// file, judges its mtime against the real boot time, and sweeps it up.
//
// THE RESTART IS THE ONLY LEVER. Boot time is read from the kernel
// (boottime_darwin.go / boottime_linux.go) and is not a flag, so a running
// sidecar's boot sweep cannot be re-armed; a second process is what runs a
// second boot sweep. Cursor recovery makes it safe: the new process recovers
// its read positions from the real store before reading a byte, so nothing
// already ingested is ingested twice.
func TestDetachedLostSweptUp(t *testing.T) {
	t.Parallel()
	// Arrange: NO window is shortened. `swept_up` is not a window at all —
	// it is the boot rule, which ranks ahead of silence precisely because it
	// says HOW we know rather than merely that the file is quiet. Leaving the
	// silence windows at production length is what proves this conclusion is
	// the boot rule's and not the silence window's.
	w := NewWorld(t, WorldOpts{})
	initial, stream, ws := dlDriveDetachedLive(t, w)
	defer stream.Close()

	ctx, cancel := context.WithTimeout(w.Ctx(), dlLostBound)
	defer cancel()

	spool := dlAwaitOneSpool(t, ctx, w)
	// The bytes must be ingested by the FIRST sidecar, so the restart is
	// resuming a run rather than discovering it for the first time.
	live := dbAwaitLiveDetachedShell(t, ctx, w, ws, initial, stream, "sleep 100000", "partial output with no terminator")
	if live.GetLive() == nil {
		t.Fatalf("detached shell state = %v, want live before the sidecar is restarted", live.GetState())
	}

	// Act: stamp the leftover spool pre-boot, then boot a second sidecar over
	// it. Stamped rather than truncated: the bytes are the run's real output
	// and the arm is about WHEN the file was last written, not about what is
	// in it.
	if err := os.Chtimes(spool, dlPreBootStamp, dlPreBootStamp); err != nil {
		t.Fatalf("e2e: stamping this test's own spool %s pre-boot: %v", spool, err)
	}
	w.Sidecar.Restart(t)

	// Assert: the arm, then the feed's LOST draw.
	dlAwaitLostReason(t, ctx, w, ws.GetDir(), spool, "swept_up")
	dlAwaitLostShell(t, ctx, w, ws, initial, stream, "sleep 100000", "swept_up")
}
