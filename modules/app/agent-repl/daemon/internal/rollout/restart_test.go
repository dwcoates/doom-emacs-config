package rollout

import (
	"context"
	"errors"
	"slices"
	"testing"

	"claude-repld/internal/bounce"
	"claude-repld/internal/ids"
)

// runRestart runs a restart of every workspace and joins it to its end.
func runRestart(t *testing.T, h *harness) {
	t.Helper()
	if _, err := h.c.Restart(context.Background(), false); err != nil {
		t.Fatalf("Restart: %v", err)
	}
	h.registry.wait()
	h.c.handoverDone.Wait()
}

func TestARestartAnnouncesAPlainBounce(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)

	// Act
	runRestart(t, h)

	// Assert
	sent := h.announcer.Sent()
	if len(sent) != 1 || sent[0].Address != nil || sent[0].GetCause().GetSelfMergeRollout() == nil {
		t.Fatalf("announcements = %v, want one self-merge rollout with no successor address", sent)
	}
}

func TestARestartSpawnsNoJoiningSuccessor(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)

	// Act
	runRestart(t, h)

	// Assert
	if told := h.spawner.Told(); len(told) != 0 {
		t.Fatalf("successors spawned = %v, want none: a joining successor cannot open an older layout", told)
	}
}

func TestARestartWritesTheIntentManifestForTheReplacementsAccounting(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)

	// Act
	if _, err := h.c.Restart(context.Background(), false); err != nil {
		t.Fatalf("Restart: %v", err)
	}
	got, found, err := ReadManifest(h.c.deps.IntentManifest)

	// Assert
	if err != nil || !found {
		t.Fatalf("ReadManifest = (found %v, %v), want the restart's manifest", found, err)
	}
	if got.Successor != "" || len(got.Sessions) != 1 || got.Sessions[0].Workspace != ws || got.Sessions[0].Intent != IntentPreserve {
		t.Fatalf("manifest = %+v, want one preserved session and no successor", got)
	}
}

func TestARestartStandsAWorkspaceDownWithoutATransferNotice(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	runRestart(t, h)

	// Assert
	if owner, err := h.db.Serving(context.Background(), ws); err != nil || owner != nil {
		t.Fatalf("serving owner after the stand-down = %v (%v), want released for the replacement", owner, err)
	}
	if calls := h.pusher.Calls(); len(calls) != 0 {
		t.Fatalf("pushes = %v, want no transfer notice: there is no successor to name", calls)
	}
	if !slices.Contains(h.order.Taken(), "detach") {
		t.Fatalf("steps = %v, want the shim detached and left running for the replacement", h.order.Taken())
	}
}

func TestACompletedRestartSpawnsTheReplacementThenExits(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)

	// Act
	runRestart(t, h)

	// Assert
	if n := h.spawner.Replacements(); n != 1 {
		t.Fatalf("replacements = %d, want one", n)
	}
	awaitExit(t, h)
}

func TestARestartWhoseReplacementWillNotStartTakesEveryWorkspaceBack(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.spawner.replacementErr = errFake

	// Act
	runRestart(t, h)

	// Assert
	owner, err := h.db.Serving(context.Background(), ws)
	if err != nil || owner == nil || *owner != selfInstance {
		t.Fatalf("serving owner = %v (%v), want this daemon serving again", owner, err)
	}
	if _, held, err := h.db.Lease(context.Background(), ws); err != nil || held {
		t.Fatalf("Lease = (held %v, %v), want the stand-down's hold released", held, err)
	}
	if adoptions := h.fleet.Adoptions(); len(adoptions) != 1 || adoptions[0] != ws {
		t.Fatalf("adoptions = %v, want the detached shim re-attached", adoptions)
	}
}

func TestARestartThatCannotFinishDoesNotExitAndFreesTheSlot(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)
	h.spawner.replacementErr = errFake

	// Act
	runRestart(t, h)

	// Assert
	select {
	case <-h.exits:
		t.Fatal("the daemon exited from a restart that could not finish")
	default:
	}
	if _, rolling := h.c.RollingOut(); rolling {
		t.Fatal("the abandoned restart still holds the rollout slot")
	}
	if !loggedError(h.log, opHandover, "the restart cannot finish") {
		t.Fatalf("records = %+v, want the abandoned restart at ERROR", h.log.Records())
	}
}

func TestARestartWithAFailedStandDownTakesBackTheOnesAlreadyDown(t *testing.T) {
	// Arrange: the first workspace stands down; the second's shim will not detach.
	h := newHarness(t)
	first, _ := h.workspace(t)
	second, _ := h.workspace(t)
	h.fleet.handOverErr[second] = errFake

	// Act
	runRestart(t, h)

	// Assert
	for _, ws := range []ids.WorkspaceID{first, second} {
		owner, err := h.db.Serving(context.Background(), ws)
		if err != nil || owner == nil || *owner != selfInstance {
			t.Fatalf("serving owner of %s = %v (%v), want this daemon serving again", ws, owner, err)
		}
		if _, held, err := h.db.Lease(context.Background(), ws); err != nil || held {
			t.Fatalf("Lease of %s = (held %v, %v), want no hold left behind", ws, held, err)
		}
	}
	if n := h.spawner.Replacements(); n != 0 {
		t.Fatalf("replacements = %d, want none for a restart that could not finish", n)
	}
}

func TestARestartIsRefusedWhileAHandoverIsInFlight(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)
	if _, err := h.c.HandOver(context.Background(), false); err != nil {
		t.Fatalf("HandOver: %v", err)
	}

	// Act
	_, err := h.c.Restart(context.Background(), false)

	// Assert
	var inFlight *ErrAlreadyRollingOut
	if !errors.As(err, &inFlight) {
		t.Fatalf("Restart = %v, want *ErrAlreadyRollingOut", err)
	}
}

func TestReplacementArgv(t *testing.T) {
	tests := []struct {
		name   string
		config []string
		want   []string
	}{
		{name: "no configuration is just the replacing flag", want: []string{"--replacing"}},
		{name: "the configuration is carried through ahead of the role", config: []string{"--prompts-dir=/prompts", "--node=node"}, want: []string{"--prompts-dir=/prompts", "--node=node", "--replacing"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange / Act
			got := replacementArgv(tt.config)

			// Assert
			if !slices.Equal(got, tt.want) {
				t.Fatalf("argv = %v, want %v", got, tt.want)
			}
		})
	}
}

// TestSpawnArgvLeavesTheConfigurationUntouched pins that a spawn's argv is a
// copy: the configuration is shared by every spawn this daemon makes, so an
// append that wrote into its backing array would hand one spawn's role to
// the next.
func TestSpawnArgvLeavesTheConfigurationUntouched(t *testing.T) {
	tests := []struct {
		name  string
		build func(config []string) []string
	}{
		{name: "successor", build: func(c []string) []string { return successorArgv(c, "127.0.0.1:1") }},
		{name: "replacement", build: replacementArgv},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: spare capacity, so an in-place append would land.
			config := make([]string, 1, 8)
			config[0] = "--node=node"

			// Act
			_ = tt.build(config)

			// Assert
			if got := config[:cap(config)][1]; got != "" {
				t.Fatalf("the configuration's backing array was written: %q", got)
			}
		})
	}
}

// A RESTART NEVER WAITS ON WORK (owner ruling, 2026-09-30): each workspace
// stands down the moment no prompt is mid-delivery, whatever turn or detached
// work is running, and its shim keeps running for the replacement to adopt.
func TestARestartStandDownDoesNotWaitForWork(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)

	// Act
	if _, err := h.c.Restart(context.Background(), false); err != nil {
		t.Fatalf("Restart: %v", err)
	}

	// Assert
	requests := h.registry.Requests()
	if len(requests) != 1 || requests[0].Req.WaitFor != bounce.GateDispatchQuiet {
		t.Fatalf("requests = %+v, want one stand-down at the dispatch-quiet gate", requests)
	}
}

func TestARestartStandDownCarriesTheQueueMemory(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)
	h.registry.sealed[ws] = bounce.Handoff{Head: "held-1"}

	// Act
	runRestart(t, h)

	// Assert
	carry, found, err := h.c.readCarry(ws)
	if err != nil || !found {
		t.Fatalf("readCarry = (%v, %v), want the replacement's carry", found, err)
	}
	if carry.Daemon != selfInstance || !carry.MidWork || carry.Queue.Head != "held-1" {
		t.Fatalf("carry = %+v, want this daemon's mid-work carry of the semantic head", carry)
	}
}

func TestAnAbandonedRestartPutsEveryCarryBack(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.registry.sealed[ws] = bounce.Handoff{Head: "held-1"}
	h.spawner.replacementErr = errFake

	// Act
	runRestart(t, h)

	// Assert
	if _, found, err := h.c.readCarry(ws); err != nil || found {
		t.Fatalf("readCarry = (%v, %v), want the carry retired with the take-back", found, err)
	}
	if got := h.registry.unsealed[ws]; got.Head != "held-1" {
		t.Fatalf("unsealed = %+v, want the sealed queue memory put back here", got)
	}
}

func TestACompletedRestartTellsTheCarriedReplacementItWasHandedAcross(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	told := make(chan error, 1)
	h.registry.across[ws] = []bounce.Request{{Reason: string(ReasonRestartVerb), Force: true, Done: func(err error) { told <- err }}}

	// Act
	runRestart(t, h)

	// Assert
	select {
	case err := <-told:
		if !errors.Is(err, bounce.ErrHandedAcross) {
			t.Fatalf("the carried restart was told %v, want handed across", err)
		}
	default:
		t.Fatalf("the carried restart's requester was never told its outcome")
	}
}
