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
		name      string
		incumbent []string
		want      []string
	}{
		{name: "no flags is just the replacing flag", want: []string{"--replacing"}},
		{name: "every other flag is carried through", incumbent: []string{"--prompts-dir", "/prompts"}, want: []string{"--prompts-dir", "/prompts", "--replacing"}},
		{name: "a separated joining flag is dropped with its value", incumbent: []string{"--joining", "127.0.0.1:9", "--node", "node"}, want: []string{"--node", "node", "--replacing"}},
		{name: "an attached joining flag is dropped", incumbent: []string{"-joining=127.0.0.1:9"}, want: []string{"--replacing"}},
		{name: "an earlier replacing flag is not doubled", incumbent: []string{"-replacing", "--node", "node"}, want: []string{"--node", "node", "--replacing"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange / Act
			got := replacementArgv(tt.incumbent)

			// Assert
			if !slices.Equal(got, tt.want) {
				t.Fatalf("argv = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestWithoutRoleFlags(t *testing.T) {
	tests := []struct {
		name      string
		incumbent []string
		want      []string
	}{
		{name: "no flags is no flags", want: []string{}},
		{name: "a non-role flag is carried through", incumbent: []string{"--prompts-dir", "/prompts"}, want: []string{"--prompts-dir", "/prompts"}},
		{name: "a separated joining flag is dropped with its value", incumbent: []string{"--joining", "127.0.0.1:9", "--node", "node"}, want: []string{"--node", "node"}},
		{name: "an attached joining flag is dropped alone", incumbent: []string{"-joining=127.0.0.1:9", "--node", "node"}, want: []string{"--node", "node"}},
		{name: "a trailing joining flag with no value is dropped", incumbent: []string{"--node", "node", "--joining"}, want: []string{"--node", "node"}},
		{name: "a replacing flag is dropped", incumbent: []string{"--replacing", "--node", "node"}, want: []string{"--node", "node"}},
		{name: "an attached replacing flag is dropped", incumbent: []string{"-replacing=true", "--node", "node"}, want: []string{"--node", "node"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange / Act
			got := withoutRoleFlags(tt.incumbent)

			// Assert
			if !slices.Equal(got, tt.want) {
				t.Fatalf("argv = %v, want %v", got, tt.want)
			}
		})
	}
}

// TestEverySpawnArgvIsTheRolelessArgvPlusItsOwnRole pins that both argv
// builders share withoutRoleFlags: a builder that strips its own way would
// leave a role flag in one of them, and the daemon it spawns refuses to start.
func TestEverySpawnArgvIsTheRolelessArgvPlusItsOwnRole(t *testing.T) {
	incumbent := []string{"--default-config-dir", "/roots", "--replacing", "-joining=127.0.0.1:9", "--node", "node"}
	roleless := withoutRoleFlags(incumbent)
	tests := []struct {
		name string
		got  []string
		role []string
	}{
		{name: "successor", got: successorArgv(incumbent, "127.0.0.1:1"), role: []string{JoiningFlag, "127.0.0.1:1"}},
		{name: "replacement", got: replacementArgv(incumbent), role: []string{"--" + ReplacingFlagName}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			want := append(slices.Clone(roleless), tt.role...)

			// Act / Assert
			if !slices.Equal(tt.got, want) {
				t.Fatalf("argv = %v, want %v", tt.got, want)
			}
		})
	}
}

func TestARestartStandDownWaitsForFreeness(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.freeness.SetFree(ws, false)

	// Act
	if _, err := h.c.Restart(context.Background(), false); err != nil {
		t.Fatalf("Restart: %v", err)
	}

	// Assert: the stand-down is not a handover, and keeps the freeness gate.
	requests := h.registry.Requests()
	if len(requests) != 1 || requests[0].Req.WaitFor != bounce.GateFreeness {
		t.Fatalf("requests = %+v, want one stand-down at freeness", requests)
	}
	if !h.registry.Pending(ws) {
		t.Fatalf("the busy workspace's stand-down is not registered behind its work")
	}
}

func TestARestartStandDownCarriesNothing(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	runRestart(t, h)

	// Assert
	if _, found, err := h.c.readCarry(ws); err != nil || found {
		t.Fatalf("readCarry = (%v, %v), want no carry: a restart has no successor to carry to", found, err)
	}
}
