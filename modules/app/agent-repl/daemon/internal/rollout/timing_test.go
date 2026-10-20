package rollout

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/restarttiming"
	"agent-repld/internal/wsm"
)

// timed answers one restart's observations for ws, each as
// phase:cause:outcome, in the order they were recorded.
func timed(h *harness, restart restarttiming.Restart, ws ids.WorkspaceID) []string {
	var out []string
	for _, o := range h.restarts.Of(restart) {
		if o.Workspace() == ws {
			out = append(out, o.Phase()+":"+o.Cause()+":"+string(o.Outcome()))
		}
	}
	return out
}

func TestAFinishedShimRestartTimesEveryStep(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	if err := runRelaunch(t, h, ws, ReasonRestartVerb); err != nil {
		t.Fatalf("bounce: %v", err)
	}

	// Assert
	want := []string{
		"prelaunch:restart_verb:ok", "stand_down:restart_verb:ok", "resume:restart_verb:ok",
		"wait:restart_verb:ok", "run:restart_verb:ok",
	}
	if got := timed(h, restarttiming.ShimRestart, ws); strings.Join(got, ",") != strings.Join(want, ",") {
		t.Fatalf("timed = %v, want %v", got, want)
	}
}

func TestAFailedShimRestartTimesItsWaitAndRunAsFailed(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.fleet.prelaunchErr[ws] = errFake

	// Act
	if err := bounceAndWait(t, h, ws, ReasonBuildStale, false); err == nil {
		t.Fatalf("the bounce succeeded with no shim to swap onto")
	}

	// Assert
	want := []string{"wait:build_stale:failed", "run:build_stale:failed"}
	if got := timed(h, restarttiming.ShimRestart, ws); strings.Join(got, ",") != strings.Join(want, ",") {
		t.Fatalf("timed = %v, want %v", got, want)
	}
}

func TestADeferredShimRestartIsNotTimedAsEnded(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	deferredBounce(t, h, ws, ReasonBuildStale)

	// Assert: only the prelaunch, which did finish, is timed.
	want := []string{"prelaunch:build_stale:ok"}
	if got := timed(h, restarttiming.ShimRestart, ws); strings.Join(got, ",") != strings.Join(want, ",") {
		t.Fatalf("timed = %v, want %v", got, want)
	}
}

func TestAHandoversTransferIsTimed(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	if err := runHandover(t, h, 1); err != nil {
		t.Fatalf("Handover: %v", err)
	}

	// Assert
	want := []string{"wait::ok", "run::ok"}
	if got := timed(h, restarttiming.Transfer, ws); strings.Join(got, ",") != strings.Join(want, ",") {
		t.Fatalf("timed = %v, want %v", got, want)
	}
}

func TestAHandoverOrRestartAskedWithoutItsDeploysStartIsRefused(t *testing.T) {
	tests := []struct {
		name string
		ask  func(h *harness) error
	}{
		{"a handover", func(h *harness) error { _, err := h.c.HandOver(context.Background(), Ask{}); return err }},
		{"a restart", func(h *harness) error { _, err := h.c.Restart(context.Background(), Ask{Force: true}); return err }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.workspace(t)

			// Act
			err := tc.ask(h)

			// Assert
			if !errors.Is(err, ErrNoDeployStart) {
				t.Fatalf("err = %v, want ErrNoDeployStart", err)
			}
			h.spawner.mu.Lock()
			spawned, replacements := len(h.spawner.told), h.spawner.replacements
			h.spawner.mu.Unlock()
			if spawned != 0 || replacements != 0 {
				t.Fatalf("a successor was spawned for an ask with no deploy start")
			}
			found := false
			for _, r := range records(h.log, opRollOut) {
				if r.Level == dlog.LevelError && strings.Contains(r.Message, "without its deploy's start") {
					found = true
				}
			}
			if !found {
				t.Fatalf("records = %+v, want the refusal at ERROR", records(h.log, opRollOut))
			}
		})
	}
}

func TestATakeoversSessionStartsAreTimedAsATakeovers(t *testing.T) {
	// Arrange
	h := newHarness(t)
	ws, _ := h.workspace(t)

	// Act
	h.c.bringUpSessionless([]wsm.Workspace{sessionlessRecord(t, h, ws)}, nil)
	h.c.bringUps.Wait()

	// Assert
	got := h.restarts.Of(restarttiming.WorkspaceStart)
	if len(got) != 1 || got[0].Workspace() != ws || got[0].Cause() != string(restarttiming.StartTakeover) {
		t.Fatalf("workspace starts = %+v, want one takeover start of %s", got, ws)
	}
}

func TestAHandoversParticipantsAreTimedBackFromItsManifest(t *testing.T) {
	// Arrange: the manifest was written 4s before the harness's instant, and
	// names this workspace's host stream but not its web stream.
	h := newHarness(t)
	ws, _ := h.workspace(t)
	h.c.armSessions(ids.InstanceID("daemon-outgoing"), instant.Add(-4*time.Second), []ManifestSession{{Workspace: ws, ExpectedHost: true}})

	// Act
	h.c.deps.Reconnects.Connected(ws, restarttiming.ReconnectHost)
	h.c.deps.Reconnects.Connected(ws, restarttiming.ReconnectWeb)

	// Assert
	got := h.restarts.Of(restarttiming.WorkspaceReconnect)
	if len(got) != 1 || got[0].Phase() != "host" || got[0].Cause() != "successor" || got[0].Seconds() != 4 {
		t.Fatalf("reconnects = %+v, want the host stream only, successor, 4s", got)
	}
}
