package rollout

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/creategate"
)

func refusedSuccessor(t *testing.T, g *creategate.Gate) (string, bool) {
	t.Helper()
	_, release, err := g.Admit(context.Background())
	release()
	var closed *creategate.ClosedError
	if !errors.As(err, &closed) {
		return "", false
	}
	return closed.Successor, true
}

func TestAHandoverClosesTheCreateGateNamingItsSuccessor(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)

	// Act
	if err := runHandover(t, h, 1); err != nil {
		t.Fatalf("Handover: %v", err)
	}

	// Assert
	if successor, refused := refusedSuccessor(t, h.creates); !refused || successor != "127.0.0.1:7788" {
		t.Fatalf("a create after the handover: refused %v naming %q, want refused naming the successor", refused, successor)
	}
}

func TestAHandoverListsWhatItServesOnlyOnceTheCreatesInFlightFinish(t *testing.T) {
	// Arrange: one workspace served, and a create in flight.
	h := newHarness(t)
	h.workspace(t)
	_, release, err := h.creates.Admit(context.Background())
	if err != nil {
		t.Fatalf("Admit: %v", err)
	}
	type accepted struct {
		workspaces int
		err        error
	}
	done := make(chan accepted, 1)
	go func() {
		got, err := h.c.HandOver(context.Background(), unforcedAsk)
		done <- accepted{got.Workspaces, err}
	}()
	// The handover is waiting on the create once it arms the holdout cadence.
	h.clock.awaitArmed(t, holdoutCadence)

	// Act: the create registers its workspace, then finishes.
	late, _ := h.workspace(t)
	release()
	got := <-done

	// Assert: the workspace the create made is in what the handover moves.
	if got.err != nil || got.workspaces != 2 {
		t.Fatalf("HandOver = %d workspaces, %v; want 2, the late create's %s among them", got.workspaces, got.err, late)
	}
}

func TestAHandoverWaitingOnCreatesNamesThemOnTheHoldoutCadence(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)
	_, release, _ := h.creates.Admit(context.Background())
	done := make(chan error, 1)
	go func() {
		_, err := h.c.HandOver(context.Background(), unforcedAsk)
		done <- err
	}()
	h.clock.awaitArmed(t, holdoutCadence)

	// Act
	h.clock.Fire(holdoutCadence)
	h.clock.awaitArmed(t, holdoutCadence)
	release()
	<-done

	// Assert
	if !hasRecord(h, opHandover, "warn", "still waiting for creates in flight to finish before listing what this daemon serves") {
		t.Fatalf("no holdout warning naming the creates in flight")
	}
}

func TestAnAbandonedHandoverReopensTheCreateGate(t *testing.T) {
	// Arrange: the successor never proves it is serving.
	h := newHarness(t)
	h.workspace(t)
	h.spawner.readyErr = errFake

	// Act
	_, err := h.c.HandOver(context.Background(), unforcedAsk)

	// Assert
	if err == nil {
		t.Fatalf("HandOver succeeded with a successor that never answered")
	}
	if _, refused := refusedSuccessor(t, h.creates); refused {
		t.Fatalf("a create after the abandoned handover was refused, want admitted")
	}
}

func TestARestartClosesTheCreateGateNamingNoSuccessor(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)

	// Act
	if _, err := h.c.Restart(context.Background(), unforcedAsk); err != nil {
		t.Fatalf("Restart: %v", err)
	}

	// Assert
	if successor, refused := refusedSuccessor(t, h.creates); !refused || successor != "" {
		t.Fatalf("a create during the restart: refused %v naming %q, want refused naming none", refused, successor)
	}
}

func TestAHandoverThatCannotFinishReopensTheCreateGate(t *testing.T) {
	// Arrange: the transfer fails and its take-back does too, so this daemon
	// stays.
	h := newHarness(t)
	h.workspace(t)
	failTransferUncleanly(h)

	// Act
	if _, err := h.c.HandOver(context.Background(), unforcedAsk); err != nil {
		t.Fatalf("HandOver: %v", err)
	}
	h.registry.wait()
	h.c.handoverDone.Wait()

	// Assert
	if _, refused := refusedSuccessor(t, h.creates); refused {
		t.Fatalf("a create after the unfinished handover was refused, want admitted by the daemon still serving")
	}
}

func TestAHandoverWhoseTransferFailedWholeKeepsTheCreateGateClosed(t *testing.T) {
	// Arrange: the transfer fails with the workspace taken back whole, so the
	// handover goes on and offers it again.
	h := newHarness(t)
	h.workspace(t)
	setSuspendErr(h, errors.New("the merge did not let go"))

	// Act
	handOverWithAWholeFailure(t, h)

	// Assert
	if _, refused := refusedSuccessor(t, h.creates); !refused {
		t.Fatalf("a create during the converging handover was admitted, want it refused naming the successor")
	}
}

func TestARestartThatCannotFinishReopensTheCreateGate(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.workspace(t)
	h.spawner.replacementErr = errFake

	// Act
	runRestart(t, h)

	// Assert
	if _, refused := refusedSuccessor(t, h.creates); refused {
		t.Fatalf("a create after the abandoned restart was refused, want admitted")
	}
}
