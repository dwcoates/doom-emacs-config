package deploy

import (
	"context"
	"errors"
	"fmt"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/internal/testsclient"
)

// testsLoaded makes the harness's test service loaded from its definition, so
// its restart is a kickstart.
func testsLoaded(t *testing.T, h *restarterHarness) {
	t.Helper()
	writeFile(t, filepath.Join(h.services.dir, TestsLabel+".plist"), "<plist/>")
}

func TestRestartingTheTestServiceHoldsTheGatesFromBeforeItUntilItAnswers(t *testing.T) {
	// Arrange
	h := newRestarter(t)
	testsLoaded(t, h)

	// Act
	err := h.r.RestartStandalone(context.Background(), Tests)

	// Assert: taken off before any act, released after the kickstart with
	// the service answering.
	if err != nil || fmt.Sprint(h.acts()) != fmt.Sprint([]string{"define " + TestsLabel, "restart " + TestsLabel}) {
		t.Fatalf("RestartStandalone = %v, acts %v; want one kickstart", err, h.acts())
	}
	if fmt.Sprint(h.testRuns.takenAt) != "[0]" || fmt.Sprint(h.testRuns.releasedAt) != "[2]" || fmt.Sprint(h.testRuns.answered) != "[<nil>]" {
		t.Fatalf("taken off at %v, released at %v told %v; want off before the first act and released answered after the kickstart",
			h.testRuns.takenAt, h.testRuns.releasedAt, h.testRuns.answered)
	}
	if h.probes() != 1 || !loggedTo(h.log, "info", "the test service answers again after its restart") {
		t.Fatalf("probes %d, records %+v; want the one answer recorded", h.probes(), h.log.Records())
	}
}

func TestARestartOfTheTestServiceWhoseGatesCannotBeTakenOffRestartsNothing(t *testing.T) {
	// Arrange
	h := newRestarter(t)
	testsLoaded(t, h)
	h.testRuns.err = errors.New("a gate did not let go of its run")

	// Act
	err := h.r.RestartStandalone(context.Background(), Tests)

	// Assert
	if err == nil || len(h.acts()) != 0 || h.probes() != 0 {
		t.Fatalf("RestartStandalone = %v, acts %v, probes %d; want the refusal and nothing restarted", err, h.acts(), h.probes())
	}
	if !loggedTo(h.log, "error", "the gates' runs could not be taken off the test service; it is NOT restarted") {
		t.Fatalf("records %+v, want the refusal at ERROR", h.log.Records())
	}
}

func TestAFailedRestartOfTheTestServiceReleasesTheGatesWithItsFailure(t *testing.T) {
	// Arrange
	h := newRestarter(t)
	testsLoaded(t, h)
	h.services.restartErr = errors.New("kickstart refused")

	// Act
	err := h.r.RestartStandalone(context.Background(), Tests)

	// Assert
	if err == nil || len(h.testRuns.answered) != 1 || h.testRuns.answered[0] != err {
		t.Fatalf("RestartStandalone = %v, released told %v; want the gates released with the failure", err, h.testRuns.answered)
	}
	if h.probes() != 0 {
		t.Fatalf("probes %d, want no answer asked of a service whose restart failed", h.probes())
	}
}

func TestTheTestServiceIsAskedToAnswerOnlyOnceItsPreviousProcessExited(t *testing.T) {
	// Arrange: the running service's process (pid 50) has not exited yet.
	h := newRestarter(t)
	testsLoaded(t, h)
	h.services.mu.Lock()
	h.services.pid[TestsLabel] = 50
	h.services.mu.Unlock()
	h.exitsMu.Lock()
	h.exits[50] = make(chan struct{})
	h.exitsMu.Unlock()
	done := make(chan error, 1)

	// Act
	go func() { done <- h.r.RestartStandalone(context.Background(), Tests) }()
	for pid := range h.awaiting {
		if pid == 50 {
			break
		}
	}
	asked := h.probes()
	h.exit(50)
	err := <-done

	// Assert
	if asked != 0 {
		t.Fatalf("asked %d times before the previous process exited, want none", asked)
	}
	if err != nil || h.probes() != 1 {
		t.Fatalf("RestartStandalone = %v, probes %d; want the successor's one answer", err, h.probes())
	}
}

func TestATestServiceWhoseSocketIsBoundLaterIsAskedOnceItIs(t *testing.T) {
	// Arrange: the service's socket is not there when its restart ends.
	h := newRestarter(t)
	testsLoaded(t, h)
	h.r.TestsSocket = filepath.Join(filepath.Dir(h.testsSock), "tests-later.sock")
	done := make(chan error, 1)

	// Act
	go func() { done <- h.r.RestartStandalone(context.Background(), Tests) }()
	listen(t, h.r.TestsSocket)
	err := <-done

	// Assert
	if err != nil || h.probes() != 1 {
		t.Fatalf("RestartStandalone = %v, probes %d; want the service asked once its socket was bound", err, h.probes())
	}
}

func TestATestServiceThatDoesNotAnswerFailsItsRestartAtItsBound(t *testing.T) {
	// Arrange: the service never answers.
	h := newRestarter(t)
	testsLoaded(t, h)
	h.answerErr = fmt.Errorf("WatchRun: %w", testsclient.ErrUnavailable)
	h.clock.fire(testsAnswerBound)

	// Act
	err := h.r.RestartStandalone(context.Background(), Tests)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "did not answer within") || !errors.Is(err, testsclient.ErrUnavailable) {
		t.Fatalf("RestartStandalone = %v, want the unanswered restart naming its last failure", err)
	}
	if len(h.testRuns.answered) != 1 || h.testRuns.answered[0] == nil {
		t.Fatalf("released told %v, want the gates released with the failure", h.testRuns.answered)
	}
	if !loggedTo(h.log, "error", "the test service did not answer within its bound after its restart") {
		t.Fatalf("records %+v, want the failure at ERROR", h.log.Records())
	}
}

func TestARestartOfTheTestServiceWithNoGatesHoldRestartsNothing(t *testing.T) {
	// Arrange
	h := newRestarter(t)
	testsLoaded(t, h)
	h.r.TestRuns = nil

	// Act
	err := h.r.RestartStandalone(context.Background(), Tests)

	// Assert
	if err == nil || len(h.acts()) != 0 || !loggedTo(h.log, "error", "the test service cannot be restarted without its gates' hold and its answer") {
		t.Fatalf("RestartStandalone = %v, acts %v, records %+v; want the restart refused at ERROR", err, h.acts(), h.log.Records())
	}
}

func TestACurrentTestServiceTakesNoGateOff(t *testing.T) {
	// Arrange: the test service runs the installed build.
	h := newRestarter(t)
	h.running(t, TestsLabel, Tests.Service, 40, "new tests", "new tests")

	// Act
	err := h.r.EnsureStandaloneCurrent(context.Background(), Tests)

	// Assert
	if err != nil || len(h.acts()) != 0 || len(h.testRuns.takenAt) != 0 || h.probes() != 0 {
		t.Fatalf("EnsureStandaloneCurrent = %v, acts %v, taken off %v, probes %d; want nothing touched", err, h.acts(), h.testRuns.takenAt, h.probes())
	}
}

func TestARestartOfAnotherStandaloneTakesNoGateOff(t *testing.T) {
	// Arrange
	h := newRestarter(t)
	writeFile(t, filepath.Join(h.services.dir, ZettelLabel+".plist"), "<plist/>")

	// Act
	err := h.r.RestartStandalone(context.Background(), Zettel)

	// Assert
	if err != nil || len(h.testRuns.takenAt) != 0 || h.probes() != 0 {
		t.Fatalf("RestartStandalone = %v, taken off %v, probes %d; want zettel restarted alone", err, h.testRuns.takenAt, h.probes())
	}
}
