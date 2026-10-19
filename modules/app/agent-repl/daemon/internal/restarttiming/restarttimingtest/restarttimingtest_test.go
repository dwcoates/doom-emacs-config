package restarttimingtest

import (
	"testing"
	"time"

	"agent-repld/internal/restarttiming"
)

func TestRecorderKeepsEveryObservationInOrder(t *testing.T) {
	// Arrange
	var r Recorder
	first := restarttiming.OfDeploy(restarttiming.DeployBuild, restarttiming.OK, time.Second)
	second := restarttiming.OfBoot(restarttiming.BootServing, restarttiming.BootCold, time.Second)

	// Act
	r.ObserveRestart(first)
	r.ObserveRestart(second)

	// Assert
	got := r.Observations()
	if len(got) != 2 || got[0] != first || got[1] != second {
		t.Fatalf("Observations = %+v, want both in order", got)
	}
}

func TestOfAnswersOneRestartsObservations(t *testing.T) {
	// Arrange
	var r Recorder
	r.ObserveRestart(restarttiming.OfDeploy(restarttiming.DeployBuild, restarttiming.OK, time.Second))
	boot := restarttiming.OfBoot(restarttiming.BootServing, restarttiming.BootCold, time.Second)
	r.ObserveRestart(boot)

	// Act
	got := r.Of(restarttiming.DaemonBoot)

	// Assert
	if len(got) != 1 || got[0] != boot {
		t.Fatalf("Of(daemon_boot) = %+v, want only the boot", got)
	}
}
