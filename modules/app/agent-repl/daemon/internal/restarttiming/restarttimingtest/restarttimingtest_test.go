package restarttimingtest

import (
	"slices"
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

func TestWaveMarksAnswersEveryWaveMarkInOrder(t *testing.T) {
	// Arrange
	var r Recorder
	r.ObserveRestart(restarttiming.OfReconnectWave(restarttiming.ReconnectHost, restarttiming.WaveFirst, restarttiming.ReconnectSuccessor, 2*time.Second))
	r.ObserveRestart(restarttiming.OfDeploy(restarttiming.DeployBuild, restarttiming.OK, time.Second))
	r.ObserveRestart(restarttiming.OfReconnectWave(restarttiming.ReconnectWeb, restarttiming.WaveSpread, restarttiming.ReconnectWebappReload, 1500*time.Millisecond))

	// Act
	got := r.WaveMarks()

	// Assert
	want := []string{"host_first successor 2", "web_spread webapp_reload 1.5"}
	if !slices.Equal(got, want) {
		t.Fatalf("WaveMarks = %v, want %v", got, want)
	}
}

func TestWaveMarksAnswersNothingWithoutAWave(t *testing.T) {
	// Arrange
	var r Recorder
	r.ObserveRestart(restarttiming.OfDeploy(restarttiming.DeployBuild, restarttiming.OK, time.Second))

	// Act
	got := r.WaveMarks()

	// Assert
	if len(got) != 0 {
		t.Fatalf("WaveMarks = %v, want none", got)
	}
}

func TestWindowsKeepsEveryWindowAndMarksItClosed(t *testing.T) {
	// Arrange
	var w Windows
	closeFirst := w.OpenWindow(restarttiming.DaemonBoot, "sessions_up", time.Minute, restarttiming.Watched{Process: restarttiming.ProcessDaemon})
	w.OpenWindow(restarttiming.ServiceRestart, "store", time.Second)

	// Act
	closeFirst()

	// Assert
	got := w.Windows()
	if len(got) != 2 || !got[0].Closed || got[1].Closed || got[0].Watch[0].Process != restarttiming.ProcessDaemon || got[1].Phase != "store" {
		t.Fatalf("windows = %+v, want both in order, the first closed", got)
	}
}
