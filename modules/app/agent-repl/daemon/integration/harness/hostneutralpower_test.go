package harness

import (
	"testing"

	"agent-repld/internal/cpugov"
)

func TestHostNeutralPowerEnvPinsACPower(t *testing.T) {
	if !envHas(HostNeutralPowerEnv(), cpugov.FixedPowerEnv+"=ac") {
		t.Fatalf("HostNeutralPowerEnv() = %v, want %s=ac", HostNeutralPowerEnv(), cpugov.FixedPowerEnv)
	}
}

func TestStartDaemonAppliesHostNeutralPowerEnvAfterTheInheritedEnvironment(t *testing.T) {
	// The one call site must state the pinned power source after os.Environ()
	// and before ExtraEnv, so it beats the host's value and a test can still
	// set one.
	assertAppendedBetweenInheritedAndExtra(t, "HostNeutralPowerEnv")
}
