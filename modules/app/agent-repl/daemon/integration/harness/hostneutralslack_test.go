package harness

import (
	"testing"

	"agent-repld/internal/externalbrowser"
)

func TestHostNeutralSlackEnvClearsTheSlackTeams(t *testing.T) {
	if !envHas(HostNeutralSlackEnv(), externalbrowser.EnvSlackTeams+"=") {
		t.Fatalf("HostNeutralSlackEnv() = %v, want %s stated empty", HostNeutralSlackEnv(), externalbrowser.EnvSlackTeams)
	}
}

func TestStartDaemonAppliesHostNeutralSlackEnvAfterTheInheritedEnvironment(t *testing.T) {
	// The one call site must state the cleared teams after os.Environ() and
	// before ExtraEnv, so it beats the host's value and a test can still set one.
	assertAppendedBetweenInheritedAndExtra(t, "HostNeutralSlackEnv")
}
