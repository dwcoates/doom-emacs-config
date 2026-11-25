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
