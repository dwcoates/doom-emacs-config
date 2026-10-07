package harness

import (
	"strings"
	"testing"

	"claude-repld/internal/chessboard"
)

func TestHostNeutralCheckoutEnvClearsTheEngineDir(t *testing.T) {
	if !envHas(HostNeutralCheckoutEnv(), chessboard.EngineDirEnv+"=") {
		t.Fatalf("HostNeutralCheckoutEnv() = %v, want %s stated empty", HostNeutralCheckoutEnv(), chessboard.EngineDirEnv)
	}
}

func TestHostNeutralCheckoutEnvLeavesTheWorldsMultiRepoRootInForce(t *testing.T) {
	// StartDaemon states MULTI_REPO_ROOT as the world's own tree, which the
	// account routing reads; a cleared value here would override it.
	for _, kv := range HostNeutralCheckoutEnv() {
		if strings.HasPrefix(kv, chessboard.MultiRepoRootEnv+"=") {
			t.Fatalf("HostNeutralCheckoutEnv() = %v, want no %s", HostNeutralCheckoutEnv(), chessboard.MultiRepoRootEnv)
		}
	}
}

func TestStartDaemonAppliesHostNeutralCheckoutEnvAfterTheInheritedEnvironment(t *testing.T) {
	// The one call site must state the cleared checkout after os.Environ() and
	// before ExtraEnv, so it beats the host's value and a test can still set one.
	assertAppendedBetweenInheritedAndExtra(t, "HostNeutralCheckoutEnv")
}
