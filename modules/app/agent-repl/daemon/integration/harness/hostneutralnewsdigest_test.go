package harness

import (
	"testing"
	"time"

	"agent-repld/internal/newsdigest"
)

func TestHostNeutralNewsDigestEnvDelaysTheScheduleBeyondAnyTest(t *testing.T) {
	if !envHas(HostNeutralNewsDigestEnv(), newsdigest.EnvStartDelay+"=24h0m0s") {
		t.Fatalf("HostNeutralNewsDigestEnv() = %v, want %s=24h0m0s", HostNeutralNewsDigestEnv(), newsdigest.EnvStartDelay)
	}
}

func TestTheNeutralNewsDigestStartDelayOutlastsTheLongestSuiteTimeout(t *testing.T) {
	// The e2e suite's 45m timeout is the longest any test process lives.
	if NeutralNewsDigestStartDelay <= 45*time.Minute {
		t.Fatalf("NeutralNewsDigestStartDelay = %s, want longer than any suite's timeout", NeutralNewsDigestStartDelay)
	}
}

func TestStartDaemonAppliesHostNeutralNewsDigestEnvAfterTheInheritedEnvironment(t *testing.T) {
	// The one call site must state the delayed schedule after os.Environ()
	// and before ExtraEnv, so it beats the host's value and a digest test can
	// still set its own.
	assertAppendedBetweenInheritedAndExtra(t, "HostNeutralNewsDigestEnv")
}
