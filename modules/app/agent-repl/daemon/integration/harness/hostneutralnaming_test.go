package harness

import (
	"strings"
	"testing"

	"agent-repld/internal/workspace"
)

func TestHostNeutralNamingEnvClearsThePrefix(t *testing.T) {
	if !envHas(HostNeutralNamingEnv(), workspace.PrefixEnv+"=") {
		t.Fatalf("HostNeutralNamingEnv() = %v, want %s stated empty", HostNeutralNamingEnv(), workspace.PrefixEnv)
	}
}

func TestHostNeutralNamingEnvClearsTheLegacyPrefix(t *testing.T) {
	if !envHas(HostNeutralNamingEnv(), workspace.LegacyPrefixEnv+"=") {
		t.Fatalf("HostNeutralNamingEnv() = %v, want %s stated empty", HostNeutralNamingEnv(), workspace.LegacyPrefixEnv)
	}
}

func TestHostNeutralNamingEnvLeavesWorkspacePrefixUnsetUnderAnInheritedOne(t *testing.T) {
	// Arrange: the developer's shell carries a prefix, e.g. CLAUDE_WORKSPACE_PREFIX=ABC.
	t.Setenv(workspace.PrefixEnv, "ABC")
	t.Setenv(workspace.LegacyPrefixEnv, "ABC")

	// Act: apply the env the way os/exec does, the last value of a key winning.
	for _, kv := range HostNeutralNamingEnv() {
		k, v, _ := strings.Cut(kv, "=")
		t.Setenv(k, v)
	}

	// Assert.
	if got := workspace.Prefix(); got != "" {
		t.Fatalf("workspace.Prefix() = %q, want empty", got)
	}
}

func TestStatedEmptyStatesEachKeyEmptyInOrder(t *testing.T) {
	// Act.
	got := statedEmpty("A", "B")

	// Assert.
	if strings.Join(got, ",") != "A=,B=" {
		t.Fatalf("statedEmpty(A, B) = %v, want [A= B=]", got)
	}
}

func envHas(env []string, entry string) bool {
	for _, kv := range env {
		if kv == entry {
			return true
		}
	}
	return false
}
