package harness

import (
	"os"
	"regexp"
	"strings"
	"testing"

	"claude-repld/internal/workspace"
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

func TestStartDaemonAppliesHostNeutralNamingEnvAfterTheInheritedEnvironment(t *testing.T) {
	// The one call site must state the cleared prefix after os.Environ() and
	// before ExtraEnv, so it beats the host's value and a test can still set one.
	src, err := os.ReadFile("daemon.go")
	if err != nil {
		t.Fatal(err)
	}
	body := string(src)
	inherited := strings.Index(body, "env := append(os.Environ(),")
	neutral := regexp.MustCompile(`env = append\(env, HostNeutralNamingEnv\(\)\.\.\.\)`).FindStringIndex(body)
	extra := strings.Index(body, "opts.ExtraEnv...")
	if inherited < 0 || neutral == nil || extra < 0 || !(inherited < neutral[0] && neutral[0] < extra) {
		t.Fatalf("StartDaemon must append HostNeutralNamingEnv() between os.Environ() and opts.ExtraEnv (offsets %d, %v, %d)", inherited, neutral, extra)
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
