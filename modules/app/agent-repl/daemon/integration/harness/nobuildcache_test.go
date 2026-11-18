package harness

import (
	"slices"
	"testing"
)

func TestNoBuildCacheEnvTurnsTheBuildCacheAndTheGoEnvironmentFileOff(t *testing.T) {
	// Act.
	got := NoBuildCacheEnv()

	// Assert.
	if want := []string{"GOCACHE=off", "GOENV=off"}; !slices.Equal(got, want) {
		t.Fatalf("NoBuildCacheEnv() = %v, want %v", got, want)
	}
}

func TestStartDaemonAppliesNoBuildCacheEnvAfterTheInheritedEnvironment(t *testing.T) {
	// The one call site must state it after os.Environ() and before ExtraEnv,
	// so it beats the host's cache and a test can still name one.
	assertAppendedBetweenInheritedAndExtra(t, "NoBuildCacheEnv")
}
