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
