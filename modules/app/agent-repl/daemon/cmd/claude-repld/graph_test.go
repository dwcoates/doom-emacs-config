package main

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// TestResolveShimBuildSHAPrecedence covers each source the shim build sha can
// come from, one case per source.
func TestResolveShimBuildSHAPrecedence(t *testing.T) {
	tests := []struct {
		name  string
		stamp string // the stamp file's content; "" means no file at all
		env   string
		want  string
	}{
		{name: "the stamp answers when it exists", stamp: "from-stamp\n", env: "from-env", want: "from-stamp"},
		{name: "the environment answers when the stamp is absent", stamp: "", env: "from-env", want: "from-env"},
		{name: "the environment is trimmed", stamp: "", env: "  spaced  ", want: "spaced"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			path := filepath.Join(t.TempDir(), ".built-sha")
			if tc.stamp != "" {
				if err := os.WriteFile(path, []byte(tc.stamp), 0o644); err != nil {
					t.Fatalf("write the stamp: %v", err)
				}
			}

			// Act.
			got, err := resolveShimBuildSHA(path, tc.env)

			// Assert.
			if err != nil {
				t.Fatalf("resolveShimBuildSHA = %v, want %q", err, tc.want)
			}
			if got != tc.want {
				t.Fatalf("resolveShimBuildSHA = %q, want %q", got, tc.want)
			}
		})
	}
}

// TestResolveShimBuildSHAWithNeitherSourceRefuses covers the boot refusal,
// which must name both sources.
func TestResolveShimBuildSHAWithNeitherSourceRefuses(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), ".built-sha")

	// Act.
	_, err := resolveShimBuildSHA(path, "")

	// Assert.
	if err == nil {
		t.Fatalf("resolveShimBuildSHA = nil error, want a refusal")
	}
	if !strings.Contains(err.Error(), path) || !strings.Contains(err.Error(), envShimBuildSHA) {
		t.Fatalf("refusal = %q, want it to name both %q and %q", err, path, envShimBuildSHA)
	}
}

// TestResolveShimBuildSHAWithABlankStampRefuses covers a present but empty
// stamp, which must never be treated as an absent one.
func TestResolveShimBuildSHAWithABlankStampRefuses(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), ".built-sha")
	if err := os.WriteFile(path, []byte("  \n"), 0o644); err != nil {
		t.Fatalf("write the stamp: %v", err)
	}

	// Act.
	_, err := resolveShimBuildSHA(path, "from-env")

	// Assert.
	if err == nil {
		t.Fatalf("resolveShimBuildSHA = nil error, want a refusal")
	}
	if !strings.Contains(err.Error(), "is empty") {
		t.Fatalf("refusal = %q, want it to name the empty stamp", err)
	}
}

func TestAnUnsetHoldoutWarnCadenceLeavesTheDefaultToTheController(t *testing.T) {
	// Arrange.
	t.Setenv(HoldoutWarnEnv, "")

	// Act.
	got, err := resolveHoldoutWarnEvery()

	// Assert.
	if err != nil || got != 0 {
		t.Fatalf("resolveHoldoutWarnEvery() = (%v, %v), want (0, nil) so the controller's default stands", got, err)
	}
}

func TestTheHoldoutWarnCadenceComesFromTheEnvironment(t *testing.T) {
	// Arrange.
	t.Setenv(HoldoutWarnEnv, "250ms")

	// Act.
	got, err := resolveHoldoutWarnEvery()

	// Assert.
	if err != nil || got != 250*time.Millisecond {
		t.Fatalf("resolveHoldoutWarnEvery() = (%v, %v), want 250ms", got, err)
	}
}

func TestAMalformedHoldoutWarnCadenceIsRefused(t *testing.T) {
	// Arrange.
	t.Setenv(HoldoutWarnEnv, "soon")

	// Act.
	_, err := resolveHoldoutWarnEvery()

	// Assert.
	if err == nil {
		t.Fatal("resolveHoldoutWarnEvery() accepted a malformed cadence; a silent test knob makes its suite lie")
	}
}

func TestANonPositiveHoldoutWarnCadenceIsRefused(t *testing.T) {
	// Arrange.
	t.Setenv(HoldoutWarnEnv, "0s")

	// Act.
	_, err := resolveHoldoutWarnEvery()

	// Assert.
	if err == nil {
		t.Fatal("resolveHoldoutWarnEvery() accepted a non-positive cadence")
	}
}
