package login

import (
	"strings"
	"testing"

	"claude-repld/internal/vendorguard"
)

func TestSpawnVendor_RefusesWhenVendorCallsAreForbidden(t *testing.T) {
	// Arrange
	t.Setenv(vendorguard.EnvVar, "1")

	// Act
	proc, err := SpawnVendor([]string{"sh", "-c", "exit 0"})("acct")

	// Assert
	if err == nil {
		_ = proc.Close()
		t.Fatalf("SpawnVendor: got nil error, want a refusal naming %s", vendorguard.EnvVar)
	}
	if !strings.Contains(err.Error(), vendorguard.EnvVar) {
		t.Fatalf("SpawnVendor error = %v, want one naming %s", err, vendorguard.EnvVar)
	}
}

func TestSpawnVendor_SpawnsWhenVendorCallsArePermitted(t *testing.T) {
	// Arrange
	t.Setenv(vendorguard.EnvVar, "")

	// Act
	proc, err := SpawnVendor([]string{"sh", "-c", "exit 0"})("acct")

	// Assert
	if err != nil {
		t.Fatalf("SpawnVendor: %v", err)
	}
	_ = proc.Close()
}
