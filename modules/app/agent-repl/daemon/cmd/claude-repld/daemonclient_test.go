package main

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"claude-repld/internal/stateroot"
)

func TestServingAddressAnswersTheAdvertisedAddress(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, "daemon.addr"), []byte("127.0.0.1:4242"), 0o644); err != nil {
		t.Fatalf("write daemon.addr: %v", err)
	}
	layout, err := stateroot.Root(dir, dir)
	if err != nil {
		t.Fatalf("stateroot.Root: %v", err)
	}

	// Act.
	address, err := servingAddress(layout)

	// Assert.
	if err != nil || address != "127.0.0.1:4242" {
		t.Fatalf("servingAddress = %q, %v; want the advertised address", address, err)
	}
}

func TestServingAddressWithNoAdvertisementIsNoDaemonServing(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	layout, err := stateroot.Root(dir, dir)
	if err != nil {
		t.Fatalf("stateroot.Root: %v", err)
	}

	// Act.
	_, err = servingAddress(layout)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "no daemon is serving: read") {
		t.Fatalf("servingAddress error = %v, want no daemon serving", err)
	}
}

func TestServingAddressWithAnEmptyAdvertisementIsNoDaemonServing(t *testing.T) {
	// Arrange.
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, "daemon.addr"), nil, 0o644); err != nil {
		t.Fatalf("write daemon.addr: %v", err)
	}
	layout, err := stateroot.Root(dir, dir)
	if err != nil {
		t.Fatalf("stateroot.Root: %v", err)
	}

	// Act.
	_, err = servingAddress(layout)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "names no address") {
		t.Fatalf("servingAddress error = %v, want an advertisement naming no address", err)
	}
}
