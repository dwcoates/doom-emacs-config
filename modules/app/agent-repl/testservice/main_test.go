package main

import (
	"path/filepath"
	"testing"
)

func TestTheCacheDirFollowsXDGWhenItIsSet(t *testing.T) {
	tests := []struct {
		name string
		xdg  string
		want string
	}{
		{name: "the home's cache by default", want: filepath.Join("/home/u", ".cache", "agent-repl")},
		{name: "XDG's cache when set", xdg: "/xdg", want: filepath.Join("/xdg", "agent-repl")},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			t.Setenv("XDG_CACHE_HOME", tt.xdg)

			// Act
			got := cacheDir("/home/u")

			// Assert
			if got != tt.want {
				t.Fatalf("cacheDir = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestTheSocketEnvironmentOverridesTheDefault(t *testing.T) {
	tests := []struct {
		name string
		env  string
		want string
	}{
		{name: "the default when unset", want: "/default.sock"},
		{name: "the environment when set", env: "/private.sock", want: "/private.sock"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			t.Setenv("AGENT_REPL_TESTS_SOCKET", tt.env)

			// Act
			got := envOr("AGENT_REPL_TESTS_SOCKET", "/default.sock")

			// Assert
			if got != tt.want {
				t.Fatalf("envOr = %q, want %q", got, tt.want)
			}
		})
	}
}
