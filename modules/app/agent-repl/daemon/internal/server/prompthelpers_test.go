package server

import (
	"testing"

	"claude-repld/internal/prompts"
)

// usePrompts points the prompt loader at this checkout's real prompts/
// directory for the duration of one test.
//
// A `go test` binary lives in the build cache, so prompts.Dir's executable
// walk-up has no checkout above it to find. Going through the ordinary
// DirEnv override means the tests exercise the same resolution path a
// user's own override would.
func usePrompts(t *testing.T) {
	t.Helper()
	dir, err := prompts.SourceDir()
	if err != nil {
		t.Fatalf("resolve the checkout's prompts directory: %v", err)
	}
	t.Setenv(prompts.DirEnv, dir)
}
