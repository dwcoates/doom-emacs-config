package merge

import (
	"testing"

	"claude-repld/internal/prompts"
)

// usePrompts points the prompt loader at this checkout's real prompts/
// directory for the duration of one test.
//
// A `go test` binary lives in the build cache, so prompts.Dir's
// executable walk-up has no checkout above it to find. Resolving from the
// package source and going through the ordinary DirEnv override means the
// tests exercise the same resolution path a user's own override would.
func usePrompts(t *testing.T) {
	t.Helper()
	dir, err := prompts.SourceDir()
	if err != nil {
		t.Fatalf("resolve the checkout's prompts directory: %v", err)
	}
	t.Setenv(prompts.DirEnv, dir)
}

// mustPrompt returns a checker bound to t, used as
// `mustPrompt(t)(res.Prompt())`. It is curried because Go only forwards a
// multi-value call into a function whose parameters are exactly those values,
// and binding t up front is what buys the one-line call site.
//
// It fails the test when a prompt could not be composed, so a broken prompt
// file surfaces as a named failure rather than an empty string quietly
// satisfying a Contains assertion.
func mustPrompt(t *testing.T) func(string, error) string {
	t.Helper()
	return func(text string, err error) string {
		t.Helper()
		if err != nil {
			t.Fatalf("compose prompt: %v", err)
		}
		return text
	}
}
