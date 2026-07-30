package sessioncontroller

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

func writeMetaprompt(t *testing.T, cwd string) string {
	t.Helper()
	dir := filepath.Join(cwd, "modules", "app", "agent-repl")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	path := filepath.Join(dir, "metaprompt.md")
	if err := os.WriteFile(path, []byte("guidelines"), 0o644); err != nil {
		t.Fatalf("write metaprompt: %v", err)
	}
	return path
}

func TestMetapromptDirectivePresent(t *testing.T) {
	// Arrange.
	cwd := t.TempDir()
	want := writeMetaprompt(t, cwd)

	// Act.
	got, ok := metapromptDirective(cwd)

	// Assert.
	if !ok {
		t.Fatal("expected ok when metaprompt.md exists")
	}
	if !strings.Contains(got, want) {
		t.Errorf("directive should name the metaprompt path %q; got %q", want, got)
	}
}

func TestMetapromptDirectiveAbsent(t *testing.T) {
	// Arrange: an empty cwd with no metaprompt file.
	cwd := t.TempDir()

	// Act.
	_, ok := metapromptDirective(cwd)

	// Assert.
	if ok {
		t.Fatal("expected not-ok when metaprompt.md is absent")
	}
}

func TestMetapromptDirectiveEmptyCwd(t *testing.T) {
	if _, ok := metapromptDirective(""); ok {
		t.Fatal("expected not-ok for an empty cwd")
	}
}

func TestWantsMetapromptRefire(t *testing.T) {
	tests := []struct {
		name string
		src  corev1.SessionSource
		want bool
	}{
		{"resume", corev1.SessionSource_SESSION_SOURCE_RESUME, true},
		{"compact_continue", corev1.SessionSource_SESSION_SOURCE_COMPACT_CONTINUE, true},
		{"fresh", corev1.SessionSource_SESSION_SOURCE_FRESH, false},
		{"unspecified", corev1.SessionSource_SESSION_SOURCE_UNSPECIFIED, false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := wantsMetapromptRefire(&corev1.SessionStarted{Source: tt.src}); got != tt.want {
				t.Errorf("wantsMetapromptRefire(%s) = %v, want %v", tt.src, got, tt.want)
			}
		})
	}
}

func TestWantsMetapromptRefireNil(t *testing.T) {
	if wantsMetapromptRefire(nil) {
		t.Fatal("nil SessionStarted must not arm the re-fire")
	}
}

func TestPrependMetaprompt(t *testing.T) {
	got := prependMetaprompt("READ THE FILE", "do the thing")
	want := "READ THE FILE\n\ndo the thing"
	if got != want {
		t.Errorf("prependMetaprompt = %q, want %q", got, want)
	}
}
