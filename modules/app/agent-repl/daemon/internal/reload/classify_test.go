package reload

import (
	"slices"
	"testing"
)

func TestClassifyBucketsEachComponentsPaths(t *testing.T) {
	tests := []struct {
		name string
		path string
		want Component
	}{
		{"daemon go", "modules/app/agent-repl/daemon/internal/server/server.go", ComponentDaemon},
		{"shim typescript", "modules/app/agent-repl/agent-shim/claude/shim/src/main.ts", ComponentShim},
		{"sidecar go", "modules/app/agent-repl/agent-shim/claude/shim-sidecar/main.go", ComponentSidecar},
		{"store go", "modules/app/agent-repl/agent-shim/shim-store/store.go", ComponentStore},
		{"webapp source", "modules/app/agent-repl/webapp/src/App.tsx", ComponentWebapp},
		{"proto contract", "modules/app/agent-repl/proto/agentshim/frontend/v1/frontend.proto", ComponentProto},
		{"elisp module", "modules/app/agent-repl/panels.el", ComponentElisp},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			paths := []string{tc.path}

			// Act.
			got := Classify(paths)

			// Assert.
			if !slices.Equal(got, []Component{tc.want}) {
				t.Fatalf("Classify(%q) = %v, want [%s]", tc.path, got, tc.want)
			}
		})
	}
}

// The batch-only ERT harness is never loaded into the live Emacs, so a merge
// that only touched it has nothing for the running stack to pick up.
func TestClassifyExcludesTheBatchOnlyTestHarness(t *testing.T) {
	// Arrange.
	paths := []string{"modules/app/agent-repl/test-panels.el"}

	// Act.
	got := Classify(paths)

	// Assert.
	if len(got) != 0 {
		t.Fatalf("Classify(%v) = %v, want no components", paths, got)
	}
}

func TestClassifyIgnoresPathsOutsideTheStack(t *testing.T) {
	// Arrange — the host repository is a whole Emacs configuration; almost all
	// of it is not the agent-repl stack.
	paths := []string{"config.el", "modules/lang/personal-org/config.el", "README.md"}

	// Act.
	got := Classify(paths)

	// Assert.
	if len(got) != 0 {
		t.Fatalf("Classify(%v) = %v, want no components", paths, got)
	}
}

func TestClassifyReportsAMixedChangeInDeployOrder(t *testing.T) {
	// Arrange — one path per component, deliberately shuffled relative to the
	// order the deploy runs them in.
	paths := []string{
		"modules/app/agent-repl/status.el",
		"modules/app/agent-repl/webapp/src/App.tsx",
		"modules/app/agent-repl/daemon/internal/reload/reload.go",
		"modules/app/agent-repl/proto/agentshim/frontend/v1/frontend.proto",
		"modules/app/agent-repl/agent-shim/claude/shim/src/main.ts",
		"modules/app/agent-repl/agent-shim/shim-store/store.go",
		"modules/app/agent-repl/agent-shim/claude/shim-sidecar/main.go",
	}
	want := []Component{
		ComponentProto, ComponentShim, ComponentDaemon,
		ComponentStore, ComponentSidecar, ComponentWebapp, ComponentElisp,
	}

	// Act.
	got := Classify(paths)

	// Assert.
	if !slices.Equal(got, want) {
		t.Fatalf("Classify(mixed) = %v, want %v", got, want)
	}
}

func TestClassifyDeduplicatesRepeatedComponentPaths(t *testing.T) {
	// Arrange — a realistic daemon change touches many files in one component.
	paths := []string{
		"modules/app/agent-repl/daemon/internal/server/server.go",
		"modules/app/agent-repl/daemon/internal/server/routes.go",
		"modules/app/agent-repl/daemon/go.mod",
	}

	// Act.
	got := Classify(paths)

	// Assert.
	if !slices.Equal(got, []Component{ComponentDaemon}) {
		t.Fatalf("Classify(%v) = %v, want [daemon]", paths, got)
	}
}

// The shim and the sidecar are siblings whose directory names share a prefix,
// so a prefix rule without its trailing slash would swallow the sidecar.
func TestClassifySeparatesTheSidecarFromTheShimBundle(t *testing.T) {
	// Arrange.
	paths := []string{"modules/app/agent-repl/agent-shim/claude/shim-sidecar/link.go"}

	// Act.
	got := Classify(paths)

	// Assert.
	if !slices.Equal(got, []Component{ComponentSidecar}) {
		t.Fatalf("Classify(%v) = %v, want [shim-claude-sidecar]", paths, got)
	}
}

func TestNeedsElispReloadReportsTheElispComponent(t *testing.T) {
	tests := []struct {
		name       string
		components []Component
		want       bool
	}{
		{"elisp present", []Component{ComponentDaemon, ComponentElisp}, true},
		{"elisp absent", []Component{ComponentDaemon, ComponentWebapp}, false},
		{"nothing classified", nil, false},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act.
			got := NeedsElispReload(tc.components)

			// Assert.
			if got != tc.want {
				t.Fatalf("NeedsElispReload(%v) = %t, want %t", tc.components, got, tc.want)
			}
		})
	}
}
