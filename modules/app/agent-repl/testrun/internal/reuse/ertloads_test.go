package reuse

import (
	"slices"
	"strings"
	"testing"
)

func TestERTModuleSourcesAreConfigItsModulesAndTheHelpers(t *testing.T) {
	// Arrange
	root := writeTree(t, map[string]string{
		"m/config.el":            `(agent-repl--load-module "core") (agent-repl--load-module "wire")`,
		"m/lisp/core.el":         "",
		"m/lisp/wire.el":         "",
		"m/lisp/unloaded.el":     "",
		"m/lisp/test-helpers.el": `(load (expand-file-name "../config.el")) (load "helper-data.el")`,
		"m/lisp/helper-data.el":  "",
	})

	// Act
	got, err := ERTModuleSources(root, "m/lisp")

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	want := []string{"m/config.el", "m/lisp/core.el", "m/lisp/helper-data.el", "m/lisp/test-helpers.el", "m/lisp/wire.el"}
	if !slices.Equal(got, want) {
		t.Fatalf("ERTModuleSources = %q, want %q", got, want)
	}
}

func TestERTModuleSourcesFailures(t *testing.T) {
	tests := []struct {
		name  string
		files map[string]string
		want  string
	}{
		{"no config.el", map[string]string{"m/lisp/test-helpers.el": ""}, "read m/config.el"},
		{"a module config.el loads that is missing", map[string]string{"m/config.el": `(agent-repl--load-module "gone")`, "m/lisp/test-helpers.el": ""}, "loads the module gone"},
		{"no helpers", map[string]string{"m/config.el": ""}, "read m/lisp/test-helpers.el"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			root := writeTree(t, tt.files)

			// Act
			_, err := ERTModuleSources(root, "m/lisp")

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("err = %v, want %q", err, tt.want)
			}
		})
	}
}

func TestERTFileLoadsNamesOnlyFilesThatExist(t *testing.T) {
	// Arrange
	root := writeTree(t, map[string]string{
		"m/lisp/test-a.el": `(load "a-data.el") (message "not-a-file.el") (load "a-data.el")`,
		"m/lisp/a-data.el": "",
	})

	// Act
	got, err := ERTFileLoads(root, "m/lisp", "test-a.el")

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if want := []string{"m/lisp/a-data.el", "m/lisp/test-a.el"}; !slices.Equal(got, want) {
		t.Fatalf("ERTFileLoads = %q, want %q", got, want)
	}
}
