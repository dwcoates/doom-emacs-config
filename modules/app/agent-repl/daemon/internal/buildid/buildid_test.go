package buildid

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// vectorFile is the cross-language vector Emacs's ERT suite asserts too.
const vectorFile = "../../../proto/vocab/elisp-build.json"

type vector struct {
	Cases []struct {
		Name    string `json:"name"`
		Modules []struct {
			Name    string  `json:"name"`
			Content *string `json:"content"`
		} `json:"modules"`
		Want string `json:"want"`
	} `json:"cases"`
}

func readVector(t *testing.T) vector {
	t.Helper()
	raw, err := os.ReadFile(vectorFile)
	if err != nil {
		t.Fatalf("read %s: %v", vectorFile, err)
	}
	var v vector
	if err := json.Unmarshal(raw, &v); err != nil {
		t.Fatalf("parse %s: %v", vectorFile, err)
	}
	if len(v.Cases) == 0 {
		t.Fatalf("%s holds no cases", vectorFile)
	}
	return v
}

// writeModuleRoot lays a checkout out on disk: config.el naming the modules
// in order, and lisp/<module>.el for each module with content.
func writeModuleRoot(t *testing.T, names []string, contents map[string]string) string {
	t.Helper()
	root := t.TempDir()
	var config strings.Builder
	config.WriteString(";;; config.el\n")
	for _, name := range names {
		config.WriteString(`(agent-repl--load-module "` + name + `")` + "\n")
	}
	if err := os.WriteFile(filepath.Join(root, "config.el"), []byte(config.String()), 0o644); err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll(filepath.Join(root, "lisp"), 0o755); err != nil {
		t.Fatal(err)
	}
	for name, content := range contents {
		if err := os.WriteFile(filepath.Join(root, "lisp", name+".el"), []byte(content), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	return root
}

func TestElispReproducesTheCrossLanguageVector(t *testing.T) {
	for _, tc := range readVector(t).Cases {
		t.Run(tc.Name, func(t *testing.T) {
			// Arrange
			names := make([]string, 0, len(tc.Modules))
			contents := map[string]string{}
			for _, m := range tc.Modules {
				names = append(names, m.Name)
				if m.Content != nil {
					contents[m.Name] = *m.Content
				}
			}
			if len(names) == 0 {
				// A loader naming nothing is refused by Elisp, so the empty
				// case is held to the pure hash alone.
				if got := ElispOf(nil); got != tc.Want {
					t.Fatalf("ElispOf(nothing) = %s, want %s", got, tc.Want)
				}
				return
			}
			root := writeModuleRoot(t, names, contents)

			// Act
			got, err := Elisp(root)

			// Assert
			if err != nil {
				t.Fatalf("Elisp: %v", err)
			}
			if got != tc.Want {
				t.Fatalf("Elisp = %s, want %s", got, tc.Want)
			}
		})
	}
}

func TestElispModules(t *testing.T) {
	tests := []struct {
		name    string
		config  string
		want    []string
		wantErr string
	}{
		{
			name:   "top-level loader forms in order",
			config: "(agent-repl--load-module \"core\")\n;; a comment\n(agent-repl--load-module \"status\")\n",
			want:   []string{"core", "status"},
		},
		{
			name:   "a form not at the start of a line is not a load",
			config: "(agent-repl--load-module \"core\")\n  (agent-repl--load-module \"nested\")\n",
			want:   []string{"core"},
		},
		{
			name:    "a loader naming nothing is refused",
			config:  ";;; config.el\n",
			wantErr: "loads no module",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			root := t.TempDir()
			if err := os.WriteFile(filepath.Join(root, "config.el"), []byte(tc.config), 0o644); err != nil {
				t.Fatal(err)
			}

			// Act
			got, err := ElispModules(root)

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("ElispModules error = %v, want %q", err, tc.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatalf("ElispModules: %v", err)
			}
			if strings.Join(got, ",") != strings.Join(tc.want, ",") {
				t.Fatalf("ElispModules = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestElispRefusesAMissingLoader(t *testing.T) {
	// Arrange
	root := t.TempDir()

	// Act
	_, err := Elisp(root)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "config.el") {
		t.Fatalf("Elisp error = %v, want one naming config.el", err)
	}
}

func TestWebapp(t *testing.T) {
	tests := []struct {
		name    string
		index   *string
		want    string
		wantErr string
	}{
		{
			name:  "the entry bundle's hash",
			index: ptr(`<script type="module" crossorigin src="/assets/index-AbC_12-x.js"></script>`),
			want:  "AbC_12-x",
		},
		{
			name:    "an index naming no entry bundle",
			index:   ptr(`<script src="/assets/vendor-1.js"></script>`),
			wantErr: "names no assets/index-<hash>.js",
		},
		{
			name:    "no index at all",
			wantErr: "read the webapp entry",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			dist := t.TempDir()
			if tc.index != nil {
				if err := os.WriteFile(filepath.Join(dist, "index.html"), []byte(*tc.index), 0o644); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			got, err := Webapp(dist)

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("Webapp error = %v, want %q", err, tc.wantErr)
				}
				return
			}
			if err != nil || got != tc.want {
				t.Fatalf("Webapp = %q, %v; want %q", got, err, tc.want)
			}
		})
	}
}

func TestShimBundleBuild(t *testing.T) {
	tests := []struct {
		name     string
		bundle   *string
		override string
		want     string
		wantErr  string
	}{
		{name: "the bundle's hash", bundle: ptr("abc"), want: "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"},
		{name: "the bundle wins over the override", bundle: ptr("abc"), override: "fake", want: "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"},
		{name: "no bundle falls to the override", override: "fake", want: "fake"},
		{name: "neither is unresolvable", wantErr: "unresolvable"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			path := filepath.Join(t.TempDir(), "main.js")
			if tc.bundle != nil {
				if err := os.WriteFile(path, []byte(*tc.bundle), 0o644); err != nil {
					t.Fatal(err)
				}
			}
			b := NewShimBundle(path, tc.override)

			// Act
			got, err := b.Build()

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("Build error = %v, want %q", err, tc.wantErr)
				}
				return
			}
			if err != nil || got != tc.want {
				t.Fatalf("Build = %q, %v; want %q", got, err, tc.want)
			}
		})
	}
}

func TestAHeldBundleCannotBeReplaced(t *testing.T) {
	// Arrange
	b := NewShimBundle(filepath.Join(t.TempDir(), "main.js"), "fake")
	_, release, err := b.Hold()
	if err != nil {
		t.Fatalf("Hold: %v", err)
	}

	// Act
	exclusive := b.mu.TryLock()

	// Assert
	if exclusive {
		b.mu.Unlock()
		t.Fatalf("the bundle could be taken exclusively while a spawn held it")
	}
	release()
	if !b.mu.TryLock() {
		t.Fatalf("the bundle stayed held after its release")
	}
	b.mu.Unlock()
}

func TestAFailedHoldHoldsNothing(t *testing.T) {
	// Arrange
	b := NewShimBundle(filepath.Join(t.TempDir(), "main.js"), "")

	// Act
	_, release, err := b.Hold()
	release()

	// Assert
	if err == nil {
		t.Fatalf("Hold succeeded with no bundle and no override")
	}
	if !b.mu.TryLock() {
		t.Fatalf("a failed hold left the bundle held")
	}
	b.mu.Unlock()
}

func TestReplaceRunsTheInstall(t *testing.T) {
	// Arrange
	b := NewShimBundle(filepath.Join(t.TempDir(), "main.js"), "")
	ran := false

	// Act
	err := b.Replace(func() error { ran = true; return nil })

	// Assert
	if err != nil || !ran {
		t.Fatalf("Replace = %v, ran %v; want the install run", err, ran)
	}
}

func ptr(s string) *string { return &s }
