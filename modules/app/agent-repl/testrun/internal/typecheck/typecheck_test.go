package typecheck

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

type recordingLog struct{ infos, errs []string }

func (l *recordingLog) Infof(format string, args ...any) {
	l.infos = append(l.infos, fmt.Sprintf(format, args...))
}

func (l *recordingLog) Errorf(format string, args ...any) {
	l.errs = append(l.errs, fmt.Sprintf(format, args...))
}

func writeFile(t *testing.T, path, body string) {
	t.Helper()
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte(body), 0o644); err != nil {
		t.Fatal(err)
	}
}

func readFile(t *testing.T, path string) string {
	t.Helper()
	b, err := os.ReadFile(path)
	if err != nil {
		t.Fatal(err)
	}
	return string(b)
}

func TestSeedPath(t *testing.T) {
	tests := []struct {
		name, suite, want, wantErr string
	}{
		{name: "a suite name", suite: "webapp", want: "/s/webapp.tsbuildinfo"},
		{name: "a path is refused", suite: "../x", wantErr: "no suite name"},
		{name: "an empty name is refused", suite: "", wantErr: "no suite name"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := SeedPath("/s", tt.suite)

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("SeedPath = %q, %v; want %q", got, err, tt.wantErr)
				}
				return
			}
			if err != nil || got != tt.want {
				t.Fatalf("SeedPath = %q, %v; want %q", got, err, tt.want)
			}
		})
	}
}

func TestSeed(t *testing.T) {
	tests := []struct {
		name       string
		local      string
		seed       string
		wantSeeded bool
		wantLocal  string
	}{
		{name: "no state of its own takes the seed", seed: "seed", wantSeeded: true, wantLocal: "seed"},
		{name: "its own state is kept", local: "own", seed: "seed", wantLocal: "own"},
		{name: "no seed leaves it without state", wantLocal: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			pkg, seeds := t.TempDir(), t.TempDir()
			seed := filepath.Join(seeds, "p.tsbuildinfo")
			if tt.local != "" {
				writeFile(t, BuildInfoPath(pkg), tt.local)
			}
			if tt.seed != "" {
				writeFile(t, seed, tt.seed)
			}

			// Act
			seeded, err := Seed(pkg, seed)

			// Assert
			if err != nil || seeded != tt.wantSeeded {
				t.Fatalf("Seed = %v, %v; want %v", seeded, err, tt.wantSeeded)
			}
			got, err := os.ReadFile(BuildInfoPath(pkg))
			if tt.wantLocal == "" {
				if !errors.Is(err, os.ErrNotExist) {
					t.Fatalf("the package has state %q, want none", got)
				}
				return
			}
			if string(got) != tt.wantLocal {
				t.Fatalf("the package's state = %q, want %q", got, tt.wantLocal)
			}
		})
	}
}

func TestPublishReplacesTheSeedWhole(t *testing.T) {
	// Arrange
	pkg, seeds := t.TempDir(), t.TempDir()
	seed := filepath.Join(seeds, "nested", "p.tsbuildinfo")
	writeFile(t, seed, "an older, longer seed")
	writeFile(t, BuildInfoPath(pkg), "new")

	// Act
	err := Publish(pkg, seed)

	// Assert
	if err != nil || readFile(t, seed) != "new" {
		t.Fatalf("Publish = %v, seed %q; want the package's state", err, readFile(t, seed))
	}
	entries, _ := os.ReadDir(filepath.Dir(seed))
	if len(entries) != 1 {
		t.Fatalf("the seed directory holds %d entries, want no temporary file left", len(entries))
	}
}

func TestPublishFailsWithoutState(t *testing.T) {
	// Act
	err := Publish(t.TempDir(), filepath.Join(t.TempDir(), "p.tsbuildinfo"))

	// Assert
	if err == nil || !strings.Contains(err.Error(), "open") {
		t.Fatalf("Publish = %v, want the missing state named", err)
	}
}

func TestRun(t *testing.T) {
	tests := []struct {
		name        string
		seed        string
		typecheck   error
		wantErr     string
		wantSeed    string
		wantSeeded  bool
		wantPublish bool
	}{
		{name: "a passing typecheck seeded from the host publishes its state", seed: "old", wantSeed: "after", wantSeeded: true, wantPublish: true},
		{name: "a passing typecheck with no seed publishes its state", wantSeed: "after", wantPublish: true},
		{name: "a failing typecheck publishes nothing", seed: "old", typecheck: errors.New("exit status 2"), wantErr: "exit status 2", wantSeed: "old", wantSeeded: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			pkg, seeds := t.TempDir(), t.TempDir()
			seed := filepath.Join(seeds, "p.tsbuildinfo")
			if tt.seed != "" {
				writeFile(t, seed, tt.seed)
			}
			var sawSeed string
			typecheck := func() error {
				if b, err := os.ReadFile(BuildInfoPath(pkg)); err == nil {
					sawSeed = string(b)
				}
				writeFile(t, BuildInfoPath(pkg), "after")
				return tt.typecheck
			}
			log := &recordingLog{}

			// Act
			err := Run(pkg, "p", seeds, typecheck, log)

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("Run = %v, want %q", err, tt.wantErr)
				}
			} else if err != nil {
				t.Fatal(err)
			}
			if tt.wantSeeded && sawSeed != tt.seed {
				t.Fatalf("the typecheck started from %q, want the seed %q", sawSeed, tt.seed)
			}
			if got, _ := os.ReadFile(seed); string(got) != tt.wantSeed {
				t.Fatalf("seed = %q, want %q", got, tt.wantSeed)
			}
			published := strings.Contains(strings.Join(log.infos, "\n"), "published")
			if published != tt.wantPublish {
				t.Fatalf("log = %q, want published %v", log.infos, tt.wantPublish)
			}
		})
	}
}

func TestRunRefusesASuiteNameNoSeedCanBeKeptUnder(t *testing.T) {
	// Arrange
	ran := false

	// Act
	err := Run(t.TempDir(), "../p", t.TempDir(), func() error { ran = true; return nil }, &recordingLog{})

	// Assert
	if err == nil || ran {
		t.Fatalf("Run = %v (typecheck ran %v), want a refusal before the typecheck", err, ran)
	}
}

// TestEveryTypecheckKeepsItsStateWhereSeedReadsIt pins that every TypeScript
// package the roster typechecks runs the same incremental typecheck, its
// state at BuildInfoPath and its declarations under CacheDir, and that git
// ignores CacheDir there.
func TestEveryTypecheckKeepsItsStateWhereSeedReadsIt(t *testing.T) {
	want := "tsc -p tsconfig.json --noEmit false --declaration --emitDeclarationOnly --outDir " + CacheDir + "/declarations --incremental --tsBuildInfoFile " + CacheDir + "/" + BuildInfoFile
	for _, pkg := range []string{"webapp", "agent-shim/claude/shim"} {
		// Arrange
		dir := filepath.Join("..", "..", "..", pkg)
		var manifest struct {
			Scripts map[string]string `json:"scripts"`
		}

		// Act
		err := json.Unmarshal([]byte(readFile(t, filepath.Join(dir, "package.json"))), &manifest)

		// Assert
		if err != nil {
			t.Fatal(err)
		}
		if got := manifest.Scripts["typecheck"]; got != want {
			t.Errorf("%s's typecheck = %q, want %q", pkg, got, want)
		}
		if ignore := readFile(t, filepath.Join(dir, ".gitignore")); !strings.Contains("\n"+ignore, "\n"+CacheDir+"/\n") {
			t.Errorf("%s/.gitignore does not ignore %s/", pkg, CacheDir)
		}
	}
}

// TestTheWebappBuildStartsWithItsIncrementalTypecheck pins that the webapp's
// production build (bin/build-frontend.sh, under `testrun typecheck`) runs the
// same incremental typecheck as the gate, so the two share one seed.
func TestTheWebappBuildStartsWithItsIncrementalTypecheck(t *testing.T) {
	// Arrange
	var manifest struct {
		Scripts map[string]string `json:"scripts"`
	}

	// Act
	err := json.Unmarshal([]byte(readFile(t, filepath.Join("..", "..", "..", "webapp", "package.json"))), &manifest)

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if got := manifest.Scripts["build"]; !strings.HasPrefix(got, "npm run typecheck && ") {
		t.Fatalf("the webapp's build = %q, want it to start with its incremental typecheck", got)
	}
}
