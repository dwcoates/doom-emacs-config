package testenv

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestProgram(t *testing.T) {
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, "shim-store"), []byte("#!/bin/sh\n"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.Mkdir(filepath.Join(dir, "a-dir"), 0o755); err != nil {
		t.Fatal(err)
	}
	tests := []struct {
		name     string
		env      string
		program  string
		coverage bool
		// instrumented marks the directory as cover-built.
		instrumented bool
		wantOK       bool
		wantErr      string
	}{
		{name: "outside a run the process builds it itself", env: "", program: "shim-store", wantOK: false},
		{name: "a run's program is read from its directory", env: dir, program: "shim-store", wantOK: true},
		{name: "a run that holds no such program is a defect", env: dir, program: "agent-repld", wantErr: "holds no program agent-repld"},
		{name: "a directory is not a program", env: dir, program: "a-dir", wantErr: "is a directory"},
		{name: "a coverage run refuses uninstrumented programs", env: dir, program: "shim-store", coverage: true, wantErr: "cannot serve a coverage run"},
		{name: "a coverage run reads an instrumented programs directory", env: dir, program: "shim-store", coverage: true, instrumented: true, wantOK: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got, ok, err := Program(func(k string) string {
				switch {
				case k == Programs:
					return tt.env
				case k == Coverage && tt.coverage:
					return "/cov"
				case k == ProgramsInstrumented && tt.instrumented:
					return "1"
				}
				return ""
			}, tt.program)

			// Assert.
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("Program error = %v, want one containing %q", err, tt.wantErr)
				}
				return
			}
			if err != nil || ok != tt.wantOK {
				t.Fatalf("Program = %q, %v, %v; want ok %v", got, ok, err, tt.wantOK)
			}
			if ok && filepath.Base(got) != tt.program {
				t.Fatalf("Program path = %q, want the program %s", got, tt.program)
			}
		})
	}
}

func TestProgramOr(t *testing.T) {
	dir := t.TempDir()
	if err := os.WriteFile(filepath.Join(dir, "shim-store"), []byte("#!/bin/sh\n"), 0o755); err != nil {
		t.Fatal(err)
	}
	cases := []struct {
		name      string
		env       string
		buildErr  error
		wantBuilt bool
		wantPath  string
		wantErr   string
	}{
		{name: "outside a run the program is built here", env: "", wantBuilt: true, wantPath: "/built/shim-store"},
		{name: "a build that fails is answered", env: "", buildErr: errors.New("no compiler"), wantBuilt: true, wantErr: "no compiler"},
		{name: "a run's program is used and nothing is built", env: dir, wantPath: "shim-store"},
		{name: "a run without the program is refused before any build", env: t.TempDir(), wantErr: "holds no program shim-store"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			getenv := func(k string) string {
				if k == Programs {
					return tc.env
				}
				return ""
			}
			built := false
			build := func() (string, error) {
				built = true
				if tc.buildErr != nil {
					return "", tc.buildErr
				}
				return "/built/shim-store", nil
			}

			// Act
			got, err := ProgramOr(getenv, "shim-store", build)

			// Assert
			if built != tc.wantBuilt {
				t.Fatalf("built = %v, want %v", built, tc.wantBuilt)
			}
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("ProgramOr error = %v, want one containing %q", err, tc.wantErr)
				}
				return
			}
			if err != nil || !strings.HasSuffix(got, tc.wantPath) {
				t.Fatalf("ProgramOr = %q, %v; want a path ending %q", got, err, tc.wantPath)
			}
		})
	}
}

// TestEverySuiteComesByAProgramThroughProgramOr pins that no suite reads a
// run's program with Program and builds its own beside it by hand: every one
// goes through ProgramOr.
func TestEverySuiteComesByAProgramThroughProgramOr(t *testing.T) {
	// Arrange
	module := filepath.Join("..", "..")
	var offenders []string

	// Act
	err := filepath.WalkDir(module, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() && (d.Name() == "node_modules" || d.Name() == "testenv") {
			return filepath.SkipDir
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") {
			return nil
		}
		src, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(src), "testenv."+"Program(") {
			offenders = append(offenders, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walk the module: %v", err)
	}
	if len(offenders) > 0 {
		t.Fatalf("%v read a run's program with testenv.Program instead of testenv.ProgramOr", offenders)
	}
}
