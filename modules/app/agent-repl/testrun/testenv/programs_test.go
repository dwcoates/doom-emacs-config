package testenv

import (
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
		wantOK   bool
		wantErr  string
	}{
		{name: "outside a run the process builds it itself", env: "", program: "shim-store", wantOK: false},
		{name: "a run's program is read from its directory", env: dir, program: "shim-store", wantOK: true},
		{name: "a run that holds no such program is a defect", env: dir, program: "agent-repld", wantErr: "holds no program agent-repld"},
		{name: "a directory is not a program", env: dir, program: "a-dir", wantErr: "is a directory"},
		{name: "a coverage run refuses uninstrumented programs", env: dir, program: "shim-store", coverage: true, wantErr: "cannot serve a coverage run"},
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
