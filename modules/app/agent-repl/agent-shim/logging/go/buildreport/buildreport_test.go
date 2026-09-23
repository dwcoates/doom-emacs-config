package buildreport

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestHashFile(t *testing.T) {
	tests := []struct {
		name    string
		content *string
		want    string
		wantErr string
	}{
		{
			name:    "the hash of the bytes",
			content: ptr("abc"),
			want:    "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad",
		},
		{
			name:    "an absent file is an error",
			content: nil,
			wantErr: "open",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			path := filepath.Join(t.TempDir(), "artifact")
			if tc.content != nil {
				if err := os.WriteFile(path, []byte(*tc.content), 0o644); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			got, err := HashFile(path)

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("HashFile error = %v, want one naming %q", err, tc.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatalf("HashFile: %v", err)
			}
			if got != tc.want {
				t.Fatalf("HashFile = %q, want %q", got, tc.want)
			}
		})
	}
}

func TestWriteThenRead(t *testing.T) {
	tests := []struct {
		name      string
		write     *Report
		raw       string
		wantFound bool
		wantErr   string
	}{
		{
			name:      "a written report reads back",
			write:     &Report{PID: 42, Build: "beef"},
			wantFound: true,
		},
		{
			name:      "no report is not found, and not an error",
			wantFound: false,
		},
		{
			name:    "an unparseable report is an error, never absent",
			raw:     "{not json",
			wantErr: "parse",
		},
		{
			name:    "a report naming no build is an error",
			raw:     `{"pid":7,"build":""}`,
			wantErr: "needs the build",
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			dir := t.TempDir()
			if tc.write != nil {
				if err := Write(dir, ServiceStore, *tc.write); err != nil {
					t.Fatalf("Write: %v", err)
				}
			}
			if tc.raw != "" {
				if err := os.WriteFile(Path(dir, ServiceStore), []byte(tc.raw), 0o644); err != nil {
					t.Fatal(err)
				}
			}

			// Act
			got, found, err := Read(dir, ServiceStore)

			// Assert
			if tc.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tc.wantErr) {
					t.Fatalf("Read error = %v, want one naming %q", err, tc.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatalf("Read: %v", err)
			}
			if found != tc.wantFound {
				t.Fatalf("Read found = %v, want %v", found, tc.wantFound)
			}
			if tc.write != nil && got != *tc.write {
				t.Fatalf("Read = %+v, want %+v", got, *tc.write)
			}
		})
	}
}

func TestWriteRefusesAnIncompleteReport(t *testing.T) {
	tests := []struct {
		name   string
		report Report
		want   string
	}{
		{name: "no pid", report: Report{Build: "beef"}, want: "pid"},
		{name: "no build", report: Report{PID: 1}, want: "build"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			dir := t.TempDir()

			// Act
			err := Write(dir, ServiceSidecar, tc.report)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("Write error = %v, want one naming %q", err, tc.want)
			}
			if _, statErr := os.Stat(Path(dir, ServiceSidecar)); !os.IsNotExist(statErr) {
				t.Fatalf("a refused report left a file behind: %v", statErr)
			}
		})
	}
}

func TestSelfReportsThisProcess(t *testing.T) {
	// Arrange
	exe, err := os.Executable()
	if err != nil {
		t.Fatal(err)
	}
	want, err := HashFile(exe)
	if err != nil {
		t.Fatal(err)
	}

	// Act
	got, err := Self()

	// Assert
	if err != nil {
		t.Fatalf("Self: %v", err)
	}
	if got.PID != os.Getpid() || got.Build != want {
		t.Fatalf("Self = %+v, want pid %d build %s", got, os.Getpid(), want)
	}
}

func ptr(s string) *string { return &s }
