package testenv

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
)

func TestProfileName(t *testing.T) {
	tests := []struct {
		name, in, want string
	}{
		{name: "a plain name is kept", in: "TestX", want: "TestX"},
		{name: "a unit id's separators become one underscore each run", in: "daemon:internal/merge:TestQueue#0", want: "daemon_internal_merge_TestQueue_0"},
		{name: "a subtest path's spaces and slashes collapse", in: "TestA/one two//three", want: "TestA_one_two_three"},
		{name: "dots and dashes stay", in: "shim[integration]/a-b.test.ts", want: "shim_integration_a-b.test.ts"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got := ProfileName(tt.in)

			// Assert.
			if got != tt.want {
				t.Fatalf("ProfileName(%q) = %q, want %q", tt.in, got, tt.want)
			}
		})
	}
}

func TestProfileNameRefusesNothing(t *testing.T) {
	// Arrange.
	defer func() {
		// Assert.
		if r := recover(); r == nil {
			t.Fatal("ProfileName(\"\") returned; an empty name is a caller's defect and must panic")
		}
	}()

	// Act.
	ProfileName("")
}

func TestProfileDirFor(t *testing.T) {
	root := t.TempDir()
	blocked := filepath.Join(t.TempDir(), "a-file")
	if err := os.WriteFile(blocked, nil, 0o644); err != nil {
		t.Fatal(err)
	}
	tests := []struct {
		name    string
		env     string
		want    string
		wantErr string
	}{
		{name: "outside a profiling run there is no directory", env: "", want: ""},
		{name: "inside one the test gets its own created subdirectory", env: root, want: filepath.Join(root, "TestA_case")},
		{name: "a relative root is refused", env: "rel/dir", wantErr: "is not absolute"},
		{name: "a root that cannot hold a directory fails", env: blocked, wantErr: "create the profiles directory of TestA/case"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act.
			got, err := ProfileDirFor(func(k string) string {
				if k == ProfileDir {
					return tt.env
				}
				return ""
			}, "TestA/case")

			// Assert.
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want one containing %q", err, tt.wantErr)
				}
				return
			}
			if err != nil {
				t.Fatal(err)
			}
			if got != tt.want {
				t.Fatalf("dir = %q, want %q", got, tt.want)
			}
			if got != "" {
				if info, err := os.Stat(got); err != nil || !info.IsDir() {
					t.Fatalf("the directory %s was not created: %v", got, err)
				}
			}
		})
	}
}

func TestExitProfileEnvIsTheSharedContracts(t *testing.T) {
	// Arrange
	raw, err := os.ReadFile("../../proto/vocab/profile-session.json")
	if err != nil {
		t.Fatal(err)
	}
	var vocab struct {
		ExitProfileEnv string `json:"exit_profile_env"`
	}
	if err := json.Unmarshal(raw, &vocab); err != nil {
		t.Fatal(err)
	}

	// Act
	got := ExitProfileEnv

	// Assert
	if got != vocab.ExitProfileEnv {
		t.Fatalf("ExitProfileEnv = %q, the vocabulary says %q", got, vocab.ExitProfileEnv)
	}
}

func TestExitProfileEnvFor(t *testing.T) {
	root := t.TempDir()
	blocked := filepath.Join(t.TempDir(), "a-file")
	if err := os.WriteFile(blocked, nil, 0o644); err != nil {
		t.Fatal(err)
	}
	tests := []struct {
		name    string
		env     string
		wantEnv []string
		wantDir string
		wantErr string
	}{
		{name: "outside a profiling run the processes are not told", env: ""},
		{name: "inside one they profile into the test's directory", env: root,
			wantEnv: []string{ExitProfileEnv + "=" + filepath.Join(root, "TestA")}, wantDir: filepath.Join(root, "TestA")},
		{name: "a directory that cannot be made fails", env: blocked, wantErr: "create the profiles directory of TestA"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			env, dir, err := ExitProfileEnvFor(func(k string) string {
				if k == ProfileDir {
					return tt.env
				}
				return ""
			}, "TestA")

			// Assert
			if tt.wantErr != "" {
				if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
					t.Fatalf("err = %v, want one containing %q", err, tt.wantErr)
				}
				return
			}
			if err != nil || !slices.Equal(env, tt.wantEnv) || dir != tt.wantDir {
				t.Fatalf("ExitProfileEnvFor = %q, %q, %v; want %q, %q", env, dir, err, tt.wantEnv, tt.wantDir)
			}
		})
	}
}

func TestExitProfileReadsTheProcesssEnvironment(t *testing.T) {
	// Arrange
	root := t.TempDir()
	t.Setenv(ProfileDir, root)

	// Act
	env, dir := ExitProfile(t)

	// Assert
	want := filepath.Join(root, ProfileName(t.Name()))
	if dir != want || !slices.Equal(env, []string{ExitProfileEnv + "=" + want}) {
		t.Fatalf("ExitProfile = %q, %q; want the test's own directory %s", env, dir, want)
	}
}

// fatalTB records a Fatalf instead of ending the test.
type fatalTB struct {
	testing.TB
	name, fatal string
}

func (f *fatalTB) Helper()      {}
func (f *fatalTB) Name() string { return f.name }
func (f *fatalTB) Fatalf(format string, args ...any) {
	f.fatal = fmt.Sprintf(format, args...)
}

func TestExitProfileFailsTheTestWhoseDirectoryCannotBeMade(t *testing.T) {
	// Arrange
	blocked := filepath.Join(t.TempDir(), "a-file")
	if err := os.WriteFile(blocked, nil, 0o644); err != nil {
		t.Fatal(err)
	}
	t.Setenv(ProfileDir, blocked)
	tb := &fatalTB{name: "TestB"}

	// Act
	ExitProfile(tb)

	// Assert
	if !strings.Contains(tb.fatal, "create the profiles directory of TestB") {
		t.Fatalf("Fatalf = %q, want the directory's failure", tb.fatal)
	}
}
