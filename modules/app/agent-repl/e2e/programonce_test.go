package e2e

import (
	"errors"
	"fmt"
	"testing"

	"agentrepl/e2e/kit/stack"
	"agentrepl/testrun/testenv"
)

// fakeFatal records require's failure instead of ending the test.
type fakeFatal struct{ failed string }

func (f *fakeFatal) Helper() {}
func (f *fakeFatal) Fatalf(format string, args ...any) {
	f.failed = fmt.Sprintf(format, args...)
}

func TestProgramOnce(t *testing.T) {
	tests := []struct {
		name       string
		buildErr   error
		calls      int
		wantPath   string
		wantBuilds int
		wantFailed string
	}{
		{name: "the first call builds and answers the path", calls: 1, wantPath: "/bin/prog", wantBuilds: 1},
		{name: "a second call answers the same path without building again", calls: 2, wantPath: "/bin/prog", wantBuilds: 1},
		{name: "a program that does not build fails every caller with why and the error",
			buildErr: errors.New("compile error"), calls: 2, wantBuilds: 1,
			wantFailed: "e2e: the suite needs prog: compile error"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			t.Setenv(testenv.Programs, "")
			t.Setenv(testenv.ProgramsInstrumented, "")
			builds := 0
			p := &stack.ProgramOnce{Name: "prog", Build: func() (string, error) {
				builds++
				if tt.buildErr != nil {
					return "", tt.buildErr
				}
				return "/bin/prog", nil
			}}

			// Act
			var path string
			fake := &fakeFatal{}
			for range tt.calls {
				fake.failed = ""
				path = p.Require(fake, "the suite needs prog")
			}

			// Assert
			if builds != tt.wantBuilds || fake.failed != tt.wantFailed {
				t.Fatalf("builds = %d, failed = %q; want %d, %q", builds, fake.failed, tt.wantBuilds, tt.wantFailed)
			}
			if tt.wantFailed == "" && path != tt.wantPath {
				t.Fatalf("path = %q, want %q", path, tt.wantPath)
			}
		})
	}
}
