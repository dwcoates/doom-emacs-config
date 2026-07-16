package stateroot

import (
	"os"
	"path/filepath"
	"testing"
)

func TestRoot(t *testing.T) {
	tests := []struct {
		name   string
		env    string
		setEnv bool
		want   func(t *testing.T) string
	}{
		{
			name:   "env var set wins over home",
			env:    "/tmp/explicit-root",
			setEnv: true,
			want:   func(*testing.T) string { return "/tmp/explicit-root" },
		},
		{
			name:   "env var unset falls back to home",
			setEnv: false,
			want: func(t *testing.T) string {
				home, err := os.UserHomeDir()
				if err != nil {
					t.Fatalf("UserHomeDir: %v", err)
				}
				return filepath.Join(home, DefaultDirName)
			},
		},
		{
			name:   "env var set but empty falls back to home",
			env:    "",
			setEnv: true,
			want: func(t *testing.T) string {
				home, err := os.UserHomeDir()
				if err != nil {
					t.Fatalf("UserHomeDir: %v", err)
				}
				return filepath.Join(home, DefaultDirName)
			},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			if tc.setEnv {
				t.Setenv(EnvVar, tc.env)
			} else {
				os.Unsetenv(EnvVar)
			}
			want := tc.want(t)

			// Act.
			got, err := Root()

			// Assert.
			if err != nil {
				t.Fatalf("Root() error = %v, want nil", err)
			}
			if got != want {
				t.Errorf("Root() = %q, want %q", got, want)
			}
		})
	}
}
