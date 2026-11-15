package account_test

import (
	"strings"
	"testing"

	"agent-repld/internal/account"
	"agent-repld/internal/dlog"
)

func TestNewRejectsMissingRoots(t *testing.T) {
	tests := []struct {
		name  string
		roots account.Roots
	}{
		{name: "no default root", roots: account.Roots{MultiRepo: "/m"}},
		{name: "no multi-repo root", roots: account.Roots{Default: "/d"}},
		{name: "neither root", roots: account.Roots{}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			log := dlog.NewTestLogger()

			// Act.
			_, err := account.New(tc.roots, log)

			// Assert.
			if err == nil {
				t.Fatal("New() = nil error, want a refusal")
			}
		})
	}
}

func TestRootsValidate(t *testing.T) {
	tests := []struct {
		name  string
		roots account.Roots
		want  string
	}{
		{name: "both roots named", roots: account.Roots{Default: "/d", MultiRepo: "/m"}},
		{name: "both roots and a multi-repo prefix", roots: account.Roots{Default: "/d", MultiRepo: "/m", MultiRepoRoot: "/w"}},
		{name: "no default root", roots: account.Roots{MultiRepo: "/m"}, want: "Roots.Default is required"},
		{name: "no multi-repo root", roots: account.Roots{Default: "/d"}, want: "Roots.MultiRepo is required"},
		{name: "neither root names the default first", roots: account.Roots{}, want: "Roots.Default is required"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: the case's roots.

			// Act.
			err := tc.roots.Validate()

			// Assert.
			if tc.want == "" {
				if err != nil {
					t.Fatalf("Validate() = %v, want nil", err)
				}
				return
			}
			if err == nil || !strings.Contains(err.Error(), tc.want) {
				t.Fatalf("Validate() = %v, want an error naming %q", err, tc.want)
			}
		})
	}
}

func TestNewRejectsMissingLogger(t *testing.T) {
	// Arrange.
	roots := account.Roots{Default: "/d", MultiRepo: "/m"}

	// Act.
	_, err := account.New(roots, nil)

	// Assert.
	if err == nil {
		t.Fatal("New() = nil error, want a refusal for the missing logger")
	}
}

func TestNotFoundErrorNamesEveryProbedPath(t *testing.T) {
	// Arrange.
	err := &account.NotFoundError{
		VendorSessionID: "uuid-1",
		WorkspaceDir:    "/ws",
		Probed:          []string{"/d/projects/-ws/uuid-1.jsonl", "/m/projects/-ws/uuid-1.jsonl"},
	}

	// Act.
	got := err.Error()

	// Assert.
	for _, want := range []string{"uuid-1", "/d/projects/-ws/uuid-1.jsonl", "/m/projects/-ws/uuid-1.jsonl"} {
		if !strings.Contains(got, want) {
			t.Fatalf("Error() = %q, want it to name %q", got, want)
		}
	}
}
