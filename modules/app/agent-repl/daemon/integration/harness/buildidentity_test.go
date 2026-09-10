package harness

import (
	"os"
	"path/filepath"
	"testing"
)

// TestBuildIdentityEnvPinsOneString pins the invariant the whole suite depends
// on: the build the daemon exports to each shim and the build it reads as its
// DEPLOYED one are ONE string, and the checkout they are resolved against is
// the harness's own.
func TestBuildIdentityEnvPinsOneString(t *testing.T) {
	tests := []struct {
		name string
		key  string
		want string
	}{
		{name: "the checkout is the harness's own", key: "AGENT_REPL_CHECKOUT", want: "/pinned"},
		{name: "the exported shim build is the fake's", key: "SHIM_BUILD_SHA", want: FakeShimDefaultBuildSHA},
		{name: "the deployed build is the same string", key: "AGENT_REPL_DEPLOY_STAMP", want: FakeShimDefaultBuildSHA},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange / Act.
			got := valueOf(BuildIdentityEnv("/pinned"), tc.key)

			// Assert.
			if got != tc.want {
				t.Fatalf("BuildIdentityEnv()[%s] = %q, want %q", tc.key, got, tc.want)
			}
		})
	}
}

// TestCheckBuildIdentityAgrees pins the self-check that runs before any test:
// a build stamp under the pinned checkout answers AHEAD of the harness's own
// identity (the shim stamp beats SHIM_BUILD_SHA), which is exactly the state
// that made the daemon judge every fake shim stale and relaunch it.
func TestCheckBuildIdentityAgrees(t *testing.T) {
	tests := []struct {
		name    string
		stamp   string
		wantErr bool
	}{
		{
			name:    "a checkout carrying neither stamp agrees",
			wantErr: false,
		},
		{
			name:    "the shim build stamp is refused",
			stamp:   filepath.Join("agent-shim", "claude", "shim", "dist", ".built-sha"),
			wantErr: true,
		},
		{
			name:    "the deploy stamp is refused",
			stamp:   filepath.Join("daemon", "bin", ".built-sha"),
			wantErr: true,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			checkout := t.TempDir()
			if tc.stamp != "" {
				path := filepath.Join(checkout, tc.stamp)
				if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
					t.Fatalf("mkdir the stamp's directory: %v", err)
				}
				if err := os.WriteFile(path, []byte("65f37409f0fec4d05360178b0be1c3dad5896286\n"), 0o644); err != nil {
					t.Fatalf("write the stamp: %v", err)
				}
			}

			// Act.
			err := checkBuildIdentityAgrees(checkout)

			// Assert.
			if (err != nil) != tc.wantErr {
				t.Fatalf("checkBuildIdentityAgrees = %v, want an error: %v", err, tc.wantErr)
			}
		})
	}
}

// TestNewPinnedCheckoutCarriesNoBuildStamp pins what makes the pinned checkout
// usable at all: it links in the render vocabulary, which has no flag
// override, while carrying no build stamp of the host's.
func TestNewPinnedCheckoutCarriesNoBuildStamp(t *testing.T) {
	// Arrange: a stand-in repository whose proto/ is the thing to be linked,
	// and whose shim stamp is the host state that must NOT come along.
	repo := t.TempDir()
	if err := os.MkdirAll(filepath.Join(repo, "proto", "vocab"), 0o755); err != nil {
		t.Fatalf("lay out the stand-in repository: %v", err)
	}
	stamp := filepath.Join(repo, "agent-shim", "claude", "shim", "dist", ".built-sha")
	if err := os.MkdirAll(filepath.Dir(stamp), 0o755); err != nil {
		t.Fatalf("lay out the stand-in shim dist: %v", err)
	}
	if err := os.WriteFile(stamp, []byte("65f37409f0fec4d05360178b0be1c3dad5896286\n"), 0o644); err != nil {
		t.Fatalf("write the stand-in stamp: %v", err)
	}

	// Act.
	checkout, err := newPinnedCheckout(filepath.Join(t.TempDir(), "checkout"), repo)
	if err != nil {
		t.Fatalf("newPinnedCheckout = %v, want a laid-out checkout", err)
	}

	// Assert.
	if _, err := os.Stat(filepath.Join(checkout, "proto", "vocab")); err != nil {
		t.Fatalf("the pinned checkout has no render vocabulary: %v", err)
	}
	if err := checkBuildIdentityAgrees(checkout); err != nil {
		t.Fatalf("the pinned checkout disagrees on the build identity: %v", err)
	}
}
