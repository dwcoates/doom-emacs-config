package harness

import (
	"os"
	"path/filepath"
	"testing"

	"agent-repld/internal/buildid"
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
	// Every checkout carries its requirements manifest, which the pinned one copies.
	if err := os.WriteFile(filepath.Join(repo, "requirements.json"), []byte(`{"requirements": []}`), 0o644); err != nil {
		t.Fatalf("write the stand-in manifest: %v", err)
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

func TestThePinnedCheckoutsManifestIncludesNothing(t *testing.T) {
	// Arrange
	repo := t.TempDir()
	if err := os.WriteFile(filepath.Join(repo, "requirements.json"), []byte(`{"includes": ["d/requirements.json"], "requirements": []}`), 0o644); err != nil {
		t.Fatalf("write the stand-in manifest: %v", err)
	}

	// Act
	checkout, err := newPinnedCheckout(filepath.Join(t.TempDir(), "checkout"), repo)

	// Assert
	if err != nil {
		t.Fatalf("newPinnedCheckout = %v, want a laid-out checkout", err)
	}
	body, err := os.ReadFile(filepath.Join(checkout, "requirements.json"))
	if err != nil || string(body) != `{"requirements":[]}` {
		t.Fatalf("the pinned checkout's manifest = %q, %v; want the repository's with no includes", body, err)
	}
}

func TestThePinnedCheckoutCarriesTheRepositorysRequirementsManifest(t *testing.T) {
	// Arrange
	repo := t.TempDir()
	if err := os.WriteFile(filepath.Join(repo, "requirements.json"), []byte(`{"requirements": []}`), 0o644); err != nil {
		t.Fatalf("write the stand-in manifest: %v", err)
	}

	// Act
	checkout, err := newPinnedCheckout(filepath.Join(t.TempDir(), "checkout"), repo)

	// Assert
	if err != nil {
		t.Fatalf("newPinnedCheckout = %v, want a laid-out checkout", err)
	}
	body, err := os.ReadFile(filepath.Join(checkout, "requirements.json"))
	if err != nil || string(body) != `{"requirements":[]}` {
		t.Fatalf("the pinned checkout's manifest = %q, %v; want the repository's", body, err)
	}
}

func TestThePinnedCheckoutStandsAsARepositorysMainWorktree(t *testing.T) {
	// Arrange
	repo := t.TempDir()
	if err := os.WriteFile(filepath.Join(repo, "requirements.json"), []byte(`{"requirements": []}`), 0o644); err != nil {
		t.Fatal(err)
	}

	// Act
	checkout, err := newPinnedCheckout(filepath.Join(t.TempDir(), "checkout"), repo)

	// Assert
	if err != nil {
		t.Fatalf("newPinnedCheckout = %v, want a laid-out checkout", err)
	}
	if info, err := os.Stat(filepath.Join(checkout, ".git")); err != nil || !info.IsDir() {
		t.Fatalf("the pinned checkout's .git = %v, %v; want the directory its fake common dir names", info, err)
	}
}

// useCheckoutRepo points newDaemonCheckout at the real repository for one
// test, as runPinned does for the suite, and restores the previous root.
func useCheckoutRepo(t *testing.T) {
	t.Helper()
	previous := checkoutRepo
	checkoutRepo = RepoRoot(t)
	t.Cleanup(func() { checkoutRepo = previous })
}

// TestEveryDaemonLaysItsOwnCheckout pins what makes parallel deploys safe: two
// daemons' checkouts are two directories, each a whole pinned checkout with no
// build stamp, so one daemon's install never touches another's files.
func TestEveryDaemonLaysItsOwnCheckout(t *testing.T) {
	// Arrange
	useCheckoutRepo(t)
	parent := t.TempDir()

	// Act
	first := newDaemonCheckout(t, filepath.Join(parent, "first", "checkout"))
	second := newDaemonCheckout(t, filepath.Join(parent, "second", "checkout"))

	// Assert
	if first == second {
		t.Fatalf("both daemons' checkouts are %s, want two directories", first)
	}
	tests := []struct {
		name    string
		rel     string
		present bool
	}{
		{name: "the fake repository's common dir", rel: ".git", present: true},
		{name: "the elisp loader", rel: "config.el", present: true},
		{name: "the requirements manifest", rel: "requirements.json", present: true},
		{name: "the render vocabulary link", rel: "proto", present: true},
		{name: "no deploy stamp", rel: filepath.Join("daemon", "bin", ".built-sha"), present: false},
		{name: "no shim stamp", rel: filepath.Join("agent-shim", "claude", "shim", "dist", ".built-sha"), present: false},
	}
	for _, checkout := range []string{first, second} {
		for _, tc := range tests {
			t.Run(filepath.Base(filepath.Dir(checkout))+"/"+tc.name, func(t *testing.T) {
				_, err := os.Lstat(filepath.Join(checkout, tc.rel))
				if got := err == nil; got != tc.present {
					t.Fatalf("%s present = %v (%v), want %v", tc.rel, got, err, tc.present)
				}
			})
		}
	}
}

// TestLayingADaemonCheckoutLeavesThePinnedElispBuild pins that the global
// every Emacs stream reports is stated once by runPinned: a daemon laying its
// checkout from a parallel test never writes it.
func TestLayingADaemonCheckoutLeavesThePinnedElispBuild(t *testing.T) {
	// Arrange
	useCheckoutRepo(t)
	previous := PinnedElispBuild
	PinnedElispBuild = "stated-once"
	t.Cleanup(func() { PinnedElispBuild = previous })

	// Act
	newDaemonCheckout(t, filepath.Join(t.TempDir(), "checkout"))

	// Assert
	if PinnedElispBuild != "stated-once" {
		t.Fatalf("PinnedElispBuild = %q after laying a daemon checkout, want it untouched", PinnedElispBuild)
	}
}

// TestADaemonCheckoutsElispBuildIsTheTemplates pins why one global suffices:
// every daemon's checkout carries the template's loader, so the build the
// daemon reads from its own checkout is the one PinnedElispBuild states.
func TestADaemonCheckoutsElispBuildIsTheTemplates(t *testing.T) {
	// Arrange
	useCheckoutRepo(t)
	template, err := newPinnedCheckout(filepath.Join(t.TempDir(), "template"), checkoutRepo)
	if err != nil {
		t.Fatalf("newPinnedCheckout = %v, want a laid-out template", err)
	}
	want, err := buildid.Elisp(template)
	if err != nil {
		t.Fatalf("the template's elisp build: %v", err)
	}

	// Act
	got, err := buildid.Elisp(newDaemonCheckout(t, filepath.Join(t.TempDir(), "checkout")))

	// Assert
	if err != nil || got != want {
		t.Fatalf("the daemon checkout's elisp build = %q, %v; want the template's %q", got, err, want)
	}
}
