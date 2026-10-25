package harness

import (
	"crypto/sha256"
	"encoding/hex"
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	"agentrepl/logging/buildreport"
)

// TestFakeDeployBuilderStagesTheServicesTheWorldRuns pins what the staged
// cache-bin carries for each service: a copy of the real binary a world names
// (so the real process's own report reads up to date), else the placeholder
// whose report the harness states itself.
//
// Not parallel: the staging copies the harness's daemon binary, a package
// global only Main sets, which this test points at a stand-in.
func TestFakeDeployBuilderStagesTheServicesTheWorldRuns(t *testing.T) {
	tests := []struct {
		name    string
		real    bool
		service string
		want    func(store, sidecar string) string
	}{
		{name: "a real store is staged as a copy of its binary", real: true, service: "shim-store",
			want: func(store, _ string) string { return readOrFail(t, store) }},
		{name: "a real sidecar is staged as a copy of its binary", real: true, service: "shim-claude-sidecar",
			want: func(_, sidecar string) string { return readOrFail(t, sidecar) }},
		{name: "no real store stages the placeholder", service: "shim-store",
			want: func(_, _ string) string { return fakeServiceBinary("shim-store") }},
		{name: "no real sidecar stages the placeholder", service: "shim-claude-sidecar",
			want: func(_, _ string) string { return fakeServiceBinary("shim-claude-sidecar") }},
		{name: "zettel stages the placeholder beside real services", real: true, service: "zettel",
			want: func(_, _ string) string { return fakeServiceBinary("zettel") }},
		{name: "zettel stages the placeholder beside placeholder services", service: "zettel",
			want: func(_, _ string) string { return fakeServiceBinary("zettel") }},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: stand-ins for every artifact the staging copies.
			dir := t.TempDir()
			previous := daemonBinary
			daemonBinary = writeStandIn(t, dir, "agent-repld", "a daemon\n")
			t.Cleanup(func() { daemonBinary = previous })
			dist := filepath.Join(dir, "dist")
			if err := os.MkdirAll(dist, 0o755); err != nil {
				t.Fatalf("mkdir %s: %v", dist, err)
			}
			src := DeploySources{
				ShimMain:   writeStandIn(t, dir, "main.js", "// shim\n"),
				WebappDist: dist,
			}
			store := writeStandIn(t, dir, "real-store", "the real store\n")
			sidecar := writeStandIn(t, dir, "real-sidecar", "the real sidecar\n")
			if tc.real {
				src.Store, src.Sidecar = store, sidecar
			}
			b := NewFakeDeployBuilder(t, filepath.Join(dir, "bin"), src)
			b.Stage(DeployCurrent)
			out := filepath.Join(dir, "staging")

			// Act.
			if raw, err := exec.Command(b.Path, "--out", out).CombinedOutput(); err != nil {
				t.Fatalf("the fake build = %v:\n%s", err, raw)
			}

			// Assert.
			if got, want := readOrFail(t, filepath.Join(out, "cache-bin", tc.service)), tc.want(store, sidecar); got != want {
				t.Fatalf("staged %s = %q, want %q", tc.service, got, want)
			}
		})
	}
}

func writeStandIn(t *testing.T, dir, name, content string) string {
	t.Helper()
	path := filepath.Join(dir, name)
	if err := os.WriteFile(path, []byte(content), 0o755); err != nil {
		t.Fatalf("write %s: %v", path, err)
	}
	return path
}

func readOrFail(t *testing.T, path string) string {
	t.Helper()
	raw, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read %s: %v", path, err)
	}
	return string(raw)
}

// TestStateServiceBuildsNamesThePidItIsGiven pins that each placeholder
// service's stated report names the pid it is handed and the placeholder's own
// build, so a world can restate them under a handover successor once the
// incumbent StageDeployBuild named has exited.
func TestStateServiceBuildsNamesThePidItIsGiven(t *testing.T) {
	for _, service := range []string{buildreport.ServiceStore, buildreport.ServiceSidecar, buildreport.ServiceZettel} {
		t.Run(service, func(t *testing.T) {
			// Arrange
			d := &Daemon{t: t, LockDir: t.TempDir(), Deploy: &DeployBuilder{}}
			sum := sha256.Sum256([]byte(fakeServiceBinary(service)))

			// Act
			d.StateServiceBuilds(4242)

			// Assert
			got, ok, err := buildreport.Read(d.LockDir, service)
			if err != nil || !ok {
				t.Fatalf("Read(%s) = %v, %v, %v, want the stated report", service, got, ok, err)
			}
			if got.PID != 4242 || got.Build != hex.EncodeToString(sum[:]) {
				t.Fatalf("report = %+v, want pid 4242 and the placeholder's build", got)
			}
		})
	}
}

// TestPlaceholderServicesAreTheOnesNoWorldRunsForReal pins which services the
// harness states the builds of: zettel in every world, the store and the
// sidecar only in a world that runs no real ones.
func TestPlaceholderServicesAreTheOnesNoWorldRunsForReal(t *testing.T) {
	tests := []struct {
		name    string
		sources DeploySources
		want    []string
	}{
		{name: "no real services", sources: DeploySources{}, want: []string{buildreport.ServiceStore, buildreport.ServiceSidecar, buildreport.ServiceZettel}},
		{name: "a real store and sidecar", sources: DeploySources{Store: "/bin/store", Sidecar: "/bin/sidecar"}, want: []string{buildreport.ServiceZettel}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := tt.sources.placeholderServices()

			// Assert
			if !slices.Equal(got, tt.want) {
				t.Fatalf("placeholderServices() = %v, want %v", got, tt.want)
			}
		})
	}
}

// TestUnservedZettelEnvNamesTheZettelSocketUnderDir pins the one statement
// every test-daemon launcher makes: the zettel socket, under the world's dir.
func TestUnservedZettelEnvNamesTheZettelSocketUnderDir(t *testing.T) {
	// Act
	got := UnservedZettelEnv("/tmp/w")

	// Assert
	if want := "AGENT_REPL_ZETTEL_SOCKET=/tmp/w/unused-zettel.sock"; got != want {
		t.Fatalf("UnservedZettelEnv = %q, want %q", got, want)
	}
}

// TestEveryTestDaemonLauncherStatesTheUnservedZettel pins both launchers of
// a test daemon to UnservedZettelEnv, so neither can reach the owner's zettel.
func TestEveryTestDaemonLauncherStatesTheUnservedZettel(t *testing.T) {
	for _, file := range []string{"daemon.go", "../../../e2e/emacs_e2e_test.go"} {
		t.Run(file, func(t *testing.T) {
			// Act
			source := readOrFail(t, file)

			// Assert
			if !strings.Contains(source, "UnservedZettelEnv(") {
				t.Fatalf("%s states no UnservedZettelEnv", file)
			}
		})
	}
}
