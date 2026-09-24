package harness

import (
	"os"
	"os/exec"
	"path/filepath"
	"testing"
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
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: stand-ins for every artifact the staging copies.
			dir := t.TempDir()
			previous := daemonBinary
			daemonBinary = writeStandIn(t, dir, "claude-repld", "a daemon\n")
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
