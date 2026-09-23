package deploy

import (
	"os"
	"path/filepath"
	"testing"

	"claude-repld/internal/dlog"
)

func TestInstallingAFileReplacesItWithItsStamps(t *testing.T) {
	// Arrange
	staged, live := t.TempDir(), t.TempDir()
	writeFile(t, filepath.Join(staged, "shim-store"), "new")
	writeFile(t, filepath.Join(staged, ".shim-store.built-sha"), "sha-new")
	writeFile(t, filepath.Join(live, "shim-store"), "old")
	in := installer{nonce: "n1", log: dlog.NewTestLogger()}

	// Act
	err := in.file(filepath.Join(staged, "shim-store"), filepath.Join(live, "shim-store"),
		stampsBeside(staged, live, "shim-store."))

	// Assert
	if err != nil {
		t.Fatalf("file: %v", err)
	}
	if got := readFile(t, filepath.Join(live, "shim-store")); got != "new" {
		t.Fatalf("installed = %q, want the staged bytes", got)
	}
	if got := readFile(t, filepath.Join(live, ".shim-store.built-sha")); got != "sha-new" {
		t.Fatalf("stamp = %q, want the staged stamp", got)
	}
	info, err := os.Stat(filepath.Join(live, "shim-store"))
	if err != nil || info.Mode().Perm()&0o100 == 0 {
		t.Fatalf("mode = %v (%v), want the staged binary's executable bit kept", info.Mode(), err)
	}
	leftovers, err := filepath.Glob(filepath.Join(live, ".*.install-*"))
	if err != nil || len(leftovers) != 0 {
		t.Fatalf("temporaries left behind: %v (%v)", leftovers, err)
	}
}

func TestAMissingStampIsWarnedNotFatal(t *testing.T) {
	// Arrange: the build wrote no source-tree stamp.
	staged, live := t.TempDir(), t.TempDir()
	writeFile(t, filepath.Join(staged, "claude-repld"), "new")
	log := dlog.NewTestLogger()
	in := installer{nonce: "n1", log: log}

	// Act
	err := in.file(filepath.Join(staged, "claude-repld"), filepath.Join(live, "claude-repld"), stampsBeside(staged, live, ""))

	// Assert
	if err != nil {
		t.Fatalf("file: %v", err)
	}
	if !loggedTo(log, "warn", "a build stamp was not staged") {
		t.Fatalf("records = %+v, want the missing stamp warned", log.Records())
	}
}

func TestAMissingStagedArtifactFailsTheInstall(t *testing.T) {
	// Arrange
	staged, live := t.TempDir(), t.TempDir()
	writeFile(t, filepath.Join(live, "main.js"), "old")
	in := installer{nonce: "n1", log: dlog.NewTestLogger()}

	// Act
	err := in.file(filepath.Join(staged, "main.js"), filepath.Join(live, "main.js"), nil)

	// Assert
	if err == nil {
		t.Fatalf("an install of nothing succeeded")
	}
	if got := readFile(t, filepath.Join(live, "main.js")); got != "old" {
		t.Fatalf("installed = %q, want the old artifact untouched", got)
	}
}

func TestInstallingATreeSwapsItWhole(t *testing.T) {
	tests := []struct {
		name    string
		hadLive bool
	}{
		{name: "over an installed tree", hadLive: true},
		{name: "where none was installed"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			staged := filepath.Join(t.TempDir(), "dist")
			parent := t.TempDir()
			live := filepath.Join(parent, "dist")
			writeFile(t, filepath.Join(staged, "index.html"), "new-index")
			writeFile(t, filepath.Join(staged, "assets", "index-B.js"), "new-bundle")
			if tc.hadLive {
				writeFile(t, filepath.Join(live, "index.html"), "old-index")
				writeFile(t, filepath.Join(live, "assets", "index-A.js"), "old-bundle")
			}
			in := installer{nonce: "n1", log: dlog.NewTestLogger()}

			// Act
			err := in.dir(staged, live)

			// Assert
			if err != nil {
				t.Fatalf("dir: %v", err)
			}
			if got := readFile(t, filepath.Join(live, "assets", "index-B.js")); got != "new-bundle" {
				t.Fatalf("bundle = %q, want the staged tree", got)
			}
			if _, err := os.Stat(filepath.Join(live, "assets", "index-A.js")); !os.IsNotExist(err) {
				t.Fatalf("the old bundle survived the swap: %v", err)
			}
			entries, err := os.ReadDir(parent)
			if err != nil || len(entries) != 1 {
				t.Fatalf("entries beside the tree = %v (%v), want the retired tree removed", entries, err)
			}
		})
	}
}

func TestLiveNamesWhereTheDaemonRunsFrom(t *testing.T) {
	tests := []struct {
		name       string
		live       Live
		wantShim   string
		wantWebapp string
	}{
		{name: "the checkout's own artifacts", live: Live{ModuleRoot: "/m"},
			wantShim: "/m/agent-shim/claude/shim/dist/main.js", wantWebapp: "/m/webapp/dist"},
		{name: "the daemon pointed elsewhere", live: Live{ModuleRoot: "/m", Shim: "/s/main.js", Webapp: "/w/dist"},
			wantShim: "/s/main.js", wantWebapp: "/w/dist"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			shim, webapp := tc.live.ShimMain(), tc.live.WebappDist()

			// Assert
			if shim != tc.wantShim || webapp != tc.wantWebapp {
				t.Fatalf("live = (%q, %q), want (%q, %q)", shim, webapp, tc.wantShim, tc.wantWebapp)
			}
		})
	}
}
