package kit

import (
	"os"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"testing"
	"time"
)

// TestWlBootBudgetMirrorsTheChildsBootBudget pins WlBootBudget to
// BOOT_BUDGET_MS in drive.ts, so the cold mount bound cannot drift below the
// budget the child itself boots its page under.
func TestWlBootBudgetMirrorsTheChildsBootBudget(t *testing.T) {
	t.Parallel()
	// Arrange.
	src, err := os.ReadFile(filepath.Join("..", "..", "webapp", "test", "webapp-layer", "drive.ts"))
	if err != nil {
		t.Fatalf("reading drive.ts: %v", err)
	}
	m := regexp.MustCompile(`export const BOOT_BUDGET_MS = ([0-9_]+);`).FindSubmatch(src)
	if m == nil {
		t.Fatal("drive.ts declares no `export const BOOT_BUDGET_MS = <n>;`")
	}

	// Act.
	ms, err := strconv.Atoi(strings.ReplaceAll(string(m[1]), "_", ""))
	if err != nil {
		t.Fatalf("BOOT_BUDGET_MS %q: %v", m[1], err)
	}

	// Assert.
	if got := time.Duration(ms) * time.Millisecond; got != WlBootBudget {
		t.Errorf("drive.ts BOOT_BUDGET_MS = %s, WlBootBudget = %s, want them equal", got, WlBootBudget)
	}
}

func TestWlParseLiveGroupMembersLeavesOutZombiesAndOtherGroups(t *testing.T) {
	t.Parallel()
	// Arrange.
	out := "  10    10 S\n  11    10 Z\n  12    10 R+\n  13    99 S\n"

	// Act.
	live, err := WlParseLiveGroupMembers(out, 10)

	// Assert.
	if err != nil || len(live) != 2 || live[0] != 10 || live[1] != 12 {
		t.Fatalf("live = %v (%v), want [10 12]", live, err)
	}
}

func TestWlParseLiveGroupMembersRefusesAMalformedLine(t *testing.T) {
	t.Parallel()
	// Arrange / Act.
	_, err := WlParseLiveGroupMembers("10 10\n", 10)

	// Assert.
	if err == nil {
		t.Fatal("a ps line with no stat was read, want it refused")
	}
}

func TestWebappWritableAcceptsAWritablePackageDirectory(t *testing.T) {
	t.Parallel()
	// Arrange: an ordinary writable directory, as a checkout is.
	dir := t.TempDir()

	// Act.
	err := WlWebappWritable(dir)

	// Assert.
	if err != nil {
		t.Fatalf("WlWebappWritable(%s) = %v, want nil", dir, err)
	}
}

func TestWebappLayerCoverageEnvHandsEachDriverADirectoryOfItsOwn(t *testing.T) {
	// Arrange
	root := t.TempDir()

	// Act
	first, err := DriverCoverageEnv(root)
	if err != nil {
		t.Fatal(err)
	}
	second, err := DriverCoverageEnv(root)
	if err != nil {
		t.Fatal(err)
	}

	// Assert
	dirs := map[string]bool{}
	for _, env := range [][]string{first, second} {
		if len(env) != 1 || !strings.HasPrefix(env[0], WebappLayerCoverageEnv+"=") {
			t.Fatalf("env = %v, want one %s assignment", env, WebappLayerCoverageEnv)
		}
		dir := strings.TrimPrefix(env[0], WebappLayerCoverageEnv+"=")
		if filepath.Dir(dir) != filepath.Join(root, "webapp-layer") {
			t.Fatalf("%s is not under %s/webapp-layer", dir, root)
		}
		if st, err := os.Stat(dir); err != nil || !st.IsDir() {
			t.Fatalf("%s was not made: %v", dir, err)
		}
		dirs[dir] = true
	}
	if len(dirs) != 2 {
		t.Fatalf("two drivers share %v", dirs)
	}
}

func TestWebappLayerCoverageEnvIsNothingWithoutCoverage(t *testing.T) {
	// Act
	env, err := DriverCoverageEnv("")

	// Assert
	if err != nil || env != nil {
		t.Fatalf("env = %v, %v, want nothing", env, err)
	}
}

func TestWebappLayerCoverageEnvRefusesAnUnwritableRoot(t *testing.T) {
	// Arrange: the root is a file, so nothing can be made under it.
	root := filepath.Join(t.TempDir(), "file")
	if err := os.WriteFile(root, nil, 0o644); err != nil {
		t.Fatal(err)
	}

	// Act
	_, err := DriverCoverageEnv(root)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "make the webapp layer's coverage directory") {
		t.Fatalf("err = %v, want the directory named", err)
	}
}
