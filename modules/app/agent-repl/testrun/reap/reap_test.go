package reap

import (
	"os"
	"os/exec"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"
)

func TestMatching(t *testing.T) {
	tests := []struct {
		name string
		ps   string
		dirs []string
		want []int
	}{
		{name: "a process naming a path under dir", ps: "  101 /tmp/artr-7/bin/daemon --state /tmp/artr-7/s\n", dirs: []string{"/tmp/artr-7"}, want: []int{101}},
		{name: "a process naming no path under dir", ps: "  102 /usr/bin/emacs -batch HOME=/Users/x\n", dirs: []string{"/tmp/artr-7"}, want: nil},
		{name: "a trailing slash on dir", ps: "  103 sh /tmp/artr-7/x.sh\n", dirs: []string{"/tmp/artr-7/"}, want: []int{103}},
		{name: "a sibling sharing the prefix", ps: "  104 sh /tmp/artr-71/x.sh TMPDIR=/tmp/artr-71/tr-1\n", dirs: []string{"/tmp/artr-7"}, want: nil},
		{name: "this process", ps: "  42 testrun /tmp/artr-7/x\n", dirs: []string{"/tmp/artr-7"}, want: nil},
		{name: "a line with no pid", ps: "  abc /tmp/artr-7/x\n", dirs: []string{"/tmp/artr-7"}, want: nil},
		{name: "a line with no command", ps: "  105\n", dirs: []string{"/tmp/artr-7"}, want: nil},
		{name: "a process handed dir only through its environment", ps: "  106 /usr/bin/emacs -batch -l lisp/test-x.el TMPDIR=/tmp/artr-7/tr-1/tu-2 HOME=/Users/x\n", dirs: []string{"/tmp/artr-7"}, want: []int{106}},
		{name: "dir itself at the end of the line", ps: "  107 go test ./... AGENT_REPL_TEST_SHORT_BASE=/tmp/artr-7\n", dirs: []string{"/tmp/artr-7"}, want: []int{107}},
		{name: "dir itself before a space", ps: "  108 go test AGENT_REPL_TEST_SHORT_BASE=/tmp/artr-7 HOME=/Users/x\n", dirs: []string{"/tmp/artr-7"}, want: []int{108}},
		{name: "a sibling first and dir after it", ps: "  109 sh /tmp/artr-71/x /tmp/artr-7/y\n", dirs: []string{"/tmp/artr-7"}, want: []int{109}},
		{name: "dir's resolved spelling", ps: "  110 emacs TMPDIR=/private/tmp/artr-7/tr-1\n", dirs: []string{"/tmp/artr-7", "/private/tmp/artr-7"}, want: []int{110}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := Matching(tc.ps, tc.dirs, 42)

			// Assert
			if !slices.Equal(got, tc.want) {
				t.Fatalf("Matching = %v, want %v", got, tc.want)
			}
		})
	}
}

func TestSpellings(t *testing.T) {
	// Arrange: a parent reached through a symlink.
	real := t.TempDir()
	resolvedReal, err := filepath.EvalSymlinks(real)
	if err != nil {
		t.Fatal(err)
	}
	link := filepath.Join(t.TempDir(), "link")
	if err := os.Symlink(resolvedReal, link); err != nil {
		t.Fatal(err)
	}
	tests := []struct {
		name string
		dir  string
		want []string
	}{
		{name: "a resolved parent is one spelling", dir: filepath.Join(resolvedReal, "artr-7"), want: []string{filepath.Join(resolvedReal, "artr-7")}},
		{name: "a symlinked parent is two", dir: filepath.Join(link, "artr-7"), want: []string{filepath.Join(link, "artr-7"), filepath.Join(resolvedReal, "artr-7")}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got, err := spellings(tc.dir)

			// Assert
			if err != nil || !slices.Equal(got, tc.want) {
				t.Fatalf("spellings = %q, %v; want %q", got, err, tc.want)
			}
		})
	}
}

func TestSpellingsFailsAMissingParent(t *testing.T) {
	// Act
	_, err := spellings(filepath.Join(t.TempDir(), "gone", "artr-7"))

	// Assert
	if err == nil || !strings.Contains(err.Error(), "resolve the directory holding") {
		t.Fatalf("spellings = %v, want the failed resolution", err)
	}
}

func TestUnderFailsAMissingParentWithoutKilling(t *testing.T) {
	// Act
	killed, err := Under(filepath.Join(t.TempDir(), "gone", "artr-7"))

	// Assert
	if err == nil || killed != 0 {
		t.Fatalf("Under = %d, %v; want the failed resolution and nothing killed", killed, err)
	}
}

func TestUnderKillsAProcessHandedDirOnlyThroughItsEnvironment(t *testing.T) {
	// Arrange: a process whose command line names nothing under dir.
	dir := t.TempDir()
	cmd := exec.Command("sh", "-c", "read x")
	cmd.Env = append(os.Environ(), "TMPDIR="+dir+"/tr-1")
	stdin, err := cmd.StdinPipe()
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { stdin.Close() })
	if err := cmd.Start(); err != nil {
		t.Fatal(err)
	}
	exited := make(chan error, 1)
	go func() { exited <- cmd.Wait() }()

	// Act
	killed, err := Under(dir)

	// Assert
	if err != nil || killed != 1 {
		t.Fatalf("Under = %d, %v; want the one process killed", killed, err)
	}
	select {
	case <-exited:
	case <-time.After(5 * time.Second):
		t.Fatal("the process outlived its kill")
	}
}

func TestUnderKillsAProcessRunningOutOfDir(t *testing.T) {
	// Arrange: a process whose command line names a path under dir, waiting
	// on its stdin in a builtin, so it starts no child of its own.
	dir := t.TempDir()
	cmd := exec.Command("sh", "-c", "read x", dir+"/marker")
	stdin, err := cmd.StdinPipe()
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { stdin.Close() })
	if err := cmd.Start(); err != nil {
		t.Fatal(err)
	}
	exited := make(chan error, 1)
	go func() { exited <- cmd.Wait() }()

	// Act
	killed, err := Under(dir)

	// Assert
	if err != nil || killed != 1 {
		t.Fatalf("Under = %d, %v; want the one process killed", killed, err)
	}
	select {
	case <-exited:
	case <-time.After(5 * time.Second):
		t.Fatal("the process outlived its kill")
	}
}

func TestReclaimsShareUnder(t *testing.T) {
	// Arrange: every reclaim of a dead run's processes.
	sites := []string{"../internal/ramdisk/ramdisk.go", "../../daemon/integration/harness/runroot.go"}

	for _, site := range sites {
		// Act
		src, err := os.ReadFile(site)
		if err != nil {
			t.Fatal(err)
		}

		// Assert
		if !strings.Contains(string(src), "reap.Under") || strings.Contains(string(src), "pid=,args=") {
			t.Errorf("%s lists processes to kill by hand; want reap.Under", site)
		}
	}
}
