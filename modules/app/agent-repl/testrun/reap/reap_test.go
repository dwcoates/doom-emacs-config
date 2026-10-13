package reap

import (
	"os"
	"os/exec"
	"slices"
	"strings"
	"testing"
	"time"
)

func TestMatching(t *testing.T) {
	tests := []struct {
		name string
		ps   string
		dir  string
		want []int
	}{
		{name: "a process naming a path under dir", ps: "  101 /tmp/artr-7/bin/daemon --state /tmp/artr-7/s\n", dir: "/tmp/artr-7", want: []int{101}},
		{name: "a process naming no path under dir", ps: "  102 /usr/bin/emacs -batch\n", dir: "/tmp/artr-7", want: nil},
		{name: "a trailing slash on dir", ps: "  103 sh /tmp/artr-7/x.sh\n", dir: "/tmp/artr-7/", want: []int{103}},
		{name: "a sibling sharing the prefix", ps: "  104 sh /tmp/artr-71/x.sh\n", dir: "/tmp/artr-7", want: nil},
		{name: "this process", ps: "  42 testrun /tmp/artr-7/x\n", dir: "/tmp/artr-7", want: nil},
		{name: "a line with no pid", ps: "  abc /tmp/artr-7/x\n", dir: "/tmp/artr-7", want: nil},
		{name: "a line with no command", ps: "  105\n", dir: "/tmp/artr-7", want: nil},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Act
			got := Matching(tc.ps, tc.dir, 42)

			// Assert
			if !slices.Equal(got, tc.want) {
				t.Fatalf("Matching = %v, want %v", got, tc.want)
			}
		})
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
