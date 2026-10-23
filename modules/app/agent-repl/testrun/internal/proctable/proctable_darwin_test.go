package proctable

import (
	"os"
	"reflect"
	"slices"
	"testing"
)

func TestListNamesThisProcess(t *testing.T) {
	// Arrange
	t.Setenv("PROCTABLE_TEST_MARK", "1")

	// Act
	entries, err := List()

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	for _, e := range entries {
		if e.Pid != os.Getpid() {
			continue
		}
		if e.Ppid != os.Getppid() || e.Start.IsZero() || e.State == '?' || !reflect.DeepEqual(e.Argv, os.Args) {
			t.Fatalf("this process is listed as %+v, want ppid %d, a start and argv %q", e, os.Getppid(), os.Args)
		}
		return
	}
	t.Fatal("this process is not listed")
}

// The environment kern.procargs2 hands over is the one the process started
// with: a variable set later is not in it, one it inherited is.
func TestListReadsTheEnvironmentAProcessStartedWith(t *testing.T) {
	// Act
	entries, err := List()

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	i := slices.IndexFunc(entries, func(e Entry) bool { return e.Pid == os.Getpid() })
	if i < 0 {
		t.Fatal("this process is not listed")
	}
	if !slices.Contains(entries[i].Env, "HOME="+os.Getenv("HOME")) {
		t.Fatalf("env %q lacks this process's HOME", entries[i].Env)
	}
}
