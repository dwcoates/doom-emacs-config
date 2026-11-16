package procgroup

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestOnlyProcgroupReadsAProcessState pins that every read of one process's
// kernel state in the daemon module goes through this package (ReadState,
// Exiting), so the platforms' readings live in one place. Listing processes
// (the harness's stray sweep) is another question and is not this one.
func TestOnlyProcgroupReadsAProcessState(t *testing.T) {
	// Arrange
	var offenders []string

	// Act
	err := filepath.WalkDir(filepath.Join("..", ".."), func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") || filepath.Base(filepath.Dir(path)) == "procgroup" {
			return nil
		}
		src, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(src), `"kern.proc.pid"`) || strings.Contains(string(src), `"/proc/" + strconv.Itoa(pid) + "/stat"`) || strings.Contains(string(src), `"/proc/%d/stat"`) {
			offenders = append(offenders, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walk the daemon module: %v", err)
	}
	if len(offenders) > 0 {
		t.Fatalf("%v read the kernel's process table themselves instead of through procgroup", offenders)
	}
}
