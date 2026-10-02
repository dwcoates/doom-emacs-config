// Package cover is `testrun cover-report`: merge the coverage counters every
// package unit of one Go module wrote, and print the module's function report
// and total, the shape bin/report-nonlisp-coverage.sh prints for it.
package cover

import (
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"

	"agentrepl/testrun/internal/command"
)

// Runner runs one go tool command in dir and returns its stdout.
type Runner func(dir string, args ...string) ([]byte, error)

// GoTool is the real Runner.
func GoTool(dir string, args ...string) ([]byte, error) {
	cmd := exec.Command("go", args...)
	cmd.Dir = dir
	return command.Output(cmd)
}

// Report merges every coverage directory under covRoot and writes the
// function report plus a summary line to out.
func Report(goTool Runner, out io.Writer, name, module, covRoot string) error {
	entries, err := os.ReadDir(covRoot)
	if err != nil {
		return fmt.Errorf("cover: read %s: %w", covRoot, err)
	}
	var dirs []string
	for _, e := range entries {
		if e.IsDir() {
			dirs = append(dirs, filepath.Join(covRoot, e.Name()))
		}
	}
	sort.Strings(dirs)
	if len(dirs) == 0 {
		return fmt.Errorf("cover: %s holds no package coverage directories", covRoot)
	}
	profile := filepath.Join(covRoot, name+".coverprofile")
	if _, err := goTool(module, "tool", "covdata", "textfmt", "-i="+strings.Join(dirs, ","), "-o="+profile); err != nil {
		return fmt.Errorf("cover: merge %s's coverage: %w", name, err)
	}
	report, err := goTool(module, "tool", "cover", "-func="+profile)
	if err != nil {
		return fmt.Errorf("cover: %s's function report: %w", name, err)
	}
	lines := strings.Split(strings.TrimRight(string(report), "\n"), "\n")
	summary := lines[len(lines)-1]
	if !strings.Contains(summary, "(statements)") {
		return fmt.Errorf("cover: %s's coverage summary is malformed: %s", name, summary)
	}
	if _, err := out.Write(report); err != nil {
		return fmt.Errorf("cover: write the report: %w", err)
	}
	_, err = fmt.Fprintf(out, "[agent-repl-coverage] %s: %s\n", name, summary)
	return err
}
