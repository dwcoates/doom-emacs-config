// Package gitinfo is the runner's one way to ask git about the checkout it
// runs in: its branch and commit, whether its files differ from that commit,
// and anything else a caller names. Every question goes through Output, so a
// failure always says which git command failed and what git printed.
package gitinfo

import (
	"bytes"
	"fmt"
	"os/exec"
	"strings"
)

// gitBinary is the git the runner runs; tests point it at a fake.
var gitBinary = "git"

// Output runs `git -C repo args...` and answers its standard output.
func Output(repo string, args ...string) ([]byte, error) {
	cmd := exec.Command(gitBinary, append([]string{"-C", repo}, args...)...)
	var stderr bytes.Buffer
	cmd.Stderr = &stderr
	out, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("git %s: %w: %s", strings.Join(args, " "), err, strings.TrimSpace(stderr.String()))
	}
	return out, nil
}

// Head answers the checkout's branch ("" when detached) and HEAD commit.
func Head(repo string) (branch, commit string, err error) {
	b, err := Output(repo, "branch", "--show-current")
	if err != nil {
		return "", "", err
	}
	c, err := Output(repo, "rev-parse", "HEAD")
	if err != nil {
		return "", "", err
	}
	return strings.TrimSpace(string(b)), strings.TrimSpace(string(c)), nil
}

// Dirty reports whether any file of the checkout differs from HEAD: a tracked
// change, or an untracked file git does not ignore.
func Dirty(repo string) (bool, error) {
	out, err := Output(repo, "status", "--porcelain", "--untracked-files=normal")
	if err != nil {
		return false, err
	}
	return len(bytes.TrimSpace(out)) > 0, nil
}
