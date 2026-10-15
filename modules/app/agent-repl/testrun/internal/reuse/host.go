package reuse

import (
	"bufio"
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"
	"time"

	"agentrepl/testrun/internal/gitinfo"
	"agentrepl/testrun/roster"
)

// HostDeps are Deps over the real host: the checkout holding module, every
// checkout of its repository, the host's pass store, and the real go, git and
// sandbox-exec.
func HostDeps(module, self, profileDir string, log Logger) (Deps, error) {
	abs, err := filepath.Abs(module)
	if err != nil {
		return Deps{}, fmt.Errorf("reuse: resolve the module root %s: %w", module, err)
	}
	realModule, err := filepath.EvalSymlinks(abs)
	if err != nil {
		return Deps{}, fmt.Errorf("reuse: resolve the module root %s: %w", module, err)
	}
	repo := filepath.Clean(filepath.Join(realModule, "..", "..", ".."))
	roots, err := checkouts(repo)
	if err != nil {
		return Deps{}, err
	}
	storeDir, err := DefaultStoreDir()
	if err != nil {
		return Deps{}, err
	}
	return Deps{
		Repo: repo, Module: realModule, DenyRoots: roots,
		Store: Store{Dir: storeDir}, GoList: ExecGoList,
		Toolchain: hostToolchain(), Environ: os.Environ(),
		Self: self, ProfileDir: profileDir,
		Probe: func() error {
			return ProbeSandbox(func(name string, args ...string) error {
				out, err := exec.Command(name, args...).CombinedOutput()
				if err != nil {
					return fmt.Errorf("%w: %s", err, strings.TrimSpace(string(out)))
				}
				return nil
			})
		},
		Head: func() (string, string, bool, error) {
			branch, commit, err := gitinfo.Head(repo)
			if err != nil {
				return "", "", false, err
			}
			dirty, err := gitinfo.Dirty(repo)
			return branch, commit, dirty, err
		},
		Now: time.Now,
		Log: log,
	}, nil
}

// checkouts is every worktree of the repository at repo, resolved through
// symlinks: `git worktree list` names the main checkout and every linked one.
func checkouts(repo string) ([]string, error) {
	out, err := gitinfo.Output(repo, "worktree", "list", "--porcelain")
	if err != nil {
		return nil, err
	}
	var roots []string
	sc := bufio.NewScanner(bytes.NewReader(out))
	for sc.Scan() {
		p, ok := strings.CutPrefix(sc.Text(), "worktree ")
		if !ok {
			continue
		}
		real, err := filepath.EvalSymlinks(p)
		if err != nil {
			// A worktree git still lists but whose directory is gone holds no
			// files to read: nothing to deny.
			if os.IsNotExist(err) {
				continue
			}
			return nil, fmt.Errorf("reuse: resolve the checkout %s: %w", p, err)
		}
		roots = append(roots, real)
	}
	if err := sc.Err(); err != nil {
		return nil, fmt.Errorf("reuse: read git worktree list: %w", err)
	}
	found := false
	for _, r := range roots {
		found = found || r == repo
	}
	if !found {
		return nil, fmt.Errorf("reuse: git worktree list does not name the checkout %s", repo)
	}
	return roots, nil
}

// hostToolchain answers the tool versions every keyed suite is built and run
// with, once per run: Go builds every keyed suite, node and npm run the
// TypeScript suites and build the Node programs others run (the sidecar's
// mocked vendor is the shim's bundle), and Emacs runs the ERT suite. A tool not on PATH is recorded as absent, which is itself a fact a
// test can observe (a suite skips its Node cases without node).
func hostToolchain() func(roster.Kind) (map[string]string, error) {
	var once sync.Once
	var tools map[string]string
	var toolsErr error
	return func(roster.Kind) (map[string]string, error) {
		once.Do(func() {
			out, err := exec.Command("go", "env", "GOVERSION", "GOOS", "GOARCH").Output()
			if err != nil {
				toolsErr = fmt.Errorf("reuse: go env: %w", err)
				return
			}
			f := strings.Fields(string(out))
			if len(f) != 3 {
				toolsErr = fmt.Errorf("reuse: go env answered %q, want three values", out)
				return
			}
			tools = map[string]string{"go": f[0], "goos": f[1], "goarch": f[2]}
			for _, tool := range []string{"node", "npm", "emacs"} {
				v, err := toolVersion(tool)
				if err != nil {
					toolsErr = err
					return
				}
				tools[tool] = v
			}
		})
		return tools, toolsErr
	}
}

// toolVersion is the first line of `tool --version`, or "absent" when tool is
// not on PATH.
func toolVersion(tool string) (string, error) {
	p, err := exec.LookPath(tool)
	if err != nil {
		return "absent", nil
	}
	out, err := exec.Command(p, "--version").Output()
	if err != nil {
		return "", fmt.Errorf("reuse: %s --version: %w", tool, err)
	}
	first, _, _ := strings.Cut(strings.TrimSpace(string(out)), "\n")
	return first, nil
}
