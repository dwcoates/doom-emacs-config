package reload

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// DeployScriptRelPath is where bin/deploy-all.sh sits inside its host
// repository. It doubles as the MARKER that identifies a directory as an
// agent-repl checkout: the script is the deploy path, so a checkout without it
// is one this package could not redeploy anyway.
const DeployScriptRelPath = stackPrefix + "bin/deploy-all.sh"

// SelfRepo is the identity of the checkout THIS daemon binary was built and
// deployed from.
//
// Both fields are needed and neither substitutes for the other:
//
//   - CommonDir is the repository identity, resolved exactly the way
//     merge.GitRepoKeyer resolves a queue key (`rev-parse --git-common-dir`,
//     canonicalized). Every linked worktree of one repository shares it, so it
//     answers "is the merge target the same REPOSITORY as mine".
//   - Root is the worktree, so it answers "is the merge target the very
//     checkout my running code was built from". A sibling worktree of the same
//     repository is the same repository but NOT the same code: rebuilding from
//     Root would deploy a tree that does not contain the merged commits at all.
type SelfRepo struct {
	// Root is the canonical absolute path of the daemon's own checkout.
	Root string
	// CommonDir is the canonical absolute path of that checkout's git common dir.
	CommonDir string
}

// ScriptPath is the redeploy entry point inside this checkout.
func (s SelfRepo) ScriptPath() string {
	return filepath.Join(s.Root, filepath.FromSlash(DeployScriptRelPath))
}

// validate reports whether the identity is usable.
func (s SelfRepo) validate() error {
	switch {
	case s.Root == "":
		return fmt.Errorf("reload: SelfRepo Root is required")
	case s.CommonDir == "":
		return fmt.Errorf("reload: SelfRepo CommonDir is required")
	}
	return nil
}

// ResolveSelf identifies the checkout that contains exePath, which callers pass
// as os.Executable(): a deployed daemon binary lives at
// <checkout>/modules/app/agent-repl/daemon/bin/claude-repld, so walking up from
// the binary to the directory holding bin/deploy-all.sh IS the checkout that
// built it.
//
// The (SelfRepo, bool, error) shape distinguishes two genuinely different
// answers, and the distinction is the whole reason this is not a plain error
// return:
//
//   - (repo, true, nil): the binary is deployed from an agent-repl checkout.
//   - (zero, false, nil): it is NOT, and that is a FACT about this binary, not
//     a failure. A `go test` binary lives in the build cache and a hand-copied
//     binary lives wherever it was copied; neither has a checkout to redeploy,
//     and neither may ever bounce the live stack. Callers turn this into a
//     loud, once-per-boot "self-redeploy disabled" record.
//   - (zero, false, err): the binary IS in a checkout but git could not answer
//     for it. That is a real failure and is surfaced, never downgraded to the
//     "not deployed" answer, because it would otherwise silently disable the
//     redeploy on a machine where it is supposed to work.
func ResolveSelf(ctx context.Context, exePath string) (SelfRepo, bool, error) {
	if exePath == "" {
		return SelfRepo{}, false, fmt.Errorf("reload: ResolveSelf needs an executable path")
	}
	resolved, err := filepath.EvalSymlinks(exePath)
	if err != nil {
		return SelfRepo{}, false, fmt.Errorf("reload: canonicalize executable %s: %w", exePath, err)
	}
	root, ok := checkoutContaining(filepath.Dir(resolved))
	if !ok {
		return SelfRepo{}, false, nil
	}
	ident, err := IdentifyRepo(ctx, root)
	if err != nil {
		return SelfRepo{}, false, fmt.Errorf("reload: identify own checkout %s: %w", root, err)
	}
	return SelfRepo{Root: ident.Toplevel, CommonDir: ident.CommonDir}, true, nil
}

// checkoutContaining walks up from dir looking for the deploy script.
func checkoutContaining(dir string) (string, bool) {
	for {
		if info, err := os.Stat(filepath.Join(dir, filepath.FromSlash(DeployScriptRelPath))); err == nil && !info.IsDir() {
			return dir, true
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			return "", false
		}
		dir = parent
	}
}

// RepoIdentity is a worktree's repository identity: which repository it belongs
// to, and which worktree of that repository it is.
type RepoIdentity struct {
	// CommonDir is the canonical git common dir, shared by every linked
	// worktree of one repository.
	CommonDir string
	// Toplevel is the canonical root of this particular worktree.
	Toplevel string
}

// IdentifyRepo resolves dir's identity with a SINGLE git invocation. One
// invocation matters because this runs on the post-merge path of EVERY merge
// the daemon performs, the overwhelming majority of which are for some other
// repository entirely and must cost as little as possible.
//
// Every failure is an error and never a synthesized identity: a guessed
// identity could equate an unrelated repository with the daemon's own and
// redeploy the live stack off somebody else's merge.
func IdentifyRepo(ctx context.Context, dir string) (RepoIdentity, error) {
	if dir == "" {
		return RepoIdentity{}, fmt.Errorf("reload: RepoIdentity needs a worktree directory")
	}
	out, err := gitCapture(ctx, dir, "rev-parse", "--git-common-dir", "--show-toplevel")
	if err != nil {
		return RepoIdentity{}, err
	}
	lines := strings.Split(out, "\n")
	if len(lines) != 2 {
		return RepoIdentity{}, fmt.Errorf("reload: rev-parse --git-common-dir --show-toplevel in %s returned %d lines, want 2: %q", dir, len(lines), out)
	}
	common := strings.TrimSpace(lines[0])
	toplevel := strings.TrimSpace(lines[1])
	if common == "" || toplevel == "" {
		return RepoIdentity{}, fmt.Errorf("reload: rev-parse returned an empty identity for %s: %q", dir, out)
	}
	// The main worktree answers the relative ".git" for the common dir; a
	// linked worktree answers an absolute path.
	if !filepath.IsAbs(common) {
		common = filepath.Join(dir, common)
	}
	canonicalCommon, err := filepath.EvalSymlinks(common)
	if err != nil {
		return RepoIdentity{}, fmt.Errorf("reload: canonicalize git common dir %s for %s: %w", common, dir, err)
	}
	// EvalSymlinks is what makes the comparison against SelfRepo sound on
	// macOS, where /var and /private/var name the same directory: two spellings
	// of one path would otherwise read as two different repositories.
	canonicalTop, err := filepath.EvalSymlinks(toplevel)
	if err != nil {
		return RepoIdentity{}, fmt.Errorf("reload: canonicalize toplevel %s for %s: %w", toplevel, dir, err)
	}
	return RepoIdentity{
		CommonDir: filepath.Clean(canonicalCommon),
		Toplevel:  filepath.Clean(canonicalTop),
	}, nil
}
