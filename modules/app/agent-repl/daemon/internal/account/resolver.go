package account

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"

	"golang.org/x/text/unicode/norm"

	"claude-repld/internal/dlog"
)

// identityFile is the vendor CLI's own name for the file that records the
// signed-in account, in every config root.
//
// The identity lives in .claude.json, not in the credential store: the
// credentials sit in the OS keychain, but the CLI records the human-readable
// account beside them.
const identityFile = ".claude.json"

// resolver is the Resolver.
type resolver struct {
	roots Roots
	log   dlog.Logger
}

// newResolver validates the roots and builds the resolver.
func newResolver(roots Roots, log dlog.Logger) (*resolver, error) {
	if log == nil {
		return nil, errors.New("account: a logger is required")
	}
	if roots.Default == "" {
		return nil, errors.New("account: Roots.Default is required (the account is determined, so every workspace must have an answer)")
	}
	if roots.MultiRepo == "" {
		return nil, errors.New("account: Roots.MultiRepo is required (the account is determined, so every workspace must have an answer)")
	}
	r := &resolver{roots: roots, log: log}
	r.log.Debug("daemon.account.new", "account resolver built", dlog.Context{
		"default_root":    roots.Default,
		"multi_repo_root": roots.MultiRepo,
		"path_prefix":     roots.MultiRepoRoot,
	})
	return r, nil
}

// ConfigDirFor implements Resolver.
//
// THE PATH IS THE ONLY INPUT. An empty MultiRepoRoot selects Default for
// everything, which is the honest reading of an unset $MULTI_REPO_ROOT: no
// path can be under a root that was never named.
func (r *resolver) ConfigDirFor(workspaceDir string) string {
	if r.roots.MultiRepoRoot == "" {
		r.log.Debug("daemon.account.config_dir_for", "no multi-repo root configured; routing to the default account", dlog.Context{
			"workspace_dir": workspaceDir,
			"config_dir":    r.roots.Default,
			"branch":        "no-root",
		})
		return r.roots.Default
	}

	dir := r.canonical(workspaceDir, "config_dir_for.workspace_dir")
	root := r.canonical(r.roots.MultiRepoRoot, "config_dir_for.multi_repo_root")

	if r.underDir(root, dir) {
		r.log.Debug("daemon.account.config_dir_for", "workspace is under the multi-repo root", dlog.Context{
			"workspace_dir":   workspaceDir,
			"resolved_dir":    dir,
			"multi_repo_root": root,
			"config_dir":      r.roots.MultiRepo,
			"branch":          "multi-repo",
		})
		return r.roots.MultiRepo
	}
	r.log.Debug("daemon.account.config_dir_for", "workspace is outside the multi-repo root", dlog.Context{
		"workspace_dir":   workspaceDir,
		"resolved_dir":    dir,
		"multi_repo_root": root,
		"config_dir":      r.roots.Default,
		"branch":          "default",
	})
	return r.roots.Default
}

// IsMultiRepo implements Resolver.
//
// THE POLICY LIVES HERE, not at the call site: a caller hands the config dir a
// session spends under and this package alone says whether it is the work
// (multi-repo) account. A single root configured for both accounts is one
// account with no distinct work root, so it answers false — there is nothing
// to distinguish. Identity is by inode where both dirs exist, so a
// differently-cased or differently-normalized spelling of the same root still
// matches; a normalized string compare is the fallback when either cannot be
// stat'd.
func (r *resolver) IsMultiRepo(configDir string) bool {
	if configDir == "" {
		return false
	}
	if r.roots.Default == r.roots.MultiRepo {
		return false
	}
	ca := r.canonical(configDir, "is_multi_repo.config_dir")
	cb := r.canonical(r.roots.MultiRepo, "is_multi_repo.multi_repo")
	if ai, err := os.Stat(ca); err == nil {
		if bi, err := os.Stat(cb); err == nil {
			if os.SameFile(ai, bi) {
				return true
			}
		}
	}
	return normalizePathForCompare(ca) == normalizePathForCompare(cb)
}

// canonical cleans and absolutizes a path and resolves its symlinks, so a
// workspace reached through a symlinked path routes the same as the path it
// points at.
//
// A path that does not exist (or a link that cannot be read) still gets an
// answer: routing must work for a workspace directory the daemon is about to
// CREATE. The cleaned absolute path is the fallback, and the fall-through is
// logged rather than swallowed.
func (r *resolver) canonical(path, site string) string {
	abs, err := filepath.Abs(filepath.Clean(path))
	if err != nil {
		r.log.Warn("daemon.account.canonical", "could not absolutize a path; using it as given", dlog.Context{
			"site":  site,
			"path":  path,
			"error": err.Error(),
		})
		abs = filepath.Clean(path)
	}
	resolved, err := filepath.EvalSymlinks(abs)
	if err != nil {
		r.log.Debug("daemon.account.canonical", "could not resolve symlinks; using the cleaned absolute path", dlog.Context{
			"site":  site,
			"path":  path,
			"abs":   abs,
			"error": err.Error(),
		})
		return abs
	}
	return resolved
}

// underDir reports whether dir lies at or under root.
//
// BREADCRUMB (account routing regression fix): this test used to be a
// byte-wise, case-sensitive `strings.HasPrefix` on the two canonicalized
// paths. That mis-routed a workspace opened through a differently-cased or
// differently-normalized path on a case-insensitive, normalization-insensitive
// filesystem (APFS on macOS): `EvalSymlinks` does NOT fold case, so a real,
// valid workspace path like `.../chesscom/...` failed `HasPrefix` against a
// root recorded as `.../ChessCom/`, routing a WORK workspace to the PERSONAL
// account. The fix asks the filesystem for identity instead of comparing
// bytes: for the parts of `dir` that EXIST it walks upward and compares each
// ancestor to root by inode with `os.SameFile`, which is immune to case and
// normalization skew because both spellings resolve to the same inode. The
// per-SEGMENT string check survives only as a fallback for the still-missing
// leading segments of a path the daemon is ABOUT TO CREATE, and even that
// fallback is now case-folded and NFC-normalized so it too survives the skew
// that the old byte compare did not.
//
// The comparison stays per path SEGMENT, not per byte: `/home/user/multi-other`
// is not under `/home/user/multi`, and a root that merely CONTAINS a
// repository (`/home/user` asked about `/home`) is not under it either.
func (r *resolver) underDir(root, dir string) bool {
	root = strings.TrimSuffix(filepath.Clean(root), string(filepath.Separator))
	dir = strings.TrimSuffix(filepath.Clean(dir), string(filepath.Separator))
	if root == "" || dir == "" {
		return false
	}

	// Inode identity for the parts that exist. If root itself cannot be
	// stat'd, there is no inode to compare against, so the normalized string
	// check is the only answer available.
	rootInfo, err := os.Stat(root)
	if err != nil {
		if !errors.Is(err, fs.ErrNotExist) {
			r.log.Warn("daemon.account.under_dir", "could not stat the multi-repo root; falling back to a normalized path check", dlog.Context{
				"root":  root,
				"dir":   dir,
				"error": err.Error(),
			})
		}
		return underDirNormalized(root, dir)
	}

	// Walk dir upward through its ancestors. The first EXISTING ancestor that
	// is the same file as root proves dir is at or under root — this is what
	// makes a differently-cased or differently-normalized spelling of a real
	// directory route correctly. A missing ancestor is expected (an
	// about-to-be-created workspace) and is the trigger to keep climbing, not
	// an error to surface.
	for p := dir; ; {
		info, err := os.Stat(p)
		switch {
		case err == nil:
			if os.SameFile(rootInfo, info) {
				return true
			}
		case !errors.Is(err, fs.ErrNotExist):
			r.log.Warn("daemon.account.under_dir", "could not stat a workspace-path ancestor; treating it as missing", dlog.Context{
				"root":     root,
				"dir":      dir,
				"ancestor": p,
				"error":    err.Error(),
			})
		}
		parent := filepath.Dir(p)
		if parent == p {
			break // reached the volume root; nothing above it to compare
		}
		p = parent
	}

	// No existing ancestor shares root's inode. Either dir is genuinely
	// outside root, or its still-missing leading segments name a path under a
	// root that itself exists but was not reached by an inode match (only when
	// none of dir's existing ancestors is root). Decide with a normalized
	// prefix check so the fallback survives case and normalization skew too.
	return underDirNormalized(root, dir)
}

// underDirNormalized is the string fallback for path segments that do not yet
// exist on disk, so cannot be compared by inode. It is case-folded and
// NFC-normalized so a differently-cased or differently-composed spelling of an
// about-to-be-created path still routes like the path it names, unlike the
// byte-wise compare this replaced. Segment boundaries are still honored, so a
// sibling whose name merely starts with the root's is not under it.
func underDirNormalized(root, dir string) bool {
	root = normalizePathForCompare(root)
	dir = normalizePathForCompare(dir)
	if root == "" || dir == "" {
		return false
	}
	if dir == root {
		return true
	}
	return strings.HasPrefix(dir, root+string(filepath.Separator))
}

// normalizePathForCompare folds case and applies Unicode NFC so two spellings
// of the same path compare equal. Separators are unaffected by either, so
// normalizing the whole string keeps the per-segment prefix check intact.
func normalizePathForCompare(p string) string {
	return strings.ToLower(norm.NFC.String(p))
}

// Read implements Resolver.
//
// A root that has never been logged into yields an empty Email, LoggedIn
// false and NO error: logged out is a state the topbar draws, not a failure
// to report. A root whose identity file exists but cannot be parsed IS an
// error — that is a corrupt install, and reporting it as logged out would send
// the user off to fix the wrong problem.
func (r *resolver) Read(ctx context.Context, configDir string) (Account, error) {
	if err := ctx.Err(); err != nil {
		return Account{}, err
	}
	if configDir == "" {
		err := errors.New("account: Read requires a config dir")
		r.log.Error("daemon.account.read", "account read rejected", dlog.Context{
			"branch": "empty-config-dir",
			"error":  err.Error(),
		})
		return Account{}, err
	}

	acct := Account{ConfigDir: configDir}
	path := filepath.Join(configDir, identityFile)

	raw, err := os.ReadFile(path) //nolint:gosec // daemon-derived path, never client input
	if err != nil {
		if errors.Is(err, fs.ErrNotExist) {
			r.log.Debug("daemon.account.read", "config root has no identity file; logged out", dlog.Context{
				"config_dir": configDir,
				"path":       path,
				"branch":     "absent",
			})
			return acct, nil
		}
		wrapped := fmt.Errorf("account: reading %s: %w", path, err)
		r.log.Error("daemon.account.read", "account identity file unreadable", dlog.Context{
			"config_dir": configDir,
			"path":       path,
			"branch":     "read-error",
			"error":      wrapped.Error(),
		})
		return acct, wrapped
	}

	// Decode ONLY the account block. .claude.json is a large, evolving vendor
	// document holding plenty the daemon has no business parsing, and a struct
	// naming more of it would break every time the CLI adds a field.
	var doc struct {
		OAuthAccount struct {
			EmailAddress string `json:"emailAddress"`
		} `json:"oauthAccount"`
	}
	if err := json.Unmarshal(raw, &doc); err != nil {
		wrapped := fmt.Errorf("account: parsing %s: %w", path, err)
		r.log.Error("daemon.account.read", "account identity file is malformed", dlog.Context{
			"config_dir": configDir,
			"path":       path,
			"branch":     "malformed",
			"error":      wrapped.Error(),
		})
		return acct, wrapped
	}

	acct.Email = doc.OAuthAccount.EmailAddress
	acct.LoggedIn = acct.Email != ""
	r.log.Debug("daemon.account.read", "account identity read", dlog.Context{
		"config_dir": configDir,
		"path":       path,
		"logged_in":  acct.LoggedIn,
		"branch":     "read",
	})
	return acct, nil
}

// Roster implements Resolver: both roots, default first. A root that fails to
// read fails the whole roster — a partial roster would draw one account as
// absent when it is merely unreadable.
func (r *resolver) Roster(ctx context.Context) ([]Account, error) {
	out := make([]Account, 0, 2)
	for _, dir := range r.rosterRoots() {
		acct, err := r.Read(ctx, dir)
		if err != nil {
			r.log.Error("daemon.account.roster", "account roster failed", dlog.Context{
				"config_dir": dir,
				"branch":     "read-error",
				"error":      err.Error(),
			})
			return nil, err
		}
		out = append(out, acct)
	}
	r.log.Debug("daemon.account.roster", "account roster resolved", dlog.Context{
		"count":  len(out),
		"branch": "resolved",
	})
	return out, nil
}

// rosterRoots is the two roots, deduplicated: one root configured twice is one
// account, not two rows drawing the same address.
func (r *resolver) rosterRoots() []string {
	if r.roots.Default == r.roots.MultiRepo {
		return []string{r.roots.Default}
	}
	return []string{r.roots.Default, r.roots.MultiRepo}
}
