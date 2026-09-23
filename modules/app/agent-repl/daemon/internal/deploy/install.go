package deploy

import (
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"claude-repld/internal/dlog"
)

const opInstall = "daemon.deploy.install"

// Live names the INSTALLED artifacts: where every process is spawned from.
type Live struct {
	// ModuleRoot is the agent-repl module root the daemon was deployed from.
	ModuleRoot string
	// CacheBin is ~/.cache/agent-repl/bin, where launchd runs the services
	// from.
	CacheBin string
}

// ShimMain is the installed shim bundle.
func (l Live) ShimMain() string {
	return filepath.Join(l.ModuleRoot, "agent-shim", "claude", "shim", "dist", "main.js")
}

// WebappDist is the installed webapp dist the daemon serves.
func (l Live) WebappDist() string { return filepath.Join(l.ModuleRoot, "webapp", "dist") }

// DaemonBin is the installed daemon binary a successor is spawned from.
func (l Live) DaemonBin() string { return filepath.Join(l.ModuleRoot, "daemon", "bin", "claude-repld") }

// CacheBinPath is an installed cache-bin artifact.
func (l Live) CacheBinPath(name string) string { return filepath.Join(l.CacheBin, name) }

// InstallFailed is a staged artifact that could not be installed. Nothing was
// restarted.
type InstallFailed struct {
	Component Component
	Detail    string
}

func (e *InstallFailed) Error() string {
	return fmt.Sprintf("deploy: install the %s build: %s", e.Component, e.Detail)
}

// installer copies staged artifacts over the live ones, each ATOMICALLY: the
// bytes land beside the destination under a temporary name and are renamed
// into place, so a process spawned at any instant runs either the old
// artifact or the new one, never a half-written file. The staging directory
// may be on another filesystem, which is why it is a copy and a rename rather
// than a rename alone.
type installer struct {
	nonce string
	log   dlog.Logger
}

// file installs one staged file over a live one, with the stamps that sit
// beside it (named relative to each directory).
func (i installer) file(staged, live string, stamps map[string]string) error {
	if err := i.copyInto(staged, live); err != nil {
		return err
	}
	for from, to := range stamps {
		if err := i.copyInto(from, to); err != nil {
			if errors.Is(err, fs.ErrNotExist) {
				// A STAMP THE BUILD DID NOT WRITE is the readiness report's to
				// notice, not a reason to refuse the artifact it describes.
				i.log.Warn(opInstall, "a build stamp was not staged; the installed stamp is left as it was",
					dlog.Context{"stamp": from, "cause": err.Error()})
				continue
			}
			return err
		}
	}
	return nil
}

// copyInto copies src to dst through a temporary sibling of dst and a rename.
func (i installer) copyInto(src, dst string) error {
	info, err := os.Stat(src)
	if err != nil {
		return fmt.Errorf("stat the staged %s: %w", src, err)
	}
	if err := os.MkdirAll(filepath.Dir(dst), 0o755); err != nil {
		return fmt.Errorf("create %s: %w", filepath.Dir(dst), err)
	}
	tmp := filepath.Join(filepath.Dir(dst), "."+filepath.Base(dst)+".install-"+i.nonce)
	if err := copyFile(src, tmp, info.Mode().Perm()); err != nil {
		return errors.Join(err, removeIfPresent(tmp))
	}
	if err := os.Rename(tmp, dst); err != nil {
		return errors.Join(fmt.Errorf("install %s: %w", dst, err), removeIfPresent(tmp))
	}
	return nil
}

// dir installs a staged directory over a live one: the new tree is copied in
// beside it, then the two are swapped by rename, then the old one is removed.
// Between the two renames the live path is briefly absent; the daemon's asset
// origin re-stats the entry per request, so the next request finds the new
// tree.
func (i installer) dir(staged, live string) error {
	incoming := filepath.Join(filepath.Dir(live), "."+filepath.Base(live)+".install-"+i.nonce)
	retired := filepath.Join(filepath.Dir(live), "."+filepath.Base(live)+".retired-"+i.nonce)
	if err := copyTree(staged, incoming); err != nil {
		return errors.Join(err, os.RemoveAll(incoming))
	}
	hadLive := true
	if err := os.Rename(live, retired); err != nil {
		if !errors.Is(err, fs.ErrNotExist) {
			return errors.Join(fmt.Errorf("retire %s: %w", live, err), os.RemoveAll(incoming))
		}
		hadLive = false
	}
	if err := os.Rename(incoming, live); err != nil {
		restore := error(nil)
		if hadLive {
			restore = os.Rename(retired, live)
		}
		return errors.Join(fmt.Errorf("install %s: %w", live, err), restore, os.RemoveAll(incoming))
	}
	if hadLive {
		if err := os.RemoveAll(retired); err != nil {
			i.log.Error(opInstall, "the retired tree could not be removed; it is left beside the installed one",
				dlog.Context{"retired": retired, "cause": err.Error()})
		}
	}
	return nil
}

func copyFile(src, dst string, perm fs.FileMode) error {
	in, err := os.Open(src)
	if err != nil {
		return fmt.Errorf("open %s: %w", src, err)
	}
	out, err := os.OpenFile(dst, os.O_CREATE|os.O_TRUNC|os.O_WRONLY, perm)
	if err != nil {
		return errors.Join(fmt.Errorf("create %s: %w", dst, err), in.Close())
	}
	_, copyErr := io.Copy(out, in)
	syncErr := out.Sync()
	closeOut := out.Close()
	closeIn := in.Close()
	if copyErr != nil {
		return errors.Join(fmt.Errorf("copy %s to %s: %w", src, dst, copyErr), closeOut, closeIn)
	}
	if err := errors.Join(syncErr, closeOut, closeIn); err != nil {
		return fmt.Errorf("finish %s: %w", dst, err)
	}
	return nil
}

func copyTree(src, dst string) error {
	return filepath.WalkDir(src, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return fmt.Errorf("walk %s: %w", path, err)
		}
		rel, err := filepath.Rel(src, path)
		if err != nil {
			return fmt.Errorf("relativize %s: %w", path, err)
		}
		target := filepath.Join(dst, rel)
		if d.IsDir() {
			if err := os.MkdirAll(target, 0o755); err != nil {
				return fmt.Errorf("create %s: %w", target, err)
			}
			return nil
		}
		info, err := d.Info()
		if err != nil {
			return fmt.Errorf("stat %s: %w", path, err)
		}
		return copyFile(path, target, info.Mode().Perm())
	})
}

func removeIfPresent(path string) error {
	if err := os.Remove(path); err != nil && !errors.Is(err, fs.ErrNotExist) {
		return fmt.Errorf("remove %s: %w", path, err)
	}
	return nil
}

// nonceOf renders a unique suffix for one deploy's temporaries.
func nonceOf(n int64) string { return strconv.FormatInt(n, 36) }

// stampsBeside answers the build stamps build-frontend writes beside an
// artifact — `.built-sha` and `.source-tree` in its directory for the shim and
// the daemon, `.<name>.built-sha` and `.<name>.source-tree` in cache-bin —
// mapped from their staged path to their live one.
func stampsBeside(stagedDir, liveDir, prefix string) map[string]string {
	out := map[string]string{}
	for _, suffix := range []string{"built-sha", "source-tree"} {
		name := "." + strings.TrimPrefix(prefix+suffix, ".")
		out[filepath.Join(stagedDir, name)] = filepath.Join(liveDir, name)
	}
	return out
}
