package deploy

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"claude-repld/internal/dlog"
)

// A FAILED DEPLOY ROLLS BACK (owner ruling, 2026-09-28). Before the install
// replaces anything, every live path it is about to write is KEPT: the
// daemon, the shim bundle and its stamps, the webapp dist, and the store,
// sidecar and lock binaries with their stamps in the cache bin. When any step
// after the install began fails — the install itself, a service restart, the
// daemon's handover or restart being refused — the kept paths are put back
// and every service the deploy restarted (or tried to) is restarted onto the
// restored build. The running daemon never handed over, so it keeps serving,
// and the host is left exactly on the build it ran before.
//
// Elisp is NOT an installed artifact: Emacs loads it from the checkout, and
// the deploy writes no elisp file or stamp, so there is nothing of it to keep.
//
// A ROLLBACK THAT FAILS IS ITS OWN FAULT: the host may then run neither build
// cleanly, and that stands on every strip until a deploy gets all the way
// through (faults.go).

// opRollback is the operation every rollback record is made under.
const opRollback = "daemon.deploy.rollback"

// kept is one live path an install was about to write, and where the bytes
// that stood there were kept.
type kept struct {
	component Component
	// artifact names the artifact the path belongs to, for the records.
	artifact string
	live     string
	// backup is where the previous bytes are; empty when nothing stood at
	// live, in which case restoring removes whatever the install put there.
	backup string
	// tree is a directory (the webapp dist) rather than a file.
	tree bool
	// bundle is the shim bundle, restored only while no spawn holds it.
	bundle bool
}

// previous is what one deploy's install replaced, in install order, and the
// services the deploy restarted: the previous build a rollback restores.
type previous struct {
	root string
	kept []kept
	// store and sidecar record the services the deploy ASKED to restart onto
	// the fresh build. A restart that failed is included: it may have left
	// the service running the fresh binary, so the rollback restarts it too.
	store, sidecar bool
}

// touched reports whether the deploy changed anything a rollback restores.
func (p *previous) touched() bool {
	return len(p.kept) > 0 || p.store || p.sidecar
}

// keep copies what stands at one live path aside before an install writes it.
func (p *previous) keep(component Component, artifact, live string, tree, bundle bool) error {
	k := kept{component: component, artifact: artifact, live: live, tree: tree, bundle: bundle}
	info, err := os.Stat(live)
	switch {
	case errors.Is(err, fs.ErrNotExist):
		p.kept = append(p.kept, k)
		return nil
	case err != nil:
		return fmt.Errorf("keep the previous %s: stat %s: %w", artifact, live, err)
	}
	k.backup = filepath.Join(p.root, strconv.Itoa(len(p.kept)), filepath.Base(live))
	if err := os.MkdirAll(filepath.Dir(k.backup), 0o755); err != nil {
		return fmt.Errorf("keep the previous %s: %w", artifact, err)
	}
	if tree {
		err = copyTree(live, k.backup)
	} else {
		err = copyFile(live, k.backup, info.Mode().Perm())
	}
	if err != nil {
		return fmt.Errorf("keep the previous %s: %w", artifact, err)
	}
	p.kept = append(p.kept, k)
	return nil
}

// RollbackFailed is a failed deploy whose rollback did not restore the
// previous build: a kept artifact could not be put back, or a service could
// not be restarted onto it. Component is the first that failed; Detail
// names every failure.
type RollbackFailed struct {
	Component Component
	Detail    string
}

func (e *RollbackFailed) Error() string {
	return fmt.Sprintf("deploy: roll back the %s: %s", e.Component, e.Detail)
}

// rollback restores every kept path, newest first, then restarts every
// service the deploy restarted onto the restored build. It does not stop at a
// failure: every path it can restore is restored, and every failure is named.
func (d *Deployer) rollback(ctx context.Context, p *previous, nonce string, cause error) error {
	ctx = context.WithoutCancel(ctx)
	fields := dlog.Context{"cause": cause.Error(), "kept": len(p.kept), "store": p.store, "sidecar": p.sidecar}
	d.log.Info(opRollback, "the deploy failed after it began installing; rolling back to the previous build", fields)
	in := installer{nonce: nonce + "-rollback", log: d.log}
	var (
		first    Component
		failures []string
	)
	fail := func(component Component, what string, err error) {
		if first == "" {
			first = component
		}
		failures = append(failures, what+": "+err.Error())
	}
	for i := len(p.kept) - 1; i >= 0; i-- {
		k := p.kept[i]
		kFields := dlog.Context{"artifact": k.artifact, "live": k.live, "had_previous": k.backup != ""}
		restore := func() error { return d.restoreKept(in, k, kFields) }
		var err error
		if k.bundle {
			// THE BUNDLE IS PUT BACK ONLY WHILE NO SPAWN HOLDS IT, exactly as
			// the install replaced it.
			err = d.deps.Bundle.Replace(restore)
		} else {
			err = restore()
		}
		if err != nil {
			d.log.Error(opRollback, "could not restore a previous artifact", withCause(kFields, err))
			fail(k.component, "restore "+k.live, err)
		}
	}
	switch {
	case p.store:
		// A STORE RESTART ALWAYS RESTARTS THE SIDECAR, in the recorded safe
		// order, so one restart puts both back onto the restored build.
		if err := d.deps.Services.RestartStore(ctx); err != nil {
			d.log.Error(opRollback, "could not restart the store and the sidecar onto the restored build", withCause(nil, err))
			fail(ComponentStore, "restart the store", err)
		} else {
			d.log.Info(opRollback, "restarted the store and the sidecar onto the restored build", nil)
		}
	case p.sidecar:
		if err := d.deps.Services.RestartSidecar(ctx); err != nil {
			d.log.Error(opRollback, "could not restart the sidecar onto the restored build", withCause(nil, err))
			fail(ComponentSidecar, "restart the sidecar", err)
		} else {
			d.log.Info(opRollback, "restarted the sidecar onto the restored build", nil)
		}
	}
	if len(failures) > 0 {
		failed := &RollbackFailed{Component: first, Detail: strings.Join(failures, "; ")}
		d.log.Error(opRollback, "the rollback did NOT restore the previous build", merge(fields, dlog.Context{
			"component": string(failed.Component), "detail": failed.Detail,
		}))
		return failed
	}
	d.log.Info(opRollback, "rolled back: the previous build is installed and running", fields)
	return nil
}

// restoreKept puts one kept path back. A path that already holds the previous
// bytes — the install never reached it, or failed before its rename — is left
// as it is, so a destination the install could not write is not written now.
func (d *Deployer) restoreKept(in installer, k kept, fields dlog.Context) error {
	same, err := k.standsAsKept()
	if err != nil {
		return err
	}
	if same {
		d.log.Debug(opRollback, "the path already holds the previous build; nothing to restore", fields)
		return nil
	}
	switch {
	case k.backup == "":
		if err := os.RemoveAll(k.live); err != nil {
			return fmt.Errorf("remove what the install put at %s: %w", k.live, err)
		}
		d.log.Info(opRollback, "removed what the install put where nothing stood before", fields)
	case k.tree:
		if err := in.dir(k.backup, k.live); err != nil {
			return err
		}
		d.log.Info(opRollback, "restored the previous artifact", fields)
	default:
		if err := in.copyInto(k.backup, k.live); err != nil {
			return err
		}
		d.log.Info(opRollback, "restored the previous artifact", fields)
	}
	return nil
}

// standsAsKept reports whether the live path already holds exactly what was
// kept: absent when nothing was, the same bytes when something was.
func (k kept) standsAsKept() (bool, error) {
	_, err := os.Lstat(k.live)
	absent := errors.Is(err, fs.ErrNotExist)
	if err != nil && !absent {
		return false, fmt.Errorf("stat %s: %w", k.live, err)
	}
	switch {
	case k.backup == "":
		return absent, nil
	case absent:
		return false, nil
	case k.tree:
		return sameTree(k.backup, k.live)
	default:
		return sameFile(k.backup, k.live)
	}
}

// sameFile reports whether two files hold the same bytes.
func sameFile(a, b string) (bool, error) {
	left, err := os.ReadFile(a)
	if err != nil {
		return false, fmt.Errorf("read %s: %w", a, err)
	}
	right, err := os.ReadFile(b)
	if err != nil {
		return false, fmt.Errorf("read %s: %w", b, err)
	}
	return bytes.Equal(left, right), nil
}

// sameTree reports whether two directory trees hold the same files with the
// same bytes.
func sameTree(a, b string) (bool, error) {
	left, err := treeFiles(a)
	if err != nil {
		return false, err
	}
	right, err := treeFiles(b)
	if err != nil {
		return false, err
	}
	if len(left) != len(right) {
		return false, nil
	}
	for rel := range left {
		if _, ok := right[rel]; !ok {
			return false, nil
		}
		same, err := sameFile(filepath.Join(a, rel), filepath.Join(b, rel))
		if err != nil || !same {
			return false, err
		}
	}
	return true, nil
}

// treeFiles lists a tree's regular files, relative to its root.
func treeFiles(root string) (map[string]struct{}, error) {
	out := map[string]struct{}{}
	err := filepath.WalkDir(root, func(path string, entry fs.DirEntry, err error) error {
		if err != nil {
			return fmt.Errorf("walk %s: %w", path, err)
		}
		if entry.IsDir() {
			return nil
		}
		rel, err := filepath.Rel(root, path)
		if err != nil {
			return fmt.Errorf("relativize %s: %w", path, err)
		}
		out[rel] = struct{}{}
		return nil
	})
	return out, err
}
