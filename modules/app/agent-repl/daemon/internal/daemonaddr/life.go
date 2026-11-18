package daemonaddr

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"agent-repld/internal/inflight"
	"agent-repld/internal/recordlock"
)

// THE LIFE LOCKS (docs/investigations/2026-11-16-several-daemons-plan.md).
// Several daemons may serve one state root at once: the newest holds the
// boot claim, and each older one serves its busy workspaces until each is
// free to move. What only the LAST daemon may do -- a contract migration
// step, recovering shims no daemon owns -- waits until no older daemon is
// running, and that is a kernel fact: every daemon holds an exclusive record
// lock on its own file under daemons/ for its whole life, and the kernel
// frees it with its holder however the holder died (LiveOthers, AwaitGone).

// LifeDirName is the directory beside daemon.addr that holds every daemon's
// life lock.
const LifeDirName = "daemons"

// lifeSuffix ends every life lock's file name.
const lifeSuffix = ".lock"

// LifePath is the life lock of daemon INSTANCE for a given daemon.addr path.
func LifePath(addrPath, instance string) string {
	return filepath.Join(filepath.Dir(addrPath), LifeDirName, instance+lifeSuffix)
}

// addressSuffix ends the file in which a daemon records its private address.
const addressSuffix = ".addr"

// AddressPath is where daemon INSTANCE records its private listener's
// address, beside its life lock.
func AddressPath(addrPath, instance string) string {
	return filepath.Join(filepath.Dir(addrPath), LifeDirName, instance+addressSuffix)
}

// ErrNoInstanceAddress marks an instance that has recorded no private
// address: it never bound one, or it has ended.
var ErrNoInstanceAddress = errors.New("the daemon instance records no private address")

// InstanceAddress answers the private address daemon INSTANCE recorded, the
// one a call for a workspace it serves is forwarded to
// (docs/investigations/2026-11-18-stable-sockets-design.md, D1).
func InstanceAddress(addrPath, instance string) (string, error) {
	raw, err := os.ReadFile(AddressPath(addrPath, instance))
	if errors.Is(err, os.ErrNotExist) {
		return "", fmt.Errorf("instance %q: %w", instance, ErrNoInstanceAddress)
	}
	if err != nil {
		return "", fmt.Errorf("read instance %q's address: %w", instance, err)
	}
	address := strings.TrimSpace(string(raw))
	if address == "" {
		return "", fmt.Errorf("instance %q: an empty address file: %w", instance, ErrNoInstanceAddress)
	}
	return address, nil
}

// Life is a held life lock.
type Life struct {
	rec      *recordlock.Record
	addrPath string
	instance string
}

// RecordAddress records ADDRESS as this daemon's private listener, atomically
// (a temporary file, then a rename), so a reader sees no address or this one.
func (l *Life) RecordAddress(address string) error {
	path := AddressPath(l.addrPath, l.instance)
	tmp := path + ".tmp"
	if err := os.WriteFile(tmp, []byte(address+"\n"), 0o600); err != nil {
		return fmt.Errorf("write the instance address %q: %w", tmp, err)
	}
	if err := os.Rename(tmp, path); err != nil {
		return fmt.Errorf("publish the instance address %q: %w", path, err)
	}
	return nil
}

// HoldLife takes daemon INSTANCE's life lock for the state root of
// ADDRPATH. It creates the daemons/ directory, never the state root. A lock
// another process holds is a second daemon claiming this instance's identity,
// an error.
func HoldLife(addrPath, instance string) (*Life, error) {
	if strings.TrimSpace(instance) == "" || strings.ContainsAny(instance, `/\`) {
		return nil, fmt.Errorf("hold the life lock: %q names no instance", instance)
	}
	dir := filepath.Join(filepath.Dir(addrPath), LifeDirName)
	if err := os.Mkdir(dir, 0o755); err != nil && !errors.Is(err, os.ErrExist) {
		return nil, fmt.Errorf("make the life locks' directory %q: %w", dir, err)
	}
	path := LifePath(addrPath, instance)
	rec, held, err := recordlock.HoldIn(path)
	if err != nil {
		return nil, fmt.Errorf("hold the life lock %q: %w", path, err)
	}
	if !held {
		return nil, fmt.Errorf("hold the life lock %q: %w", path, ErrClaimed)
	}
	return &Life{rec: rec, addrPath: addrPath, instance: instance}, nil
}

// Release drops the life lock and removes its file: this daemon is ending.
func (l *Life) Release() error {
	if err := os.Remove(AddressPath(l.addrPath, l.instance)); err != nil && !errors.Is(err, os.ErrNotExist) {
		return fmt.Errorf("remove the instance address: %w", err)
	}
	path := l.rec.Path()
	if err := os.Remove(path); err != nil && !errors.Is(err, os.ErrNotExist) {
		return fmt.Errorf("remove the life lock %q: %w", path, err)
	}
	return l.rec.Release()
}

// OtherLives lists the instances, other than SELF, whose life lock files
// stand beside ADDRPATH now, sorted. A file stands until its holder releases
// it, or until a daemon that outlived it removes it (LiveOthers, AwaitGone).
func OtherLives(addrPath, self string) ([]string, error) {
	dir := filepath.Join(filepath.Dir(addrPath), LifeDirName)
	entries, err := os.ReadDir(dir)
	if errors.Is(err, os.ErrNotExist) {
		return nil, nil
	}
	if err != nil {
		return nil, fmt.Errorf("list the life locks in %q: %w", dir, err)
	}
	var out []string
	for _, e := range entries {
		name, ok := strings.CutSuffix(e.Name(), lifeSuffix)
		if !ok || e.IsDir() || name == self {
			continue
		}
		out = append(out, name)
	}
	sort.Strings(out)
	return out, nil
}

// LiveOthers answers the instances, other than SELF, that are running now:
// each whose life lock beside ADDRPATH another process holds, sorted. A
// leftover lock nobody holds -- a daemon that died without removing its file
// -- is removed, never answered. A lock that cannot be probed is an error,
// never read as free.
func LiveOthers(addrPath, self string) ([]string, error) {
	others, err := OtherLives(addrPath, self)
	if err != nil {
		return nil, err
	}
	var live []string
	for _, instance := range others {
		path := LifePath(addrPath, instance)
		rec, _, err := recordlock.TryExclusive(path)
		switch {
		case errors.Is(err, recordlock.ErrWouldBlock):
			live = append(live, instance)
			continue
		case err != nil:
			return nil, fmt.Errorf("probe the life lock %q: %w", path, err)
		}
		if err := os.Remove(AddressPath(addrPath, instance)); err != nil && !errors.Is(err, os.ErrNotExist) {
			rec.Release()
			return nil, fmt.Errorf("remove the leftover instance address of %q: %w", instance, err)
		}
		if err := os.Remove(path); err != nil && !errors.Is(err, os.ErrNotExist) {
			rec.Release()
			return nil, fmt.Errorf("remove the leftover life lock %q: %w", path, err)
		}
		if err := rec.Release(); err != nil {
			return nil, fmt.Errorf("release the leftover life lock %q: %w", path, err)
		}
	}
	return live, nil
}

// Alive answers whether daemon INSTANCE runs on the state root of ADDRPATH:
// another process holds its life lock. An instance with no lock file is not
// running. A lock that cannot be probed is an error, never read as free.
func Alive(addrPath, instance string) (bool, error) {
	path := LifePath(addrPath, instance)
	if _, err := os.Stat(path); errors.Is(err, os.ErrNotExist) {
		return false, nil
	} else if err != nil {
		return false, fmt.Errorf("stat the life lock %q: %w", path, err)
	}
	rec, _, err := recordlock.TryExclusive(path)
	switch {
	case errors.Is(err, recordlock.ErrWouldBlock):
		return true, nil
	case err != nil:
		return false, fmt.Errorf("probe the life lock %q: %w", path, err)
	}
	if err := rec.Release(); err != nil {
		return false, fmt.Errorf("release the probed life lock %q: %w", path, err)
	}
	return false, nil
}

// AwaitGone returns once each of INSTANCES, daemons beside ADDRPATH, has
// ended: it waits in the kernel for each one's life lock, never polling, and
// removes each freed lock's file. A lock that cannot be waited for is an
// error, never read as its holder gone. CTX ends the wait for the caller; a
// kernel wait already under way ends with its holder.
func AwaitGone(ctx context.Context, addrPath string, instances []string) error {
	done := make(chan error, len(instances))
	for _, instance := range instances {
		path := LifePath(addrPath, instance)
		f, err := recordlock.OpenFile(path)
		if err != nil {
			return fmt.Errorf("open the life lock %q: %w", path, err)
		}
		inflight.Standing(func() {
			rec, err := recordlock.BlockExclusive(f, path)
			if err != nil {
				done <- fmt.Errorf("wait for the life lock %q: %w", path, err)
				return
			}
			if err := os.Remove(path); err != nil && !errors.Is(err, os.ErrNotExist) {
				rec.Release()
				done <- fmt.Errorf("remove the freed life lock %q: %w", path, err)
				return
			}
			done <- rec.Release()
		})
	}
	var errs []error
	for range instances {
		select {
		case err := <-done:
			if err != nil {
				errs = append(errs, err)
			}
		case <-ctx.Done():
			return fmt.Errorf("await the older daemons beside %q: %w", addrPath, ctx.Err())
		}
	}
	return errors.Join(errs...)
}
