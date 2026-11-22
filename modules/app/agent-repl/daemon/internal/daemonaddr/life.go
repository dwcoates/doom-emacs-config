package daemonaddr

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strconv"
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
//
// A LIFE LOCK THAT STANDS WITH NO HOLDER IS A DEATH. A daemon's file is its
// own: it appears only already locked and naming its pid (HoldLife), nothing
// is shared across a yield or a handover, Emacs never touches it, and an
// orderly end REMOVES THE FILE BEFORE IT RELEASES THE LOCK (Life.Release).
// So a reaper that takes a life lock and finds the very file it locked still
// standing at its path knows, by kernel fact and with no dial, that its
// holder ended without Release: panicked, SIGKILLed, or killed with its
// session. Every reaper (LiveOthers, AwaitGone) reports each such Death,
// and removes the file before releasing the lock, so no second reaper can
// find it: each death is reported exactly once. The caller records it
// (daemon.boot.predecessor_died) and owes the deploy mediator its check.

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

// AddressSuffix ends each instance's address file under LifeDirName.
const AddressSuffix = addressSuffix

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
	pid      int
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
// file that already stands, or a pending one another holder has, is a second
// daemon claiming this instance's identity, an error.
//
// THE FILE APPEARS ONLY ALREADY LOCKED AND NAMING THIS PROCESS'S PID: it is
// made, locked and written under a pending name no reaper lists, then linked
// to its own. A reaper that listed it in the instant between a plain create
// and the lock would otherwise have taken it unheld and reported a death.
func HoldLife(addrPath, instance string) (*Life, error) {
	if strings.TrimSpace(instance) == "" || strings.ContainsAny(instance, `/\`) {
		return nil, fmt.Errorf("hold the life lock: %q names no instance", instance)
	}
	dir := filepath.Join(filepath.Dir(addrPath), LifeDirName)
	if err := os.Mkdir(dir, 0o755); err != nil && !errors.Is(err, os.ErrExist) {
		return nil, fmt.Errorf("make the life locks' directory %q: %w", dir, err)
	}
	path := LifePath(addrPath, instance)
	pending := path + pendingSuffix
	rec, held, err := recordlock.HoldIn(pending)
	if err != nil {
		return nil, fmt.Errorf("hold the life lock %q: %w", pending, err)
	}
	if !held {
		return nil, fmt.Errorf("hold the life lock %q: %w", pending, ErrClaimed)
	}
	abandon := func(cause error) error {
		errs := []error{cause}
		if err := os.Remove(pending); err != nil && !errors.Is(err, os.ErrNotExist) {
			errs = append(errs, fmt.Errorf("remove the pending life lock %q: %w", pending, err))
		}
		if err := rec.Release(); err != nil {
			errs = append(errs, fmt.Errorf("release the pending life lock %q: %w", pending, err))
		}
		return errors.Join(errs...)
	}
	pid := os.Getpid()
	if err := rec.Overwrite(lifeContent(pid, false)); err != nil {
		return nil, abandon(fmt.Errorf("write this daemon's pid into its life lock %q: %w", pending, err))
	}
	if err := os.Link(pending, path); err != nil {
		if errors.Is(err, os.ErrExist) {
			return nil, abandon(fmt.Errorf("hold the life lock %q: %w", path, ErrClaimed))
		}
		return nil, abandon(fmt.Errorf("publish the life lock %q: %w", path, err))
	}
	life := &Life{rec: rec, addrPath: addrPath, instance: instance, pid: pid}
	if err := os.Remove(pending); err != nil {
		return nil, errors.Join(fmt.Errorf("remove the life lock's pending name %q: %w", pending, err), life.Release())
	}
	return life, nil
}

// Release drops the life lock and removes its file: this daemon is ending.
//
// THE FILE IS REMOVED BEFORE THE LOCK IS RELEASED, and that order is the
// invariant every reaper's judgement rests on: a life lock taken while its
// file still stands was never released by its holder, so its holder died. A
// file that cannot be removed would read as that death, so it is marked
// ended orderly through the held descriptor first (no directory entry is
// needed to write it), and a reaper reads the mark as an orderly end. Every
// failure is returned; the lock is released whatever failed.
func (l *Life) Release() error {
	var errs []error
	if err := os.Remove(AddressPath(l.addrPath, l.instance)); err != nil && !errors.Is(err, os.ErrNotExist) {
		errs = append(errs, fmt.Errorf("remove the instance address: %w", err))
	}
	path := LifePath(l.addrPath, l.instance)
	if err := os.Remove(path); err != nil && !errors.Is(err, os.ErrNotExist) {
		errs = append(errs, fmt.Errorf("remove the life lock %q: %w", path, err))
		if err := l.rec.Overwrite(lifeContent(l.pid, true)); err != nil {
			errs = append(errs, fmt.Errorf("mark the life lock %q ended orderly; the next daemon will record this end as a death: %w", path, err))
		}
	}
	if err := l.rec.Release(); err != nil {
		errs = append(errs, fmt.Errorf("release the life lock %q: %w", path, err))
	}
	return errors.Join(errs...)
}

// pendingSuffix ends the name a life lock is made under before it is linked
// to its own (HoldLife). It does not end in lifeSuffix, so no reaper lists it.
const pendingSuffix = ".pending"

// A life lock's content: a "pid=<n>" line naming its holder, and an
// "ended=orderly" line when its holder ended orderly but could not remove it.
const (
	lifePIDKey     = "pid="
	lifeOrderlyEnd = "ended=orderly"
)

// lifeContent is the content of the life lock of process PID, marked ended
// orderly when ORDERLY.
func lifeContent(pid int, orderly bool) []byte {
	s := lifePIDKey + strconv.Itoa(pid) + "\n"
	if orderly {
		s += lifeOrderlyEnd + "\n"
	}
	return []byte(s)
}

// lifeRecord is what a life lock's content says.
type lifeRecord struct {
	pid      int
	pidKnown bool
	orderly  bool
}

// parseLife reads RAW, a life lock's content. Only a line its newline ends
// counts, so a write its holder died inside is read as no line at all. A file
// naming no pid (one an older build left empty) names its holder unknown.
func parseLife(raw []byte) lifeRecord {
	var r lifeRecord
	lines := strings.Split(string(raw), "\n")
	for _, line := range lines[:len(lines)-1] {
		if v, ok := strings.CutPrefix(line, lifePIDKey); ok {
			if pid, err := strconv.Atoi(v); err == nil && pid > 0 {
				r.pid, r.pidKnown = pid, true
			}
		}
		if line == lifeOrderlyEnd {
			r.orderly = true
		}
	}
	return r
}

// Death is a daemon that ended without its orderly Release, reaped from the
// life lock it left standing with no holder.
type Death struct {
	// Instance is the daemon's instance id.
	Instance string
	// PID is its process id, when its life lock named it (PIDKnown).
	PID      int
	PIDKnown bool
}

// reapHeld judges the life lock of INSTANCE, which this process has just
// taken as REC, and releases it. The file REC holds still standing at its path
// is a holder that ended without Release (the invariant Life.Release keeps):
// its files are removed BEFORE the lock is released, so no other reaper can
// find it again, and it is answered as a Death. A file already gone from its
// path is an orderly end, as is one its holder marked so. A death whose files
// were removed is answered even when the release after them fails.
func reapHeld(addrPath, instance string, rec *recordlock.Record) (Death, bool, error) {
	path := LifePath(addrPath, instance)
	fail := func(err error) (Death, bool, error) {
		rec.Release()
		return Death{}, false, err
	}
	held, err := rec.Stat()
	if err != nil {
		return fail(fmt.Errorf("stat the taken life lock %q: %w", path, err))
	}
	standing, err := os.Stat(path)
	if errors.Is(err, os.ErrNotExist) || (err == nil && !os.SameFile(held, standing)) {
		// Its holder removed the file it held before releasing it.
		if err := rec.Release(); err != nil {
			return Death{}, false, fmt.Errorf("release the life lock %q: %w", path, err)
		}
		return Death{}, false, nil
	}
	if err != nil {
		return fail(fmt.Errorf("stat the life lock %q: %w", path, err))
	}
	raw, err := rec.ReadAll()
	if err != nil {
		return fail(fmt.Errorf("read the leftover life lock %q: %w", path, err))
	}
	life := parseLife(raw)
	if err := os.Remove(AddressPath(addrPath, instance)); err != nil && !errors.Is(err, os.ErrNotExist) {
		return fail(fmt.Errorf("remove the leftover instance address of %q: %w", instance, err))
	}
	if err := os.Remove(path); err != nil {
		return fail(fmt.Errorf("remove the leftover life lock %q: %w", path, err))
	}
	var releaseErr error
	if err := rec.Release(); err != nil {
		releaseErr = fmt.Errorf("release the leftover life lock %q: %w", path, err)
	}
	if life.orderly {
		return Death{}, false, releaseErr
	}
	return Death{Instance: instance, PID: life.pid, PIDKnown: life.pidKnown}, true, releaseErr
}

// OtherLives lists the instances, other than SELF, whose life lock files
// stand beside ADDRPATH now, sorted. A file stands until its holder releases
// it, or until a daemon that outlived it reaps it (LiveOthers, AwaitGone).
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
// each whose life lock beside ADDRPATH another holder has, sorted. Beside
// them it answers every death it reaped: a leftover lock nobody holds is a
// daemon that died without Release, and it is removed and reported once
// (reapHeld). A file gone since it was listed is an orderly end. A lock that
// cannot be probed or reaped is an error, never read as free; the others are
// still judged, and the deaths reaped are answered with the error, so none is
// lost.
func LiveOthers(addrPath, self string) ([]string, []Death, error) {
	others, err := OtherLives(addrPath, self)
	if err != nil {
		return nil, nil, err
	}
	var (
		live   []string
		deaths []Death
		errs   []error
	)
	for _, instance := range others {
		path := LifePath(addrPath, instance)
		rec, _, err := recordlock.TryExistingExclusive(path)
		switch {
		case errors.Is(err, recordlock.ErrWouldBlock):
			live = append(live, instance)
			continue
		case errors.Is(err, os.ErrNotExist):
			continue
		case err != nil:
			errs = append(errs, fmt.Errorf("probe the life lock %q: %w", path, err))
			continue
		}
		death, dead, err := reapHeld(addrPath, instance, rec)
		if dead {
			deaths = append(deaths, death)
		}
		if err != nil {
			errs = append(errs, err)
		}
	}
	return live, deaths, errors.Join(errs...)
}

// Alive answers whether daemon INSTANCE runs on the state root of ADDRPATH:
// another holder has its life lock. An instance with no lock file is not
// running, and neither is one whose leftover lock nobody holds; Alive reaps
// nothing and creates nothing. A lock that cannot be probed is an error,
// never read as free.
func Alive(addrPath, instance string) (bool, error) {
	path := LifePath(addrPath, instance)
	rec, _, err := recordlock.TryExistingExclusive(path)
	switch {
	case errors.Is(err, recordlock.ErrWouldBlock):
		return true, nil
	case errors.Is(err, os.ErrNotExist):
		return false, nil
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
// judges each freed lock (reapHeld): a holder that ended without Release is
// told to REAPED, once, and its file removed. A lock whose file is already
// gone is an orderly end. A lock that cannot be waited for or reaped is an
// error, never read as its holder gone. CTX ends the wait for the caller; a
// kernel wait already under way ends with its holder, and a death it then
// reaps is still told to REAPED, after AwaitGone has returned.
func AwaitGone(ctx context.Context, addrPath string, instances []string, reaped func(Death)) error {
	done := make(chan error, len(instances))
	for _, instance := range instances {
		path := LifePath(addrPath, instance)
		f, err := recordlock.OpenExisting(path)
		if errors.Is(err, os.ErrNotExist) {
			done <- nil
			continue
		}
		if err != nil {
			return fmt.Errorf("open the life lock %q: %w", path, err)
		}
		inflight.Standing(func() {
			rec, err := recordlock.BlockExclusive(f, path)
			if err != nil {
				done <- fmt.Errorf("wait for the life lock %q: %w", path, err)
				return
			}
			death, dead, err := reapHeld(addrPath, instance, rec)
			if dead {
				reaped(death)
			}
			done <- err
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
