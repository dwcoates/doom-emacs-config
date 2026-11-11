package reuse

import (
	"fmt"
	"os"
	"path/filepath"
)

// A KEYED SUBJECT is a suite (Decide) or one of its packages
// (DecidePackages): both are keyed, confined, looked up and recorded the
// same way, through decideKeyed and recorder.

// keyedDecision is one keyed subject's decision.
type keyedDecision struct {
	Key Key
	// Profile is the file confining the subject's units to its inputs.
	Profile string
	// Reused is the record a run reuses instead of running the subject.
	Reused *Record
	// Why says why a subject that runs is not reused.
	Why string
}

// decideKeyed keys set under facts as name (the subject's record name in the
// store), writes the profile confining it, and answers the record a run in
// mode reuses, or why the subject runs.
func decideKeyed(d Deps, name string, set *InputSet, facts Facts, mode Mode) (keyedDecision, error) {
	key, err := ComputeKey(d.Repo, set, facts)
	if err != nil {
		return keyedDecision{}, fmt.Errorf("reuse: key %s: %w", name, err)
	}
	profile, err := Profile(d.Repo, d.DenyRoots, set)
	if err != nil {
		return keyedDecision{}, fmt.Errorf("reuse: %s's profile: %w", name, err)
	}
	path := filepath.Join(d.ProfileDir, name+".sb")
	if err := os.WriteFile(path, []byte(profile), 0o644); err != nil {
		return keyedDecision{}, fmt.Errorf("reuse: write %s's profile: %w", name, err)
	}
	out := keyedDecision{Key: key, Profile: path}
	switch mode {
	case NoReuse:
		out.Why = "--no-reuse"
		return out, nil
	case Targeted:
		out.Why = "a targeted run neither reuses nor records"
		return out, nil
	}
	rec, ok, err := d.Store.Lookup(name, key.Digest)
	if err != nil {
		return keyedDecision{}, err
	}
	if ok {
		out.Reused = &rec
		return out, nil
	}
	newest, ok, err := d.Store.Newest(name)
	if err != nil {
		return keyedDecision{}, err
	}
	if !ok {
		out.Why = "no record"
		return out, nil
	}
	out.Why = "changed since its newest green record: " + nameEntries(key.Manifest.Changed(newest.Manifest))
	return out, nil
}

// recorder stores the passes of one run's keyed subjects, reading the
// checkout's HEAD once, for the first subject it records.
type recorder struct {
	d              Deps
	branch, commit string
	dirty, read    bool
}

// record re-keys set under facts and stores a pass of name on key when
// nothing moved while the subject ran, answering whether it recorded. A
// subject whose inputs moved, or that ran on a node_modules its lock did not
// install, records nothing, and label names it in the run's log.
func (r *recorder) record(name, label string, set *InputSet, facts Facts, key Key, units int) (bool, error) {
	after, err := ComputeKey(r.d.Repo, set, facts)
	if err != nil {
		return false, fmt.Errorf("reuse: re-key %s after its run: %w", name, err)
	}
	if after.Digest != key.Digest {
		r.d.Log.Errorf("%s: not recorded, its inputs changed while it ran: %s", label, nameEntries(after.Manifest.Changed(key.Manifest)))
		return false, nil
	}
	stale, err := staleInstall(r.d.Repo, set)
	if err != nil {
		return false, fmt.Errorf("reuse: check %s's installed packages: %w", name, err)
	}
	if stale != "" {
		r.d.Log.Errorf("%s: not recorded, it ran on packages its key does not name: %s", label, stale)
		return false, nil
	}
	if !r.read {
		if r.branch, r.commit, r.dirty, err = r.d.Head(); err != nil {
			return false, fmt.Errorf("reuse: read the checkout's HEAD: %w", err)
		}
		r.read = true
	}
	rec := Record{Suite: name, Key: key.Digest, PassedAt: r.d.Now(), Checkout: r.d.Repo,
		Branch: r.branch, Commit: r.commit, Dirty: r.dirty, Units: units, Manifest: key.Manifest}
	if err := r.d.Store.Put(rec); err != nil {
		return false, err
	}
	return true, nil
}
