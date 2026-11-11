package reuse

import (
	"fmt"
)

// A KEYED SUBJECT is one reuse unit of a suite (units.go): a Go package, a
// test file, a suite-wide check, or a whole suite. Every one is keyed,
// confined, looked up and recorded the same way, through decideKeyed and
// recorder.

// keyedDecision is one keyed subject's decision.
type keyedDecision struct {
	Key Key
	// Profile is the file confining the subject's units to its inputs.
	Profile string
	// Reused is the record a run reuses instead of running the subject.
	Reused *Record
	// Why says why a subject that runs is not reused.
	Why Why
}

// decideKeyed keys set under facts as name (the subject's record name in the
// store), writes the profile confining it, and answers the record a run in
// mode reuses, or why the subject runs: it failed before, its inputs changed
// since its newest green record, or it was never recorded.
func decideKeyed(d Deps, name string, set *InputSet, facts Facts, mode Mode) (keyedDecision, error) {
	key, err := ComputeKeyCached(d.Repo, set, facts, d.Hashes)
	if err != nil {
		return keyedDecision{}, fmt.Errorf("reuse: key %s: %w", name, err)
	}
	profile, err := writeProfile(d, name, set)
	if err != nil {
		return keyedDecision{}, err
	}
	out := keyedDecision{Key: key, Profile: profile}
	switch mode {
	case NoReuse:
		out.Why = Why{Kind: WhyNoReuse}
		return out, nil
	case Targeted:
		out.Why = Why{Kind: WhyTargeted}
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
	last, hasLast, err := d.Store.Last(name)
	if err != nil {
		return keyedDecision{}, err
	}
	if hasLast && last.Failed {
		var changed []string
		if last.Key != key.Digest {
			changed = key.Manifest.Changed(last.Manifest)
		}
		out.Why = Why{Kind: WhyFailedBefore, Changed: changed}
		return out, nil
	}
	newest, ok, err := d.Store.Newest(name)
	if err != nil {
		return keyedDecision{}, err
	}
	if !ok {
		out.Why = Why{Kind: WhyNeverRecorded}
		return out, nil
	}
	out.Why = Why{Kind: WhyInputsChanged, Changed: key.Manifest.Changed(newest.Manifest)}
	return out, nil
}

// recorder stores the outcomes of one run's keyed subjects, reading the
// checkout's HEAD once, for the first subject it records, and every input
// afresh through its own cache (never the decision's), so a re-key sees what
// changed while the subjects ran.
type recorder struct {
	d              Deps
	cache          *HashCache
	branch, commit string
	dirty, read    bool
}

// newRecorder is a recorder over d with an empty cache.
func newRecorder(d Deps) *recorder { return &recorder{d: d, cache: NewHashCache()} }

// record re-keys set under facts and stores the outcome of name on key when
// nothing moved while the subject ran: a pass record when it passed, and its
// last outcome either way, answering whether it recorded. A subject whose
// inputs moved, or that passed on a node_modules its lock did not install,
// records nothing, and label names it in the run's log.
func (r *recorder) record(name, label string, set *InputSet, facts Facts, key Key, units int, failed bool) (bool, error) {
	after, err := ComputeKeyCached(r.d.Repo, set, facts, r.cache)
	if err != nil {
		return false, fmt.Errorf("reuse: re-key %s after its run: %w", name, err)
	}
	if after.Digest != key.Digest {
		r.d.Log.Errorf("%s: not recorded, its inputs changed while it ran: %s", label, nameEntries(after.Manifest.Changed(key.Manifest)))
		return false, nil
	}
	if !failed {
		stale, err := staleInstall(r.d.Repo, set)
		if err != nil {
			return false, fmt.Errorf("reuse: check %s's installed packages: %w", name, err)
		}
		if stale != "" {
			r.d.Log.Errorf("%s: not recorded, it ran on packages its key does not name: %s", label, stale)
			return false, nil
		}
	}
	if !r.read {
		if r.branch, r.commit, r.dirty, err = r.d.Head(); err != nil {
			return false, fmt.Errorf("reuse: read the checkout's HEAD: %w", err)
		}
		r.read = true
	}
	rec := Record{Suite: name, Key: key.Digest, PassedAt: r.d.Now(), Failed: failed, Checkout: r.d.Repo,
		Branch: r.branch, Commit: r.commit, Dirty: r.dirty, Units: units, Manifest: key.Manifest}
	if !failed {
		if err := r.d.Store.Put(rec); err != nil {
			return false, err
		}
	}
	if err := r.d.Store.PutLast(rec); err != nil {
		return false, err
	}
	return true, nil
}

// confinedRefusal says how a read the subject's declaration (field, the
// roster field: Reuse) omits shows in a confined failure: the kernel's
// refusal, or a directory listing it emptied silently.
func confinedRefusal(field string) string {
	return `a failure reading a checkout file with "operation not permitted", or a listing of a checkout directory that comes back empty (filepath.Glob drops the refusal), is a read its ` + field + ` does not declare`
}

// passedClause names the inputs a reused subject's record passed on, and when
// and where it passed, as every reuse report says it.
func passedClause(key Key, r Record) string {
	return fmt.Sprintf("inputs %s, passed %s on %s", key.Short(), r.PassedAt.Local().Format("2006-01-02 15:04"), Source(r))
}

// reusedOnes is the name of every verdict reused, in order: name answers a
// verdict's name and whether it was reused.
func reusedOnes[V any](verdicts []V, name func(V) (string, bool)) []string {
	var out []string
	for _, v := range verdicts {
		if n, reused := name(v); reused {
			out = append(out, n)
		}
	}
	return out
}
