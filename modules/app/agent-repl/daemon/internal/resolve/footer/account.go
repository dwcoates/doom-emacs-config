package footer

import (
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// THE USAGE IS THE ACCOUNT'S, NOT THE WORKSPACE'S (owner ruling, 2026-10-06).
// The vendor meters an ACCOUNT ROOT (a Claude config dir), and every
// workspace whose session spends from that root is drawing the same
// allowances. So the evidence lives in one accountUsage per root, every
// workspace bound to the root points at it, and a figure any of them learns
// is drawn by all of them at once (`mutate` republishes the root's other
// workspaces). The last evidence per root is persisted through the injected
// sink (WithAccountUsageSink) and handed back at construction
// (WithAccountUsages), so a restarted daemon draws it before any session has
// spoken.

// accountUsage is ONE account root's usage evidence.
type accountUsage struct {
	// rate is the drawn allowances.
	rate rateState
	// noAllowance reports that the vendor's usage service answered for the
	// account with NO five-hour window: an account billed by spend rather
	// than by allowance windows. Any figure for a window outranks it.
	noAllowance bool
	// generation counts the changes to this evidence. `mutate` compares it
	// across a change to know the root's other workspaces need republishing
	// and the evidence persisting, and the persister orders its writes by it.
	generation uint64
}

// figured reports whether any window has a figure to draw.
func (u *accountUsage) figured() bool {
	return u.rate.session.figured || u.rate.weekly.figured || u.rate.overage.figured
}

// observed reports whether anything at all is known about the account.
func (u *accountUsage) observed() bool {
	return u.figured() || u.noAllowance
}

// arm names what the enduring line draws from this evidence, for the logs.
func (u *accountUsage) arm() string {
	switch {
	case u.figured():
		return "usage"
	case u.noAllowance:
		return "no_allowance"
	default:
		return "unobserved"
	}
}

// touch records that new evidence was filed at `at`.
func (u *accountUsage) touch(at time.Time) {
	u.rate.at = at
	u.generation++
}

// record is the evidence as the state store keeps it.
func (u *accountUsage) record(root string) wsm.AccountUsage {
	return wsm.AccountUsage{
		ConfigDir:   root,
		ObservedAt:  u.rate.at,
		NoAllowance: u.noAllowance,
		Session:     u.rate.session.figures(),
		Weekly:      u.rate.weekly.figures(),
		Overage:     u.rate.overage.figures(),
	}
}

// usageFromRecord rebuilds an account's evidence from its stored row.
func usageFromRecord(rec wsm.AccountUsage) *accountUsage {
	u := &accountUsage{noAllowance: rec.NoAllowance}
	u.rate.at = rec.ObservedAt
	u.rate.session.restore(rec.Session)
	u.rate.weekly.restore(rec.Weekly)
	u.rate.overage.restore(rec.Overage)
	return u
}

// figures is the window as the state store keeps it, nil when unfigured.
func (w *allowanceWindow) figures() *wsm.AllowanceFigures {
	if !w.figured {
		return nil
	}
	return &wsm.AllowanceFigures{
		Utilization: w.utilization,
		ResetsAtS:   w.resetsAtS,
		SampledAtMs: w.sampledAtMs,
		Verdict:     w.verdict,
	}
}

// restore refills the window from its stored figures; nil leaves it
// unfigured.
func (w *allowanceWindow) restore(f *wsm.AllowanceFigures) {
	if f == nil {
		return
	}
	*w = allowanceWindow{
		figured:     true,
		utilization: f.Utilization,
		resetsAtS:   f.ResetsAtS,
		sampledAtMs: f.SampledAtMs,
		verdict:     f.Verdict,
	}
}

// verdictOf is a rate-limit event's status arm as a stored verdict. An event
// the vendor left statusless is VerdictNone: nothing is defaulted to
// "allowed".
func verdictOf(status *conversationv1.SessionRateLimitStatus) wsm.AllowanceVerdict {
	switch status.GetStatus().(type) {
	case *conversationv1.SessionRateLimitStatus_Allowed:
		return wsm.VerdictAllowed
	case *conversationv1.SessionRateLimitStatus_AllowedWarning:
		return wsm.VerdictAllowedWarning
	case *conversationv1.SessionRateLimitStatus_Rejected:
		return wsm.VerdictRejected
	default:
		return wsm.VerdictNone
	}
}

// SetAccount binds the workspace to the account root its session spends from.
//
// A workspace bound before any evidence keeps none of its own: it draws the
// root's. A workspace that learned figures while still unbound hands them to
// a root that has none yet, so nothing it read is lost to the binding.
func (r *resolver) SetAccount(ws ids.WorkspaceID, root string) {
	r.mutate(ws, "daemon.footer.set_account", "the footer bound the workspace to its account root",
		dlog.Context{"config_dir": root}, func(s *wsState) {
			r.bindAccountLocked(ws, s, root)
		})
}

// bindAccountLocked points the workspace at the root's shared evidence. An
// empty root is a caller defect: it is recorded as an error and the binding
// left as it stands, never filed under a root nobody named.
func (r *resolver) bindAccountLocked(ws ids.WorkspaceID, s *wsState, root string) {
	if root == "" {
		r.logOf(ws, s).Error("daemon.footer.set_account",
			"refused to bind the workspace to an unnamed account root; its usage stays where it was", nil)
		return
	}
	if s.account == root {
		return
	}
	shared, ok := r.accounts[root]
	if !ok {
		shared = &accountUsage{}
		r.accounts[root] = shared
	}
	if s.account == "" && !shared.observed() && s.usage.observed() {
		*shared = *s.usage
		shared.generation++
	}
	s.account = root
	s.usage = shared
}

// peersLocked answers every OTHER published workspace bound to the root.
func (r *resolver) peersLocked(ws ids.WorkspaceID, root string) []ids.WorkspaceID {
	if root == "" {
		return nil
	}
	var out []ids.WorkspaceID
	for id, s := range r.states {
		if id != ws && s.seen && s.account == root {
			out = append(out, id)
		}
	}
	return out
}

// usageWrite is one persistence of an account's evidence, snapshotted under
// the resolver's lock.
type usageWrite struct {
	generation uint64
	record     wsm.AccountUsage
}

// persistUsage hands the evidence to the sink, IN GENERATION ORDER: writes are
// made after the resolver's lock is released, so two changes may reach here
// in either order, and the older is dropped rather than allowed to overwrite
// the newer. A sink failure is recorded as an error; the drawn evidence is
// unaffected and the next change writes again.
func (r *resolver) persistUsage(w usageWrite) {
	if r.opts.usageSink == nil {
		return
	}
	r.persistMu.Lock()
	defer r.persistMu.Unlock()
	root := w.record.ConfigDir
	if last, ok := r.persisted[root]; ok && w.generation <= last {
		return
	}
	if err := r.opts.usageSink(w.record); err != nil {
		r.log.Global().Error("daemon.footer.account_usage_persist",
			"could not keep the account's usage durable; a restarted daemon will draw the last kept figures",
			dlog.Context{"config_dir": root, "cause": err.Error()})
		return
	}
	r.persisted[root] = w.generation
}
