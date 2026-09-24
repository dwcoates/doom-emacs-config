package rollout

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"syscall"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/sessionlock"
	"claude-repld/internal/wsm"
)

// Intent is what the outgoing daemon MEANT to happen to one session. It is half
// of the bounce accounting: the other half is the kernel lock the incoming
// daemon probes, and the two together name the disposition.
type Intent string

// The intents.
const (
	// IntentPreserve is a session handed over alive: its shim keeps running and
	// keeps its kernel lock across the whole handover.
	IntentPreserve Intent = "preserve"
	// IntentStandDown is a session the outgoing daemon deliberately ended.
	IntentStandDown Intent = "stand_down"
	// IntentNoSession is a workspace handed over with NO SHIM AT ALL: it was
	// registered and never opened, or its session was already gone when the
	// manifest was written. It carries an entry because the entry is what arms
	// the successor's rendezvous, and it is a DISTINCT intent because there is
	// no process whose survival could be judged — reading a free lock for one
	// of these as a session that silently died would raise a fault on the most
	// ordinary handover there is.
	IntentNoSession Intent = "no_session"
	// IntentUnattested is a session whose outgoing daemon stated NO intent at
	// all, because it wrote no manifest: it crashed or was force-killed. There
	// is nothing to compare a lock against, which is why every such session is
	// UNKNOWN rather than judged.
	IntentUnattested Intent = "unattested"
)

// DispositionKind is what actually became of one session. THE FOUR ARE NEVER
// COLLAPSED and never counted: after a crash or a force-kill, WHICH sessions
// silently died is the whole point of the record.
type DispositionKind string

// The dispositions.
const (
	// DispositionPreserved is a session meant to survive that did: its lock is
	// still held.
	DispositionPreserved DispositionKind = "PRESERVED"
	// DispositionRolled is a session meant to end that did: its lock is free.
	DispositionRolled DispositionKind = "ROLLED"
	// DispositionDied is a session meant to survive whose lock is free — it
	// died silently, and this record is the only place that says so.
	DispositionDied DispositionKind = "DIED"
	// DispositionUnknown is a probe that could not tell, or a session meant to
	// end whose lock is still held. Never read as either of the other two.
	DispositionUnknown DispositionKind = "UNKNOWN"
)

// Manifest is the stand-down intent manifest: what the outgoing daemon meant
// for every session it was serving. It is a FILE in the shared state root, not
// a channel — the writer never reads it back, and the reader is a different
// process that came up afterwards.
type Manifest struct {
	// Daemon is the outgoing daemon's instance id.
	Daemon ids.InstanceID `json:"daemon"`
	// Successor is the address the outgoing daemon announced.
	Successor string `json:"successor"`
	// WrittenAt is when the manifest was written.
	WrittenAt time.Time `json:"written_at"`
	// Forced records that the handover was FORCED: the transfers did not wait
	// for freeness, and the successor bounces the stale shims it adopts at once
	// too, rather than registering them behind their work.
	Forced bool `json:"forced"`
	// Sessions is one record per session, in workspace order.
	Sessions []ManifestSession `json:"sessions"`
}

// ManifestSession is one session's stand-down record.
type ManifestSession struct {
	// Workspace is the session's workspace.
	Workspace ids.WorkspaceID `json:"workspace"`
	// Dir is the workspace directory, so the incoming daemon can derive the
	// kernel lock path without the registry having to agree first.
	Dir string `json:"dir"`
	// ShimPID is the shim process serving the session, 0 when none was up.
	ShimPID int `json:"shim_pid"`
	// VendorSessionID is the vendor's own identity for the session.
	VendorSessionID string `json:"vendor_session_id"`
	// Intent is what the outgoing daemon meant to happen to it.
	Intent Intent `json:"intent"`
	// ExpectedHost records that a WatchHostWorkspace stream was held at
	// announcement, so the incoming daemon knows the rendezvous owes a host
	// call. It rides the manifest because there is NO daemon-to-daemon channel
	// to carry it.
	ExpectedHost bool `json:"expected_host"`
	// ExpectedWeb records the same for the WatchWebWorkspace stream.
	ExpectedWeb bool `json:"expected_web"`
}

// Disposition is one session's reconciled outcome.
type Disposition struct {
	// Workspace is the session's workspace.
	Workspace ids.WorkspaceID
	// Intent is what the outgoing daemon meant.
	Intent Intent
	// Lock is what the kernel lock said.
	Lock sessionlock.State
	// Kind is the disposition the two together name.
	Kind DispositionKind
}

// AdoptedSession is one SESSION a boot adopted from a surviving shim: a
// workspace whose kernel lock read HELD, which by the shim's lock contract
// means a session was started on that process and is still running.
//
// IT CARRIES THE PID BECAUSE THE ACCOUNTING NAMES IT. Reconciling a bounce
// with no manifest used to record `shim_pid: 0` for every survivor — the zero
// value of an empty manifest entry, not a reading of anything — which read as
// "no process" for the one case where a process demonstrably answered a dial.
type AdoptedSession struct {
	// Workspace is the adopted session's workspace.
	Workspace ids.WorkspaceID
	// ShimPID is the surviving shim's process id, as the adoption dialed it.
	ShimPID int
}

// FaultBounceDisposition is the fault kind an ORDINARY reconciled session is
// recorded under — one the bounce preserved or rolled, opened and closed in the
// same breath so the per-session accounting survives without polluting the open
// fault set.
//
// A disposition that NEEDS A HUMAN is recorded under its own health kind
// instead, because that is what the host view renders: health.KindBounceDied
// and health.KindBounceUnknown are the two SessionFault/HostFault arms the
// contract spells for a bounce, and a fault opened under this generic kind
// would reach neither.
const FaultBounceDisposition = "bounce_disposition"

// faultKind names the WSM fault kind one disposition is recorded under.
func faultKind(kind DispositionKind) string {
	switch kind {
	case DispositionDied:
		return health.KindBounceDied
	case DispositionUnknown:
		return health.KindBounceUnknown
	default:
		return FaultBounceDisposition
	}
}

// writeManifest records the outgoing daemon's intent for every workspace it is
// handing over. It is written ATOMICALLY — a half-written manifest would make
// the incoming daemon's accounting worse than no manifest at all.
func (c *controller) writeManifest(ctx context.Context, m Manifest) error {
	fields := dlog.Context{"path": c.deps.IntentManifest, "sessions": len(m.Sessions)}
	if c.deps.IntentManifest == "" {
		err := errors.New("rollout: no intent manifest path is configured")
		c.log.Error(opManifest, "cannot write the stand-down intent manifest", withCause(fields, err))
		return err
	}
	body, err := json.MarshalIndent(m, "", "  ")
	if err != nil {
		c.log.Error(opManifest, "could not encode the intent manifest", withCause(fields, err))
		return fmt.Errorf("rollout: encode the intent manifest: %w", err)
	}
	dir := filepath.Dir(c.deps.IntentManifest)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		c.log.Error(opManifest, "could not create the intent directory", withCause(fields, err))
		return fmt.Errorf("rollout: create %s: %w", dir, err)
	}
	tmp, err := os.CreateTemp(dir, "manifest-*.json")
	if err != nil {
		c.log.Error(opManifest, "could not open the intent manifest for writing", withCause(fields, err))
		return fmt.Errorf("rollout: create the intent manifest: %w", err)
	}
	if _, err := tmp.Write(body); err != nil {
		err = errors.Join(err, tmp.Close(), os.Remove(tmp.Name()))
		c.log.Error(opManifest, "could not write the intent manifest", withCause(fields, err))
		return fmt.Errorf("rollout: write the intent manifest: %w", err)
	}
	if err := tmp.Close(); err != nil {
		err = errors.Join(err, os.Remove(tmp.Name()))
		c.log.Error(opManifest, "could not close the intent manifest", withCause(fields, err))
		return fmt.Errorf("rollout: close the intent manifest: %w", err)
	}
	if err := os.Rename(tmp.Name(), c.deps.IntentManifest); err != nil {
		err = errors.Join(err, os.Remove(tmp.Name()))
		c.log.Error(opManifest, "could not install the intent manifest", withCause(fields, err))
		return fmt.Errorf("rollout: install the intent manifest: %w", err)
	}
	c.log.Info(opManifest, "wrote the stand-down intent manifest", fields)
	_ = ctx
	return nil
}

// ReadManifest loads the stand-down intent manifest. The bool reports whether
// one was there at all: an ordinary boot finds none, which is not a failure.
func ReadManifest(path string) (Manifest, bool, error) {
	body, err := os.ReadFile(path)
	if errors.Is(err, os.ErrNotExist) {
		return Manifest{}, false, nil
	}
	if err != nil {
		return Manifest{}, false, fmt.Errorf("rollout: read the intent manifest %s: %w", path, err)
	}
	var m Manifest
	if err := json.Unmarshal(body, &m); err != nil {
		return Manifest{}, false, fmt.Errorf("rollout: decode the intent manifest %s: %w", path, err)
	}
	return m, true, nil
}

// Reconcile reads the intent manifest against the kernel locks actually held
// and records ONE DISPOSITION PER SESSION.
//
// The matrix is the whole of the accounting:
//
//	preserve   + held    → PRESERVED   (survived, as intended)
//	preserve   + free    → DIED        (meant to survive; it did not)
//	stand_down + free    → ROLLED      (ended, as intended)
//	stand_down + held    → UNKNOWN     (meant to end; something still holds it)
//	any        + unknown → UNKNOWN     (the probe could not tell)
//
// PRESERVED and ROLLED are recorded as ALREADY-RESOLVED faults, so the record
// exists per session without polluting the open-fault set; DIED and UNKNOWN
// stay OPEN, because each is a workspace whose session state needs a human.
//
// A MANIFEST IS CONSUMED EXACTLY ONCE. It is one outgoing daemon's statement
// about one bounce, so once every disposition it names is durably recorded it
// is retired (retireManifest) and no later boot reads it again. Nothing used
// to remove it: a manifest written 2026-09-23 12:30 was reconciled again on
// every boot for a day, re-opening a bounce_died fault and a WARN each time for
// a shim that had died once, long before.
func (c *controller) Reconcile(ctx context.Context, adopted []AdoptedSession) ([]Disposition, error) {
	out, _, landed, err := c.reconcile(ctx, adopted)
	if err != nil {
		return nil, err
	}
	if landed {
		c.retireManifest("every disposition it names is recorded")
	}
	return out, nil
}

// reconcile is Reconcile without the retirement, so a joining daemon can arm
// its rendezvous from the manifest before the file goes away. It reports
// whether a manifest was found and whether every disposition it named LANDED
// durably — false when any record failed, or was deferred behind a read-only
// handle, which is exactly when the manifest must be kept.
func (c *controller) reconcile(ctx context.Context, adopted []AdoptedSession) ([]Disposition, bool, bool, error) {
	m, found, err := ReadManifest(c.deps.IntentManifest)
	if err != nil {
		c.log.Error(opReconcile, "could not read the intent manifest",
			withCause(dlog.Context{"path": c.deps.IntentManifest}, err))
		return nil, false, false, err
	}
	if !found {
		return c.reconcileWithoutManifest(ctx, adopted), false, false, nil
	}
	out := make([]Disposition, 0, len(m.Sessions))
	landed := true
	for _, session := range m.Sessions {
		state := sessionlock.StateUnknown
		if c.deps.LockProbe != nil {
			probed, err := c.deps.LockProbe(session.Dir)
			if err != nil {
				c.log.Warn(opReconcile, "a workspace's lock probe could not tell", withCause(dlog.Context{
					"workspace": string(session.Workspace), "dir": session.Dir,
				}, err))
			}
			state = probed
		}
		// AN ENTRY WITH NO PID NAMES NO PROCESS, whatever intent it carries.
		// The write site records IntentNoSession for these, and this
		// normalization is what makes a manifest written by an older daemon —
		// which spelled every entry `preserve` — reconcile to the same
		// disposition rather than raising a fault over a workspace that never
		// had a shim.
		intent := session.Intent
		if session.ShimPID == 0 && intent != IntentStandDown {
			intent = IntentNoSession
		}
		d := Disposition{
			Workspace: session.Workspace,
			Intent:    intent,
			Lock:      state,
			Kind:      disposition(intent, state),
		}
		out = append(out, d)
		if !c.recordDisposition(ctx, session, d) {
			landed = false
		}
	}
	c.log.Info(opReconcile, "reconciled the stand-down intent manifest against the kernel locks",
		dlog.Context{"outgoing_daemon": string(m.Daemon), "sessions": len(out), "recorded": landed})
	return out, true, landed, nil
}

// retireManifest removes the intent manifest once what it states is recorded.
//
// A MANIFEST THAT CANNOT BE REMOVED IS RECORDED AT ERROR AND LEFT. The boot is
// not failed for it: everything the manifest names is already accounted for,
// and the cost of the leftover is that the next boot accounts for it again —
// which this record is the explanation of.
func (c *controller) retireManifest(why string) {
	fields := dlog.Context{"path": c.deps.IntentManifest, "why": why}
	err := os.Remove(c.deps.IntentManifest)
	switch {
	case err == nil:
		c.log.Info(opManifest, "retired the stand-down intent manifest; no later boot reads it again", fields)
	case errors.Is(err, os.ErrNotExist):
		c.log.Debug(opManifest, "the stand-down intent manifest was already retired", fields)
	default:
		c.log.Error(opManifest, "could not retire the stand-down intent manifest; the next boot will account for it again",
			withCause(fields, err))
	}
}

// clearStaleManifest removes a manifest left over from an EARLIER bounce before
// this daemon spawns a successor. A joining successor takes the first manifest
// it finds as its incumbent's, and it starts looking the moment it boots —
// before this daemon has written anything — so a leftover would be read as
// this handover's intent. The leftover is one no daemon finished accounting
// for, and its evidence is recorded here before it goes.
func (c *controller) clearStaleManifest(fields dlog.Context) error {
	// NOTHING IS AT THE PATH when it, or a directory on the way to it, does
	// not exist; a path whose parent is a FILE holds no manifest either, and
	// the write that follows reports that path's own failure.
	if _, err := os.Lstat(c.deps.IntentManifest); errors.Is(err, os.ErrNotExist) || errors.Is(err, syscall.ENOTDIR) {
		return nil
	}
	m, found, readErr := ReadManifest(c.deps.IntentManifest)
	if !found && readErr == nil {
		return nil
	}
	evidence := merge(fields, dlog.Context{"path": c.deps.IntentManifest})
	if readErr == nil {
		evidence = merge(evidence, dlog.Context{
			"stale_outgoing_daemon": string(m.Daemon),
			"stale_written_at":      m.WrittenAt.Format(time.RFC3339Nano),
			"stale_sessions":        len(m.Sessions),
		})
	} else {
		evidence = merge(evidence, dlog.Context{"stale_read_error": readErr.Error()})
	}
	if err := os.Remove(c.deps.IntentManifest); err != nil && !errors.Is(err, os.ErrNotExist) {
		c.log.Error(opHandover, "could not remove a stale intent manifest; a successor would read it as this handover's",
			withCause(evidence, err))
		return fmt.Errorf("rollout: remove the stale intent manifest %s: %w", c.deps.IntentManifest, err)
	}
	c.log.Info(opHandover, "removed an intent manifest an earlier bounce left unconsumed, before spawning the successor", evidence)
	return nil
}

// reconcileWithoutManifest accounts for a boot that found NO manifest.
//
// AN ABSENT MANIFEST IS LEGITIMATE ON MOST BOOTS. Only Handover writes one, so
// every boot that is not the successor of a self-merge rollout — a cold start,
// a launchd restart, an operator's kill — finds none by design. With no
// surviving SESSION that is an ordinary boot and there is nothing to account
// for, and an inert surviving shim is not a surviving session: it never took
// the workspace lock because it never started one, so there is no process
// whose survival could be judged. The boot passes only the lock-held
// survivors, which is what makes the ordinary case ordinary.
//
// With sessions this boot ADOPTED it is a crash or a force-kill: the outgoing
// daemon never stood down, so nothing states what its bounce meant for them,
// and BOUNCE ACCOUNTABILITY surfaces that per workspace rather than passing
// over it. Each adopted session gets an OPEN bounce_unknown fault.
//
// THE LOCK STATE IS NOT INVENTED HERE. It reads HELD because the caller's
// contract is that every entry is a lock-held survivor; nothing in this
// function guesses a state it did not read.
func (c *controller) reconcileWithoutManifest(ctx context.Context, adopted []AdoptedSession) []Disposition {
	if len(adopted) == 0 {
		c.log.Debug(opReconcile, "no intent manifest is present and no session survived; this is an ordinary boot",
			dlog.Context{"path": c.deps.IntentManifest})
		return nil
	}
	c.log.Warn(opReconcile, "sessions survived a bounce that wrote no intent manifest; each one is unaccounted for",
		dlog.Context{"path": c.deps.IntentManifest, "adopted": len(adopted)})
	out := make([]Disposition, 0, len(adopted))
	for _, session := range adopted {
		d := Disposition{
			Workspace: session.Workspace,
			Intent:    IntentUnattested,
			Lock:      sessionlock.StateHeld,
			Kind:      DispositionUnknown,
		}
		out = append(out, d)
		c.recordDisposition(ctx, ManifestSession{
			Workspace: session.Workspace,
			ShimPID:   session.ShimPID,
		}, d)
	}
	return out
}

// disposition names what an intent and a lock state together mean.
func disposition(intent Intent, state sessionlock.State) DispositionKind {
	switch {
	case intent == IntentNoSession:
		// NOTHING WAS TO BE PRESERVED, so nothing failed to be. The lock is
		// not consulted: there is no shim behind it either way, and an
		// undetermined probe over a workspace with no process is not an
		// unknown disposition.
		return DispositionPreserved
	case state == sessionlock.StateUnknown:
		return DispositionUnknown
	case intent == IntentPreserve && state == sessionlock.StateHeld:
		return DispositionPreserved
	case intent == IntentPreserve && state == sessionlock.StateFree:
		return DispositionDied
	case intent == IntentStandDown && state == sessionlock.StateFree:
		return DispositionRolled
	default:
		return DispositionUnknown
	}
}

// pendingDisposition is one deferred accounting write.
type pendingDisposition struct {
	session ManifestSession
	d       Disposition
}

// flushDispositions writes the dispositions reconciled while the handle was
// read-only. The successor calls it the moment its handle is promoted, so the
// bounce accounting is deferred by exactly the read-only window and by nothing
// else.
func (c *controller) flushDispositions(ctx context.Context) {
	c.mu.Lock()
	pending := c.pendingDispositions
	c.pendingDispositions = nil
	c.mu.Unlock()
	for _, p := range pending {
		c.logTransition(opReconcile, p.session.Workspace, "disposition_deferred", true, false,
			dlog.Context{"disposition": string(p.d.Kind)})
	}
	landed := true
	for _, p := range pending {
		if !c.recordDisposition(ctx, p.session, p.d) {
			landed = false
		}
	}
	if len(pending) > 0 {
		c.log.Debug(opReconcile, "wrote the bounce dispositions deferred by the read-only window",
			dlog.Context{"dispositions": len(pending), "recorded": landed})
		// THE DEFERRED ACCOUNTING IS WHAT KEPT THE MANIFEST: once it is
		// written, the manifest has been consumed.
		if landed {
			c.retireManifest("the dispositions deferred by the read-only window are recorded")
		}
	}
}

// recordDisposition writes one session's disposition as a WSM fault. It is
// never collapsed into a count: one record per session, naming the workspace.
// It reports whether the record LANDED: false when it was deferred behind a
// read-only handle or its write failed (which is recorded at ERROR here).
func (c *controller) recordDisposition(ctx context.Context, session ManifestSession, d Disposition) bool {
	ws := session.Workspace
	// A READ-ONLY HANDLE IS NOT A FAILURE HERE, it is the joining successor's
	// ordinary state: the incumbent is still the sole writer. The accounting
	// is HELD, not dropped, and flushDispositions writes it at the promotion.
	if c.deps.DB.ReadOnly() {
		c.mu.Lock()
		before := len(c.pendingDispositions)
		c.pendingDispositions = append(c.pendingDispositions, pendingDisposition{session: session, d: d})
		after := len(c.pendingDispositions)
		c.mu.Unlock()
		c.logTransition(opReconcile, ws, "pending_dispositions", before, after,
			dlog.Context{"disposition": string(d.Kind)})
		c.log.Debug(opReconcile, "deferred a bounce disposition until the state handle writes",
			dlog.Context{"workspace": string(ws), "disposition": string(d.Kind)})
		return false
	}
	fields := dlog.Context{
		"workspace":   string(ws),
		"intent":      string(d.Intent),
		"lock":        d.Lock.String(),
		"disposition": string(d.Kind),
		"shim_pid":    session.ShimPID,
	}
	id, err := c.deps.DB.OpenFault(ctx, wsm.Fault{
		Workspace: &ws,
		Kind:      faultKind(d.Kind),
		Detail: fmt.Sprintf("the bounce meant to %s this session; its workspace lock reads %s",
			d.Intent, d.Lock),
		Evidence: map[string]string{
			"disposition":       string(d.Kind),
			"intent":            string(d.Intent),
			"lock_state":        d.Lock.String(),
			"shim_pid":          fmt.Sprint(session.ShimPID),
			"vendor_session_id": session.VendorSessionID,
		},
		OpenedAt: c.deps.Clock.Now(),
	})
	if err != nil {
		c.log.Error(opReconcile, "could not record a session's bounce disposition", withCause(fields, err))
		return false
	}
	if d.Kind == DispositionPreserved || d.Kind == DispositionRolled {
		// The record exists; nothing needs doing about it. Closing it here is
		// what keeps the OPEN fault set meaningful without losing the per
		// session accounting.
		if err := c.deps.DB.CloseFault(ctx, id, c.deps.Clock.Now()); err != nil {
			c.log.Error(opReconcile, "could not resolve an ordinary bounce disposition", withCause(fields, err))
			return false
		}
		c.log.Debug(opReconcile, "recorded an ordinary bounce disposition", fields)
		return true
	}
	c.log.Warn(opReconcile, "a session's bounce disposition needs a human", fields)
	return true
}
