package server

import (
	"context"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/health"
	"claude-repld/internal/ids"
	"claude-repld/internal/publish"
	"claude-repld/internal/wsm"
)

// THE HOST VIEW. WatchHostWorkspace carries one STATE arm (`host`, whole-
// replaced per push) beside four EVENT arms (notification, transferred,
// reload_webapp, open_in_editor). State and events cannot share one topic: a
// Topic replays exactly its latest value to a late subscriber, so a subscriber
// arriving after an event would be handed that event and never the state. The
// state therefore gets its own topic, and the stream serves both.
//
// The view is composed HERE rather than resolved in a package of its own
// because every fact it carries is already a dependency of the server: the
// registry and the session record (wsm), the occupancy lease (wsm), the open
// faults (health), and the live half (SessionFacts, below).

// SessionFacts answers the LIVE half of the host view — the facts only the
// party that spawns and supervises shims can know.
//
// It is an interface here, and implemented by the workspace fleet, because the
// server must not reach into the fleet's internals for them; the fleet mints
// the session identity and owns the controller generation.
type SessionFacts interface {
	// HostSessionFacts answers one workspace's live session facts, reporting
	// false when this daemon is operating no session for it.
	HostSessionFacts(ws ids.WorkspaceID) (HostFacts, bool)
}

// HostFacts is the live half of one workspace's host view.
type HostFacts struct {
	// SessionID is the daemon-minted host session identity — the echo token
	// Emacs correlates transcripts, health probes and fault windows against.
	SessionID string
	// Generation is the daemon-minted controller generation operating the
	// session. It rotates on restart without the session id changing.
	Generation string
	// ShimAttached reports whether the session's shim process is attached
	// right now; false while the daemon is between shim starts.
	ShimAttached bool
	// Backfill is whether the on-disk transcript has reached the store.
	Backfill BackfillState
	// BackfillDetail is the sidecar's account, carried only by BackfillFailed.
	BackfillDetail string
}

// BackfillState is HostBackfill's arm, named rather than spelled as a proto
// message so the fleet states the fact without importing the wire types.
type BackfillState int

// The backfill states, in HostBackfill's own order.
const (
	// BackfillNone is no transcript on disk: a genuinely fresh workspace.
	BackfillNone BackfillState = iota
	// BackfillPending is a transcript on disk with no file-plane event yet.
	BackfillPending
	// BackfillDone is the file plane delivered into the store.
	BackfillDone
	// BackfillFailed is part of the transcript that could not be read.
	BackfillFailed
)

// unwiredSessionFacts is the facts source a daemon has when NONE was wired. It
// is an explicit named type rather than a nil check so the absence is a thing
// the code says out loud, and it answers `false` — "this daemon operates no
// session for this workspace" — which is the only answer it can honestly give.
//
// It is not a fallback that hides the gap: New records the missing seam at
// ERROR, and every session that HAS a record then withholds its host view with
// its own ERROR naming the remediation. A daemon in this state serves no host
// state at all, loudly, rather than serving an invented one.
type unwiredSessionFacts struct{}

func (unwiredSessionFacts) HostSessionFacts(ids.WorkspaceID) (HostFacts, bool) {
	return HostFacts{}, false
}

// hostStateTopic answers a workspace's host STATE topic, minting it on first
// use exactly as hostTopic mints the event one.
func (s *server) hostStateTopic(ws ids.WorkspaceID) *publish.Topic[*agentreplv1.HostWorkspace] {
	s.mu.Lock()
	defer s.mu.Unlock()
	t, ok := s.hostStateTopics[ws]
	if !ok {
		t = &publish.Topic[*agentreplv1.HostWorkspace]{}
		s.hostStateTopics[ws] = t
	}
	return t
}

// PublishHostWorkspace composes one workspace's host view and publishes it.
// Every edge that can change the view calls it — a session's start, death or
// restart, a naming change, a lease taken or released — and the topic's own
// proto.Equal dedupe drops a re-render that changed nothing, so no caller has
// to decide whether its edge actually moved the view.
func (s *server) PublishHostWorkspace(ctx context.Context, ws ids.WorkspaceID) {
	const op = "daemon.server.publish_host_workspace"
	log, err := s.workspaceLog(ctx, "WatchHostWorkspace", ws)
	if err != nil {
		s.log.Error(op, "could not resolve the workspace's log sink", dlog.Context{
			"workspace": string(ws), "cause": err.Error(),
		})
		return
	}
	view, ok := s.composeHostWorkspace(ctx, log, ws)
	if !ok {
		return
	}
	s.hostStateTopic(ws).Publish(view)
	log.Debug(op, "published the host view", nil)
}

// composeHostWorkspace builds the WHOLE host view, reporting false when it
// cannot prove one. A view is never published in part: the alternative to an
// unprovable session arm is an invented one, and Emacs gates its composer and
// correlates its processes on exactly these facts.
func (s *server) composeHostWorkspace(
	ctx context.Context,
	log dlog.Logger,
	ws ids.WorkspaceID,
) (*agentreplv1.HostWorkspace, bool) {
	const op = "daemon.server.compose_host_workspace"

	record, err := s.deps.DB.Workspace(ctx, ws)
	if err != nil {
		log.Error(op, "could not read the workspace record", dlog.Context{"cause": err.Error()})
		return nil, false
	}
	session, hasSession, err := s.deps.DB.Session(ctx, ws)
	if err != nil {
		log.Error(op, "could not read the session record", dlog.Context{"cause": err.Error()})
		return nil, false
	}

	view := &agentreplv1.HostWorkspace{Naming: hostNaming(record)}

	facts, live := s.deps.SessionFacts.HostSessionFacts(ws)
	switch {
	case live:
		existing, ok := s.hostExisting(ctx, log, ws, session, hasSession, facts)
		if !ok {
			return nil, false
		}
		view.Session = &agentreplv1.HostWorkspace_Existing{Existing: existing}
	case !hasSession:
		// Registered, and no session was ever created for it. This is the one
		// session arm that needs no live facts at all.
		view.Session = &agentreplv1.HostWorkspace_None{None: &agentreplv1.HostSessionNone{}}
	default:
		// A session record with no live facts: the session's identity lives
		// with the party that minted it, and HostSessionExisting.id is not
		// optional. An empty id would be a sentinel, and a client correlating
		// on it would correlate wrongly, so the view is WITHHELD and the gap
		// is recorded rather than papered over.
		log.Error(op, "a session record has no live facts; the host view was withheld",
			dlog.Context{
				"invariant_violation": "the session's host identity is unknown to this daemon",
				"remediation":         "the session fleet must answer HostSessionFacts for every session it holds",
			})
		return nil, false
	}
	return view, true
}

// hostExisting composes the `existing` arm: the session's identity, common to
// every standing, and the standing itself.
func (s *server) hostExisting(
	ctx context.Context,
	log dlog.Logger,
	ws ids.WorkspaceID,
	session wsm.Session,
	hasSession bool,
	facts HostFacts,
) (*agentreplv1.HostSessionExisting, bool) {
	const op = "daemon.server.compose_host_workspace"
	if facts.SessionID == "" {
		log.Error(op, "the session facts name no session identity; the host view was withheld",
			dlog.Context{
				"invariant_violation": "HostSessionFacts answered without a session id",
				"remediation":         "mint the host session identity where the session is created",
			})
		return nil, false
	}
	out := &agentreplv1.HostSessionExisting{
		Id: &agentreplv1.HostSessionId{Value: facts.SessionID},
	}

	// TERMINAL WINS. A session whose record carries a death is terminal no
	// matter what the fleet still holds for it: the record is the durable
	// truth and the fleet's entry is what has not been reaped yet.
	if hasSession && session.Terminal != nil {
		out.Standing = &agentreplv1.HostSessionExisting_Terminal{
			Terminal: &agentreplv1.HostSessionTerminal{
				// A DELETED session refuses resurrection; every other death
				// leaves the vendor conversation resumable.
				Rehydratable: session.Terminal.Kind != terminalDeleted && session.VendorSessionID != "",
			},
		}
		return out, true
	}

	live, ok := s.hostLive(ctx, log, ws, session, hasSession, facts)
	if !ok {
		return nil, false
	}
	out.Standing = &agentreplv1.HostSessionExisting_Live{Live: live}
	return out, true
}

// terminalDeleted is wsm's spelling of the one death that refuses resurrection.
const terminalDeleted = "deleted"

// hostLive composes the `live` arm.
func (s *server) hostLive(
	ctx context.Context,
	log dlog.Logger,
	ws ids.WorkspaceID,
	session wsm.Session,
	hasSession bool,
	facts HostFacts,
) (*agentreplv1.HostSessionLive, bool) {
	const op = "daemon.server.compose_host_workspace"
	if facts.Generation == "" {
		log.Error(op, "the session facts name no controller generation; the host view was withheld",
			dlog.Context{
				"invariant_violation": "HostSessionFacts answered without a generation",
				"remediation":         "the fleet's per-workspace generation must ride the facts",
			})
		return nil, false
	}
	out := &agentreplv1.HostSessionLive{
		Generation:   &agentreplv1.HostGenerationId{Value: facts.Generation},
		ShimAttached: facts.ShimAttached,
		Backfill:     hostBackfill(facts),
	}
	// The vendor arm is UNSET until a vendor conversation exists, which is
	// what an empty vendor session id means.
	if hasSession && session.VendorSessionID != "" {
		out.VendorInfo = &agentreplv1.HostSessionLive_Claude{
			Claude: &agentreplv1.HostVendorClaude{
				SessionId: session.VendorSessionID,
				ConfigDir: session.ConfigDir,
			},
		}
	}
	if !s.setHostComposer(ctx, log, ws, out) {
		return nil, false
	}
	out.Faults = s.hostFaults(ctx, log, ws)
	return out, true
}

// hostComposer resolves the composer gate from the OCCUPANCY LEASE. The lease
// is the one place that knows a session is spoken for, so the gate is read
// from it rather than assembled from each holder's own signal — which is what
// keeps the gate and the prompt queue's refusal from disagreeing.
// It SETS the arm on out rather than returning one because the generated
// oneof wrapper interface is unexported: no type outside the proto package can
// name it.
func (s *server) setHostComposer(
	ctx context.Context,
	log dlog.Logger,
	ws ids.WorkspaceID,
	out *agentreplv1.HostSessionLive,
) bool {
	const op = "daemon.server.compose_host_workspace"
	lease, held, err := s.deps.DB.Lease(ctx, ws)
	if err != nil {
		log.Error(op, "could not read the occupancy lease", dlog.Context{"cause": err.Error()})
		return false
	}
	if !held {
		out.Composer = &agentreplv1.HostSessionLive_Open{Open: &agentreplv1.HostComposerOpen{}}
		return true
	}
	switch lease.Holder {
	case wsm.HolderMerge:
		// The merge's two shapes are told apart by the POLICY, not by the
		// holder: a merge parked for guidance leaves the composer open and
		// routes what is typed to the resolution agent.
		if lease.Policy == wsm.PolicyParked {
			out.Composer = &agentreplv1.HostSessionLive_MergeParked{
				MergeParked: &agentreplv1.HostComposerMergeParked{}}
			return true
		}
		out.Composer = &agentreplv1.HostSessionLive_Merging{Merging: &agentreplv1.HostComposerMerging{}}
		return true
	case wsm.HolderRestart:
		out.Composer = &agentreplv1.HostSessionLive_Restarting{
			Restarting: &agentreplv1.HostComposerRestarting{}}
		return true
	case wsm.HolderDrain, wsm.HolderHibernate:
		// Hibernation is a drain of one workspace, and the composer arm the
		// contract spells for "the daemon is standing this down" is draining.
		out.Composer = &agentreplv1.HostSessionLive_Draining{
			Draining: &agentreplv1.HostComposerDraining{}}
		return true
	default:
		log.Error(op, "the occupancy lease names a holder with no composer arm", dlog.Context{
			"holder":              int(lease.Holder),
			"invariant_violation": "every lease holder must state what it does to the composer",
		})
		return false
	}
}

// hostFaults renders the workspace's open faults onto the live arm. A fault
// read that fails is recorded and answers NO faults rather than withholding
// the whole view: the faults supplement the standing, and a host that cannot
// see them is better off than a host that sees nothing at all.
func (s *server) hostFaults(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID) []*agentreplv1.HostFault {
	const op = "daemon.server.compose_host_workspace"
	scope := ws
	open, err := s.deps.Health.OpenFaults(ctx, wsm.FaultScope{Workspace: &scope})
	if err != nil {
		log.Error(op, "could not read the workspace's open faults", dlog.Context{"cause": err.Error()})
		return nil
	}
	out := make([]*agentreplv1.HostFault, 0, len(open))
	for _, f := range open {
		out = append(out, hostFault(f))
	}
	if len(out) == 0 {
		return nil
	}
	return out
}

// hostFault renders one recorded fault as the host stream's own HostFault. The
// kinds are SessionHealth's kinds and the arm messages are the same messages:
// a session's fault classes do not change because the host stream is what
// reports them.
func hostFault(f wsm.Fault) *agentreplv1.HostFault {
	out := &agentreplv1.HostFault{Detail: f.Detail, OpenedAtMs: f.OpenedAt.UnixMilli()}
	if out.Detail == "" {
		out.Detail = f.Kind
	}
	switch f.Kind {
	case health.KindShimStartFailed:
		out.Kind = &agentreplv1.HostFault_ShimStartFailed{
			ShimStartFailed: &agentreplv1.SessionFaultShimStartFailed{
				ExitCode:   faultExitCode(f),
				StderrTail: f.Evidence["stderr_tail"],
			},
		}
	case health.KindShimDied:
		out.Kind = &agentreplv1.HostFault_ShimDied{
			ShimDied: &agentreplv1.SessionFaultShimDied{ExitCode: faultExitCode(f)},
		}
	case health.KindLinkSevered:
		out.Kind = &agentreplv1.HostFault_LinkSevered{
			LinkSevered: &agentreplv1.SessionFaultLinkSevered{}}
	case health.KindResumeFailed:
		out.Kind = &agentreplv1.HostFault_ResumeFailed{
			ResumeFailed: &agentreplv1.SessionFaultResumeFailed{Cause: faultEvidence(f, "cause")},
		}
	case health.KindBounceDied:
		out.Kind = &agentreplv1.HostFault_BounceDied{
			BounceDied: &agentreplv1.SessionFaultBounceDied{}}
	case health.KindBounceUnknown:
		out.Kind = &agentreplv1.HostFault_BounceUnknown{
			BounceUnknown: &agentreplv1.SessionFaultBounceUnknown{}}
	case health.KindClassifierFailed:
		out.Kind = &agentreplv1.HostFault_ClassifierFailed{
			ClassifierFailed: &agentreplv1.SessionFaultClassifierFailed{Detail: faultEvidence(f, "detail")},
		}
	case health.KindShimReported:
		out.Kind = &agentreplv1.HostFault_ShimReported{
			ShimReported: &agentreplv1.SessionFaultShimReported{
				Component: f.Evidence["component"],
				Kind:      f.Evidence["kind"],
			},
		}
	}
	// A kind with no typed arm keeps its detail line: an unreportable fault is
	// still a fault, exactly as the health reporter treats one.
	return out
}

// faultExitCode reads a recorded exit code, answering zero when the record
// carries none — the arm's field is not optional, and an absent exit code is
// reported through the detail line rather than invented as a number.
func faultExitCode(f wsm.Fault) int32 {
	return health.EvidenceExitCode(f)
}

// faultEvidence reads one evidence value, falling back to the fault's prose
// detail so an arm is never empty when the record said something.
func faultEvidence(f wsm.Fault, key string) string {
	if v := f.Evidence[key]; v != "" {
		return v
	}
	return f.Detail
}

// hostNaming composes what Emacs names buffers from. Both fields are OPTIONAL
// and stay unset rather than blank: the slug is unset until the daemon has
// derived one, and the title until the vendor supplies one.
func hostNaming(record wsm.Workspace) *agentreplv1.HostWorkspaceNaming {
	out := &agentreplv1.HostWorkspaceNaming{}
	if record.Name != "" {
		slug := record.Name
		out.Slug = &slug
	}
	return out
}

// hostBackfill renders the transcript's backfill state.
func hostBackfill(facts HostFacts) *agentreplv1.HostBackfill {
	out := &agentreplv1.HostBackfill{}
	switch facts.Backfill {
	case BackfillPending:
		out.State = &agentreplv1.HostBackfill_Pending{Pending: &agentreplv1.HostBackfillPending{}}
	case BackfillDone:
		out.State = &agentreplv1.HostBackfill_Done{Done: &agentreplv1.HostBackfillDone{}}
	case BackfillFailed:
		out.State = &agentreplv1.HostBackfill_Failed{
			Failed: &agentreplv1.HostBackfillFailed{Detail: facts.BackfillDetail}}
	default:
		out.State = &agentreplv1.HostBackfill_None{None: &agentreplv1.HostBackfillNone{}}
	}
	return out
}
