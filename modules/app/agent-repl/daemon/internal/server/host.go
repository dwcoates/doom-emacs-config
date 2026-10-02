package server

import (
	"context"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

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
		// A CANCELLED CONTEXT IS THE CLIENT LEAVING. This publish runs under
		// the host watch's own context on the composing path, and that context
		// is cancelled the moment the client goes away or the daemon exits in
		// an orderly way. The view is simply not published; there is nobody
		// left to publish it to, and it is not a fault.
		if endedOnCancel(err) {
			s.log.Info(op, "the host view was not published; the stream's context was cancelled",
				dlog.Context{
					"stream": "WatchHostWorkspace", "workspace": string(ws), "cause": err.Error(),
				})
			return
		}
		s.log.Error(op, "could not resolve the workspace", dlog.Context{
			"workspace": string(ws), "cause": err.Error(),
		})
		return
	}
	// THE WEB LINK'S IDENTITY MOVES ON EXACTLY THESE EDGES. It is composed
	// from the same session facts the host view's identity is, so giving it a
	// second set of publish sites would only give it a way to drift from them.
	s.publishWebSessionIdentity(ctx, log, ws)
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
		if endedOnCancel(err) {
			log.Info(op, "the host view was not composed; the stream's context was cancelled",
				dlog.Context{"stream": "WatchHostWorkspace", "cause": err.Error()})
			return nil, false
		}
		log.Error(op, "could not read the workspace record", dlog.Context{"cause": err.Error()})
		return nil, false
	}
	session, hasSession, err := s.deps.DB.Session(ctx, ws)
	if err != nil {
		if endedOnCancel(err) {
			log.Info(op, "the host view was not composed; the stream's context was cancelled",
				dlog.Context{"stream": "WatchHostWorkspace", "cause": err.Error()})
			return nil, false
		}
		log.Error(op, "could not read the session record", dlog.Context{"cause": err.Error()})
		return nil, false
	}

	view := &agentreplv1.HostWorkspace{Naming: hostNaming(record)}
	// THE STANDING HELD-PROMPT EDIT is state, so it rides every composition:
	// a subscriber always reads the edit that stands now, and its absence is
	// what tells the editor the edit ended.
	if edit, editing := s.deps.Queue.Editing(ws); editing {
		view.HeldPromptEdit = &agentreplv1.HostHeldPromptEdit{
			Turn: &conversationv1.TurnId{Value: string(edit.Turn)},
			Said: edit.Said,
			Edit: edit.ID,
		}
		log.Debug(op, "the host view carries the standing held-prompt edit",
			dlog.Context{"turn": string(edit.Turn), "edit": edit.ID})
	}

	facts, live := s.deps.SessionFacts.HostSessionFacts(ws)
	switch {
	case live:
		s.clearAwaitedHostIdentity(ws)
		existing, ok := s.hostExisting(ctx, log, ws, session, hasSession, facts)
		if !ok {
			return nil, false
		}
		view.Session = &agentreplv1.HostWorkspace_Existing{Existing: existing}
	case !hasSession:
		// Registered, and no session was ever created for it. This is the one
		// session arm that needs no live facts at all.
		s.clearAwaitedHostIdentity(ws)
		view.Session = &agentreplv1.HostWorkspace_None{None: &agentreplv1.HostSessionNone{}}
	case session.HostSessionID != "":
		s.clearAwaitedHostIdentity(ws)
		// A SESSION THIS DAEMON DOES NOT OPERATE is an ordinary state, not a
		// gap: it was killed, it was handed to a successor, or it has not
		// been opened since this daemon booted. The record carries the
		// identity the host stream correlates on, so the arm is composed from
		// it -- terminal when the record carries a death, and live with the
		// shim UNATTACHED otherwise, which is exactly what "live but
		// momentarily unwired" means.
		log.Debug(op, "the session is recorded but not operated here; composing from the record",
			dlog.Context{"host_session_id": session.HostSessionID})
		existing, ok := s.hostExisting(ctx, log, ws, session, hasSession, HostFacts{
			SessionID: session.HostSessionID,
			// The generation belongs to the party OPERATING the session, and
			// nobody is; zero is the honest answer for "no controller".
			Generation:   "0",
			ShimAttached: false,
			Backfill:     BackfillNone,
		})
		if !ok {
			return nil, false
		}
		view.Session = &agentreplv1.HostWorkspace_Existing{Existing: existing}
	default:
		// A session record with NO IDENTITY AT ALL. The identity is minted
		// where the session is created, and HostSessionExisting.id is not
		// optional: an empty id would be a sentinel, and a client correlating
		// on it would correlate wrongly. The view is WITHHELD either way.
		//
		// A RESOURCE REQUESTED BEFORE ITS PRODUCER HAS PUBLISHED IS A STARTUP
		// TRANSIENT. Right after Emacs subscribes to WatchHostWorkspace, the
		// shim may not yet have described the session, so the identity is
		// legitimately absent for a moment. The FIRST withholding per workspace
		// is DEBUG, and the missing identity only escalates to the ERROR that
		// names a mint defect once it has persisted past
		// hostIdentityDescribeBound.
		if elapsed, transient := s.awaitHostIdentity(ws); transient {
			log.Debug(op, "awaiting host identity; the session is not yet described, so the host view was withheld",
				dlog.Context{
					"reason":         "host session id not yet minted",
					"withheld_for":   elapsed.String(),
					"escalate_after": hostIdentityDescribeBound.String(),
				})
			return nil, false
		} else {
			log.Error(op, "a session record carries no host identity; the host view was withheld",
				dlog.Context{
					"invariant_violation": "a session record exists with no host session id",
					"remediation":         "mint the host session identity where the session is created",
					"withheld_for":        elapsed.String(),
				})
			return nil, false
		}
	}
	return view, true
}

// awaitHostIdentity records the first time a workspace's host identity was
// found missing and reports how long it has been missing since, and whether
// that is still within the startup-transient window. The first observation is
// always transient: the shim has just been asked for a session it may not have
// described yet.
func (s *server) awaitHostIdentity(ws ids.WorkspaceID) (time.Duration, bool) {
	s.mu.Lock()
	defer s.mu.Unlock()
	now := s.now()
	first, ok := s.hostIdentityAwaited[ws]
	if !ok {
		s.hostIdentityAwaited[ws] = now
		return 0, true
	}
	elapsed := now.Sub(first)
	return elapsed, elapsed <= hostIdentityDescribeBound
}

// clearAwaitedHostIdentity forgets any pending missing-identity window for a
// workspace, because its host view was composed with an identity: the next
// withholding, if any, starts a fresh transient window rather than inheriting a
// stale one.
func (s *server) clearAwaitedHostIdentity(ws ids.WorkspaceID) {
	s.mu.Lock()
	defer s.mu.Unlock()
	delete(s.hostIdentityAwaited, ws)
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
	// A HIBERNATION IS A PARK, NOT A DEATH. The idle sweep stands the shim
	// down and a prompt brings the session straight back, so the host keeps
	// the LIVE arm with the shim UNATTACHED — which is exactly what "live but
	// momentarily unwired" means, and is what keeps a parked workspace
	// indistinguishable from an idle one on the frontend.
	if hasSession && session.Terminal != nil && session.Terminal.Kind != terminalHibernated {
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

// terminalHibernated is wsm's spelling of the idle sweep's PARK. It is not a
// death: the session is recoverable by a prompt, so it never composes the
// terminal arm.
const terminalHibernated = "hibernated"

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
		if endedOnCancel(err) {
			log.Info(op, "the occupancy lease was not read; the stream's context was cancelled",
				dlog.Context{"stream": "WatchHostWorkspace", "cause": err.Error()})
			return false
		}
		log.Error(op, "could not read the occupancy lease", dlog.Context{"cause": err.Error()})
		return false
	}
	if !held {
		out.Composer = &agentreplv1.HostSessionLive_Open{Open: &agentreplv1.HostComposerOpen{}}
		return true
	}
	switch lease.Holder {
	case wsm.HolderMerge:
		// A HOLDING MERGE LEASE LEAVES THE COMPOSER OPEN: what is submitted
		// while the merge runs is held until it ends, so the gate agrees with
		// the queue. A merge lease an older build wrote refuses, and closes it.
		if lease.Policy == wsm.PolicyHold {
			out.Composer = &agentreplv1.HostSessionLive_Open{Open: &agentreplv1.HostComposerOpen{}}
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
		if endedOnCancel(err) {
			log.Info(op, "the open faults were not read; the stream's context was cancelled",
				dlog.Context{"stream": "WatchHostWorkspace", "cause": err.Error()})
			return nil
		}
		log.Error(op, "could not read the workspace's open faults", dlog.Context{"cause": err.Error()})
		return nil
	}
	out := make([]*agentreplv1.HostFault, 0, len(open))
	var withheld []string
	for _, f := range open {
		rendered, ok := hostFault(f)
		if !ok {
			withheld = append(withheld, f.Kind)
			continue
		}
		out = append(out, rendered)
	}
	if len(withheld) > 0 {
		// DEBUG, not an error: the fault is already recorded once by the layer
		// that opened it, and this line would otherwise repeat on every render
		// of a standing fault. What it says is that the WIRE cannot carry it.
		log.Debug(op, "standing faults have no HostFault arm; they are withheld from the view",
			dlog.Context{"kinds": withheld})
	}
	if len(out) == 0 {
		return nil
	}
	return out
}

// hostFault renders one recorded fault as the host stream's own HostFault,
// reporting false when the fault's kind has no arm to render it through.
//
// THE ARM IS THE FAULT CLASS. `HostFault.kind' is a oneof Emacs REFUSES when it
// is unset, and it refuses the WHOLE WatchHostWorkspace push with it -- one
// unrenderable fault used to cost the editor the entire host view of the
// workspace. A kind with no arm is withheld from the wire rather than sent as a
// prose-only line; it stays recorded, and loud, at the site that opened it.
//
// The kinds are SessionHealth's kinds and the arm messages are the same
// messages: a session's fault classes do not change because the host stream is
// what reports them.
func hostFault(f wsm.Fault) (*agentreplv1.HostFault, bool) {
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
	case health.KindResumeFailed, health.KindRelaunchResumeFailed:
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
	case health.KindConversationAbandoned:
		out.Kind = &agentreplv1.HostFault_ConversationAbandoned{
			ConversationAbandoned: &agentreplv1.SessionFaultConversationAbandoned{
				VendorSessionId: f.Evidence["vendor_session_id"],
			},
		}
	case health.KindSessionAbsent:
		out.Kind = &agentreplv1.HostFault_SessionAbsent{
			SessionAbsent: &agentreplv1.SessionFaultSessionAbsent{}}
	case health.KindWatchOpenRefused:
		out.Kind = &agentreplv1.HostFault_WatchOpenRefused{
			WatchOpenRefused: &agentreplv1.SessionFaultWatchOpenRefused{
				Operation: f.Evidence["operation"],
				Handle:    f.Evidence["handle"],
			},
		}
	case health.KindStateUnreadable:
		out.Kind = &agentreplv1.HostFault_DaemonStateUnreadable{
			DaemonStateUnreadable: &agentreplv1.SessionFaultDaemonStateUnreadable{
				Cause: faultEvidence(f, "cause"),
			},
		}
	case health.KindAdoptionWindowExpired:
		out.Kind = &agentreplv1.HostFault_AdoptionWindowExpired{
			AdoptionWindowExpired: &agentreplv1.SessionFaultAdoptionWindowExpired{
				AdoptionWindow: f.Evidence["adoption_window"],
			},
		}
	case health.KindFinalAnswerUnresolved:
		out.Kind = &agentreplv1.HostFault_FinalAnswerUnresolved{
			FinalAnswerUnresolved: &agentreplv1.SessionFaultFinalAnswerUnresolved{
				Turn: f.Evidence["turn"],
				Unit: f.Evidence["unit"],
				Why:  f.Evidence["why"],
			},
		}
	// THE VENDOR-START ARMS carry the very SessionFault* messages, read off
	// the evidence by health's own renderers so the two surfaces agree.
	case health.KindVendorStartRetrying:
		out.Kind = &agentreplv1.HostFault_VendorStartRetrying{VendorStartRetrying: health.VendorStartRetryingArm(f)}
	case health.KindVendorStartRejected:
		out.Kind = &agentreplv1.HostFault_VendorStartRejected{VendorStartRejected: health.VendorStartRejectedArm(f)}
	case health.KindVendorStartFailed:
		out.Kind = &agentreplv1.HostFault_VendorStartFailed{VendorStartFailed: health.VendorStartFailedArm(f)}
	default:
		return nil, false
	}
	return out, true
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
