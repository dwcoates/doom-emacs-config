package frontend

// paintgate.go — the delivery-order sequencer for WorkspaceState (F5).
//
// THE PROBLEM IT REMOVES. The SSM resolves a workspace's render state once and
// the frontend server used to fan it to every connected frontend at the same
// instant. Two frontends draw that one fact on two surfaces — the webview's
// footer and cards, Emacs's tab bar — and nothing ordered the two, so the
// surfaces could visibly disagree for as long as either took to paint. A green
// tab beside a yellow dot was observed in production, and "each side is fast,
// so they rarely disagree" is a probability, not a guarantee.
//
// THE ORDER. Every state update now travels: resolver → the frontends that
// PAINT it → their acknowledgment → the frontends that merely OBSERVE it. A
// state Emacs is showing has therefore already been drawn (or explicitly
// declined) by every painting frontend, for every state in the vocabulary
// rather than only for green.
//
// WHY IT IS A GATE AND NOT A DELAY. The release condition is an acknowledgment
// carrying the emission's own generation, so the sequencer never guesses that
// enough time has passed. A stale acknowledgment names an older generation and
// settles nothing; a frontend that cannot paint says so and settles the state
// anyway; a frontend that goes away stops being counted and the state settles
// immediately. There is no branch in here whose correctness depends on timing.
//
// WHY IT LIVES BESIDE THE CLIENT REGISTRY. Every decision it makes — is anyone
// painting this workspace, does this acknowledgment settle the held state, did
// the last painter just leave — is taken under the SAME lock that guards the
// connection set, and the delivery into each connection's queue happens while
// that lock is still held. That is what makes the mode transitions race-free
// rather than merely unlikely: a connect, a disconnect, an emission and an
// acknowledgment cannot interleave, and the order the sequencer decided is the
// order the observer's queue carries.

import (
	"sort"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/dlog"

	"google.golang.org/protobuf/proto"
)

// Role names what a frontend connection does with a workspace's render state.
//
// It is fixed when the connection is accepted, from the endpoint that accepted
// it, so there is no window in which a connection exists with an undecided
// role. Declaring it in a first frame instead would create exactly that
// window: the connection would be registered, and a state could be emitted
// against it, before it had said what it was.
type Role int

const (
	// RoleObserver renders a state it is given and attests nothing. The Emacs
	// tab bar is the production observer; so is the webapp's short-lived
	// bootstrap socket, which exists only to create a session and close.
	RoleObserver Role = iota
	// RolePainter draws the state onto surfaces a user reads and reports what
	// the render produced. The webapp's session view is the production
	// painter.
	RolePainter
)

func (r Role) String() string {
	if r == RolePainter {
		return "painter"
	}
	return "observer"
}

// ClientKind names the frontend product/transport behind a connection.  It is
// intentionally independent of Role: both Emacs and GUI bootstrap are
// observers, but only the Emacs host may receive or complete host actions.
// The endpoint selects it at accept time; clients never self-assert it.
type ClientKind int

const (
	ClientKindHost         ClientKind = iota // the Emacs frontend UDS host
	ClientKindGUIBootstrap                   // unscoped /frontend bootstrap WebSocket
	ClientKindGUIPainter                     // scoped /sessions/{id}/stream painter
	ClientKindGUIObserver                    // scoped GUI observer/test connection
)

func (k ClientKind) String() string {
	switch k {
	case ClientKindHost:
		return "host"
	case ClientKindGUIBootstrap:
		return "gui_bootstrap"
	case ClientKindGUIPainter:
		return "gui_painter"
	case ClientKindGUIObserver:
		return "gui_observer"
	default:
		return "unknown"
	}
}

func (k ClientKind) isHost() bool { return k == ClientKindHost }

// gateEntry is one workspace's delivery bookkeeping.
type gateEntry struct {
	// gen is the last generation stamped for this workspace, strictly
	// increasing. pending, when set, always carries exactly this generation.
	gen uint64
	// pending is the newest emission, withheld from observers until a painter
	// settles it. Nil when nothing is held.
	pending *frontendv1.WorkspaceState
	// released is the newest emission observers have been given. Nil until the
	// workspace has settled once, which is the honest answer to "what is Emacs
	// showing for this workspace" before it has been told anything.
	released *frontendv1.WorkspaceState
}

// newest returns the state a PAINTING frontend should be looking at: the held
// emission when one is held, else the last settled one.
func (e *gateEntry) newest() *frontendv1.WorkspaceState {
	if e.pending != nil {
		return e.pending
	}
	return e.released
}

// paintGate sequences WorkspaceState delivery per workspace. Every method
// requires the frontend Server's client-registry lock to be held; the type owns
// no lock of its own precisely so that "who is connected" and "what is held"
// can never be read at two different instants.
type paintGate struct {
	logf    dlog.Logf
	entries map[string]*gateEntry
}

func newPaintGate(logf dlog.Logf) *paintGate {
	return &paintGate{logf: logf, entries: map[string]*gateEntry{}}
}

// entryLocked returns the workspace's bookkeeping, creating it empty on first
// sight.
func (g *paintGate) entryLocked(workspace string) *gateEntry {
	if e, ok := g.entries[workspace]; ok {
		return e
	}
	e := &gateEntry{}
	g.entries[workspace] = e
	return e
}

// emitLocked stamps ws with its workspace's next generation and records the
// emission. painting says whether any connected frontend will paint this
// workspace and acknowledge it.
//
// Returns the stamped state (the ONLY copy anything should send) and whether
// observers may have it now. With no painting frontend the emission settles
// immediately, which is the same delivery the sequencer produces once a
// painter acknowledges — the two modes differ in when observers are told, never
// in what they are told.
func (g *paintGate) emitLocked(ws *frontendv1.WorkspaceState, painting bool) (*frontendv1.WorkspaceState, bool) {
	e := g.entryLocked(ws.GetWorkspace())
	e.gen++
	stamped := proto.Clone(ws).(*frontendv1.WorkspaceState)
	stamped.Generation = e.gen
	if painting {
		e.pending = stamped
		return stamped, false
	}
	e.pending = nil
	e.released = stamped
	return stamped, true
}

// settleLocked releases a workspace's held state on an acknowledgment covering
// generation, and returns the state observers should now be sent (nil when
// nothing settles).
//
// A generation BELOW the held one is a stale acknowledgment: it describes a
// render of an emission the resolver has already superseded, and honoring it
// would put a state on the tab bar that no frontend has drawn. That is the one
// failure this identity exists to make impossible, so it is logged and
// dropped rather than tolerated.
func (g *paintGate) settleLocked(workspace string, generation uint64) *frontendv1.WorkspaceState {
	e, ok := g.entries[workspace]
	if !ok || e.pending == nil {
		return nil
	}
	if generation < e.pending.GetGeneration() {
		g.logf("frontend: paint ack stale ws=%s generation=%d held=%d — the held state stays held",
			workspace, generation, e.pending.GetGeneration())
		return nil
	}
	st := e.pending
	e.pending = nil
	e.released = st
	return st
}

// orphanedLocked releases every held state whose workspace no longer has a
// painting frontend, in stable workspace order, and returns them in that order.
//
// stillPainting is the Server's live read of the connection set, taken under
// the same lock: a painter that disappeared cannot acknowledge anything, so
// holding its workspace's state would wedge the observer forever. Releasing it
// here is the same code path a workspace with no painter at all takes, which is
// why a disconnect mid-flight neither drops the held state nor delivers it
// twice — it is delivered exactly once, by whichever of the two paths reaches
// it first, and the entry is cleared before the delivery.
func (g *paintGate) orphanedLocked(stillPainting func(workspace, sessionID string) bool) []*frontendv1.WorkspaceState {
	names := make([]string, 0, len(g.entries))
	for ws, e := range g.entries {
		if e.pending != nil {
			names = append(names, ws)
		}
	}
	sort.Strings(names)
	out := make([]*frontendv1.WorkspaceState, 0, len(names))
	for _, ws := range names {
		e := g.entries[ws]
		if stillPainting(ws, e.pending.GetSessionId()) {
			continue
		}
		g.logf("frontend: no painting frontend left for ws=%s — settling held generation %d",
			ws, e.pending.GetGeneration())
		st := e.pending
		e.pending = nil
		e.released = st
		out = append(out, st)
	}
	return out
}

// unpaintedLocked names the workspaces a departed painter was covering that no
// other painter covers now — the ones whose paint attestation must be
// withdrawn, because the renderer that made it is gone.
//
// scope is the departed connection's scope (nil for an unscoped painter, which
// covered everything). A workspace still served by another painter keeps its
// attestation: that painter's claim is still current.
func (g *paintGate) unpaintedLocked(scope *Scope, stillPainting func(workspace, sessionID string) bool) []string {
	names := make([]string, 0, len(g.entries))
	for ws, e := range g.entries {
		if scope != nil && !scope.matches(sessionOf(e), ws) {
			continue
		}
		if stillPainting(ws, sessionOf(e)) {
			continue
		}
		names = append(names, ws)
	}
	sort.Strings(names)
	return names
}

// sessionOf reports the session id of a workspace's newest emission, or "" when
// the sequencer has emitted nothing for it yet.
func sessionOf(e *gateEntry) string {
	if st := e.newest(); st != nil {
		return st.GetSessionId()
	}
	return ""
}

// snapshotLocked rewrites snap's workspace states into the view role is
// entitled to.
//
// A painter is shown the newest emission, so what it renders is exactly what
// its acknowledgment will settle — the two can never describe different states.
// An observer is shown only what has settled, and a workspace whose first
// emission is still held is OMITTED rather than substituted: "Emacs has not
// been told yet" is a real answer, and inventing one would reintroduce the
// divergence on the reconnect path that the push path just closed.
//
// A workspace the sequencer has never seen is ADOPTED as already settled.
// Nothing is holding it, no painter was ever asked to draw it, and both
// audiences read it from their own connect snapshot — so there is nothing for
// an acknowledgment to release and no divergence to prevent. Holding it instead
// would wedge the observer behind an acknowledgment nobody could ever send.
func (g *paintGate) snapshotLocked(snap *frontendv1.StateSnapshot, role Role) *frontendv1.StateSnapshot {
	if snap == nil {
		return &frontendv1.StateSnapshot{}
	}
	out := proto.Clone(snap).(*frontendv1.StateSnapshot)
	states := make([]*frontendv1.WorkspaceState, 0, len(out.GetWorkspaces()))
	for _, w := range out.GetWorkspaces() {
		e := g.entryLocked(w.GetWorkspace())
		if e.newest() == nil {
			e.gen++
			adopted := proto.Clone(w).(*frontendv1.WorkspaceState)
			adopted.Generation = e.gen
			e.released = adopted
		}
		var view *frontendv1.WorkspaceState
		if role == RolePainter {
			view = e.newest()
		} else {
			view = e.released
		}
		if view == nil {
			continue
		}
		states = append(states, view)
	}
	out.Workspaces = states
	return out
}

// settlesDelivery reports whether a paint acknowledgment may settle a held
// state. Both real outcomes do: a frontend that drew the state and a frontend
// that reported it cannot draw have each said everything the sequencer needs,
// and the second is what keeps a hidden webview from wedging the tab bar. An
// acknowledgment naming no outcome settles nothing — it is a malformed frame,
// and the handler refuses it loudly.
func settlesDelivery(cmd *frontendv1.PaintAckCmd) bool {
	switch cmd.GetOutcome() {
	case frontendv1.PaintOutcome_PAINT_OUTCOME_PAINTED,
		frontendv1.PaintOutcome_PAINT_OUTCOME_SUSPENDED:
		return true
	default:
		return false
	}
}

// AttestsPaint reports whether a paint acknowledgment attests an actual render,
// which is the only outcome that may advance the SSM's paint watermark.
//
// A SUSPENDED acknowledgment must NOT: it says the frontend holds the state and
// cannot draw it, and greening a workspace on that would be the precise lie the
// paint attestation exists to prevent.
func AttestsPaint(cmd *frontendv1.PaintAckCmd) bool {
	return cmd.GetOutcome() == frontendv1.PaintOutcome_PAINT_OUTCOME_PAINTED
}
