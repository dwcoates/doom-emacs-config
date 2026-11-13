// Package rostermirror is a JOINING daemon's look at its incumbent's roster:
// for every workspace the successor has not adopted yet, the status arm the
// incumbent's own roster draws.
//
// WHY IT EXISTS. During a handover Emacs takes its whole roster stream from
// the successor (lisp/roster.el, `agent-repl-roster-on-link-handover`), but
// the successor's row for a workspace it has not adopted comes from its own
// footer state, which is frozen at what it read before the incumbent kept
// serving the workspace. On 2026-11-13 a mid-work holdout
// (eval-cache-torch-human) stayed `idle_async` in the sidebar while the
// incumbent published `thinking` six times over twelve minutes. The daemon
// that serves a workspace is the one that can say its status, so until the
// successor adopts it the row is drawn from the incumbent.
//
// IT IS THE ONE DAEMON-TO-DAEMON SUBSCRIPTION, and its scope is written down
// where the rule it excepts is stated (internal/rollout/adopt.go, Join): the
// joining window only, the roster only. It reads the incumbent's public
// WatchWorkspaceRoster stream, the same one every editor reads, so it adds no
// rpc, and it carries status only: ownership, state and every write stay with
// the manifest and the serving row.
//
// LIFETIME. Run subscribes once and ends with the first of: the incumbent's
// planned ending on the stream (it exits), the stream failing, Stop (the
// successor took over), or the caller's context (the successor itself is
// stopped, which is how an abandoned handover ends). At every end the mirror
// is withdrawn, so each row is this daemon's own again. Nothing is retried: a
// mirror that ended leaves the rows as this daemon resolves them, which is
// what they were before the mirror existed.
package rostermirror

import (
	"context"
	"errors"
	"fmt"
	"sync"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	"agentrepl/proto/agentrepl/v1/agentreplv1connect"
	frontendv1 "agentrepl/proto/frontend/v1"
	"agentrepl/protohelpers/rosterwalk"

	"agent-repld/internal/dlog"
	"agent-repld/internal/h2cclient"
	"agent-repld/internal/ids"
	"agent-repld/internal/protoarm"
	"agent-repld/internal/resolve/ladder"
)

// Op is every record's operation.
const Op = "daemon.rostermirror"

// Stream is the incumbent's roster stream as the mirror reads it: the shape
// of connect's server stream for WatchWorkspaceRoster.
type Stream interface {
	Receive() bool
	Msg() *agentreplv1.WatchWorkspaceRosterResponse
	Err() error
	Close() error
}

// Dial opens the incumbent's roster stream at ADDRESS.
type Dial func(ctx context.Context, address string) (Stream, error)

// DialIncumbent is the production Dial: WatchWorkspaceRoster over h2c, the
// call every editor makes.
func DialIncumbent(ctx context.Context, address string) (Stream, error) {
	stream, err := agentreplv1connect.NewAgentReplClient(h2cclient.Client(), "http://"+address).
		WatchWorkspaceRoster(ctx, connect.NewRequest(&agentreplv1.WatchWorkspaceRosterRequest{}))
	if err != nil {
		return nil, fmt.Errorf("rostermirror: WatchWorkspaceRoster at %s: %w", address, err)
	}
	return stream, nil
}

// Deps are what the mirror needs.
type Deps struct {
	// Log is where every record goes.
	Log dlog.Logger
	// Incumbent is the incumbent's address (the successor's --joining value).
	Incumbent string
	// Dial opens the incumbent's roster stream.
	Dial Dial
	// NotYetAdopted reports whether this daemon has not adopted WS yet: the
	// rollout controller's standing, the one authority on it. Only those rows
	// are mirrored.
	NotYetAdopted func(ws ids.WorkspaceID) bool
	// Sink is handed, WHOLE, every mirrored arm by workspace after every
	// change, and nil once the mirror ends (sidebar.Resolver.SetIncumbentArms).
	Sink func(arms map[ids.WorkspaceID]string)
}

// Mirror is one joining window's subscription.
type Mirror struct {
	deps Deps

	// pub serializes publish, so two changes can never reach the sink in the
	// reverse of the order they were computed in.
	pub sync.Mutex

	mu sync.Mutex
	// arms are the last incumbent roster's decodable status arms, by
	// workspace, nil before the first roster and after the end.
	arms map[ids.WorkspaceID]string
	// undecodable are the rows of the last roster whose status this build
	// cannot read, so each is logged when it becomes so, not on every push.
	undecodable map[ids.WorkspaceID]bool
	// ended is set once, at the mirror's end; nothing is mirrored after it.
	ended bool
	// cancel ends Run's subscription; nil until Run starts.
	cancel context.CancelFunc
	// stopReason is why Stop ended the mirror, empty when it did not.
	stopReason string
}

// New builds a mirror. Every dependency is required.
func New(deps Deps) (*Mirror, error) {
	switch {
	case deps.Log == nil:
		return nil, errors.New("rostermirror: a logger is required")
	case deps.Incumbent == "":
		return nil, errors.New("rostermirror: the incumbent's address is required")
	case deps.Dial == nil:
		return nil, errors.New("rostermirror: a dial is required")
	case deps.NotYetAdopted == nil:
		return nil, errors.New("rostermirror: the adoption standing is required; only a row not yet adopted may be mirrored")
	case deps.Sink == nil:
		return nil, errors.New("rostermirror: a sink is required")
	}
	return &Mirror{deps: deps}, nil
}

// Run subscribes to the incumbent's roster and mirrors it until the mirror
// ends (see the package comment). It blocks, and is called once.
func (m *Mirror) Run(ctx context.Context) {
	fields := dlog.Context{"incumbent": m.deps.Incumbent}
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	m.mu.Lock()
	if m.ended {
		reason := m.stopReason
		m.mu.Unlock()
		m.deps.Log.Debug(Op, "the mirror ended before it subscribed", dlog.Merge(fields, dlog.Context{"stopped_because": reason}))
		return
	}
	m.cancel = cancel
	m.mu.Unlock()

	stream, err := m.deps.Dial(ctx, m.deps.Incumbent)
	if err != nil {
		m.end(ctx, fields, "the incumbent's roster stream could not be opened; every row is this daemon's own", err)
		return
	}
	defer func() {
		if err := stream.Close(); err != nil {
			m.deps.Log.Debug(Op, "the incumbent's roster stream closed with an error", dlog.WithCause(fields, err))
		}
	}()
	m.deps.Log.Info(Op, "subscribed to the incumbent's roster; rows not yet adopted draw its status", fields)
	for stream.Receive() {
		switch push := stream.Msg().GetPush().(type) {
		case *agentreplv1.WatchWorkspaceRosterResponse_Roster:
			m.take(push.Roster, fields)
		case *agentreplv1.WatchWorkspaceRosterResponse_Ending:
			m.deps.Log.Info(Op, "the incumbent ended its roster stream; every row is this daemon's own", fields)
			m.withdraw()
			return
		default:
			m.deps.Log.Error(Op, "the incumbent pushed a roster frame this build cannot read; it is skipped", dlog.Merge(fields, dlog.Context{
				"arm":         fmt.Sprintf("%T", push),
				"remediation": "the incumbent is newer than its successor, which a deploy never makes; find who spawned it",
			}))
		}
	}
	err = stream.Err()
	if err == nil {
		err = errors.New("the stream ended without the incumbent's planned ending")
	}
	m.end(ctx, fields, "the incumbent's roster stream failed; every row is this daemon's own", err)
}

// end withdraws the mirror after its subscription ended with ERR: at DEBUG
// when the mirror's own context ended it (Stop, or the successor's
// lifetime), at ERROR otherwise.
func (m *Mirror) end(ctx context.Context, fields dlog.Context, message string, err error) {
	if ctx.Err() != nil {
		m.mu.Lock()
		reason := m.stopReason
		m.mu.Unlock()
		m.deps.Log.Debug(Op, "the mirror's subscription ended with the mirror", dlog.WithCause(dlog.Merge(fields, dlog.Context{"stopped_because": reason}), err))
	} else {
		m.deps.Log.Error(Op, message, dlog.WithCause(fields, err))
	}
	m.withdraw()
}

// Stop ends the mirror for REASON: the handover it served is over. The
// subscription is cancelled and every row is this daemon's own from here on.
// It may be called before Run, and more than once.
func (m *Mirror) Stop(reason string) {
	m.mu.Lock()
	already := m.ended
	if !already {
		m.stopReason = reason
	}
	cancel := m.cancel
	m.mu.Unlock()
	if already {
		return
	}
	m.deps.Log.Info(Op, "the mirror stopped; every row is this daemon's own", dlog.Context{
		"incumbent": m.deps.Incumbent, "stopped_because": reason,
	})
	if cancel != nil {
		cancel()
	}
	m.withdraw()
}

// OwnershipChanged republishes the mirror after a workspace's standing moved
// (an adoption, a create): a row this daemon now owns is no longer drawn from
// the incumbent.
func (m *Mirror) OwnershipChanged() {
	m.publish()
}

// withdraw ends the mirror: nothing it held is drawn any more.
func (m *Mirror) withdraw() {
	m.mu.Lock()
	m.ended = true
	m.arms = nil
	m.undecodable = nil
	m.mu.Unlock()
	m.publish()
}

// take installs the incumbent's ROSTER. A row whose status this build cannot
// read (an unset oneof, as an arm a newer or older schema names and this one
// does not decodes, or an arm no claim places) is logged at ERROR when it
// becomes so and is left out, so it keeps this daemon's own state.
func (m *Mirror) take(roster *frontendv1.WorkspaceRoster, fields dlog.Context) {
	arms := map[ids.WorkspaceID]string{}
	undecodable := map[ids.WorkspaceID]bool{}
	var fresh []dlog.Context
	m.mu.Lock()
	if m.ended {
		m.mu.Unlock()
		return
	}
	for _, row := range rosterwalk.AllRows(roster) {
		ws := ids.WorkspaceID(row.GetWorkspace().GetWorkspace().GetId())
		arm, problem := statusOf(row)
		if problem != "" {
			undecodable[ws] = true
			if !m.undecodable[ws] {
				fresh = append(fresh, dlog.Merge(fields, dlog.Context{
					"workspace_id": string(ws), "arm": arm, "problem": problem,
				}))
			}
			continue
		}
		arms[ws] = arm
	}
	m.arms, m.undecodable = arms, undecodable
	m.mu.Unlock()
	for _, ctx := range fresh {
		m.deps.Log.Error(Op, "the incumbent's row has a status this build cannot read; the row keeps this daemon's own status", dlog.Merge(ctx, dlog.Context{
			"remediation": "an arm the incumbent's build names and this build does not is a contract break: the roster status oneof only ever gains arms",
		}))
	}
	m.publish()
}

// statusOf answers ROW's status arm, or the reason it cannot be mirrored.
func statusOf(row *frontendv1.RosterRow) (string, string) {
	name, ok := protoarm.Name(row.ProtoReflect(), "status")
	if !ok {
		return "", "no status arm this build decodes is set"
	}
	arm := string(name)
	if _, ok := ladder.RosterArmClaim(arm); !ok {
		return arm, "the arm projects onto no claim this build places"
	}
	return arm, ""
}

// publish hands the sink every mirrored arm of a workspace not yet adopted,
// nil when none is.
func (m *Mirror) publish() {
	m.pub.Lock()
	defer m.pub.Unlock()
	m.mu.Lock()
	arms := m.arms
	m.mu.Unlock()
	var out map[ids.WorkspaceID]string
	for ws, arm := range arms {
		if !m.deps.NotYetAdopted(ws) {
			continue
		}
		if out == nil {
			out = map[ids.WorkspaceID]string{}
		}
		out[ws] = arm
	}
	m.deps.Sink(out)
}
