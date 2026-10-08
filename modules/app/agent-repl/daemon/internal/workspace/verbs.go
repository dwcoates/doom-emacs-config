package workspace

import (
	"context"
	"errors"
	"fmt"
	"sync"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/resolve/sidebar"
	"claude-repld/internal/wsm"
)

// The operation names this package's records carry, one per verb.
const (
	opResolve        = "daemon.workspace.resolve"
	opRoster         = "daemon.workspace.roster"
	opRegister       = "daemon.workspace.register"
	opCreate         = "daemon.workspace.create"
	opCommandSupport = "daemon.workspace.request_command_support"
	opOpen           = "daemon.workspace.open"
	opClose          = "daemon.workspace.close"
	opKill           = "daemon.workspace.kill"
	opNuke           = "daemon.workspace.nuke"
	opForget         = "daemon.workspace.forget"
	opRestart        = "daemon.workspace.restart"
	opSelect         = "daemon.workspace.select"
	opMarkViewed     = "daemon.workspace.mark_viewed"
	opSetPriority    = "daemon.workspace.set_priority"
	opCreateTask     = "daemon.workspace.create_task"
	opUpdateTask     = "daemon.workspace.update_task"
	opAssignTask     = "daemon.workspace.assign_task"
	opColdGate       = "daemon.workspace.answer_cold_gate"
	opSetModel       = "daemon.workspace.set_model"
	opSetMode        = "daemon.workspace.set_permission_mode"
	opInterrupt      = "daemon.workspace.interrupt"
	opAnswerPerm     = "daemon.workspace.answer_permission"
	opAnswerQuestion = "daemon.workspace.answer_question"
	opOpenExternal   = "daemon.workspace.open_external"
	opOpenInEditor   = "daemon.workspace.open_in_editor"
	opBringUp        = "daemon.workspace.bring_up"
	opSelectAccount  = "daemon.workspace.select_account"
)

// verbs is the whole verb surface. It holds no FACTS: every fact it reads is
// WSM's or a resolver's, so two verbs racing cannot disagree about a workspace.
// What it does hold is coordination: the selection lock and the revival
// flights, which order concurrent verbs without remembering anything.
type verbs struct {
	deps   Deps
	load   PromptLoader
	splice PromptSplicer
	now    func() time.Time
	// restartStopBound bounds the restart's forced end of the running turn;
	// see DefaultRestartStopBound.
	restartStopBound time.Duration

	// selection serializes Select's selection section — the read of the
	// current workspace through the roster push — so two concurrent selects
	// land whole, one after the other, and WSM's current and the roster's
	// selection can never be left naming different workspaces.
	selection sync.Mutex
	// revivals holds at most one revival in flight per workspace.
	revivals revivalFlights
}

// load resolves one workspace's durable record and its own logger. A REGISTERED
// WORKSPACE ALWAYS RESOLVES TO A SINK: when its directory cannot host one — a
// scratch path, a worktree that has been deleted — the records go centrally
// with the workspace named on them, and the verb still runs. Failing the verb
// on its own logging turned every per-workspace rpc on such a workspace into an
// internal error in place of the verb's own answer.
func (v *verbs) record(ctx context.Context, rpc string, ws ids.WorkspaceID) (wsm.Workspace, dlog.Logger, error) {
	global := v.deps.Log.Global().With(dlog.Context{"workspace": string(ws)})
	record, err := v.deps.DB.Workspace(ctx, ws)
	if err != nil {
		return wsm.Workspace{}, nil, refuse(global, rpc, ArmUnknownWorkspace,
			fmt.Sprintf("no workspace %q is registered", ws), true)
	}
	log := v.deps.Log.WorkspaceOrCentral(record.Dir)
	return record, log.With(dlog.Context{"workspace": string(ws)}), nil
}

// owned resolves a workspace AND refuses it when this daemon does not serve it.
// Every per-workspace verb goes through here: acting on a workspace mid-handover
// would race the daemon that owns it.
func (v *verbs) owned(ctx context.Context, rpc string, ws ids.WorkspaceID) (wsm.Workspace, dlog.Logger, error) {
	record, log, err := v.record(ctx, rpc, ws)
	if err != nil {
		return wsm.Workspace{}, nil, err
	}
	standing, err := v.deps.Ownership.Standing(ctx, ws)
	if err != nil {
		log.Error(rpc, "could not determine the serving standing", dlog.Context{"cause": err.Error()})
		return wsm.Workspace{}, nil, fmt.Errorf("workspace %q: serving standing: %w", ws, err)
	}
	switch standing {
	case StandingOwned:
		log.Debug(rpc, "this daemon serves the workspace", dlog.Context{"standing": "owned"})
		return record, log, nil
	case StandingTransferringAway:
		return wsm.Workspace{}, nil, refuse(log, rpc, ArmTransferringAway,
			fmt.Sprintf("workspace %q has been handed to a successor daemon", ws), false)
	case StandingNotYetAdopted:
		return wsm.Workspace{}, nil, refuse(log, rpc, ArmNotYetAdopted,
			fmt.Sprintf("workspace %q has not been adopted by this daemon yet", ws), false)
	default:
		return wsm.Workspace{}, nil, fmt.Errorf("workspace %q: unknown serving standing %d", ws, standing)
	}
}

// republishRegistry re-renders the roster from WSM. Every verb that changes a
// durable registry fact calls it, which is what keeps the roster from needing
// its own copy of the registry.
func (v *verbs) republishRegistry(ctx context.Context, log dlog.Logger, operation string) {
	reg, ok := v.readRegistry(ctx, log, operation)
	if !ok {
		return
	}
	v.deps.Sidebar.SetRegistry(reg)
	logRepublished(log, operation, reg)
}

// readRegistry reads the roster's registry from WSM. A read that fails is
// recorded here and answers false; a cancelled context is this daemon going
// away and is recorded at info.
func (v *verbs) readRegistry(ctx context.Context, log dlog.Logger, operation string) (sidebar.Registry, bool) {
	workspaces, err := v.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		// A CANCELLED CONTEXT IS THIS DAEMON GOING AWAY, not a read that
		// broke: the exit cancels the serving context under whatever was in
		// flight, and a roster nobody is left to receive is no loss. Every
		// other refusal keeps its ERROR. The precedent is the state client's
		// own read, which has always answered a cancellation at info.
		if canceled(err) {
			log.Info(operation, "the roster read ended when its context was cancelled", dlog.Context{"cause": err.Error()})
			return sidebar.Registry{}, false
		}
		log.Error(operation, "could not list the workspaces for the roster", dlog.Context{"cause": err.Error()})
		return sidebar.Registry{}, false
	}
	repositories, err := v.deps.DB.ListRepositories(ctx)
	if err != nil {
		log.Error(operation, "could not list the repositories for the roster", dlog.Context{"cause": err.Error()})
		return sidebar.Registry{}, false
	}
	tasks, err := v.deps.DB.Tasks(ctx)
	if err != nil {
		log.Error(operation, "could not list the tasks for the roster", dlog.Context{"cause": err.Error()})
		return sidebar.Registry{}, false
	}
	current, err := v.deps.DB.Current(ctx)
	if err != nil {
		log.Error(operation, "could not read the current workspace for the roster", dlog.Context{"cause": err.Error()})
		return sidebar.Registry{}, false
	}
	view, err := v.deps.DB.SidebarView(ctx)
	if err != nil {
		log.Error(operation, "could not read the sidebar's view state for the roster", dlog.Context{"cause": err.Error()})
		return sidebar.Registry{}, false
	}
	sessions, err := sessionRecords(ctx, v.deps.DB, workspaces)
	if err != nil {
		log.Error(operation, "could not read the session records for the roster", dlog.Context{"cause": err.Error()})
		return sidebar.Registry{}, false
	}
	return sidebarRegistry(log, workspaces, repositories, tasks, sessions, current, view), true
}

// logRepublished records a registry republish.
//
// THE ROSTER REPUBLISH STANDS AT INFO. Every verb that reaches here has just
// changed the roster clients read — a select above all, whose switch otherwise
// left no trace in an info-level log. It is one concise line per discrete
// roster mutation, never a per-frame push, so it informs without spamming.
func logRepublished(log dlog.Logger, operation string, reg sidebar.Registry) {
	log.Info(operation, "republished the roster registry", dlog.Context{
		"workspaces": len(reg.Workspaces), "repositories": len(reg.Repositories), "tasks": len(reg.Tasks),
		"sessions": len(reg.Sessions),
	})
}

// promptSubmission composes the queue submission for a daemon-born prompt: the
// workspace-creation origin, no bubble target. It exists so the create verb and
// the command-file ingress cannot spell the same submission differently.
func promptSubmission(ws ids.WorkspaceID, turn ids.TurnID, said *conversationv1.UserSaid) promptqueue.Submission {
	return daemonSubmission(ws, turn, said, conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_CREATED)
}

// daemonSubmission composes a daemon-born submission under a named origin.
// Every daemon-born prompt goes through here, so none of them can reach the
// queue without the required origin.
func daemonSubmission(ws ids.WorkspaceID, turn ids.TurnID, said *conversationv1.UserSaid, origin conversationv1.PromptOrigin) promptqueue.Submission {
	return promptqueue.Submission{WS: ws, Turn: turn, Said: said, Origin: origin}
}

// rootFeed is the workspace's top-level feed, which is where every
// daemon-synthesized row this package produces lands.
func rootFeed() feedid.Feed { return feedid.Feed{Root: true} }

// coldGateRowID is the cold gate row's identity: the gate's own address, one
// per parked conversation. The raise upserts under it and the answer retires
// it, so the two can never name different rows.
func coldGateRowID(ws ids.WorkspaceID, vendorSessionID string) *frontendv1.FeedId {
	return feedid.Encode(feedid.Ref{
		WS:   ws,
		Feed: rootFeed(),
		Row:  feedid.RowKey{Kind: feedid.KindColdGate, ID: vendorSessionID},
	})
}

// canceled reports whether err is a context ending -- this daemon's own exit,
// or a caller that left -- rather than something that broke. Work abandoned
// that way is stated at info; everything else keeps its error.
func canceled(err error) bool {
	return errors.Is(err, context.Canceled) || errors.Is(err, context.DeadlineExceeded)
}
