package workspace

import (
	"context"
	"fmt"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
	"claude-repld/internal/promptqueue"
	"claude-repld/internal/wsm"
)

// The operation names this package's records carry, one per verb.
const (
	opResolve        = "daemon.workspace.resolve"
	opRegister       = "daemon.workspace.register"
	opCreate         = "daemon.workspace.create"
	opCommandSupport = "daemon.workspace.request_command_support"
	opOpen           = "daemon.workspace.open"
	opClose          = "daemon.workspace.close"
	opKill           = "daemon.workspace.kill"
	opNuke           = "daemon.workspace.nuke"
	opRestart        = "daemon.workspace.restart"
	opSelect         = "daemon.workspace.select"
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
)

// verbs is the whole verb surface. It holds no state: every fact it reads is
// WSM's or a resolver's, so two verbs racing cannot disagree about a workspace.
type verbs struct {
	deps   Deps
	load   PromptLoader
	splice PromptSplicer
	now    func() time.Time
}

// load resolves one workspace's durable record and its own logger. Failing to
// resolve a KNOWN workspace's sink is an invariant violation, never a reason to
// write globally, so it is surfaced.
func (v *verbs) record(ctx context.Context, rpc string, ws ids.WorkspaceID) (wsm.Workspace, dlog.Logger, error) {
	global := v.deps.Log.Global().With(dlog.Context{"workspace": string(ws)})
	record, err := v.deps.DB.Workspace(ctx, ws)
	if err != nil {
		return wsm.Workspace{}, nil, refuse(global, rpc, ArmUnknownWorkspace,
			fmt.Sprintf("no workspace %q is registered", ws), true)
	}
	log, err := v.deps.Log.Workspace(record.Dir)
	if err != nil {
		global.Error(rpc, "could not resolve the workspace log sink", dlog.Context{
			"dir": record.Dir, "cause": err.Error(),
		})
		return wsm.Workspace{}, nil, fmt.Errorf("workspace %q: resolve log sink %q: %w", ws, record.Dir, err)
	}
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
	workspaces, err := v.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		log.Error(operation, "could not list the workspaces for the roster", dlog.Context{"cause": err.Error()})
		return
	}
	repositories, err := v.deps.DB.ListRepositories(ctx)
	if err != nil {
		log.Error(operation, "could not list the repositories for the roster", dlog.Context{"cause": err.Error()})
		return
	}
	tasks, err := v.deps.DB.Tasks(ctx)
	if err != nil {
		log.Error(operation, "could not list the tasks for the roster", dlog.Context{"cause": err.Error()})
		return
	}
	current, err := v.deps.DB.Current(ctx)
	if err != nil {
		log.Error(operation, "could not read the current workspace for the roster", dlog.Context{"cause": err.Error()})
		return
	}
	v.deps.Sidebar.SetRegistry(sidebarRegistry(workspaces, repositories, tasks, current))
	log.Debug(operation, "republished the roster registry", dlog.Context{
		"workspaces": len(workspaces), "repositories": len(repositories), "tasks": len(tasks),
	})
}

// promptSubmission composes the queue submission for a daemon-born prompt: the
// workspace-creation origin, no bubble target. It exists so the create verb and
// the command-file ingress cannot spell the same submission differently.
func promptSubmission(ws ids.WorkspaceID, turn ids.TurnID, said *conversationv1.UserSaid) promptqueue.Submission {
	return promptqueue.Submission{
		WS:     ws,
		Turn:   turn,
		Said:   said,
		Origin: conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_CREATED,
	}
}

// rootFeed is the workspace's top-level feed, which is where every
// daemon-synthesized row this package produces lands.
func rootFeed() feedid.Feed { return feedid.Feed{Root: true} }

// coldGateRow renders the answered gate as its resolved row. The row's id comes
// from the gate's own address, so the answer UPSERTS the standing row rather
// than adding a second one beneath it.
func coldGateRow(ws ids.WorkspaceID, vendorSessionID string, answer *frontendv1.FeedColdGateResolved) *frontendv1.FeedRow {
	ref := feedid.Ref{
		WS:   ws,
		Feed: rootFeed(),
		Row:  feedid.RowKey{Kind: feedid.KindColdGate, ID: vendorSessionID},
	}
	return &frontendv1.FeedRow{
		Id: feedid.Encode(ref),
		Row: &frontendv1.FeedRow_ColdGate{
			ColdGate: &frontendv1.FeedColdGate{
				State: &frontendv1.FeedColdGate_Resolved{Resolved: answer},
			},
		},
	}
}
