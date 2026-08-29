package workspace

import (
	"context"
	"fmt"

	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/sidebar"
	"claude-repld/internal/wsm"
)

// Resolve turns a client's echoed WorkspaceRef into a workspace. It keys on
// `id` — a path is never an identity — and REFUSES a ref whose `dir` disagrees
// with the registry, because a client echoing a stale dir is echoing a stale
// view and the verb it is about to run would land on the wrong tree.
//
// An empty echoed dir is NOT a mismatch: the ref's dir is a convenience for the
// webview URL, and a client that has only the id is still naming one workspace
// unambiguously.
func (v *verbs) Resolve(ctx context.Context, ref *workspacev1.WorkspaceRef) (wsm.Workspace, error) {
	global := v.deps.Log.Global()
	if ref == nil {
		return wsm.Workspace{}, refuse(global, "", ArmUnknownWorkspace, "no workspace ref was supplied", true)
	}
	id := ids.WorkspaceID(ref.GetId())
	if id == "" {
		return wsm.Workspace{}, refuse(global.With(dlog.Context{"dir": ref.GetDir()}),
			"", ArmUnknownWorkspace, "the workspace ref names no id", true)
	}

	record, log, err := v.owned(ctx, "", id)
	if err != nil {
		return wsm.Workspace{}, err
	}

	echoed := ref.GetDir()
	if echoed == "" {
		log.Debug(opResolve, "resolved a ref that echoed no dir", dlog.Context{"registry_dir": record.Dir})
		return record, nil
	}
	normalized, err := normalizeDir(echoed)
	if err != nil {
		return wsm.Workspace{}, refuse(log, "", ArmWorkspaceRefMismatch,
			fmt.Sprintf("the echoed dir %q cannot be normalized: %v", echoed, err), false)
	}
	if normalized != record.Dir {
		return wsm.Workspace{}, refuse(log, "", ArmWorkspaceRefMismatch,
			fmt.Sprintf("the echoed dir %q resolves to %q but workspace %q is registered at %q",
				echoed, normalized, id, record.Dir), false)
	}
	log.Debug(opResolve, "resolved a ref whose dir agrees with the registry", dlog.Context{
		"registry_dir": record.Dir,
	})
	return record, nil
}

// sidebarRegistry composes the roster's durable half. It lives beside Resolve
// because both are pure translations of WSM facts into another package's
// vocabulary.
func sidebarRegistry(workspaces []wsm.Workspace, repositories []wsm.Repository, tasks []wsm.Task, current *ids.WorkspaceID) sidebar.Registry {
	return sidebar.Registry{
		Workspaces:   workspaces,
		Repositories: repositories,
		Tasks:        tasks,
		Current:      current,
	}
}
