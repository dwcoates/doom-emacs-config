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
func sidebarRegistry(workspaces []wsm.Workspace, repositories []wsm.Repository, tasks []wsm.Task, sessions []wsm.Session, current *ids.WorkspaceID) sidebar.Registry {
	return sidebar.Registry{
		Workspaces:   workspaces,
		Repositories: repositories,
		Tasks:        tasks,
		Sessions:     sessions,
		Current:      current,
	}
}

// sessionRecords reads one durable session record per registered workspace.
//
// THE ROSTER CANNOT TELL A PARK FROM A FAULT WITHOUT THEM. sidebar.Registry
// has carried a Sessions field since the resolver landed, and the status
// precedence keys three of its answers on it -- `none` (an assertion that no
// session has EVER existed), the receding of a killed row, and the idle arm a
// HIBERNATED session keeps instead of the link's `dead`. Nothing populated the
// field, so every one of those read a nil record: a session the idle sweep
// parked on purpose resolved as `dead` and Emacs painted the tab as broken.
//
// A workspace with no session contributes no record; the resolver keys on the
// record's own workspace, and an absent one is the `none` arm it asserts.
func sessionRecords(ctx context.Context, db wsm.DB, workspaces []wsm.Workspace) ([]wsm.Session, error) {
	sessions := make([]wsm.Session, 0, len(workspaces))
	for _, ws := range workspaces {
		session, found, err := db.Session(ctx, ws.ID)
		if err != nil {
			return nil, fmt.Errorf("read workspace %q's session record: %w", ws.ID, err)
		}
		if !found {
			continue
		}
		sessions = append(sessions, session)
	}
	return sessions, nil
}
