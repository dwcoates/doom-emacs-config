package topbar

import (
	"context"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// THE CONTROLS OUTLIVE THE SHIM (owner request, 2026-10-29). The model
// selector, the permission-mode picker and the effort selector are drawn, and
// can be picked, while a workspace has no shim: the last model and mode in
// force, the effort last picked and the catalog the login last served are kept
// in wsm (layout 89) and installed when the workspace's account is, before any
// session states its own. A pick then starts the shim (workspace.verbs), and
// the live session's facts replace the remembered ones as they arrive.

// SelectionStore keeps a workspace's last-known controls and each login's
// catalog (wsm.DB's selections.go).
type SelectionStore interface {
	WorkspaceSelections(ctx context.Context, id ids.WorkspaceID) (wsm.Selections, bool, error)
	PutWorkspaceModel(ctx context.Context, id ids.WorkspaceID, model string) error
	PutWorkspacePermissionMode(ctx context.Context, id ids.WorkspaceID, mode string) error
	PutWorkspaceEffort(ctx context.Context, id ids.WorkspaceID, level conversationv1.AgentEffortLevel) error
	AccountModelCatalog(ctx context.Context, configDir string) (*conversationv1.ModelCatalog, bool, error)
	PutAccountModelCatalog(ctx context.Context, configDir string, catalog *conversationv1.ModelCatalog) error
}

// WithSelectionStore keeps the controls in STORE: each is written as it
// changes and installed for a workspace whose session has not started.
func WithSelectionStore(store SelectionStore) Option {
	return func(o *options) { o.selections = store }
}

const opSelections = "daemon.topbar.selections"

// remember writes one changed control through WRITE. A write that fails is
// recorded at ERROR: the control is still drawn from the live session, and
// only what a later sessionless strip draws is stale.
func (r *resolver) remember(ws ids.WorkspaceID, what string, write func(context.Context, SelectionStore) error) {
	store := r.opts.selections
	if store == nil {
		return
	}
	if err := write(context.Background(), store); err != nil {
		r.mu.Lock()
		log := r.logOf(ws, r.stateLocked(ws))
		r.mu.Unlock()
		log.Error(opSelections, "could not keep a topbar control for when the shim is down", dlog.Context{"control": what, "cause": err.Error()})
	}
}

// rememberModel keeps MODEL as WS's model in force.
func (r *resolver) rememberModel(ws ids.WorkspaceID, model string) {
	if model == "" {
		return
	}
	r.remember(ws, "model", func(ctx context.Context, st SelectionStore) error { return st.PutWorkspaceModel(ctx, ws, model) })
}

// rememberMode keeps MODE as WS's permission mode in force.
func (r *resolver) rememberMode(ws ids.WorkspaceID, mode string) {
	if mode == "" {
		return
	}
	r.remember(ws, "permission_mode", func(ctx context.Context, st SelectionStore) error {
		return st.PutWorkspacePermissionMode(ctx, ws, mode)
	})
}

// rememberCatalog keeps CATALOG as the catalog WS's login served.
func (r *resolver) rememberCatalog(ws ids.WorkspaceID, catalog []*conversationv1.ModelOption) {
	if len(catalog) == 0 {
		return
	}
	r.mu.Lock()
	configDir := r.stateLocked(ws).accountConfigDir
	r.mu.Unlock()
	if configDir == "" {
		return
	}
	r.remember(ws, "model_catalog", func(ctx context.Context, st SelectionStore) error {
		return st.PutAccountModelCatalog(ctx, configDir, &conversationv1.ModelCatalog{Options: catalog})
	})
}

// seedSelections installs WS's remembered controls and its login's catalog,
// read from the store, wherever no session has stated its own. CONFIG_DIR is
// the login just installed. A read that fails is recorded at ERROR and seeds
// nothing; the controls are drawn once a session states them.
func (r *resolver) seedSelections(ws ids.WorkspaceID, configDir string) {
	store := r.opts.selections
	if store == nil {
		return
	}
	ctx := context.Background()
	selections, found, err := store.WorkspaceSelections(ctx, ws)
	var catalog *conversationv1.ModelCatalog
	if err == nil && configDir != "" {
		catalog, _, err = store.AccountModelCatalog(ctx, configDir)
	}
	if err != nil {
		r.mu.Lock()
		log := r.logOf(ws, r.stateLocked(ws))
		r.mu.Unlock()
		log.Error(opSelections, "could not read the remembered topbar controls; they are drawn once a session states them", dlog.Context{"cause": err.Error()})
		return
	}
	if !found && len(catalog.GetOptions()) == 0 {
		return
	}
	r.mutate(ws, "daemon.topbar.seed_selections", "the topbar took the remembered controls for a workspace with no session",
		dlog.Context{"model": selections.Model, "permission_mode": selections.PermissionMode, "effort": selections.Effort.String(), "catalog_options": len(catalog.GetOptions())},
		func(s *wsState) {
			if s.started {
				return
			}
			if s.model == "" {
				s.model = selections.Model
			}
			if s.permissionMode == "" && selections.PermissionMode != "" {
				s.permissionMode = selections.PermissionMode
				s.picker = switchablePermissionModes()
			}
			if s.pickedEffort == conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_UNSPECIFIED {
				s.pickedEffort = selections.Effort
			}
			if len(s.catalog) == 0 {
				s.catalog = catalog.GetOptions()
			}
		})
}
