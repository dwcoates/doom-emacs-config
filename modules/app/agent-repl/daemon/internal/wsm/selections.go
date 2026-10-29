package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"strings"

	"google.golang.org/protobuf/proto"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/dlog"
)

// selectionsDDL is the layout-89 addition: the topbar's last-known controls,
// kept so they are drawn, and can be picked, while a workspace has no shim
// (owner request, 2026-10-29). IT IS ADDITIVE: two new tables the previous
// build never names.
//
// workspace_selections holds each workspace's last model in force, its last
// permission mode, and the effort the user last picked, each NULL until it is
// first known. account_model_catalogs holds the model catalog the last session
// on each account root served (conversation.v1.ModelCatalog), keyed by the
// root, because the catalog is the login's, not any one workspace's.
const selectionsDDL = `
CREATE TABLE workspace_selections (
  workspace_id    TEXT PRIMARY KEY REFERENCES workspaces(id) ON DELETE CASCADE,
  model           TEXT,
  permission_mode TEXT,
  effort          INTEGER
);
CREATE TABLE account_model_catalogs (
  config_dir TEXT PRIMARY KEY,
  catalog    BLOB NOT NULL
);
`

// Selections is a workspace's last-known topbar controls. An empty string, or
// an UNSPECIFIED effort, is a control never known for the workspace.
type Selections struct {
	// Model is the last model in force.
	Model string
	// PermissionMode is the last permission mode in force.
	PermissionMode string
	// Effort is the effort the user last picked.
	Effort conversationv1.AgentEffortLevel
}

// WorkspaceSelections answers workspace ID's last-known controls, false when
// none was ever recorded.
func (s *store) WorkspaceSelections(ctx context.Context, id WorkspaceID) (Selections, bool, error) {
	var out Selections
	found := false
	err := s.read(ctx, "daemon.wsm.workspace_selections", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		var model, mode sql.NullString
		var effort sql.NullInt64
		err := s.q(ctx).QueryRowContext(ctx,
			`SELECT model, permission_mode, effort FROM workspace_selections WHERE workspace_id = ?`, id).
			Scan(&model, &mode, &effort)
		switch {
		case errors.Is(err, sql.ErrNoRows):
			return nil
		case err != nil:
			return err
		}
		if effort.Valid {
			if _, known := conversationv1.AgentEffortLevel_name[int32(effort.Int64)]; !known {
				return &DecodeError{Table: "workspace_selections", Row: string(id), Field: "effort", Err: fmt.Errorf("%d is no effort level", effort.Int64)}
			}
			out.Effort = conversationv1.AgentEffortLevel(effort.Int64)
		}
		out.Model, out.PermissionMode, found = model.String, mode.String, true
		return nil
	})
	return out, found, err
}

// selectionColumn is one recorded control's column.
type selectionColumn string

const (
	selectionModel          selectionColumn = "model"
	selectionPermissionMode selectionColumn = "permission_mode"
	selectionEffort         selectionColumn = "effort"
)

// putSelection records VALUE in workspace ID's COLUMN, keeping its others.
func (s *store) putSelection(ctx context.Context, op string, id WorkspaceID, column selectionColumn, value any) error {
	fields := dlog.Context{"workspace": string(id), string(column): value}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx, fmt.Sprintf(
			`INSERT INTO workspace_selections (workspace_id, %[1]s) VALUES (?, ?)
			 ON CONFLICT(workspace_id) DO UPDATE SET %[1]s = excluded.%[1]s`, column), id, value)
		return err
	})
}

// PutWorkspaceModel records MODEL as workspace ID's last model in force. A
// blank model is a bug in the caller, refused loudly.
func (s *store) PutWorkspaceModel(ctx context.Context, id WorkspaceID, model string) error {
	if strings.TrimSpace(model) == "" {
		return fmt.Errorf("wsm: a workspace's model in force names a model (workspace %s)", id)
	}
	return s.putSelection(ctx, "daemon.wsm.put_workspace_model", id, selectionModel, model)
}

// PutWorkspacePermissionMode records MODE as workspace ID's last permission
// mode in force. A blank mode is a bug in the caller, refused loudly.
func (s *store) PutWorkspacePermissionMode(ctx context.Context, id WorkspaceID, mode string) error {
	if strings.TrimSpace(mode) == "" {
		return fmt.Errorf("wsm: a workspace's permission mode names a mode (workspace %s)", id)
	}
	return s.putSelection(ctx, "daemon.wsm.put_workspace_permission_mode", id, selectionPermissionMode, mode)
}

// PutWorkspaceEffort records LEVEL as the effort the user last picked on
// workspace ID. UNSPECIFIED is no pick, refused loudly.
func (s *store) PutWorkspaceEffort(ctx context.Context, id WorkspaceID, level conversationv1.AgentEffortLevel) error {
	if level == conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_UNSPECIFIED {
		return fmt.Errorf("wsm: a workspace's picked effort names a level (workspace %s)", id)
	}
	return s.putSelection(ctx, "daemon.wsm.put_workspace_effort", id, selectionEffort, int64(level))
}

// AccountModelCatalog answers the catalog the last session on account root
// CONFIG_DIR served, false when none was recorded.
func (s *store) AccountModelCatalog(ctx context.Context, configDir string) (*conversationv1.ModelCatalog, bool, error) {
	var catalog *conversationv1.ModelCatalog
	err := s.read(ctx, "daemon.wsm.account_model_catalog", dlog.Context{"config_dir": configDir}, func(ctx context.Context) error {
		var blob []byte
		err := s.q(ctx).QueryRowContext(ctx, `SELECT catalog FROM account_model_catalogs WHERE config_dir = ?`, configDir).Scan(&blob)
		switch {
		case errors.Is(err, sql.ErrNoRows):
			return nil
		case err != nil:
			return err
		}
		decoded := &conversationv1.ModelCatalog{}
		if err := proto.Unmarshal(blob, decoded); err != nil {
			return &DecodeError{Table: "account_model_catalogs", Row: configDir, Field: "catalog", Err: err}
		}
		catalog = decoded
		return nil
	})
	return catalog, catalog != nil, err
}

// PutAccountModelCatalog records CATALOG as the one account root CONFIG_DIR
// last served, replacing any earlier one. A blank root or an empty catalog is
// a bug in the caller, refused loudly.
func (s *store) PutAccountModelCatalog(ctx context.Context, configDir string, catalog *conversationv1.ModelCatalog) error {
	const op = "daemon.wsm.put_account_model_catalog"
	fields := dlog.Context{"config_dir": configDir, "options": len(catalog.GetOptions())}
	if strings.TrimSpace(configDir) == "" || len(catalog.GetOptions()) == 0 {
		err := fmt.Errorf("wsm: an account model catalog needs a root and at least one option (root %q)", configDir)
		s.log.Error(op, "refused a blank account root or an empty catalog", withError(fields, err))
		return err
	}
	blob, err := proto.Marshal(catalog)
	if err != nil {
		return fmt.Errorf("wsm: encode the model catalog of %s: %w", configDir, err)
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO account_model_catalogs (config_dir, catalog) VALUES (?, ?)
			 ON CONFLICT(config_dir) DO UPDATE SET catalog = excluded.catalog`, configDir, blob)
		return err
	})
}
