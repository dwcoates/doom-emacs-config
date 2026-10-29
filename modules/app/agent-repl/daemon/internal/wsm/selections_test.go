package wsm

import (
	"context"
	"errors"
	"testing"

	"google.golang.org/protobuf/proto"

	conversationv1 "agentrepl/proto/conversation/v1"
)

func TestWorkspaceSelections(t *testing.T) {
	high := conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_HIGH
	tests := []struct {
		name      string
		put       func(context.Context, *store, WorkspaceID) error
		want      Selections
		wantFound bool
	}{
		{name: "a workspace with none recorded has none", put: func(context.Context, *store, WorkspaceID) error { return nil }},
		{name: "a model is read back alone", put: func(ctx context.Context, s *store, ws WorkspaceID) error {
			return s.PutWorkspaceModel(ctx, ws, "opus")
		}, want: Selections{Model: "opus"}, wantFound: true},
		{name: "each control keeps the others", put: func(ctx context.Context, s *store, ws WorkspaceID) error {
			return errors.Join(s.PutWorkspaceModel(ctx, ws, "opus"), s.PutWorkspacePermissionMode(ctx, ws, "plan"), s.PutWorkspaceEffort(ctx, ws, high))
		}, want: Selections{Model: "opus", PermissionMode: "plan", Effort: high}, wantFound: true},
		{name: "a later model replaces the earlier", put: func(ctx context.Context, s *store, ws WorkspaceID) error {
			return errors.Join(s.PutWorkspaceModel(ctx, ws, "opus"), s.PutWorkspaceModel(ctx, ws, "sonnet"))
		}, want: Selections{Model: "sonnet"}, wantFound: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s).ID
			if err := tt.put(context.Background(), s, ws); err != nil {
				t.Fatalf("put: %v", err)
			}

			// Act
			got, found, err := s.WorkspaceSelections(context.Background(), ws)

			// Assert
			if err != nil || found != tt.wantFound || got != tt.want {
				t.Fatalf("WorkspaceSelections = %+v, %v, %v; want %+v, %v", got, found, err, tt.want, tt.wantFound)
			}
		})
	}
}

func TestABlankSelectionIsRefused(t *testing.T) {
	tests := []struct {
		name string
		put  func(context.Context, *store, WorkspaceID) error
	}{
		{"a blank model", func(ctx context.Context, s *store, ws WorkspaceID) error { return s.PutWorkspaceModel(ctx, ws, " ") }},
		{"a blank permission mode", func(ctx context.Context, s *store, ws WorkspaceID) error {
			return s.PutWorkspacePermissionMode(ctx, ws, "")
		}},
		{"an unspecified effort", func(ctx context.Context, s *store, ws WorkspaceID) error {
			return s.PutWorkspaceEffort(ctx, ws, conversationv1.AgentEffortLevel_AGENT_EFFORT_LEVEL_UNSPECIFIED)
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s).ID

			// Act
			err := tt.put(context.Background(), s, ws)

			// Assert
			if _, found, _ := s.WorkspaceSelections(context.Background(), ws); err == nil || found {
				t.Fatalf("put = %v (recorded %v), want refused and nothing recorded", err, found)
			}
		})
	}
}

func TestAStoredEffortThatIsNoLevelIsADecodeError(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s).ID
	if _, err := s.db().ExecContext(context.Background(), `INSERT INTO workspace_selections (workspace_id, effort) VALUES (?, 999)`, ws); err != nil {
		t.Fatalf("seed: %v", err)
	}

	// Act
	_, _, err := s.WorkspaceSelections(context.Background(), ws)

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Field != "effort" {
		t.Fatalf("err = %v, want a DecodeError on effort", err)
	}
}

func TestAWorkspacesSelectionsGoWithIt(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s).ID
	if err := s.PutWorkspaceModel(context.Background(), ws, "opus"); err != nil {
		t.Fatalf("PutWorkspaceModel: %v", err)
	}

	// Act
	if _, err := s.db().ExecContext(context.Background(), `DELETE FROM workspaces WHERE id = ?`, ws); err != nil {
		t.Fatalf("delete: %v", err)
	}

	// Assert
	var rows int
	if err := s.db().QueryRowContext(context.Background(), `SELECT COUNT(*) FROM workspace_selections`).Scan(&rows); err != nil || rows != 0 {
		t.Fatalf("selections rows = %d (%v), want none after the workspace went", rows, err)
	}
}

func catalogOf(names ...string) *conversationv1.ModelCatalog {
	catalog := &conversationv1.ModelCatalog{}
	for _, name := range names {
		catalog.Options = append(catalog.Options, &conversationv1.ModelOption{Model: &conversationv1.AgentModel{Name: name}, DisplayName: name})
	}
	return catalog
}

func TestAccountModelCatalog(t *testing.T) {
	tests := []struct {
		name      string
		put       []*conversationv1.ModelCatalog
		want      *conversationv1.ModelCatalog
		wantFound bool
	}{
		{name: "a root with none recorded has none"},
		{name: "a recorded catalog is read back whole", put: []*conversationv1.ModelCatalog{catalogOf("opus", "sonnet")}, want: catalogOf("opus", "sonnet"), wantFound: true},
		{name: "a later catalog replaces it", put: []*conversationv1.ModelCatalog{catalogOf("opus"), catalogOf("haiku")}, want: catalogOf("haiku"), wantFound: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			for _, catalog := range tt.put {
				if err := s.PutAccountModelCatalog(context.Background(), "/accounts/work", catalog); err != nil {
					t.Fatalf("PutAccountModelCatalog: %v", err)
				}
			}

			// Act
			got, found, err := s.AccountModelCatalog(context.Background(), "/accounts/work")

			// Assert
			if err != nil || found != tt.wantFound || !proto.Equal(got, tt.want) {
				t.Fatalf("AccountModelCatalog = %v, %v, %v; want %v, %v", got, found, err, tt.want, tt.wantFound)
			}
		})
	}
}

func TestAnEmptyCatalogOrBlankRootIsRefused(t *testing.T) {
	tests := []struct {
		name    string
		root    string
		catalog *conversationv1.ModelCatalog
	}{
		{"an empty catalog", "/accounts/work", catalogOf()},
		{"a blank root", " ", catalogOf("opus")},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)

			// Act
			err := s.PutAccountModelCatalog(context.Background(), tt.root, tt.catalog)

			// Assert
			if err == nil {
				t.Fatalf("PutAccountModelCatalog accepted %s", tt.name)
			}
		})
	}
}

func TestAStoredCatalogThatDoesNotDecodeIsADecodeError(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	if _, err := s.db().ExecContext(context.Background(), `INSERT INTO account_model_catalogs (config_dir, catalog) VALUES ('/accounts/work', x'ff')`); err != nil {
		t.Fatalf("seed: %v", err)
	}

	// Act
	_, _, err := s.AccountModelCatalog(context.Background(), "/accounts/work")

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Field != "catalog" {
		t.Fatalf("err = %v, want a DecodeError on catalog", err)
	}
}
