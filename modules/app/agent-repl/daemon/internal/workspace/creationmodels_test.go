package workspace

import (
	"context"
	"errors"
	"slices"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
	"google.golang.org/protobuf/proto"

	"agent-repld/internal/dlog"
)

// catalogOf is a recorded catalog offering NAMES, in order.
func catalogOf(names ...string) *conversationv1.ModelCatalog {
	catalog := &conversationv1.ModelCatalog{}
	for _, name := range names {
		catalog.Options = append(catalog.Options, &conversationv1.ModelOption{
			Model: &conversationv1.AgentModel{Name: name}, DisplayName: name,
		})
	}
	return catalog
}

func TestCreationCatalog(t *testing.T) {
	tests := []struct {
		name      string
		catalogs  map[string]*conversationv1.ModelCatalog
		wantKnown bool
		wantNames []string
	}{
		{name: "the login's recorded catalog is answered in order", catalogs: map[string]*conversationv1.ModelCatalog{"/config": catalogOf("opus", "sonnet")}, wantKnown: true, wantNames: []string{"opus", "sonnet"}},
		{name: "a login with no recorded catalog is unknown", catalogs: nil, wantKnown: false, wantNames: []string{}},
		{name: "another login's catalog is not this login's", catalogs: map[string]*conversationv1.ModelCatalog{"/other": catalogOf("opus")}, wantKnown: false, wantNames: []string{}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.db.modelCatalogs = tt.catalogs

			// Act.
			got, err := f.verbs.CreationCatalog(context.Background(), mainWorktree(t, f))

			// Assert.
			if err != nil {
				t.Fatalf("CreationCatalog: %v", err)
			}
			if got.ConfigDir != "/config" || got.Known != tt.wantKnown || !slices.Equal(got.Names(), tt.wantNames) {
				t.Fatalf("catalog = {%q %v %v}, want {/config %v %v}", got.ConfigDir, got.Known, got.Names(), tt.wantKnown, tt.wantNames)
			}
		})
	}
}

func TestCreationCatalogSurfacesAFailedCatalogRead(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.db.modelCatalogErr = errors.New("disk on fire")
	repoDir := mainWorktree(t, f)

	// Act.
	_, err := f.verbs.CreationCatalog(context.Background(), repoDir)

	// Assert.
	if err == nil {
		t.Fatal("CreationCatalog succeeded, want the read failure")
	}
	record, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Message: "could not read the login's model catalog"})
	if !ok {
		t.Fatalf("records = %+v, want the read failure at ERROR", f.log.logger.Records())
	}
	if record.Context["config_dir"] != "/config" || record.Context["repo_dir"] != repoDir || record.Context["cause"] != "disk on fire" {
		t.Errorf("context = %v, want the login, the repository and the cause", record.Context)
	}
}

func TestCreationCatalogSurfacesAnUnresolvableRepository(t *testing.T) {
	// Arrange: a directory with no .git, so no worktree parent.
	f := newFixture(t)
	repoDir := t.TempDir()

	// Act.
	_, err := f.verbs.CreationCatalog(context.Background(), repoDir)

	// Assert.
	if err == nil {
		t.Fatal("CreationCatalog succeeded, want the worktree parent failure")
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Message: "could not resolve where the repository's worktrees are made"}); !ok {
		t.Fatalf("records = %+v, want the failure at ERROR", f.log.logger.Records())
	}
}

func TestCreateRefusesAModelOutsideTheLoginsCatalogBeforeSpendingAnything(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.db.modelCatalogs = map[string]*conversationv1.ModelCatalog{"/config": catalogOf("opus", "sonnet")}
	spec := standardSpec(t, f)
	spec.Model = "gpt-9"

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert.
	refusal := asRefusal(t, err, ArmModelNotInCatalog)
	if refusal.Fields["model"] != "gpt-9" || refusal.Fields["config_dir"] != "/config" {
		t.Fatalf("fields = %v, want the model and the login", refusal.Fields)
	}
	options, _ := refusal.Fields["options"].([]proto.Message)
	if len(options) != 2 || options[0].(*conversationv1.ModelOption).GetModel().GetName() != "opus" {
		t.Fatalf("options = %v, want the login's catalog", refusal.Fields["options"])
	}
	if len(f.headless.calls) != 0 || len(f.git.created) != 0 || len(f.db.registered) != 0 {
		t.Fatalf("headless %v, worktrees %v, registered %v: want nothing spent", f.headless.calls, f.git.created, f.db.registered)
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Message: refusal.Error()}); !ok {
		t.Fatalf("records = %+v, want the refusal at INFO", f.log.logger.Records())
	}
}

func TestCreateRefusesAModelOnALoginWithNoCatalog(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	spec := standardSpec(t, f)
	spec.Model = "opus"

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert.
	refusal := asRefusal(t, err, ArmModelCatalogUnknown)
	if refusal.Fields["model"] != "opus" || refusal.Fields["config_dir"] != "/config" {
		t.Fatalf("fields = %v, want the model and the login", refusal.Fields)
	}
	if len(f.headless.calls) != 0 || len(f.git.created) != 0 {
		t.Fatalf("headless %v, worktrees %v: want nothing spent", f.headless.calls, f.git.created)
	}
}

func TestCreateSurfacesAFailedCatalogReadForAModel(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.db.modelCatalogErr = errors.New("disk on fire")
	spec := standardSpec(t, f)
	spec.Model = "opus"

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert.
	if err == nil {
		t.Fatal("Create succeeded, want the read failure")
	}
	if _, refused := AsRefusal(err); refused {
		t.Fatalf("error = %v, want a failure rather than a refusal", err)
	}
	if len(f.git.created) != 0 {
		t.Fatalf("worktrees %v, want nothing built", f.git.created)
	}
}

func TestCreateWithNoModelReadsNoCatalog(t *testing.T) {
	// Arrange: a read that would fail, which a model-less create never makes.
	f := newFixture(t)
	f.db.modelCatalogErr = errors.New("must not be read")
	spec := standardSpec(t, f)

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert.
	if err != nil {
		t.Fatalf("Create: %v", err)
	}
}
