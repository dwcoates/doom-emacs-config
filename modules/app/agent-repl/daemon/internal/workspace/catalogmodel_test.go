package workspace

import (
	"context"
	"errors"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/wsm"
)

// catalogFixture is a workspace on the login /config, whose recorded catalog
// offers opus and sonnet and whose selector served nothing (a hidden merge
// workspace's).
func catalogFixture(t *testing.T) *fixture {
	t.Helper()
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.sessions["w1"] = wsm.Session{Workspace: "w1", HostSessionID: "host-1", ConfigDir: "/config"}
	f.db.modelCatalogs = map[string]*conversationv1.ModelCatalog{"/config": {Options: []*conversationv1.ModelOption{
		{Model: &conversationv1.AgentModel{Name: "opus"}}, {Model: &conversationv1.AgentModel{Name: "sonnet"}}}}}
	return f
}

func TestRequireCatalogModel(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(f *fixture)
		model   string
		want    string
	}{
		{name: "a model the login's catalog offers passes", model: "sonnet"},
		{name: "a model it does not offer is refused", model: "haiku", want: ArmNotInCatalog},
		{name: "a login that served no catalog offers nothing", arrange: func(f *fixture) { f.db.modelCatalogs = nil }, model: "opus", want: ArmNotInCatalog},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := catalogFixture(t)
			if tc.arrange != nil {
				tc.arrange(f)
			}

			// Act.
			err := f.verbs.RequireCatalogModel(context.Background(), "w1", tc.model)

			// Assert.
			if tc.want == "" {
				if err != nil {
					t.Fatalf("RequireCatalogModel = %v, want it passed", err)
				}
				return
			}
			refusal, ok := AsRefusal(err)
			if !ok || refusal.Arm != tc.want {
				t.Fatalf("RequireCatalogModel = %v, want the %s refusal", err, tc.want)
			}
		})
	}
}

func TestRequireCatalogModelSurfacesAFailedCatalogRead(t *testing.T) {
	// Arrange.
	f := catalogFixture(t)
	f.db.modelCatalogErr = errors.New("disk gone")

	// Act.
	err := f.verbs.RequireCatalogModel(context.Background(), "w1", "opus")

	// Assert: the failure surfaced, and recorded at ERROR.
	if !errors.Is(err, f.db.modelCatalogErr) {
		t.Fatalf("err = %v, want the read's failure wrapped", err)
	}
	if records := recordsAt(f.log, opSetModel, "error"); len(records) != 1 || records[0]["config_dir"] != "/config" {
		t.Fatalf("records = %v, want one ERROR naming the login", records)
	}
}
