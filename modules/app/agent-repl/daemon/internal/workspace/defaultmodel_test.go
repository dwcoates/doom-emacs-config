package workspace

import (
	"context"
	"errors"
	"testing"

	"agent-repld/internal/wsm"
)

func TestLoginDefaultModel(t *testing.T) {
	cases := []struct {
		name      string
		chosen    map[string]string
		configDir string
		want      string
	}{
		{name: "a personal login nobody chose for is opus", configDir: "/config", want: DefaultLaunchModel},
		{name: "the work login nobody chose for is sonnet", configDir: "/config-work", want: WorkDefaultLaunchModel},
		{name: "a chosen default outranks the built-in", chosen: map[string]string{"/config-work": "haiku"}, configDir: "/config-work", want: "haiku"},
		{name: "another login's choice is not this login's", chosen: map[string]string{"/config-work": "haiku"}, configDir: "/config", want: DefaultLaunchModel},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			db := &fakeDB{defaultModels: tc.chosen}
			accounts := &fakeAccounts{configDir: "/config", multiRepoDir: "/config-work"}

			// Act.
			got, err := loginDefaultModel(context.Background(), db, accounts, tc.configDir)

			// Assert.
			if err != nil {
				t.Fatalf("loginDefaultModel: %v", err)
			}
			if got != tc.want {
				t.Fatalf("loginDefaultModel(%q) = %q, want %q", tc.configDir, got, tc.want)
			}
		})
	}
}

func TestLoginDefaultModelSurfacesAFailedRead(t *testing.T) {
	// Arrange.
	db := &fakeDB{defaultModelErr: errors.New("disk gone")}

	// Act.
	_, err := loginDefaultModel(context.Background(), db, &fakeAccounts{}, "/config")

	// Assert.
	if err == nil || !errors.Is(err, db.defaultModelErr) {
		t.Fatalf("err = %v, want the read's failure wrapped", err)
	}
}

// defaultModelFixture is a workspace on the login /config whose selector
// served opus and sonnet.
func defaultModelFixture(t *testing.T) *fixture {
	t.Helper()
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	f.db.sessions["w1"] = wsm.Session{Workspace: "w1", HostSessionID: "host-1", ConfigDir: "/config"}
	f.cards.models, f.cards.hasModels = []string{"opus", "sonnet"}, true
	return f
}

func TestSetDefaultModelFilesTheChoiceAgainstTheLogin(t *testing.T) {
	// Arrange.
	f := defaultModelFixture(t)

	// Act.
	if err := f.verbs.SetDefaultModel(context.Background(), "w1", "sonnet"); err != nil {
		t.Fatalf("SetDefaultModel: %v", err)
	}

	// Assert.
	if got := f.db.defaultModels["/config"]; got != "sonnet" {
		t.Fatalf("login default = %q, want sonnet filed against /config", got)
	}
}

func TestSetDefaultModelFilesAgainstTheChosenAccountRoot(t *testing.T) {
	// Arrange: the reader switched the workspace to the work login, so the
	// default is that login's.
	f := defaultModelFixture(t)
	f.db.sessions["w1"] = wsm.Session{Workspace: "w1", HostSessionID: "host-1", ConfigDir: "/config", SelectedConfigDir: "/config-work"}

	// Act.
	if err := f.verbs.SetDefaultModel(context.Background(), "w1", "opus"); err != nil {
		t.Fatalf("SetDefaultModel: %v", err)
	}

	// Assert.
	if _, ok := f.db.defaultModels["/config"]; ok || f.db.defaultModels["/config-work"] != "opus" {
		t.Fatalf("login defaults = %v, want opus filed against /config-work alone", f.db.defaultModels)
	}
}

func TestSetDefaultModelMovesTheLoginsStrips(t *testing.T) {
	// Arrange.
	f := defaultModelFixture(t)

	// Act.
	if err := f.verbs.SetDefaultModel(context.Background(), "w1", "sonnet"); err != nil {
		t.Fatalf("SetDefaultModel: %v", err)
	}

	// Assert.
	if len(f.topbarLoginDefaults) != 1 || f.topbarLoginDefaults[0] != [2]string{"/config", "sonnet"} {
		t.Fatalf("topbar login defaults = %v, want one move of /config to sonnet", f.topbarLoginDefaults)
	}
}

func TestSetDefaultModelLogsTheChoice(t *testing.T) {
	// Arrange.
	f := defaultModelFixture(t)

	// Act.
	if err := f.verbs.SetDefaultModel(context.Background(), "w1", "sonnet"); err != nil {
		t.Fatalf("SetDefaultModel: %v", err)
	}

	// Assert.
	records := recordsAt(f.log, opSetDefaultModel, "info")
	if len(records) != 1 || records[0]["config_dir"] != "/config" || records[0]["model"] != "sonnet" {
		t.Fatalf("records = %v, want one INFO naming the login and the model", records)
	}
}

func TestSetDefaultModelRefusesAModelOutsideTheServedCatalog(t *testing.T) {
	// Arrange.
	f := defaultModelFixture(t)

	// Act.
	err := f.verbs.SetDefaultModel(context.Background(), "w1", "not-a-real-model")

	// Assert.
	asRefusal(t, err, ArmNotInCatalog)
	if len(f.db.defaultModels) != 0 || len(f.topbarLoginDefaults) != 0 {
		t.Fatalf("defaults = %v, strips = %v, want nothing filed or moved", f.db.defaultModels, f.topbarLoginDefaults)
	}
}

func TestSetDefaultModelRefusesWhenNoCatalogWasServed(t *testing.T) {
	// Arrange.
	f := defaultModelFixture(t)
	f.cards.models, f.cards.hasModels = nil, false

	// Act.
	err := f.verbs.SetDefaultModel(context.Background(), "w1", "opus")

	// Assert.
	asRefusal(t, err, ArmNoSession)
	if len(f.db.defaultModels) != 0 {
		t.Fatalf("defaults = %v, want nothing filed", f.db.defaultModels)
	}
}

func TestSetDefaultModelSurfacesAFailedWrite(t *testing.T) {
	// Arrange.
	f := defaultModelFixture(t)
	f.db.putDefaultModelErr = errors.New("disk full")

	// Act.
	err := f.verbs.SetDefaultModel(context.Background(), "w1", "sonnet")

	// Assert.
	if !errors.Is(err, f.db.putDefaultModelErr) {
		t.Fatalf("err = %v, want the write's failure wrapped", err)
	}
	if len(f.topbarLoginDefaults) != 0 {
		t.Fatalf("strips = %v, want none moved for a default that was not filed", f.topbarLoginDefaults)
	}
	records := recordsAt(f.log, opSetDefaultModel, "error")
	if len(records) != 1 || records[0]["config_dir"] != "/config" || records[0]["model"] != "sonnet" {
		t.Fatalf("records = %v, want one ERROR naming the login and the model", records)
	}
}

func TestPublishAccountCarriesTheLoginsDefault(t *testing.T) {
	// Arrange.
	f := defaultModelFixture(t)
	f.db.defaultModels = map[string]string{"/config": "haiku"}
	record := f.db.workspaces["w1"]

	// Act.
	if err := f.verbs.(*verbs).publishAccount(context.Background(), f.log.Global(), record, "/config"); err != nil {
		t.Fatalf("publishAccount: %v", err)
	}

	// Assert.
	got := f.topbarAccounts[len(f.topbarAccounts)-1]
	if got.ConfigDir != "/config" || got.DefaultModel != "haiku" {
		t.Fatalf("account cell = %+v, want login /config carrying the default haiku", got)
	}
}

func TestPublishAccountSurfacesAFailedDefaultRead(t *testing.T) {
	// Arrange.
	f := defaultModelFixture(t)
	f.db.defaultModelErr = errors.New("disk gone")
	record := f.db.workspaces["w1"]
	before := len(f.topbarAccounts)

	// Act.
	err := f.verbs.(*verbs).publishAccount(context.Background(), f.log.Global(), record, "/config")

	// Assert.
	if !errors.Is(err, f.db.defaultModelErr) {
		t.Fatalf("err = %v, want the read's failure wrapped", err)
	}
	if len(f.topbarAccounts) != before {
		t.Fatalf("account cells = %d, want none installed without a default", len(f.topbarAccounts)-before)
	}
	if records := recordsAt(f.log, opRegister, "error"); len(records) == 0 {
		t.Fatalf("want an ERROR record for the failed default read")
	}
}
