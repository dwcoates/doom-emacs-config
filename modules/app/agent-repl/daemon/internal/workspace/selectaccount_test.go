package workspace

import (
	"context"
	"testing"

	"claude-repld/internal/account"
	"claude-repld/internal/rollout"
	"claude-repld/internal/wsm"
)

// twoRoots is the fixture arrangement the account cell's dropdown exists for:
// a machine that knows two roots, one of them the one the workspace is on.
func twoRoots(f *fixture) {
	f.account.configDir = "/config"
	f.account.email = "dev@example.com"
	f.account.roster = []account.Account{
		{ConfigDir: "/config", Email: "dev@example.com", LoggedIn: true},
		{ConfigDir: "/config-work", Email: "work@example.com", LoggedIn: true},
	}
}

func TestSelectAccountRecordsTheChosenRootOnTheSessionRow(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	twoRoots(f)
	f.db.sessions["w1"] = wsm.Session{Workspace: "w1", HostSessionID: "host-1", ConfigDir: "/config"}

	// Act
	if _, err := f.verbs.SelectAccount(context.Background(), "w1", "/config-work"); err != nil {
		t.Fatalf("SelectAccount: %v", err)
	}

	// Assert
	if got := f.db.sessions["w1"].SelectedConfigDir; got != "/config-work" {
		t.Fatalf("selected root = %q, want the root the reader chose", got)
	}
}

func TestSelectAccountLeavesTheRecordedRootAloneUntilTheBounceMovesIt(t *testing.T) {
	// Arrange — the recorded root says where the session CAME UP, and only the
	// relaunch that follows may change that.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	twoRoots(f)
	f.db.sessions["w1"] = wsm.Session{Workspace: "w1", HostSessionID: "host-1", ConfigDir: "/config"}

	// Act
	if _, err := f.verbs.SelectAccount(context.Background(), "w1", "/config-work"); err != nil {
		t.Fatalf("SelectAccount: %v", err)
	}

	// Assert
	if got := f.db.sessions["w1"].ConfigDir; got != "/config" {
		t.Fatalf("recorded root = %q, want the root the session came up under", got)
	}
}

func TestSelectAccountRestartsTheSession(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	twoRoots(f)
	f.db.sessions["w1"] = wsm.Session{Workspace: "w1", HostSessionID: "host-1", ConfigDir: "/config"}

	// Act
	if _, err := f.verbs.SelectAccount(context.Background(), "w1", "/config-work"); err != nil {
		t.Fatalf("SelectAccount: %v", err)
	}

	// Assert
	f.rollout.awaitRelaunch(t)
	if got := f.rollout.relaunchCalls(); len(got) != 1 || got[0].Reason != rollout.ReasonRestartVerb {
		t.Fatalf("relaunches = %+v, want the switch to go through the restart verb's engine", got)
	}
}

func TestSelectAccountRefusesARootTheDaemonDoesNotKnow(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	twoRoots(f)
	f.db.sessions["w1"] = wsm.Session{Workspace: "w1", HostSessionID: "host-1", ConfigDir: "/config"}

	// Act
	_, err := f.verbs.SelectAccount(context.Background(), "w1", "/config-elsewhere")

	// Assert
	asRefusal(t, err, ArmUnknownAccount)
}

func TestARefusedRootIsNeitherRecordedNorBounced(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	twoRoots(f)
	f.db.sessions["w1"] = wsm.Session{Workspace: "w1", HostSessionID: "host-1", ConfigDir: "/config"}

	// Act
	_, _ = f.verbs.SelectAccount(context.Background(), "w1", "/config-elsewhere")

	// Assert
	if got := f.db.sessions["w1"].SelectedConfigDir; got != "" {
		t.Fatalf("selected root = %q after a refusal, want none", got)
	}
}

func TestSelectAccountAnswersThatAChosenRootIsLoggedIn(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	twoRoots(f)
	f.db.sessions["w1"] = wsm.Session{Workspace: "w1", HostSessionID: "host-1", ConfigDir: "/config"}

	// Act
	loggedIn, err := f.verbs.SelectAccount(context.Background(), "w1", "/config-work")

	// Assert
	if err != nil {
		t.Fatalf("SelectAccount: %v", err)
	}
	if !loggedIn {
		t.Fatalf("logged in = false for a root that holds a login")
	}
}

func TestALoggedOutRootIsHonoredAndSaidToBeLoggedOut(t *testing.T) {
	// Arrange — refusing would leave the reader no way to log a known root in.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	twoRoots(f)
	f.account.roster[1] = account.Account{ConfigDir: "/config-work"}
	f.db.sessions["w1"] = wsm.Session{Workspace: "w1", HostSessionID: "host-1", ConfigDir: "/config"}

	// Act
	loggedIn, err := f.verbs.SelectAccount(context.Background(), "w1", "/config-work")

	// Assert
	if err != nil {
		t.Fatalf("SelectAccount on a logged-out root: %v", err)
	}
	if loggedIn {
		t.Fatalf("logged in = true for a root with no login")
	}
}

func TestSelectAccountRepublishesTheAccountCellWithTheNewRootMarked(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	twoRoots(f)
	f.db.sessions["w1"] = wsm.Session{Workspace: "w1", HostSessionID: "host-1", ConfigDir: "/config"}

	// Act
	if _, err := f.verbs.SelectAccount(context.Background(), "w1", "/config-work"); err != nil {
		t.Fatalf("SelectAccount: %v", err)
	}

	// Assert
	cells := f.topbarAccounts
	if len(cells) == 0 {
		t.Fatalf("the account cell was never republished after the switch")
	}
	last := cells[len(cells)-1]
	if last.Email != "work@example.com" {
		t.Fatalf("cell email = %q, want the chosen root's address", last.Email)
	}
	for _, option := range last.Options {
		if option.Current != (option.ConfigDir == "/config-work") {
			t.Fatalf("option %+v, want only the chosen root marked current", option)
		}
	}
}

func TestSelectAccountFilesASessionRowForAWorkspaceThatHasNone(t *testing.T) {
	// Arrange — a workspace registered but never brought up. The choice still
	// has to survive to its first start.
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	twoRoots(f)

	// Act
	if _, err := f.verbs.SelectAccount(context.Background(), "w1", "/config-work"); err != nil {
		t.Fatalf("SelectAccount: %v", err)
	}

	// Assert
	session, ok := f.db.sessions["w1"]
	if !ok {
		t.Fatalf("no session row was filed for the choice to live on")
	}
	if session.SelectedConfigDir != "/config-work" {
		t.Fatalf("selected root = %q, want the root the reader chose", session.SelectedConfigDir)
	}
	if session.HostSessionID == "" {
		t.Fatalf("the filed row names no host session identity, which withholds the host view")
	}
}

func TestABringUpSpendsFromTheChosenRootRatherThanTheRoutedOne(t *testing.T) {
	// Arrange
	accounts := &fakeAccounts{configDir: "/config"}

	// Act
	got := accountRootFor(accounts, "/work", wsm.Session{SelectedConfigDir: "/config-work"})

	// Assert
	if got != "/config-work" {
		t.Fatalf("root = %q, want the chosen one to outrank the path routing", got)
	}
}

func TestABringUpFollowsTheRoutingWhenNobodyHasChosen(t *testing.T) {
	// Arrange
	accounts := &fakeAccounts{configDir: "/config"}

	// Act
	got := accountRootFor(accounts, "/work", wsm.Session{ConfigDir: "/config-stale"})

	// Assert
	if got != "/config" {
		t.Fatalf("root = %q, want the routing to decide at every start", got)
	}
}

func TestABounceSpawnsUnderTheChosenRoot(t *testing.T) {
	// Arrange
	accounts := &fakeAccounts{configDir: "/config"}

	// Act
	got := spawnRootFor(accounts, "/work", wsm.Session{ConfigDir: "/config", SelectedConfigDir: "/config-work"})

	// Assert
	if got != "/config-work" {
		t.Fatalf("root = %q, want the chosen one to outrank the recorded root", got)
	}
}

func TestABounceSpawnsWhereTheSessionLivesWhenNobodyHasChosen(t *testing.T) {
	// Arrange — a bounce replaces the process under a session already filed
	// somewhere, so it does not re-decide the routing.
	accounts := &fakeAccounts{configDir: "/config-routed"}

	// Act
	got := spawnRootFor(accounts, "/work", wsm.Session{ConfigDir: "/config-filed"})

	// Assert
	if got != "/config-filed" {
		t.Fatalf("root = %q, want the root the session is filed under", got)
	}
}

// THE ACCOUNT SWITCH BOUNCES AT FREENESS: it is the one unforced shim bounce
// left, because the user asked to change accounts, not to end the work.
func TestSelectAccountBouncesUnforced(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.workspace("w1", t.TempDir())
	twoRoots(f)
	f.db.sessions["w1"] = wsm.Session{Workspace: "w1", HostSessionID: "host-1", ConfigDir: "/config"}

	// Act
	if _, err := f.verbs.SelectAccount(context.Background(), "w1", "/config-work"); err != nil {
		t.Fatalf("SelectAccount: %v", err)
	}

	// Assert
	f.rollout.awaitRelaunch(t)
	if got := f.rollout.relaunchCalls(); len(got) != 1 || got[0].Force {
		t.Fatalf("relaunches = %+v, want one unforced bounce", got)
	}
}
