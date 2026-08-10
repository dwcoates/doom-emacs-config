package server

import (
	"fmt"
	"path/filepath"
	"testing"

	"claude-repld/internal/registry"
	"claude-repld/internal/session"
)

// ---------------------------------------------------------------------------
// THE ACCOUNT RULE. A human's selection, else the path, and nothing else. The
// two must stay distinguishable: reading a resolved account as a selection is
// what turned "the daemon named no account" into "the user chose ~/.claude"
// and pinned a workspace under $MULTI_REPO_ROOT to the wrong root forever.
// ---------------------------------------------------------------------------

// accountsOver builds a resolver over a registry holding recs.
func accountsOver(t *testing.T, recs ...registry.Record) AccountResolver {
	t.Helper()
	reg := openTestRegistry(t)
	for _, rec := range recs {
		if err := reg.Put(rec); err != nil {
			t.Fatalf("put %s: %v", rec.SessionID, err)
		}
	}
	return AccountResolver{Reg: reg, Logf: t.Logf}
}

// multiRepo points $MULTI_REPO_ROOT at a temp root and returns (root, account).
func multiRepo(t *testing.T) (string, string) {
	t.Helper()
	home := t.TempDir()
	root := filepath.Join(home, "workspace", "ChessCom")
	account := filepath.Join(home, ".claude-chesscom")
	t.Setenv(session.MultiRepoRootEnv, root)
	t.Setenv(session.MultiRepoConfigDirEnv, account)
	return root, account
}

func TestAccountFollowsThePathWhenNobodyHasSelected(t *testing.T) {
	// Arrange
	root, account := multiRepo(t)
	cwd := filepath.Join(root, "explanation-engine-worktrees", "slack-thread-pr-link")
	accounts := accountsOver(t)

	// Act
	got, err := accounts.Resolve(cwd, cwd)

	// Assert
	if err != nil || got != account {
		t.Fatalf("Resolve = (%q, %v), want the multi-repo account %q", got, err, account)
	}
}

func TestASelectionOutranksThePath(t *testing.T) {
	// Arrange — the webapp switcher moved this ChessCom workspace to the
	// default account. That choice is the one thing that may move it.
	root, _ := multiRepo(t)
	cwd := filepath.Join(root, "explanation-engine-worktrees", "w")
	chosen := filepath.Join(t.TempDir(), ".claude")
	accounts := accountsOver(t, registry.Record{
		SessionID: "s1", CWD: cwd, ConfigDir: chosen, ConfigDirOverride: chosen,
		CreatedAt: "2026-08-09T10:00:00Z",
	})

	// Act
	got, err := accounts.Resolve(cwd, cwd)

	// Assert
	if err != nil || got != chosen {
		t.Fatalf("Resolve = (%q, %v), want the selected account %q", got, err, chosen)
	}
}

func TestAResolvedAccountIsNotASelection(t *testing.T) {
	// Arrange — a record that RAN under an account but carries no selection.
	// Reading ConfigDir as a choice is the whole bug.
	root, account := multiRepo(t)
	cwd := filepath.Join(root, "explanation-engine-worktrees", "w")
	accounts := accountsOver(t, registry.Record{
		SessionID: "s1", CWD: cwd, ConfigDir: "/somewhere-else",
		CreatedAt: "2026-08-09T10:00:00Z",
	})

	// Act
	got, err := accounts.Resolve(cwd, cwd)

	// Assert
	if err != nil || got != account {
		t.Fatalf("Resolve = (%q, %v), want the path's account %q", got, err, account)
	}
}

func TestTheNewestSelectionWins(t *testing.T) {
	// Arrange — a workspace switched twice. The later choice is the live one.
	_, _ = multiRepo(t)
	accounts := accountsOver(t,
		registry.Record{SessionID: "s1", CWD: "/w", ConfigDirOverride: "/first", CreatedAt: "2026-08-01T10:00:00Z"},
		registry.Record{SessionID: "s2", CWD: "/w", ConfigDirOverride: "/second", CreatedAt: "2026-08-09T10:00:00Z"},
	)

	// Act
	got := accounts.SelectionFor("/w")

	// Assert
	if got != "/second" {
		t.Fatalf("SelectionFor = %q, want the newest selection /second", got)
	}
}

func TestASelectionIsNotLostToANewerRecordWithoutOne(t *testing.T) {
	// Arrange — every restore writes a new record, and most of them carry no
	// selection. Taking the newest record outright would drop the choice the
	// moment one of those sorted last.
	_, _ = multiRepo(t)
	accounts := accountsOver(t,
		registry.Record{SessionID: "s1", CWD: "/w", ConfigDirOverride: "/chosen", CreatedAt: "2026-08-01T10:00:00Z"},
		registry.Record{SessionID: "s2", CWD: "/w", CreatedAt: "2026-08-09T10:00:00Z"},
	)

	// Act
	got := accounts.SelectionFor("/w")

	// Assert
	if got != "/chosen" {
		t.Fatalf("SelectionFor = %q, want the selection to survive a later record without one", got)
	}
}

func TestAChildInheritsItsParentsSelection(t *testing.T) {
	// Arrange — a workspace created from one whose account a human switched.
	_, _ = multiRepo(t)
	accounts := accountsOver(t, registry.Record{
		SessionID: "s1", CWD: "/parent", ConfigDirOverride: "/chosen",
		CreatedAt: "2026-08-09T10:00:00Z",
	})

	// Act
	got := accounts.InheritSelection("/parent")

	// Assert
	if got != "/chosen" {
		t.Fatalf("InheritSelection = %q, want the parent's selection", got)
	}
}

func TestAChildInheritsNothingFromAParentThatChoseNothing(t *testing.T) {
	// Arrange — a parent that merely sits under $MULTI_REPO_ROOT has chosen
	// nothing, and its resolved account must not ride along to a child whose
	// own path answers differently.
	_, _ = multiRepo(t)
	accounts := accountsOver(t, registry.Record{
		SessionID: "s1", CWD: "/parent", ConfigDir: "/parent-ran-here",
		CreatedAt: "2026-08-09T10:00:00Z",
	})

	// Act
	got := accounts.InheritSelection("/parent")

	// Assert
	if got != "" {
		t.Fatalf("InheritSelection = %q, want nothing inherited", got)
	}
}

func TestAPinnedOneShotHasNoParentToInheritFrom(t *testing.T) {
	// Arrange — the one-shot flow nominates no source workspace, so there is
	// nothing to inherit and its path answers: a ChessCom one-shot lands on the
	// ChessCom account whatever workspace the keystroke came from.
	_, _ = multiRepo(t)
	accounts := accountsOver(t)

	// Act
	got := accounts.InheritSelection("")

	// Assert
	if got != "" {
		t.Fatalf("InheritSelection = %q, want nothing inherited", got)
	}
}

func TestNoRegistryFindsNoSelection(t *testing.T) {
	// Arrange — a selection that cannot be read must never be guessed at, so
	// the path answers alone rather than a remembered value being invented.
	_, account := multiRepo(t)
	accounts := AccountResolver{Logf: t.Logf}

	// Act
	got, err := accounts.Resolve("/w", "/w")

	// Assert
	if got != account && got != "" {
		t.Fatalf("Resolve = (%q, %v), want the path's answer with no registry", got, err)
	}
	if accounts.SelectionFor("/w") != "" {
		t.Fatal("a selection was reported with no registry to read one from")
	}
}

// ---------------------------------------------------------------------------
// The switch is where a selection is MADE, and the create is where it is
// honored. These two pin the round trip: without the first the choice is never
// recorded, and without the second the next create quietly undoes it.
// ---------------------------------------------------------------------------

func TestAnAccountSwitchRecordsTheSelection(t *testing.T) {
	// Arrange
	h := newHarnessWith(t, Config{Accounts: accountRoster()})
	id := createSession(t, h, `{"cwd":"/w"}`)
	markControllerOperational(t, h, "/w")

	// Act
	resp := postAccountSwitch(t, h, id, "/cfg-work")
	defer resp.Body.Close()

	// Assert — the account it RAN under and the account a human CHOSE are both
	// recorded, and they are different facts.
	rec, ok := h.reg.Get(id)
	if !ok {
		t.Fatal("the switched session has no record")
	}
	if rec.ConfigDir != "/cfg-work" || rec.ConfigDirOverride != "/cfg-work" {
		t.Fatalf("record = %+v, want both the resolved account and the selection", rec)
	}
}

func TestSwitchingToTheDefaultAccountRecordsAnAbsolutePath(t *testing.T) {
	// Arrange — the roster spells the default account "", and storing that as
	// the selection would make "nobody chose" and "the default was chosen"
	// the same value. The selection is the root's absolute path instead.
	h := newHarnessWith(t, Config{Accounts: accountRoster()})
	id := createSession(t, h, `{"cwd":"/w"}`)
	markControllerOperational(t, h, "/w")
	if resp := postAccountSwitch(t, h, id, "/cfg-work"); resp != nil {
		resp.Body.Close()
	}

	// Act — back to the default.
	resp := postAccountSwitch(t, h, id, "")
	defer resp.Body.Close()

	// Assert
	rec, _ := h.reg.Get(id)
	if rec.ConfigDir != "" {
		t.Fatalf("record = %+v, want the default account spelled empty", rec)
	}
	if rec.ConfigDirOverride != session.DefaultClaudeConfigDir() {
		t.Fatalf("selection = %q, want the default root's absolute path", rec.ConfigDirOverride)
	}
}

func TestACreateHonorsAPriorSelectionOverTheFramesAccount(t *testing.T) {
	// Arrange — a workspace a human switched, then recreated. The frame carries
	// the editor's path-computed answer, which is exactly what used to overwrite
	// the choice on the very next create.
	h := newHarnessWith(t, Config{Accounts: accountRoster()})
	id := createSession(t, h, `{"cwd":"/w"}`)
	markControllerOperational(t, h, "/w")
	if resp := postAccountSwitch(t, h, id, "/cfg-work"); resp != nil {
		resp.Body.Close()
	}

	// Act — the rule the frontend create applies before building its opts.
	accounts := AccountResolver{Reg: h.reg, Logf: t.Logf}
	selection := accounts.SelectionFor("/w")
	resolved, err := accounts.Resolve("/w", "/w")
	if err != nil {
		t.Fatalf("Resolve: %v", err)
	}
	next := createSession(t, h, fmt.Sprintf(`{"cwd":"/w","config_dir":%q,"config_dir_override":%q}`, resolved, selection))

	// Assert — the second session came up under the selected account and still
	// carries the selection, so the one after it does too.
	rec, _ := h.reg.Get(next)
	if rec.ConfigDir != "/cfg-work" || rec.ConfigDirOverride != "/cfg-work" {
		t.Fatalf("record = %+v, want the create to honor and carry the selection", rec)
	}
}
