package workspace

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"testing"

	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/bounce"
	"agent-repld/internal/dlog"
	"agent-repld/internal/gitclient"
	"agent-repld/internal/ids"
	"agent-repld/internal/refusalarm"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/rollout"
	"agent-repld/internal/wsm"
)

// renameFixture is a fixture with no workspace-name prefix and workspace w1,
// named "sample" on branch "ABC/sample".
func renameFixture(t *testing.T) *fixture {
	t.Helper()
	f := newFixture(t)
	t.Setenv(PrefixEnv, "")
	t.Setenv(LegacyPrefixEnv, "")
	f.workspace("w1", renameWorktree(t))
	return f
}

// renameWorktree is a linked worktree's directory inside a repository whose
// main worktree holds a `.git` directory, so the directory a rename moves it
// to (WorktreeDir) is derived, under the test's own temp dir.
func renameWorktree(t *testing.T) string {
	t.Helper()
	repo := filepath.Join(t.TempDir(), "repo")
	dir := filepath.Join(repo, "w1")
	if err := os.MkdirAll(filepath.Join(repo, ".git"), 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	return dir
}

// renamePublished reports what the rename republished, for a test that
// asserts nothing was.
func renamePublished(f *fixture, registriesBefore int) []string {
	var published []string
	if len(f.sidebar.registries) != registriesBefore {
		published = append(published, "roster")
	}
	if len(f.topbarNames) != 0 {
		published = append(published, "topbar")
	}
	if len(f.host.hostPublishes) != 0 {
		published = append(published, "host")
	}
	if len(f.merge.relabeled) != 0 {
		published = append(published, "merge queue")
	}
	if len(f.feed.names) != 0 {
		published = append(published, "feed")
	}
	return published
}

func TestSetNameRecordsTheNameAndRepublishesEverySurface(t *testing.T) {
	// Arrange.
	f := renameFixture(t)
	before := f.db.workspaces["w1"]
	registries := len(f.sidebar.registries)

	// Act.
	renamed, err := f.verbs.SetName(context.Background(), "w1", "  better-name  ")
	name := renamed.Name

	// Assert: the name alone changed, and every surface drawing it was told.
	if err != nil || name != "better-name" {
		t.Fatalf("SetName = %q, %v; want %q", name, err, "better-name")
	}
	after := f.db.workspaces["w1"]
	if after.Name != "better-name" || after.Branch != before.Branch || after.Dir != before.Dir {
		t.Errorf("record name, branch, dir = %q, %q, %q; want the new name and the unchanged %q, %q", after.Name, after.Branch, after.Dir, before.Branch, before.Dir)
	}
	if len(f.sidebar.registries) == registries || !registryNames(f.sidebar.registries[len(f.sidebar.registries)-1], "w1", "better-name") {
		t.Errorf("the roster was not republished with the new name")
	}
	if !slices.Equal(f.topbarNames, []string{"better-name"}) {
		t.Errorf("topbar names = %q, want [better-name]", f.topbarNames)
	}
	if !slices.Equal(f.host.hostPublishes, []ids.WorkspaceID{"w1"}) {
		t.Errorf("host publishes = %v, want [w1]", f.host.hostPublishes)
	}
	if !slices.Equal(f.merge.relabeled, []ids.WorkspaceID{"w1"}) {
		t.Errorf("merge relabels = %v, want [w1]", f.merge.relabeled)
	}
	if !slices.Equal(f.feed.names, []fakeFeedName{{ws: "w1", name: "better-name"}}) {
		t.Errorf("feed names = %v, want w1 named better-name", f.feed.names)
	}
	r, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opSetName})
	if !ok || r.Message != "renamed the workspace" || r.Context["name"] != "better-name" || r.Context["previous"] != "sample" || r.Context["branch"] != "ABC/sample" {
		t.Errorf("record = %+v, want an INFO %s naming the new name, the previous one and the branch", r, opSetName)
	}
}

// registryNames reports whether REG holds WS under NAME.
func registryNames(reg sidebar.Registry, ws ids.WorkspaceID, name string) bool {
	for _, record := range reg.Workspaces {
		if record.ID == ws {
			return record.Name == name
		}
	}
	return false
}

func TestSetNameKeepsEverythingKeyedByTheWorkspacesID(t *testing.T) {
	// Arrange: w1 awaits w2, is backlogged and a one-shot.
	f := renameFixture(t)
	f.workspace("w2", t.TempDir())
	awaited := ids.WorkspaceID("w2")
	ws := f.db.workspaces["w1"]
	ws.Awaiting, ws.Backlogged, ws.OneShot = &awaited, true, true
	f.db.workspaces["w1"] = ws

	// Act.
	_, err := f.verbs.SetName(context.Background(), "w1", "better-name")

	// Assert: the record is the one it was, but for its name.
	if err != nil {
		t.Fatalf("SetName: %v", err)
	}
	after := f.db.workspaces["w1"]
	after.Name = ws.Name
	if after.Awaiting != ws.Awaiting || after.Backlogged != ws.Backlogged || after.OneShot != ws.OneShot || after.ID != ws.ID {
		t.Errorf("after the rename = %+v, want %+v but for the name", after, ws)
	}
}

func TestSetNameAppliesTheWorkspaceNamePrefix(t *testing.T) {
	// Arrange.
	f := renameFixture(t)
	t.Setenv(PrefixEnv, "ABC")

	// Act.
	renamed, err := f.verbs.SetName(context.Background(), "w1", "better-name")
	name := renamed.Name

	// Assert.
	if err != nil || name != "ABC/better-name" || f.db.workspaces["w1"].Name != "ABC/better-name" {
		t.Fatalf("SetName = %q, %v, recorded %q; want ABC/better-name", name, err, f.db.workspaces["w1"].Name)
	}
}

func TestSetNameToTheNameItHasChangesNothing(t *testing.T) {
	// Arrange: w1's branch and directory already carry its name.
	f := renameFixture(t)
	carriesName(t, f, "w1", "sample")
	registries := len(f.sidebar.registries)

	// Act.
	renamed, err := f.verbs.SetName(context.Background(), "w1", "sample")
	name := renamed.Name

	// Assert.
	if err != nil || name != "sample" {
		t.Fatalf("SetName = %q, %v; want sample, no error", name, err)
	}
	if published := renamePublished(f, registries); len(published) != 0 {
		t.Errorf("republished %v for no change", published)
	}
	r, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opSetName})
	if !ok || r.Message != "the workspace already has the name; nothing changed" {
		t.Errorf("record = %+v, want an INFO %s saying nothing changed", r, opSetName)
	}
}

func TestSetNameMayNameTheWorkspaceAfterItsOwnBranch(t *testing.T) {
	// Arrange: w1 holds "ABC/sample" as its branch.
	f := renameFixture(t)

	// Act.
	renamed, err := f.verbs.SetName(context.Background(), "w1", "ABC/sample")
	name := renamed.Name

	// Assert.
	if err != nil || name != "ABC/sample" {
		t.Fatalf("SetName = %q, %v; want its own branch accepted", name, err)
	}
}

func TestSetNameGivesAMediatorItsOwnRolesName(t *testing.T) {
	cases := []struct {
		name      string
		role      wsm.MediatorRole
		requested string
		wantArm   string
	}{
		{name: "a mediator takes its own role's name, unprefixed", role: wsm.MediatorProduction, requested: "prod-mediator"},
		{name: "a mediator may not take another role's name", role: wsm.MediatorProduction, requested: "deploy-mediator", wantArm: ArmInvalidName},
		{name: "a workspace that is no mediator may not take a role's name", role: wsm.NotMediator, requested: "prod-mediator", wantArm: ArmInvalidName},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange: a prefix every other rename would take.
			f := renameFixture(t)
			t.Setenv(PrefixEnv, "ABC")
			ws := f.db.workspaces["w1"]
			ws.Mediator = tc.role
			f.db.workspaces["w1"] = ws

			// Act.
			renamed, err := f.verbs.SetName(context.Background(), "w1", tc.requested)
			name := renamed.Name

			// Assert.
			if tc.wantArm == "" {
				if err != nil || name != tc.requested || f.db.workspaces["w1"].Name != tc.requested {
					t.Fatalf("SetName = %q, %v, recorded %q; want %q as it stands", name, err, f.db.workspaces["w1"].Name, tc.requested)
				}
				return
			}
			if refusal, ok := AsRefusal(err); !ok || refusal.Arm != tc.wantArm || f.db.workspaces["w1"].Name != "sample" {
				t.Fatalf("SetName = %q, %v, recorded %q; want the %s refusal and the name kept", name, err, f.db.workspaces["w1"].Name, tc.wantArm)
			}
		})
	}
}

func TestSetNameRefusesANameThatBreaksTheNamingRule(t *testing.T) {
	tests := []struct {
		name      string
		requested string
		component string
		rule      refusalarm.Nested
	}{
		{name: "a slug that is not lowercase hyphenated words", requested: "Better Name", component: "Better Name", rule: NameRuleSlug},
		{name: "a prefix that is not letters and digits", requested: "A B/better", component: "A B", rule: NameRulePrefix},
		{name: "a standing workspace's name", requested: "prod-mediator", component: "prod-mediator", rule: NameRuleReserved},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := renameFixture(t)
			registries := len(f.sidebar.registries)

			// Act.
			_, err := f.verbs.SetName(context.Background(), "w1", tt.requested)

			// Assert: refused naming the component and its rule; nothing written
			// or republished.
			refusal := asRefusal(t, err, ArmInvalidName)
			if refusal.Fields["name"] != tt.requested || refusal.Fields["component"] != tt.component || refusal.Fields["rule"] != tt.rule {
				t.Errorf("refusal fields = %v, want %q at %q breaking %s", refusal.Fields, tt.requested, tt.component, tt.rule)
			}
			if got := f.db.workspaces["w1"].Name; got != "sample" {
				t.Errorf("name = %q, want the unchanged sample", got)
			}
			if published := renamePublished(f, registries); len(published) != 0 {
				t.Errorf("republished %v for a refused rename", published)
			}
			r, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opRefusal})
			if !ok || r.Context["arm"] != ArmInvalidName || r.Context["rpc"] != rpcSetName {
				t.Errorf("refusal record = %+v, want an INFO refusal of %s naming %s", r, rpcSetName, ArmInvalidName)
			}
		})
	}
}

func TestSetNameRefusesANameAlreadyHeld(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, f *fixture)
		// holder is the workspace the refusal names, empty for a pending one.
		holder ids.WorkspaceID
	}{
		{name: "another workspace's name", holder: "w2", arrange: func(t *testing.T, f *fixture) {
			f.db.with(wsm.Workspace{ID: "w2", Name: "better-name", Branch: "other-branch", Dir: t.TempDir(), Repo: "repo-1"})
		}},
		{name: "another workspace's branch, renamed away from", holder: "w2", arrange: func(t *testing.T, f *fixture) {
			f.db.with(wsm.Workspace{ID: "w2", Name: "something-else", Branch: "better-name", Dir: t.TempDir(), Repo: "repo-1"})
		}},
		{name: "a closed workspace's name", holder: "w2", arrange: func(t *testing.T, f *fixture) {
			f.db.with(wsm.Workspace{ID: "w2", Name: "better-name", Branch: "better-name", Dir: t.TempDir(), Repo: "repo-1", Closed: true})
		}},
		{name: "a pending async merge's reservation", arrange: func(_ *testing.T, f *fixture) {
			f.db.reserved = map[string]ids.WorkspaceID{"better-name": "w9"}
		}},
		{name: "a create's claim in flight", arrange: func(_ *testing.T, f *fixture) {
			_, release, taken, err := f.verbs.ClaimMergeName(context.Background(), claimRepo(t, f), "better-name")
			if err != nil || taken {
				t.Fatalf("ClaimMergeName = %v, %v; want the name claimed", taken, err)
			}
			t.Cleanup(release)
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := renameFixture(t)
			tt.arrange(t, f)
			registries := len(f.sidebar.registries)

			// Act.
			_, err := f.verbs.SetName(context.Background(), "w1", "better-name")

			// Assert: refused naming what holds it; nothing written or
			// republished.
			refusal := asRefusal(t, err, ArmNameTaken)
			if refusal.Fields["name"] != "better-name" {
				t.Errorf("refusal name = %v, want better-name", refusal.Fields["name"])
			}
			if tt.holder == "" {
				if refusal.Fields["holder"] != refusalarm.Nested("pending") {
					t.Errorf("refusal fields = %v, want the pending holder", refusal.Fields)
				}
			} else if ref, ok := refusal.Fields["workspace"].(*workspacev1.WorkspaceRef); !ok || ref.GetId() != string(tt.holder) || ref.GetDir() == "" {
				t.Errorf("refusal fields = %v, want the holder %s by its ref", refusal.Fields, tt.holder)
			}
			if got := f.db.workspaces["w1"].Name; got != "sample" {
				t.Errorf("name = %q, want the unchanged sample", got)
			}
			if published := renamePublished(f, registries); len(published) != 0 {
				t.Errorf("republished %v for a refused rename", published)
			}
		})
	}
}

func TestSetNameOfAnUnknownWorkspaceIsRefused(t *testing.T) {
	// Arrange.
	f := renameFixture(t)

	// Act.
	_, err := f.verbs.SetName(context.Background(), "absent", "better-name")

	// Assert.
	asRefusal(t, err, ArmUnknownWorkspace)
	if len(f.topbarNames) != 0 {
		t.Errorf("topbar names = %q, want none", f.topbarNames)
	}
}

func TestSetNameFailsLoudly(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fixture) error
		message string
	}{
		{name: "when the store will not write", message: "could not record the workspace's new name", arrange: func(f *fixture) error {
			f.db.setNameErr = errors.New("disk full")
			return f.db.setNameErr
		}},
		{name: "when the names taken cannot be read", message: "could not read the names already taken; the workspace is not renamed", arrange: func(f *fixture) error {
			f.db.reservedErr = errors.New("store closed")
			return f.db.reservedErr
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := renameFixture(t)
			cause := tt.arrange(f)
			registries := len(f.sidebar.registries)

			// Act.
			_, err := f.verbs.SetName(context.Background(), "w1", "better-name")

			// Assert: the cause surfaces and is recorded with its context, and
			// no surface is told a name the store does not hold.
			if !errors.Is(err, cause) {
				t.Fatalf("SetName = %v, want the cause %v", err, cause)
			}
			if got := f.db.workspaces["w1"].Name; got != "sample" {
				t.Errorf("name = %q, want the unchanged sample", got)
			}
			if published := renamePublished(f, registries); len(published) != 0 {
				t.Errorf("republished %v for a failed rename", published)
			}
			r, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opSetName})
			if !ok || r.Message != tt.message || r.Context["name"] != "better-name" || r.Context["cause"] == nil {
				t.Errorf("record = %+v, want an ERROR %s %q naming the name and the cause", r, opSetName, tt.message)
			}
		})
	}
}

func TestSetNameRelabelsTheFootersOfTheWorkspacesAwaitingIt(t *testing.T) {
	// Arrange: w2 awaits w1, w3 awaits w2.
	f := renameFixture(t)
	for _, waiting := range []struct{ id, awaits ids.WorkspaceID }{{"w2", "w1"}, {"w3", "w2"}} {
		awaited := waiting.awaits
		f.db.with(wsm.Workspace{ID: waiting.id, Name: string(waiting.id), Branch: string(waiting.id), Dir: t.TempDir(), Repo: "repo-1", Awaiting: &awaited})
	}

	// Act.
	_, err := f.verbs.SetName(context.Background(), "w1", "better-name")

	// Assert.
	if err != nil {
		t.Fatalf("SetName: %v", err)
	}
	if got := f.footer.awaiting["w2"]; got != "better-name" {
		t.Errorf("w2's awaiting line names %q, want better-name", got)
	}
	if _, told := f.footer.awaiting["w3"]; told {
		t.Errorf("w3, which awaits another workspace, was told %q", f.footer.awaiting["w3"])
	}
}

func TestSetNameStandsWhenASurfaceCannotBeRepublished(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fixture)
		level   string
		message string
	}{
		{name: "the awaiting workspaces cannot be listed", level: dlog.LevelError,
			message: "could not list the workspaces awaiting the renamed one; their footers keep its old name",
			arrange: func(f *fixture) { f.db.listWorkspacesErrAfterRename = errors.New("store closed") }},
		{name: "the merge queue cannot relabel it", level: dlog.LevelDebug,
			message: "the merge queue could not relabel the renamed workspace; its owner recorded why",
			arrange: func(f *fixture) { f.merge.relabelErr = errors.New("queue unreadable") }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := renameFixture(t)
			tt.arrange(f)

			// Act.
			renamed, err := f.verbs.SetName(context.Background(), "w1", "better-name")
			name := renamed.Name

			// Assert: the rename is recorded and answered, and the surface that
			// could not follow is on the record.
			if err != nil || name != "better-name" || f.db.workspaces["w1"].Name != "better-name" {
				t.Fatalf("SetName = %q, %v, recorded %q; want the rename to stand", name, err, f.db.workspaces["w1"].Name)
			}
			r, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: tt.level, Operation: opSetName, Message: tt.message})
			if !ok || r.Context["cause"] == nil {
				t.Errorf("no %s %s record %q naming the cause: %+v", tt.level, opSetName, tt.message, f.log.logger.Records())
			}
		})
	}
}

// carriesName moves WS's record to the branch NAME and the directory a rename to
// NAME derives, creating that directory, so the workspace already carries the
// name everywhere.
func carriesName(t *testing.T, f *fixture, ws ids.WorkspaceID, name string) {
	t.Helper()
	record := f.db.workspaces[ws]
	repo, _ := wsm.RepositoryWithID(f.db.repositories, record.Repo)
	dir, err := WorktreeDir(repo.Dir, name)
	if err != nil {
		t.Fatalf("WorktreeDir: %v", err)
	}
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	record.Name, record.Branch, record.Dir = name, name, dir
	f.db.workspaces[ws] = record
}

// targetOf is the directory a rename of WS to NAME moves it to.
func targetOf(t *testing.T, f *fixture, ws ids.WorkspaceID, name string) string {
	t.Helper()
	repo, _ := wsm.RepositoryWithID(f.db.repositories, f.db.workspaces[ws].Repo)
	dir, err := WorktreeDir(repo.Dir, name)
	if err != nil {
		t.Fatalf("WorktreeDir: %v", err)
	}
	return dir
}

func TestSetNameOwesTheMoveOfAnOpenWorkspaceAndAsksForIt(t *testing.T) {
	// Arrange
	f := renameFixture(t)
	before := f.db.workspaces["w1"]

	// Act
	renamed, err := f.verbs.SetName(context.Background(), "w1", "better-name")

	// Assert: the name is recorded, the move is owed to the derived branch
	// and directory, and the bounce registry is asked to run it unforced.
	if err != nil || renamed.Move != MoveOwed || renamed.To.Branch != "better-name" || renamed.To.Dir != targetOf(t, f, "w1", "better-name") {
		t.Fatalf("SetName = (%+v, %v), want the move owed to better-name", renamed, err)
	}
	if m, owed := f.db.owedMoves["w1"]; !owed || m.From.Dir != before.Dir || m.From.Branch != before.Branch {
		t.Fatalf("owed move = %+v (%v), want one from w1's branch and directory", m, owed)
	}
	if calls := f.rollout.relaunches; len(calls) != 1 || calls[0].Reason != rollout.ReasonRename || calls[0].Force {
		t.Fatalf("bounces = %+v, want one unforced rename", calls)
	}
}

func TestSetNameToItsOwnNameMovesABranchThatDiffers(t *testing.T) {
	// Arrange: w1 is named "sample" on branch "ABC/sample".
	f := renameFixture(t)

	// Act
	renamed, err := f.verbs.SetName(context.Background(), "w1", "sample")

	// Assert
	if err != nil || renamed.Move != MoveOwed || renamed.To.Branch != "sample" {
		t.Fatalf("SetName = (%+v, %v), want its branch and directory moved to its name", renamed, err)
	}
}

func TestSetNameOfAClosedWorkspaceMovesItAtOnce(t *testing.T) {
	// Arrange
	f := renameFixture(t)
	ws := f.db.workspaces["w1"]
	ws.Closed = true
	f.db.workspaces["w1"] = ws
	f.git.existingBranches = map[string]bool{"ABC/sample": true}
	to := targetOf(t, f, "w1", "better-name")

	// Act
	renamed, err := f.verbs.SetName(context.Background(), "w1", "better-name")

	// Assert
	if err != nil || renamed.Move != MoveMoved {
		t.Fatalf("SetName = (%+v, %v), want the move made before the answer", renamed, err)
	}
	after := f.db.workspaces["w1"]
	if after.Branch != "better-name" || after.Dir != to {
		t.Fatalf("record = (%q, %q), want the moved branch and directory %s", after.Branch, after.Dir, to)
	}
	if _, err := os.Stat(to); err != nil {
		t.Fatalf("the worktree is not at its new directory: %v", err)
	}
	if len(f.rollout.relaunches) != 0 {
		t.Fatalf("bounces = %+v, want none for a closed workspace", f.rollout.relaunches)
	}
}

func TestSetNameOfAMainCheckoutMovesNothing(t *testing.T) {
	// Arrange: w1 is its repository's main checkout.
	f := renameFixture(t)
	ws := f.db.workspaces["w1"]
	f.db.repositories = []wsm.Repository{{ID: ws.Repo, Dir: ws.Dir}}

	// Act
	renamed, err := f.verbs.SetName(context.Background(), "w1", "better-name")

	// Assert
	if err != nil || renamed.Move != MoveNone || f.db.workspaces["w1"].Name != "better-name" {
		t.Fatalf("SetName = (%+v, %v), want the name alone changed", renamed, err)
	}
	if len(f.db.owedMoves) != 0 || len(f.rollout.relaunches) != 0 {
		t.Fatalf("owed moves %v, bounces %v; want none", f.db.owedMoves, f.rollout.relaunches)
	}
}

func TestSetNameRefusesAMoveItCannotMake(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, f *fixture)
		arm     string
		field   string
	}{
		{name: "its merge stands in a queue", arm: ArmMerging, arrange: func(_ *testing.T, f *fixture) {
			ws := f.db.workspaces["w1"]
			ws.MergePending = true
			f.db.workspaces["w1"] = ws
		}},
		{name: "a merge holds its lease", arm: ArmMerging, arrange: func(_ *testing.T, f *fixture) {
			f.db.lease = &wsm.Lease{Holder: wsm.HolderMerge}
		}},
		{name: "an async merge waits on its branch", arm: ArmMerging, arrange: func(_ *testing.T, f *fixture) {
			f.db.asyncRequests = []wsm.AsyncMergeRequest{{Source: wsm.MergeSource{Branch: "ABC/sample"}}}
		}},
		{name: "its branch tracks a remote one", arm: ArmBranchPublished, field: "upstream", arrange: func(_ *testing.T, f *fixture) {
			f.git.existingBranches = map[string]bool{"ABC/sample": true}
			f.git.upstreams = map[string]gitclient.Upstream{"ABC/sample": {Remote: "origin", Ref: "origin/ABC/sample", Branch: "ABC/sample"}}
		}},
		{name: "a branch by the new name exists", arm: ArmBranchExists, field: "branch", arrange: func(_ *testing.T, f *fixture) {
			f.git.existingBranches = map[string]bool{"better-name": true}
		}},
		{name: "the new directory exists", arm: ArmDirectoryExists, field: "dir", arrange: func(t *testing.T, f *fixture) {
			if err := os.MkdirAll(targetOf(t, f, "w1", "better-name"), 0o755); err != nil {
				t.Fatal(err)
			}
		}},
		{name: "its previous move is under way", arm: ArmMoving, arrange: func(_ *testing.T, f *fixture) {
			f.db.owedMoves = map[ids.WorkspaceID]wsm.OwedMove{"w1": {Workspace: "w1", Moving: true}}
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := renameFixture(t)
			tt.arrange(t, f)
			registries := len(f.sidebar.registries)

			// Act
			_, err := f.verbs.SetName(context.Background(), "w1", "better-name")

			// Assert: refused on its arm, nothing recorded or asked for.
			refusal := asRefusal(t, err, tt.arm)
			if tt.field != "" && refusal.Fields[tt.field] == nil {
				t.Errorf("refusal fields = %v, want %s", refusal.Fields, tt.field)
			}
			if got := f.db.workspaces["w1"].Name; got != "sample" {
				t.Errorf("name = %q, want the unchanged sample", got)
			}
			if published := renamePublished(f, registries); len(published) != 0 {
				t.Errorf("republished %v for a refused rename", published)
			}
			if len(f.rollout.relaunches) != 0 {
				t.Errorf("bounces = %+v, want none", f.rollout.relaunches)
			}
		})
	}
}

func TestSetNameWhoseMoveTheRegistryRefusesStaysOwed(t *testing.T) {
	tests := []struct {
		name  string
		err   error
		level string
	}{
		{name: "a workspace moving to another daemon is that daemon's to move", err: bounce.ErrMovedAway, level: dlog.LevelInfo},
		{name: "any other refusal is an error", err: errors.New("registry closed"), level: dlog.LevelError},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := renameFixture(t)
			f.rollout.refuseErr = tt.err

			// Act
			renamed, err := f.verbs.SetName(context.Background(), "w1", "better-name")

			// Assert
			if err != nil || renamed.Move != MoveOwed {
				t.Fatalf("SetName = (%+v, %v), want the move owed", renamed, err)
			}
			if _, owed := f.db.owedMoves["w1"]; !owed {
				t.Fatal("the move is not owed")
			}
			if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: tt.level, Operation: opRenameMove}); !ok {
				t.Fatalf("records = %v, want one at %s", f.log.logger.Records(), tt.level)
			}
		})
	}
}

func TestApplyOwedMoveMovesEverythingThenTheRecords(t *testing.T) {
	// Arrange
	f := renameFixture(t)
	from := f.db.workspaces["w1"].Dir
	f.git.existingBranches = map[string]bool{"ABC/sample": true}
	if _, err := f.verbs.SetName(context.Background(), "w1", "better-name"); err != nil {
		t.Fatalf("SetName: %v", err)
	}
	to := targetOf(t, f, "w1", "better-name")
	registries := len(f.sidebar.registries)

	// Act
	err := f.verbs.ApplyOwedMove(context.Background(), "w1")

	// Assert
	if err != nil {
		t.Fatalf("ApplyOwedMove: %v", err)
	}
	if !slices.Equal(f.git.moves, [][2]string{{from, to}}) || !slices.Equal(f.git.renamedBranches, [][2]string{{"ABC/sample", "better-name"}}) {
		t.Fatalf("git moves %v, renames %v; want the worktree and the branch moved", f.git.moves, f.git.renamedBranches)
	}
	if !slices.Equal(f.account.projectMoves, [][2]string{{from, to}}) {
		t.Fatalf("project moves = %v, want the vendor folder moved", f.account.projectMoves)
	}
	after := f.db.workspaces["w1"]
	if after.Branch != "better-name" || after.Dir != to || len(f.db.owedMoves) != 0 {
		t.Fatalf("record = (%q, %q), owed %v; want the move completed", after.Branch, after.Dir, f.db.owedMoves)
	}
	if len(f.sidebar.registries) == registries {
		t.Fatal("the roster was not republished with the moved directory")
	}
}

func TestApplyOwedMoveWithNothingOwedDoesNothing(t *testing.T) {
	// Arrange
	f := renameFixture(t)

	// Act
	err := f.verbs.ApplyOwedMove(context.Background(), "w1")

	// Assert
	if err != nil || len(f.git.moves) != 0 {
		t.Fatalf("ApplyOwedMove = %v, moves %v; want nothing done", err, f.git.moves)
	}
}

func TestApplyOwedMoveSkipsWhatIsNotOnDisk(t *testing.T) {
	// Arrange: the worktree and the branch are gone (a landed workspace).
	f := renameFixture(t)
	if _, err := f.verbs.SetName(context.Background(), "w1", "better-name"); err != nil {
		t.Fatalf("SetName: %v", err)
	}
	if err := os.RemoveAll(f.db.workspaces["w1"].Dir); err != nil {
		t.Fatal(err)
	}

	// Act
	err := f.verbs.ApplyOwedMove(context.Background(), "w1")

	// Assert
	if err != nil || len(f.git.moves) != 0 || len(f.git.renamedBranches) != 0 {
		t.Fatalf("ApplyOwedMove = %v, moves %v, renames %v; want only the records moved", err, f.git.moves, f.git.renamedBranches)
	}
	if f.db.workspaces["w1"].Branch != "better-name" {
		t.Fatal("the records did not take the new branch")
	}
}

func TestApplyOwedMoveFailsLoudlyAndStaysOwed(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(f *fixture) error
		message string
	}{
		{name: "the move cannot be marked begun", message: "could not record that the move begins; nothing is moved", arrange: func(f *fixture) error {
			f.db.markMovingErr = errors.New("disk full")
			return f.db.markMovingErr
		}},
		{name: "the worktree cannot move", arrange: func(f *fixture) error {
			f.git.moveErr = errors.New("locked")
			return f.git.moveErr
		}},
		{name: "the vendor folder cannot move", arrange: func(f *fixture) error {
			f.account.projectMoveErr = errors.New("collision")
			return f.account.projectMoveErr
		}},
		{name: "the records cannot take the move", message: "the branch and directory moved but the records could not take them; the next boot finishes the move", arrange: func(f *fixture) error {
			f.db.completeMoveErr = errors.New("disk full")
			return f.db.completeMoveErr
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := renameFixture(t)
			if _, err := f.verbs.SetName(context.Background(), "w1", "better-name"); err != nil {
				t.Fatalf("SetName: %v", err)
			}
			cause := tt.arrange(f)

			// Act
			err := f.verbs.ApplyOwedMove(context.Background(), "w1")

			// Assert: the cause surfaces, the move stays owed, and the
			// verb's own step is recorded at ERROR when it is the verb's.
			if !errors.Is(err, cause) {
				t.Fatalf("ApplyOwedMove = %v, want %v", err, cause)
			}
			if _, owed := f.db.owedMoves["w1"]; !owed {
				t.Fatal("the failed move is no longer owed")
			}
			if tt.message != "" {
				if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opRenameMove, Message: tt.message}); !ok {
					t.Fatalf("records = %v, want %q at ERROR", f.log.logger.Records(), tt.message)
				}
			}
		})
	}
}

func TestFinishMovingMovesFinishesOnlyTheMovesUnderWay(t *testing.T) {
	// Arrange: w1's move had begun; w2's had not.
	f := renameFixture(t)
	f.workspace("w2", renameWorktree(t))
	for _, ws := range []ids.WorkspaceID{"w1", "w2"} {
		if _, err := f.verbs.SetName(context.Background(), ws, "moved-"+string(ws)); err != nil {
			t.Fatalf("SetName(%s): %v", ws, err)
		}
	}
	m := f.db.owedMoves["w1"]
	m.Moving = true
	f.db.owedMoves["w1"] = m

	// Act
	err := f.verbs.FinishMovingMoves(context.Background())

	// Assert
	if err != nil {
		t.Fatalf("FinishMovingMoves: %v", err)
	}
	if _, owed := f.db.owedMoves["w1"]; owed {
		t.Fatal("the move under way was not finished")
	}
	if _, owed := f.db.owedMoves["w2"]; !owed {
		t.Fatal("the move not yet begun was run at boot; its bounce runs it")
	}
}

func TestMoveOwedIsTrueOnlyForAMoveNotUnderWay(t *testing.T) {
	tests := []struct {
		name  string
		moves map[ids.WorkspaceID]wsm.OwedMove
		want  bool
	}{
		{name: "no move owed", want: false},
		{name: "a move owed", moves: map[ids.WorkspaceID]wsm.OwedMove{"w1": {Workspace: "w1"}}, want: true},
		{name: "a move under way", moves: map[ids.WorkspaceID]wsm.OwedMove{"w1": {Workspace: "w1", Moving: true}}, want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := renameFixture(t)
			f.db.owedMoves = tt.moves

			// Act
			got, err := f.verbs.MoveOwed(context.Background(), "w1")

			// Assert
			if err != nil || got != tt.want {
				t.Fatalf("MoveOwed = (%v, %v), want %v", got, err, tt.want)
			}
		})
	}
}
