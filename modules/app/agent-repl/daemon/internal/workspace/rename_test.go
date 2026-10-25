package workspace

import (
	"context"
	"errors"
	"slices"
	"testing"

	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/refusalarm"
	"agent-repld/internal/resolve/sidebar"
	"agent-repld/internal/wsm"
)

// renameFixture is a fixture with no workspace-name prefix and workspace w1,
// named "sample" on branch "ABC/sample".
func renameFixture(t *testing.T) *fixture {
	t.Helper()
	f := newFixture(t)
	t.Setenv(PrefixEnv, "")
	t.Setenv(LegacyPrefixEnv, "")
	f.workspace("w1", t.TempDir())
	return f
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
	return published
}

func TestSetNameRecordsTheNameAndRepublishesEverySurface(t *testing.T) {
	// Arrange.
	f := renameFixture(t)
	before := f.db.workspaces["w1"]
	registries := len(f.sidebar.registries)

	// Act.
	name, err := f.verbs.SetName(context.Background(), "w1", "  better-name  ")

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
	// Arrange: w1 has a principal link, awaits w2, is hidden and a one-shot.
	f := renameFixture(t)
	f.workspace("w2", t.TempDir())
	link, awaited := "https://example.com/pr/1", ids.WorkspaceID("w2")
	ws := f.db.workspaces["w1"]
	ws.PrincipalLink, ws.Awaiting, ws.Hidden, ws.OneShot = &link, &awaited, true, true
	f.db.workspaces["w1"] = ws

	// Act.
	_, err := f.verbs.SetName(context.Background(), "w1", "better-name")

	// Assert: the record is the one it was, but for its name.
	if err != nil {
		t.Fatalf("SetName: %v", err)
	}
	after := f.db.workspaces["w1"]
	after.Name = ws.Name
	if after.PrincipalLink != ws.PrincipalLink || after.Awaiting != ws.Awaiting || after.Hidden != ws.Hidden || after.OneShot != ws.OneShot || after.ID != ws.ID {
		t.Errorf("after the rename = %+v, want %+v but for the name", after, ws)
	}
}

func TestSetNameAppliesTheWorkspaceNamePrefix(t *testing.T) {
	// Arrange.
	f := renameFixture(t)
	t.Setenv(PrefixEnv, "ABC")

	// Act.
	name, err := f.verbs.SetName(context.Background(), "w1", "better-name")

	// Assert.
	if err != nil || name != "ABC/better-name" || f.db.workspaces["w1"].Name != "ABC/better-name" {
		t.Fatalf("SetName = %q, %v, recorded %q; want ABC/better-name", name, err, f.db.workspaces["w1"].Name)
	}
}

func TestSetNameToTheNameItHasChangesNothing(t *testing.T) {
	// Arrange.
	f := renameFixture(t)
	registries := len(f.sidebar.registries)

	// Act.
	name, err := f.verbs.SetName(context.Background(), "w1", "sample")

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
	name, err := f.verbs.SetName(context.Background(), "w1", "ABC/sample")

	// Assert.
	if err != nil || name != "ABC/sample" {
		t.Fatalf("SetName = %q, %v; want its own branch accepted", name, err)
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
			name, err := f.verbs.SetName(context.Background(), "w1", "better-name")

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
