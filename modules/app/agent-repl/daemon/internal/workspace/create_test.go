package workspace

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/account"
	"claude-repld/internal/ids"
	"claude-repld/internal/prompts"
	"claude-repld/internal/wsm"
)

// mainWorktree makes a directory that looks like a repository's MAIN worktree,
// which is what the worktree-directory rule branches on, and REGISTERS it.
//
// The registration is not decoration. A create naming a repository the registry
// does not hold is refused on `unknown_repository' (create.go), so a fixture
// repository nothing registered would refuse every create in the package.
func mainWorktree(t *testing.T, f *fixture) string {
	t.Helper()
	dir := t.TempDir()
	if err := os.MkdirAll(filepath.Join(dir, ".git"), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	normalized, err := normalizeDir(dir)
	if err != nil {
		t.Fatalf("normalizeDir: %v", err)
	}
	f.db.repositories = append(f.db.repositories, wsm.Repository{
		ID:            ids.RepoID("repo-" + filepath.Base(normalized)),
		Dir:           normalized,
		Name:          filepath.Base(normalized),
		DefaultBranch: "master",
	})
	return normalized
}

// standardSpec is the ordinary creation form against a real, registered main
// worktree.
func standardSpec(t *testing.T, f *fixture) CreateSpec {
	t.Helper()
	return CreateSpec{RepoDir: mainWorktree(t, f), InitialPrompt: "fix the login bug"}
}

// oneShotBriefs arranges the two briefs the one-shot decoration reads.
func oneShotBriefs(f *fixture) {
	f.briefs[BriefAutonomousPreamble] = prompts.Prompt{Name: BriefAutonomousPreamble, Body: "PREAMBLE\n"}
	f.briefs[BriefOneShotCompletionDirective] = prompts.Prompt{
		Name: BriefOneShotCompletionDirective, Body: "DIRECTIVE",
	}
}

func TestCreateNamesTheBranchFromTheModelsAnswer(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	t.Setenv(PrefixEnv, "ABC")
	t.Setenv(LegacyPrefixEnv, "")

	// Act.
	if _, err := f.verbs.Create(context.Background(), standardSpec(t, f)); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if len(f.git.created) != 1 || f.git.created[0].Branch != "ABC/"+FixtureMintedName {
		t.Fatalf("created worktrees = %+v, want branch ABC/%s", f.git.created, FixtureMintedName)
	}
}

func TestCreatePutsTheWorktreeInTheSiblingWorktreesDirectory(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	t.Setenv(PrefixEnv, "ABC")
	spec := standardSpec(t, f)

	// Act.
	if _, err := f.verbs.Create(context.Background(), spec); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	want := filepath.Join(filepath.Dir(spec.RepoDir), filepath.Base(spec.RepoDir)+WorktreeDirSuffix, FixtureMintedName)
	if f.git.created[0].WorktreeDir != want {
		t.Fatalf("worktree dir = %q, want %q", f.git.created[0].WorktreeDir, want)
	}
}

func TestCreateUsesTheSuppliedName(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	t.Setenv(PrefixEnv, "ABC")
	spec := standardSpec(t, f)
	spec.Name = "chosen-name"

	// Act.
	if _, err := f.verbs.Create(context.Background(), spec); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if f.git.created[0].Branch != "ABC/chosen-name" {
		t.Fatalf("branch = %q, want ABC/chosen-name", f.git.created[0].Branch)
	}
}

func TestCreateKeepsASuppliedNameThatAlreadyCarriesAPrefix(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	t.Setenv(PrefixEnv, "ABC")
	spec := standardSpec(t, f)
	spec.Name = "OTHER/chosen"

	// Act.
	if _, err := f.verbs.Create(context.Background(), spec); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if f.git.created[0].Branch != "OTHER/chosen" {
		t.Fatalf("branch = %q, want OTHER/chosen", f.git.created[0].Branch)
	}
}

func TestCreateDefaultsTheBaseRefToTheRepositoryDefaultBranch(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.git.defaultBranch = "main"

	// Act.
	if _, err := f.verbs.Create(context.Background(), standardSpec(t, f)); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if f.git.created[0].BaseRef != "main" {
		t.Fatalf("base ref = %q, want main", f.git.created[0].BaseRef)
	}
}

func TestCreateKeepsAnExplicitBaseRef(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	spec := standardSpec(t, f)
	spec.BaseRef = "release/1.2"

	// Act.
	if _, err := f.verbs.Create(context.Background(), spec); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if f.git.created[0].BaseRef != "release/1.2" {
		t.Fatalf("base ref = %q, want release/1.2", f.git.created[0].BaseRef)
	}
}

func TestCreateRecordsTheCreationJobBeforeMaterialization(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	if _, err := f.verbs.Create(context.Background(), standardSpec(t, f)); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert: the FIRST recorded job says the worktree does not exist yet.
	if len(f.db.putJobs) < 2 {
		t.Fatalf("creation jobs recorded = %d, want at least two", len(f.db.putJobs))
	}
	if f.db.putJobs[0].Materialized {
		t.Fatal("the first recorded creation job is already marked materialized")
	}
	if !f.db.putJobs[len(f.db.putJobs)-1].Materialized {
		t.Fatal("the last recorded creation job is not marked materialized")
	}
}

func TestCreateRecordsTheMergeLayoutAtCreation(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	spec := standardSpec(t, f)

	// Act.
	if _, err := f.verbs.Create(context.Background(), spec); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert: a merge is refused rather than guessed later, so the geometry is
	// whole here or nowhere.
	layout := f.db.putJobs[0].Layout
	switch {
	case layout.SourceBranch == "":
		t.Fatal("the recorded layout names no source branch")
	case layout.SourceDir == "":
		t.Fatal("the recorded layout names no source dir")
	case layout.TargetDir != spec.RepoDir:
		t.Fatalf("target dir = %q, want the main worktree %q", layout.TargetDir, spec.RepoDir)
	case layout.Origin != OriginCreateStandard:
		t.Fatalf("origin = %q, want %q", layout.Origin, OriginCreateStandard)
	}
}

func TestCreateTargetsTheParentWorktreeWhenCutFromOne(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	parentDir := t.TempDir()
	parent := f.workspace("parent", parentDir)
	spec := standardSpec(t, f)
	id := parent.ID
	spec.ForkFrom = &id
	f.db.sessions[parent.ID] = wsm.Session{Workspace: parent.ID, VendorSessionID: "vendor-1"}
	f.account.transcript = account.Transcript{Path: "/transcripts/vendor-1.jsonl", ConfigDir: "/roots/default"}

	// Act.
	if _, err := f.verbs.Create(context.Background(), spec); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if f.db.putJobs[0].Layout.TargetDir != parent.Dir {
		t.Fatalf("target dir = %q, want the parent worktree %q", f.db.putJobs[0].Layout.TargetDir, parent.Dir)
	}
}

func TestCreateRecordsTheSpawningParentOnTheWorkspace(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	parent := f.workspace("parent", t.TempDir())
	spec := standardSpec(t, f)
	id := parent.ID
	spec.Parent = &id

	// Act.
	record, err := f.verbs.Create(context.Background(), spec)
	if err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if record.Parent == nil || *record.Parent != parent.ID {
		t.Fatalf("parent = %v, want %q recorded at creation", record.Parent, parent.ID)
	}
}

func TestCreateRecordsNoParentWhenSpawnedFromNone(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	record, err := f.verbs.Create(context.Background(), standardSpec(t, f))
	if err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if record.Parent != nil {
		t.Fatalf("parent = %q, want none for a top-level create", *record.Parent)
	}
}

func TestCreateRegistersOnlyAfterTheWorktreeExists(t *testing.T) {
	// Arrange: materialization fails, so nothing may be registered.
	f := newFixture(t)
	f.git.createErr = errors.New("branch already checked out")

	// Act.
	_, err := f.verbs.Create(context.Background(), standardSpec(t, f))

	// Assert.
	if err == nil {
		t.Fatal("Create() = nil error, want the materialization failure surfaced")
	}
	if len(f.db.registered) != 0 {
		t.Fatalf("registered %d workspaces, want none after a failed materialization", len(f.db.registered))
	}
}

func TestCreateRefusesAnUngatedModeWithoutConsent(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	spec := standardSpec(t, f)
	spec.PermissionMode = "bypassPermissions"

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert.
	asRefusal(t, err, ArmUngatedWithoutConsent)
}

func TestCreateAcceptsAnUngatedModeWithConsent(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	spec := standardSpec(t, f)
	spec.PermissionMode = "bypassPermissions"
	spec.ConsentedUngatedMode = "bypassPermissions"

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert.
	if err != nil {
		t.Fatalf("Create: %v", err)
	}
}

// TestCreateRefusesAPromptlessOneShotWithNoSlug pins the ONE site `no_slug`
// still has now that word truncation is deleted: a one-shot with nothing to
// run, which is an argument-validation failure and not a naming failure.
func TestCreateRefusesAPromptlessOneShotWithNoSlug(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	oneShotBriefs(f)
	spec := standardSpec(t, f)
	spec.InitialPrompt = ""
	spec.OneShot = true

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert.
	asRefusal(t, err, ArmNoSlug)
}

func TestCreateSubmitsTheInitialPromptWithTheCreationOrigin(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	if _, err := f.verbs.Create(context.Background(), standardSpec(t, f)); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if len(f.queue.submissions) != 1 {
		t.Fatalf("submissions = %d, want exactly one", len(f.queue.submissions))
	}
	if f.queue.submissions[0].Origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_CREATED {
		t.Fatalf("origin = %v, want WORKSPACE_CREATED", f.queue.submissions[0].Origin)
	}
}

func TestCreateSubmitsTheInitialPromptOnlyAfterTheSessionIsUp(t *testing.T) {
	// Arrange: the session refuses to come up.
	f := newFixture(t)
	f.fleet.startErr = errors.New("the shim died during bring-up")

	// Act.
	_, err := f.verbs.Create(context.Background(), standardSpec(t, f))

	// Assert.
	if err == nil {
		t.Fatal("Create() = nil error, want the bring-up failure surfaced")
	}
	if len(f.queue.submissions) != 0 {
		t.Fatalf("submissions = %d, want none before the session is up", len(f.queue.submissions))
	}
}

func TestCreateWithoutAPromptSubmitsNothing(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	spec := CreateSpec{RepoDir: mainWorktree(t, f), Name: "promptless"}

	// Act.
	if _, err := f.verbs.Create(context.Background(), spec); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if len(f.queue.submissions) != 0 {
		t.Fatalf("submissions = %d, want none", len(f.queue.submissions))
	}
}

func TestCreateDecoratesTheOneShotPrompt(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	oneShotBriefs(f)
	spec := standardSpec(t, f)
	spec.OneShot = true

	// Act.
	if _, err := f.verbs.Create(context.Background(), spec); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert: preamble, the user's own words, then the framing sentence and
	// the repository's directive, in that order and verbatim.
	sent := f.queue.submissions[0].Said.GetContent().GetBlocks()[0].GetText().GetText()
	want := prompts.Wrap("PREAMBLE\n") + "fix the login bug" +
		prompts.Wrap("\n"+completionDirectiveLead+"DIRECTIVE")
	if sent != want {
		t.Fatalf("the one-shot prompt = %q, want %q", sent, want)
	}
}

func TestCreateRefusesAOneShotWhoseDirectiveDeclaresAPlaceholder(t *testing.T) {
	// Arrange: the directive is plain English and the daemon fills nothing in,
	// so a declared placeholder has no value and the composition fails.
	f := newFixture(t)
	oneShotBriefs(f)
	f.briefs[BriefOneShotCompletionDirective] = prompts.Prompt{
		Name: BriefOneShotCompletionDirective, Body: "merge into {{target}}.",
		Placeholders: []string{"target"},
	}
	spec := standardSpec(t, f)
	spec.OneShot = true

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert.
	asRefusal(t, err, ArmBriefMissing)
}

func TestCreateRefusesAOneShotWhenACorpusBriefIsMissing(t *testing.T) {
	// Arrange: the daemon's OWN repository, whose policy is the corpus. A
	// brief is never defaulted, so its absence from the corpus refuses the
	// creation at decoration.
	f := newFixture(t)
	spec := standardSpec(t, f)
	f.ownRepository(t, spec.RepoDir)
	spec.OneShot = true

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert.
	asRefusal(t, err, ArmBriefMissing)
}

func TestCreateRefusesAOneShotInARepositoryThatStatesNoPolicy(t *testing.T) {
	// Arrange: an ordinary repository, whose policy is its own tree and which
	// holds none of it. The corpus is not a fallback for it.
	f := newFixture(t)
	oneShotBriefs(f)
	delete(f.briefs, BriefAutonomousPreamble)
	delete(f.briefs, BriefOneShotCompletionDirective)
	spec := standardSpec(t, f)
	spec.OneShot = true

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert.
	refusal := asRefusal(t, err, ArmOneShotPolicyMissing)
	want := []string{BriefAutonomousPreamble + prompts.Suffix, BriefOneShotCompletionDirective + prompts.Suffix}
	got, _ := refusal.Fields["missing_files"].([]string)
	if strings.Join(got, ",") != strings.Join(want, ",") {
		t.Fatalf("missing_files = %v, want %v", got, want)
	}
}

// TestCreateRefusesAOneShotWithNoPolicyBeforeSpendingANamingCall pins the
// ORDERING of the two checks that both sit at the front of `Create': the
// one-shot policy requirement runs BEFORE the naming call, so a create that is
// going to be refused for a missing policy never pays for a model call. The
// spec supplies no name, which is exactly the shape that would otherwise name
// itself through `branchFor'.
func TestCreateRefusesAOneShotWithNoPolicyBeforeSpendingANamingCall(t *testing.T) {
	// Arrange: a repository that states no policy, and a nameless one-shot.
	f := newFixture(t)
	spec := standardSpec(t, f)
	spec.Name = ""
	spec.OneShot = true

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert.
	asRefusal(t, err, ArmOneShotPolicyMissing)
	if len(f.headless.calls) != 0 {
		t.Fatalf("headless calls = %v, want none: the policy refusal precedes the naming call", f.headless.calls)
	}
}

func TestCreateNamesTheRepositoryPolicyDirectoryInTheRefusal(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	spec := standardSpec(t, f)
	spec.OneShot = true

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert: the arm carries the directory the user must write, not only a
	// sentence about it.
	refusal := asRefusal(t, err, ArmOneShotPolicyMissing)
	if got := refusal.Fields["policy_dir"]; got != prompts.PolicyDir(spec.RepoDir) {
		t.Fatalf("policy_dir = %v, want %v", got, prompts.PolicyDir(spec.RepoDir))
	}
	if got := refusal.Fields["repository_root"]; got != spec.RepoDir {
		t.Fatalf("repository_root = %v, want %v", got, spec.RepoDir)
	}
}

func TestCreateMintsNothingWhenTheRepositoryStatesNoPolicy(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	spec := standardSpec(t, f)
	spec.OneShot = true

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert: no creation job, no worktree, no registration.
	if err == nil {
		t.Fatal("Create = nil error, want the policy refusal")
	}
	if len(f.db.jobs) != 0 {
		t.Fatalf("recorded creation jobs = %v, want none", f.db.jobs)
	}
	if len(f.git.created) != 0 {
		t.Fatalf("created worktrees = %v, want none", f.git.created)
	}
}

func TestCreateOfAOneShotInTheDaemonsOwnRepositoryIsNeverPolicyRefused(t *testing.T) {
	// Arrange: the corpus IS this repository's policy, and the fixture's
	// corpus holds every brief.
	f := newFixture(t)
	oneShotBriefs(f)
	spec := standardSpec(t, f)
	f.ownRepository(t, spec.RepoDir)
	spec.OneShot = true

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert.
	if err != nil {
		t.Fatalf("Create in the daemon's own repository = %v, want a success", err)
	}
}

func TestCreateRecordsTheOneShotFormInTheCreationJob(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	oneShotBriefs(f)
	spec := standardSpec(t, f)
	spec.OneShot = true

	// Act.
	if _, err := f.verbs.Create(context.Background(), spec); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if !f.db.putJobs[0].OneShot {
		t.Fatal("the creation job does not record the one-shot form")
	}
}

func TestCreateForksTheParentTranscriptBeforeTheSessionStarts(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	parent := f.workspace("parent", t.TempDir())
	f.db.sessions[parent.ID] = wsm.Session{Workspace: parent.ID, VendorSessionID: "vendor-1"}
	f.account.transcript = account.Transcript{Path: "/transcripts/vendor-1.jsonl", ConfigDir: "/roots/default"}
	spec := standardSpec(t, f)
	id := parent.ID
	spec.ForkFrom = &id

	// Act.
	if _, err := f.verbs.Create(context.Background(), spec); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if len(f.account.ported) != 1 || f.account.ported[0].Path != "/transcripts/vendor-1.jsonl" {
		t.Fatalf("ported transcripts = %+v, want the parent's ported once", f.account.ported)
	}
}

// TestCreateForkRecordsAFreshConversationForResume covers the fork's identity
// rule: a vendor session id is single-occupancy under the shim's session lock,
// so the child is recorded against an id of its OWN -- never the parent's,
// which its live shim still holds -- and the ported copy is filed under that
// same id.
func TestCreateForkRecordsAFreshConversationForResume(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	parent := f.workspace("parent", t.TempDir())
	f.db.sessions[parent.ID] = wsm.Session{Workspace: parent.ID, VendorSessionID: "vendor-1"}
	f.account.transcript = account.Transcript{Path: "/transcripts/vendor-1.jsonl", ConfigDir: "/roots/default"}
	spec := standardSpec(t, f)
	id := parent.ID
	spec.ForkFrom = &id

	// Act.
	created, err := f.verbs.Create(context.Background(), spec)
	if err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert: a fork RESUMES a conversation, under a fresh id of its own.
	recorded := f.db.sessions[created.ID].VendorSessionID
	if recorded == "" || recorded == "vendor-1" {
		t.Fatalf("child session = %+v, want a fresh vendor session id, never the parent's", f.db.sessions[created.ID])
	}
	if len(f.account.ported) != 1 || f.account.ported[0].VendorSessionID != recorded {
		t.Fatalf("ported transcripts = %+v, want the copy filed under the child's own id %q", f.account.ported, recorded)
	}
}

func TestCreateRefusesAForkOfAParentWithNoConversation(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	parent := f.workspace("parent", t.TempDir())
	spec := standardSpec(t, f)
	id := parent.ID
	spec.ForkFrom = &id

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert.
	asRefusal(t, err, ArmForkParentHasNoConversation)
}

func TestCreateRecordsThePriority(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	spec := standardSpec(t, f)
	priority := wsm.PriorityP1
	spec.Priority = &priority

	// Act.
	created, err := f.verbs.Create(context.Background(), spec)
	if err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	got := f.db.priorities[created.ID]
	if got == nil || *got != wsm.PriorityP1 {
		t.Fatalf("recorded priority = %v, want P1", got)
	}
}

func TestCreateRecordsTheModelAndModeAsSpawnFacts(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	spec := standardSpec(t, f)
	spec.Model = "opus"
	spec.PermissionMode = "plan"

	// Act.
	created, err := f.verbs.Create(context.Background(), spec)
	if err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	session := f.db.sessions[created.ID]
	if session.Model != "opus" || session.PermissionMode != "plan" || session.ConfigDir != "/config" {
		t.Fatalf("spawn facts = %+v, want the model, mode and routed config dir", session)
	}
}

// THE SPAWN-FACTS ROW IS A SESSION RECORD the host view is composed from the
// instant it exists, so it names its identity from the instant it exists.
func TestCreateMintsTheHostSessionIdentityWithTheSpawnFacts(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	spec := standardSpec(t, f)

	// Act.
	created, err := f.verbs.Create(context.Background(), spec)
	if err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if got := f.db.sessions[created.ID].HostSessionID; got == "" {
		t.Fatalf("the created workspace's session record carries no host session id")
	}
}

func TestSaidTextComposesOneTextBlock(t *testing.T) {
	// Arrange. Act.
	said := SaidText("hello")

	// Assert.
	blocks := said.GetContent().GetBlocks()
	if len(blocks) != 1 || blocks[0].GetText().GetText() != "hello" {
		t.Fatalf("SaidText() = %v, want one text block", said)
	}
}

func TestMergeTargetDirRefusesAnUnknownParent(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	spec := standardSpec(t, f)
	unknown := ids.WorkspaceID("nope")
	spec.ForkFrom = &unknown

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert.
	if err == nil {
		t.Fatal("Create(unknown parent) = nil error, want the lookup failure surfaced")
	}
}

func TestCreateAcceptsAutoWithoutConsent(t *testing.T) {
	// Arrange: `auto` KEEPS a gate — a classifier decides each ask instead of
	// the user — so it is not an ungated mode and needs no creation consent.
	f := newFixture(t)
	spec := standardSpec(t, f)
	spec.PermissionMode = "auto"

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert.
	if err != nil {
		t.Fatalf("Create(auto): %v", err)
	}
}

func TestCreateWithNeitherANameNorAPromptNamesTheBranchAfterTheWorkspaceID(t *testing.T) {
	// Arrange: the empty standard form, which the create contract calls an
	// empty workspace rather than a refusal.
	f := newFixture(t)
	t.Setenv(PrefixEnv, "ABC")
	t.Setenv(LegacyPrefixEnv, "")
	spec := standardSpec(t, f)
	spec.InitialPrompt = ""

	// Act.
	if _, err := f.verbs.Create(context.Background(), spec); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if len(f.git.created) != 1 {
		t.Fatalf("created worktrees = %+v, want exactly one", f.git.created)
	}
	branch := f.git.created[0].Branch
	if !strings.HasPrefix(branch, "ABC/"+UnnamedSlugPrefix) {
		t.Fatalf("branch = %q, want it named after the minted workspace id", branch)
	}
}

func TestCreateWithAnUnresolvableBaseRefIsRefused(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.git.resolveErr = errors.New("fatal: invalid reference: does-not-exist")
	spec := standardSpec(t, f)
	spec.BaseRef = "does-not-exist"

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert.
	asRefusal(t, err, ArmBaseRefUnresolved)
	if len(f.git.created) != 0 {
		t.Fatalf("created worktrees = %+v, want none for an unresolvable base ref", f.git.created)
	}
}

func TestCreateBaseRefRefusalNamesTheRefAsTheArmsField(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.git.resolveErr = errors.New("fatal: invalid reference: does-not-exist")
	spec := standardSpec(t, f)
	spec.BaseRef = "does-not-exist"

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert.
	refusal, ok := AsRefusal(err)
	if !ok {
		t.Fatalf("Create = %v, want the base_ref_unresolved refusal", err)
	}
	if got := refusal.Fields["ref"]; got != "does-not-exist" {
		t.Fatalf("the arm's ref field = %v, want the base ref that did not resolve", got)
	}
}

// TestCreateRefusesARepositoryThatIsNotOnDisk pins the arm for a repository
// whose directory is gone. The registry may still hold the row -- a worktree
// removed underneath it, a scratch repository a run cleaned up -- and the
// create then ran on to WorktreeDir's bare `stat <repo>/.git` failure: one
// ERROR from the verb and a second `the rpc failed` from the boundary, for a
// refusal the contract has an arm for.
func TestCreateRefusesARepositoryThatIsNotOnDisk(t *testing.T) {
	tests := []struct {
		name    string
		removed bool
		wantArm string
	}{
		{name: "the repository is on disk", removed: false},
		{name: "the repository directory was removed", removed: true, wantArm: ArmUnknownRepository},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			spec := standardSpec(t, f)
			if tt.removed {
				if err := os.RemoveAll(spec.RepoDir); err != nil {
					t.Fatalf("remove the repository: %v", err)
				}
			}

			// Act.
			_, err := f.verbs.Create(context.Background(), spec)

			// Assert.
			if tt.wantArm == "" {
				if err != nil {
					t.Fatalf("Create: %v", err)
				}
				return
			}
			asRefusal(t, err, tt.wantArm)
		})
	}
}

// TestCreateRefusesARepositoryTheRegistryDoesNotHold pins the WRITE half of
// the repository invariant: a workspace whose repository is unregistered is an
// invariant violation (owner ruling, 2026-09-13), so the create that would
// mint one is refused at the verb rather than at one of the three ways in.
func TestCreateRefusesARepositoryTheRegistryDoesNotHold(t *testing.T) {
	tests := []struct {
		name       string
		registered bool
		wantArm    string
	}{
		{name: "the repository is registered", registered: true},
		{name: "the repository is not registered", registered: false, wantArm: ArmUnknownRepository},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			spec := standardSpec(t, f)
			if !tt.registered {
				f.db.repositories = nil
			}

			// Act.
			_, err := f.verbs.Create(context.Background(), spec)

			// Assert.
			if tt.wantArm == "" {
				if err != nil {
					t.Fatalf("Create: %v", err)
				}
				return
			}
			asRefusal(t, err, tt.wantArm)
		})
	}
}

// A refused create MATERIALIZES NOTHING. The refusal lands before the creation
// job is recorded and before git is touched, so an unregistered repository
// leaves no worktree and no half-built row behind.
func TestACreateRefusedForAnUnregisteredRepositoryBuildsNothing(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	spec := standardSpec(t, f)
	f.db.repositories = nil

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert.
	asRefusal(t, err, ArmUnknownRepository)
	if len(f.git.created) != 0 {
		t.Fatalf("created worktrees = %+v, want none", f.git.created)
	}
	if len(f.db.registered) != 0 {
		t.Fatalf("registered %d workspaces, want none", len(f.db.registered))
	}
}

// The registry read's own failure is SURFACED, never read as "not registered":
// a create refused on `unknown_repository' because the database was
// unreachable would tell the user their repository is gone.
func TestCreateSurfacesARegistryReadFailureRatherThanRefusing(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	spec := standardSpec(t, f)
	f.db.listRepositoriesErr = errors.New("the state database is unreachable")

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert.
	if err == nil {
		t.Fatal("Create() = nil error, want the registry read failure surfaced")
	}
	if _, ok := AsRefusal(err); ok {
		t.Fatalf("Create() = %v, want a plain failure rather than a refusal arm", err)
	}
}

func TestCreateMintsAutoWhenTheCreationNamesNoMode(t *testing.T) {
	// Arrange: owner ruling 2026-09-14 — the unstated mode is auto, and the
	// row says so rather than staying empty.
	f := newFixture(t)
	spec := standardSpec(t, f)
	spec.PermissionMode = ""

	// Act.
	created, err := f.verbs.Create(context.Background(), spec)
	if err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if got := f.db.sessions[created.ID].PermissionMode; got != "auto" {
		t.Fatalf("minted permission mode = %q, want auto", got)
	}
}

// recordingProgress records the stages a Create reports through its
// CreateProgress reporter, in order.
type recordingProgress struct{ stages []CreateStage }

func (r *recordingProgress) Stage(stage CreateStage) { r.stages = append(r.stages, stage) }

// TestCreateReportsItsStageSequencePerForm pins the stages each create form
// reports, at the real points and in order: DerivingName only when the daemon
// mints the name, CreatingWorktree always, and StartingSession for every create
// that gets past the worktree, with or without an initial prompt.
func TestCreateReportsItsStageSequencePerForm(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(t *testing.T) (*fixture, CreateSpec)
		want    []CreateStage
	}{
		{
			name: "unnamed with an initial prompt",
			arrange: func(t *testing.T) (*fixture, CreateSpec) {
				f := newFixture(t)
				return f, standardSpec(t, f)
			},
			want: []CreateStage{CreateStageDerivingName, CreateStageCreatingWorktree, CreateStageStartingSession},
		},
		{
			name: "named with an initial prompt",
			arrange: func(t *testing.T) (*fixture, CreateSpec) {
				f := newFixture(t)
				spec := standardSpec(t, f)
				spec.Name = "chosen-name"
				return f, spec
			},
			want: []CreateStage{CreateStageCreatingWorktree, CreateStageStartingSession},
		},
		{
			name: "named without an initial prompt",
			arrange: func(t *testing.T) (*fixture, CreateSpec) {
				f := newFixture(t)
				return f, CreateSpec{RepoDir: mainWorktree(t, f), Name: "promptless"}
			},
			want: []CreateStage{CreateStageCreatingWorktree, CreateStageStartingSession},
		},
		{
			name: "unnamed without an initial prompt",
			arrange: func(t *testing.T) (*fixture, CreateSpec) {
				f := newFixture(t)
				return f, CreateSpec{RepoDir: mainWorktree(t, f)}
			},
			want: []CreateStage{CreateStageCreatingWorktree, CreateStageStartingSession},
		},
		{
			name: "unnamed fork",
			arrange: func(t *testing.T) (*fixture, CreateSpec) {
				return namingForkFixture(t, "", "wire the iterm2 integration")
			},
			want: []CreateStage{CreateStageDerivingName, CreateStageCreatingWorktree, CreateStageStartingSession},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			t.Setenv(PrefixEnv, "ABC")
			f, spec := tc.arrange(t)
			rec := &recordingProgress{}
			spec.Progress = rec

			// Act.
			if _, err := f.verbs.Create(context.Background(), spec); err != nil {
				t.Fatalf("Create: %v", err)
			}

			// Assert.
			if !slices.Equal(rec.stages, tc.want) {
				t.Fatalf("stages = %v, want %v", rec.stages, tc.want)
			}
		})
	}
}

// TestCreateEndsItsStagesAtTheFailingStep pins that a create failing at a step
// reports every stage up to and including the one it failed in, and none after:
// the failure itself is the verb's returned error, which the caller maps to the
// terminal failed step.
func TestCreateEndsItsStagesAtTheFailingStep(t *testing.T) {
	cases := []struct {
		name    string
		arrange func(f *fixture)
		want    []CreateStage
	}{
		{
			name:    "the worktree cannot be materialized",
			arrange: func(f *fixture) { f.git.createErr = errors.New("branch already checked out") },
			want:    []CreateStage{CreateStageCreatingWorktree},
		},
		{
			name:    "the session does not come up",
			arrange: func(f *fixture) { f.fleet.startErr = errors.New("the shim died during bring-up") },
			want:    []CreateStage{CreateStageCreatingWorktree, CreateStageStartingSession},
		},
		{
			name:    "the initial prompt is not accepted by the queue",
			arrange: func(f *fixture) { f.queue.submitErr = errors.New("the queue refused the submission") },
			want:    []CreateStage{CreateStageCreatingWorktree, CreateStageStartingSession},
		},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			t.Setenv(PrefixEnv, "ABC")
			spec := standardSpec(t, f)
			spec.Name = "chosen-name"
			rec := &recordingProgress{}
			spec.Progress = rec
			tc.arrange(f)

			// Act.
			_, err := f.verbs.Create(context.Background(), spec)

			// Assert.
			if err == nil {
				t.Fatal("Create() = nil error, want the failure surfaced")
			}
			if !slices.Equal(rec.stages, tc.want) {
				t.Fatalf("stages = %v, want %v", rec.stages, tc.want)
			}
		})
	}
}

// ---- a fork is named from its prompt plus the conversation it continues ----

// namingForkFixture arranges a fork of a parent whose recorded conversation is
// SAID, with the fork's own prompt set to PROMPT.
func namingForkFixture(t *testing.T, prompt string, said ...string) (*fixture, CreateSpec) {
	t.Helper()
	rows := make([]wsm.PortedPrompt, 0, len(said))
	for i, text := range said {
		rows = append(rows, wsm.PortedPrompt{
			Turn: ids.TurnID("turn-" + string(rune('a'+i))), Ordinal: int64(i), Text: text, Origin: "webapp",
		})
	}
	f, _, spec := forkFixture(t, rows)
	t.Setenv(PrefixEnv, "")
	t.Setenv(LegacyPrefixEnv, "")
	spec.InitialPrompt = prompt
	return f, spec
}

func TestCreateNamesABlankPromptForkByTheModel(t *testing.T) {
	// Arrange: the incident's shape — a fork with no prompt of its own.
	f, spec := namingForkFixture(t, "", "wire the iterm2 integration")

	// Act.
	if _, err := f.verbs.Create(context.Background(), spec); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert: the model named it, never the minted-id rule.
	if f.git.created[0].Branch != FixtureMintedName {
		t.Fatalf("branch = %q, want the model's name %q", f.git.created[0].Branch, FixtureMintedName)
	}
}

func TestCreateHandsAForksNamingCallTheParentConversation(t *testing.T) {
	// Arrange.
	f, spec := namingForkFixture(t, "", "wire the iterm2 integration")

	// Act.
	if _, err := f.verbs.Create(context.Background(), spec); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if len(f.headless.calls) != 1 || !strings.Contains(f.headless.calls[0].Prompt, "wire the iterm2 integration") {
		t.Fatalf("naming calls = %+v, want one carrying the parent's conversation", f.headless.calls)
	}
}

func TestCreateHandsAForksNamingCallItsOwnPrompt(t *testing.T) {
	// Arrange.
	f, spec := namingForkFixture(t, "now port it to kitty", "wire the iterm2 integration")

	// Act.
	if _, err := f.verbs.Create(context.Background(), spec); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if !strings.Contains(f.headless.calls[0].Prompt, "now port it to kitty") {
		t.Fatalf("naming prompt = %q, want the fork's own prompt in it", f.headless.calls[0].Prompt)
	}
}

func TestCreateRefusesAForkWhoseNamingAnswerIsNotAName(t *testing.T) {
	// Arrange: the incident's answer, twice — a sentence, not a name.
	f, spec := namingForkFixture(t, "", "wire the iterm2 integration")
	script(f,
		headlessAnswer{text: "Describe the work you want to do in the workspace."},
		headlessAnswer{text: "Describe the work you want to do in the workspace."})

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert: a failure, never a generic fallback name.
	asRefusal(t, err, ArmNamingFailed)
}

func TestCreateRefusesAForkOfAConversationlessParentBeforeTheNamingCall(t *testing.T) {
	// Arrange: a parent with no session at all.
	f := newFixture(t)
	parent := f.workspace("parent", t.TempDir())
	spec := standardSpec(t, f)
	id := parent.ID
	spec.ForkFrom = &id

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert.
	asRefusal(t, err, ArmForkParentHasNoConversation)
	if len(f.headless.calls) != 0 {
		t.Fatalf("naming calls = %+v, want none paid for a fork that cannot happen", f.headless.calls)
	}
}

func TestCreateNamesABlankForkOfAParentWithNoRecordedRequestAfterItsId(t *testing.T) {
	// Arrange: nothing to name from — no prompt and no recorded request.
	f, spec := namingForkFixture(t, "")

	// Act.
	created, err := f.verbs.Create(context.Background(), spec)
	if err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if !strings.HasPrefix(created.Name, UnnamedSlugPrefix) || len(f.headless.calls) != 0 {
		t.Fatalf("name = %q, calls = %d, want the minted-id rule and no naming call", created.Name, len(f.headless.calls))
	}
}

func TestCreateFailsAForkWhoseParentConversationCannotBeRead(t *testing.T) {
	// Arrange.
	f, spec := namingForkFixture(t, "", "wire the iterm2 integration")
	f.db.conversationErr = errors.New("disk on fire")

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "disk on fire") {
		t.Fatalf("Create = %v, want the conversation read failure surfaced", err)
	}
}

func TestCreateHandsAPlainCreatesNamingCallNoConversation(t *testing.T) {
	// Arrange: the fixture brief brackets the conversation placeholder with
	// spaces, so an empty conversation splices to two adjacent spaces.
	f := newFixture(t)
	spec := standardSpec(t, f)
	spec.InitialPrompt = "fix the login bug"

	// Act.
	if _, err := f.verbs.Create(context.Background(), spec); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if !strings.Contains(f.headless.calls[0].Prompt, "fix the login bug  ") {
		t.Fatalf("naming prompt = %q, want an empty conversation", f.headless.calls[0].Prompt)
	}
}
