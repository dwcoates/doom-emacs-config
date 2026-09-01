package workspace

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/ids"
	"claude-repld/internal/prompts"
	"claude-repld/internal/wsm"
)

// mainWorktree makes a directory that looks like a repository's MAIN worktree,
// which is what the worktree-directory rule branches on.
func mainWorktree(t *testing.T) string {
	t.Helper()
	dir := t.TempDir()
	if err := os.MkdirAll(filepath.Join(dir, ".git"), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}
	normalized, err := normalizeDir(dir)
	if err != nil {
		t.Fatalf("normalizeDir: %v", err)
	}
	return normalized
}

// standardSpec is the ordinary creation form against a real main worktree.
func standardSpec(t *testing.T) CreateSpec {
	t.Helper()
	return CreateSpec{RepoDir: mainWorktree(t), InitialPrompt: "fix the login bug"}
}

// oneShotBriefs arranges the three briefs the one-shot decoration reads.
func oneShotBriefs(f *fixture) {
	f.briefs[BriefAutonomousPreamble] = prompts.Prompt{Name: BriefAutonomousPreamble, Body: "PREAMBLE\n"}
	f.briefs[BriefOneShotSuccessSuffix] = prompts.Prompt{
		Name: BriefOneShotSuccessSuffix, Body: "invoke {{invocation}} to {{action_phrase}}.",
		Placeholders: []string{"invocation", "action_phrase"},
	}
	f.briefs[BriefOneShotCreatePrFollowup] = prompts.Prompt{
		Name: BriefOneShotCreatePrFollowup, Body: "after {{create_pr_command}} run {{wrapup_command}}.",
		Placeholders: []string{"create_pr_command", "wrapup_command"},
	}
}

func TestCreateDerivesTheBranchFromTheInitialPrompt(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	t.Setenv(PrefixEnv, "DWC")
	t.Setenv(LegacyPrefixEnv, "")

	// Act.
	if _, err := f.verbs.Create(context.Background(), standardSpec(t)); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if len(f.git.created) != 1 || f.git.created[0].Branch != "DWC/fix-the-login" {
		t.Fatalf("created worktrees = %+v, want branch DWC/fix-the-login", f.git.created)
	}
}

func TestCreatePutsTheWorktreeInTheSiblingWorktreesDirectory(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	t.Setenv(PrefixEnv, "DWC")
	spec := standardSpec(t)

	// Act.
	if _, err := f.verbs.Create(context.Background(), spec); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	want := filepath.Join(filepath.Dir(spec.RepoDir), filepath.Base(spec.RepoDir)+WorktreeDirSuffix, "fix-the-login")
	if f.git.created[0].WorktreeDir != want {
		t.Fatalf("worktree dir = %q, want %q", f.git.created[0].WorktreeDir, want)
	}
}

func TestCreateUsesTheSuppliedName(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	t.Setenv(PrefixEnv, "DWC")
	spec := standardSpec(t)
	spec.Name = "chosen-name"

	// Act.
	if _, err := f.verbs.Create(context.Background(), spec); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	if f.git.created[0].Branch != "DWC/chosen-name" {
		t.Fatalf("branch = %q, want DWC/chosen-name", f.git.created[0].Branch)
	}
}

func TestCreateKeepsASuppliedNameThatAlreadyCarriesAPrefix(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	t.Setenv(PrefixEnv, "DWC")
	spec := standardSpec(t)
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
	if _, err := f.verbs.Create(context.Background(), standardSpec(t)); err != nil {
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
	spec := standardSpec(t)
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
	if _, err := f.verbs.Create(context.Background(), standardSpec(t)); err != nil {
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
	spec := standardSpec(t)

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
	spec := standardSpec(t)
	id := parent.ID
	spec.ForkFrom = &id
	f.db.sessions[parent.ID] = wsm.Session{Workspace: parent.ID, VendorSessionID: "vendor-1"}
	f.account.transcript = "/transcripts/vendor-1.jsonl"

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
	spec := standardSpec(t)
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
	record, err := f.verbs.Create(context.Background(), standardSpec(t))
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
	_, err := f.verbs.Create(context.Background(), standardSpec(t))

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
	spec := standardSpec(t)
	spec.PermissionMode = "bypassPermissions"

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert.
	asRefusal(t, err, ArmUngatedWithoutConsent)
}

func TestCreateAcceptsAnUngatedModeWithConsent(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	spec := standardSpec(t)
	spec.PermissionMode = "bypassPermissions"
	spec.ConsentedUngatedMode = "bypassPermissions"

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert.
	if err != nil {
		t.Fatalf("Create: %v", err)
	}
}

func TestCreateRefusesAOneShotWithNoFinishAction(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	spec := standardSpec(t)
	spec.OneShot = true

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert.
	asRefusal(t, err, ArmFinishRequired)
}

func TestCreateRefusesAFinishActionOnTheStandardForm(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	spec := standardSpec(t)
	spec.Finish = &OneShotFinish{SelfMerge: true}

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert.
	asRefusal(t, err, ArmFinishNotOneShot)
}

func TestCreateRefusesWhenNoSlugCanBeDerived(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	spec := standardSpec(t)
	spec.InitialPrompt = "!!! ???"

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert.
	asRefusal(t, err, ArmNoSlug)
}

func TestCreateSubmitsTheInitialPromptWithTheCreationOrigin(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	if _, err := f.verbs.Create(context.Background(), standardSpec(t)); err != nil {
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
	_, err := f.verbs.Create(context.Background(), standardSpec(t))

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
	spec := CreateSpec{RepoDir: mainWorktree(t), Name: "promptless"}

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
	spec := standardSpec(t)
	spec.OneShot = true
	spec.Finish = &OneShotFinish{SelfMerge: true}

	// Act.
	if _, err := f.verbs.Create(context.Background(), spec); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert.
	sent := f.queue.submissions[0].Said.GetContent().GetBlocks()[0].GetText().GetText()
	if !strings.Contains(sent, "PREAMBLE") || !strings.Contains(sent, "fix the login bug") ||
		!strings.Contains(sent, selfMergeActionPhrase) {
		t.Fatalf("the one-shot prompt = %q, want preamble, prompt and wrap-up", sent)
	}
}

func TestCreateRefusesAOneShotWhenABriefIsMissing(t *testing.T) {
	// Arrange: a brief is never defaulted, so its absence refuses the creation.
	f := newFixture(t)
	spec := standardSpec(t)
	spec.OneShot = true
	spec.Finish = &OneShotFinish{SelfMerge: true}

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert.
	asRefusal(t, err, ArmBriefMissing)
}

func TestCreateRecordsTheOneShotFinishInTheCreationJob(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	oneShotBriefs(f)
	spec := standardSpec(t)
	spec.OneShot = true
	spec.Finish = &OneShotFinish{SelfMerge: true}

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
	f.account.transcript = "/transcripts/vendor-1.jsonl"
	spec := standardSpec(t)
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

func TestCreateForkRecordsTheParentConversationForResume(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	parent := f.workspace("parent", t.TempDir())
	f.db.sessions[parent.ID] = wsm.Session{Workspace: parent.ID, VendorSessionID: "vendor-1"}
	f.account.transcript = "/transcripts/vendor-1.jsonl"
	spec := standardSpec(t)
	id := parent.ID
	spec.ForkFrom = &id

	// Act.
	created, err := f.verbs.Create(context.Background(), spec)
	if err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert: a fork RESUMES; it never starts a fresh conversation.
	if f.db.sessions[created.ID].VendorSessionID != "vendor-1" {
		t.Fatalf("child session = %+v, want the parent conversation recorded", f.db.sessions[created.ID])
	}
}

func TestCreateRefusesAForkOfAParentWithNoConversation(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	parent := f.workspace("parent", t.TempDir())
	spec := standardSpec(t)
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
	spec := standardSpec(t)
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
	spec := standardSpec(t)
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
	spec := standardSpec(t)
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
	spec := standardSpec(t)
	spec.PermissionMode = "auto"

	// Act.
	_, err := f.verbs.Create(context.Background(), spec)

	// Assert.
	if err != nil {
		t.Fatalf("Create(auto): %v", err)
	}
}

func TestCreateRecordsTheOneShotFinishAction(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	oneShotBriefs(f)
	spec := standardSpec(t)
	spec.OneShot = true
	spec.Finish = &OneShotFinish{OpenPr: &OneShotOpenPr{AddToMergeQueue: true}}

	// Act.
	if _, err := f.verbs.Create(context.Background(), spec); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert: the action is durable before the worktree exists, because the
	// turn that takes it may run after a restart.
	if f.db.putJobs[0].Finish != "open_pr+add_to_merge_queue" {
		t.Fatalf("recorded finish = %q, want open_pr+add_to_merge_queue", f.db.putJobs[0].Finish)
	}
}
