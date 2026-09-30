//go:build integration

package integration

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"claude-repld/integration/harness"

	"connectrpc.com/connect"
)

// ---- Standard create ----

func TestCreateWorkspaceStandardDerivesTheSlugCreatesTheBranchAndSubmitsTheInitialPromptWithWorkspaceCreatedOrigin(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	repository := createRepositoryRef(t, d, repo)

	// Act
	resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repository,
		Form: &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{
			InitialPrompt: said("fix the flaky reconnect test"),
		}},
	}))

	// Assert: minted ref, worktree and branch created off the default branch.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace(standard) = (%v, %v), want a success", resp, err)
	}
	ws := resp.Msg.GetSuccess().GetWorkspace()
	if ws.GetId() == "" || ws.GetDir() == "" {
		t.Fatalf("CreateWorkspace success = %v, want a minted workspace ref", resp.Msg)
	}
	if !repo.HasWorktree(ws.GetDir()) {
		t.Fatalf("the repository's worktrees = %v, want the new worktree %q", repo.Worktrees(), ws.GetDir())
	}
	if got := len(repo.Branches()); got < 2 {
		t.Fatalf("the repository's branches = %v, want a new branch cut off %q for the workspace", repo.Branches(), harness.DefaultBranch)
	}

	// Assert: a slug was derived.
	host := d.WatchHost(ws)
	naming := harness.AwaitView(t, d.Ctx(), host, "a derived slug", func(r *agentreplv1.WatchHostWorkspaceResponse) bool {
		return r.GetHost().GetNaming().GetSlug() != ""
	}).GetHost().GetNaming()
	if naming.GetSlug() == "" {
		t.Fatalf("the new workspace's naming.slug = %q, want a slug derived from the initial prompt", naming.GetSlug())
	}

	// Assert: the initial prompt was submitted, WORKSPACE_CREATED-attributed.
	shim := d.Shim(ws)
	fresh := shim.ExpectStartSession()
	if fresh.GetFresh() == nil {
		t.Fatalf("StartSession request = %v, want fresh for a brand-new workspace", fresh)
	}
	turn := shim.ExpectStartTurn()
	if text(turn.GetSaid()) != "fix the flaky reconnect test" {
		t.Fatalf("StartTurn.said = %q, want the initial prompt verbatim", text(turn.GetSaid()))
	}
	if turn.GetOrigin() != conversationv1.PromptOrigin_PROMPT_ORIGIN_WORKSPACE_CREATED {
		t.Fatalf("StartTurn.origin = %v, want WORKSPACE_CREATED", turn.GetOrigin())
	}
}

func TestCreateWorkspaceHonorsAnExplicitBaseRef(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	repository := createRepositoryRef(t, d, repo)
	repo.Branch("release")
	repo.Checkout(harness.DefaultBranch)
	baseRef := "release"

	// Act
	resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repository,
		Form: &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{
			BaseRef: &baseRef,
		}},
	}))

	// Assert: the worktree was cut from the named base ref, not the default.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace(base_ref=%q) = (%v, %v), want a success", baseRef, resp, err)
	}
	found := false
	for _, c := range d.Git.Calls() {
		if createArgsContainAll(c.Args, "worktree", "add") && createArgsContain(c.Args, baseRef) {
			found = true
		}
	}
	if !found {
		t.Fatalf("git calls = %v, want a `worktree add` naming the base ref %q", d.Git.Calls(), baseRef)
	}
}

func TestCreateWorkspaceWithABadBaseRefIsRefused(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of the bad base ref the test stages.
	d.ExpectWarnings("daemon.gitclient.resolve_ref")
	repo := harness.NewRepo(t)
	repository := createRepositoryRef(t, d, repo)
	bad := "does-not-exist"

	// Act
	resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repository,
		Form: &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{
			BaseRef: &bad,
		}},
	}))

	// Assert
	if err != nil {
		t.Fatalf("CreateWorkspace(bad base_ref) = error %v, want a typed base_ref_unresolved answer", err)
	}
	unresolved := resp.Msg.GetError().GetBaseRefUnresolved()
	if unresolved == nil || unresolved.GetRef() != bad {
		t.Fatalf("CreateWorkspace(bad base_ref) = %v, want error.base_ref_unresolved naming %q", resp.Msg, bad)
	}
}

func TestCreateWorkspaceWithASuppliedNameWinsOverTheDerivedSlug(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	repository := createRepositoryRef(t, d, repo)
	name := "my-explicit-name"

	// Act
	resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repository,
		Form: &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{
			InitialPrompt: said("something entirely different"),
			Name:          &name,
		}},
	}))

	// Assert: the supplied name, not a prompt-derived slug, names the worktree.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace(name=%q) = (%v, %v), want a success", name, resp, err)
	}
	ws := resp.Msg.GetSuccess().GetWorkspace()
	if !strings.Contains(ws.GetDir(), name) {
		t.Fatalf("the new workspace's dir = %q, want the supplied name %q to win over the derived slug", ws.GetDir(), name)
	}
}

func TestCreateWorkspaceWithAParentNestsTheChildAndTargetsTheParentsWorktreeOnMerge(t *testing.T) {
	t.Parallel()
	// Arrange: a parent, top-level workspace.
	//
	// The daemon's own checkout IS this repository and a test-all script
	// stands in for the gate, because the emacs-repo method is the one that
	// runs `git merge` at all: in every other repository the landing belongs
	// to the merge prompts, and no git merge is issued for this assertion to
	// find.
	repo := harness.NewRepo(t)
	script := harness.NewTestAllScript(t, repo.Dir)
	script.SetExitCode(0)
	script.SetStdout("daemon: passed in 1s\n")
	d := newDaemon(t, harness.Opts{
		SelfRepo: repo.Dir,
		ExtraEnv: []string{"AGENT_REPL_TEST_ALL_SCRIPT=" + script.Path},
	})
	repository := createRepositoryRef(t, d, repo)
	parentResp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repository,
		Form:       &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{}},
	}))
	if err != nil || parentResp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace(parent) = (%v, %v), want a success", parentResp, err)
	}
	parent := parentResp.Msg.GetSuccess().GetWorkspace()
	roster := d.WatchRoster()

	// Act: a child spawned from it.
	childResp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repository,
		Form:       &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{}},
		Parent:     &agentreplv1.CreateWorkspaceParent{Workspace: parent},
	}))
	if err != nil || childResp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace(parent=%s) = (%v, %v), want a success", parent.GetId(), childResp, err)
	}
	child := childResp.Msg.GetSuccess().GetWorkspace()

	// Assert: the roster nests the child under the parent.
	awaitRoster(t, d, roster, "the child nested under its parent", func(r *frontendv1.WorkspaceRoster) bool {
		parentRow := rosterRepoRow(r, parent.GetId())
		if parentRow == nil {
			return false
		}
		for _, c := range parentRow.GetChildren() {
			if c.GetWorkspace().GetWorkspace().GetId() == child.GetId() {
				return true
			}
		}
		return false
	})

	// Assert: the child's merge lands into the parent's worktree, not main.
	harness.CommitWork(t, child.GetDir())
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: child})); err != nil {
		t.Fatalf("MergeWorkspace(child) = error %v, want the merge enqueued", err)
	}
	// The merge has to have REACHED ITS TERMINAL before the git calls say
	// anything: `merge_queued` is published before the run starts, so waiting
	// on it would read the trace of a merge that has not run yet.
	awaitRoster(t, d, roster, "the child's merge landed", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, child.GetId())
		return row != nil && row.GetMerged() != nil
	})
	// The git client selects its directory with `-C <dir>` and never with the
	// child process's cwd, so "inside the parent's worktree" is read off the
	// selection argument.
	targetedParent := false
	for _, c := range d.Git.Calls() {
		if createArgsContain(c.Args, "merge") && createGitDir(c.Args) == parent.GetDir() {
			targetedParent = true
		}
	}
	if !targetedParent {
		t.Fatalf("git calls = %v, want a merge run inside the parent's worktree %q", d.Git.Calls(), parent.GetDir())
	}
	// A clean landing into the parent's worktree (a passing test-all gate, no
	// conflict) reaches none of internal/merge's WARN sites, and the parent's
	// worktree is not this daemon's own checkout, so selfReload never fires
	// either.
}

func TestCreateWorkspaceForkPortsTheParentsTranscriptAndResumesIt(t *testing.T) {
	t.Parallel()
	// Arrange: a parent with a minted vendor session and a fake transcript on
	// disk under its config root's project dir, exactly what a real vendor
	// session would have left behind.
	f := newOpened(t, harness.Opts{})
	req := f.shim.ExpectStartSession()
	if req.GetFresh() == nil {
		t.Fatalf("the parent's StartSession = %v, want fresh", req)
	}
	vendorID := f.shim.Info().VendorSessionID
	if vendorID == "" {
		t.Fatal("the fake shim reports no vendor session id after StartSession(fresh)")
	}
	parentProject := createProjectDir(f.d.DefaultConfigDir, f.ws.GetDir())
	if err := os.MkdirAll(parentProject, 0o755); err != nil {
		t.Fatalf("mkdir the parent's project dir: %v", err)
	}
	transcript := filepath.Join(parentProject, vendorID+".jsonl")
	if err := os.WriteFile(transcript, []byte(`{"type":"summary"}`+"\n"), 0o644); err != nil {
		t.Fatalf("seed the parent's transcript: %v", err)
	}
	repository := createRepositoryRef(t, f.d, f.repo)

	// Act
	resp, err := f.d.Client().CreateWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repository,
		Form:       &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{}},
		Parent: &agentreplv1.CreateWorkspaceParent{
			Workspace: f.ws,
			Fork:      &agentreplv1.CreateWorkspaceFork{},
		},
	}))

	// Assert: the child resumes a conversation of its OWN. A vendor session id
	// is single-occupancy -- the shim takes session-<id>.lock inside
	// StartSession -- so a child resuming the live parent's id could never come
	// up; the daemon mints a fresh id and files the copy under it.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace(fork) = (%v, %v), want a success", resp, err)
	}
	child := resp.Msg.GetSuccess().GetWorkspace()
	childShim := f.d.Shim(child)
	resume := childShim.ExpectStartSession().GetResume()
	if resume == nil {
		t.Fatalf("the forked child's StartSession = %v, want a resume", resume)
	}
	forked := resume.GetVendorSessionId()
	if forked == "" || forked == vendorID {
		t.Fatalf("the forked child resumes %q, want a fresh vendor session id and never the parent's %q", forked, vendorID)
	}

	// Assert: the parent's conversation is copied under the child's project
	// dir, filed under the child's own id, and the parent keeps its own.
	childProject := createProjectDir(f.d.DefaultConfigDir, child.GetDir())
	ported := filepath.Join(childProject, forked+".jsonl")
	if _, err := os.Stat(ported); err != nil {
		t.Fatalf("stat the ported transcript %s: %v, want the parent's conversation copied under the child's own id", ported, err)
	}
	if _, err := os.Stat(transcript); err != nil {
		t.Fatalf("stat the parent's transcript %s: %v, want it left in place", transcript, err)
	}
}

// TestCreateWorkspaceForkRemintsEveryIdentityInTheChildsTranscript covers the
// fork's FILE PLANE. A fork mints a new AgentId, so the ported history must be
// the child's own conversation: the sidecar keys rows out of the records
// themselves (`activity:<tool_use_id>`, `terminal:<agent>:<record uuid>`), so a
// byte copy would re-key the parent's rows under the child's book, the store
// would refuse the batch ("would move the row from book A to book B"), and the
// child's transcript would be parked instead of read.
func TestCreateWorkspaceForkRemintsEveryIdentityInTheChildsTranscript(t *testing.T) {
	t.Parallel()
	// Arrange: a parent whose transcript carries the identity material the file
	// plane keys on -- record uuids linked across records, an assistant message
	// id, a tool_use settled by a tool_result -- plus the sidecar directory a
	// spawned subagent leaves beside it.
	f := newOpened(t, harness.Opts{})
	if req := f.shim.ExpectStartSession(); req.GetFresh() == nil {
		t.Fatalf("the parent's StartSession = %v, want fresh", req)
	}
	vendorID := f.shim.Info().VendorSessionID
	if vendorID == "" {
		t.Fatal("the fake shim reports no vendor session id after StartSession(fresh)")
	}
	parentProject := createProjectDir(f.d.DefaultConfigDir, f.ws.GetDir())
	if err := os.MkdirAll(parentProject, 0o755); err != nil {
		t.Fatalf("mkdir the parent's project dir: %v", err)
	}
	transcript := filepath.Join(parentProject, vendorID+".jsonl")
	parentBody := strings.Join([]string{
		`{"type":"user","uuid":"` + forkRecordOne + `","sessionId":"` + vendorID + `","message":{"role":"user","content":"go"}}`,
		`{"type":"assistant","uuid":"` + forkRecordTwo + `","parentUuid":"` + forkRecordOne + `","sessionId":"` + vendorID +
			`","message":{"id":"` + forkMessageID + `","content":[{"type":"tool_use","id":"` + forkToolUseID + `","name":"Agent","input":{"subagent_type":"Explore"}}]}}`,
		`{"type":"user","uuid":"` + forkRecordThree + `","parentUuid":"` + forkRecordTwo + `","sessionId":"` + vendorID +
			`","message":{"role":"user","content":[{"type":"tool_result","tool_use_id":"` + forkToolUseID + `","content":"done"}]}}`,
	}, "\n") + "\n"
	if err := os.WriteFile(transcript, []byte(parentBody), 0o644); err != nil {
		t.Fatalf("seed the parent's transcript: %v", err)
	}
	subagents := filepath.Join(parentProject, vendorID, "subagents")
	if err := os.MkdirAll(subagents, 0o755); err != nil {
		t.Fatalf("mkdir the parent's sidecar: %v", err)
	}
	if err := os.WriteFile(filepath.Join(subagents, "agent-"+forkLocator+".meta.json"),
		[]byte(`{"agentType":"Explore","description":"look","toolUseId":"`+forkToolUseID+`","spawnDepth":1}`), 0o644); err != nil {
		t.Fatalf("seed the subagent meta: %v", err)
	}
	if err := os.WriteFile(filepath.Join(subagents, "agent-"+forkLocator+".jsonl"),
		[]byte(`{"type":"user","uuid":"`+forkRecordFour+`","agentId":"`+forkLocator+`","isSidechain":true,"sessionId":"`+vendorID+`"}`+"\n"), 0o644); err != nil {
		t.Fatalf("seed the subagent transcript: %v", err)
	}
	repository := createRepositoryRef(t, f.d, f.repo)

	// Act
	resp, err := f.d.Client().CreateWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repository,
		Form:       &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{}},
		Parent: &agentreplv1.CreateWorkspaceParent{
			Workspace: f.ws,
			Fork:      &agentreplv1.CreateWorkspaceFork{},
		},
	}))

	// Assert: the child came up on a conversation of its own.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace(fork) = (%v, %v), want a success", resp, err)
	}
	child := resp.Msg.GetSuccess().GetWorkspace()
	forked := f.d.Shim(child).ExpectStartSession().GetResume().GetVendorSessionId()
	if forked == "" || forked == vendorID {
		t.Fatalf("the forked child resumes %q, want a fresh vendor session id and never the parent's %q", forked, vendorID)
	}

	// Assert: NOTHING the parent was identified by survives into the child's
	// files -- transcript or sidecar.
	childProject := createProjectDir(f.d.DefaultConfigDir, child.GetDir())
	childSidecar := filepath.Join(childProject, forked, "subagents")
	ported := forkReadFile(t, filepath.Join(childProject, forked+".jsonl"))
	locator := forkSubagentLocator(t, childSidecar)
	portedMeta := forkReadFile(t, filepath.Join(childSidecar, "agent-"+locator+".meta.json"))
	portedSubagent := forkReadFile(t, filepath.Join(childSidecar, "agent-"+locator+".jsonl"))
	whole := ported + portedMeta + portedSubagent
	for _, identity := range []string{
		vendorID, forkRecordOne, forkRecordTwo, forkRecordThree, forkRecordFour,
		forkMessageID, forkToolUseID, forkLocator,
	} {
		if strings.Contains(whole, identity) {
			t.Fatalf("the child's ported conversation still carries the parent's identity %q:\n%s", identity, whole)
		}
	}

	// Assert: the mapping was ONE mapping -- the links inside the history still
	// point at each other, and the sidecar agrees with the transcript about
	// which call spawned the subagent.
	// The child's own session appends to the file it resumes (the fake shim
	// lays down a session_started line, as the vendor CLI does), so the ported
	// HISTORY is the leading three records.
	lines := strings.Split(strings.TrimRight(ported, "\n"), "\n")
	if len(lines) < 3 {
		t.Fatalf("the ported transcript has %d lines, want at least the parent's 3:\n%s", len(lines), ported)
	}
	lines = lines[:3]
	if got, want := forkField(t, lines[1], "parentUuid"), forkField(t, lines[0], "uuid"); got != want {
		t.Fatalf("parentUuid = %q, want the re-minted uuid %q of the record it links to", got, want)
	}
	if got, want := forkField(t, lines[2], "parentUuid"), forkField(t, lines[1], "uuid"); got != want {
		t.Fatalf("parentUuid = %q, want the re-minted uuid %q of the record it links to", got, want)
	}
	if got, want := forkField(t, lines[0], "sessionId"), forked; got != want {
		t.Fatalf("sessionId = %q, want the child's own vendor session id %q", got, want)
	}
	call := forkBlockID(t, lines[1])
	if got := forkField(t, lines[2], "message", "content", "0", "tool_use_id"); got != call {
		t.Fatalf("the tool_result settles %q, want the re-minted call %q it was paired with", got, call)
	}
	if got := forkField(t, portedMeta, "toolUseId"); got != call {
		t.Fatalf("the subagent meta names %q as its spawning call, want the transcript's own %q", got, call)
	}
	if got := forkField(t, portedSubagent, "agentId"); got != locator {
		t.Fatalf("the subagent record states agentId %q, want its own file name's locator %q", got, locator)
	}
}

// The parent identities the fork test seeds, all of which the child's files must
// be free of.
const (
	forkRecordOne   = "11111111-1111-4111-8111-111111111111"
	forkRecordTwo   = "22222222-2222-4222-8222-222222222222"
	forkRecordThree = "33333333-3333-4333-8333-333333333333"
	forkRecordFour  = "44444444-4444-4444-8444-444444444444"
	forkMessageID   = "msg_01ForkParentMessage"
	forkToolUseID   = "toolu_01ForkParentCall"
	forkLocator     = "aef975b7bc3422d4b"
)

// forkReadFile reads one of the child's ported files.
func forkReadFile(t *testing.T, path string) string {
	t.Helper()
	raw, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("ReadFile(%s) = %v, want the ported file", path, err)
	}
	return string(raw)
}

// forkSubagentLocator answers the re-minted `agent-<id>` the child's sidecar
// filed its one subagent under.
func forkSubagentLocator(t *testing.T, dir string) string {
	t.Helper()
	entries, err := os.ReadDir(dir)
	if err != nil {
		t.Fatalf("ReadDir(%s) = %v, want the ported sidecar", dir, err)
	}
	for _, entry := range entries {
		if strings.HasPrefix(entry.Name(), "agent-") && strings.HasSuffix(entry.Name(), ".jsonl") {
			return strings.TrimSuffix(strings.TrimPrefix(entry.Name(), "agent-"), ".jsonl")
		}
	}
	t.Fatalf("the ported sidecar %s holds %v, want a subagent transcript", dir, entries)
	return ""
}

// forkField walks one ported record to a string leaf; a numeric step indexes an
// array.
func forkField(t *testing.T, body string, steps ...string) string {
	t.Helper()
	var value any
	if err := json.Unmarshal([]byte(strings.TrimSpace(body)), &value); err != nil {
		t.Fatalf("Unmarshal(%q) = %v", body, err)
	}
	for _, step := range steps {
		switch typed := value.(type) {
		case map[string]any:
			value = typed[step]
		case []any:
			index, err := strconv.Atoi(step)
			if err != nil || index >= len(typed) {
				t.Fatalf("step %q does not index %v", step, typed)
			}
			value = typed[index]
		default:
			t.Fatalf("step %q has nothing to walk in %v", step, typed)
		}
	}
	leaf, ok := value.(string)
	if !ok {
		t.Fatalf("%v is not a string leaf of %q", value, body)
	}
	return leaf
}

// forkBlockID answers the tool_use block's re-minted id.
func forkBlockID(t *testing.T, line string) string {
	t.Helper()
	return forkField(t, line, "message", "content", "0", "id")
}

// ---- One-shot create ----

func TestCreateWorkspaceOneShotDecoratesThePromptFromPrompts(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	repository := createRepositoryRef(t, d, repo)

	// Act
	resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repository,
		Form: &agentreplv1.CreateWorkspaceRequest_OneShot{OneShot: &agentreplv1.CreateWorkspaceOneShot{
			Prompt: said("add a health check endpoint"),
		}},
	}))

	// Assert: the sent prompt is the user's text PLUS the preamble and the
	// repository's completion directive, never the bare text.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace(one_shot) = (%v, %v), want a success", resp, err)
	}
	ws := resp.Msg.GetSuccess().GetWorkspace()
	shim := d.Shim(ws)
	shim.ExpectStartSession()
	got := text(shim.ExpectStartTurn().GetSaid())
	if !strings.Contains(got, "add a health check endpoint") {
		t.Fatalf("the one-shot's decorated prompt = %q, want the user's prompt spliced in", got)
	}
	if !strings.Contains(got, "when you're all done, please do the following postprocessing directive: ") {
		t.Fatalf("the one-shot's decorated prompt = %q, want the completion directive's framing sentence", got)
	}
}

func TestCreateWorkspaceOneShotCarriesTheRepositorysOwnDirective(t *testing.T) {
	t.Parallel()
	// Arrange: the repository states its own directive, and the daemon
	// concatenates THAT text rather than anything of its own.
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	directive := filepath.Join(repo.PolicyDir(), "oneshot-completion-directive.md")
	if err := os.WriteFile(directive,
		[]byte("<!-- used by: the daemon; placeholders: none -->\nfile a report in the ledger.\n"), 0o644); err != nil {
		t.Fatalf("write the repository's completion directive: %v", err)
	}
	repository := createRepositoryRef(t, d, repo)

	// Act
	resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repository,
		Form: &agentreplv1.CreateWorkspaceRequest_OneShot{OneShot: &agentreplv1.CreateWorkspaceOneShot{
			Prompt: said("ship the fix"),
		}},
	}))

	// Assert.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace(one_shot) = (%v, %v), want a success", resp, err)
	}
	ws := resp.Msg.GetSuccess().GetWorkspace()
	shim := d.Shim(ws)
	shim.ExpectStartSession()
	got := text(shim.ExpectStartTurn().GetSaid())
	if !strings.Contains(got, "file a report in the ledger.") {
		t.Fatalf("the one-shot's decorated prompt = %q, want the repository's own directive", got)
	}
}

func TestCreateWorkspaceOneShotRefusesARepositoryThatStatesNoPolicy(t *testing.T) {
	t.Parallel()
	// Arrange: a repository with no `.agent-repl/prompts` of its own. The
	// daemon's corpus is the policy of the daemon's OWN repository and is
	// never a fallback for this one (owner ruling, 2026-09-12).
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	if err := os.RemoveAll(repo.PolicyDir()); err != nil {
		t.Fatalf("remove the repository policy: %v", err)
	}
	repository := createRepositoryRef(t, d, repo)

	// Act.
	resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repository,
		Form: &agentreplv1.CreateWorkspaceRequest_OneShot{OneShot: &agentreplv1.CreateWorkspaceOneShot{
			Prompt: said("ship the fix"),
		}},
	}))

	// Assert: the arm carries the directory to write and the files it needs.
	if err != nil {
		t.Fatalf("CreateWorkspace(one_shot) = error %v, want a typed refusal", err)
	}
	missing := resp.Msg.GetError().GetOneShotPolicyMissing()
	if missing == nil {
		t.Fatalf("CreateWorkspace(one_shot) = %v, want one_shot_policy_missing", resp.Msg)
	}
	if missing.GetPolicyDir() != repo.PolicyDir() {
		t.Fatalf("policy_dir = %q, want %q", missing.GetPolicyDir(), repo.PolicyDir())
	}
	want := []string{"workspace-autonomous-preamble.md", "oneshot-completion-directive.md"}
	if strings.Join(missing.GetMissingFiles(), ",") != strings.Join(want, ",") {
		t.Fatalf("missing_files = %v, want %v", missing.GetMissingFiles(), want)
	}
}

func TestCreateWorkspaceOneShotRefusesARepositoryMissingOnlyTheDirective(t *testing.T) {
	t.Parallel()
	// Arrange: a policy that states the preamble and no completion directive.
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	if err := os.Remove(filepath.Join(repo.PolicyDir(), "oneshot-completion-directive.md")); err != nil {
		t.Fatalf("remove the completion directive: %v", err)
	}
	repository := createRepositoryRef(t, d, repo)

	// Act.
	resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repository,
		Form: &agentreplv1.CreateWorkspaceRequest_OneShot{OneShot: &agentreplv1.CreateWorkspaceOneShot{
			Prompt: said("ship the fix"),
		}},
	}))

	// Assert.
	if err != nil {
		t.Fatalf("CreateWorkspace(one_shot) = error %v, want a typed refusal", err)
	}
	missing := resp.Msg.GetError().GetOneShotPolicyMissing()
	if missing == nil {
		t.Fatalf("CreateWorkspace(one_shot) = %v, want one_shot_policy_missing", resp.Msg)
	}
	got := missing.GetMissingFiles()
	if len(got) != 1 || got[0] != "oneshot-completion-directive.md" {
		t.Fatalf("missing_files = %v, want only the completion directive", got)
	}
}

func TestCreateWorkspaceOneShotTakesNoFinishActionOnCompletion(t *testing.T) {
	t.Parallel()
	// Arrange: the daemon performs no finish (owner ruling, 2026-09-12) — the
	// agent carries the directive out itself.
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	repository := createRepositoryRef(t, d, repo)
	resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repository,
		Form: &agentreplv1.CreateWorkspaceRequest_OneShot{OneShot: &agentreplv1.CreateWorkspaceOneShot{
			Prompt: said("ship the fix"),
		}},
	}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace(one_shot) = (%v, %v), want a success", resp, err)
	}
	ws := resp.Msg.GetSuccess().GetWorkspace()
	shim := d.Shim(ws)
	shim.ExpectStartSession()
	shim.ExpectStartTurn()

	// Act: the turn concludes with the ordinary success terminal.
	shim.PushAgentFrame(mainAgent, successFrame(mainAgent, nil))

	// Assert: the queue's own turn-ended handling has run to completion, so
	// reading the merge queue past this point is race-free rather than a
	// timing guess — and no merge was ever enqueued.
	d.AwaitWorkspaceLogOperation(ws.GetDir(), "daemon.promptqueue.turn_ended")
	evict, err := d.Client().UpdateMergeQueue(d.Ctx(), connect.NewRequest(&agentreplv1.UpdateMergeQueueRequest{
		Action: &agentreplv1.UpdateMergeQueueRequest_Evict{Evict: &agentreplv1.UpdateMergeQueueEvict{Workspace: ws}},
	}))
	if err != nil {
		t.Fatalf("UpdateMergeQueue(evict) = error %v, want a success carrying no_such_queued_merge", err)
	}
	if evict.Msg.GetError().GetNoSuchQueuedMerge() == nil {
		t.Fatalf("UpdateMergeQueue(evict) after a one-shot conclusion = %v, want no_such_queued_merge: the daemon takes no finish action", evict.Msg)
	}
}

// ---- Merge actions ----

func TestCreateWorkspaceMergeActionsAreRecordedAndReadBackByALaterMerge(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	repository := createRepositoryRef(t, d, repo)
	resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repository,
		Form: &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{
			MergeActions: &agentreplv1.CreateWorkspaceMergeActions{
				BeforeWsMerge: said("run the pre-merge checklist"),
			},
		}},
	}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace(merge_actions) = (%v, %v), want a success", resp, err)
	}
	ws := resp.Msg.GetSuccess().GetWorkspace()
	shim := d.Shim(ws)
	shim.ExpectStartSession()

	// Act: a later merge reads the recorded action back.
	harness.CommitWork(t, ws.GetDir())
	if _, err := d.Client().MergeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.MergeWorkspaceRequest{Workspace: ws})); err != nil {
		t.Fatalf("MergeWorkspace = error %v, want the merge enqueued", err)
	}

	// Assert
	pre := shim.ExpectStartTurn()
	if text(pre.GetSaid()) != "run the pre-merge checklist" {
		t.Fatalf("the merge's pre-prompt turn = %q, want the recorded before_ws_merge action read back", text(pre.GetSaid()))
	}
	if pre.GetOrigin() != conversationv1.PromptOrigin_PROMPT_ORIGIN_MERGE_BEFORE_ACTION {
		t.Fatalf("the pre-prompt's origin = %v, want MERGE_BEFORE_ACTION", pre.GetOrigin())
	}
	// Created through CreateWorkspace (real layout facts), so the enqueue and
	// pre-prompt admission reach none of internal/merge's WARN sites.
}

// ---- NukeWorkspace: kill-before-destroy ordering, and a git failure ----

func TestNukeWorkspaceKillsTheLiveSessionBeforeAnyGitCommandRuns(t *testing.T) {
	t.Parallel()
	// Arrange: an opened, LIVE workspace, its fake shim HUNG so KillSession's
	// arrival is observable before it is ever answered.
	//
	// The workspace is a WORKTREE of its repository, not the repository root
	// the shared fixture registers. Nuking a workspace whose directory IS the
	// repository root deletes the repository itself, so the `worktree prune`
	// that follows the removal has no repository left to run in and git fails
	// on the arrangement rather than on anything the daemon did; the subject
	// here is the ORDER of the kill against the git commands.
	f := newOpenedWorktree(t, harness.Opts{}, "nuke-order")
	// The sweep covers every test; the declared records are evidence of a session fault the test opens.
	f.d.ExpectWarnings("daemon.health.open_fault")
	// The nuke ends a LIVE session whose shim is hung, which is the whole
	// arrangement; the trail it leaves is evidence, not a fault.
	expectSessionKillRecords(f.d)
	f.shim.ExpectStartSession()
	f.shim.Hang()

	// Act: NukeWorkspace blocks on the hung KillSession
	// (internal/workspace/teardown.go's kill() awaits it before Nuke() ever
	// touches git), so it runs in its own goroutine.
	done := make(chan struct{})
	go func() {
		defer close(done)
		resp, err := f.d.Client().NukeWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.NukeWorkspaceRequest{Workspace: f.ws}))
		if err != nil || resp.Msg.GetSuccess() == nil {
			t.Errorf("NukeWorkspace = (%v, %v), want a success", resp, err)
		}
	}()

	// Assert: KillSession has reached the shim -- proof of ORDER, since the
	// fake's own log write happens on arrival, before it is gated on the
	// hang -- while the fake git's recorded calls carry no worktree removal
	// yet, because the daemon-side call is still blocked on the hung
	// KillSession and nothing past it has run.
	f.d.AwaitShimVerbOrder(f.ws.GetDir(), harness.RPCKillSession)
	for _, c := range f.d.Git.Calls() {
		if createArgsContainAll(c.Args, "worktree", "remove") {
			t.Fatalf("a worktree remove already ran while KillSession is still hung: %v, want the kill to finish first", c.Args)
		}
	}

	// Act: release the fake, letting KillSession answer and the nuke proceed.
	f.shim.Unhang()
	<-done

	// Assert: the worktree removal now follows the completed kill.
	found := false
	for _, c := range f.d.Git.Calls() {
		if createArgsContainAll(c.Args, "worktree", "remove") {
			found = true
		}
	}
	if !found {
		t.Fatalf("git calls = %v, want a worktree remove once the kill completed", f.d.Git.Calls())
	}
}

func TestNukeWorkspaceAGitFailureDuringTheWorktreeRemoveAnswersGitFailed(t *testing.T) {
	t.Parallel()
	// Arrange: a registered (no live session) workspace whose worktree
	// removal is scripted to fail.
	d := newDaemon(t, harness.Opts{})
	repo := harness.NewRepo(t)
	dir := worktreeOf(t, repo, "nuke-fails")
	ws := harness.Register(t, d, dir)
	repo.ScriptFailure(repo.Dir, 1, "fatal: unable to remove worktree", "worktree", "remove")

	// Act
	resp, err := d.Client().NukeWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.NukeWorkspaceRequest{Workspace: ws}))

	// Assert: NukeWorkspaceError.git_failed{detail} is the LANDED arm
	// endpoint_nuke_workspace.proto contracts for exactly this condition.
	// EXPECTED RED: grepping internal/, no production site wraps
	// internal/workspace/teardown.go's Nuke() git error into a
	// workspace.Refusal naming this arm (no "git_failed" producer anywhere
	// outside the proto and daemon/ERROR-ARMS.md's own commentary), so
	// server.answerRefusal's asRefusal falls through to a bare Connect
	// error today instead of this typed answer.
	if err != nil {
		t.Fatalf("NukeWorkspace with a scripted worktree-remove failure = transport error %v, want the git_failed arm", err)
	}
	failed := resp.Msg.GetError().GetGitFailed()
	if failed == nil {
		t.Fatalf("NukeWorkspace with a scripted worktree-remove failure = %v, want NukeWorkspaceError.git_failed", resp.Msg)
	}
	if !strings.Contains(failed.GetDetail(), "unable to remove worktree") {
		t.Fatalf("git_failed.detail = %q, want git's own account of the failure", failed.GetDetail())
	}
	// The git leaf records its own command failure at ERROR — that record IS
	// the evidence the refusal's detail is drawn from — and the verb answers
	// the refusal above it.
	d.ExpectWarnings("NukeWorkspace", "daemon.gitclient.remove_worktree")
}

// ---- CloseWorkspace: blocked by live detached work with no turn open ----

func TestCloseWorkspaceBlockedByLiveDetachedWorkWithNoTurnOpenAnswersBlocked(t *testing.T) {
	t.Parallel()
	// Arrange: detached work announced with NO turn EVER opened -- distinct
	// from a turn-in-flight refusal (internal/workspace/open.go's
	// closeBlocker: `running.Turn != nil` is checked FIRST and answers
	// "turn_in_flight"; this test's own branch is reached only once that is
	// nil AND live work remains, answering "live_work" instead).
	f := newOpened(t, harness.Opts{})
	footer := f.d.WatchFooter(f.ws)
	pushDetachedShell(f.shim, "work-close-1", "sleep 100")
	awaitLiveWork(t, f, 1)

	// Act
	resp, err := f.d.Client().CloseWorkspace(f.d.Ctx(), connect.NewRequest(&agentreplv1.CloseWorkspaceRequest{Workspace: f.ws}))

	// Assert: the exact refusal arm the proto contracts.
	if err != nil {
		t.Fatalf("CloseWorkspace with live detached work and no open turn = transport error %v, want the blocked arm", err)
	}
	if resp.Msg.GetError().GetBlocked() == nil {
		t.Fatalf("CloseWorkspace with live detached work and no open turn = %v, want CloseWorkspaceError.blocked", resp.Msg)
	}

	// Assert: the footer's composed reason names the LIVE_WORK cause, not a
	// turn in flight (CloseWorkspaceBlocked itself carries no field per
	// ERROR-ARMS.md, so the evidence rides only the footer's own text).
	fv := awaitFooter(t, f, footer, "footer closing.blocked with the live-work reason", func(v *frontendv1.FooterView) bool {
		return v.GetStrip().GetStatus().GetClosing().GetBlocked() != nil
	})
	line := fv.GetStrip().GetStatus().GetClosing().GetActivity().GetSalient().GetCloseBlocked().GetText()
	if !strings.Contains(line, "detached") {
		t.Fatalf("close-blocked activity text = %q, want it to name the live detached work, not a turn in flight", line)
	}
	f.d.ExpectWarnings("daemon.workspace.close")
}

// ---- create* helpers (prefixed create* so they cannot collide) ----

// createRepositoryRef registers a fresh workspace under a repository and
// reads back the daemon-minted RepositoryRef from the roster's repo section,
// since CreateWorkspace needs the echo token and no direct mint RPC exists.
func createRepositoryRef(t *testing.T, d *harness.Daemon, repo *harness.Repo) *workspacev1.RepositoryRef {
	t.Helper()
	ws := harness.Register(t, d, repo.Dir)
	roster := d.WatchRoster()
	got := harness.AwaitView(t, d.Ctx(), roster, "the repository section", func(r *frontendv1.WorkspaceRoster) bool {
		return rosterRepoRow(r, ws.GetId()) != nil
	})
	for _, s := range got.GetRepository().GetSections() {
		for _, row := range s.GetRows().GetRows() {
			if row.GetWorkspace().GetWorkspace().GetId() == ws.GetId() {
				return s.GetKey().GetRepository()
			}
		}
	}
	t.Fatalf("no repository section found for %s", repo.Dir)
	return nil
}

// createProjectDir answers the vendor CLI's project directory for one
// workspace, through the harness's single spelling of that rule.
//
// IT DELEGATES ON PURPOSE. The rule is "every byte that is not [A-Za-z0-9]
// becomes a dash", not "every slash becomes a dash", and the two coincide only
// while the temp root happens to be alphanumeric. Under macOS's per-user
// /var/folders/<hash>/T root it has an underscore in it, so a slashes-only
// spelling seeded the parent's transcript at a path the daemon never probes
// and the fork's port silently had nothing to carry.
func createProjectDir(configDir, workspaceDir string) string {
	return harness.ProjectDir(configDir, workspaceDir)
}

// createArgsContain reports whether a git call's args contain an exact token.
// createGitDir answers the directory a recorded git call selected with -C.
func createGitDir(args []string) string {
	for i, a := range args {
		if a == "-C" && i+1 < len(args) {
			return args[i+1]
		}
	}
	return ""
}

func createArgsContain(args []string, want string) bool {
	for _, a := range args {
		if a == want {
			return true
		}
	}
	return false
}

// createArgsContainAll reports whether a git call's args contain every token,
// in any position.
func createArgsContainAll(args []string, want ...string) bool {
	for _, w := range want {
		if !createArgsContain(args, w) {
			return false
		}
	}
	return true
}

// TestCreateWorkspaceUnderTheVendorGuardAloneSpawnsAFakeShim asserts a guarded
// daemon CREATES a workspace, and that the shim its bring-up spawned is running
// in fake mode.
//
// THIS IS THE REGRESSION THE WORKSPACE REALTESTS FOUND. The guard used to
// refuse the shim spawn outright, so a guarded daemon could not create a
// workspace at all: the spawn raised the guard's ForbiddenError, the bring-up
// answered `spawn_failed`, and every realtest that needed a second workspace
// died there. The guard means "never touch the real vendor", and a shim is our
// own process with a fake mode, so it now FORCES the fake instead of refusing.
//
// NoFake withholds the whole stack's fake mode and WithoutFakeShimsHook
// withholds AGENT_REPL_FAKE_SHIMS, which leaves the vendor guard as the ONLY
// thing that can make this spawn fake — so a shim that comes up fake here came
// up fake because of the guard and nothing else.
func TestCreateWorkspaceUnderTheVendorGuardAloneSpawnsAFakeShim(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{NoFake: true, WithoutFakeShimsHook: true})
	repo := harness.NewRepo(t)
	repository := createRepositoryRef(t, d, repo)

	// Act
	resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repository,
		Form: &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{
			InitialPrompt: said("fix the flaky reconnect test"),
			// A NAME IS SUPPLIED SO THE SUBJECT STAYS THE SHIM. This daemon is
			// started without AGENT_REPL_CLAUDE_BIN on purpose, so the
			// headless naming call a nameless create makes would be refused by
			// the very guard this test is about — and the create would never
			// reach the spawn it is asserting on.
			Name: strPtr("guarded-spawn"),
		}},
	}))

	// Assert: the create succeeded rather than answering the spawn refusal.
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace under the vendor guard alone = (%v, %v), want a success", resp, err)
	}
	ws := resp.Msg.GetSuccess().GetWorkspace()

	// Assert: the bring-up spawned a shim, and it is a FAKE one.
	shim := d.Shim(ws)
	if !shim.Info().Fake {
		t.Fatalf("the spawned shim's argv = %v, want --fake forced by the vendor guard", shim.Info().Argv)
	}
}

// TestCreateWorkspaceUnderTheVendorGuardAloneStartsTheSession asserts the
// bring-up under the guard goes all the way to a started session, not merely to
// a live process: the fake-mode shim serves StartSession and the initial prompt
// reaches it.
func TestCreateWorkspaceUnderTheVendorGuardAloneStartsTheSession(t *testing.T) {
	t.Parallel()
	// Arrange
	d := newDaemon(t, harness.Opts{NoFake: true, WithoutFakeShimsHook: true})
	repo := harness.NewRepo(t)
	repository := createRepositoryRef(t, d, repo)

	// Act
	resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repository,
		Form: &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{
			InitialPrompt: said("fix the flaky reconnect test"),
			// See the sibling test: the name is supplied so the guarded
			// naming call is never made and the subject stays the session.
			Name: strPtr("guarded-session"),
		}},
	}))
	if err != nil || resp.Msg.GetSuccess() == nil {
		t.Fatalf("CreateWorkspace under the vendor guard alone = (%v, %v), want a success", resp, err)
	}

	// Assert
	shim := d.Shim(resp.Msg.GetSuccess().GetWorkspace())
	if fresh := shim.ExpectStartSession(); fresh.GetFresh() == nil {
		t.Fatalf("StartSession request = %v, want fresh for a brand-new workspace", fresh)
	}
	if turn := shim.ExpectStartTurn(); text(turn.GetSaid()) != "fix the flaky reconnect test" {
		t.Fatalf("StartTurn.said = %q, want the initial prompt verbatim", text(turn.GetSaid()))
	}
}

// TestCreateWorkspaceWhoseShimWillNotComeUpAnswersTheSpawnFailedArm pins the
// gap the workspace realtests left behind, now closed: CreateWorkspaceError
// carries `spawn_failed` as of 2026-09-12, so a create whose bring-up cannot
// start a shim states its refusal IN BAND, as its OpenWorkspace twin
// (roster_test.go) always could.
//
// The refusal itself never changed — `workspace.ArmSpawnFailed`, renamed onto
// the rpc that raised it — and no daemon change followed the arm: `server.fill`
// already supplied `detail` for any arm that has the field, so the handler
// switched onto the arm by the arm simply existing. This test was written
// asserting the unlanded-arm Connect error and prescribing this very flip.
func TestCreateWorkspaceWhoseShimWillNotComeUpAnswersTheSpawnFailedArm(t *testing.T) {
	t.Parallel()
	// Arrange: the create mints the workspace dir, so the dying shim is
	// scripted through the profile EVERY spawn falls back to.
	d := newDaemon(t, harness.Opts{})
	// The sweep covers every test; the declared records are evidence of the bring-up death the test scripts.
	d.ExpectWarnings("daemon.shimclient.redial", "daemon.shimclient.exit", "daemon.shimclient.spawn",
		"daemon.workspace.bring_up", "daemon.workspace.open", "daemon.workspace.create")
	d.WriteDefaultShimProfile(harness.ShimProfile{
		ExitOn: harness.ExitOnStartup, ExitCode: 7, Stderr: "boom: fake bring-up death",
	})
	repo := harness.NewRepo(t)
	repository := createRepositoryRef(t, d, repo)

	// Act
	resp, err := d.Client().CreateWorkspace(d.Ctx(), connect.NewRequest(&agentreplv1.CreateWorkspaceRequest{
		Repository: repository,
		Form: &agentreplv1.CreateWorkspaceRequest_Standard{Standard: &agentreplv1.CreateWorkspaceStandard{
			InitialPrompt: said("fix the flaky reconnect test"),
		}},
	}))

	// Assert: the refusal is the typed arm, carrying the spawn's own account.
	if err != nil {
		t.Fatalf("CreateWorkspace onto a dying shim: %v", err)
	}
	detail := resp.Msg.GetError().GetSpawnFailed().GetDetail()
	if detail == "" {
		t.Fatalf("CreateWorkspace onto a dying shim = %v, want the spawn_failed arm carrying a detail",
			resp.Msg.GetResult())
	}
}
