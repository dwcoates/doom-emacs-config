package commandfile

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/merge"
	"claude-repld/internal/workspace"
)

func TestNewRefusesMissingCollaborators(t *testing.T) {
	full := Deps{
		Dir: "/output", Verbs: newFakeVerbs(), Merge: &fakeMerge{},
		Prompts: &fakePrompts{}, Log: newFakeSurfaces(),
		Serves: func() bool { return true }, Home: "/Users/me",
	}
	tests := []struct {
		name  string
		strip func(*Deps)
	}{
		{name: "no directory", strip: func(d *Deps) { d.Dir = "" }},
		{name: "no verbs", strip: func(d *Deps) { d.Verbs = nil }},
		{name: "no merge orchestrator", strip: func(d *Deps) { d.Merge = nil }},
		{name: "no prompt handler", strip: func(d *Deps) { d.Prompts = nil }},
		{name: "no log surfaces", strip: func(d *Deps) { d.Log = nil }},
		{name: "no serving answer", strip: func(d *Deps) { d.Serves = nil }},
		{name: "no home directory", strip: func(d *Deps) { d.Home = "" }},
		{name: "a relative home directory", strip: func(d *Deps) { d.Home = "me" }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			deps := full
			tt.strip(&deps)
			// Act.
			_, err := New(deps)
			// Assert.
			if err == nil {
				t.Fatalf("New(%s) = nil error, want a refusal", tt.name)
			}
		})
	}
}

func TestApplyFileClaimsByRename(t *testing.T) {
	// Arrange: the rename IS the claim, so nothing else marks a file as taken.
	f := newFixture(t)
	f.workspace("w1", "/tree/w1")
	path := f.write(t, "workspace_commands_a.json", `[{"type":"merge","workspace":"w1"}]`)

	// Act.
	if err := f.ingress.ApplyFile(context.Background(), path); err != nil {
		t.Fatalf("ApplyFile: %v", err)
	}

	// Assert.
	if _, err := os.Stat(path); !os.IsNotExist(err) {
		t.Fatalf("the original file still exists: %v", err)
	}
	if got := entries(t, filepath.Join(f.dir, "claimed")); len(got) != 1 {
		t.Fatalf("claimed files = %v, want exactly one", got)
	}
}

func TestApplyFileQuarantinesAMalformedFile(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	path := f.write(t, "workspace_commands_bad.json", `{not even an array`)

	// Act.
	err := f.ingress.ApplyFile(context.Background(), path)

	// Assert.
	if err == nil {
		t.Fatal("ApplyFile(malformed) = nil error, want the parse failure surfaced")
	}
	if got := entries(t, filepath.Join(f.dir, "quarantine")); len(got) != 1 {
		t.Fatalf("quarantined files = %v, want exactly one", got)
	}
}

func TestApplyFileAppliesNothingFromAMalformedFile(t *testing.T) {
	// Arrange: one bad entry means the whole array applies nothing.
	f := newFixture(t)
	f.workspace("w1", "/tree/w1")
	path := f.write(t, "workspace_commands_mixed.json",
		`[{"type":"merge","workspace":"w1"},{"type":"merge"}]`)

	// Act.
	_ = f.ingress.ApplyFile(context.Background(), path)

	// Assert.
	if len(f.merge.enqueued) != 0 {
		t.Fatalf("enqueued merges = %v, want none", f.merge.enqueued)
	}
}

// TestApplyFileEnqueuesAMergeAsTheAgentsAsk pins who a command-file merge is
// from: an agent's turn, whose merge must never displace that very turn
// (merge.Requester; 2026-09-28, prompt-bubble-height).
func TestApplyFileEnqueuesAMergeAsTheAgentsAsk(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", "/tree/w1")
	path := f.write(t, "workspace_commands_merge.json", `[{"type":"merge","workspace":"w1"}]`)

	// Act.
	if err := f.ingress.ApplyFile(context.Background(), path); err != nil {
		t.Fatalf("ApplyFile: %v", err)
	}

	// Assert.
	if len(f.merge.by) != 1 || f.merge.by[0] != merge.RequestedByAgent {
		t.Fatalf("the merge was enqueued as %v, want the agent's ask", f.merge.by)
	}
}

func TestApplyFileLogsAWarningForAQuarantine(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	path := f.write(t, "workspace_commands_bad.json", `nope`)

	// Act.
	_ = f.ingress.ApplyFile(context.Background(), path)

	// Assert.
	var warned bool
	for _, record := range f.log.logger.Records() {
		if record.Operation == opQuarantine && record.Level == "warn" {
			warned = true
		}
	}
	if !warned {
		t.Fatalf("records = %v, want a warning under %s", f.log.logger.Records(), opQuarantine)
	}
}

func TestApplyFileMapsCreateOntoTheCreationVerb(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	path := f.write(t, "workspace_commands_c.json",
		`[{"type":"create","name":"DWC/x","git_root":"/repo","prompt":"do a thing"}]`)

	// Act.
	if err := f.ingress.ApplyFile(context.Background(), path); err != nil {
		t.Fatalf("ApplyFile: %v", err)
	}

	// Assert.
	if len(f.verbs.calls) != 1 || f.verbs.calls[0].Verb != "create" {
		t.Fatalf("verb calls = %v, want one create", verbNames(f.verbs.calls))
	}
	spec := f.verbs.calls[0].Spec
	if spec.RepoDir != "/repo" || spec.Name != "DWC/x" || spec.InitialPrompt != "do a thing" {
		t.Fatalf("create spec = %+v, want the entry's own fields", spec)
	}
}

// TestApplyFileNamelessCreateLeavesTheNamingToTheVerb pins that the file
// channel has NO naming of its own: a nameless entry reaches `Create' with an
// empty Name, which is exactly what makes the daemon's headless naming call
// fire for it. There is one naming site, not two.
func TestApplyFileNamelessCreateLeavesTheNamingToTheVerb(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	path := f.write(t, "workspace_commands_c.json",
		`[{"type":"create","git_root":"/repo","prompt":"fix the flaky login test"}]`)

	// Act.
	if err := f.ingress.ApplyFile(context.Background(), path); err != nil {
		t.Fatalf("ApplyFile: %v", err)
	}

	// Assert.
	spec := f.verbs.calls[0].Spec
	if spec.Name != "" {
		t.Fatalf("create spec name = %q, want it empty so the verb's naming call mints one", spec.Name)
	}
	if spec.InitialPrompt != "fix the flaky login test" {
		t.Fatalf("create spec prompt = %q, want the entry's own prompt for the naming call", spec.InitialPrompt)
	}
}

func TestApplyFileOneShotCreateDispatchesTheOneShotFormAlone(t *testing.T) {
	// Arrange: the channel names no finish, and neither does the spec — what
	// happens on completion is the repository's own directive.
	f := newFixture(t)
	path := f.write(t, "workspace_commands_c.json",
		`[{"type":"create","git_root":"/repo","prompt":"do a thing","one_shot":true}]`)

	// Act.
	if err := f.ingress.ApplyFile(context.Background(), path); err != nil {
		t.Fatalf("ApplyFile: %v", err)
	}

	// Assert.
	want := workspace.CreateSpec{RepoDir: "/repo", InitialPrompt: "do a thing", OneShot: true}
	if got := f.verbs.calls[0].Spec; !reflect.DeepEqual(got, want) {
		t.Fatalf("create spec = %+v, want %+v", got, want)
	}
}

func TestApplyFileMapsPromptOntoSubmitPromptsOwnBody(t *testing.T) {
	// Arrange: a command-file prompt must be indistinguishable from a typed one.
	f := newFixture(t)
	f.workspace("w1", "/tree/w1")
	path := f.write(t, "workspace_commands_p.json",
		`[{"type":"prompt","workspace":"w1","prompt":"hello"}]`)

	// Act.
	if err := f.ingress.ApplyFile(context.Background(), path); err != nil {
		t.Fatalf("ApplyFile: %v", err)
	}

	// Assert.
	if len(f.prompts.submissions) != 1 || f.prompts.submissions[0].Text != "hello" {
		t.Fatalf("submissions = %+v, want one carrying the entry's text", f.prompts.submissions)
	}
}

func TestApplyFileTreatsSendAsPrompt(t *testing.T) {
	// Arrange: "send" is the older spelling of the same request.
	f := newFixture(t)
	f.workspace("w1", "/tree/w1")
	path := f.write(t, "workspace_commands_s.json",
		`[{"type":"send","workspace":"w1","prompt":"hello"}]`)

	// Act.
	if err := f.ingress.ApplyFile(context.Background(), path); err != nil {
		t.Fatalf("ApplyFile: %v", err)
	}

	// Assert.
	if len(f.prompts.submissions) != 1 {
		t.Fatalf("submissions = %+v, want exactly one", f.prompts.submissions)
	}
}

func TestApplyFileStampsThePromptOrigin(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", "/tree/w1")
	path := f.write(t, "workspace_commands_p.json",
		`[{"type":"prompt","workspace":"w1","prompt":"hello"}]`)

	// Act.
	if err := f.ingress.ApplyFile(context.Background(), path); err != nil {
		t.Fatalf("ApplyFile: %v", err)
	}

	// Assert.
	if f.prompts.submissions[0].Origin != conversationv1.PromptOrigin_PROMPT_ORIGIN_LEGACY_HOST_PROMPT {
		t.Fatalf("origin = %v, want the host-written channel's origin", f.prompts.submissions[0].Origin)
	}
}

func TestApplyFileKeysTheIdempotencyOnTheFileAndIndex(t *testing.T) {
	// Arrange: a file dropped twice must not run one prompt twice.
	f := newFixture(t)
	f.workspace("w1", "/tree/w1")
	path := f.write(t, "workspace_commands_p.json",
		`[{"type":"prompt","workspace":"w1","prompt":"a"},{"type":"prompt","workspace":"w1","prompt":"b"}]`)

	// Act.
	if err := f.ingress.ApplyFile(context.Background(), path); err != nil {
		t.Fatalf("ApplyFile: %v", err)
	}

	// Assert.
	keys := []string{f.prompts.submissions[0].Key, f.prompts.submissions[1].Key}
	if keys[0] != "workspace_commands_p.json:0" || keys[1] != "workspace_commands_p.json:1" {
		t.Fatalf("idempotency keys = %v, want the file name and entry index", keys)
	}
}

func TestApplyFileMapsEveryWorkspaceVerb(t *testing.T) {
	tests := []struct {
		name string
		body string
		want string
	}{
		{name: "close", body: `[{"type":"close","workspace":"w1"}]`, want: "close"},
		{name: "forget", body: `[{"type":"forget","workspace":"w1"}]`, want: "forget"},
		{name: "open", body: `[{"type":"open","workspace":"w1"}]`, want: "open"},
		{name: "switch", body: `[{"type":"switch","workspace":"w1"}]`, want: "select"},
		{name: "task create", body: `[{"type":"task-create","title":"t"}]`, want: "create_task"},
		{
			name: "task toggle done",
			body: `[{"type":"task-toggle-done","id":"task-1","done":true}]`,
			want: "update_task",
		},
		{
			name: "task add workspace",
			body: `[{"type":"task-add-workspace","id":"task-1","workspace":"w1"}]`,
			want: "assign_task",
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.workspace("w1", "/tree/w1")
			path := f.write(t, "workspace_commands_v.json", tt.body)
			// Act.
			if err := f.ingress.ApplyFile(context.Background(), path); err != nil {
				t.Fatalf("ApplyFile: %v", err)
			}
			// Assert.
			if len(f.verbs.calls) != 1 || f.verbs.calls[0].Verb != tt.want {
				t.Fatalf("verb calls = %v, want one %s", verbNames(f.verbs.calls), tt.want)
			}
		})
	}
}

func TestApplyFileMapsMergeOntoTheOrchestrator(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", "/tree/w1")
	path := f.write(t, "workspace_commands_m.json", `[{"type":"merge","workspace":"w1"}]`)

	// Act.
	if err := f.ingress.ApplyFile(context.Background(), path); err != nil {
		t.Fatalf("ApplyFile: %v", err)
	}

	// Assert.
	if len(f.merge.enqueued) != 1 || f.merge.enqueued[0] != "w1" {
		t.Fatalf("enqueued merges = %v, want w1", f.merge.enqueued)
	}
}

func TestApplyFileResolvesAnEntryNamingOnlyADirectory(t *testing.T) {
	// Arrange: the older producers write a dir rather than an id.
	f := newFixture(t)
	f.workspace("w1", "/tree/w1")
	path := f.write(t, "workspace_commands_d.json", `[{"type":"close","dir":"/tree/w1"}]`)

	// Act.
	if err := f.ingress.ApplyFile(context.Background(), path); err != nil {
		t.Fatalf("ApplyFile: %v", err)
	}

	// Assert.
	if len(f.verbs.calls) != 1 || f.verbs.calls[0].WS != "w1" {
		t.Fatalf("verb calls = %+v, want one close of w1", f.verbs.calls)
	}
}

func TestApplyFileResolvesTheSkillsMergeByItsProjectDir(t *testing.T) {
	// Arrange: VERBATIM what the /create-or-update-workspace skill wrote for a
	// one-shot's merge on 2026-09-21 — the workspace's NAME beside the
	// canonical `project_dir`. It was refused `unknown_workspace` and
	// quarantined, because `workspace` was read as an id and `project_dir` was
	// not read at all.
	f := newFixture(t)
	f.workspace("db6c528bba4043b4", "/worktrees/glimmer-intensity-boost")
	path := f.write(t, "workspace_commands_skill.json",
		`[{"type":"merge","workspace":"glimmer-intensity-boost","project_dir":"/worktrees/glimmer-intensity-boost"}]`)

	// Act.
	if err := f.ingress.ApplyFile(context.Background(), path); err != nil {
		t.Fatalf("ApplyFile: %v", err)
	}

	// Assert.
	if len(f.merge.enqueued) != 1 || f.merge.enqueued[0] != "db6c528bba4043b4" {
		t.Fatalf("enqueued merges = %v, want the one workspace at that project_dir", f.merge.enqueued)
	}
}

func TestApplyFileNeverResolvesTheDisplayNameWhenADirectoryIsGiven(t *testing.T) {
	// Arrange: the display name happens to BE another workspace's name. The
	// contract says the name is never used to resolve, so the directory wins.
	f := newFixture(t)
	f.workspace("w1", "/tree/w1")
	f.workspace("w2", "/tree/w2")
	path := f.write(t, "workspace_commands_name.json",
		`[{"type":"close","workspace":"some-display-name","project_dir":"/tree/w2"}]`)

	// Act.
	if err := f.ingress.ApplyFile(context.Background(), path); err != nil {
		t.Fatalf("ApplyFile: %v", err)
	}

	// Assert.
	if len(f.verbs.calls) != 1 || f.verbs.calls[0].WS != "w2" {
		t.Fatalf("verb calls = %+v, want one close of the workspace at the directory", f.verbs.calls)
	}
}

func TestApplyFilePrefersProjectDirOverTheOlderDir(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", "/tree/w1")
	f.workspace("w2", "/tree/w2")
	path := f.write(t, "workspace_commands_both.json",
		`[{"type":"close","project_dir":"/tree/w2","dir":"/tree/w1"}]`)

	// Act.
	if err := f.ingress.ApplyFile(context.Background(), path); err != nil {
		t.Fatalf("ApplyFile: %v", err)
	}

	// Assert.
	if len(f.verbs.calls) != 1 || f.verbs.calls[0].WS != "w2" {
		t.Fatalf("verb calls = %+v, want the project_dir's workspace", f.verbs.calls)
	}
}

func TestApplyFileRefusesAnIdThatNamesADifferentWorkspaceThanTheDirectory(t *testing.T) {
	// Arrange: an ID, not a display name — and it is another workspace's.
	f := newFixture(t)
	f.workspace("w1", "/tree/w1")
	f.workspace("w2", "/tree/w2")
	path := f.write(t, "workspace_commands_cross.json",
		`[{"type":"close","workspace":"w1","project_dir":"/tree/w2"}]`)

	// Act.
	err := f.ingress.ApplyFile(context.Background(), path)

	// Assert.
	if err == nil {
		t.Fatal("ApplyFile(id of one workspace, directory of another) = nil error, want the refusal surfaced")
	}
	if len(f.verbs.calls) != 0 {
		t.Fatalf("verb calls = %v, want none", verbNames(f.verbs.calls))
	}
}

func TestApplyFileRefusesAnEntryWhoseDirDisagreesWithTheRegistry(t *testing.T) {
	// Arrange: the command-file channel is held to the same mismatch refusal as
	// the wire.
	f := newFixture(t)
	f.workspace("w1", "/tree/w1")
	path := f.write(t, "workspace_commands_d.json",
		`[{"type":"close","workspace":"w1","dir":"/somewhere/else"}]`)

	// Act.
	err := f.ingress.ApplyFile(context.Background(), path)

	// Assert.
	if err == nil {
		t.Fatal("ApplyFile(mismatched dir) = nil error, want the refusal surfaced")
	}
	if len(f.verbs.calls) != 0 {
		t.Fatalf("verb calls = %v, want none", verbNames(f.verbs.calls))
	}
}

func TestApplyFileSurfacesAVerbFailure(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", "/tree/w1")
	f.verbs.err = errors.New("the workspace is not quiet")
	path := f.write(t, "workspace_commands_e.json", `[{"type":"close","workspace":"w1"}]`)

	// Act.
	err := f.ingress.ApplyFile(context.Background(), path)

	// Assert.
	if err == nil {
		t.Fatal("ApplyFile() = nil error, want the verb failure surfaced")
	}
}

func TestApplyFileAppliesEveryEntryEvenWhenOneFails(t *testing.T) {
	// Arrange: the entries were all validated, so a later one is still owed its
	// attempt when an earlier one's verb refuses.
	f := newFixture(t)
	f.workspace("w1", "/tree/w1")
	f.merge.err = errors.New("no layout facts")
	path := f.write(t, "workspace_commands_e.json",
		`[{"type":"merge","workspace":"w1"},{"type":"close","workspace":"w1"}]`)

	// Act.
	err := f.ingress.ApplyFile(context.Background(), path)

	// Assert.
	if err == nil {
		t.Fatal("ApplyFile() = nil error, want the merge failure surfaced")
	}
	if len(f.verbs.calls) != 1 || f.verbs.calls[0].Verb != "close" {
		t.Fatalf("verb calls = %v, want the close still attempted", verbNames(f.verbs.calls))
	}
}

func TestApplyFileRefusesAMissingFile(t *testing.T) {
	// Arrange.
	f := newFixture(t)

	// Act.
	err := f.ingress.ApplyFile(context.Background(), filepath.Join(f.dir, "workspace_commands_gone.json"))

	// Assert.
	if err == nil {
		t.Fatal("ApplyFile(missing) = nil error, want the claim failure surfaced")
	}
}

func TestApplyFileOfAFileAnotherSweeperClaimedIsNoFault(t *testing.T) {
	// Arrange: the file is already gone -- a handover's other daemon, finishing
	// a sweep it began, renamed it into its own claim first.
	f := newFixture(t)

	// Act.
	err := f.ingress.ApplyFile(context.Background(), filepath.Join(f.dir, "workspace_commands_gone.json"))

	// Assert.
	if !errors.Is(err, ErrClaimedElsewhere) {
		t.Fatalf("ApplyFile(claimed elsewhere) = %v, want ErrClaimedElsewhere", err)
	}
	for _, r := range f.log.logger.Records() {
		if r.Level == "error" || r.Level == "warn" {
			t.Fatalf("record %+v: losing the claim to another sweeper is the exclusivity working, never a fault", r)
		}
	}
}

func TestASweepAppliesTheIntakeOnlyWhileThisDaemonServes(t *testing.T) {
	tests := []struct {
		name       string
		serves     bool
		wantMerges int
	}{
		{name: "a serving daemon applies the file", serves: true, wantMerges: 1},
		{name: "a daemon that does not serve leaves it", serves: false, wantMerges: 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			f := newFixture(t)
			f.serves = tt.serves
			f.workspace("w1", "/tree/w1")
			path := f.write(t, "workspace_commands_r.json", `[{"type":"merge","workspace":"w1"}]`)
			ctx, cancel := context.WithCancel(context.Background())

			// Act: the first sweep runs before the loop selects on the ticker.
			cancel()
			_ = f.ingress.Run(ctx)

			// Assert.
			if len(f.merge.enqueued) != tt.wantMerges {
				t.Fatalf("enqueued merges = %v, want %d", f.merge.enqueued, tt.wantMerges)
			}
			if _, err := os.Stat(path); tt.serves == (err == nil) {
				t.Fatalf("stat %s = %v: the file must be claimed exactly when this daemon serves", path, err)
			}
		})
	}
}

func TestSettledAcceptsAnAgedFile(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	path := f.write(t, "workspace_commands_a.json", `[{"type":"task-create","title":"t"}]`)

	// Act.
	got, err := f.ingress.(*ingress).settled(path)

	// Assert.
	if err != nil || !got {
		t.Fatalf("settled() = (%v, %v), want a settled file", got, err)
	}
}

func TestSettledLeavesAYoungHalfWrittenFile(t *testing.T) {
	// Arrange: a file still mid-token must never be ingested.
	f := newFixture(t)
	path := filepath.Join(f.dir, "workspace_commands_young.json")
	if err := os.WriteFile(path, []byte(`[{"type":"task-cre`), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	// The clock reads the file's own mtime, so its age is exactly zero:
	// a wall-clock read after the write aged it by however long the
	// scheduler held this goroutine, past Interval under load.
	info, err := os.Stat(path)
	if err != nil {
		t.Fatalf("stat: %v", err)
	}
	f.now = info.ModTime()

	// Act.
	got, err := f.ingress.(*ingress).settled(path)

	// Assert.
	if err != nil {
		t.Fatalf("settled: %v", err)
	}
	if got {
		t.Fatal("settled() accepted a young, half-written file")
	}
}

func TestSettledAcceptsAYoungButCompleteFile(t *testing.T) {
	// Arrange: a complete document needs no settling window.
	f := newFixture(t)
	path := filepath.Join(f.dir, "workspace_commands_young.json")
	if err := os.WriteFile(path, []byte(`[{"type":"task-create","title":"t"}]`), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}
	// The clock reads the file's own mtime, so its age is exactly zero:
	// a wall-clock read after the write aged it by however long the
	// scheduler held this goroutine, past Interval under load.
	info, err := os.Stat(path)
	if err != nil {
		t.Fatalf("stat: %v", err)
	}
	f.now = info.ModTime()

	// Act.
	got, err := f.ingress.(*ingress).settled(path)

	// Assert.
	if err != nil || !got {
		t.Fatalf("settled() = (%v, %v), want the complete document accepted", got, err)
	}
}

func TestRunAppliesAFileAndStops(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", "/tree/w1")
	f.write(t, "workspace_commands_r.json", `[{"type":"merge","workspace":"w1"}]`)
	ctx, cancel := context.WithCancel(context.Background())

	// Act: the first sweep runs before the loop ever selects on the ticker, so
	// cancelling immediately still leaves the file applied.
	cancel()
	err := f.ingress.Run(ctx)

	// Assert.
	if !errors.Is(err, context.Canceled) {
		t.Fatalf("Run() = %v, want the cancellation", err)
	}
	if len(f.merge.enqueued) != 1 {
		t.Fatalf("enqueued merges = %v, want the file applied by the first sweep", f.merge.enqueued)
	}
}

func TestRunIgnoresAFileTheGlobDoesNotMatch(t *testing.T) {
	// Arrange: producers write through a dot-prefixed temp name the glob cannot
	// claim, then rename into place.
	f := newFixture(t)
	f.workspace("w1", "/tree/w1")
	f.write(t, ".workspace_commands_tmp.json", `[{"type":"merge","workspace":"w1"}]`)
	ctx, cancel := context.WithCancel(context.Background())

	// Act.
	cancel()
	_ = f.ingress.Run(ctx)

	// Assert.
	if len(f.merge.enqueued) != 0 {
		t.Fatalf("enqueued merges = %v, want none from an unmatched name", f.merge.enqueued)
	}
}

func TestSaidTextIsTheSharedPromptComposition(t *testing.T) {
	// Arrange: the ingress composes prompts exactly as the creation verb does.
	// Act.
	said := workspace.SaidText("hello")

	// Assert.
	if said.GetContent().GetBlocks()[0].GetText().GetText() != "hello" {
		t.Fatalf("SaidText() = %v, want one text block", said)
	}
}

// TestAnEntryRefusedAtApplyTimeQuarantinesTheFile pins the file route's answer
// to a refusal: the rpc route answers its caller, and a file has none, so the
// refusal is recorded and the file retires to quarantine rather than sitting
// in the claimed directory forever.
func TestAnEntryRefusedAtApplyTimeQuarantinesTheFile(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", "/tree/w1")
	f.merge.err = errors.New("no layout facts")
	path := f.write(t, "workspace_commands_refused.json",
		`[{"type":"merge","workspace":"w1"}]`)

	// Act.
	err := f.ingress.ApplyFile(context.Background(), path)

	// Assert.
	if !errors.Is(err, ErrQuarantined) {
		t.Fatalf("ApplyFile = %v, want an error naming ErrQuarantined", err)
	}
	quarantined := filepath.Join(f.dir, "quarantine", "workspace_commands_refused.json")
	if _, statErr := os.Stat(quarantined); statErr != nil {
		t.Fatalf("stat %q: %v, want the refused file in quarantine", quarantined, statErr)
	}
}

// TestApplyFileExpandsTheSkillsTildeGitRoot pins the create the
// /create-or-update-workspace skill dispatched on 2026-09-28: `git_root` was
// the literal `~/.config/doom`, which the skill's contract says is expanded
// downstream. The daemon absolutized it against its working directory instead,
// looked for `/Users/me/~/.config/doom`, and quarantined the file.
func TestApplyFileExpandsTheSkillsTildeGitRoot(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	path := f.write(t, "workspace_commands_tilde.json",
		`[{"type":"create","name":"merge-rebase-first","git_root":"~/.config/doom","prompt":"rework the merge"}]`)

	// Act.
	if err := f.ingress.ApplyFile(context.Background(), path); err != nil {
		t.Fatalf("ApplyFile: %v", err)
	}

	// Assert.
	if len(f.verbs.calls) != 1 || f.verbs.calls[0].Spec.RepoDir != fixtureHome+"/.config/doom" {
		t.Fatalf("verb calls = %+v, want one create of %s/.config/doom", f.verbs.calls, fixtureHome)
	}
}

func TestApplyFileResolvesATildeProjectDir(t *testing.T) {
	// Arrange.
	f := newFixture(t)
	f.workspace("w1", fixtureHome+"/tree/w1")
	path := f.write(t, "workspace_commands_tilde_merge.json",
		`[{"type":"merge","workspace":"w1-name","project_dir":"~/tree/w1"}]`)

	// Act.
	if err := f.ingress.ApplyFile(context.Background(), path); err != nil {
		t.Fatalf("ApplyFile: %v", err)
	}

	// Assert.
	if len(f.merge.enqueued) != 1 || f.merge.enqueued[0] != "w1" {
		t.Fatalf("enqueued merges = %v, want w1", f.merge.enqueued)
	}
}

// TestApplyFileQuarantinesARelativeDirectory pins the refusal: a relative
// directory is never guessed against the working directory, so the whole file
// applies nothing, retires to quarantine, and the warning names the field.
func TestApplyFileQuarantinesARelativeDirectory(t *testing.T) {
	// Arrange: a valid merge first, so a partial apply would show.
	f := newFixture(t)
	f.workspace("w1", "/tree/w1")
	path := f.write(t, "workspace_commands_relative.json",
		`[{"type":"merge","project_dir":"/tree/w1"},{"type":"create","name":"x","git_root":".config/doom"}]`)

	// Act.
	err := f.ingress.ApplyFile(context.Background(), path)

	// Assert.
	if !errors.Is(err, ErrQuarantined) {
		t.Fatalf("ApplyFile = %v, want ErrQuarantined", err)
	}
	if len(f.merge.enqueued) != 0 || len(f.verbs.calls) != 0 {
		t.Fatalf("enqueued %v and called %v, want nothing applied", f.merge.enqueued, verbNames(f.verbs.calls))
	}
	if got := entries(t, filepath.Join(f.dir, "quarantine")); len(got) != 1 {
		t.Fatalf("quarantined files = %v, want exactly one", got)
	}
	var cause string
	for _, record := range f.log.logger.Records() {
		if record.Operation == opQuarantine && record.Level == "warn" {
			cause, _ = record.Context["cause"].(string)
		}
	}
	if !strings.Contains(cause, "entry 1: create: git_root: ") || !strings.Contains(cause, "not an absolute path") {
		t.Fatalf("quarantine warning cause = %q, want entry 1's git_root named as not absolute", cause)
	}
}
