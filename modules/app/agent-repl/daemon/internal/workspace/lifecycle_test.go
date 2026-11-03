package workspace

import (
	"context"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	repoconfigv1 "agentrepl/proto/repoconfig/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/lifecycleconfig"
	"agent-repld/internal/scriptrunner"
)

// creationThread is the Slack message an emoji-ack create is dispatched for.
const creationThread = "https://acme.slack.com/archives/C1/p1730000000123456"

// emojiAckSpec is the standard create as the emoji-ack system makes it.
func emojiAckSpec(t *testing.T, f *fixture) CreateSpec {
	spec := standardSpec(t, f)
	spec.Originator, spec.SlackThread = lifecycleconfig.OriginatorEmojiAck, creationThread
	return spec
}

// creationHooks makes every repository's lifecycle config a creation stage
// of ENTRIES for user_or_workspace workspaces.
func (f *fixture) creationHooks(entries ...*repoconfigv1.HookEntry) {
	f.lifecycle.cfg = &repoconfigv1.LifecycleConfig{PreDeploy: &repoconfigv1.DeployPreDeployStage{},
		Creation: &repoconfigv1.LifecycleStageHooks{UserOrWorkspace: &repoconfigv1.LifecycleHookList{Hooks: entries}}}
}

// anyTargetEntry is HOOK, run for any target.
func anyTargetEntry(hook *repoconfigv1.Hook) *repoconfigv1.HookEntry {
	return &repoconfigv1.HookEntry{Hook: hook, RunsFor: &repoconfigv1.HookRunsFor{Scope: &repoconfigv1.HookRunsFor_AnyTarget{AnyTarget: &repoconfigv1.HookRunsForAnyTarget{}}}}
}

// initialPrompt is the text of the one initial prompt the create submitted.
func (f *fixture) initialPrompt(t *testing.T) string {
	t.Helper()
	if len(f.queue.submissions) != 1 {
		t.Fatalf("submissions = %+v, want the initial prompt alone", f.queue.submissions)
	}
	return f.queue.submissions[0].Said.GetContent().GetBlocks()[0].GetText().GetText()
}

func TestACreateRecordsWhoCreatedIt(t *testing.T) {
	tests := []struct {
		name string
		spec func(*testing.T, *fixture) CreateSpec
		want string
	}{
		{name: "the user or a workspace", spec: standardSpec, want: "user_or_workspace"},
		{name: "the emoji-ack system", spec: emojiAckSpec, want: "emoji_ack"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)

			// Act
			record, err := f.verbs.Create(context.Background(), tt.spec(t, f))

			// Assert
			if err != nil {
				t.Fatalf("Create: %v", err)
			}
			if got := f.db.originators[record.ID]; got != tt.want {
				t.Fatalf("originator = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestASlackOriginIsRecordedAndStoredAsAReferenceLink(t *testing.T) {
	// Arrange
	f := newFixture(t)

	// Act
	record, err := f.verbs.Create(context.Background(), emojiAckSpec(t, f))

	// Assert
	if err != nil {
		t.Fatalf("Create: %v", err)
	}
	if f.db.slackOrigins[record.ID] != creationThread {
		t.Fatalf("slack origin = %q, want the thread", f.db.slackOrigins[record.ID])
	}
	links := f.db.links[record.ID]
	if len(links) != 1 || links[0].URL != creationThread || links[0].Label != SlackOriginLabel || links[0].Owned {
		t.Fatalf("links = %+v, want the thread stored as a reference", links)
	}
}

func TestACreateSpecWithNoOriginatorIsRefused(t *testing.T) {
	tests := []struct {
		name string
		edit func(*CreateSpec)
		want string
	}{
		{name: "no originator", edit: func(s *CreateSpec) { s.Originator = 0 }, want: "names no originator"},
		{name: "emoji-ack with no Slack message", edit: func(s *CreateSpec) { s.Originator = lifecycleconfig.OriginatorEmojiAck }, want: "names the Slack message"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			spec := standardSpec(t, f)
			tt.edit(&spec)

			// Act
			_, err := f.verbs.Create(context.Background(), spec)

			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("Create = %v, want a refusal naming %q", err, tt.want)
			}
			if len(f.git.created) != 0 {
				t.Fatalf("worktrees = %+v, want nothing built", f.git.created)
			}
			if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opCreate}); !ok {
				t.Fatalf("records = %+v, want the spec's defect at ERROR", f.log.logger.Records())
			}
		})
	}
}

func TestAnEmojiAckCreateCarriesTheDefaultCreationPrompt(t *testing.T) {
	// Arrange: the repository states no lifecycle config.
	f := newFixture(t)

	// Act
	if _, err := f.verbs.Create(context.Background(), emojiAckSpec(t, f)); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert
	want, _ := lifecycleconfig.ExpandPrompt(lifecycleconfig.DefaultCreationPrompt, creationThread)
	if got := f.initialPrompt(t); got != "fix the login bug\n\n"+want {
		t.Fatalf("initial prompt = %q, want the commission then the default naming the thread", got)
	}
}

func TestAUserOrWorkspaceCreateCarriesNoDefaultCreationPrompt(t *testing.T) {
	// Arrange
	f := newFixture(t)

	// Act
	if _, err := f.verbs.Create(context.Background(), standardSpec(t, f)); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert
	if got := f.initialPrompt(t); got != "fix the login bug" {
		t.Fatalf("initial prompt = %q, want the commission alone", got)
	}
}

func TestARepositorysCreationPromptJoinsTheInitialPrompt(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.creationHooks(anyTargetEntry(lifecycleconfig.PromptHook("read NOTES.md first")))

	// Act
	if _, err := f.verbs.Create(context.Background(), standardSpec(t, f)); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert
	if got := f.initialPrompt(t); got != "fix the login bug\n\nread NOTES.md first" {
		t.Fatalf("initial prompt = %q, want the commission then the creation prompt, in one prompt", got)
	}
}

func TestAnEmojiAckOverrideReplacesTheDefaultCreationPrompt(t *testing.T) {
	// Arrange: the emoji-ack list set empty.
	f := newFixture(t)
	f.lifecycle.cfg = &repoconfigv1.LifecycleConfig{PreDeploy: &repoconfigv1.DeployPreDeployStage{},
		Creation: &repoconfigv1.LifecycleStageHooks{EmojiAck: &repoconfigv1.LifecycleHookList{}}}

	// Act
	if _, err := f.verbs.Create(context.Background(), emojiAckSpec(t, f)); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert
	if got := f.initialPrompt(t); got != "fix the login bug" {
		t.Fatalf("initial prompt = %q, want the commission alone", got)
	}
}

func TestACreationScriptRunsFromTheRepositoryRootWithItsEnvironment(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.creationHooks(anyTargetEntry(lifecycleconfig.ScriptHook("bin/setup.sh", 30)))
	spec := emojiAckSpec(t, f)
	f.lifecycle.cfg.Creation.EmojiAck = f.lifecycle.cfg.Creation.UserOrWorkspace

	// Act
	record, err := f.verbs.Create(context.Background(), spec)

	// Assert
	if err != nil {
		t.Fatalf("Create: %v", err)
	}
	if len(f.hooks.calls) != 1 {
		t.Fatalf("script runs = %+v, want one", f.hooks.calls)
	}
	run := f.hooks.calls[0]
	if run.dir != spec.RepoDir || !slices.Equal(run.argv, []string{"bash", filepath.Join(spec.RepoDir, "bin/setup.sh")}) {
		t.Fatalf("ran %v in %s, want the script from the repository root %s", run.argv, run.dir, spec.RepoDir)
	}
	for _, want := range []string{"AGENT_REPL_CALLING_WORKSPACE=" + record.Dir, lifecycleconfig.EnvHookStage + "=creation", lifecycleconfig.EnvSlackThread + "=" + creationThread} {
		if !slices.Contains(run.env, want) {
			t.Errorf("env = %v, want %s", run.env, want)
		}
	}
}

func TestAFailedCreationScriptUndoesTheCreate(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.creationHooks(anyTargetEntry(lifecycleconfig.ScriptHook("bin/setup.sh", 30)))
	f.hooks.runs = []scriptrunner.Run{{ExitCode: 2, Output: "no node\n"}}

	// Act
	_, err := f.verbs.Create(context.Background(), standardSpec(t, f))

	// Assert
	refusal, refused := AsRefusal(err)
	if !refused || refusal.Arm != ArmCreationHookFailed || refusal.Fields["path"] != "bin/setup.sh" || refusal.Fields["detail"] != "it exited 2" {
		t.Fatalf("Create = %v, want the creation_hook_failed refusal naming the script and its exit", err)
	}
	logPath, _ := refusal.Fields["log_path"].(string)
	if body, readErr := os.ReadFile(logPath); readErr != nil || string(body) != "no node\n" {
		t.Fatalf("log %q = %q, %v; want the script's whole output kept", logPath, body, readErr)
	}
	if len(f.git.nuked) != 1 {
		t.Fatalf("nuked = %+v, want the half-made workspace undone", f.git.nuked)
	}
	if len(f.fleet.started) != 0 || len(f.queue.submissions) != 0 {
		t.Fatalf("sessions %v, submissions %+v; want no session and no prompt for an undone create", f.fleet.started, f.queue.submissions)
	}
	record, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: opCreate, Message: "a creation script hook failed; the create is undone"})
	if !ok || record.Context["script"] != "bin/setup.sh" || record.Context["failure"] != "it exited 2" {
		t.Fatalf("records = %+v, want the failure at ERROR with the script and how it failed", f.log.logger.Records())
	}
	if _, callers := callerInputRefusal(err); callers {
		t.Fatal("a failed creation script is the repository's fault, which the main worktree investigates, not the caller's input")
	}
}

func TestAnUnusableLifecycleConfigRefusesTheCreateBeforeAnythingIsBuilt(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.lifecycle.err = &lifecycleconfig.InvalidError{Path: "/r/.agent-repl/lifecycle.yaml", Detail: "unknown field \"creatoin\""}

	// Act
	_, err := f.verbs.Create(context.Background(), standardSpec(t, f))

	// Assert
	refusal, refused := AsRefusal(err)
	if !refused || refusal.Arm != ArmLifecycleConfigInvalid || refusal.Fields["detail"] != "unknown field \"creatoin\"" {
		t.Fatalf("Create = %v, want lifecycle_config_invalid naming why", err)
	}
	if len(f.git.created) != 0 || len(f.headless.calls) != 0 {
		t.Fatalf("worktrees %+v, naming calls %d; want nothing built and nothing spent", f.git.created, len(f.headless.calls))
	}
}

func TestAPromptlessCreateRunsNoCreationPrompt(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.creationHooks(anyTargetEntry(lifecycleconfig.PromptHook("read NOTES.md first")))
	spec := standardSpec(t, f)
	spec.InitialPrompt, spec.Name = "", "promptless"

	// Act
	if _, err := f.verbs.Create(context.Background(), spec); err != nil {
		t.Fatalf("Create: %v", err)
	}

	// Assert
	if len(f.queue.submissions) != 0 {
		t.Fatalf("submissions = %+v, want none: a creation prompt joins an initial prompt and starts no turn", f.queue.submissions)
	}
	if _, ok := dlog.FindRecord(f.log.logger.Records(), dlog.RecordQuery{Level: dlog.LevelInfo, Operation: opCreate,
		Message: "created without an initial prompt; the creation stage's prompt hooks have no prompt to join, and are not run"}); !ok {
		t.Fatalf("records = %+v, want the skipped prompts recorded", f.log.logger.Records())
	}
}

func TestACreationPromptNamingTheThreadOfAWorkspaceWithNoneFailsTheCreate(t *testing.T) {
	// Arrange
	f := newFixture(t)
	f.creationHooks(anyTargetEntry(lifecycleconfig.PromptHook("reply in {{slack_thread}}")))

	// Act
	_, err := f.verbs.Create(context.Background(), standardSpec(t, f))

	// Assert
	if refusal, refused := AsRefusal(err); !refused || refusal.Arm != ArmCreationHookFailed {
		t.Fatalf("Create = %v, want creation_hook_failed", err)
	}
	if len(f.git.nuked) != 1 || len(f.queue.submissions) != 0 {
		t.Fatalf("nuked %+v, submissions %+v; want the create undone and nothing sent", f.git.nuked, f.queue.submissions)
	}
}

// TestTheDaemonNeverPostsToSlack pins that no production code of the daemon
// sends to Slack: what a thread is told is its repository's hooks' to say.
func TestTheDaemonNeverPostsToSlack(t *testing.T) {
	// Arrange
	daemon, err := filepath.Abs(filepath.Join("..", ".."))
	if err != nil {
		t.Fatal(err)
	}
	var posting []string

	// Act
	err = filepath.WalkDir(daemon, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		raw, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		for _, verb := range []string{`"slack", "send"`, `"slack", "react"`, "slack send", "slack react"} {
			if strings.Contains(string(raw), verb) && !strings.Contains(path, filepath.Join("internal", "lifecycleconfig")) {
				posting = append(posting, path)
				break
			}
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(posting) != 0 {
		t.Fatalf("Slack is posted to from %v, want nowhere in the daemon", posting)
	}
}
