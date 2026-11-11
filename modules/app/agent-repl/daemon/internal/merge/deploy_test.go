package merge

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
	repoconfigv1 "agentrepl/proto/repoconfig/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/lifecycleconfig"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/wsm"
)

// checkScript is the pre-deploy script hook the hooks-only configs name.
const checkScript = "bin/check.sh"

// hooksOnly makes the harness's repository's config a deploy of hooks alone:
// no local merge, and ENTRY as its one pre-deploy hook.
func (h *harness) hooksOnly(entry *repoconfigv1.HookEntry) {
	h.mu.Lock()
	defer h.mu.Unlock()
	h.config = &repoconfigv1.LifecycleConfig{PreDeploy: &repoconfigv1.DeployPreDeployStage{
		UserOrWorkspace: &repoconfigv1.LifecycleHookList{Hooks: []*repoconfigv1.HookEntry{entry}}}}
}

// scriptAt is the path the harness's runner sees a script of the repository
// as: under the repository's main checkout root.
func (h *harness) scriptAt(path string) string { return filepath.Join(h.targetD, path) }

func TestAScriptHookRunsFromTheRepositoryRootWithItsEnvironment(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.hooksOnly(anyTarget(scriptHookOf(checkScript)))
	enqueue(t, h)

	// Act
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the deploy failed: %v", err)
	}

	// Assert
	runs := h.hooks.ran()
	if len(runs) != 1 {
		t.Fatalf("script runs = %+v, want one", runs)
	}
	got := runs[0]
	if got.dir != h.targetD || !slices.Equal(got.argv, []string{"bash", h.scriptAt(checkScript)}) {
		t.Fatalf("ran %v in %s, want bash and the script, in the repository root %s", got.argv, got.dir, h.targetD)
	}
	for _, want := range []string{EnvCallingWorkspace + "=" + h.sourceD, lifecycleconfig.EnvHookStage + "=pre_deploy", EnvDeployDaemon + "=" + harnessSelfAddress} {
		if !slices.Contains(got.env, want) {
			t.Errorf("env = %v, want %s", got.env, want)
		}
	}
	if !slices.ContainsFunc(got.env, func(kv string) bool { v, ok := strings.CutPrefix(kv, EnvDeployLedger+"="); return ok && v != "" }) {
		t.Errorf("env = %v, want the deploy's ledger", got.env)
	}
	if facts, _ := h.o.Facts(theWorkspace); facts.State != StateMerged {
		t.Fatalf("the deploy is %q, want %q", facts.State, StateMerged)
	}
}

func TestASucceededScriptHooksTabSettlesWithItsExitAndTail(t *testing.T) {
	// Arrange: a script that writes more lines than the tab keeps.
	h := newHarness(t)
	h.hooksOnly(anyTarget(scriptHookOf(checkScript)))
	var lines []string
	for i := range scriptTailLines + 5 {
		lines = append(lines, fmt.Sprintf("line %d", i))
	}
	h.hooks.lines = map[string][]string{h.scriptAt(checkScript): lines}
	enqueue(t, h)

	// Act
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the deploy failed: %v", err)
	}

	// Assert
	tab := h.feed.lastTabOfKind(TabPreDeploy).GetHookScript()
	if tab.GetSettled().GetSucceeded() == nil || tab.GetExit().GetExited().GetStatus() != 0 {
		t.Fatalf("the tab = %v, want settled succeeded on exit 0", tab)
	}
	if tab.GetPath().GetText() != checkScript || !IsLog(filepath.Base(tab.GetLog().GetPath())) {
		t.Fatalf("the tab names %q and log %q, want the script and a retained log", tab.GetPath().GetText(), tab.GetLog().GetPath())
	}
	tail := tab.GetTail()
	if len(tail) != scriptTailLines || tail[0].GetText() != "line 5" || tail[len(tail)-1].GetText() != fmt.Sprintf("line %d", scriptTailLines+4) {
		t.Fatalf("the tail = %v, want the last %d lines", tail, scriptTailLines)
	}
}

func TestAFailedScriptHookFailsTheDeploy(t *testing.T) {
	tests := []struct {
		name        string
		arrange     func(h *harness)
		wantDetail  string
		wantLevel   string
		wantMessage string
		wantExit    func(*frontendv1.FeedMergeHookScriptExit) bool
	}{
		{
			name:        "a non-zero exit",
			arrange:     func(h *harness) { h.hooks.exits = map[string]int{h.scriptAt(checkScript): 3} },
			wantDetail:  "its pre-deploy script bin/check.sh exited 3",
			wantLevel:   dlog.LevelInfo,
			wantMessage: "a script hook exited non-zero; the deploy fails",
			wantExit:    func(e *frontendv1.FeedMergeHookScriptExit) bool { return e.GetExited().GetStatus() == 3 },
		},
		{
			name:        "a timeout",
			arrange:     func(h *harness) { h.hooks.timeouts = map[string]bool{h.scriptAt(checkScript): true} },
			wantDetail:  "its pre-deploy script bin/check.sh ran past its 30s timeout",
			wantLevel:   dlog.LevelInfo,
			wantMessage: "a script hook ran past its timeout and was killed; the deploy fails",
			wantExit:    func(e *frontendv1.FeedMergeHookScriptExit) bool { return e.GetTimedOut().GetTimeoutSeconds() == 30 },
		},
		{
			name:        "a script that cannot be run",
			arrange:     func(h *harness) { h.hooks.err = errors.New("fork failed") },
			wantDetail:  "its pre-deploy script bin/check.sh could not be run: fork failed",
			wantLevel:   dlog.LevelError,
			wantMessage: "a script hook could not be run; the deploy fails",
			wantExit:    func(e *frontendv1.FeedMergeHookScriptExit) bool { return e == nil },
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.hooksOnly(anyTarget(scriptHookOf(checkScript)))
			tt.arrange(h)
			enqueue(t, h)

			// Act
			_ = h.admit(context.Background())

			// Assert
			facts, _ := h.o.Facts(theWorkspace)
			if facts.State != StateFailed || facts.FailedArea != footer.FailedHook || !strings.Contains(facts.Detail, tt.wantDetail) {
				t.Fatalf("the deploy is %q in %q reading %q, want %q in %q reading %q",
					facts.State, facts.FailedArea, facts.Detail, StateFailed, footer.FailedHook, tt.wantDetail)
			}
			tab := h.feed.lastTabOfKind(TabPreDeploy).GetHookScript()
			if tab.GetSettled().GetFailed().GetSummary() != tt.wantDetail || !tt.wantExit(tab.GetExit()) {
				t.Fatalf("the tab = %v, want settled failed with its exit", tab)
			}
			record, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: tt.wantLevel, Operation: "daemon.merge.hook", Message: tt.wantMessage})
			if !ok || record.Context["script"] != checkScript || record.Context["stage"] != TabPreDeploy {
				t.Fatalf("daemon.merge.hook record = %+v (found %v), want %q naming the script and stage", record, ok, tt.wantMessage)
			}
		})
	}
}

// brokenLog is a hook log whose every write fails.
type brokenLog struct{}

func (brokenLog) Write([]byte) (int, error) { return 0, errors.New("disk full") }
func (brokenLog) Close() error              { return nil }

func TestAScriptHookWhoseLogCannotBeKeptFailsTheDeploy(t *testing.T) {
	tests := []struct {
		name string
		open func(string) (io.WriteCloser, error)
		want string
	}{
		{name: "a log that cannot be opened", open: func(string) (io.WriteCloser, error) { return nil, errors.New("no space") },
			want: "its pre-deploy script bin/check.sh could not keep a log: no space"},
		{name: "a log that cannot be written", open: func(string) (io.WriteCloser, error) { return brokenLog{}, nil },
			want: "its pre-deploy script bin/check.sh could not keep a log: disk full"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.hooksOnly(anyTarget(scriptHookOf(checkScript)))
			h.hooks.lines = map[string][]string{h.scriptAt(checkScript): {"working"}}
			h.o.openHookLog = tt.open
			enqueue(t, h)

			// Act
			_ = h.admit(context.Background())

			// Assert
			facts, _ := h.o.Facts(theWorkspace)
			if facts.State != StateFailed || !strings.Contains(facts.Detail, tt.want) {
				t.Fatalf("the deploy is %q reading %q, want failed reading %q", facts.State, facts.Detail, tt.want)
			}
			if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.merge.hook"}); !ok {
				t.Fatalf("no ERROR daemon.merge.hook record: %+v", h.logs.Records())
			}
		})
	}
}

func TestAMainOnlyHookIsSkippedForALandingInAParent(t *testing.T) {
	// Arrange: the target is a parent workspace's checkout, not the main one.
	h := newHarness(t)
	h.hooksOnly(mainOnly(scriptHookOf(checkScript)))
	h.git.mainWorktrees = map[string]string{h.targetD: t.TempDir()}
	enqueue(t, h)

	// Act
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the deploy failed: %v", err)
	}

	// Assert
	if runs := h.hooks.ran(); len(runs) != 0 {
		t.Fatalf("script runs = %+v, want none for a landing off main", runs)
	}
}

func TestAHeldRepositorySkipsTheDeployHookAndCountsTheSkip(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ownCheckout()
	h.db.deployHold = wsm.DeployHold{Held: true}
	h.landsCleanly("abc123def4567")
	h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
	h.gatePasses("daemon")
	enqueue(t, h)

	// Act
	if err := h.admit(context.Background()); err != nil {
		t.Fatalf("the deploy failed: %v", err)
	}

	// Assert: the local merge landed, the deploy hook did not run.
	if facts, _ := h.o.Facts(theWorkspace); facts.State != StateMerged {
		t.Fatalf("the deploy is %q, want %q", facts.State, StateMerged)
	}
	if runs := h.hooks.ran(); len(runs) != 0 {
		t.Fatalf("script runs = %+v, want none while held", runs)
	}
	h.db.mu.Lock()
	skipped := h.db.deployHold.Skipped
	h.db.mu.Unlock()
	if skipped != 1 {
		t.Fatalf("skipped = %d, want the one skip recorded", skipped)
	}
}

func TestAHeadExtendsItsChainOnlyOnceItKnowsItsDeploy(t *testing.T) {
	// Arrange: the admission's own chain extension is the one that follows
	// the head's session; a sourcescan finds it after takeSession.
	src, err := os.ReadFile("run.go")
	if err != nil {
		t.Fatal(err)
	}
	body := string(src)
	admit := body[strings.Index(body, "func (r *run) admit("):]
	admit = admit[:strings.Index(admit, "\n}\n")]

	// Assert
	take, extend := strings.Index(admit, "r.takeSession(ctx)"), strings.Index(admit, "o.extendChain(")
	if take < 0 || extend < 0 || extend < take {
		t.Fatalf("admit extends its chain at %d and takes its session at %d; want the extension after the session", extend, take)
	}
}

func TestEnqueueRefusesARepositoryWithNoUsableConfig(t *testing.T) {
	tests := []struct {
		name   string
		err    error
		arm    string
		fields map[string]any
	}{
		{name: "no config", err: &lifecycleconfig.MissingError{Path: "/repo/.agent-repl/lifecycle.yaml"}, arm: ArmNoDeployConfig,
			fields: map[string]any{"path": "/repo/.agent-repl/lifecycle.yaml"}},
		{name: "an unusable config", err: &lifecycleconfig.InvalidError{Path: "/repo/.agent-repl/lifecycle.yaml", Detail: "pre_deploy is required"}, arm: ArmDeployConfigInvalid,
			fields: map[string]any{"path": "/repo/.agent-repl/lifecycle.yaml", "detail": "pre_deploy is required"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.mu.Lock()
			h.configErr = tt.err
			h.mu.Unlock()

			// Act
			err := h.request(t, ownBranch, RequestedByUser)

			// Assert
			refusal, refused := Refused(err)
			if !refused || refusal.Arm != tt.arm {
				t.Fatalf("Enqueue = %v, want the %s refusal", err, tt.arm)
			}
			for k, v := range tt.fields {
				if refusal.Fields[k] != v {
					t.Errorf("refusal field %s = %v, want %v", k, refusal.Fields[k], v)
				}
			}
			if _, recorded := h.entryState(); recorded {
				t.Fatal("a refused request was recorded")
			}
			if _, logged := h.recordFor(dlog.LevelInfo, "daemon.merge.enqueue"); !logged {
				t.Fatal("the refusal was not recorded in the merge log")
			}
		})
	}
}

func TestEnqueueFailsLoudlyOnAConfigThatCannotBeRead(t *testing.T) {
	// Arrange: a read failure that is neither absence nor an unusable file.
	h := newHarness(t)
	h.mu.Lock()
	h.configErr = errors.New("disk on fire")
	h.mu.Unlock()

	// Act
	err := h.request(t, ownBranch, RequestedByUser)

	// Assert
	if _, refused := Refused(err); err == nil || refused || !strings.Contains(err.Error(), "disk on fire") {
		t.Fatalf("Enqueue = %v, want the read's own failure, not a refusal", err)
	}
	if _, recorded := h.entryState(); recorded {
		t.Fatal("a failed request was recorded")
	}
}

func TestRollOutLanding(t *testing.T) {
	t.Run("with no ledger it starts at once", func(t *testing.T) {
		// Arrange
		h := newHarness(t)

		// Act
		registered, err := h.o.RollOutLanding(context.Background(), "")

		// Assert
		h.rollout.mu.Lock()
		fired := h.rollout.fired
		h.rollout.mu.Unlock()
		if err != nil || registered || fired != 1 {
			t.Fatalf("RollOutLanding = (%v, %v) and fired %d, want started at once", registered, err, fired)
		}
	})
	t.Run("a ledger no running deploy holds is refused", func(t *testing.T) {
		// Arrange
		h := newHarness(t)

		// Act
		_, err := h.o.RollOutLanding(context.Background(), "00000000deadbeef")

		// Assert
		refusal, refused := Refused(err)
		if !refused || refusal.Arm != ArmUnknownDeploy {
			t.Fatalf("RollOutLanding = %v, want the %s refusal", err, ArmUnknownDeploy)
		}
		if _, logged := h.recordFor(dlog.LevelWarn, "daemon.merge.roll_out_landing"); !logged {
			t.Fatal("the refusal was not recorded")
		}
	})
	t.Run("a running deploy's ledger registers the rollout for its teardown", func(t *testing.T) {
		// Arrange: agent-repl's own deploy, whose script asks with its ledger.
		h := newHarness(t)
		h.ownCheckout()
		h.landsCleanly("abc123def4567")
		h.git.changed = []string{"modules/app/agent-repl/daemon/x.go"}
		h.gatePasses("daemon")
		enqueue(t, h)

		// Act
		if err := h.admit(context.Background()); err != nil {
			t.Fatalf("the deploy failed: %v", err)
		}

		// Assert
		h.rollout.mu.Lock()
		fired, leasesAtFire := h.rollout.fired, h.rollout.leasesAtFire
		h.rollout.mu.Unlock()
		if fired != 1 || leasesAtFire == 0 {
			t.Fatalf("fired %d with %d lease(s) released, want once, after the lease's release", fired, leasesAtFire)
		}
		if record, ok := h.recordFor(dlog.LevelInfo, "daemon.merge.roll_out_landing"); !ok ||
			record.Message != "registered the rollout of the deploy's landing; it fires at the deploy's teardown" {
			t.Fatalf("roll_out_landing record = %+v (found %v), want the registration", record, ok)
		}
	})
}

// emojiAckOwner makes the harness workspace one the Slack emoji-ack system
// created for theThread, with an empty finish stage so its deploy is put in
// line at once.
func (h *harness) emojiAckOwner() {
	h.db.setOriginator(theWorkspace, "emoji_ack", theThread)
	h.mu.Lock()
	defer h.mu.Unlock()
	h.config.Finish = &repoconfigv1.LifecycleStageHooks{EmojiAck: &repoconfigv1.LifecycleHookList{}}
}

func TestADeployRunsItsOwnersKindsHooks(t *testing.T) {
	const emojiScript, otherScript = "bin/emoji.sh", "bin/other.sh"
	tests := []struct {
		name  string
		emoji bool
		want  string
	}{
		{name: "an emoji-ack owner runs the emoji_ack list", emoji: true, want: emojiScript},
		{name: "any other owner runs the user_or_workspace list", emoji: false, want: otherScript},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.mu.Lock()
			h.config = &repoconfigv1.LifecycleConfig{PreDeploy: &repoconfigv1.DeployPreDeployStage{
				EmojiAck:        &repoconfigv1.LifecycleHookList{Hooks: []*repoconfigv1.HookEntry{anyTarget(scriptHookOf(emojiScript))}},
				UserOrWorkspace: &repoconfigv1.LifecycleHookList{Hooks: []*repoconfigv1.HookEntry{anyTarget(scriptHookOf(otherScript))}},
			}, PostDeploy: &repoconfigv1.LifecycleStageHooks{EmojiAck: &repoconfigv1.LifecycleHookList{}}}
			h.mu.Unlock()
			if tt.emoji {
				h.emojiAckOwner()
			}
			enqueue(t, h)

			// Act
			if err := h.admit(t.Context()); err != nil {
				t.Fatalf("the deploy failed: %v", err)
			}

			// Assert
			runs := h.hooks.ran()
			if len(runs) != 1 || !slices.Equal(runs[0].argv, []string{"bash", h.scriptAt(tt.want)}) {
				t.Fatalf("script runs = %+v, want %s alone", runs, tt.want)
			}
		})
	}
}

func TestAnEmojiAckOwnersPostDeployDefaultReactsMerged(t *testing.T) {
	// Arrange: no post_deploy stage, so the emoji-ack default runs.
	h := newHarness(t)
	h.hooksOnly(anyTarget(promptHookOf("check")))
	h.emojiAckOwner()
	enqueue(t, h)

	// Act
	if err := h.admit(t.Context()); err != nil {
		t.Fatalf("the deploy failed: %v", err)
	}

	// Assert
	runs := h.hooks.ran()
	script := filepath.Join(harnessCheckoutRoot, lifecycleconfig.DefaultPostDeployScript)
	if len(runs) != 1 || !slices.Equal(runs[0].argv, []string{"bash", script}) || runs[0].dir != h.targetD {
		t.Fatalf("script runs = %+v, want the built-in reaction script %s, from the repository root", runs, script)
	}
	if !slices.Contains(runs[0].env, lifecycleconfig.EnvSlackThread+"="+theThread) || !slices.Contains(runs[0].env, lifecycleconfig.EnvHookStage+"=post_deploy") {
		t.Fatalf("env = %v, want the originating thread and the stage", runs[0].env)
	}
}

func TestADeployPromptExpandsItsOwnersThread(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.hooksOnly(anyTarget(promptHookOf("tell {{slack_thread}} the checks passed")))
	h.mu.Lock()
	h.config.PreDeploy.EmojiAck = h.config.PreDeploy.UserOrWorkspace
	h.config.PostDeploy = &repoconfigv1.LifecycleStageHooks{EmojiAck: &repoconfigv1.LifecycleHookList{}}
	h.mu.Unlock()
	h.emojiAckOwner()
	enqueue(t, h)

	// Act
	if err := h.admit(t.Context()); err != nil {
		t.Fatalf("the deploy failed: %v", err)
	}

	// Assert
	h.queue.mu.Lock()
	defer h.queue.mu.Unlock()
	if len(h.queue.submissions) != 1 || h.queue.submissions[0].Said.GetContent().GetBlocks()[0].GetText().GetText() != "tell "+theThread+" the checks passed" {
		t.Fatalf("submissions = %+v, want the prompt with its thread", h.queue.submissions)
	}
}

func TestADeployPromptNamingTheThreadOfAWorkspaceWithNoneFailsTheDeploy(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.hooksOnly(anyTarget(promptHookOf("tell {{slack_thread}} the checks passed")))
	enqueue(t, h)

	// Act
	_ = h.admit(t.Context())

	// Assert
	if facts, _ := h.o.Facts(theWorkspace); facts.State != StateFailed {
		t.Fatalf("the deploy is %q, want %q", facts.State, StateFailed)
	}
	if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.merge.hook"}); !ok {
		t.Fatalf("no ERROR record of the unresolvable placeholder: %+v", h.logs.Records())
	}
	h.queue.mu.Lock()
	defer h.queue.mu.Unlock()
	if len(h.queue.submissions) != 0 {
		t.Fatalf("submissions = %+v, want none sent", h.queue.submissions)
	}
}
