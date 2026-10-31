package merge

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"strings"
	"testing"
	"time"

	repoconfigv1 "agentrepl/proto/repoconfig/v1"

	"agent-repld/internal/deployconfig"
	"agent-repld/internal/dlog"
	"agent-repld/internal/scriptrunner"
	"agent-repld/internal/wsm"
)

// theScope names the harness's registered repository.
var theScope = RepositoryScope{ID: theRepository}

// heldWith makes the harness's repository held, with SKIPPED deploys skipped.
func (h *harness) heldWith(skipped int) {
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	h.db.deployHold = wsm.DeployHold{Held: true, Since: time.Unix(1, 0), Skipped: skipped}
}

// deployHoldNow answers the harness's repository's hold as stored.
func (h *harness) deployHoldNow() wsm.DeployHold {
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	return h.db.deployHold
}

func TestHoldDeploysHoldsAFreeRepository(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	err := h.o.HoldDeploys(context.Background(), theScope)

	// Assert
	if err != nil || !h.deployHoldNow().Held {
		t.Fatalf("HoldDeploys = %v with hold %+v, want the repository held", err, h.deployHoldNow())
	}
	if record, ok := h.recordFor(dlog.LevelInfo, opHoldDeploys); !ok || record.Context["repository"] != h.repoRoot {
		t.Fatalf("hold record = %+v (found %v), want the repository named", record, ok)
	}
}

func TestHoldDeploysRefusesAHeldRepositoryAndKeepsItsHold(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.heldWith(2)

	// Act
	err := h.o.HoldDeploys(context.Background(), theScope)

	// Assert
	if refusal, refused := Refused(err); !refused || refusal.Arm != ArmDeploysAlreadyHeld {
		t.Fatalf("HoldDeploys = %v, want the %s refusal", err, ArmDeploysAlreadyHeld)
	}
	if hold := h.deployHoldNow(); !hold.Held || hold.Skipped != 2 || !hold.Since.Equal(time.Unix(1, 0)) {
		t.Fatalf("hold = %+v, want the first hold untouched", hold)
	}
}

func TestHoldAndReleaseFailLoudlyOnTheStore(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		act     func(h *harness) error
		op      string
		message string
	}{
		{name: "a hold whose read fails", arrange: func(h *harness) { h.db.deployHoldErr = errors.New("db gone") },
			act: func(h *harness) error { return h.o.HoldDeploys(context.Background(), theScope) },
			op:  opHoldDeploys, message: "could not read the repository's deploy hold"},
		{name: "a release whose read fails", arrange: func(h *harness) { h.db.deployHoldErr = errors.New("db gone") },
			act: func(h *harness) error { _, err := h.o.ReleaseDeploys(context.Background(), theScope); return err },
			op:  opReleaseDeploys, message: "could not read the repository's deploy hold"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tt.arrange(h)

			// Act
			err := tt.act(h)

			// Assert
			if _, refused := Refused(err); err == nil || refused {
				t.Fatalf("err = %v, want the store's failure", err)
			}
			if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: tt.op, Message: tt.message}); !ok {
				t.Fatalf("no ERROR %s record %q: %+v", tt.op, tt.message, h.logs.Records())
			}
		})
	}
}

func TestReleaseDeploysRefusesARepositoryThatIsNotHeld(t *testing.T) {
	// Arrange
	h := newHarness(t)

	// Act
	_, err := h.o.ReleaseDeploys(context.Background(), theScope)

	// Assert
	if refusal, refused := Refused(err); !refused || refusal.Arm != ArmDeploysNotHeld {
		t.Fatalf("ReleaseDeploys = %v, want the %s refusal", err, ArmDeploysNotHeld)
	}
}

func TestReleaseDeploysAnswersWhatRanForTheSkippedDeploys(t *testing.T) {
	scriptPath := deployScript
	tests := []struct {
		name       string
		skipped    int
		arrange    func(h *harness)
		wantKind   DeployReleaseKind
		wantRuns   int
		wantDetail string
	}{
		{name: "nothing skipped", skipped: 0, arrange: func(h *harness) { h.ownCheckout() }, wantKind: ReleaseNothingSkipped},
		{name: "a script deploy hook that succeeds", skipped: 2, arrange: func(h *harness) { h.ownCheckout() }, wantKind: ReleaseScriptRan, wantRuns: 1},
		{name: "a script deploy hook that fails", skipped: 1, arrange: func(h *harness) {
			h.ownCheckout()
			h.hooks.exits = map[string]int{filepath.Join(h.repoRoot, scriptPath): 4}
		}, wantKind: ReleaseScriptFailed, wantRuns: 1, wantDetail: "it exited 4"},
		{name: "a script deploy hook that times out", skipped: 1, arrange: func(h *harness) {
			h.ownCheckout()
			h.hooks.timeouts = map[string]bool{filepath.Join(h.repoRoot, scriptPath): true}
		}, wantKind: ReleaseScriptFailed, wantRuns: 1, wantDetail: "it ran past its 30s timeout"},
		{name: "a prompt deploy hook", skipped: 1, arrange: func(h *harness) {
			h.emacsRepo()
			h.mu.Lock()
			h.config.Deploy = mainOnly(promptHookOf("open the pull request"))
			h.mu.Unlock()
		}, wantKind: ReleasePromptNotRun},
		{name: "no deploy hook", skipped: 1, arrange: func(h *harness) { h.emacsRepo() }, wantKind: ReleaseNoDeployHook,
			wantDetail: "has no deploy hook for its main branch"},
		{name: "a config that cannot be read", skipped: 1, arrange: func(h *harness) {
			h.mu.Lock()
			h.configErr = &deployconfig.InvalidError{Path: "deploy.yaml", Detail: "broken"}
			h.mu.Unlock()
		}, wantKind: ReleaseNoDeployHook, wantDetail: "broken"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			tt.arrange(h)
			h.heldWith(tt.skipped)

			// Act
			release, err := h.o.ReleaseDeploys(context.Background(), theScope)

			// Assert
			if err != nil {
				t.Fatalf("ReleaseDeploys: %v", err)
			}
			if release.Kind != tt.wantKind || release.Skipped != tt.skipped || !strings.Contains(release.Detail, tt.wantDetail) {
				t.Fatalf("release = %+v, want kind %v, %d skipped, detail naming %q", release, tt.wantKind, tt.skipped, tt.wantDetail)
			}
			if h.deployHoldNow().Held {
				t.Fatal("the repository is still held after its release")
			}
			if runs := h.hooks.ran(); len(runs) != tt.wantRuns {
				t.Fatalf("script runs = %+v, want %d", runs, tt.wantRuns)
			}
		})
	}
}

func TestAReleasedScriptRunsWithNoOwnerAndKeepsARetainedLog(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.ownCheckout()
	h.heldWith(1)

	// Act
	release, err := h.o.ReleaseDeploys(context.Background(), theScope)

	// Assert
	if err != nil || release.Kind != ReleaseScriptRan {
		t.Fatalf("ReleaseDeploys = (%+v, %v), want the script run", release, err)
	}
	runs := h.hooks.ran()
	if len(runs) != 1 || runs[0].dir != h.repoRoot {
		t.Fatalf("script runs = %+v, want one in the repository root %s", runs, h.repoRoot)
	}
	if !slices.Equal(runs[0].env, []string{EnvDeployStage + "=deploy"}) {
		t.Fatalf("env = %v, want the stage alone: a release has no calling workspace and no ledger", runs[0].env)
	}
	if release.Path != deployScript || filepath.Dir(release.LogPath) != LogsDir(h.o.deps.StateDir) || !IsLog(filepath.Base(release.LogPath)) {
		t.Fatalf("release = %+v, want the script and a retained log", release)
	}
	h.rollout.mu.Lock()
	fired := h.rollout.fired
	h.rollout.mu.Unlock()
	if fired != 1 {
		t.Fatalf("the rollout fired %d times, want once: the script asked with no deploy named", fired)
	}
}

// TestAReleaseReadsTheDeployHooksScope covers an any-target deploy hook, which
// runs for the main branch a release deploys.
func TestAReleaseReadsTheDeployHooksScope(t *testing.T) {
	// Arrange: a deploy hook for any target, which also runs for main.
	h := newHarness(t)
	h.emacsRepo()
	h.mu.Lock()
	h.config.Deploy = &repoconfigv1.DeployHookEntry{Hook: scriptHookOf(deployScript), RunsFor: &repoconfigv1.DeployHookRunsFor{
		Scope: &repoconfigv1.DeployHookRunsFor_AnyTarget{AnyTarget: &repoconfigv1.DeployHookRunsForAnyTarget{}}}}
	h.mu.Unlock()
	h.heldWith(1)

	// Act
	release, err := h.o.ReleaseDeploys(context.Background(), theScope)

	// Assert
	if err != nil || release.Kind != ReleaseScriptRan {
		t.Fatalf("ReleaseDeploys = (%+v, %v), want an any-target deploy hook run", release, err)
	}
}

func TestOwnerlessRunFailureNamesHowTheScriptEnded(t *testing.T) {
	script := deployconfig.ScriptHook("bin/x.sh", 7).GetScript()
	tests := []struct {
		name string
		ran  ownerlessRun
		want string
	}{
		{name: "an exit of zero", ran: ownerlessRun{run: scriptrunner.Run{ExitCode: 0}}, want: ""},
		{name: "a script that could not be run", ran: ownerlessRun{err: errors.New("no bash")}, want: "no bash"},
		{name: "a script killed at its timeout", ran: ownerlessRun{run: scriptrunner.Run{ExitCode: -1, TimedOut: true}}, want: "it ran past its 7s timeout"},
		{name: "a non-zero exit", ran: ownerlessRun{run: scriptrunner.Run{ExitCode: 3}}, want: "it exited 3"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := tt.ran.failure(script)

			// Assert
			if got != tt.want {
				t.Fatalf("failure = %q, want %q", got, tt.want)
			}
		})
	}
}

func TestRunOwnerlessScriptRunsTheScriptFromTheRepositoryBoundedByItsTimeout(t *testing.T) {
	// Arrange
	h := newHarness(t)
	script := deployconfig.ScriptHook("bin/x.sh", 7).GetScript()

	// Act
	h.o.runOwnerlessScript(context.Background(), h.repoRoot, script, []string{"A=b"}, filepath.Join(t.TempDir(), "x.log"))

	// Assert
	runs := h.hooks.ran()
	want := hookRun{dir: h.repoRoot, argv: []string{"bash", filepath.Join(h.repoRoot, "bin/x.sh")}, env: []string{"A=b"}, timeout: 7 * time.Second}
	if len(runs) != 1 || runs[0].dir != want.dir || !slices.Equal(runs[0].argv, want.argv) || !slices.Equal(runs[0].env, want.env) || runs[0].timeout != want.timeout {
		t.Fatalf("runs = %+v, want one %+v", runs, want)
	}
}

func TestRunOwnerlessScriptKeepsTheScriptsWholeOutput(t *testing.T) {
	// Arrange
	h := newHarness(t)
	script := deployconfig.ScriptHook("bin/x.sh", 7).GetScript()
	h.hooks.outputs[filepath.Join(h.repoRoot, "bin/x.sh")] = "one\ntwo\n"
	path := filepath.Join(t.TempDir(), "x.log")

	// Act
	ran := h.o.runOwnerlessScript(context.Background(), h.repoRoot, script, nil, path)

	// Assert
	body, err := os.ReadFile(path)
	if ran.logErr != nil || err != nil || string(body) != "one\ntwo\n" {
		t.Fatalf("log = %q, %v (log error %v), want the script's whole output", body, err, ran.logErr)
	}
}

func TestRunOwnerlessScriptReportsALogThatCannotBeWritten(t *testing.T) {
	// Arrange: the log's directory is a file.
	h := newHarness(t)
	script := deployconfig.ScriptHook("bin/x.sh", 7).GetScript()
	blocker := filepath.Join(t.TempDir(), "file")
	if err := os.WriteFile(blocker, nil, 0o644); err != nil {
		t.Fatal(err)
	}

	// Act
	ran := h.o.runOwnerlessScript(context.Background(), h.repoRoot, script, nil, filepath.Join(blocker, "x.log"))

	// Assert
	if ran.logErr == nil || ran.err != nil || ran.failure(script) != "" {
		t.Fatalf("run = %+v, want a script that ran and a log error", ran)
	}
}

func TestEveryOwnerlessScriptRunsThroughRunOwnerlessScript(t *testing.T) {
	// Arrange: a deploy's own hooks run in deploy.go with their owner; every
	// other script run is runOwnerlessScript's.
	entries, err := os.ReadDir(".")
	if err != nil {
		t.Fatal(err)
	}
	calls := map[string]int{}

	// Act
	for _, entry := range entries {
		name := entry.Name()
		if !strings.HasSuffix(name, ".go") || strings.HasSuffix(name, "_test.go") {
			continue
		}
		raw, err := os.ReadFile(name)
		if err != nil {
			t.Fatal(err)
		}
		if n := strings.Count(string(raw), "HookRunner.RunLinesWith("); n > 0 {
			calls[name] = n
		}
	}

	// Assert
	if len(calls) != 2 || calls["deploy.go"] != 1 || calls["deployhold.go"] != 1 {
		t.Fatalf("HookRunner.RunLinesWith is called in %v, want once in deploy.go (a deploy's own hooks) and once in deployhold.go (runOwnerlessScript)", calls)
	}
}
