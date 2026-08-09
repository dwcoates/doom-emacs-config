package reload

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"
)

// THE LATCH THAT NEVER RE-ARMS.
//
// `Trigger.launched` is a once-per-process guard on self-redeploy, and its
// stated justification is that a launch's own success ends this process's
// lifetime (reload.go's NO-LOOP ARGUMENT). That is true when the deploy
// succeeds. It is exactly false when the deploy FAILS AFTER LAUNCHING.
//
// The launch is a spawn: `Launch` returns as soon as `cmd.Start` succeeds, and
// the deploy's own outcome arrives later, on the reap goroutine, which does
// nothing with it but write a line — "self-redeploy EXITED NON-ZERO without
// bouncing the daemon". The latch, set before that line could exist, stays set
// for the rest of the process's life. Every later self-merge is answered with
// "a redeploy launched earlier in this daemon's lifetime is still the live
// one", which is a claim that is now false: nothing is live, the deploy died,
// and the daemon is running superseded code with self-redeploy permanently
// disabled. The user's only signal is a log line at the moment nobody is
// watching.
//
// WHAT MUST BE TRUE. A deploy that FAILED is not a deploy that is still the
// live one. Its failure RE-ARMS the latch, so the next self-merge may try
// again. (The comment's own argument for holding the latch after a failure —
// "re-running on the next merge would only break again" — is an argument for
// making the failure loud, not for disabling the mechanism silently and
// forever. A merge the user performs after fixing the deploy path must be able
// to deploy.)
//
// WHY THIS TEST SPAWNS A REAL PROCESS. The failure being modelled is precisely
// the one that a `Launcher` returning an error does NOT model: the launch
// SUCCEEDS and the deploy dies afterwards. Only the production
// `DetachedScript`, over a script that genuinely exits non-zero, produces that
// sequence. The script is an inert fake — no test in this package ever runs the
// real bin/deploy-all.sh.

// countingScript writes a fake deploy script that appends a line to a witness
// file on every run and then exits with `code`. The witness is how RUNS are
// counted: the deploy is detached and outlives the call that started it, so the
// only honest evidence that a second one happened is one the child itself
// wrote.
func countingScript(t *testing.T, witness string, code int) string {
	t.Helper()
	path := filepath.Join(t.TempDir(), "deploy-all.sh")
	body := "echo run >> " + witness + "\nexit " + itoa(code) + "\n"
	if err := os.WriteFile(path, []byte("#!/bin/sh\n"+body), 0o755); err != nil {
		t.Fatalf("write counting script: %v", err)
	}
	return path
}

func itoa(n int) string {
	if n == 0 {
		return "0"
	}
	digits := ""
	for n > 0 {
		digits = string(rune('0'+n%10)) + digits
		n /= 10
	}
	return digits
}

// witnessRuns counts the lines the script has written so far.
func witnessRuns(t *testing.T, witness string) int {
	t.Helper()
	data, err := os.ReadFile(witness)
	if err != nil {
		if os.IsNotExist(err) {
			return 0
		}
		t.Fatalf("read deploy witness: %v", err)
	}
	return strings.Count(string(data), "run\n")
}

// awaitRuns waits until the witness records at least `want` runs. It is bounded:
// a redeploy that never happens must fail here by name, never hang the package.
func awaitRuns(t *testing.T, witness string, want int, what string) {
	t.Helper()
	deadline := time.Now().Add(10 * time.Second)
	for time.Now().Before(deadline) {
		if witnessRuns(t, witness) >= want {
			return
		}
	}
	t.Fatalf("the deploy script ran %d time(s), want at least %d: %s", witnessRuns(t, witness), want, what)
}

// awaitLog waits until the recorder has seen a record containing substr.
func awaitLog(t *testing.T, rec *recordingLogf, substr, what string) {
	t.Helper()
	deadline := time.Now().Add(10 * time.Second)
	for time.Now().Before(deadline) {
		if rec.contains(substr) {
			return
		}
	}
	t.Fatalf("no canonical record containing %q was written: %s", substr, what)
}

// TestAFailedDeployReArmsTheSelfRedeployLatch covers THE RE-ARM.
func TestAFailedDeployReArmsTheSelfRedeployLatch(t *testing.T) {
	// Arrange — a daemon whose own checkout has just taken a merge, and a
	// deploy script that launches cleanly and then dies.
	dir := mergedFixture(t, "feature", map[string]string{
		"modules/app/agent-repl/daemon/internal/reload/reload.go": "package reload\n",
	})
	witness := filepath.Join(t.TempDir(), "runs.txt")
	rec := &recordingLogf{}
	script, err := NewDetachedScript(ScriptConfig{
		Script: countingScript(t, witness, 3),
		Dir:    t.TempDir(),
		LogDir: filepath.Join(t.TempDir(), "reload"),
		Logf:   rec.logf,
	})
	if err != nil {
		t.Fatalf("NewDetachedScript: %v", err)
	}
	trigger := newTestTrigger(t, fixtureSelf(t, dir), script)

	// Act — the first self-merge launches a deploy that fails...
	if err := trigger.AfterMerged(context.Background(), selfMergeRequest(dir, "feature")); err != nil {
		t.Fatalf("first AfterMerged: %v", err)
	}
	awaitRuns(t, witness, 1, "the first self-merge must launch a redeploy at all")
	awaitLog(t, rec, "EXITED NON-ZERO",
		"the reaper is the only place a launched-then-failed deploy is observed, and the re-arm must be downstream of that observation")

	// ... and a later self-merge must be able to deploy again.
	commitOnBranch(t, dir, "second", "more daemon work", map[string]string{
		"modules/app/agent-repl/daemon/internal/reload/launch.go": "package reload\n",
	})
	cherryPickBranch(t, dir, "second")
	if err := trigger.AfterMerged(context.Background(), selfMergeRequest(dir, "second")); err != nil {
		t.Fatalf("second AfterMerged: %v", err)
	}

	// Assert.
	awaitRuns(t, witness, 2,
		"the redeploy launched by the first merge EXITED NON-ZERO without bouncing anything, so it is not 'still the live one'; a latch that stays held on a failed deploy disables self-redeploy for the rest of the daemon's life and leaves it running superseded code with a log line as the only trace")
}

// TestASucceededLaunchStillLatchesUntilItsDeployFails covers THE OTHER SIDE:
// re-arming on failure must not become re-arming on every merge. While a
// launched deploy is STILL RUNNING — the ordinary case, in which it is about to
// kill this process — a second self-merge must not start a competing one.
func TestASucceededLaunchStillLatchesUntilItsDeployFails(t *testing.T) {
	// Arrange — a deploy that launches and stays alive, as a real one does
	// right up to the moment it stops the daemon.
	dir := mergedFixture(t, "feature", map[string]string{
		"modules/app/agent-repl/daemon/internal/reload/reload.go": "package reload\n",
	})
	witness := filepath.Join(t.TempDir(), "runs.txt")
	release := filepath.Join(t.TempDir(), "release")
	path := filepath.Join(t.TempDir(), "deploy-all.sh")
	// The script parks until the test releases it, so "the deploy is still
	// running" is a state the test OWNS rather than a window it races.
	body := "echo run >> " + witness + "\nwhile [ ! -f " + release + " ]; do sleep 0.01; done\nexit 0\n"
	if err := os.WriteFile(path, []byte("#!/bin/sh\n"+body), 0o755); err != nil {
		t.Fatalf("write parking script: %v", err)
	}
	var releaseOnce sync.Once
	releaseDeploy := func() {
		releaseOnce.Do(func() { _ = os.WriteFile(release, []byte("go\n"), 0o644) })
	}
	t.Cleanup(releaseDeploy)
	script, err := NewDetachedScript(ScriptConfig{
		Script: path,
		Dir:    t.TempDir(),
		LogDir: filepath.Join(t.TempDir(), "reload"),
		Logf:   t.Logf,
	})
	if err != nil {
		t.Fatalf("NewDetachedScript: %v", err)
	}
	trigger := newTestTrigger(t, fixtureSelf(t, dir), script)

	// Act — one merge launches the parked deploy; a second arrives while it runs.
	if err := trigger.AfterMerged(context.Background(), selfMergeRequest(dir, "feature")); err != nil {
		t.Fatalf("first AfterMerged: %v", err)
	}
	awaitRuns(t, witness, 1, "the first self-merge must launch a redeploy at all")
	commitOnBranch(t, dir, "second", "more daemon work", map[string]string{
		"modules/app/agent-repl/daemon/internal/reload/launch.go": "package reload\n",
	})
	cherryPickBranch(t, dir, "second")
	if err := trigger.AfterMerged(context.Background(), selfMergeRequest(dir, "second")); err != nil {
		t.Fatalf("second AfterMerged: %v", err)
	}

	// Assert — the running deploy was not joined by a second one.
	if got := witnessRuns(t, witness); got != 1 {
		t.Fatalf("the deploy script ran %d times, want exactly 1: a redeploy that is still running IS the live one, and a second self-merge must not start a competing deploy on top of it", got)
	}
	releaseDeploy()
}
