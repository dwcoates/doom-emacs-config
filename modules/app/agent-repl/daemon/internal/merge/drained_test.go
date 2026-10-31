package merge

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"slices"
	"testing"
	"time"

	repoconfigv1 "agentrepl/proto/repoconfig/v1"

	"agent-repld/internal/deployconfig"
	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// The harness's queue_drained scripts.
const (
	releaseScript  = "bin/push-release.sh"
	announceScript = "bin/announce.sh"
)

// drainOwedHarness is a harness whose queue owes its drain, with its target
// registered as the repository whose queue it is and SCRIPTS (each bounded at
// 9s) as its config's queue_drained.
func drainOwedHarness(t *testing.T, scripts ...string) *harness {
	t.Helper()
	h := newHarness(t)
	h.registerRepo("repo-target", h.targetD)
	for _, path := range scripts {
		h.config.QueueDrained = append(h.config.QueueDrained, deployconfig.ScriptHook(path, 9).GetScript())
	}
	h.db.mu.Lock()
	h.db.drainOwed[h.repoKey()] = true
	h.db.mu.Unlock()
	return h
}

// drainScript is where the harness runs a queue_drained script from.
func (h *harness) drainScript(path string) string { return filepath.Join(h.targetD, path) }

// drainStillOwed reports whether the harness's queue still owes its drain.
func (h *harness) drainStillOwed() bool {
	h.db.mu.Lock()
	defer h.db.mu.Unlock()
	return h.db.drainOwed[h.repoKey()]
}

// scriptsRan names the scripts the harness's hook runner ran, in order.
func (h *harness) scriptsRan() []string {
	var out []string
	for _, r := range h.hooks.ran() {
		out = append(out, r.argv[len(r.argv)-1])
	}
	return out
}

// drainRecord finds the queue drain's record at LEVEL with MESSAGE.
func (h *harness) drainRecord(level, message string) (dlog.Record, bool) {
	return dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: level, Operation: opQueueDrained, Message: message})
}

func TestADepletedQueueRunsItsDrainScriptsInOrderAndClearsTheDebt(t *testing.T) {
	// Arrange
	h := drainOwedHarness(t, releaseScript, announceScript)

	// Act
	h.o.settleDrained(context.Background(), h.repoKey())

	// Assert
	runs := h.hooks.ran()
	if len(runs) != 2 || runs[0].dir != h.targetD || !slices.Equal(runs[0].argv, []string{"bash", h.drainScript(releaseScript)}) ||
		runs[0].timeout != 9*time.Second || runs[0].env != nil || runs[1].argv[1] != h.drainScript(announceScript) {
		t.Fatalf("runs = %+v, want both scripts from the repository root, in order, bounded and with no deploy environment", runs)
	}
	if h.drainStillOwed() {
		t.Fatal("the drain is still owed, want it cleared once every script succeeded")
	}
	if _, ok := h.drainRecord(dlog.LevelInfo, "the repository's queue_drained scripts all succeeded"); !ok {
		t.Fatalf("records = %+v, want the drain's success recorded", h.logs.Records())
	}
}

func TestADrainScriptsOutputIsKeptAmongTheQueuesLogs(t *testing.T) {
	// Arrange
	h := drainOwedHarness(t, releaseScript)
	h.hooks.outputs[h.drainScript(releaseScript)] = "pushed master\n"

	// Act
	h.o.settleDrained(context.Background(), h.repoKey())

	// Assert
	record, ok := h.drainRecord(dlog.LevelInfo, "a queue_drained script succeeded")
	if !ok {
		t.Fatalf("records = %+v, want the script's success recorded", h.logs.Records())
	}
	path, _ := record.Context["log"].(string)
	body, err := os.ReadFile(path)
	if filepath.Dir(path) != LogsDir(h.stateDir) || !IsLog(filepath.Base(path)) || err != nil || string(body) != "pushed master\n" {
		t.Fatalf("log %q = %q, %v; want the script's whole output among the queue's retained logs", path, body, err)
	}
}

func TestTheDrainReleasesTheRepositoryLockItTook(t *testing.T) {
	// Arrange
	h := drainOwedHarness(t, releaseScript)

	// Act
	h.o.settleDrained(context.Background(), h.repoKey())

	// Assert
	if !h.lockFree(t) {
		t.Fatal("the repository lock is still held after the drain")
	}
}

func TestTheDrainRunsNothingWhen(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
	}{
		{name: "nothing is owed", arrange: func(h *harness) {
			h.db.mu.Lock()
			h.db.drainOwed[h.repoKey()] = false
			h.db.mu.Unlock()
		}},
		{name: "the queue is paused", arrange: func(h *harness) {
			h.db.mu.Lock()
			h.db.paused[h.repoKey()] = true
			h.db.mu.Unlock()
		}},
		{name: "a merge is in the queue", arrange: func(h *harness) { enqueueWorkspace(h.t, h, theWorkspace) }},
		{name: "a run holds the slot", arrange: func(h *harness) {
			h.o.mu.Lock()
			h.o.running[h.repoKey()] = &run{}
			h.o.mu.Unlock()
		}},
		{name: "a run stands in the chain", arrange: func(h *harness) {
			h.o.mu.Lock()
			h.o.chains[h.repoKey()] = []*run{{}}
			h.o.mu.Unlock()
		}},
		{name: "the daemon is draining", arrange: func(h *harness) {
			h.o.mu.Lock()
			h.o.draining = true
			h.o.mu.Unlock()
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := drainOwedHarness(t, releaseScript)
			owedBefore := h.drainStillOwed()
			tt.arrange(h)
			owedArranged := h.drainStillOwed()

			// Act
			h.o.settleDrained(context.Background(), h.repoKey())

			// Assert
			if ran := h.scriptsRan(); len(ran) != 0 {
				t.Fatalf("scripts ran = %v, want none", ran)
			}
			if !owedBefore || h.drainStillOwed() != owedArranged {
				t.Fatalf("owed = %v, want the debt left as it stood (%v)", h.drainStillOwed(), owedArranged)
			}
		})
	}
}

func TestAConfigWithNoDrainScriptsClearsTheDebt(t *testing.T) {
	// Arrange
	h := drainOwedHarness(t)

	// Act
	h.o.settleDrained(context.Background(), h.repoKey())

	// Assert
	if ran := h.scriptsRan(); len(ran) != 0 || h.drainStillOwed() {
		t.Fatalf("scripts ran = %v, owed = %v; want nothing run and the debt cleared", ran, h.drainStillOwed())
	}
}

func TestAFailedDrainScriptStopsTheRestAndLeavesTheDebtOwed(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		failure string
	}{
		{name: "a non-zero exit", arrange: func(h *harness) { h.hooks.exits[h.drainScript(releaseScript)] = 1 }, failure: "it exited 1"},
		{name: "a timeout", arrange: func(h *harness) { h.hooks.timeouts[h.drainScript(releaseScript)] = true }, failure: "it ran past its 9s timeout"},
		{name: "a script that cannot be run", arrange: func(h *harness) { h.hooks.err = errors.New("no bash") }, failure: "no bash"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := drainOwedHarness(t, releaseScript, announceScript)
			tt.arrange(h)

			// Act
			h.o.settleDrained(context.Background(), h.repoKey())

			// Assert
			if ran := h.scriptsRan(); !slices.Equal(ran, []string{h.drainScript(releaseScript)}) {
				t.Fatalf("scripts ran = %v, want the failing script alone", ran)
			}
			if !h.drainStillOwed() {
				t.Fatal("the drain was cleared, want it still owed after a failure")
			}
			record, ok := h.drainRecord(dlog.LevelError, "a queue_drained script failed; the scripts after it do not run, and the drain stays owed")
			if !ok || record.Context["failure"] != tt.failure || record.Context["script"] != releaseScript ||
				record.Context["repo_root"] != h.targetD || record.Context["log"] == nil {
				t.Fatalf("records = %+v, want an ERROR naming the script, how it failed (%q), the root and the log", h.logs.Records(), tt.failure)
			}
			if !h.lockFree(t) {
				t.Fatal("the repository lock is still held after a failed drain")
			}
		})
	}
}

func TestADrainScriptWhoseLogCannotBeKeptFailsTheDrain(t *testing.T) {
	// Arrange: the queue's log directory is a file.
	h := drainOwedHarness(t, releaseScript)
	if err := os.WriteFile(LogsDir(h.stateDir), nil, 0o644); err != nil {
		t.Fatal(err)
	}

	// Act
	h.o.settleDrained(context.Background(), h.repoKey())

	// Assert
	if !h.drainStillOwed() {
		t.Fatal("the drain was cleared, want it still owed when a script's log could not be kept")
	}
	if _, ok := h.drainRecord(dlog.LevelError, "a queue_drained script failed; the scripts after it do not run, and the drain stays owed"); !ok {
		t.Fatalf("records = %+v, want the lost log recorded as the script's failure", h.logs.Records())
	}
}

func TestTheDrainRecordsEveryFailureToStartAndLeavesTheDebtOwed(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		message string
	}{
		{name: "the debt cannot be read", arrange: func(h *harness) { h.db.drainReadErr = errors.New("disk I/O error") },
			message: "could not read whether the repository owes its drain"},
		{name: "the pause cannot be read", arrange: func(h *harness) { h.db.pausedReadErr = errors.New("disk I/O error") },
			message: "could not read whether the repository's queue is paused"},
		{name: "the queue cannot be read", arrange: func(h *harness) { h.db.queueReadErr = errors.New("disk I/O error") },
			message: "could not read the repository's merge queue"},
		{name: "no registered repository has the queue", arrange: func(h *harness) {
			h.db.mu.Lock()
			h.db.repos = h.db.repos[:1]
			h.db.mu.Unlock()
		}, message: "could not resolve the repository whose queue owes its drain; the drain stays owed"},
		{name: "the deploy config cannot be read", arrange: func(h *harness) { h.configErr = errors.New("deploy.yaml: unknown field") },
			message: "could not read the deploy config of the repository whose queue owes its drain; the drain stays owed"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := drainOwedHarness(t, releaseScript)
			tt.arrange(h)

			// Act
			h.o.settleDrained(context.Background(), h.repoKey())

			// Assert
			if ran := h.scriptsRan(); len(ran) != 0 {
				t.Fatalf("scripts ran = %v, want none", ran)
			}
			record, ok := h.drainRecord(dlog.LevelError, tt.message)
			if !ok || record.Context["repo"] != string(h.repoKey()) || record.Context["error"] == nil {
				t.Fatalf("records = %+v, want an ERROR %q naming the queue and the cause", h.logs.Records(), tt.message)
			}
			h.db.drainReadErr = nil
			if !h.drainStillOwed() {
				t.Fatal("the drain was cleared, want it still owed")
			}
		})
	}
}

func TestAFailedClearIsRecordedAndLeavesTheDebtOwed(t *testing.T) {
	// Arrange
	h := drainOwedHarness(t, releaseScript)
	h.db.drainClearErr = errors.New("disk I/O error")

	// Act
	h.o.settleDrained(context.Background(), h.repoKey())

	// Assert
	record, ok := h.drainRecord(dlog.LevelError, "could not clear the repository's drain once its queue_drained scripts succeeded; the drain stays owed")
	if !ok || record.Context["error"] != "disk I/O error" || !h.drainStillOwed() {
		t.Fatalf("records = %+v, owed %v; want the failed clear recorded and the debt standing", h.logs.Records(), h.drainStillOwed())
	}
}

func TestAnotherDaemonHoldingTheQueueIsLeftTheDrain(t *testing.T) {
	// Arrange
	h := drainOwedHarness(t, releaseScript)
	lock, taken, err := acquireRepoLock(h.o.lockDir, string(h.repoKey()))
	if err != nil || !taken {
		t.Fatalf("taking the repository lock as another daemon: %v, %v", taken, err)
	}
	t.Cleanup(func() { _ = lock.Release() })

	// Act
	h.o.settleDrained(context.Background(), h.repoKey())

	// Assert
	if ran := h.scriptsRan(); len(ran) != 0 || !h.drainStillOwed() {
		t.Fatalf("scripts ran = %v, owed %v; want nothing run and the debt left to the lock's holder", ran, h.drainStillOwed())
	}
	if _, ok := h.drainRecord(dlog.LevelInfo, "another daemon holds the repository's merge queue; the drain is left to it"); !ok {
		t.Fatalf("records = %+v, want the yield recorded", h.logs.Records())
	}
}

func TestADaemonExitingOnceTheScriptsSucceededLeavesTheDebtToItsSuccessor(t *testing.T) {
	// Arrange: the daemon begins its exit while the script runs.
	h := drainOwedHarness(t, releaseScript)
	entered, release := h.hooks.hold(h.drainScript(releaseScript), "")
	go func() {
		<-entered
		h.o.mu.Lock()
		h.o.draining = true
		h.o.mu.Unlock()
		close(release)
	}()

	// Act
	h.o.settleDrained(context.Background(), h.repoKey())

	// Assert
	if !h.drainStillOwed() {
		t.Fatal("the drain was cleared by an exiting daemon, want it left owed")
	}
	if _, ok := h.drainRecord(dlog.LevelInfo, "the daemon began its exit once the queue_drained scripts succeeded; the drain stays owed to its successor"); !ok {
		t.Fatalf("records = %+v, want the exit recorded", h.logs.Records())
	}
}

func TestThePumpRunsTheDrainBeforeGoingIdleHoldingTheQueuesAdmissions(t *testing.T) {
	// Arrange: an asynchronous orchestrator, its drain script held.
	h := drainOwedHarness(t, releaseScript)
	h.o.async = true
	entered, release := h.hooks.hold(h.drainScript(releaseScript), "")

	// Act
	h.o.kick(h.repoKey())
	select {
	case <-entered:
	case <-time.After(waitBound):
		t.Fatal("the pump never ran the drain")
	}
	h.o.mu.Lock()
	pumping := h.o.pumping[h.repoKey()]
	h.o.mu.Unlock()
	close(release)
	select {
	case <-liveGone(h.o):
	case <-time.After(waitBound):
		t.Fatal("the pump never went idle after the drain")
	}

	// Assert
	if !pumping {
		t.Fatal("the pump went idle while the drain ran, want the queue's admissions held for it")
	}
	if h.drainStillOwed() {
		t.Fatal("the drain is still owed after the pump ran it")
	}
}

func TestAdmittingAMergeOwesTheDrainThePumpThenRuns(t *testing.T) {
	// Arrange: a merge that lands, then an empty queue.
	h := newHarness(t)
	h.registerRepo("repo-target", h.targetD)
	h.config.QueueDrained = []*repoconfigv1.DeployHookScript{deployconfig.ScriptHook(releaseScript, 9).GetScript()}
	h.landsCleanly("abc123")
	enqueueWorkspace(t, h, theWorkspace)

	// Act
	if _, err := h.o.pumpOnce(context.Background(), h.repoKey()); err != nil {
		t.Fatalf("pumpOnce: %v", err)
	}
	owed := h.drainStillOwed()
	h.o.settleDrained(context.Background(), h.repoKey())

	// Assert
	if !owed {
		t.Fatal("the landed merge left no drain owed")
	}
	if ran := h.scriptsRan(); !slices.Contains(ran, h.drainScript(releaseScript)) || h.drainStillOwed() {
		t.Fatalf("scripts ran = %v, owed %v; want the release pushed and the debt cleared", ran, h.drainStillOwed())
	}
}

func TestOfferOwedDrainsKicksEachOwingQueueNotAlreadyOffered(t *testing.T) {
	// Arrange: the target owes its drain, the harness's other repository does not.
	h := drainOwedHarness(t, releaseScript)
	var kicked []wsm.RepoKey
	h.o.onKick = func(repo wsm.RepoKey) { kicked = append(kicked, repo) }

	// Act
	err := h.o.offerOwedDrains(context.Background(), "daemon.merge.test", nil)
	already := kicked
	kicked = nil
	againErr := h.o.offerOwedDrains(context.Background(), "daemon.merge.test", []wsm.RepoKey{h.repoKey()})

	// Assert
	if err != nil || !slices.Equal(already, []wsm.RepoKey{h.repoKey()}) {
		t.Fatalf("offerOwedDrains = %v, kicked %v; want the owing queue alone", err, already)
	}
	if againErr != nil || kicked != nil {
		t.Fatalf("offerOwedDrains = %v, kicked %v; want an already offered queue left alone", againErr, kicked)
	}
}

func TestOfferOwedDrainsSurfacesAFailedRead(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
		message string
	}{
		{name: "the registry", arrange: func(h *harness) { h.db.reposErr = errors.New("database is closed") },
			message: "could not read the repository registry to offer the owed queue drains"},
		{name: "a debt", arrange: func(h *harness) { h.db.drainReadErr = errors.New("database is closed") },
			message: "could not read whether a repository owes its queue drain"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := drainOwedHarness(t, releaseScript)
			tt.arrange(h)

			// Act
			err := h.o.offerOwedDrains(context.Background(), "daemon.merge.test", nil)

			// Assert
			if err == nil {
				t.Fatal("offerOwedDrains succeeded, want the read's error")
			}
			if _, ok := dlog.FindRecord(h.logs.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.merge.test", Message: tt.message}); !ok {
				t.Fatalf("records = %+v, want an ERROR %q", h.logs.Records(), tt.message)
			}
		})
	}
}

func TestTheBootAndTheTakeoverOfferEveryOwedDrain(t *testing.T) {
	tests := []struct {
		name string
		call func(o *orchestrator) error
	}{
		{name: "the boot", call: func(o *orchestrator) error { return o.Recover(context.Background()) }},
		{name: "a successor's takeover", call: func(o *orchestrator) error { return o.ReviewQueues(context.Background()) }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: an empty queue owing its drain.
			h := drainOwedHarness(t, releaseScript)
			var kicked []wsm.RepoKey
			h.o.onKick = func(repo wsm.RepoKey) { kicked = append(kicked, repo) }

			// Act
			err := tt.call(h.o)

			// Assert
			if err != nil || !slices.Equal(kicked, []wsm.RepoKey{h.repoKey()}) {
				t.Fatalf("err = %v, kicked %v; want the owing queue offered once", err, kicked)
			}
		})
	}
}
