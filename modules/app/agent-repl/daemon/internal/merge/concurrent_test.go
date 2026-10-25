package merge

import (
	"context"
	"testing"

	repoconfigv1 "agentrepl/proto/repoconfig/v1"

	"agent-repld/internal/deployconfig"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/footer"
)

// The scripts the concurrent configs name: the deploy stage's pull request,
// and the post-deploy update of the main checkout.
const (
	prScript     = "bin/open-pr.sh"
	updateScript = "bin/update-master.sh"
)

// concurrentThrough is a hooks-only deploy whose leading stages run
// concurrently through THROUGH: a script deploy hook and a script post-deploy
// hook, explanation-engine's shape.
func concurrentThrough(through string) *repoconfigv1.DeployConfig {
	cfg := &repoconfigv1.DeployConfig{
		PreDeploy:  &repoconfigv1.DeployPreDeployStage{},
		Deploy:     anyTarget(scriptHookOf(prScript)),
		PostDeploy: []*repoconfigv1.DeployHookEntry{anyTarget(scriptHookOf(updateScript))},
	}
	switch through {
	case "pre_deploy":
		cfg.Concurrent = &repoconfigv1.DeployConcurrentStages{Through: &repoconfigv1.DeployConcurrentStages_ThroughPreDeploy{
			ThroughPreDeploy: &repoconfigv1.DeployConcurrentThroughPreDeploy{}}}
	case "deploy":
		cfg.Concurrent = &repoconfigv1.DeployConcurrentStages{Through: &repoconfigv1.DeployConcurrentStages_ThroughDeploy{
			ThroughDeploy: &repoconfigv1.DeployConcurrentThroughDeploy{}}}
	}
	return cfg
}

// concurrentWorld is a harness whose repository runs pre_deploy and deploy
// concurrently, with the harness's merge at the head and ws-b queued behind
// it, and the idle seam a deploy waiting for its turn reports on.
type concurrentWorld struct {
	h    *harness
	idle chan ids.WorkspaceID
	dirs map[ids.WorkspaceID]string
}

const behind ids.WorkspaceID = "ws-b"

func newConcurrentWorld(t *testing.T) *concurrentWorld {
	t.Helper()
	h := newHarness(t)
	h.mu.Lock()
	h.config = concurrentThrough("deploy")
	h.mu.Unlock()
	w := &concurrentWorld{h: h, idle: make(chan ids.WorkspaceID, 64), dirs: map[ids.WorkspaceID]string{theWorkspace: h.sourceD}}
	h.o.onChainIdle = func(ws ids.WorkspaceID) { w.idle <- ws }
	enqueue(t, h)
	queueAnother(t, h, behind, "b")
	h.db.mu.Lock()
	w.dirs[behind] = h.db.workspaces[behind].Dir
	h.db.mu.Unlock()
	return w
}

// hold parks the next run of SCRIPT by WS's deploy.
func (w *concurrentWorld) hold(script string, ws ids.WorkspaceID) (entered, release chan struct{}) {
	return w.h.hooks.hold(w.h.scriptAt(script), w.dirs[ws])
}

// ranBy answers, in order, the workspaces whose deploys ran SCRIPT.
func (w *concurrentWorld) ranBy(script string) []ids.WorkspaceID {
	var out []ids.WorkspaceID
	for _, run := range w.h.hooks.ran() {
		if run.argv[len(run.argv)-1] != w.h.scriptAt(script) {
			continue
		}
		for ws, dir := range w.dirs {
			if callingWorkspace(run.env) == dir {
				out = append(out, ws)
			}
		}
	}
	return out
}

// awaitIdle waits, bounded, for WS to wait at a boundary.
func (w *concurrentWorld) awaitIdle(t *testing.T, ws ids.WorkspaceID) {
	t.Helper()
	receiveUntil(t, w.idle, string(ws)+" waiting for its turn", func(got ids.WorkspaceID) bool { return got == ws })
}

func TestTwoDeploysRunTheirConcurrentStageAtOnce(t *testing.T) {
	// Arrange: both deploys' pull-request hooks hold once they start.
	w := newConcurrentWorld(t)
	headIn, releaseHead := w.hold(prScript, theWorkspace)
	behindIn, releaseBehind := w.hold(prScript, behind)

	// Act
	done := admitAsync(w.h, context.Background())
	receiveWithin(t, headIn, "the head's deploy hook starting")
	receiveWithin(t, behindIn, "the deploy behind the head starting its deploy hook while the head's still runs")

	// Assert: both are inside their deploy stage at once; neither ran the
	// one-at-a-time stage.
	if got := w.ranBy(updateScript); len(got) != 0 {
		t.Fatalf("post-deploy ran for %v while both deploy hooks were still running", got)
	}
	close(releaseHead)
	close(releaseBehind)
	receiveWithin(t, done, "the head's deploy ending")
	w.h.admit(context.Background())
}

func TestTheOneAtATimeStageIsEnteredInQueueOrder(t *testing.T) {
	// Arrange: the deploy behind the head finishes its concurrent stage
	// first, while the head's pull request still waits.
	w := newConcurrentWorld(t)
	headIn, releaseHead := w.hold(prScript, theWorkspace)
	done := admitAsync(w.h, context.Background())
	receiveWithin(t, headIn, "the head's deploy hook starting")
	w.awaitIdle(t, behind)
	if got := w.ranBy(updateScript); len(got) != 0 {
		t.Fatalf("post-deploy ran for %v before the head's turn ended", got)
	}

	// Act: the head's pull request merges; the head runs its post-deploy and
	// ends, and the pump promotes the deploy behind it.
	close(releaseHead)
	receiveWithin(t, done, "the head's deploy ending")
	if err := w.h.admit(context.Background()); err != nil {
		t.Fatalf("the promoted deploy ended on: %v", err)
	}

	// Assert
	got := w.ranBy(updateScript)
	if len(got) != 2 || got[0] != theWorkspace || got[1] != behind {
		t.Fatalf("post-deploy ran for %v, want the head then %s, in queue order", got, behind)
	}
}

func TestADeployWaitingForItsTurnSaysHowManyAreAhead(t *testing.T) {
	// Arrange
	w := newConcurrentWorld(t)
	headIn, releaseHead := w.hold(prScript, theWorkspace)
	done := admitAsync(w.h, context.Background())
	receiveWithin(t, headIn, "the head's deploy hook starting")

	// Act
	w.awaitIdle(t, behind)
	facts := w.h.factsOf(behind)

	// Assert
	if facts.Step != footer.StepAwaitingTurn || facts.Ahead != 1 {
		t.Fatalf("facts = step %q ahead %d, want %q with 1 ahead", facts.Step, facts.Ahead, footer.StepAwaitingTurn)
	}
	if facts.Line.GetEnqueued() == nil {
		t.Fatalf("line = %v, want the deploy ahead and its step", facts.Line)
	}
	close(releaseHead)
	receiveWithin(t, done, "the head's deploy ending")
	w.h.admit(context.Background())
}

func TestAConcurrentChainHoldsEveryDeployInLine(t *testing.T) {
	// Arrange: a window the power source narrowed to the head alone, which a
	// racing chain would keep to.
	h := newHarness(t)
	h.o.window = 1
	head := &run{target: deployTarget{cfg: concurrentThrough("deploy")}}

	// Act
	got := h.o.windowFor(head)

	// Assert
	if got <= 1 {
		t.Fatalf("windowFor = %d, want every deploy in line", got)
	}
}

func TestANarrowedWindowEvictsNoMemberOfAConcurrentChain(t *testing.T) {
	// Arrange
	h := newHarness(t)
	h.o.window = 1
	cfg := concurrentThrough("deploy")
	repo := h.repoKey()
	h.o.mu.Lock()
	h.o.chains[repo] = []*run{
		{ws: theWorkspace, target: deployTarget{cfg: cfg}},
		{ws: behind, target: deployTarget{cfg: cfg}, speculative: true},
	}
	h.o.mu.Unlock()

	// Act
	got := h.o.beyondWindow()

	// Assert
	if len(got) != 0 {
		t.Fatalf("beyondWindow = %v, want none of a concurrent chain", got)
	}
}

func TestAConfigWithoutConcurrentStagesKeepsItsWindowAndItsChain(t *testing.T) {
	tests := []struct {
		name   string
		cfg    *repoconfigv1.DeployConfig
		chains bool
	}{
		{name: "a local merge races in the window, as agent-repl's does", cfg: localMergeConfig(), chains: true},
		{name: "hooks alone, every stage one at a time, form no chain", cfg: concurrentThrough(""), chains: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			h.o.window = 1
			head := &run{ws: theWorkspace, target: deployTarget{cfg: tt.cfg}}

			// Act
			window, chains := h.o.windowFor(head), h.o.chainsIn(h.repoKey(), head)

			// Assert
			if window != 1 || chains != tt.chains {
				t.Fatalf("windowFor = %d, chainsIn = %v; want the window in force (1) and %v", window, chains, tt.chains)
			}
		})
	}
}

func TestOnlyAOneAtATimeStageOfAConcurrentConfigWaitsForItsTurn(t *testing.T) {
	// A run standing behind its head, in no chain, would wait forever or fail
	// at a boundary it is held at; every boundary that passes answers nil at
	// once, which is what this asserts.
	tests := []struct {
		name  string
		cfg   *repoconfigv1.DeployConfig
		stage deployconfig.Stage
	}{
		{name: "pre_deploy of a config through deploy", cfg: concurrentThrough("deploy"), stage: deployconfig.StagePreDeploy},
		{name: "deploy of a config through deploy", cfg: concurrentThrough("deploy"), stage: deployconfig.StageDeploy},
		{name: "pre_deploy of a config through pre_deploy", cfg: concurrentThrough("pre_deploy"), stage: deployconfig.StagePreDeploy},
		{name: "post_deploy of a config with no concurrent stages", cfg: concurrentThrough(""), stage: deployconfig.StagePostDeploy},
		{name: "post_deploy of agent-repl's local merge", cfg: localMergeConfig(), stage: deployconfig.StagePostDeploy},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t)
			r := &run{o: h.o, ws: behind, repo: h.repoKey(), speculative: true, target: deployTarget{cfg: tt.cfg}}

			// Act
			err := r.awaitTurnFor(context.Background(), tt.stage)

			// Assert
			if err != nil {
				t.Fatalf("awaitTurnFor = %v, want it passed at once", err)
			}
		})
	}
}

func TestADeployBehindTheHeadInNoChainCannotTakeItsTurn(t *testing.T) {
	// Arrange
	h := newHarness(t)
	r := &run{o: h.o, ws: behind, repo: h.repoKey(), speculative: true, target: deployTarget{cfg: concurrentThrough("deploy")}}

	// Act
	err := r.awaitTurnFor(context.Background(), deployconfig.StagePostDeploy)

	// Assert
	if err == nil {
		t.Fatal("awaitTurnFor = nil, want the defect refused")
	}
	if !loggedLike(h, logLike{level: "error", op: "daemon.merge.concurrent", text: "stands in no chain"}) {
		t.Fatal("no ERROR record names the deploy that stands in no chain")
	}
}
