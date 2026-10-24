//go:build integration

package integration

import (
	"strings"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/integration/harness"

	"connectrpc.com/connect"
)

// deploy_concurrent_test.go drives a repository whose deploy runs its leading
// stages concurrently (repoconfig.v1.DeployConfig.concurrent), end to end:
// two deploys queued in one repository, each with a prompt deploy hook that
// waits on something outside the daemon (the fake shim holds the turn open),
// and a prompt post-deploy hook run one deploy at a time.

// concurrentDeploy is explanation-engine's shape: a deploy hook that opens a
// pull request and waits for it to merge, run concurrently, and a post-deploy
// hook that updates the main checkout, run one deploy at a time.
const concurrentDeploy = `pre_deploy: {}
concurrent:
  through_deploy: {}
deploy:
  hook:
    prompt:
      text: open the pull request and wait for it to merge
  runs_for:
    any_target: {}
post_deploy:
  - hook:
      prompt:
        text: update the main checkout
    runs_for:
      any_target: {}
`

func TestConcurrentDeploysRunTheirDeployHooksTogetherAndPostDeployInQueueOrder(t *testing.T) {
	t.Parallel()
	// Arrange: two workspaces in a repository whose deploy runs pre_deploy
	// and deploy concurrently.
	repo := harness.NewRepo(t)
	repo.WriteDeployConfig(concurrentDeploy)
	d := harness.StartDaemon(t, harness.Opts{Timeout: harness.MergeChainTimeout})
	repoRef := mergeRepositoryRef(t, d, repo)
	first := mergeCreateChild(t, d, repoRef, "concurrentfirst", "do the first thing", nil)
	second := mergeCreateChild(t, d, repoRef, "concurrentsecond", "do the second thing", nil)
	roster := d.WatchRoster()

	// Act: both finish, the first ahead of the second in the queue.
	for _, f := range []*fixture{first, second} {
		harness.CommitWork(t, f.ws.GetDir())
		if _, err := d.Client().FinishWorkspace(d.Ctx(), connect.NewRequest(harness.FinishRequest(f.ws))); err != nil {
			t.Fatalf("FinishWorkspace(%s) = error %v, want the deploy enqueued", f.ws.GetDir(), err)
		}
	}

	// Assert: both deploy hooks run at once; neither has concluded.
	for _, f := range []*fixture{first, second} {
		f.shim.ExpectStartTurn()
		d.AwaitWorkspaceLogOperationCount(f.ws.GetDir(), harness.OpTurnOpened, 2)
	}

	// Act: the second deploy's pull request merges first.
	pushConcludedTurn(second.shim, mainAgent, "second-deploy")

	// Assert: the second deploy waits for its turn, and runs no post-deploy
	// while the first deploy's hook still runs.
	d.AwaitWorkspaceLogRecord(second.ws.GetDir(), "the second deploy waiting for its turn", func(r harness.LogRecord) bool {
		return r.Operation == "daemon.merge.concurrent" && strings.Contains(r.Message, "it waits for the deploys ahead")
	})
	if got := second.shim.Count(harness.RPCStartTurn); got != 2 {
		t.Fatalf("the second deploy's turns = %d, want its initial and deploy hook turns alone while the first's deploy runs", got)
	}

	// Act: the first deploy's pull request merges; it runs its post-deploy
	// and lands.
	pushConcludedTurn(first.shim, mainAgent, "first-deploy")
	first.shim.ExpectStartTurn()
	d.AwaitWorkspaceLogOperationCount(first.ws.GetDir(), harness.OpTurnOpened, 3)
	pushConcludedTurn(first.shim, mainAgent, "first-post-deploy")
	awaitRoster(t, d, roster, "the first deploy merged", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, first.ws.GetId())
		return row == nil || row.GetMerged() != nil
	})

	// Assert: only then does the second deploy take its turn and land.
	second.shim.ExpectStartTurn()
	d.AwaitWorkspaceLogOperationCount(second.ws.GetDir(), harness.OpTurnOpened, 3)
	pushConcludedTurn(second.shim, mainAgent, "second-post-deploy")
	awaitRoster(t, d, roster, "the second deploy merged", func(r *frontendv1.WorkspaceRoster) bool {
		row := rosterRow(r, second.ws.GetId())
		return row == nil || row.GetMerged() != nil
	})
}
