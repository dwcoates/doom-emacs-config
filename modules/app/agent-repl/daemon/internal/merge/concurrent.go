package merge

import (
	"context"
	"errors"
	"math"

	"agent-repld/internal/deployconfig"
	"agent-repld/internal/dlog"
	"agent-repld/internal/resolve/footer"
	"agent-repld/internal/wsm"
)

// This file is a deploy's CONCURRENT STAGES (repoconfig.v1.DeployConfig
// .concurrent): the leading run of stages that the deploys waiting in one
// repository's queue run side by side, ahead of their turn, before each runs
// the rest one at a time, in queue order.
//
// THE CHAIN CARRIES THEM (chain.go). A queue whose head's config has
// concurrent stages chains every deploy waiting in line behind the head, with
// no window: what those deploys run ahead of their turn is hooks, never the
// tests a racing chain's window rations. Each member runs its concurrent
// stages as a speculative run does its rebase, and at the first one-at-a-time
// stage it waits for its turn (awaitTurn).
//
// ORDER IS THE CHAIN'S, AND SO STRUCTURAL. Only the pump promotes, and only
// the chain's first run once its entry stands first in line (promote): a
// deploy whose concurrent stages finished first still enters the
// one-at-a-time stages after every deploy queued ahead of it. A member waits
// on the chain's own signal, closed under the orchestrator's mutex at every
// change, so a promotion is never missed and nothing polls.
//
// A LOCAL MERGE IS NEVER CONCURRENT (deployconfig refuses the combination):
// it lands on the target, which only the head moves. A config with a local
// merge runs as it always has: its chain races (racesIn), within the window.

// chainsIn reports whether the queue chains deploys behind HEAD at all: a
// head whose deploy races (racesIn), or one whose config runs leading stages
// concurrently.
func (o *orchestrator) chainsIn(repo wsm.RepoKey, head *run) bool {
	return head.concurrentStages() || o.racesIn(repo, head)
}

// concurrentStages reports whether the run's deploy config runs any stage
// concurrently. A run that has not read its config yet runs none.
func (r *run) concurrentStages() bool {
	r.mu.Lock()
	defer r.mu.Unlock()
	return r.target.cfg != nil && deployconfig.AnyConcurrent(r.target.cfg)
}

// windowFor is how many runs the chain behind HEAD may hold: every deploy in
// line when the head's config has concurrent stages, else the window in force
// (queueWindow), which the power source and the racing switch decide for a
// chain that tests ahead of its turn.
func (o *orchestrator) windowFor(head *run) int {
	if head != nil && head.concurrentStages() {
		return math.MaxInt
	}
	return o.queueWindow()
}

// awaitTurnFor is the boundary before STAGE: a run whose config runs STAGE one
// at a time, standing behind its chain's head, waits here for its turn. A
// stage the config runs concurrently, and every stage of a config with no
// concurrent stages, passes at once: a racing chain's members are held at the
// landing (race.go), never here.
func (r *run) awaitTurnFor(ctx context.Context, stage deployconfig.Stage) error {
	r.mu.Lock()
	cfg := r.target.cfg
	r.mu.Unlock()
	if cfg == nil || !deployconfig.AnyConcurrent(cfg) || deployconfig.Concurrent(cfg, stage) {
		return nil
	}
	return r.awaitTurn(ctx)
}

// awaitTurn waits until the run heads its chain: every deploy queued ahead of
// it has finished its one-at-a-time stages or left the queue, and the pump
// promoted this one into the repository's slot.
//
// WHILE IT WAITS, its footer stands on StepAwaitingTurn with how many deploys
// are ahead of it, and its line names the deploy right ahead and that
// deploy's step, as a merge waiting in line draws the merge being worked on.
func (r *run) awaitTurn(ctx context.Context) error {
	const op = "daemon.merge.concurrent"
	log := r.o.log(ctx, r.ws)
	logged := false
	for {
		if err := stillWanted(ctx); err != nil {
			return err
		}
		place := r.o.chainPosition(r)
		switch {
		case place.head:
			if logged {
				log.Info(op, "the deploy's turn came; it runs its one-at-a-time stages", dlog.Context{
					"workspace": string(r.ws), "repo": string(r.repo)})
			}
			return nil
		case place.rank < 0:
			err := errors.New("merge: a deploy waiting for its turn is no member of its chain")
			log.Error(op, "a deploy behind the head stands in no chain; it cannot take its turn", dlog.Context{
				"workspace": string(r.ws), "repo": string(r.repo)})
			return err
		}
		ahead, found := r.o.runFor(place.ahead)
		r.setStep(ctx, footer.StepAwaitingTurn, func(f *footer.MergeFacts) {
			f.Ahead = place.rank
			if found {
				f.Line, f.LineAt = ahead.enqueuedLine(r.o.workspaceName(ctx, ahead.ws))
			}
		})
		if !logged {
			logged = true
			log.Info(op, "the deploy finished its concurrent stages; it waits for the deploys ahead of it to finish theirs one at a time", dlog.Context{
				"workspace": string(r.ws), "repo": string(r.repo), "ahead": place.rank, "next_ahead": string(place.ahead)})
		}
		if err := r.idle(ctx, place.changed); err != nil {
			return err
		}
	}
}
