package main

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"google.golang.org/protobuf/proto"
)

// runtimeRefusals is each runtime, repository and editor verb's describer
// with its cases.
var runtimeRefusals = func() []describedRefusal {
	hold := &agentreplv1.UpdateDeployHoldError{}
	rollout := &agentreplv1.RollOutLandingError{}
	prune := &agentreplv1.PruneStorageError{}
	reap := &agentreplv1.ReapWorktreesError{}
	register := &agentreplv1.RegisterRepositoryError{}
	eval := &agentreplv1.EvaluateInEditorError{}
	return []describedRefusal{
		{name: "deploy hold", empty: hold, oneof: "cause",
			cases: []refusalCase{
				{name: "already_held", err: armed(hold, "already_held", nil), want: "the repository's deploys are already held"},
				{name: "not_held", err: armed(hold, "not_held", nil), want: "the repository's deploys are not held"},
				{name: "unknown_repository", err: armed(hold, "unknown_repository", nil), want: "the daemon has no repository by the one -repository named"},
			},
			describe: func(e proto.Message) (string, error) {
				return describeDeployHoldRefusal(e.(*agentreplv1.UpdateDeployHoldError))
			}},
		{name: "roll out", empty: rollout, oneof: "cause",
			cases: []refusalCase{
				{name: "unknown_deploy", err: armed(rollout, "unknown_deploy", nil), want: `no deploy "d1" runs in this daemon: it ended, or its ledger was never minted here`},
			},
			describe: func(e proto.Message) (string, error) {
				return describeRollOutRefusal("d1", e.(*agentreplv1.RollOutLandingError))
			}},
		{name: "prune", empty: prune, oneof: "cause",
			cases: []refusalCase{
				{name: "already_running", err: armed(prune, "already_running", nil), want: "a storage sweep is already running in this daemon"},
				{name: "held_elsewhere", err: armed(prune, "held_elsewhere", nil), want: "another daemon, the other half of a handover, holds the storage sweep's lock"},
				{name: "failed", err: armedWith(prune, "failed", map[string]any{"detail": "disk"}), want: "the storage sweep could not run or ended early (disk)"},
			},
			describe: func(e proto.Message) (string, error) {
				return describePruneRefusal(e.(*agentreplv1.PruneStorageError))
			}},
		{name: "reap", empty: reap, oneof: "cause",
			cases: []refusalCase{
				{name: "already_running", err: armed(reap, "already_running", nil), want: "a worktree reap is already running in this daemon"},
				{name: "held_elsewhere", err: armed(reap, "held_elsewhere", nil), want: "another daemon, the other half of a handover, holds the worktree reaper's lock"},
				{name: "failed", err: armedWith(reap, "failed", map[string]any{"detail": "git"}), want: "the worktree reap could not run or ended early (git)"},
			},
			describe: func(e proto.Message) (string, error) {
				return describeReapRefusal(e.(*agentreplv1.ReapWorktreesError))
			}},
		{name: "register", empty: register, oneof: "cause",
			cases: []refusalCase{
				{name: "not_in_a_repository", err: armed(register, "not_in_a_repository", nil),
					want: "/p lies inside no git work tree, or inside a bare repository, which has no main worktree"},
				{name: "unreadable_path", err: armed(register, "unreadable_path", nil), want: "/p cannot be read: it is not there, or its directory cannot be stat'ed"},
				{name: "inside_temporary_directory", err: armedWith(register, "inside_temporary_directory", map[string]any{"dir": "/tmp/r", "temporary_root": "/tmp"}),
					want: "the repository's main worktree /tmp/r lies inside the temporary directory /tmp"},
			},
			describe: func(e proto.Message) (string, error) {
				return describeRegisterRefusal("/p", e.(*agentreplv1.RegisterRepositoryError))
			}},
		{name: "eval", empty: eval, oneof: "cause",
			cases: []refusalCase{
				{name: "unknown_workspace", err: armed(eval, "unknown_workspace", nil), want: "the daemon has no workspace ws"},
				{name: "no_editor", err: armed(eval, "no_editor", nil), want: "no editor serves ws, so nothing can evaluate the text"},
				{name: "timed_out", err: armedWith(eval, "timed_out", map[string]any{"deadline_ms": uint32(5000)}), want: "the editor did not answer within 5000ms"},
			},
			describe: func(e proto.Message) (string, error) {
				return describeEvalRefusal("ws", e.(*agentreplv1.EvaluateInEditorError))
			}},
	}
}()

func TestDescribeRuntimeRefusals(t *testing.T) {
	testDescribedRefusals(t, runtimeRefusals)
}
