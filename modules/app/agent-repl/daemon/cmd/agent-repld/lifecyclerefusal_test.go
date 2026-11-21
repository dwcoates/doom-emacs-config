package main

import (
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"

	"google.golang.org/protobuf/proto"
)

// lifecycleRefusals is each lifecycle verb's describer with its cases.
var lifecycleRefusals = func() []describedRefusal {
	open := &agentreplv1.OpenWorkspaceError{}
	closing := &agentreplv1.CloseWorkspaceError{}
	kill := &agentreplv1.KillWorkspaceError{}
	sel := &agentreplv1.SelectWorkspaceError{}
	restart := &agentreplv1.RestartWorkspaceError{}
	park := &agentreplv1.ParkWorkspaceError{}
	interrupt := &agentreplv1.InterruptError{}
	compact := &agentreplv1.CompactWorkspaceError{}
	status := &agentreplv1.GetWorkspaceStatusError{}
	gate := &agentreplv1.PassColdGateError{}
	notify := &agentreplv1.NotifyWorkspaceError{}
	return []describedRefusal{
		{name: "open", empty: open, oneof: "cause",
			cases: append(append(sharedWorkspaceCases(open, "open it again", theFourWorkspaceArms...),
				refusalCase{name: "session_deleted", err: armed(open, "session_deleted", nil), want: "ws's session has been deleted and cannot be reopened"},
				refusalCase{name: "transcript_missing", err: armed(open, "transcript_missing", &agentreplv1.OpenWorkspaceTranscriptMissing{VendorSessionId: "v1", SearchedPaths: []string{"/a", "/b"}}),
					want: "ws's vendor transcript v1 is nowhere on disk (searched /a, /b)"},
				refusalCase{name: "spawn_failed", err: armedWith(open, "spawn_failed", map[string]any{"detail": "exec"}), want: "spawning ws's shim failed (exec)"},
				refusalCase{name: "vendor_start_failed", err: armedWith(open, "vendor_start_failed", map[string]any{"detail": "threw"}), want: "the vendor failed to start ws's session (threw)"},
				refusalCase{name: "lock_holder_unavailable", err: armed(open, "lock_holder_unavailable", &agentreplv1.OpenWorkspaceLockHolderUnavailable{Failure: &conversationv1.LockHolderFailure{Binary: "/bin/lock"}}),
					want: "ws's shim came up, but its lock helper /bin/lock failed, so it would not start a session it could not prove it alone owns"},
				refusalCase{name: "worktree_unrestorable", err: armedWith(open, "worktree_unrestorable", map[string]any{"dir": "/w/ws", "branch": "b", "detail": "gone"}),
					want: "ws's directory /w/ws is gone and its branch b no longer exists, so there is nothing to restore its worktree from (gone)"},
				refusalCase{name: "not_merged", err: armed(open, "not_merged", nil),
					want: "ws's merge has not landed, or it was reopened since, so there is no merge to revert; nothing was opened"},
				refusalCase{name: "brief_missing", err: armedWith(open, "brief_missing", map[string]any{"detail": "absent"}),
					want: "the merge-revert brief could not be composed (absent); nothing was opened"}),
				parentNotOpenCases(open, "open ws again")...),
			describe: func(e proto.Message) (string, error) {
				return describeOpenRefusal("ws", e.(*agentreplv1.OpenWorkspaceError))
			}},
		{name: "close", empty: closing, oneof: "cause",
			cases: append(sharedWorkspaceCases(closing, "close it again", theFourWorkspaceArms...),
				refusalCase{name: "blocked", err: armedWith(closing, "blocked", map[string]any{"summary": "a turn runs"}),
					want: "ws has work in flight (a turn runs); the footer shows what blocks the close"},
				openChildrenCase(closing, "close it again")),
			describe: func(e proto.Message) (string, error) {
				return describeCloseRefusal("ws", e.(*agentreplv1.CloseWorkspaceError))
			}},
		{name: "kill", empty: kill, oneof: "cause",
			cases: append(sharedWorkspaceCases(kill, "kill it again", theFourWorkspaceArms...), openChildrenCase(kill, "kill it again")),
			describe: func(e proto.Message) (string, error) {
				return describeKillRefusal("ws", e.(*agentreplv1.KillWorkspaceError))
			}},
		{name: "select", empty: sel, oneof: "cause",
			cases: append(sharedWorkspaceCases(sel, "select it again", theFourWorkspaceArms...),
				refusalCase{name: "standing_down", err: armed(sel, "standing_down", nil),
					want: "the daemon is standing down, so it recorded ws as selected but starts no session; the daemon that serves next revives it"}),
			describe: func(e proto.Message) (string, error) {
				return describeSelectRefusal("ws", e.(*agentreplv1.SelectWorkspaceError))
			}},
		{name: "restart", empty: restart, oneof: "cause",
			cases: append(sharedWorkspaceCases(restart, "restart it again", theFourWorkspaceArms...),
				refusalCase{name: "relaunch_failed", err: armedWith(restart, "relaunch_failed", map[string]any{"detail": "spawn"}),
					want: "relaunching ws failed, so it was not restarted (spawn)"}),
			describe: func(e proto.Message) (string, error) {
				return describeRestartRefusal("ws", e.(*agentreplv1.RestartWorkspaceError))
			}},
		{name: "park", empty: park, oneof: "cause",
			cases: sharedWorkspaceCases(park, "park it again", theFourWorkspaceArms...),
			describe: func(e proto.Message) (string, error) {
				return describeParkRefusal("ws", e.(*agentreplv1.ParkWorkspaceError))
			}},
		{name: "interrupt", empty: interrupt, oneof: "kind",
			cases: append(sharedWorkspaceCases(interrupt, "interrupt it again", theFourWorkspaceArms...),
				refusalCase{name: "confirm_required", err: armedWith(interrupt, "confirm_required", map[string]any{"live_agent_count": int64(2)}),
					want: "ws has 2 live detached agents; interrupt it again with -confirm-agents to stop them with the turn, or stop them first with -all-agents"},
				refusalCase{name: "not_detached_work", err: armed(interrupt, "not_detached_work", nil), want: "the interrupt named no detached work of ws"},
				refusalCase{name: "no_session", err: armed(interrupt, "no_session", nil), want: "ws has no session to interrupt"},
				refusalCase{name: "shim_refused", err: armedWith(interrupt, "shim_refused", map[string]any{"detail": "nothing_running"}),
					want: "ws's session refused the interrupt (nothing_running)"},
				refusalCase{name: "not_merge_tab", err: armed(interrupt, "not_merge_tab", nil), want: "the interrupt named no agentic tab of a merge ws's feed draws"}),
			describe: func(e proto.Message) (string, error) {
				return describeInterruptRefusal("ws", e.(*agentreplv1.InterruptError))
			}},
		{name: "compact", empty: compact, oneof: "cause",
			cases: append(sharedWorkspaceCases(compact, "", "unknown_workspace", "workspace_ref_mismatch"),
				refusalCase{name: "closed", err: armed(compact, "closed", nil), want: "ws is closed, so no session runs to compact"},
				refusalCase{name: "no_session", err: armed(compact, "no_session", nil), want: "ws has no session to compact"},
				refusalCase{name: "cold_gate", err: armed(compact, "cold_gate", nil),
					want: "ws is parked at its cold gate, where compacting is the gate's compact or compact-and-resume answer, which the user must ask for or approve; nothing was submitted"}),
			describe: func(e proto.Message) (string, error) {
				return describeCompactRefusal("ws", e.(*agentreplv1.CompactWorkspaceError))
			}},
		{name: "status", empty: status, oneof: "cause",
			cases: sharedWorkspaceCases(status, "", "unknown_workspace", "workspace_ref_mismatch"),
			describe: func(e proto.Message) (string, error) {
				return describeStatusRefusal("ws", e.(*agentreplv1.GetWorkspaceStatusError))
			}},
		{name: "cold gate", empty: gate, oneof: "cause",
			cases: append(sharedWorkspaceCases(gate, "pass the gate again", theFourWorkspaceArms...),
				refusalCase{name: "no_cold_gate", err: armed(gate, "no_cold_gate", nil), want: "no cold gate stands on ws"},
				refusalCase{name: "unserved_remediation", err: armed(gate, "unserved_remediation", nil),
					want: "the cold gate on ws offered no Haiku compaction of all its context, the one way an agent passes a gate"}),
			describe: func(e proto.Message) (string, error) {
				return describePassColdGateRefusal("ws", e.(*agentreplv1.PassColdGateError))
			}},
		{name: "notify", empty: notify, oneof: "cause",
			cases: append(sharedWorkspaceCases(notify, "", "unknown_workspace", "workspace_ref_mismatch"),
				refusalCase{name: "workspace_closed", err: armed(notify, "workspace_closed", nil), want: "ws is closed, so a click on the banner would have no tab to select"},
				refusalCase{name: "no_banner_program", err: armedWith(notify, "no_banner_program", map[string]any{"cause": "no terminal-notifier"}),
					want: "no desktop banner program could be found on this host (no terminal-notifier)"},
				refusalCase{name: "standing_down", err: armed(notify, "standing_down", nil), want: "the daemon is standing down and starts no banner"}),
			describe: func(e proto.Message) (string, error) {
				return describeNotifyRefusal("ws", e.(*agentreplv1.NotifyWorkspaceError))
			}},
	}
}()

func TestDescribeLifecycleRefusals(t *testing.T) {
	testDescribedRefusals(t, lifecycleRefusals)
}
