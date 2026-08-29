# Intended-but-unlanded error arms

The ledger of refusals the daemon must make for which the contract has no
typed error arm yet.

When a state must be refused and `<Rpc>Error` has no arm for it, the handler
answers a Connect error — `CodeFailedPrecondition` for a state refusal,
`CodeNotFound` for an unknown id — whose message is EXACTLY:

```
intended arm: <RpcName>Error.<arm_name>: <reason>
```

and logs the intended arm at WARNING with operation
`daemon.refusal.unlanded_arm`. `server.UnlandedArm` is the one helper that does
both; nothing else spells the message by hand.

Every such site is recorded here. The daemon teamlead batches the table to the
project lead, who lands the arms; a landed arm is deleted from this table in
the same commit that switches the handler onto it.

| rpc | arm | condition | package |
| --- | --- | --- | --- |
| SelectWorkspace / OpenWorkspace / CloseWorkspace / KillWorkspace / NukeWorkspace / RestartWorkspace / SetWorkspacePriority / AssignWorkspaceTask / Interrupt / AnswerPermission / AnswerQuestion / AnswerColdGate / SetModel / SetPermissionMode / OpenExternal / OpenInEditor / SessionHealth / RequestCommandSupport | `unknown_workspace` | the ref names an id the registry does not hold (answered NotFound) | workspace |
| every per-workspace rpc | `workspace_ref_mismatch` | the echoed `dir` disagrees with the registry, or cannot be normalized | workspace |
| every per-workspace rpc | `transferring_away` | the workspace has been handed to a successor daemon | workspace |
| every per-workspace rpc | `not_yet_adopted` | a joining daemon has not adopted the workspace yet | workspace |
| RegisterWorkspace | `not_a_worktree` | the announced directory is not a git worktree | workspace |
| CreateWorkspace | `ungated_without_consent` | the requested permission mode disables the consent gate and no `allow_ungated` was recorded | workspace |
| CreateWorkspace | `no_slug` | no name was supplied and no slug can be derived from the initial prompt (also: a one-shot with no prompt) | workspace |
| CreateWorkspace | `finish_required` | the one-shot form names no finish action | workspace |
| CreateWorkspace | `finish_not_one_shot` | a finish action was supplied on the standard form | workspace |
| CreateWorkspace | `fork_parent_has_no_conversation` | the parent workspace has no conversation to fork | workspace |
| CreateWorkspace | `brief_missing` | a prompts/ brief the composition needs is absent or will not splice | workspace |
| OpenWorkspace | `session_deleted` | the session record is terminal by deletion; a deleted session refuses resurrection | workspace |
| OpenWorkspace | `transcript_missing` | a resume whose vendor transcript file is gone, refused BEFORE any spawn | workspace |
| CloseWorkspace | `blocked` (LANDED as `CloseWorkspaceError.blocked`; the composed reason has no field yet) | the workspace is not quiet: turn_in_flight, live_work, held_prompts or merge_queued | workspace |
| Interrupt | `confirm_required` (LANDED; raised as `workspace.ConfirmRequired`) | detached agents are live and the caller has not confirmed | workspace |
| Interrupt | `unserved_answer` | the addressed FeedId's row kind addresses no detached work (answered NotFound) | workspace |
| AnswerPermission | `unserved_answer` | the permission ask is not standing, or the answer carries no decision | workspace |
| AnswerPermission | `no_standing_offer` | `allow_standing` on an ask that offered no standing grant | workspace |
| AnswerQuestion | `unserved_answer` | the batch is not standing, or a question text or chosen label was never served | workspace |
| AnswerQuestion | `multi_pick_on_single_select` | more than one choice on a single-select question | workspace |
| AnswerColdGate | `no_cold_gate` | no cold gate is standing on the workspace | workspace |
| AnswerColdGate | `unserved_remediation` | the compaction names a model or scope outside the served menu, or no choice at all | workspace |
| AnswerPermission / AnswerQuestion / AnswerColdGate | `no_session` | the workspace has no live session to answer through | workspace |
| SetModel | `unserved_answer` | no model was named | workspace |
| SetPermissionMode | `mode_not_served` | the mode is outside exactly what the topbar's picker served | workspace |
| SetPermissionMode | `ungated_without_consent` | an ungated mode with no consent recorded at creation | workspace |
| OpenExternal | `unserved_answer` | the url is not absolute, or no external browser is configured | workspace |
| OpenInEditor | `path_escapes_workspace` | the path is blank or resolves outside the workspace | workspace |
| CreateTask / UpdateTask | `blank_title` | a blank title, or an update that changes nothing | workspace |
| RequestCommandSupport | `blank_command` | no command was named | workspace |
| RequestCommandSupport | `brief_missing` | `prompts/add-support-slash-command.md` is absent or will not splice | workspace |
| SubmitPrompt (the one-shot finish hook) | `brief_missing` | `prompts/oneshot-create-pr-then-close-followup.md` is absent or will not splice when the one-shot's turn concludes | workspace |
