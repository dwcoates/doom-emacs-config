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

THE TABLE IS EMPTY: every arm the daemon had recorded here landed in the
landing-4 contract batch, and each row was deleted as its rpc's `<Rpc>Error`
gained the arm. Record the next one the same way — a row here, then the row
deleted in the commit that switches the handler onto the landed arm.

| rpc | arm | condition | package |
| --- | --- | --- | --- |

One thing the batch did NOT land: `CloseWorkspaceError.blocked` exists but
`CloseWorkspaceBlocked` is still an EMPTY message, so the composed reason the
close refusal carries (turn_in_flight, live_work, held_prompts, merge_queued
and its sentence) has no field to ride in. The arm is usable; the evidence is
not yet expressible.

## Landing-4 batch must also answer (e2e seam, project lead request)

1. Final names of the arms the suite asserts: `transferring_away{address}`,
   `not_yet_adopted{}`, and the DaemonFault / SessionFault / HostFault kind
   arms.
2. The merge test-gate invocation: command line, cwd, env, how pass/fail is
   read, and its AGENT_REPL_* fake knob.
3. The `.claude.json` key path the daemon reads (and writes, if any) for the
   CLAUDE_CONFIG_DIR project entry.
4. The metaprompt sentinels (exact strings) the daemon wraps around a held or
   merged prompt.
