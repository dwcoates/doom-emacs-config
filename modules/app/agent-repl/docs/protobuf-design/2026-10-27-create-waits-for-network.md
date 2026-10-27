# A create waits for the network

On 2026-10-27 the machine's network flapped and two identical one-shot creates
failed with `agentrepl.v1.CreateWorkspaceError.naming_failed`: both Haiku naming
attempts timed out while a `network_unreachable` fault stood. The owner asked
that such a create be QUEUED until the network is back and then completed, and
that repeated identical creates be DEDUPLICATED into the one queued or
in-flight create. The contract change was pre-approved (owner, 2026-10-27:
"decide and carry them through").

## Core design principles

- **A create held for the network is an answer, not a failure.** The caller
  is told the create waits; it is never drawn as FAILED, and it does not
  trigger the main-worktree create-failure report.
  - Consequence: the wait is its own response arm and its own progress step,
    never a `CreateWorkspaceError` arm.
  - Does not claim: that a queued create can never fail. A resumed create that
    fails for a non-network cause still fails and is still reported.

## Landed changes

### 1. `waiting_for_network` outcome, `PendingCreateRef`, `joined`

- WHAT: `agentrepl.v1.CreateWorkspaceResponse.result` gains
  `waiting_for_network` (`agentrepl.v1.CreateWorkspaceWaitingForNetwork`), the
  synchronous form's answer for a held create.
  `agentrepl.v1.WorkspaceCreateProgress.step` gains the same message as a
  NON-terminal step for the option-B form. `agentrepl.v1.PendingCreateRef` is
  the typed identity of a pending create, minted by the daemon.
  `agentrepl.v1.CreateWorkspaceAccepted.joined` (optional
  `agentrepl.v1.CreateWorkspaceJoined`) says an option-B request joined an
  identical pending create.
- WHY: the owner wants the network named, the create completed, and repeats
  collapsed into one create.
- Why one message for both the response arm and the progress step: they are
  one fact (this create waits for the network) on two channels of one rpc, not
  two UI components, so one canonical form holds.
- Why no evidence field on the wire: the client draws "waiting for the
  network" and nothing more. The evidence (open fault, CLI words, the
  daemon's reachability check) is recorded in the daemon log.
- Consequences:
  - Emacs keeps an op registered across `waiting_for_network`, because the
    terminal step can arrive much later, possibly from a successor daemon.
  - A synchronous duplicate is answered with the existing create's outcome
    (`success` naming the same workspace, or `waiting_for_network` with the
    same ref), so the sync form needs no `joined` field.
  - The pending create's ref survives daemon restarts and handovers: it is the
    key of the durable queued-create row.
