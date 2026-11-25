# Standing close request (2026-11-25)

A workspace whose work had landed asked to close itself and was refused because it still had an open child. Its only recourse was to promise to close again "if prompted" once the child closed, so the close depended on someone remembering to prompt it. The owner asked that a workspace be able to register itself as closed, so the daemon closes it automatically once nothing blocks the close any more, until the workspace withdraws that registration.

## Assumptions (autonomous design)

- Objective: a close blocked by something that clears on its own is carried out by the daemon when it clears, with no further prompt.
- Built: a durable standing close request on the workspace row, a daemon component that judges it again on every edge that can clear a blocker, a withdraw verb, and its visibility in the footer and in `WorkspaceStatus`.
- Frontend: the footer's existing closing status gains one step (`waiting`); no new component.
- Systems: daemon (registry, verbs, server, CLI, edges), webapp (footer step label), proto. Emacs is untouched: the editor's close gesture never asks to stand, so it never receives the new outcome.
- Non-additive: the self-close's `close_when_turn_ends` mark is retired into the standing request (one mechanism, not two). A self-close that something other than its asking turn blocks now stands instead of being refused.

## Iteration sequence

Transport unchanged (Connect). Endpoints: `CloseWorkspace` changes, `WithdrawWorkspaceClose` is new. Conventions: the package's `success`/`error` outcome oneof. Then the shapes one endpoint at a time, then `WorkspaceStatus`, then the footer step.

## Landed changes

### `CloseWorkspaceRequest.stand` and `CloseWorkspaceSuccess.stands`

- WHAT: the request gains `stand`; the success outcome gains the `stands` arm carrying `WorkspaceCloseWaits` (what blocked the close when it was asked).
- WHY explicit rather than automatic for every requester: the editor's close gesture is answered at once in the footer, and a user who clicked close on a busy workspace and walked away must not have its tab torn down an hour later. Every agent-API close (`agent-repld workspace close`) asks to stand, which is what "register itself as closed" asks for, and the answer says the request now stands. A self-close always stands, whatever `stand` says: the workspace's own agent asking to close is exactly the registration the owner described.
- WHY a success arm and not a refusal: a standing request is an accepted close, carried out later, the same kind of answer as `closes_when_turn_ends`. Routing it through the error arm would make an accepted request read as a failure.
- Consequence: the `blocked` and `has_open_children` refusals still exist, for `stand = false` (the editor).
- Consequence: `closes_when_turn_ends` is now a standing request whose only blocker is the asking turn; if something else blocks by the turn's end (a held prompt), the request keeps standing instead of being abandoned. The `workspace-self-close-refused` brief is retired with the abandon path.

### `workspace_close.proto`: `CloseWorkspaceBlocked` moved, `WorkspaceCloseWaits` added

- WHAT: `CloseWorkspaceBlocked` moved out of the close endpoint file into a shared file (same package, same full name, so no wire change), beside the new `WorkspaceCloseWaits`, because the status now carries it too. The file model keeps endpoint files private to one rpc.
- `WorkspaceCloseWaits` embeds `WorkspaceHasOpenChildren` whole (family.proto) rather than respelling children and landing-into lists.

### `WithdrawWorkspaceClose`

- WHAT: a new rpc taking a workspace and a requester; success is `withdrawn` or `none_stood`; errors are the routing arms every workspace rpc has.
- WHY a separate rpc rather than a flag on `CloseWorkspaceRequest`: withdrawing is not a close, and a close request carrying a "cancel" flag would carry `stand` beside it meaninglessly.
- `none_stood` is a success: the caller's wish already holds.

### `WorkspaceStatus.close_request`

- WHAT: optional `WorkspaceStatusCloseRequest` with `requested_at_ms`, who asked (user or agent, a oneof), and the live `WorkspaceCloseWaits`.
- The registry records only whether the user or an agent asked (the close it carries out is recorded under that cause), so the arm names no workspace.

### Footer: `FooterStatusClosing.waiting`

- WHAT: a third member of `FooterStatusClosing` (number 3, since `activity` holds 2), drawn while a standing request waits; the activity line is the same composed close-blocked sentence.
- Consequence: the closing rung outranks thinking and waiting on the ladder, so a standing request yields to a turn in flight and to a wait on the user (the footer's `closing` draws nil for it then). A one-time refusal (`blocked`) keeps its rung as before.

## Withdrawal events (decided)

- `WithdrawWorkspaceClose` (CLI `workspace close WS -cancel`).
- Any prompt the user submits to the workspace (`SubmitPrompt` with a user origin), the same edge that takes a one-shot back.
- The user releasing a held prompt (`UpdateHeldPrompt` release): delivering held user intent is the user working with the workspace again.
- The close itself (`MarkClosed` clears the request), so a request never outlives its workspace's close, however that close came about (kill, nuke, merge, the user).
- NOT withdrawn by: notices and agent prompts (they block until their turn ends), dropping a held prompt (that clears a blocker), a backlog.

## Re-evaluation edges (decided)

One serial runner (`workspace.StandingCloses`) judges requests; nothing polls.

- Per workspace: a turn ending (observed or unobserved), the workspace falling free (the session watcher's freeness edge: the last turn or detached item ended), its held prompts changing (the prompt queue's holds sink), its merge landing or leaving the queue unlanded.
- Every standing request: any workspace closing, killed or nuked, and any retarget (`SetParent`, `RestoreParent`), since open descendants and workspaces landing into a workspace block it; requests are few, so judging all of them is cheaper and safer than walking ancestry.
- At boot, every standing request once: edges that fired while no daemon served are not replayed.
- Registration itself reports the workspace to the runner, so a blocker that cleared between the judgment and the write is never missed.
- The runner's close does not interrupt a turn (unlike the user's close): it carries the close out only when the workspace is quiet.

## Storage

- Layout 215 (additive): `close_requested_at`, `close_requested_by` on `workspaces`, filled from `close_when_turn_ends` (by `agent`), with triggers mirroring the old column both ways while a predecessor daemon shares the file. Layout 216 (contract) drops the triggers and `close_when_turn_ends`.

## Rendering (decided)

- The webapp draws `closing · waiting` as "waiting to close", standing still like `close blocked`, under the closing arm's existing BLUE.
- `composer_open_substatuses` declares `closing: [waiting]`: a standing close leaves the workspace usable, and a prompt the user sends is exactly what withdraws it, so the composer stays open.
- Open for the owner: whether a standing close should keep the closing arm's blue (an unusable workspace) at all, or draw in another color; the color is per arm, so a different color would need its own arm.
