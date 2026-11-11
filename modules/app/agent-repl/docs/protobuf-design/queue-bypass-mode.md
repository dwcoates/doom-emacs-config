# Queue bypass mode

A personal-account workspace can hold its repository's queue bypass mode: the
repository's merge queue is hard-stopped and the workspace lands each change on
the main branch itself, under a system prompt whose landing section is the
bypass directive instead of the merge-queue rules, with the merge-queue skill
switched off. Designed autonomously (owner request, 2026-11-10).

## Assumptions

- The mode is one repository's, held by at most one of its workspaces (owner
  ruling 2.2). The store enforces it with a table keyed by repository.
- Personal versus work is `account.IsMultiRepo` on the session's config dir:
  the work account is the multi-repo one, every other account is personal.
- Each workspace keeps two conversations, one per landing; the toggle switches
  which is bound (owner ruling on 2.1: a resumed session under a swapped
  system prompt re-reads the whole transcript uncached, so a second session is
  kept instead). The one not running is kept for the next switch (ruling 3.2).
- The FIRST switch into the mode forks the queue conversation into the bypass
  conversation (owner request, 2026-11-10): the bind records an owed fork of
  the workspace's own conversation (`pending_forks.parent_vendor_session_id`,
  layout 114) in the same write, and the bring-up makes it through
  `shim.v1.ForkSession` and resumes it as a rebind. Later switches resume the
  two parked conversations as they stand, so the bypass conversation does not
  pick up queue turns taken after the fork.
- Both conversations are kept warm in tandem (owner request, 2026-11-10).
  Each keeps its own cache clock: the parked row carries the parked one's
  (`workspace_parked_conversations.clock_at`, `.cold`, layout 116), and a
  switch swaps it with the workspace row's, stamping
  `workspaces.conversation_bound_at` so turns from before the binding time
  nothing. The keeper's beat is due at the earlier of the two clocks' next
  beats and beats both; the parked one goes out through
  `shim.v1.SendKeepaliveRequest.parked`, which the shim sends on a query of
  its own under the parked conversation's profile, resuming before its
  trailing keep-alive turns, and answers once the vendor has. The shim
  refuses to hibernate (`turn_in_flight`) while a parked keep-alive is out.
- The SDK's `skillOverrides` flag setting (`off`) hides a skill from the
  listing and the Skill tool; the shim checks the session's reported commands
  for its absence at start.
- Taking the mode may name how the bypass session gets its context (owner
  request, 2026-11-11), through a menu on the chip drawn as the topbar's other
  menus are: a full fork of the queue conversation, a compacted fork of it, the
  parked bypass conversation, or the parked bypass conversation plus a
  compacted summary of the queue conversation. The last two are offered only
  while a bypass conversation is parked; the daemon decides, the webapp never
  infers. Releasing the mode is unchanged. `toggle`, and an `on` naming no
  context, keep the behavior from before the menu.
- The other topbar menus (model, effort, permission mode, account) carry a
  daemon-composed `display_name` and no hint, so the context options carry a
  `display_name` alone.
- Compaction reuses the shim's one summarizer (the cold gate's): the
  compacted fork is compacted at its first start by the `compact` remediation
  `StartSessionResume.cold_remediation` already carries, which the shim
  carries out whether or not the context is cold. The summary of the queue
  conversation is written by the same summarizer on an ephemeral fork, which
  writes nothing to either transcript. Its model and scope are the cold gate's
  defaults (Haiku, the whole transcript).
- The summary reaches the bypass conversation as a meta-wrapped note that
  opens the user's next prompt, the way the owed-summary note closes one: no
  agent turn is started for it, and it is in context when the user next
  prompts.
- The compaction or summary a switch owes is DURABLE: it is recorded with the
  binding, carried by every start of the workspace until one succeeds, and
  forgotten by any later binding. A start that fails on it fails the switch
  with `compaction_failed`; the next start asks for it again.

## Landed changes

- `agentrepl.v1.UpdateQueueBypassMode` (endpoint_update_queue_bypass_mode.proto):
  on, off and toggle, refused for a work account, while another workspace of
  the repository holds the mode, and while a turn is in flight.
- `frontend.v1.TopbarView.queue_bypass` (field 17, `TopbarQueueBypass`): the
  chip, set only for a personal account; arms on, off, held_elsewhere, and a
  daemon-composed tooltip.
- `frontend.v1.TopbarWarningStrip.severity`: the strip's resolved severity
  (errors when a session fault or a failed deploy stands, else warnings_only),
  unset when the list is empty. The heading's title draws the warnings in that
  color and opens them; the warning chip is retired (owner request,
  2026-11-10).
- `shim.v1.SessionProfileAgent.landing` (`SessionProfileAgentLanding`):
  through_queue or bypassing_queue, required. It picks the system prompt's
  landing section and whether the merge-queue skill is switched off.
- Sweep: nothing obviated. The warning chip's message family stays: the
  heading draws the same list.
- `frontend.v1.TopbarQueueBypassContext` (topbar.proto): the four ways the
  bypass session gets its context, a oneof of empty arms (`full_main`,
  `compacted_main`, `existing`, `existing_plus_compacted_main`). It is the
  TYPED ECHO TOKEN the chip's menu rows serve and the request echoes, so the
  arms are declared once. It lives in `frontend.v1` because the daemon serves
  it in the topbar view and `frontend.v1` may not import `agentrepl.v1`; the
  request imports it, as other endpoints import `frontend.v1.FeedId`.
- `frontend.v1.TopbarQueueBypassOff.options` (field 1,
  `TopbarQueueBypassContextOption`: `context`, `display_name`): the menu's
  rows in display order, the two main-context options always and the two
  existing ones only while a bypass conversation is parked.
- `agentrepl.v1.UpdateQueueBypassModeOn.context` (field 1, optional
  `frontend.v1.TopbarQueueBypassContext`): the settled "oneof context with
  four empty-message arms" lands as the echo token holding that oneof, rather
  than a second oneof declared on the request, which would respell the
  topbar's vocabulary. Unset is the default behavior; set with no arm is
  InvalidArgument.
- `agentrepl.v1.UpdateQueueBypassModeError`: `no_bypass_conversation` (8,
  empty: an existing option named while nothing is parked; nothing changed)
  and `compaction_failed` (9, `detail`: the compaction or summary did not
  complete; the swap stands and the next start asks again).
- `shim.v1.StartSessionResume.carry_summary` (6, `StartSessionCarrySummary`:
  `vendor_session_id`, `summarizer` as the imported
  `conversation.v1.SessionColdCompact`) and
  `shim.v1.StartSessionSuccess.carried_summary` (2,
  `StartSessionCarriedSummary.text`): the resume that carries a summary of
  another conversation of the workspace, and the summary it answers.
- `shim.v1.StartSessionVendorStartFailed.compaction` (3,
  `StartSessionCompactionFailed`, presence is the fact): the failure was the
  asked-for compaction or summary, so the daemon answers `compaction_failed`
  rather than `switch_failed`. The retry arm labels it as for any failure.
- `shim.v1.StartSessionResume.cold_remediation`'s comment now states what the
  shim always did: a set remediation is carried out warm or cold.
