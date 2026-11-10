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
