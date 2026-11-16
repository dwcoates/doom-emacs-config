# Emoji-ack intake routes a reaction to an open workspace

A second Slack reaction about a task that already has a workspace used to fork a
duplicate. The intake now lists the workspaces a reaction may go to, forwards
it to one clear match, asks several close ones, or forks as before. The owner
settled the routing on 2026-11-16 and asked that it start loose.

Design mode: autonomous. Every shape below was approved by the implementing
workspace (`emoji-ack-intake-routing`), as the owner directed.

## Landed changes

### `agentrepl.v1.FindWorkspacesRequest.login` (`workspace list -login here|WS`)

- WHAT: an optional `FindWorkspacesLogin` whose one arm, `of_workspace`, names
  a workspace. The find keeps only workspaces whose repository's main checkout
  is routed to the same login as that workspace's repository, plus the
  children of those workspaces in any repository. A ref the registry does not
  hold is the new `unknown_login_workspace` error arm.
- WHY: the intake must see only eligible workspaces in one command. The owner
  defined eligibility as open workspaces in work repositories, plus their
  children, which need not be in a work repository.
- The login is named by a workspace and not by a config dir, because the
  intake always runs in a work repository. It names itself (`here`), so it
  never has to know the config-dir spelling. A config-dir arm can be added
  later as a second arm.
- Routing is read from the REPOSITORY's directory and not from the session's
  selected account. "A work repository" is a property of the repository, and
  the account a reader picked for one session does not move its repository.
- Children: only DIRECT children of a routed workspace are added. A
  grandchild is a child of a non-routed child, which is out of scope for now.
  The owner will tighten it later.

### `agentrepl.v1.SubmitPromptDelivery.SUBMIT_PROMPT_DELIVERY_AFTER_TOOL_CALL` (`-skip-classification`)

- WHAT: a third delivery. While a turn runs, the prompt earns the classifier's
  `after_tool_call` verdict without asking the classifier, and joins the
  running turn through the ordinary join path. Refused beside `feed`.
- WHY: the owner wanted intake questions delivered after the target's current
  tool call, never interrupting and never held for the turn's end.
- It is a value of the existing delivery enum, not a new bool. Delivery is
  already that enum, and a bool beside it would be a second, contradictory
  way to say how a prompt is delivered.
- CONSEQUENCE (accepted): a join the running turn cannot take (a context cut,
  a turn past its last tool boundary) leaves the prompt held for the turn's
  end, as every join that cannot go already does. Joining is the only way to
  reach a running turn without interrupting it.
- CONSEQUENCE: `held_prompts.delivery` now stores 2. The column is an integer
  with no check, so no migration is needed. A build older than this one fails
  loudly (`DecodeError`) on such a row, and never reads it as ordinary.
- `GatherWorkspacesRequest.delivery` carries the same enum, so one gather
  delivers every target the same way.

### `agentrepl.v1.SubmitPromptRequest.slack_forward` (`workspace prompt -slack-forward URL`)

- WHAT: an optional `SubmitPromptSlackForward{thread_url}` on an agent-sent
  prompt. The daemon records the thread on the recipient (a new wsm table,
  once per thread) and stores it as a reference link, "Forwarded Slack
  thread". It also appends its built-in directive asking the recipient to post
  one brief reply in the thread naming itself.
- WHY: the owner wanted the reacted thread told which workspace took the
  reaction, and a workspace able to hold Slack threads beyond the one it was
  created for.
- Posting goes through the recipient's agent, as the built-in emoji-ack
  creation and finish defaults already do. master's
  `TestTheDaemonNeverPostsToSlack` forbids the daemon itself running a Slack
  send. `agent-repl-slack-threads` is waiting on an owner ruling about that
  test, so a direct daemon post waits on that ruling.
- Which posts each thread gets: a forwarded thread gets only the forward
  reply. The PR announcement and the `:merged:` reaction stay with the origin
  thread (`slack_origin`). The owner may widen this later.
- Coordinated with `agent-repl-slack-threads`. Its reaction bindings carry
  `-thread announced|subscribed`. A forward from a subscribed binding should
  also subscribe the target. That port waits on its landing and is a todo row.
