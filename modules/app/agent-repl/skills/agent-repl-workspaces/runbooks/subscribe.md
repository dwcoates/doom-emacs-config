# Subscribing to a workspace

- `workspace subscribe WS -events ...` has this workspace's agent told, as a
  prompt drawn nowhere, when WS's turn starts or ends, it needs input, it lands,
  the build carrying its landing is running (`deployed`, a different moment
  from `landed`; a failed deploy is told too and means the running build does
  NOT carry it; told only for a landing on a repository whose deploy the
  daemon runs, that is agent-repl's own master), its merge leaves the merge
  queue without landing (`evicted`; the notice carries the reason: tests failed
  out, conflicts unresolved, a person took it off, a restart abandoned it), its
  session parks at its cold gate (`cold_gated`; `needs_input` does NOT cover
  it, that is only a question or permission), it closes (`closed`, which says
  how: closed, killed or nuked) or it takes a GitHub pull request as its own
  (`pull_request`: it stores an owned link to one it did not own before; the
  notice names the pull request). Omit `-events`, or name `lifecycle`, to hold
  the lifecycle bundle: landed, closed (with how), deployed, evicted and
  cold_gated. Turn events (`turn_started`, `turn_ended`), `needs_input` and
  `pull_request` are not in the bundle; ask for them by name. WS may be in
  any repository. Subscribe instead of polling, to
  follow a peer or an unrelated workspace (a parent is given its child's one update, a one-shot's landing or
  any other child's first turn end, without subscribing; it subscribes with
  `-events turn_ended,needs_input` only when it drives the child's work turn
  by turn); `workspace unsubscribe WS`
  ends it. A subscription ends with this workspace's close (re-opening does not
  restore it) and when WS is forgotten; WS closing is an event it tells, and
  it goes on following WS if WS is opened again. A notice that waited
  behind your turns lists every change since the last one you were told,
  oldest first, and its status is WS as it stands when the notice is made.

- A SUBSCRIPTION TO WS FOLLOWS THE MERGE OF EVERY PULL REQUEST WS OWNS,
  whatever its events. The daemon subscribes this workspace to each one WS
  owns when the subscription is taken, and to each one WS stores as owned
  while it is followed, and stores each as this workspace's reference link
  under WS's label. Such a follow (origin `followed_workspace`, event
  `merged`) tells the merge alone, or the close as the pull request's end,
  never its comments, checks or merge-queue progress. A link or a
  subscription this workspace already held to the pull request is left as
  it is. `workspace unsubscribe WS` does not end the follows already taken;
  `pr-unsubscribe` ends one.

- TURN-END SUBSCRIPTIONS (`-events turn_ended`) ARE THE TIGHTEST LEVEL, and
  nothing grants one automatically, not even `create -relationship child`:
  each notice wakes this workspace's agent at the end of every turn of WS.
  Subscribe so only when this workspace truly needs very tight coordination
  with WS: a SUBTASK it dispatched and builds on turn by turn (it integrates
  each result, answers each handoff, or cannot proceed until WS's turn is
  over), right after the create (`workspace subscribe NEW -events
  turn_ended`; the answer carries WS's status, so no turn end slips between).
  Never for a tangential one-shot, a side task, a peer, or a child whose
  landing or close is all this workspace waits for: those are told their
  landing, close and questions already (a child) or by subscribing to
  the default lifecycle subscription (`workspace subscribe WS`, optionally
  narrowed to `-events landed,closed`) (anything else). Unsubscribe once the tight coordination
  ends.
