# A deploy's hot reload, drawn in the deploy mediator's feed

The owner asked (2026-11-20) for a bubble beside the deploy mediator's "testing
the fix" bubble (`frontend.v1.FeedFixGate`) that shows a deploy's hot reload:
which services are scheduled, building, being reloaded and fully loaded, with
durations, in the same look as the testing bubble. Each service is selectable:
selecting it shows that service's detail (for the shim, every workspace's shim
and its state), with a back control to the list. The detail should give the
pertinent facts without noise, so the reader learns what a hot reload does.

Designed autonomously (create-or-update-protobufs, autonomous mode).

## Assumptions

- ASSUMPTION (objective): one card per runtime deploy (`deploy.Deployer.Deploy`),
  drawn in the deploy mediator's root feed of the repository the runtime is
  deployed from, as `frontend.v1.FeedTurnActivity.hot_reload`. A repository
  with no deploy mediator draws no card.
- ASSUMPTION (systems): proto, daemon (deploy, rollout, server, wiring), webapp.
  Emacs lisp draws no feed row and is untouched.
- ASSUMPTION (non-additive): none on the wire. The daemon's `deploy.Builder`
  gains a per-step observer, and the rollout's handover manifest carries the
  card to the successor.

## Iteration sequence

One frontend component (the card), walked top-down: card → service → step and
instance → the shared state vocabulary. No rpc: the selection is the reader's
own view state and never reaches the daemon.

## Landed changes

### `frontend.v1.FeedHotReload` (feed.proto, "A deploy's hot reload")

- WHAT: a new `FeedTurnActivity` unit `hot_reload = 14`, and its family:
  head (its own `FeedHotReloadLabel`, as the fix gate has `FeedFixGateLabel`), a live/settled badge reusing
  `FeedMergeTabLive`/`FeedMergeTabSettled` (the fix gate's badge vocabulary),
  shared steps, and services. A service carries its name (the reader's
  selection key), a state, an optional instance summary, an about sentence,
  optional builds (previous → fresh short hashes), its own steps, and its
  instances.
- ONE STATE VOCABULARY (`FeedHotReloadState`) for services, steps and instances:
  scheduled, running, waiting, done, unchanged, failed, skipped. Every arm
  carries daemon-resolved words; the timed arms carry their epoch-ms bounds and
  the client ticks or spans them. `waiting` is separate from `running` because
  a stale shim behind a turn, or a service waiting on the successor daemon, is
  not the deploy working, and the owner's status colors draw it still and grey.
- WHY THE DETAIL IS NOT ITS OWN MESSAGE: the detail view draws only facts the
  service row already carries (about, builds, steps, instances), so a second
  message would be a second copy of one component's props. The list row draws
  name, state and summary; the detail draws the rest.
- WHY NO SELECT EVENT: the selection is per-reader view state, kept by the
  client per row; nothing about it is the daemon's to know.
- Consequence (accepted): the card is replaced whole on every push, so the
  client keeps the selected service name outside the drawn element and redraws
  the detail on each push.
- Consequence: the daemon is the one author of every word, count and order; a
  deploy that hands the daemon over hands the card on in the rollout manifest,
  so the successor goes on drawing the same row, and only one daemon writes it
  at a time (the incumbent until the manifest is written or the handover fails,
  the successor from its takeover).

Obviated-declaration sweep: nothing obviated (purely additive).
