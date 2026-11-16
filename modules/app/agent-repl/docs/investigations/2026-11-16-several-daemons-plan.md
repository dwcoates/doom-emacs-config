# Several daemons at once: the newest one in charge

Owner rulings, 2026-11-15 and 2026-11-16:

- Old daemons stay until none of their workspaces is still busy, however long
  that takes, and nothing is interrupted to hurry a handover.
- The newest daemon serves the queue, runs the deploy check and takes deploys
  as soon as its own startup completes, whatever older daemons still hold.
- A workspace an old daemon frees moves straight to the newest daemon, never
  through each generation in turn.

Phase 1 (0.446.0): the queue admits the first merge whose workspace this
daemon serves, and a merge an old daemon keeps goes back to its place in line.

## Phase 2: the newest daemon, at the end of the transfer pass

Today the successor takes over when the incumbent EXITS: only then does it
take the boot lock and `daemon.addr`, say `updated` (which starts the deploy
check), and accept a deploy of its own. One busy workspace on the incumbent
holds all of it.

The incumbent's TRANSFER PASS ends once every workspace it serves has either
moved or been registered behind work of its own (`completeHandover`'s unfolded
and folded requests). At that edge the incumbent YIELDS its boot claim
(`daemonaddr.Claim.Yield`: the serving lock and the boot lock released, the
listener kept), so the successor's kernel wait for the claim returns and its
takeover runs. The incumbent keeps serving its holdouts and transfers each as
it frees, then exits as it does today.

The takeover's steps split by what they need:

- Needs only the boot claim, run at the takeover: promote the handle,
  publish `daemon.addr`, adopt the stragglers no live daemon serves, review
  the queues, pay owed reports, judge adopted shims and say `updated`.
- Needs NO older daemon at all, run once every older daemon has exited:
  contract migration steps (a running older build may still read what they
  remove), recovery of shims no daemon owns (an older daemon's live shims are
  its own), and the creates an outgoing daemon held for the network.

"No older daemon is running" is a kernel fact, not a guess: every daemon holds
an exclusive record lock on `daemons/<instance>.lock` in the state root for
its whole life (`daemonaddr.HoldLife`). The newest waits for every other
instance's lock in the kernel (`daemonaddr.AwaitOthersGone`); the kernel frees
a lock with its holder, however it died.

## Phase 3: a freed workspace moves to the newest daemon

An incumbent's transfer names its successor, fixed when the handover began.
When a newer daemon has taken over since (`daemon.addr` names another daemon),
a holdout freed later goes to the NEWEST daemon, read at each move. The newest
was never told of it in a manifest, so the outgoing daemon announces the move
to it directly (`AcceptHandedWorkspace`), arming its adopt rendezvous for that one
workspace, and then transfers it there exactly as it would to its own
successor.

## Open questions

- A FRESH boot (no handover) that finds the boot claim free while a yielded
  older daemon still serves its holdouts would recover that daemon's live
  shims as orphans. It arises only when the newest daemon died while an older
  one still served something. The fresh boot should defer its orphan recovery
  to the same life-lock wait the takeover uses (`daemonaddr.LiveOthers`).
- `acceptHandedBound` (1s) comes from an idle-host measure of a loopback
  unary (6-7ms); it should be measured again under a loaded deploy.
