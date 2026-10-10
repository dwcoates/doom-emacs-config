# A held prompt waits at the cold gate, and never claims a classification nobody is running

The incident (doom workspace `3e2d9cadc6794e13`, 2026-10-10 21:36): a prompt
sent to a workspace with no live session was held for the revival. The
revival brought the shim up parked at its cold gate. The queue logged "the
session is up and the vendor serves it", released the hold, and tried to
deliver into a session that did not exist ("the workspace has no session
watcher", ERROR "a prompt released from the after-reconnect hold was not
delivered"). The release had already un-stamped the hold, so the prompt sat
in `held_prompts` with no hold and no verdict. Nothing re-drove it, and the
tray drew it as "queued — classifying", because a record with no verdict
defaulted to the `classifying` arm.

The owner's rulings (2026-10-10):

- A prompt never opens the cold gate. Judgement call 11 ("The cold gate's
  feedback", 2026-09-14) and owner ruling 2 (2026-09-13) stand: only the
  gate's own answer spends the user's money.
- A prompt waiting behind the gate shows that it is waiting on the gate.
- A held prompt whose delivery failed is never stranded.
- `classifying` is drawn only while a classifier call is in flight.

## Landed changes (`frontend/v1/daemon_hold.proto`)

### 1. `HeldPrompt.hold` gains `cold_gate = 24` (`HeldPromptColdGateHold`)

- **What it says:** the workspace's session is parked at its cold gate, and
  the prompt waits for the user's answer.
- **Exits:** delivery when the answer brings the session up, through the same
  session-up edge (`promptqueue.Queue.ReleaseReconnectHolds`) that releases
  `reconnect`, or the user's cancel. No classifier runs and there is no
  force-through.
- **Why its own arm, not `reconnect`:** a reconnect waits on machinery that
  ends by itself. A gate waits on the user, and the card has to say so.
- **Durable:** `wsm.HoldColdGate` (stored value 4). The change is additive,
  so the wsm schema version is unchanged (AGENTS.md "The `wsm schema version`
  goes up only for a breaking table change").

### 2. `HeldPrompt.classification` gains `awaiting_delivery = 23` (`HeldPromptAwaitingDelivery`)

- **What it says:** no classifier is running and no daemon condition holds
  the entry. It waits its place in the queue, and the next drain delivers it.
- **Why:** the queue stamps `wsm.ArmClassifying` before every classifier
  call. A record with no verdict and no hold therefore means nothing is
  classifying it, and drawing `classifying` there was false.
- `classifying` keeps its meaning and its "queued — classifying" badge, set
  only while a call is in flight.

### 3. `HeldPromptBadge.stands_for` gains `cold_gate = 14` and `awaiting_delivery = 15`

- The one-badge precedence (`held-prompt-one-badge.md`: editing, then the
  hold, then the verdict) is unchanged. `cold_gate` is a hold arm and ranks
  with the holds. `awaiting_delivery` is a verdict arm and ranks with the
  verdicts.
- **Words:** `cold_gate` reads "awaiting cold gate" (a badge label is one to
  three words, `TestHeldStatusLabelsAreOneToThreeWords`), detail "held until
  the cold gate is answered; a prompt never answers it". `awaiting_delivery` reads
  "queued", detail "waits its turn in the queue; nothing is classifying it".
- **Webapp colors:** `cold_gate` is teal, like `reconnect`, because the
  session is not up. `awaiting_delivery` is muted, because nothing is acting
  on it.

## Daemon behavior that goes with it

- The session-up release (`releaseReconnectHoldsLocked`) releases only into a
  session that serves: no cold gate stands, a session is started, and its
  watcher is in place. Behind a standing gate, the session-waiting entries
  (reconnect holds and free entries) are stamped `cold_gate`. With no started
  session, they are stamped `reconnect`. The release also drives free
  entries when nothing runs, which is what re-drives an entry stranded by an
  older build.
- The pop (`mayPop`) delivers nothing into a parked session. A delivery that
  finds no session (`deliverHeld`) puts the entry back on the hold that names
  why, so the next session-up edge delivers it. A no-session delivery the
  gate does not explain keeps its ERROR.
- The revival logs "revived" only for a session that is up. A revival that
  parks at the gate says so.
- At boot, every adopted shim with no started session (a parked one) has its
  holds settled once its carried gate stands. Its prompts wear `cold_gate`,
  and the answer delivers them.
