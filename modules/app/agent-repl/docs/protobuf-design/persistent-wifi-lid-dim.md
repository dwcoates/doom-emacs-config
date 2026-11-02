# Persistent wifi mode: the lid-aware display

Owner request, 2026-11-02. While persistent wifi mode is on, closing the laptop
lid dims the built-in display fully (0), and opening it restores the brightness
the display had the moment before it closed. With the lid open the mode leaves
the user's brightness alone, which replaces the old dim-on-enable. Turning the
mode off restores the pre-close brightness when a lid-close dim is in force,
and otherwise leaves brightness alone (the old jump to 1.0 is gone).

Design mode: AUTONOMOUS (owner pre-approved contract changes).

## Assumptions (step 1)

- Objective: the display follows the lid, not the mode switch, while the mode is on.
- Built: a lid watcher inside agent-repld (IOKit clamshell interest notification
  through cgo), a durable pre-close brightness, a reconciled display step.
- Frontend: yes. The topbar's persistent-wifi chip and the Emacs standing
  echo both state the lid.
- Systems: proto (agentrepl.v1, frontend.v1), daemon (persistentwifi, wsm,
  topbar resolver, composition root), webapp topbar chip, Emacs
  `lisp/persistent-wifi.el`, the integration harness, the e2e Emacs world.
- Non-additive changes: the display step's two arms change meaning
  (`dimmed` is now "dimmed for a closed lid", `restored` is "restored to the
  pre-close level"); the fixed 0.0625 and 1.0 levels are retired.

## Iteration sequence

1. `agentrepl.v1.PersistentWifiState` (the shared standing).
2. `agentrepl.v1.UpdatePersistentWifiModeDisplay` (the step outcome).
3. `frontend.v1.TopbarPersistentWifi` (the chip).

## Landed changes

### 1. The lid and the lid dim on the standing

WHAT: `PersistentWifiState` gains a `lid` oneof (open, closed, no lid,
unwatched with its cause) and an optional `lid_dim` carrying the level the
display returns to.

WHY: the owner wants the lid state visible wherever the standing is shown,
and a dim in force is a fact the user must be able to see (their display is
at 0 and will come back to a stated level).

Consequences:
- The lid oneof is UNASSIGNED only for the instant between the boot read and
  the watcher's subscription; readers draw it as not yet heard.
- `lid_unwatched` is the explicit degraded state of a failed or ended
  subscription. There is no polling fallback, so the standing says it loudly.
- `lid_dim` is independent of the lid: a restore that failed leaves it set
  with the lid open, which is honest (the display is still dimmed).
- No-lid hosts (desktops) are a real configuration, so `no_lid` is an arm
  rather than a degraded state.
- The pre-close level persists in the daemon's wsm store (a singleton row),
  the daemon's one durable store; the mode itself keeps no agent-repl state
  (its truth is `pmset`), so wsm is the only home that survives a restart and
  a handover alike.

### 2. The display step's outcome

WHAT: `UpdatePersistentWifiModeDisplay` gains an `unchanged` arm; `dimmed`
carries the level it will restore to; `restored` carries the level it
restored.

WHY: with the lid open the mode no longer touches brightness, so the
commonest outcome of the step is now "nothing to do", which must be stated
rather than mislabelled as dimmed or restored.

Consequences:
- `dimmed` now only answers turning the mode on with the lid closed.
- `restored` now only answers turning the mode off while a lid dim was in
  force; turning off otherwise is `unchanged`.
- A brightness READ failure (the pre-close level could not be taken) is the
  `failed` arm, so nothing is dimmed without a level to come back to.

### 3. The chip's lid arm

WHAT: `TopbarPersistentWifi` gains a `lid` oneof of four empty arms; the
daemon's tooltip names the lid, the dim in force and an unwatched cause.

WHY: the owner asked for the lid in the topbar. Following the wifi arm's
precedent the arm rides as a data attribute and the tooltip says it, so the
chip's paint is unchanged (no look-and-feel change).

Consequences:
- The tooltip keeps the owner's sentence first and appends the lid sentence.

Obviated-declaration sweep: the constants `brightnessDimmed` and
`brightnessRestored` in the daemon die; no proto declaration is obviated.
