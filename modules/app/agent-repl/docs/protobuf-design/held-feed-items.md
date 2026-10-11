# Held feed items

The feed's bottom moves while the feed updates. The clearest case is a live
merge bubble: growing at the tail, it slides the feed's bottom under the
footer, because the scroll owner refuses to chase a merge bubble's growth
(`TailFollow.holdForExempt`, webapp/src/scroll.ts), and the next unrelated
update snaps the view back. Held prompts already avoid this, because they
draw in their own region at the scroll content's tail (`#hold-tray`), where
their growth is ordinary content growth that a standing follow keeps in view.

The owner's direction (2026-10-10): items that are live and not yet settled
into the conversation's history are HELD. They sit in ONE section of the feed
at its tail, drawn exactly like the newest rows, and the daemon decides what
is held. A held item growing pushes the feed up, and never scrolls it. When an
item settles (a held prompt is delivered, a merge lands), it takes its feed
key at that moment, which is the bottom, so moving into the feed changes
nothing the reader sees. The daemon folding a merge bubble when its merge
lands remains the only automatic open-to-folded change. Held prompts become
held feed items in the same section, rather than a separate tray on a separate
stream.

## Process

- 2026-10-10, owner: the owner reviews the ASCII layouts of the UI only. The
  protobuf shapes are designed and landed by the orchestrator afterwards,
  without putting them to the owner, checked box-for-box against the agreed
  layouts and against this skill's design principles. A question that only a
  protobuf could raise is put to the owner as a question about the UI or the
  behavior, never as a sketch.

## Landed changes
