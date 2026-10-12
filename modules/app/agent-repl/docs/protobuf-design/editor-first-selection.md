# Editor-first workspace selection

## Problem

A workspace switch in Emacs waits on the daemon before anything moves: the
keystroke sends `SelectWorkspace`, and the frame and tab bar follow only when
a roster push names the new `current`. Measured 2026-10-12, that round trip is
45-50ms of every switch (select applied in 2ms, the 153-row roster republished
at +15ms, then decoded in Emacs), against an owner target of low tens of
milliseconds for the whole switch. The owner approved moving the frame on the
keystroke and letting the daemon catch up, provided the ordering between the
editor's own selections and the roster's `current` stays structurally
guaranteed rather than probable.

## Landed changes

(none yet)
