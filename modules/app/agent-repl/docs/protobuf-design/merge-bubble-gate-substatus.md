# Merge bubble: one home, a gate substatus, and suite counts

Owner requests, 2026-10-08. Every contract change below was pre-approved by
the owner; this records what changed and why, in plain words.

## 1. A merge draws nothing in the expanded footer

A merge used to show itself twice: the merge bubble in the feed, and the
expanded footer's merge tests panel, which the daemon also opened on its own
(a new "focus" every testing round). The owner asked for the bubble alone.

What was retired from the footer contract:

- The merge tests panel (the expanded footer's `merge_tests` panel) and its
  per-suite row messages.
- The 🧪 chip on the strip. Its only job was to open that panel, so with the
  panel gone it would have been a control that opens nothing.
- The merge tests arm of the daemon's focus, so a merge never opens the
  expanded footer.

Each retired field's tag and name are reserved. The strip's merging status,
substatus and activity line are unchanged. The merge bubble's tests tab
already carries every suite, so nothing the panel said is lost.

On the webapp, a page that last had the retired panel open had remembered it
in its browser storage; that stored value is now forgotten quietly instead of
raising a warning.
