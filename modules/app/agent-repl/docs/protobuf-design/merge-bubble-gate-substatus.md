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

## 2. The merge bubble stays folded until the merge fails

The bubble's fold (`FeedMergeFold`) was shipped folded for every state and
read by the webapp at the bubble's first draw only. Now:

- The daemon ships it folded while the merge is queued or running, when it
  lands, and when it is abandoned; it ships it open once the merge has failed.
- The webapp applies the fold at the first draw and again whenever a push
  changes it, once. A push that repeats the fold changes nothing, so a reader
  who closes the failed bubble keeps it closed.

No field was added; the change is to the field's documented meaning.

## 3. Suite counts and the running suite's dot

Two owner requests on the merge bubble's tests tab:

- Each suite's row carries its test counts, drawn "3/1/12" on the right
  (passed in green, failed in red, the total in the row's own color). The
  suite message gained a counts message with the three figures. It is unset
  while the daemon does not yet know how many tests the suite runs, and
  nothing is drawn then.
- A running suite's dot is never purple. The running state gained a oneof
  saying what the suite's tests have said so far: nothing yet (grey), every
  verdict a pass (green), or at least one failure (red). The daemon resolves
  the arm from the counts; the webapp only paints it. Finished suites keep
  their green and red.

Where the numbers come from: the test runner (`testrun`, behind
`bin/test-all.sh`) schedules each suite as units (a Go package, a chunk of
test files, a harness script) and prints one verdict line per unit. It now
also prints, as a suite starts, how many units the suite runs. The merge gate
counts each suite's unit verdicts against that total: "ok" is a pass,
"FAILED" or "NOT RUN" (cancelled because a unit it depends on failed) is a
failure, and a declined unit is neither. Units are the finest grain the
runner reports a verdict for; per-test counts inside a Go package or a vitest
chunk are not reported by the runner today.
