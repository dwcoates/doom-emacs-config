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

## 4. A merge waiting on the user

While a merge resolves a conflict or fixes failed tests, the workspace's own
session runs the merge's turns, and that session can stop on a permission ask
or a question. The merging status outranks the waiting status, so the strip
used to show only "merging · conflict resolution" and never said the merge
was stuck on the user.

What changed in the footer contract:

- The merging status's substatus gained "waiting on user". It stands while
  the merge is on its conflict-resolution or fixing step and the session has a
  permission ask or a question batch open, and it gives way to the step's own
  substatus the moment the last ask is answered or withdrawn.
- The merging activity line gained two kinds, reused from the waiting status:
  the gated call ("Bash: rm -rf build") and the question batch's lead
  ("2 questions · Which approach?"). Under "waiting on user" one of them is
  the line; a consent ask outranks a question batch, as under waiting.

The coarse claim is unchanged: it is still the merging rung, so the roster row
and the tab bar keep drawing "merging".

How the fact travels: the session watcher hands every permission and question
edge to the footer resolver, which already keeps the open asks per workspace;
the substatus is resolved from those and the merge's step, with no new input.

## 5. The merge bubble's tab says ❓ while the merge waits on the user

The conflicts and fixes tabs' state oneofs each gained a "waiting on user"
arm: a live tab whose agent has a permission ask or a question batch open.
It carries the instant the tab's work began, so the tab's clock keeps ticking
as a live tab's does, and the webapp draws its glyph as the ❓ emoji (the one
emoji among the tab glyphs, by the owner's request). The tab returns to live
when the last ask is answered or withdrawn, and settles as before.

The merge orchestrator reads the asks from the same session-watcher edges the
footer reads, through a small ask set built before the orchestrator (so an
ask opened before a merge resumes at boot is still known). The ask set and the
footer apply the same rule: only conflict resolution and fixing, and any open
ask in the workspace's session. Each redraw of the live tab reads the asks at
that moment under the run's lock, so the last row drawn always says what the
last ask edge left, and a settled tab is never redrawn live behind its own
settle.
