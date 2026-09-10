# STATE — owner 5 (plan B14–B16: failed, hibernated, merging/done/parked)

Branch `overhaul/int-play-05`, rebased onto overhaul/integration at 359f08557.

## Twice-green
Runs 7 and 8 on the current tree, both green, every picture read against its
manifest sentence and matching. Logs `play05-run7.log`, `play05-run8.log` in the
owner-05 scratch dir; pictures under `out-run7/`, `out-run8/`.
Each run also passes the three `TestPlaytestSandbox*` font checks, which is the
lead's own confirmation that the image in use is the rebuilt one.

## Production defect fixed this session
- `fix(shim/compaction): a summarizer's plan mode is not the session's posture`
  The compaction's throwaway `plan`-mode query resumes the user's own vendor
  session id, so `plan` became the last `permissionMode` the transcript stated,
  and `engine/cold.ts` restores a resume's posture from exactly that field. A
  parked session revived in plan mode. The compaction's summary line — the last
  record it appends — now states the session's own mode.
  - unit: two in `shim/test/engine/compaction.test.ts` (the field; and the
    read-back through `readTranscriptFacts` after a plan-mode throwaway line).
  - integration: `TestRevivalRunsInTheSessionsModeNotTheSummarizers` in
    `e2e/hibernation_e2e_test.go`. Verified to FAIL with the fix reverted,
    reporting `[mode=plan]`.

## Playbook corrections
- B.15's revival sentence named "nothing between them"; the product draws the
  park's own context cut there, and a second one after the revived turn because
  this playbook compresses the idle cutoff. Both are named, and both the
  compaction row and the topbar's mode are now page assertions.

## Earlier session (already committed on this branch)
The four daemon fixes and their integration tests (drain republish x2,
workspace session records, footer/topbar park exemption) — see git log.
