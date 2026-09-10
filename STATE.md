# Owner 12 (E37-E41, permissions / questions / mode picker) — STATE

Branch: overhaul/int-play-12, rebased onto overhaul/integration 359f08557.

## Runs
- run-1 (scratch owner-12/run-1.log): AllowFamily PASS, DenyFamily PASS, HoldSurvivesASwitch PASS,
  QuestionFamily FAIL (unanswered arm), ModePicker FAIL (mode reveal never opened on a wrap click).
- run-diag (scratch owner-12/run-diag.log): ModePicker PASS after clicking `.topbar-mode-button`;
  QuestionFamily FAIL on `!ask-single`'s settle conjunction, with the page reading SETTLED at the
  final poll — split into two guarded awaits to name the failing half.

## Next
1. Full-section run on the rebased tree.
2. Diagnose the ask-single settle half from the split awaits.
3. Two consecutive greens, then read every PNG against MANIFEST.md.
4. Drop this file in the last commit.
