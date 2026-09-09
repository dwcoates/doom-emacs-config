# Owner 8 (C22-C24, composer extras) -- paused state

Branch `overhaul/int-play-08`, worktree `integration-agents/play-08`, cut from `overhaul/integration` at b5d4c0b18.

## Authored (all committed, trailer on every commit)
- `modules/app/agent-repl/e2e/playtest_08_composer_extras_test.go`: three playbooks
  (TestPlaytestContextPrompts = C22, TestPlaytestClipboardImageAttachment = C23,
  TestPlaytestHistorySearchRecall = C24). Artifacts under `playtest/08-composer-extras-*/`.
- Production fix (2c26bb44f): the composer's `[image attached: ...]` marker rode INSIDE the
  submitted text block. `agent-repl--image-insert-marker` now propertizes the marker span
  (`agent-repl--input-image-marker-property`, defconst in lisp/input.el) and
  `agent-repl--read-input-buffer` strips it. Unit tests in test-clipboard-image.el (4) and
  test-input.el (2); byte-compile clean; lisp aggregate 3737/3737.
- Integration test (9e1253fc7): `TestEmacsAttachedImageMarkerNeverRidesAsWords` in
  e2e/emacs_composer_e2e_test.go, spec scenario 25a in EMACS-LAYER-SPEC.md. Compiles; NOT yet run
  in the sandbox.

## Runs
- Run 5 (all three playbooks, commit 44f6366d8): GREEN, first of the two required consecutive
  green runs. Captures inspected in earlier runs: C22 04/05/06 match their sentences; C23
  bubble capture shows `unsupported block: image` (filed, see below); C24 matches.
- The C23 `05-composer-thumbnail.png` from run 5 (1286 colors) is NOT yet inspected.

## Filed (not mine to fix)
- Daemon: `feed.UnproducedImageResolver` -- a prompt's ImageBlock draws as `unsupported block:
  image`; no asset origin resolves a path to a src. Deliberate gap per
  daemon/internal/resolve/feed/unproduced.go; needs an owner ruling.
- Lisp/product: `agent-repl-attach-clipboard-image` captures via osascript only; on Linux/X it
  refuses ("no image found on the clipboard") even with an image on the clipboard.
- Substrate: the FIRST capture in a world taken right after `openPanel` photographed a stale
  framebuffer (481 colors, webview unpainted, composer text undrawn) in 3 of 3 runs while the
  next capture ~100ms later was painted. Mitigated in C23 by an `awaitArm(:none)` after
  `openPanel` (as owner 4 has); root cause not found.
- Plan text says `SPC TAB e`/`E`; the product binds `SPC j e e`/`SPC j e E`.

## Next
1. Inspect run 5's C23 thumbnail capture.
2. Second consecutive green run of the three playbooks.
3. Rebase onto overhaul/integration 86f038026 (merge-tree dry run: clean).
4. Gates after rebase: gofmt/vet (both tag sets), ordinary host e2e package, byte-compile,
   lisp aggregate; run the new Emacs-layer scenario 25a in the sandbox.
5. Final report.
