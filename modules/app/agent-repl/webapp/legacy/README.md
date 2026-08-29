# `legacy/` — reference-only renderers from the pre-Connect webapp

These files are the OLD renderers, kept verbatim for ONE purpose: the new
components must port their LOOK AND FEEL. The overhaul's standing directive is
that the existing look and feel does not change except where the new feature
set or a prescription requires it, and these files (with `src/styles.css`) are
the record of what that look was.

## Rules

- **REFERENCE ONLY. Importing from `legacy/` is forbidden.** No file under
  `src/`, `test/`, or `index.html` may import anything here, directly or
  transitively. The new components are written fresh against the generated
  Connect clients and the `frontend.v1` view messages; this directory is read
  with the eyes, not by the module resolver.
- **These files do not compile and are not meant to.** They still import the
  hand-decoded transport and store modules (`ws.ts`, `protocol.ts`,
  `store.ts`, `frontend-proto.ts`, …) that the port deleted, and their relative
  imports (`./store.js`) no longer resolve from this directory. That is
  deliberate: rewriting them to compile would be maintenance on code that is
  scheduled for deletion. They are therefore excluded from `tsconfig.json`'s
  `include`, from vitest, and from the vite build (nothing reachable from
  `index.html` imports them, so the bundle contains none of it).
- **Their tests were deleted with the move.** The old suites are not a source
  of truth for the new components; coverage is rebuilt from the contract.
- **This whole directory is deleted by the final seam agent** once the new
  components have landed and the look has been ported.

## What is here, and what it is the ancestor of

| file | the look it records |
| --- | --- |
| `render.ts` | every feed row: response bubbles, tool cards, permission and question cards, separations, folds |
| `sidebar.ts` | the workspaces rail (roster rows, status dots, grouping heads) |
| `progress-footer.ts` | the docked status footer (status line, clock, tokens cell, chips, expansion sheet) |
| `topbar.ts`, `topbar-view.ts` | the thin top strip and its reveals |
| `token-breakdown-view.ts` | the context/token breakdown menu |
| `main.ts` | the old boot and wiring (what the new `src/main.ts` replaces) |
| `hibernation.ts` | the blocking revival gate — the cold gate's visual ancestor |
| `merge-dequeue.ts` | the held-offer card |
| `drain.ts` | the scheduled-restart banner |
| `login.ts`, `login-terminal.ts` | the full-screen xterm login overlay |
| `permission-preview.ts` | the permission card's payload preview |
| `prompt-body.ts` | the user/agent prompt bubble body |
| `skill-body.ts` | the skill card body |
| `failure-card.ts`, `local-failure.ts` | failure cards and their tone/classification |
| `unsupported.ts` | the unsupported-command refusal card (with its add-support offer) |
| `catalogue.ts`, `catalogue.html` | the element catalogue: every element's look on one page |
| `merge-gate.ts` | the composer's merge-gate notice |
| `async-bubble.ts`, `async-render.ts`, `subfeed.ts` | detached/subagent bubbles and their nested sub-feed rendering |
| `turn.ts`, `status.ts` | turn framing and status rows |
| `tokens.ts`, `response-usage-stamp.ts` | the token cell and the per-response usage stamp |
| `clear-compact.ts` | the context cleared/compacted dividers |
| `pending-mode.ts` | the permission-mode picker's pending state |
| `account.ts` | the account chip and its menu |
| `lazy-item.ts` | the lazy/capped section reveal |
| `ungated.ts` | the ungated-session banner (the mode itself is now the surface) |
