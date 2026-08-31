# STOPPING POINT — webapp system (teamlead), overhaul wind-down

Branch `overhaul/webapp`, worktree `~/.config/doom-overhaul/webapp`. Tip at the
time of the final report is stated there; every merged increment below is green
(`npm run typecheck`, `npm test`, `npm run build` from `webapp/`) except the
reds explicitly assigned below.

## Merged and green

- chassis (legacy/ reference move, dead-code strip, new index.html shell + shell.ts)
- rpc core (Connect transport, strict decode incl. unknown-field refusal, watchStream
  registry/backoff/abort semantics, log.ts over ClientLog, clock, link.ts with
  renderExternalLink/renderEditorLink, vocab.ts over proto/vocab, failure sink+overlay, boot main.ts)
- feed core (pages/tail/upserts by FeedId, ONE bubble plumbing for subagent+merge,
  revealRow breadcrumb walk, prompt/turn-ended/separation/subagent rows)
- feed cards A (response bubble, tool card with five output forms, hook card, paint classes)
- hold tray + dev composer + command panels + refusal card
- footer (status strip, expanded panels, turn stop + agents stop-all)
- sidebar (both groupings, attention cadence, nine workspace verbs, tasks, create form)
- landings 1–5 of the proto remediation and the corrected vocabulary files
  (render-colors.json incl. footer_allowance; paint-classes.json)

## Running at wind-down (merge each on its report, then remove its worktree)

- cards B + asks (worktree webapp-agents/feed-cards-b): skill/artifact/plan/findings/shell
  cards, permission/question/cold-gate asks, typed Answer*/Interrupt refusal arms, cold-gate
  compact scope trace.
- landing-3/4 remediation (worktree webapp-agents/landing-3): tool-card input form +
  none output, turn-ended headline (deletes the client sentence table), detached confirm
  removal, FooterAllowance oneof (UNSET legal) via vocab footer_allowance, Interrupt arms
  at the three stop controls, FeedResponse.notice register. OWNS the two known reds:
  src/footer/strip.ts type error + test, tool-card output-form arm guard.
- integration suite (worktree webapp-agents/integration-suite): fake daemon (flush-on-accept),
  fixtures, harness, 11 suites; landing-4/5 arm tables; the ruled token-format table pinned;
  test/integration excluded from the main tsconfig until wiring flips it.

## Queued briefs, in dispatch order (all opus-low; brief text in webapp-briefs/)

1. topbar + lifecycle COMBINED — webapp-briefs/COMBINED-topbar-login-lifecycle.md
   (execute lifecycle.md then topbar-login.md; lifecycle carries the FINAL no-redial +
   adopt-at-boot sequence; topbar carries the landed permission-mode picker and the
   server-stream login link + SendLoginInput; both carry the landing-4 typed-refusal rule).
   Worktree webapp-agents/topbar-login exists; the lifecycle worktree was folded into it.
2. merge bubble — webapp-briefs/merge-bubble.md (+ its landing-4 SelectWorkspace note).
   Worktree webapp-agents/merge-bubble exists.
3. landing-4 refusal pass — webapp-briefs/landing-4-refusals.md (typed arms at every
   already-merged call site; shared refusalSentence helper in src/rpc/refusal.ts).
4. wiring/seam agent (LAST) — duties:
   - src/main.ts: adoptAtBoot BEFORE any stream; mount order sidebar/topbar/feed/tray/
     footer/composer(dev)/login/lifecycle; keep `import "./styles.css"`.
   - Assemble the RowRenderers registry (15 keys; commandPanel/commandRefused from
     src/panels; mergeHead/mergeBody from the merge-bubble agent's modules).
   - Composer gate from FooterHandle.onStatus (closed on merging/closing/disconnected,
     reason = the footer's word); ComposerFactory into mountFeed.
   - THREE MODULES MISSING until briefs 1–2 land: src/topbar, src/login, src/lifecycle,
     src/feed/merge — wiring goes out only after them.
   - Flip the tsconfig exclusion of test/integration back in; reconcile every harness
     import path and §5b DOM hook (webapp-briefs/WEBAPP-AGENT-PREAMBLE.md §5/§5b).
   - Formatter unification per the daemon's canon (rules + example table in
     webapp-briefs/DISPATCH-NOTES.txt): one shared src/format.ts, cold gate adopts it,
     cards-B tests updated to the table.
   - Delete legacy/, prune styles.css dead sections, rewrite webapp/AGENTS.md.
   - Then the teamlead runs `npm run test:integration` and drives the remediation loop,
     then fresh-context fable auditors per TEAMLEAD.md until clean.

## Open UX questions for the user

webapp-briefs/UX-LIST.md is the running list (footer/topbar/sidebar/tray/cards choices,
working rulings on interrupt placement, prompt markdown fidelity, empty-tray treatment,
panel auto-close, and the cards-B accepted rulings). The proposed cold-gate copy ships in
src/feed/asks/cold-gate.ts as COLD_GATE_COPY (one object, one place to reword).

## Worktrees left in place at wind-down

webapp-agents/{topbar-login, merge-bubble} (queued, untouched at base) and whichever of
{feed-cards-b, landing-3, integration-suite} had not yet merged when the final report was
sent (each holds committed work on its overhaul/webapp-* branch; resume by SendMessage to
the agent id in the session ledger, or re-dispatch fresh from the brief if the session is gone).

## Standing constraints that survive the pause

3-concurrent-agent fair-share cap; new dispatches at opus-low with the sonnet-medium
offload allowance (preamble §0b); no proto edits ever; pause→ack→merge on every landing;
the piecemeal/no-redial/adopt-at-boot rulings as written in the briefs.
