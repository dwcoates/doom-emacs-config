# Zettel deploy-check labels

The merge queue's deploy-check label beside a repository's name reads the
product's name, Zettel, which will soon replace "agent-repl" everywhere, and
gains a stage for the time a remediation's fix is being tested (owner
ruling, 2026-11-16). The words are fixed text, the same for every
repository; only agent-repl's repository opts into deploy checks today.

## Assumptions

- Autonomous design mode: the owner asked for the shape to be approved by
  the implementer.
- Systems: the daemon (`merge` stages, `resolve/sidebar`), the webapp
  sidebar, and the editor's roster decoder (`lisp/wire-roster.el`).

## Landed changes

### `frontend.v1.RosterMergeQueueMediation.standing` gains `testing`, retires `finishing`

- WHAT: arm `testing` (tag 6, `frontend.v1.RosterMergeQueueMediationTesting`
  with a required target) is added. Arm `finishing` (tag 5) and its message
  are removed; the tag and name are reserved.
- WHY: the owner's stages are four: hot-loading (a deploy rolls out),
  assessing health (the intake checks it), fixing issues (a remediation
  fixes), testing fixes (the finish's gate and the short landing after it).
  `finishing` drew one label over gate, landing and deploy. Its deploy step
  is a deploy rolling out, so it is drawn as `waiting`; a gate that sends
  the workspace back is drawn as `fixing`. Nothing of `finishing` is left
  that `testing` and `waiting` do not cover, so it is retired rather than
  kept beside `testing`.
- Words, written lowercase so the header's uppercase styling draws them:
  `hot-loading zettel`, `assessing zettel health`, `fixing zettel issues`,
  `testing zettel fixes`.

## Consequences

- `daemon/internal/merge/mediation.go` `rosterMediation` maps a finish's
  `gating` and `landing` to `sidebar.MediationTesting`, its `deploying` to
  `sidebar.MediationWaiting` selecting the owed check's intake (none for an
  escalation deploying after its chain ended), and its `fixing` to the owed
  check (`sidebar.MediationFixing` on the remediation).
- `daemon/internal/merge/remediationfinish.go` republishes the queue at every
  step move: gate passed (landing), landed (deploying), and deploy begun
  (deployed), beside the begin (gating) and resume (fixing) republishes
  already there.
- `sidebar.MediationFinishing` is renamed `sidebar.MediationTesting`.
- The editor's decoder reads `:testing` in place of `:finishing`.
