# Work-account harness compaction

Work (multi-repo) accounts must never pay for the HARNESS's own compaction —
the shim's throwaway summarizing session — while the VENDOR CLI's own
auto-compaction stays on for every account. Before this change the inverse was
implemented: work-account shims were spawned with `DISABLE_COMPACT=1`, the
vendor never compacted, and a work session (puzzle-analysis-visuals,
2026-09-30) filled its 1M window until every prompt failed "Prompt is too
long". The vendor switch was removed separately; this record covers the
contract changes needed to switch off the harness compaction per account.

## Landed changes

### Hibernation needs no contract change (retracted proposal)

- A `compaction` oneof on `shim.v1.HibernateRequest` was proposed and briefly
  landed, then withdrawn before any consumer used it.
- ROOT CAUSE of the error: the proposal trusted the stale header comments of
  `shim/v1/endpoint_hibernate.proto` and `agent-shim/claude/shim/src/engine/compaction.ts`
  (documentation tier), which still say hibernation compacts. The code
  (`session.ts` `hibernate()`) shows the owner ruled on 2026-09-20 that
  hibernation never compacts for any account; it only checks freeness and acks.
- So the cold gate is the ONLY harness compaction trigger, and gating it is the
  whole change.

### frontend.v1.FeedColdGateStanding.compact is optional

- WHAT: `frontend.v1.FeedColdGateStanding.compact` is now `optional`; UNSET
  means this account does not offer compaction.
- WHY: the cold gate's compact choice runs the same harness compaction
  (`conversation.v1.SessionColdCompact`), which work accounts must not pay
  for.
- CONSEQUENCES:
  - The daemon omits the menu for work accounts and its AnswerColdGate verb
    refuses a compact answer when none was served.
  - The webapp draws only pay and clear when the menu is absent.
  - Adding `optional` to a message field is wire-compatible.
