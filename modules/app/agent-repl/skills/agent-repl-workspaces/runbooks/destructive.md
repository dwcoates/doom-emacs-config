# Destructive and gated acts

- **CRITICAL: `workspace kill` runs ONLY when the user explicitly asked for
  that act or explicitly approved it.** The daemon cannot observe that
  approval, so this rule is yours to keep.
- `workspace pass-gate WS` passes WS's standing cold gate by the Haiku
  summarizer's compaction of all its context; the prompts the user left held
  at the gate stay held for the user. It takes no answer and no flags: paying
  the cold read and clearing the context are never offered to an agent and
  stay the user's answers on the gate card. `workspace status` names the gate.
  Refusal arms: `unknown_workspace`, `workspace_ref_mismatch`,
  `transferring_away`, `not_yet_adopted`, `no_cold_gate` (no gate stands),
  `unserved_remediation` (the gate offered no Haiku compaction of all the
  context). When to run it is the metaprompt's "Child cold gates".
