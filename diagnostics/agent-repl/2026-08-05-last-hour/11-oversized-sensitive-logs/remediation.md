# Remediation: enforce bounded, transition-oriented diagnostics

1. Normalize logged predicates to booleans. For the metaprompt gate, record only enabled state, prompt length, and a nonreversible version identifier when needed.
2. Replace perspective `%S` output with workspace name, perspective identity, buffer count, modified count, and saved count.
3. Inventory operations firing more than once per second and either remove their per-poll record, emit only on state change, or aggregate counts over a bounded interval.
4. Deduplicate response diagnostics by API message ID and payload fingerprint while preserving the first event and a bounded repeat count.
5. Add tests that reject known metaprompt text, perspective printed representations, oversized messages, and repeated identical warning emission.
6. Keep error decisions and all causal identities. Data minimization must not remove the fields needed to diagnose a branch.

Success criteria: no prompt body or perspective structure appears in JSONL, repeat volume drops materially, and every retained warning remains identity-complete.

Protobuf decision: no change.
