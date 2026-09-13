# Store volume: what is written, and what to stop writing

Owner ruling (2026-09-13): volume is not solved by narrowing discovery.
Analyze what the store keeps and stop keeping what is never served.

## Measured (copy of the live events.db, 2026-09-13 00:28: 1.64 GB, 612,214 rows)

| kind | rows | share of rows | bytes | served to any reader? |
|---|---|---|---|---|
| attachment/hook_success (vendor_specific residue) | 229,013 | 37% | 218 MB | no: the stream plane owns the served hook row; the transcript copy is unjoinable residue |
| page_line (transcript lines) | 216,244 | 35% | 452 MB | yes: feed pages |
| attachment/total_tokens_reminder | 57,376 | 9% | 45 MB | no |
| queue-operation | 18,458 | 3% | 34 MB | no |
| last-prompt / mode / ai-title / permission-mode | 37,611 | 6% | 16 MB | partly (mode, permission) |
| user_prompt | 8,712 | 1% | 20 MB | yes |
| file-history-snapshot | 4,006 | <1% | 63 MB | no |
| unparsed residue | 348 | <1% | 24 MB | no |
| write_ledger (absorption index, never pruned) | 735,329 | n/a | 204 MB with indexes | internal |

99.65% of rows come from the sidecar (file plane); the shim's stream plane is under 1 MB.

## Proposal

1. Do not store residue kinds that no reader serves: `attachment/hook_success`,
   `attachment/total_tokens_reminder`, `queue-operation`, `file-history-snapshot`.
   Removes 47% of rows and ~360 MB. The sidecar still READS every line (the
   discovery mandate is untouched); it just does not persist unservable ones.
   Needs an explicit allowlist of residue kinds worth keeping, in the sidecar's
   AGENTS.md, so "unknown kind" defaults to kept (forward-compat) and only the
   named never-served kinds are dropped.
2. `write_ledger` gets a retention rule: entries older than the newest
   transcript offset they could ever absorb again are prunable (design
   question: absorption correctness vs growth without bound).
3. Residue is stored as `google.protobuf.Struct` (≈ the source JSON size);
   storing the raw line bytes once (blob) instead would roughly halve the
   bytes for `page_line`, but it changes the store's read contract. Later.

The owner rules on 1 and 2 before anything lands.
