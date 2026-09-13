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

1. LANDED (first two kinds), 2026-09-13. Do not store residue kinds that no
   reader serves. `attachment/hook_success` and `attachment/total_tokens_reminder`
   are now classified and then not written — 286,389 rows and ~263 MB, 47% of the
   residue this item names. The sidecar still READS every line (the discovery
   mandate is untouched). The list is NAMED rather than a predicate, in
   `internal/convert/neverpersist.go` and documented in the sidecar's AGENTS.md
   under "residue kinds never persisted", so an unknown kind stays persisted for
   forward compatibility and only the named kinds are dropped. Each drop is DEBUG
   and the boot walk states one INFO summary per file with the counts by kind.

   STILL OPEN: `queue-operation` (18,458 rows / 34 MB) and
   `file-history-snapshot` (4,006 rows / 63 MB), which are top-level withheld
   line kinds rather than attachments, and which the owner has not ruled on.
2. `write_ledger` gets a retention rule: entries older than the newest
   transcript offset they could ever absorb again are prunable (design
   question: absorption correctness vs growth without bound).
3. Residue is stored as `google.protobuf.Struct` (≈ the source JSON size);
   storing the raw line bytes once (blob) instead would roughly halve the
   bytes for `page_line`, but it changes the store's read contract. Later.

The owner ruled item 1's first two kinds on 2026-09-13; item 2 and the
remaining item-1 kinds are still open.
