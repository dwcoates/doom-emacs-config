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

1. LANDED IN FULL, 2026-09-13. THE SIDECAR PERSISTS NO RESIDUE AT ALL. Nothing
   in the daemon, the webapp or the editor reads `vendor_specific`, `unknown` or
   `unparsed` — zero readers — so the whole class was stored for nothing. Only
   TYPED entries are now written; every residue outcome is classified, counted,
   and withheld.

   That removes every no-reader row this table measures — `hook_success`,
   `total_tokens_reminder`, `queue-operation`, `file-history-snapshot`, the
   unparsed residue, and every other residue kind — leaving `page_line`,
   `user_prompt`'s served half, and the typed rows.

   The sidecar still READS every line: the discovery mandate and the
   classification are untouched, so the counts and the per-record debug records
   still name exactly what was seen. The predicate is `convert.IsResidue`
   (`internal/convert/neverpersist.go`) and it is applied at `cycle.go`'s
   `withholdResidue`, immediately above `storeWrite` — the sidecar's ONLY door to
   the store — so no producer can route around it. `keepalive` rides the same
   `unserved_item` field and is NOT residue: it is a typed fact with no book and
   stays persisted. Documented in the sidecar's AGENTS.md under "Residue is never
   persisted".

   Forward compatibility no longer rests on the stored row. A residue arm nobody
   has modelled is still classified and still counted, and the sidecar's sources
   are the vendor's own durable files — the day it earns a model, the file is
   re-read.
2. `write_ledger` gets a retention rule: entries older than the newest
   transcript offset they could ever absorb again are prunable (design
   question: absorption correctness vs growth without bound).
3. Residue is stored as `google.protobuf.Struct` (≈ the source JSON size);
   storing the raw line bytes once (blob) instead would roughly halve the
   bytes for `page_line`, but it changes the store's read contract. Later.

The owner ruled item 1 in full on 2026-09-13; items 2 and 3 are still open.
