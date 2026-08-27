# POST-COMPACTION BOOTSTRAP

Load these FULLY into context before any other action — they contain the
real information; this file only names them:

1. docs/implementation/ORCHESTRATION-META.md       (process rules)
2. docs/implementation/daemon.md                   (daemon architecture)
3. docs/implementation/webapp.md                   (webapp architecture)
4. docs/protobuf-design/figma-to-idl-redesign.vetting.md
5. docs/protobuf-design/figma-to-idl-redesign.deferred.md

The CANONICAL design record is docs/protobuf-design/figma-to-idl-redesign.md
— authoritative but large; do not load it whole by default. Read the
per-system digests under docs/protobuf-design/digests/ as needed; load the
full record only for contract-change work. Also load
docs/implementation/<subsystem>.md for any other subsystem being worked
(shim, elisp, store, sidecar).
