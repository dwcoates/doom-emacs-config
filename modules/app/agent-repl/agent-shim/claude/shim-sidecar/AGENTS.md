# agent-shim/claude/shim-sidecar/

The Claude file-plane reader (Go, singleton, launchd-managed). Responsibility:
observe the Claude harness's on-disk artifacts (session transcripts, agent
sidechain transcripts, workflow journals, `/tmp` task spools), parse them with
cursored, truncation-aware tailing, convert records into agent-shim protocol
events (same loud-validation contract as the shims), infer terminal `LOST`
transitions per the staleness policy, and write everything to the shim-store
with atomic cursor advancement.

The sidecar is 100% specific to Claude's file formats BY DESIGN; its entire job
is converting that vendor reality into the (treated-as-)vendor-agnostic
protocol. It interprets no resolved state and owns no database.

## Total-ingestion mandate

The sidecar handles ALL JSONL objects in the files it reads. If a JSON object
exists in a file on disk, it MUST end up written to the shim-store as a
protobuf shape — somewhere in the SQLite database, ultimately. No exceptions.
Ever.

- No sampling, no skipping, no "not visually interesting" filtering — curation
  is a downstream (daemon/frontend) concern, never an ingestion concern.
- The mandate binds INGESTION only. Downstream consumers (daemon, frontends)
  are free to never read a stored record, and to ignore records they do
  read — irrelevance to the user is a legitimate consumption-side judgment.
  It is never a legitimate reason to skip parsing a record or to leave it
  out of the database.
- A shape the schema cannot express is a SCHEMA GAP to be surfaced loudly and
  fixed (via the extras-enforcement contract that fails the build on
  undocumented extras), never a record to silently drop.
- The zero-`UnparsedEvent` golden-corpus contract is the executable form of
  this mandate; weakening it violates this document.

## Vendor carry-over (viral)

Any future vendor-equivalent sidecar (e.g. a codex sidecar) MUST inherit this
AGENTS.md's mandates into its own AGENTS.md — including the total-ingestion
mandate above AND this carry-over clause itself, so the directive propagates
to every subsequent vendor equivalent in turn.

Dependencies: `proto/agentshim/` (generated Go), the shim-store UDS socket,
the Claude harness file formats it parses.
