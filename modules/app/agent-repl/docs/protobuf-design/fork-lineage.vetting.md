# Fork lineage — vetting register

Assumptions the landed fork lineage (see `fork-lineage.md`) rests on, and
residuals accepted rather than excluded.

## V1. A same-millisecond, unstamped row of the parent's next turn

- **Assumption:** no row without a turn stamp belonging to the parent's next turn is written in the same millisecond the inherited turn concluded.
- **Affected:**
  - The fork's inherited plane, which selects the parent's rows by `through` and turn stamps.
  - A violation draws one row of the parent's next turn in the fork.
- **Verify:** grep the daemon log on a fork for `daemon.feed.lineage`, then compare the fork's oldest inherited page against the parent's.
- **Status:** ACCEPTED 2026-10-12 (owner ruling). Excluding it structurally needs the store to order rows by turn.

## V2. Only the vendor's session fork stamps `forkedFrom`

- **Assumption:** the vendor stamps `forkedFrom` only on records its `forkSession` copied, never on records a session writes for itself, including sessions started by the CLI's own fork paths.
- **Affected:**
  - The sidecar withholds every record stamped `forkedFrom` (`shim-sidecar/internal/convert/fork.go`).
  - A record the session wrote itself that carried the stamp would never reach the store.
- **Verify:** on each SDK upgrade, grep `sdk.mjs` and the CLI bundle for `forkedFrom` writers.
- **Status:** OPEN. Verified for `@anthropic-ai/claude-agent-sdk` 0.3.280's `forkSession`. The CLI's interactive fork path was not read.
