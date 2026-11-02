# Cross-account fork: the parent transcript is imported into the child's account root

## Problem

A fork runs in the CHILD's shim (`shim.v1.ForkSession`). The vendor's
`forkSession` reads the parent transcript from the account root the calling
process runs under (`CLAUDE_CONFIG_DIR`), which is the child's. The daemon
picks a root per repository, so a fork between two repositories on different
accounts (a doom workspace under `~/.claude` forked into an explanation-engine
workspace under `~/.claude-chesscom`) always failed as `unknown_session`: the
parent transcript was not under the child's root. Three "move this workspace"
creates failed this way on 2026-11-02 around 17:44.

## Assumptions (autonomous design)

- Objective: a fork across two account roots works by design, with no retry and
  no fallback search across roots.
- No frontend component; the systems are the daemon (producer of the request)
  and the Claude shim (consumer).
- Non-additive changes: none. The contract gains one request field and one
  failure arm, both additive, so `shim.v1` keeps its version.

## Iteration sequence

One increment: `ForkSession`'s request and response, together. Transport,
endpoint inventory and the outcome convention are unchanged.

## Landed changes

### `shim.v1.ForkSessionRequest.parent_account_root` and `shim.v1.ForkSessionFailure.import_failed`

- WHAT: the request names the account root the parent's transcript is filed
  under. The failure oneof gains `import_failed` for a cross-account import
  that could not be made.
- WHY: the daemon is the one party that knows both roots. The child shim knows
  only its own, so the daemon states the parent's and the shim decides whether
  an import is needed by comparing it with its own `CLAUDE_CONFIG_DIR`.
- The daemon fills the field from the parent's recorded session
  `wsm.Session.ConfigDir`: the root the parent's session was spawned under,
  which is where its shim writes the transcript. It is NOT `accountRootFor`
  (which can name a root the reader selected but the parent has not yet been
  restarted into) and NOT the path routing. A parent with a conversation but no
  recorded root is refused loudly by the daemon (`ErrForkRefused`).
- Same root: the shim behaves exactly as before; nothing is copied.
- Different root: the shim copies `<source>/projects/<slug>/<id>.jsonl` and its
  sibling `<id>/` directory (subagent transcripts, tool results) into the
  child's root under the same project slug, calls the vendor's fork, files the
  fork under the child's directory, then REMOVES the imported copy.
- WHY THE COPY IS REMOVED (consequence accepted): a copy left in the child's
  root would be a stale snapshot of a conversation that keeps growing under its
  own root, and the daemon's account-switch port (`account.MoveTranscript`)
  refuses a destination that already exists, so a lingering copy would break a
  later account switch of the parent into the child's root. The import's
  lifetime is therefore the fork call. The vendor's fork reads only the main
  transcript (verified in the shipped `sdk.mjs`: it loads the transcript and
  drops sidechain records), so the fork does not depend on the copy afterwards.
- An import whose destination already exists is refused (`import_failed`),
  never overwritten: the destination is a file this import did not write.
- `unknown_session` now names the transcript under the PARENT's root, and the
  fork point is resolved against that file, so every refusal still precedes
  every write.
- Failure to remove the imported copy after the fork fails the call hard: a
  copy that survives is the defect the removal exists to prevent.
