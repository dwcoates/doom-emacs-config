# Slack channel completion — `#` in the Emacs composer

The owner asked (2026-11-19) that agent-repl know every Slack channel the user
is subscribed to on their work account, and that the Emacs composer complete
those channels when a word starts with `#`.

Designed autonomously (the owner did not ask to co-design), so each assumption
and ruling below was settled by the orchestrator and is recorded for review.

## Context (assumptions)

- **Objective**: typing `#gen` in the composer offers `#general` and every other
  channel the user is a member of.
- **Source of truth**: the work Slack is reached through the GNS CLI, which is
  already how agent-repl touches Slack (`bin/hooks/slack-merged-reaction.sh`
  runs `gns slack react`; the chess board runs `gns cee`). `gns slack channel
  list-mine --all --json` answers every public and private channel the GNS
  login's user is a member of. Evidence tier: OBSERVED — run on 2026-11-19 it
  answered 520 channels (54 private, 0 archived) in 13.0s, 15.4s and 13.1s.
- **"Work account"** is the GNS login: GNS is the work org's tool, so its token
  is the work Slack's. agent-repl's personal/work Claude account split
  (`~/.claude` versus `~/.claude-chesscom`) plays no part.
- **Systems**: the proto (`agentrepl.v1`), the daemon (owns the list and its
  reads), the Emacs lisp (wire codec, rpc table, the composer capf), and the
  lisp tests' fake daemon. No webapp, shim, store or sidecar change.
- **Non-additive changes**: none.

## Iteration sequence

Default sequence, walked alone: transport (ConnectRPC, the daemon's one
service) → inventory (one rpc, `ListSlackChannels`, HOST section beside
`ListSlashCommands`) → conventions (the package's `oneof result { success;
error }`) → the one endpoint's shapes.

## Landed changes

### 1. `agentrepl.v1.ListSlackChannels` (endpoint_list_slack_channels.proto)

- WHAT: an empty request; a response whose success arm is `listed` (the
  channels) or `pending` (the first read is under way), and whose error arm's
  one cause is `read_failed` (no list is held because the last read failed and
  none ever succeeded). Each `agentrepl.v1.SlackChannel` carries Slack's id,
  the bare name, a `visibility` oneof (`public_channel` / `private_channel`)
  and an optional purpose.
- WHY the request names no workspace: channel membership is the person's, so
  one list serves every composer. This also means the handover refusal arms
  `ListSlashCommands` carries (unknown workspace, ref mismatch, transferring
  away, not yet adopted) have no meaning here and are not modeled.
- WHY `pending` is success and `read_failed` is error: a read under way is an
  ordinary answer about what the daemon knows; a failed read with nothing to
  fall back on means the question could not be answered.
- WHY a failed REFRESH still answers `listed`: the earlier list is at most an
  hour or so stale and is still the best completion source. The failure is
  logged loudly by the daemon (the owner of the read), not carried on the wire,
  because no composer acts on it.
- WHY `visibility` is a oneof rather than `bool is_private`: per the
  bool-selects-interpretation rule; the composer annotates a private channel.
- WHY `purpose` and not `topic`: the purpose says what a channel is for, which
  is what helps pick between similarly named channels in a popup; the topic is
  transient chatter.
- WHY the id rides along though the composer inserts names: it is the channel's
  stable identity and what a `<#id>` mention or a link names, so a later
  consumer need not re-resolve names.
- No `read_at` timestamp: no consumer acts on the list's age.

### Consequences

- The daemon reads on DEMAND (the first ask, and the first ask after the list
  is an hour old), never at boot: a daemon in the integration harness that no
  composer asks therefore never spawns `gns`, keeping external services out of
  tests. Cost accepted: the first `#` after a daemon restart offers nothing for
  the ~14s the first read takes; the composer asks again at its next `#`.
- One read at a time, structurally: the daemon's channel directory starts a
  read only when none is in flight, under its own lock.
- The read is bounded at 40s (2–3x the measured 13–15s healthy read).

## Sweep

Nothing is obviated: the endpoint is purely additive.
