# Resolved vendor notices leave the feed

The vendor writes its error notices as assistant prose. agent-repl draws each
as a "vendor notice" card, but only notices of trouble waiting cures were
ever retired, and only by another turn's service. The owner (2026-11-11)
asked that every notice whose cause has been answered leaves the feed at
once, keyed on what the notice is about rather than on its drawn words.

Design ran autonomously (no co-design was asked for).

## Assumptions

- The vendor's declared class (`SDKAssistantMessageError` in
  `@anthropic-ai/claude-agent-sdk`) rides the stream record's `error` and the
  transcript record's `error` alike. Evidence tier: existing code (the sidecar
  already reads `record["error"] == "server_error"`); the transcript carrying
  every class is not verified, so the sidecar falls back to wording when no
  class is present.
- Involved systems: proto, shim (stream converter), sidecar (transcript
  converter), daemon (feed resolver), webapp (notice card). No non-additive
  wire change.

## Iteration sequence

1. `conversation.v1.AgentResponseSynthesizedNotice.subject` gains its arm.
2. `frontend.v1.FeedResponseNotice` gains the fields the daemon and the card
   need.

## Landed changes

### 1. `conversation.v1.AgentResponseSynthesizedNotice.account` (`AgentNoticeAccount`, field 7)

- WHAT: a new subject arm for a notice reporting the vendor refused the
  account: `authentication_failed`, `oauth_org_not_allowed`,
  `account_on_hold`, `verification_required`, `cloud_credential_error`.
- WHY: these fell to `unclassified` and drew as a bare "vendor notice" that
  offered nothing and never left.
- Why one arm and not five: no consumer acts differently on which account
  failure it was; the vendor's words are the notice's face.
- Consequence: the shim and the sidecar each hold one class→subject table;
  a shared fixture (`testdata/vendor-notices/subjects.json`) pins that both
  read the observed notice texts the same way.

### 2. `frontend.v1.FeedResponseNotice.synthesized` (3) and `.sign_in` (4)

- WHAT: the notice row carries the producer's
  `conversation.v1.AgentResponseSynthesizedNotice` whole, and an optional
  `FeedOutcomeSignIn` drawn in the card for the account subject.
- WHY: the daemon's retirement rule keys on the notice's subject, and the row
  is the one record that reaches the feed by every path (live, replay,
  scroll-back page). The earlier rule compared the marker's drawn detail text,
  which couples retirement to wording.
- Why the conversation message is embedded whole rather than a frontend
  respelling: the client draws nothing from it (the marker's detail is the
  drawn subject), so it is daemon bookkeeping, not an element; importing the
  encompassing message avoids a second hand-kept subject vocabulary. The
  frontend package already imports `conversation.v1` for echo values
  (`FeedOutcomeResend.said`).
- `sign_in` reuses `FeedOutcomeSignIn`, the same action the broken bubble
  (`FeedResponseError.sign_in`) and the fault marker offer.

## Sweep

Nothing obviated: `transientNotice`'s marker-text comparison in the daemon
is code, not schema, and is replaced in the implementation.
