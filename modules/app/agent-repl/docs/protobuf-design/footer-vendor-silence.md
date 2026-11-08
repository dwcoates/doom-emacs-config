# Footer: the vendor-silence line under `working · waiting`

When the footer's working step reads `waiting` (the vendor has been silent past
the silence threshold), the activity cell must explain the wait — how long since
anything came back from the vendor API, and what was last heard — instead of
whatever ordinary transient or enduring line happens to stand.

Mode: autonomous (owner pre-approved contract changes for footer lines).

## Assumptions

- Systems involved: the daemon's footer resolver (producer), the webapp's footer
  activity cell (consumer). No shim change: the shim's silence on a `tool_use`
  block start and on `input_json_delta` / `signature_delta` stays as it is; the
  owner has not ruled on splitting the step.
- Additive only. No existing arm changes.

## Iteration sequence

One increment: the new salient kind on `frontend.v1.FooterStatusWorkingSalient`
and its leaf messages.

## Landed changes

### `vendor_silence` joins the working status's salient kinds

- WHAT: `frontend.v1.FooterStatusWorkingSalient.vendor_silence` (tag 8), of the
  new `frontend.v1.FooterStatusActivityVendorSilence`, whose `response` oneof is
  `not_started` or `stalled`; each arm carries the always-set
  `FooterStatusActivityVendorSilenceLastHeard` (daemon-composed words).
- WHY: the waiting step said nothing about the wait. The salient's `at` is the
  instant the vendor was last heard from, so the age the client already ticks
  for every salient line IS "time since the last API response"; no new clock
  field was needed.
- Response stage: read from the last frame heard. Thinking or reply-text frames,
  and any item still running, mean a response was streaming and went quiet
  (`stalled`); an item that landed, or a lifecycle-less item, closed out an
  exchange, so the next request is out with nothing back (`not_started`).
  Rejected: a typed oneof per item kind for the last-heard fact — the webapp
  words nothing under figma→idl, and the daemon already names every item arm
  (`footer.feedKinds`), so a composed sentence carries the same fact with no
  second vocabulary to keep aligned.
- Rank: compaction, retrying, vendor_silence, update, notification. In practice
  a retry never meets it: a retried call draws `vendor_fault · api_retrying`,
  which outranks the thinking rung on the ladder. A standing compaction line
  under the waiting step (the cold gate's answer narrating a compaction while
  a turn runs) is outranked by the silence line, because the step and its line
  are one decision in `footer.resolver.thinking()`.
- Consequences: a long tool-argument stream reads as `stalled` after the
  preceding text or thinking frame, which is honest about what the daemon
  saw; the shim gap is recorded in `org/todo.md`.
- Sweep: nothing obviated.
