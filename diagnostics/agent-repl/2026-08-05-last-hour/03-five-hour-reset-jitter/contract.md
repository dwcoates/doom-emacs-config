# Provider contract: five-hour reset-window identity

## Primary evidence

1. The repository pins Anthropic's `@anthropic-ai/claude-agent-sdk` at
   `0.3.220`. Its published `SDKControlGetUsageResponse` contract describes
   `rate_limits.five_hour.resets_at` only as an ISO 8601 timestamp for when the
   window resets. It supplies neither a stable window ID nor a timestamp
   granularity. The exact package is locked by the registry URL and integrity
   hash in `agent-shim/claude/shim/package-lock.json`.
2. Anthropic's Help Center documents the session limit as a five-hour reset
   period:
   <https://support.claude.com/en/articles/12293051-use-claude-in-xcode#usage-limits>.
   The same account limit is shared across Claude product surfaces.
3. The pinned Claude Code `2.1.220` artifact independently describes its
   status-line `five_hour.resets_at` value as Unix epoch seconds, but that
   representation is not used as evidence for the experimental SDK method's
   precision. In particular, it does not justify rounding SDK timestamps to a
   second or minute.

## Canonical comparison

The only provider-defined identity structure available is the five-hour
cadence. Compare two parsed reset instants by dividing their signed difference
by five hours and rounding symmetrically to the nearest whole reset cycle. A
cycle displacement of zero means the same window; any nonzero displacement
means a different window.

The half-cycle decision boundary is derived from the provider's cadence: it is
the midpoint between adjacent reset events. It is not a guessed timestamp
granularity, tolerance, or epoch alignment. The comparison therefore unifies
subsecond sampling jitter on either side of a display minute while preserving
a genuine five-hour crossing.

Instrumentation records both raw timestamps, both parsed instants, the signed
raw difference, the canonical cycle displacement, the residual jitter after
removing that displacement, the comparison outcome, and the contract version.

## Protobuf decision

No protobuf field is added. Anthropic does not expose a stable window ID, and
the existing reset timestamp fields contain enough information for the
cadence-derived comparison. Inventing a local absolute ID would require an
unpublished epoch alignment and would therefore guess at provider semantics.
