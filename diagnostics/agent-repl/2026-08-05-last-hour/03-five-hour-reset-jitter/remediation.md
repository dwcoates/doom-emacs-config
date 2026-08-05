# Remediation: compare canonical reset boundaries

1. Confirm the provider's documented reset timestamp granularity or stable window identifier.
2. Parse every available reset timestamp once, reject malformed values, and derive a canonical boundary according to that contract.
3. Compare canonical boundaries while retaining both raw timestamps in diagnostics.
4. Keep genuine missing observations and genuine boundary crossings as distinct unavailable reasons.
5. Test fractional jitter on both sides of the minute, a true five-hour crossing, malformed values, and missing start or end samples.
6. Log raw timestamps, canonical identities, comparison result, and the provider-contract version used.

Success criteria: the five observed false classifications produce valid deltas, while a real window crossing remains unavailable.

Protobuf decision: conditional. Existing reset timestamp fields suffice if canonical identity is derivable. Add a field only if the provider supplies a stable window ID that cannot be derived without guessing.
