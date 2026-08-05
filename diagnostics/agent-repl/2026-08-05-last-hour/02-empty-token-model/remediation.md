# Remediation: enforce model identity at durable ingress

1. Make nonblank model identity part of canonical live and historical `TokenUtilization` validation before any durable write.
2. Require synthetic producers to state an explicit documented synthetic model identity. Do not manufacture an identity for vendor records.
3. Add an aggregate invariant that refuses a blank map key if invalid stored data reaches the reducer.
4. Audit persisted token records for blank models. Quarantine or delete only records whose invalidity is proven, with an explicit migration report. Never relabel them by inference.
5. Add validation, store, replay, aggregation, snapshot, and webapp decode tests for blank live and historical model values.
6. Log the failed field path, API message ID, model value, source plane, and session identities at the rejecting boundary.

Success criteria: no blank-model record can become durable, historical invalid rows are enumerated explicitly, and one invalid record cannot recur as a series of undecodable frontend frames.

Protobuf decision: no change. The required `model` field already exists and has the right meaning.
