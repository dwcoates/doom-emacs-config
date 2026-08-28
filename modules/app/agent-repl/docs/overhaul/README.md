# Implementation planning documents (fanout inputs)

One document per subsystem plus MAIN.md (the orchestrator's own reference).
Each subsystem doc is the reference its fanout orchestrator and implementers
read BEFORE any work, and carries: (1) DEAD CODE TO REMOVE — code stranded by
the contract redesign that reconciliation deliberately left standing;
(2) REPLACEMENT INTEGRATION-TEST SPECS for coverage deleted at
reconciliation (unit coverage is NOT prescribed — it falls out of the
proto→code mapping convention); (3) subsystem-specific gotchas surfaced
during reconciliation. MAIN.md carries the e2e replacement specs.
