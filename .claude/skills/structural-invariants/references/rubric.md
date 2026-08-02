# Structural Invariants Rubric

The audit assigns every row below exactly one verdict for the aspect under audit:

| Verdict | Meaning |
|---|---|
| `STRUCTURAL` | The guarantee is inherent — no execution ordering, timing, load, or caller discipline can violate it. |
| `PROBABILISTIC` | The guarantee holds in practice but a schedule, restart, race, or drift can break it. |
| `ABSENT` | Nothing enforces the guarantee at all. |
| `N/A` | The row does not apply to this aspect, with a stated reason. |

`PROBABILISTIC` and `STRUCTURAL` are never blurred. "Very unlikely", "fast enough in practice", "we always call it in the right order", and "the retry covers it" are all `PROBABILISTIC`.

---

## Part A — Is the invariant structurally established?

**A1. Sequencing.** Ordering is enforced by a primitive the participants must pass through, never by elapsed time.
- `STRUCTURAL`: mutex, condition variable, channel, semaphore, latch the caller awaits, barrier, rendezvous owned by something outliving both parties (an init-system-owned socket where connects queue until the service accepts).
- `PROBABILISTIC`: `sleep`, fixed delay, grace window, backoff, "poll until it looks ready", retry loop covering a startup race, ordering held only by convention or comment.

**A2. Exclusivity.** Uniqueness and mutual exclusion are arbitrated by an authority outside the racing parties.
- `STRUCTURAL`: kernel-enforced exclusivity that dies with its holder (`flock`), single-writer ownership of the contended resource, atomic compare-and-swap, a unique database constraint.
- `PROBABILISTIC`: check-then-act on a pid file or existence probe, "we only spawn one of these", advisory flags.

**A3. Contract identity.** Every participant consumes one definition of the shared contract.
- `STRUCTURAL`: a single protobuf or schema generating every peer's types, one shared interface or client library, one generated constant table.
- `PROBABILISTIC`: per-system re-declarations of the same message, field name, wire tag, enum, or endpoint, kept aligned by review, by a "keep in sync" comment, or by a test that compares two hand-written copies.

**A4. Behavior identity.** Logic that must agree across call sites lives in one place.
- `STRUCTURAL`: one shared helper, one codepath, one table both sites read.
- `PROBABILISTIC`: near-duplicate blocks that must be edited together, copy-paste with local tweaks, parallel switch statements over the same enum.

**A5. Representability.** Invalid states cannot be constructed.
- `STRUCTURAL`: sum types or tagged unions covering only legal states, newtypes, validating constructors that are the only constructor, an operation scoped to the connection or session that makes it valid so ordering stops mattering.
- `PROBABILISTIC`: a struct whose illegal field combinations are merely never produced by current callers, validation performed at some call sites, an enum with a meaningless zero value.

**A6. Lifetime and ownership.** Acquisition and release are bound together.
- `STRUCTURAL`: scope-bound release (`defer`, RAII, `with`, `unwind-protect`), an owner whose death releases the resource.
- `PROBABILISTIC`: a cleanup call on the happy path, a reaper that sweeps leaks later, "every return path calls close".

---

## Part B — Is a violation surfaced and fatal?

**B1. Detection.** The invariant is explicitly checked at the point it is relied upon, rather than assumed.
- `ABSENT` when a violation would simply produce wrong behavior downstream with no check firing.

**B2. Response is fail-fast.** A detected violation aborts through the codebase's loudest mechanism — assertion, panic, thrown error, hard exit.
- `PROBABILISTIC` (and a finding) when the violation is met by any of: a fallback value, a default, a retry, a degraded or best-effort mode, a `nil`/empty return, a skipped step, a swallowed error, or a log-and-continue.
- A violated invariant is never recoverable. Recovery defers and obscures the bug rather than fixing it.
- This is distinct from genuine, expected runtime error conditions — a missing file, a network failure, bad user input. Those are surfaced through the established error channel, never asserted on, and never swallowed either.

**B3. Diagnostics.** The violation enters the shared log through the owning module's canonical logging helper, once, at a defined ownership point.
- `STRUCTURAL` requires structured context sufficient to diagnose from the log alone: the operation, the relevant identifiers and resolved inputs, the concrete cause, and the branch or outcome that failed.
- `PROBABILISTIC` when the record is an ad hoc print, a bare message, a side buffer, a one-off logger, a direct file write, or a message whose dynamic context is absent or in a per-call format incompatible with the shared one.
- Re-logging the same violation at every propagation layer is also a finding. Propagating or surfacing an error never replaces logging it.

---

## Part C — Is the invariant covered by tests?

**C1. The guarantee is asserted.** A test asserts the invariant holds, exercising the interleaving, restart, or scale that a probabilistic implementation would fail.

**C2. The violation is asserted.** A test drives the invariant into violation and asserts all three of:
- the hard failure reaches the caller,
- the canonical logging helper receives the expected diagnostic context,
- no partial state mutation survives where the operation must abort.

**C3. The violation path is reachable in a test.** When a violation cannot be exercised deterministically, the code is restructured around an injectable or mockable boundary until it can be, rather than left untested.

**C4. Tests do not themselves rely on timing.** A test that synchronizes with `sleep` instead of a channel, wait group, latch, or other primitive is a finding in its own right.

---

## Remediation principles

- Prefer the design that makes the failure unrepresentable over the one that makes it unlikely, and name explicitly which of the two each candidate achieves.
- Reaching for a retry, a delay, or an ordering convention is the signal to stop and ask what reorganization would make the race unrepresentable.
- Never stage the structural fix behind a probabilistic one. A compromise shipped "for now" becomes the design.
- Implementation volume, diff size, and file count carry no weight in choosing between candidate designs. Design compromise is the expensive thing; code is cheap.
- Legitimate reasons to reject the more thorough option are engineering objections — it breaks a documented contract, its blast radius is not yet justified by evidence, its correctness is unclear — and never its cost.
