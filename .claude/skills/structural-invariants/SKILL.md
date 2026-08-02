---
name: structural-invariants
description: Audit whether a named aspect of a program or system enforces its invariants STRUCTURALLY (inherent, unbreakable by any schedule or caller) rather than probabilistically (near-certain, high-probability, "fast enough", "we always call it in order"). Checks sequencing via synchronization primitives instead of sleeps and retries, exclusivity via kernel or database arbitration instead of check-then-act, one shared schema or interface instead of contracts duplicated across systems, one shared helper instead of drifting near-duplicate code, unrepresentable illegal states, and scope-bound resource lifetimes. Also checks that every invariant violation is detected, fails hard with no fallback or degraded mode, is recorded once through the module's canonical logging helper with structured context, and is covered by tests of both the guarantee and the violation. Use when asked whether something is structurally guaranteed, whether a guarantee is real or merely likely, to hunt races, sleep-based synchronization, duplicated contracts, or "limp along" recovery paths, or when invoked as /structural-invariants.
argument-hint: "<aspect to audit> [--scope branch|commit|uncommitted|all] [--path <path>] [--fix] [--iteration]"
allowed-tools: Bash(.claude/skills/structural-invariants/run.sh:*), Read, Grep, Glob, Edit, Write, Bash(git:*)
lineage_root: user.dodge.skills.structural-invariants
---

## What This Skill Does

Takes one named aspect of a program or system and decides whether its invariants are guaranteed by construction or merely by probability, then reports every gap and how to close it structurally.

An invariant that holds because of a sleep, a retry, a grace window, a convention, or a duplicated definition kept aligned by review is a defect, not a guarantee. It fails on the schedule of load, timing, restarts, and scale rather than never.

## Arguments

| Argument | Behaviour |
|---|---|
| `<aspect to audit>` | Required free-text description of the aspect, invariant, subsystem, or interaction to audit. |
| `--scope branch` | Audit the code the current branch changed. **Default when `--scope` is omitted.** |
| `--scope commit` | Audit the code the most recent commit changed. |
| `--scope uncommitted` | Audit uncommitted changes only. |
| `--scope all` | Audit the whole tracked tree. |
| `--path <path>` | Narrow the resolved scope to paths containing this substring. Repeatable. |
| `--fix` | After reporting, apply the remediations in the report. Without it the skill is read-only. |
| `--iteration` | Emit the iteration signal at the end of every exit path (for use by `/iterate`). |

## Iterate Signals

- `ITERATE_SIGNAL: CONTINUE ITERATION` — at least one rubric row is `PROBABILISTIC` or `ABSENT`. Paired with a `## Remediation Plan`.
- `ITERATE_SIGNAL: TERMINATE ITERATION SUCCESS` — every applicable rubric row is `STRUCTURAL`. No remediation plan.
- `ITERATE_SIGNAL: TERMINATE ITERATION FAILURE` — the aspect could not be located in the resolved scope.

---

## Steps

0. Read the rubric at `.claude/skills/structural-invariants/references/rubric.md` in full before anything else.
  - The rubric's rows, verdicts, and remediation principles are the entire specification of this audit.
  - **Why this lives here**: every later step assigns rubric verdicts by row identifier, so an audit run without the rubric loaded produces unanchored prose instead of a verdict.

1. Parse arguments: extract the aspect description, `--scope` (defaulting to `branch`), every `--path`, `--fix`, and `--iteration`.

2. Harvest the scope and its evidence.
  - Run `.claude/skills/structural-invariants/run.sh --scan --scope <scope> [--path <path>]...`.
    - `EXIT CODE 0:` Read the file path it printed with the `Read` tool. Continue to step 3.
    - `EXIT CODE 1:` The scope resolved to no auditable files. Tell the user the scope is empty, suggest a wider `--scope`, and stop. If `--iteration` was passed, emit `ITERATE_SIGNAL: TERMINATE ITERATION FAILURE` as the last line.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.
  - *NOTE*: the evidence hits are lexical smells that narrow attention. They are NEVER a verdict on their own, and an absence of hits is NEVER a `STRUCTURAL` verdict.

3. Locate the aspect within the scope.
  - Read the in-scope files that implement the named aspect, following the evidence hits and the aspect description.
  - Read the production code AND its tests. A rubric Part C verdict requires having read the tests.
  - If the named aspect is not present in the resolved scope, stop and report that, emitting `ITERATE_SIGNAL: TERMINATE ITERATION FAILURE` as the last line when `--iteration` was passed.

4. Enumerate the aspect's invariants explicitly, before judging any of them.
  - List each thing the code guarantees: an ordering, a uniqueness, a contract agreement, a behavioral agreement, a state legality, a lifetime.
  - For each, name the concrete mechanism the code currently relies on to hold it.
  - CRITICAL: an invariant nothing enforces still belongs on this list. Omitting it hides the very gap the audit exists to find.

5. Assign a verdict to every rubric row (A1–A6, B1–B3, C1–C4) for the aspect.
  - Each row gets exactly one of `STRUCTURAL`, `PROBABILISTIC`, `ABSENT`, or `N/A`, plus a `file:line` citation and one sentence of justification.
  - `N/A` REQUIRES a stated reason. An unjustified `N/A` is forbidden.
  - **Never soften a `PROBABILISTIC` verdict.** "Extremely unlikely", "has never happened", "the window is microseconds", and "the retry handles it" are all `PROBABILISTIC`.
  - **Never infer `STRUCTURAL` from absent evidence.** A row is `STRUCTURAL` only when a named mechanism in the code makes the violation unrepresentable.

6. Determine the overall verdict.
  - `pass` if and ONLY if every row is `STRUCTURAL` or a justified `N/A`.
  - `fail` if ANY row is `PROBABILISTIC` or `ABSENT`.

7. Write the findings to a file and format the report.
  - a. Write the findings to a scratch file containing, in order:
    - `## Invariants` — the enumeration from step 4.
    - `## Rubric Verdicts` — a table of row, verdict, `file:line`, justification.
    - `## Findings` — one entry per non-`STRUCTURAL` row: what breaks it, and the concrete interleaving, restart, or drift that breaks it.
    - `## Remediation Plan` — included if and ONLY if the verdict is `fail`. One entry per finding, each naming the replacement mechanism and stating explicitly whether it makes the failure IMPOSSIBLE or merely UNLIKELY.
      - Under NO circumstance propose a remediation that makes the failure merely unlikely when an impossible-by-construction option exists.
      - Under NO circumstance justify a remediation by its size, speed, or risk. Justify it by the invariant it establishes.
  - b. Run `.claude/skills/structural-invariants/run.sh --format-report --verdict <pass|fail> --findings <scratch file> [--iteration]`.
    - `EXIT CODE 0:` Read the file path it printed with the `Read` tool and re-emit its contents verbatim.
    - `EXIT CODE 2:` IMMEDIATELY terminate and surface the raw error.

8. Apply the remediations (`--fix` only, and only when the verdict is `fail`).
  - a. Apply each entry in the remediation plan.
  - b. Add or update the tests that rubric rows C1–C4 found missing, one test per invariant and one per violation path.
  - c. Run the test command the repository documents for the touched modules, and drive it to green before stopping.
  - d. Restart step 2 with the same inputs to re-audit the amended code.
  - *NOTE*: without `--fix` this skill modifies nothing. Report and stop.

## Notes

- **CRITICAL NOTE: `STRUCTURAL` and `PROBABILISTIC` are never blurred.** A guarantee that a schedule, restart, race, or drift can break is `PROBABILISTIC`, no matter how improbable, and the report says so.
- **CRITICAL NOTE: A violated invariant is never recoverable.** Never propose, accept, or preserve a fallback, default, retry, degraded mode, or swallowed error as the response to a violation. Fail hard instead.
- **CRITICAL NOTE: Never remove or weaken existing error-handling coverage as part of a remediation.** A remediation that changes how a failure manifests adapts the existing coverage rather than dropping it.
- **IMPORTANT NOTE: Without `--fix` this skill is read-only.** Report findings only — modify no files.
- **IMPORTANT NOTE: Under NO circumstances attempt to resolve a `run.sh` failure yourself. IMMEDIATELY terminate and surface the raw error.**
- **CRITICAL NOTE: Under NO circumstances read or edit `run.sh` for this skill or any other. `run.sh` files are infrastructure — treat them as black boxes.**
