---
name: build-skill
description: "Create, audit, or update a Claude Code skill. Use for any modification to a SKILL.md or run.sh — new skills and edits to existing ones alike. Covers anatomy, frontmatter, step conventions, run.sh patterns, ITERATE_SIGNAL API, and implementation-independence."
argument-hint: "<new skill-name|audit skill-name|update skill-name>"
---

## What This Skill Does

Guides the creation, auditing, or updating of Claude Code skills under `.claude/skills/`. Operates in three modes: `new` (create from scratch), `audit` (evaluate against conventions and report findings), and `update` (evaluate and apply fixes). **Use this skill for any modification to an existing skill, not just for new-skill creation** — small edits go through `update` mode so the audit runs against the change. All conventions for skill anatomy — frontmatter, sections, run.sh patterns, iterate signals, and Notes guardrails — are defined in the Conventions section below.

**Reference implementations:** When creating, auditing, or updating a skill, actively read `.claude/skills/check-cicd/` and `.claude/skills/iterate/` (both SKILL.md and run.sh) as exemplars. These are the most mature skills in the repo and demonstrate the full pattern set: subcommand-based run.sh, exit code handling in steps, iterate signal integration, and comprehensive Notes sections.

## Arguments

| Argument | Behaviour |
|---|---|
| `new <skill-name>` | Create a new skill from scratch. |
| `audit <skill-name>` | Audit an existing skill against conventions; report findings only. |
| `update <skill-name>` | Audit an existing skill, then apply fixes for any failing checks. |

---

## Conventions

Reference material for building and evaluating skills. Steps reference this section — it is not itself procedural.

### SKILL.md Anatomy

#### Frontmatter

| Field | Required | Notes |
|---|---|---|
| `name` | yes | Kebab-case, matches directory name |
| `description` | yes | One sentence. Specific enough to be useful in the skill list |
| `argument-hint` | no | `[optional]` for optional args, `<required>` for required. `A\|B\|C` (pipe-separated) for mutually exclusive values |
| `allowed-tools` | no | Scope to minimum needed. For run.sh skills: `Bash(.claude/skills/<name>/run.sh:*)` plus any other tools needed |

#### Sections

Every SKILL.md follows this order:

1. **`## What This Skill Does`** — 2-4 sentences describing what and when. If `/iterate`-compatible, include a compatibility callout.
2. **`## Arguments`** — table with every argument and its behavior. Default behavior noted with `*(none)*` or `*(default)*`.
3. **`## Iterate Signals`** — only if `/iterate`-compatible. See ITERATE_SIGNAL Protocol below.
4. **`## Steps`** — numbered, procedural actions. See Step Conventions below.
5. **`## Notes`** — hard constraints and guardrails. See Notes Section below.

#### Step Conventions

- **Numbered steps** — one logical action per step.
- **Self-contained** — each step independently understandable without reading surrounding steps.
- **Purely procedural** — steps describe what to DO, not how things work in general. Reference documentation belongs in a separate section (like this Conventions section).
- **Exit code handling** — every step that invokes a command (not just `run.sh`) must enumerate how to respond to each possible outcome. There is no "implicit" handling: success and failure paths are spelled out.
  ```
  - On success: <continue to step N+1 / surface output / etc.>
  - On error (non-zero exit, plugin error response, or STOP trigger): <surface to user / run failure procedure / stop>
  ```
  For `run.sh` callers the same shape applies, keyed on specific exit codes:
  ```
  - EXIT CODE 0: <action, typically "continue to step N+1">
  - EXIT CODE 1: <action, typically a non-error condition>
  - EXIT CODE 2: Stop and surface the error to the user.
  ```
  Steps that invoke commands without enumerating outcomes are an audit failure (see checklist).
- **Early termination** — if a step can result in stopping, state the output and add "and stop."
- **Code blocks** for any shell commands.
- **No ambiguity** — if a step has conditional behavior, enumerate every branch. The LLM should never have to guess.

#### Implementation independence (black-box principle)

**Skills name the contract, never the mechanism.** A skill describes what command to invoke and what behavior to expect from the response — nothing about how that behavior is produced. If a future maintainer rewrites the system underneath, the skill's text should remain accurate without edits.

This rule emerged from the cee-usage / analyze-position skills, where every passing reference to internals (daemon names, file paths, JSON-RPC framing, build commands, source dirs) had to be repeatedly stripped as the underlying system evolved. The fix is treating skills as **black-box callers** by default.

**Forbidden in skill bodies (steps, notes, descriptions):**

| Category | Examples |
|---|---|
| Process / architecture names | `daemon`, `plugin`, `proxy`, `host`, `server`, `client` |
| File system paths | `/tmp/...`, `~/.config/...`, `~/.gns/...`, source-tree paths (`apps/`, `internal/`, `vendor/`) |
| Protocol or framing details | `JSON-RPC`, `gRPC`, `stdin/stdout`, `UDS`, wire formats |
| Build / install internals | `justfile`, `just install`, `make`, `cargo build`, install paths |
| Internal symbol / module names | Go package names, C++ class names, internal function names |
| Postmortems / war stories | "Was caused by X bug"; "Used to silently route to Y"; FIXED/RETRACTED histories |
| Future-state speculation | "Once X ships..."; "Until Y lands..."; "Planned: Z" |
| Industry meta-analysis | MCP-vs-CLI debates; "2026 best practice"; architecture tier numbering |
| Reasoning about why the system is built a certain way | Anything that would belong in an ADR, not in usage docs |

**Allowed (this is the contract surface):**

- The commands to invoke (`gns cee init`, `gh pr view`, etc.) and their user-facing flags.
- Behavior to expect (response shape at the contract level, not implementation-level fields).
- When to call which command (the workflow).
- Policy: what to do if a command errors or returns an anomaly.
- Cross-references to other skills that govern the same domain.

**Heuristic for auditing:** if the skill text would become inaccurate after a refactor that preserves user-facing CLI semantics, it's leaking implementation. Strip it.

**Where the leaked detail SHOULD live:**

- Engineering postmortems → commit messages and ADR/decision docs.
- File paths, protocol details → the system's developer docs (READMEs, AGENTS.md inside the implementation tree).
- Future-state plans → roadmap docs or tickets, not the skill.
- Why-it's-built-this-way → architecture decision records.

The skill links out to those if needed; it does not embed them.

#### Notes Section

The Notes section contains hard constraints that prevent the LLM from going off-rails. Every skill with any risk factor needs one — not just skills with `run.sh`.

**Risk-profile framework** — evaluate the target skill against these factors to determine which guardrails to include:

| Risk Factor | Guardrail | Example Skills |
|---|---|---|
| Has a `run.sh` | "`run.sh` is a black box — do not read, edit, or reason about its internals." | check-cicd, iterate, create-or-update-github-pr |
| Has a `run.sh` | "No self-remediation of `run.sh` failures — stop immediately and surface the error." | check-cicd, iterate, create-or-update-github-pr |
| Touches git state | "No mutating git commands (resets, checkouts, rebases, merges, pushes)." | check-cicd, iterate |
| Every step is mandatory | "No skipping steps." | create-or-update-github-pr |
| Orchestrates sub-skills via `run.sh` | "No direct skill invocations other than X." | create-or-update-github-pr |
| Calls external APIs or services | "No self-remediation of API failures — surface the error." | standup, gns |
| Modifies files in a scoped directory | "Do not modify files outside `<scope>`." | build-skill |
| Has a read-only mode | "In `<mode>`, do not modify any files." | build-skill (audit mode) |
| Could tempt the LLM to execute its output | "Do not execute the target skill/artifact." | build-skill |

**Writing style:** Use `**CRITICAL:**` or `**IMPORTANT:**` prefix for emphasis, matching established patterns. Each note is a standalone bullet.

**When to omit Notes:** Only truly simple, self-contained skills with no external side effects (e.g. a skill that only reads files and reports findings). When in doubt, include Notes.

### run.sh Pattern

#### When to Extract to run.sh

A `run.sh` driver script should be co-located with `SKILL.md` when any of the following apply:

| Condition | Reason |
|---|---|
| Shell logic exceeds ~10 lines or has branching | Keeps SKILL.md declarative; avoids embedding scripts in markdown |
| Orchestrates external tools (gh, git, gns, docker) with retries/polling | Deterministic retry/poll logic belongs in a script, not in LLM interpretation |
| State management across steps (temp files, session hashes) | Scripts manage state reliably; LLMs should consume results, not manage files |
| Operation must be atomic or idempotent | Scripts can use `set -euo pipefail` and handle cleanup |
| Multiple exit codes convey different conditions | A dispatch table is clearer than prose instructions to the LLM |

**When NOT to use `run.sh`**: Simple skills with a few short, linear Bash commands (e.g. `commit`, `run-tests`) are fine with inline code blocks in SKILL.md.

#### Structure Template

```bash
#!/usr/bin/env bash
# run.sh — driver for the <skill-name> skill
# Usage:
#   run.sh --subcommand-a [args]     description; exit codes
#   run.sh --subcommand-b [args]     description; exit codes
# Exit codes: 0=success  1=condition  2=script error  3+=domain-specific

set -uo pipefail

die()  { echo "ERROR: $*" >&2; exit 2; }
log()  { echo "▶ $*"; }

case "${1:-}" in

--subcommand-a)
  # implementation
  ;;

--subcommand-b)
  # implementation
  ;;

*)
  cat >&2 <<'USAGE'
Usage:
  run.sh --subcommand-a [args]
  run.sh --subcommand-b [args]
USAGE
  exit 1
  ;;

esac
```

#### Conventions

- **Subcommand dispatch**: Top-level `case` statement. Each subcommand is a `--flag` (e.g. `--poll`, `--init`, `--format-report`).
- **Exit codes**: Consistent scheme documented in the header comment:
  - `0` — success
  - `1` — a condition, not an error (e.g. "no PR found", "nothing to do")
  - `2` — script/bash error (use `die()` for these)
  - `3+` — domain-specific conditions (document each one)
- **Helpers**: `die()` for fatal errors, `log()` for progress output. Keep minimal.
- **Output contract**: Document what each subcommand writes to stdout. SKILL.md parses this — it is the interface.
- **State files**: `/tmp/<project>-<skill>-<identifier>` naming. Clean up on completion when practical.
- **No interactive prompts**: `run.sh` must work non-interactively.
- **Notifications**: Use `osascript` for macOS notifications when appropriate.
- **Default case**: Prints usage to stderr, exits 1.

### ITERATE_SIGNAL Protocol

Only applicable when the skill will be used as a target of `/iterate`.

#### Signal Values

| Signal | Meaning |
|---|---|
| `ITERATE_SIGNAL: TERMINATE ITERATION SUCCESS — <reason>` | `/iterate` stops; iteration succeeded (nothing to do, or all clean). Reason surfaced to user. |
| `ITERATE_SIGNAL: TERMINATE ITERATION FAILURE — <reason>` | `/iterate` stops; iteration is blocked (error, infrastructure failure, or cannot proceed). Reason surfaced to user. |
| `ITERATE_SIGNAL: CONTINUE ITERATION` | `/iterate` applies remediation plan and loops. |

Every sub-skill must classify each TERMINATE condition as either SUCCESS or FAILURE. SUCCESS means the skill completed its work or had nothing to do. FAILURE means something prevented it from completing (git access failure, production code change required, CI failure, etc.).

#### Signal Rules

1. Only emit when `--iteration` is present in the arguments. Without it, produce the same output but omit the signal line.
2. The signal must be the absolute last output of the skill. Nothing may follow it.
3. Every exit path must emit exactly one signal (when `--iteration` is active).
4. After emitting the signal, the skill must stop.
5. `/iterate` scans only the **last non-blank line** for the `ITERATE_SIGNAL:` prefix.

#### ITERATE_OUTCOME Protocol

When `/iterate` is invoked via `claude -p` (e.g., from the PR skill's `run.sh`), the caller needs to know whether the iteration succeeded or failed. The iterate skill's `run.sh` emits an `ITERATE_OUTCOME:` line after the progress table upon termination:

| Outcome | Meaning |
|---|---|
| `ITERATE_OUTCOME: SUCCESS` | All iterations completed cleanly. |
| `ITERATE_OUTCOME: FAILURE — <reason>` | Iteration was blocked or hit max iterations with unresolved issues. |

Callers of `claude -p "/iterate ..."` can grep stdout for this line to determine the result.

#### SKILL.md Integration

- Add `--iteration` to `argument-hint` and the Arguments table.
- Add `## Iterate Signals` section after `## Arguments`, before `## Steps`, with:
  - A callout block explaining conditional emission.
  - An exhaustive table mapping every exit condition to its signal, classifying each TERMINATE as SUCCESS or FAILURE.

### Remediation Plan Protocol

The `## Remediation Plan` is a markdown section that describes concrete, actionable steps to fix issues identified by a skill. It is the core mechanism that enables `/iterate` to autonomously fix problems in a loop: `/iterate` extracts the plan, executes it, commits the result, and re-runs the skill.

#### How `/iterate` Consumes Plans

`/iterate`'s `run.sh` extracts the plan using strict `awk` parsing:

```
awk '/^## Remediation Plan/{found=1; next} found && /^## /{exit} found{print}'
```

This means:
- The heading must be exactly `## Remediation Plan` (not `###`, not `# Remediation Plan`).
- Everything between `## Remediation Plan` and the next `## ` heading (or EOF) is extracted as the plan body.
- The `ITERATE_SIGNAL` line must come **after** the plan (it is always the last non-blank line).

#### When to Emit a Remediation Plan

| Condition | Emit Plan? | Signal |
|---|---|---|
| Issues found that the skill can fix | **Yes** | `CONTINUE ITERATION` |
| No issues found | No | `TERMINATE ITERATION SUCCESS` |
| Fix requires changes outside the skill's scope (e.g., production code from a test skill) | No | `TERMINATE ITERATION FAILURE` |
| `--no-fix` flag is active | No | (signal still emitted) |

**Key rule:** A remediation plan is only meaningful when paired with `ITERATE_SIGNAL: CONTINUE ITERATION`. If the skill terminates (SUCCESS or FAILURE), any plan would be ignored.

Without `--iteration`, the plan is still emitted (it's useful for the user to see) but no signal follows it.

#### Plan Content Conventions

Each item in the plan must be specific enough for the LLM to execute without further research:

- **File path** — which file to modify.
- **Current state** — what exists now (or a reference to a line/section).
- **Target state** — what the change should be.
- **Scope** — one discrete change per item.

Avoid vague directives like "fix the test" or "address the comment." Each item should be independently actionable.

#### Generation Patterns

There are two patterns for generating remediation plans:

| Pattern | How It Works | When to Use |
|---|---|---|
| **LLM-driven** | SKILL.md steps instruct the LLM to analyze findings and write the plan as markdown output. | When plan items require judgment, reading source files, or context-dependent decisions. (e.g., `tests-coverage`, `tests-quality`, `check-cicd` comments mode) |
| **Script-driven** | `run.sh` generates the plan from structured data (e.g., jq templates over a JSON result file). The LLM may contribute via an `--analysis` argument that `run.sh` embeds in the plan. | When the plan is mechanical / templatable from tool output. (e.g., `check-cicd` CI failure mode) |

For LLM-driven plans, the SKILL.md step should specify:
1. What to analyze (e.g., "for each coverage gap...").
2. What to include per item (file, change description, helpers to introduce).
3. Scope constraints (e.g., "do not propose changes to production code").

For script-driven plans, `run.sh` owns the `## Remediation Plan` heading and structure. The LLM's contribution (if any) is passed via a flag like `--analysis`.

#### SKILL.md Integration

- If `/iterate`-compatible, every exit path must either emit a `## Remediation Plan` section (with `CONTINUE ITERATION`) or omit it (with a `TERMINATE` signal).
- Document in the `## Iterate Signals` table which conditions produce a plan and which don't.
- In Steps, specify the plan format: what each item contains and how it is grouped.

### Nested Skill Invocation (Agent-Tool Dispatch)

**When a skill needs to invoke another skill mid-flow, dispatch the sub-skill via the `Agent` tool rather than invoking it directly in the parent conversation.** The parent then branches on structured tokens the subagent returns, in the same shape it branches on `run.sh` exit codes.

#### Why this pattern exists

Direct sub-skill invocation lures the parent's response stream into rendering the sub-skill's output (CI reports, iteration summaries, etc.). Once the parent has rendered a visually complete block, the model is prone to treat the nested completion as the end of its own flow and stop — skipping later steps that the SKILL says are mandatory. Wrapping the call in an Agent dispatch keeps the sub-skill's output isolated to the subagent's tool result; the parent only sees a small structured signal it can act on mechanically.

This failure mode is identical regardless of how clearly the parent SKILL.md says "this is a sub-step, not a handoff" — relying on the model to follow the rule it just broke is fragile. The Agent dispatch makes the failure structurally impossible.

#### When to wrap

Wrap a sub-skill invocation in an Agent dispatch when any of the following apply:

- The sub-skill emits a substantial human-readable report (more than a couple lines of structured tokens).
- The parent has steps that MUST run after the sub-skill returns (queueing, follow-up checks, notifications).
- The sub-skill could take a long time or involve polling — the parent shouldn't be holding state across that.

Skip the wrapper only when the sub-skill is trivial, emits no rendered output, and is the last action of the parent flow.

#### Dispatch contract

The Agent prompt MUST:

1. Name the sub-skill and its exact arguments verbatim.
2. Forbid the subagent from taking actions beyond invoking the sub-skill — no remediation, no file edits, no additional commands, no further skill invocations.
3. Specify the exact response format expected — a small set of named tokens, one per line, with documented value domains (e.g. `CHECK_CICD_RESULT: PASS | FAIL`). Free-form prose is not acceptable.
4. Require the response to end with the tokens; nothing may follow them.

The parent step MUST:

1. Parse the tokens from the subagent's response. If any token is missing or malformed, surface that as an error and stop — never guess.
2. Surface user-visible artifacts (e.g. the sub-skill's report file) by reading the file path the subagent returned. This is a mechanical surfacing step, not a narrative beat — it must not be written in a way that reads as a turn-ending action.
3. Branch deterministically on the token values; each value must have an explicit follow-on (continue, retry once, fall through to failure path, etc.).
4. Restate explicitly that the parent continues past the subagent's return — "the subagent's completion is NOT a handoff" — and identify the next step number.

#### Examples in this repo

- `.claude/skills/create-or-update-pr/SKILL.md` steps 5 and 6 dispatch `/iterate` via Agent; prompt is sourced from `run.sh` stdout on exit 10 and the tokens are `ITERATE_OUTCOME: SUCCESS | FAILURE`.
- `.claude/skills/create-or-update-pr/SKILL.md` steps 15 and 19 dispatch `/check-cicd` via Agent; the tokens are `CHECK_CICD_RESULT`, `REMEDIATION_PLAN_PRESENT`, and `REPORT_PATH`.

---

## Steps

### 1. Parse arguments and determine mode

Extract the mode (`new`, `audit`, or `update`) and the skill name.

- **`new`**: Continue to step 2.
- **`audit`**: Read `.claude/skills/<skill-name>/SKILL.md` (and `run.sh` if present). Continue to step 4.
- **`update`**: Read `.claude/skills/<skill-name>/SKILL.md` (and `run.sh` if present). Continue to step 4.

---

### 2. Gather requirements (new mode only)

Ask the user (or infer from context):

- **Name**: kebab-case, concise (e.g. `check-cicd`, `tests-quality`)
- **Purpose**: one sentence — what does it do, and when should it be invoked?
- **`/iterate` compatible?**: will this skill be used as a target of `/iterate`?
- **Complexity**: does it need a `run.sh` driver, or is inline Bash sufficient?
- **Allowed tools**: which Bash commands does it need? (e.g. `Bash(git:*)`, `Bash(gh:*)`)

---

### 3. Create the skill (new mode only)

Create `.claude/skills/<skill-name>/SKILL.md` (and `run.sh` if applicable) following the **Conventions** section above.

- If a `run.sh` is needed (per the extraction criteria in Conventions > run.sh Pattern > When to Extract), create it following the structure template and conventions in that section.
- Create SKILL.md following the anatomy in Conventions > SKILL.md Anatomy. Include all applicable sections: frontmatter, What This Skill Does, Arguments, Iterate Signals (if applicable), Steps, Notes.
- Apply the black-box principle from Conventions > Implementation independence — every sentence must describe the contract, never the mechanism behind it.
- Write the Notes section using the risk-profile framework in Conventions > SKILL.md Anatomy > Notes Section. Evaluate the skill's risk factors and include the corresponding guardrails.
- If `/iterate`-compatible, add the iterate signal infrastructure per Conventions > ITERATE_SIGNAL Protocol.

After creation, continue to step 4.

---

### 4. Audit the skill

Evaluate the skill against the following checklists.

#### SKILL.md checklist

| # | Check | Details |
|---|---|---|
| 1 | **Frontmatter completeness** | `name`, `description` present and accurate. `argument-hint` uses correct syntax. `allowed-tools` scoped to minimum. |
| 2 | **"What This Skill Does" section** | Present, 2-4 sentences, describes both what and when. |
| 3 | **Arguments table** | Every argument documented. Default behavior noted. |
| 4 | **Step numbering and structure** | Numbered, one action per step, self-contained. Steps are purely procedural — no embedded reference material. |
| 5 | **Outcome handling** | Every step that invokes a command (CLI tool, `run.sh`, sub-skill, etc.) explicitly enumerates success and failure paths. No implicit handling. For `run.sh` callers: every possible exit code listed with an action. |
| 6 | **Early termination paths** | Every exit path explicitly states output and "stop." |
| 7 | **Notes section** | Present when skill has any risk factor (run.sh, git, external APIs, file mutation, read-only modes, etc.). Contains appropriate guardrails per the risk-profile framework in Conventions. |
| 8 | **Iterate signals** | If `/iterate`-compatible: `## Iterate Signals` table is exhaustive, `--iteration` in frontmatter and arguments. |
| 9 | **Remediation plan** | If `/iterate`-compatible: every `CONTINUE ITERATION` path produces a `## Remediation Plan`. Plan items are concrete (file path, current state, target state). Generation pattern (LLM-driven vs script-driven) is appropriate for the content. Uses exact `## Remediation Plan` heading. |
| 10 | **No embedded scripts** | Shell logic >10 lines is extracted to `run.sh`, not inlined in SKILL.md. |
| 11 | **Allowed-tools scoping** | Tools restricted to what the skill actually needs. |
| 12 | **Implementation independence** | No leakage per the black-box principle (Conventions > Implementation independence). Grep the skill body for forbidden categories: process names, file paths, protocol details, build internals, postmortems, future-state speculation, meta-analysis. Each hit is a failing check. |
| 13 | **Nested skill invocations dispatched via Agent tool** | Any step that invokes another skill (`/iterate`, `/check-cicd`, etc.) must dispatch it via the `Agent` tool, not invoke it directly in the parent conversation. The dispatch prompt must name the sub-skill, forbid further actions, and specify a structured token format; the parent step must branch on those tokens. See Conventions > Nested Skill Invocation. |

#### run.sh checklist (if present)

| # | Check | Details |
|---|---|---|
| 1 | **Header comment** | Documents all subcommands and exit codes. |
| 2 | **`set -uo pipefail`** | Present at top of script. |
| 3 | **Subcommand dispatch** | Top-level `case` with `--flag` style subcommands. |
| 4 | **Exit code consistency** | Follows 0/1/2/3+ scheme. Documented in header. |
| 5 | **`die()` and `log()` helpers** | Present and used consistently. |
| 6 | **Default case** | Prints usage to stderr, exits 1. |
| 7 | **Output contract** | Each subcommand's stdout is documented and parseable. |
| 8 | **No interactive prompts** | Script works non-interactively. |
| 9 | **State file naming** | Uses `/tmp/<project>-<skill>-<identifier>` pattern. |

#### Common anti-patterns to flag

- **SKILL.md contains long shell scripts** — extract to `run.sh`.
- **Step invokes a command without enumerating outcomes** — every command-invoking step must spell out the success path and the failure/error path. "Run X." with no follow-on instruction is a failing check.
- **Steps don't handle all exit codes** — for `run.sh` callers, every documented exit code must have an explicit action; for plain commands, the success/non-zero split must be named.
- **"Read the run.sh to understand..."** — SKILL.md must never need to read `run.sh` internals.
- **Missing Notes section** — skills with risk factors need guardrails.
- **Notes section too narrow** — only checks for `run.sh` presence, not full risk profile.
- **Overly broad `allowed-tools`** — scope down to what's actually used.
- **Ambiguous steps** — LLM has to guess what to do in some condition.
- **Missing early termination** — a step can fail but doesn't say what happens.
- **Reference material in Steps** — conventions/templates belong in a Conventions section, not inline in procedural steps.
- **`run.sh` uses `set -e` without `pipefail`** — prefer `set -uo pipefail`.
- **Remediation plan heading mismatch** — must be exactly `## Remediation Plan` for `/iterate` awk extraction to work. `###` or other headings will be silently missed.
- **Vague plan items** — "fix the failing test" is not actionable. Each item needs file path, what to change, and how.
- **Plan emitted without `CONTINUE ITERATION`** — a plan paired with a `TERMINATE` signal is dead code; `/iterate` will never execute it.
- **Implementation leak in the skill body** — names of internal processes/files/protocols, postmortems with "FIXED:" or "Root cause:" prose, "until X ships" future-state, MCP-vs-CLI / 2026-best-practice meta-analysis, source-tree paths. Strip per the black-box principle.
- **Postmortems embedded in the skill** — bug histories and root-cause writeups belong in commit messages or ADRs, never in a skill that's loaded into agent context every time.
- **Future-state design discussion** — `(planned)`, `(not yet implemented)`, `(once Y lands)`. If something doesn't exist yet, the skill shouldn't mention it. Add to a roadmap doc and re-edit the skill when it ships.
- **Reasoning about why a command is built the way it is** — the skill is a usage contract, not an architecture explainer. Reasoning belongs in ADRs.
- **Direct sub-skill invocation in the parent response stream** — when a skill needs to invoke another skill mid-flow, wrap it in an Agent-tool dispatch instead. Invoking `/check-cicd`, `/iterate`, etc. directly from the parent's response lures the model into treating the nested skill's rendered output as the end of the parent's flow. The Agent wrapper isolates the sub-skill's output to a tool result and returns structured tokens the parent acts on — see Conventions > Nested Skill Invocation.

#### After the audit

- **`audit` mode**: Output the checklist results and anti-patterns found. Stop. Do not modify any files.
- **`update` mode**: Output the checklist results. Then apply fixes for each failing check. After all fixes are applied, continue to step 5.
- **`new` mode**: Output the checklist results. If any checks fail, apply fixes. Continue to step 5.

---

### 5. Verify (update and new modes only)

Lightweight final-state consistency check — distinct from the audit. Re-read the final state of all skill files and confirm:

- SKILL.md parses cleanly (frontmatter is valid YAML, sections are in the correct order per Conventions > SKILL.md Anatomy > Sections).
- If `run.sh` exists: the header comment matches the actual subcommands, and SKILL.md and `run.sh` agree on exit codes and stdout format.
- If `/iterate`-compatible: every exit path in Steps emits exactly one `ITERATE_SIGNAL`.
- No content was accidentally deleted or corrupted during edits.

---

## Notes

- **Do not modify skills other than the target.** This skill operates on exactly one skill at a time (the `<skill-name>` argument). Do not read, modify, or create files outside `.claude/skills/<skill-name>/`.
- **`audit` mode is read-only.** When the mode is `audit`, do not modify any files. Report findings only.
- **Do not execute the target skill.** This skill creates and evaluates skill definitions. It does not test them by running them.
- **Do not create files outside the skill directory.** All created files go in `.claude/skills/<skill-name>/`. Do not create temp files, test files, or any other artifacts elsewhere.
- **Do not modify `AGENTS.md` or `CLAUDE.md`.** If the skill needs to be referenced from project-level docs, note it in the report for the user to handle.
