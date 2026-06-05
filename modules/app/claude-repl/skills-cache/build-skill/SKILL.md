---
name: build-skill
description: "Create, audit, or update a Claude Code skill. ALWAYS use this skill when creating, modifying, or generating any skill — never write or edit skill files directly. Use for any modification to a SKILL.md or run.sh — new skills and edits to existing ones alike. Carries a Specializations section with extra conventions for skill families (currently: position-analysis skills)."
argument-hint: "<new|audit|update> <skill-name>"
allowed-tools: Read, Write, Edit, Glob, Grep, Bash(ls:*)
---

## What This Skill Does

Creates new skills or audits/updates existing ones against the conventions below. Three modes: `new` (create from scratch), `audit` (report findings, read-only), `update` (audit + apply fixes). Any modification to an existing SKILL.md or run.sh goes through `update`.

When the target skill belongs to a named family (e.g. `analyze-position-*`), the Specializations section at the bottom of this skill defines the additional conventions that apply on top of the general ones.

## Arguments

| Argument | Behaviour |
|---|---|
| `new <skill-name>` | Create a new skill from scratch. |
| `audit <skill-name>` | Audit an existing skill against conventions; report findings only. **Read-only.** |
| `update <skill-name>` | Audit an existing skill, then apply fixes for every failing check. |

## Reference Implementation

**`.claude/skills/check-cicd/`** is the canonical example. Read both `SKILL.md` and `run.sh` before doing any work in any mode. Every convention below has a direct corresponding example there — when in doubt, mirror its structure verbatim.

---

## Conventions

These are the structural attributes every well-formed skill exhibits.

### 1. Argument table is the spec
- Every flag/argument gets a one-liner row in a `| Argument | Behaviour |` table.
- No prose explanation outside the table.

### 2. Step 0 pattern for setup
- Setup/preflight goes in **Step 0**, not a "Setup" heading.
- Setup must be idempotent — the fast path is a no-op.
- Include a `**Why this lives here**:` sub-bullet when the step's presence is non-obvious.

### 3. Steps are mechanical, not heuristic
- Short procedural imperatives; no decision-making prose; no "use judgment to pick X".
- Sub-steps via letters (`a.`, `b.`) and roman numerals (`i.`, `ii.`) when branching.
- Anything that would be a judgment call must be either pushed into `run.sh` or replaced with a hard prescription.

### 4. Every run.sh call enumerates EVERY exit code
- `EXIT CODE 0:` / `EXIT CODE 1:` / etc., one per line, each its own bullet.
- Each code maps to a specific prescriptive action: "Continue to step N", "IMMEDIATELY terminate and surface the raw error", "Restart step 3".
- **No implicit "if it fails, do something reasonable".** Every code is named.

### 5. Mode dispatch via letter branches
- For multi-mode behavior inside a step, fork via `a. If in standard mode` / `b. If in <other> mode`.
- Each branch is fully self-contained.
- Prefer this over separate top-level steps when the branches share a numeric position in the flow.

### 6. All-caps imperatives at decision points
- Use `NEVER`, `IMMEDIATELY`, `CRITICAL`, `IMPORTANT`, `Under NO circumstance` exactly where the model is most likely to go off-rails.
- Sparingly but firmly.

### 7. `*NOTE*:` callouts for operational asides
- Use `*NOTE*:` inside steps for conditional-flag-threading and auxiliary mechanics.
- Demarcates aside-grade content from primary instructions.

### 8. No long paragraphs
- Content is bullets, tables, or 1–2 sentence imperatives.
- Zero philosophical prose, zero meta-commentary, zero "why we do this" outside Step 0's `Why this lives here`.

### 9. `run.sh` is the workhorse
- SKILL.md is mostly "call run.sh with X, react to exit code Y, read the file path it printed, re-emit verbatim".
- Anything algorithmic, stateful, polling, or retrying lives in `run.sh`.

### 10. Notes section is short and uniform
- 3–6 bullets, each prefixed with `IMPORTANT NOTE:` / `CRITICAL NOTE:` / `CRITICAL:`.
- Standard guardrails: don't mutate git, don't self-remediate `run.sh` failures, don't read `run.sh` internals.
- Skill-specific guardrails added sparingly.

### 11. Terminal step is a context-aware downstream dispatch (when applicable)
- The final step is often a hand-off to another skill or system action (e.g. `/workspace-merge`).
- Gate on multiple ALL-must-hold conditions, each enumerated.
- Skip the dispatch entirely when any gate fails — never half-do it.

### 12. `/iterate` integration (when applicable)
- Thread `--iteration` through frontmatter, argument table, and every exit path.
- `ITERATE_SIGNAL` is the last non-blank line; emit exactly one per exit path.
- Pair `CONTINUE ITERATION` with a `## Remediation Plan`; pair `TERMINATE` with no plan.
- **If a skill doesn't make sense to iterate, skip this entirely** — `/iterate`-compatibility is not a default requirement.

### 13. Implementation independence (black-box principle)
- Skills name the *contract*, never the *mechanism*. This rule applies to every part of the SKILL.md — step bodies, exit-code descriptions, `Why this lives here` sub-bullets, Notes guardrails, everything.
- **`run.sh` is a black box.** SKILL.md MUST NOT poke holes in it. CRITICAL: any description of what `run.sh` does internally — prefixing rules, suffix generation, file-write paths, atomic-mv patterns, env-var resolution order, fallback chains, the names of helpers it calls, the consumer that picks up its output — is FORBIDDEN. The skill says *which verb to call*, *what input to pipe in*, and *how to react to each exit code*. Nothing else about `run.sh`'s behavior may appear in SKILL.md.
  - When auditing, treat any sentence that begins "`run.sh` automatically …", "`run.sh` resolves …", "`run.sh` writes …", "`run.sh` falls back to …", or similar as a finding and strip it. Replace with passive contract language ("X is handled downstream", "the disambiguator is added downstream") that names neither the mechanism nor the resolver.
  - When auditing, treat references to `run.sh`'s output location, output naming, or its downstream consumer (file watcher, daemon, queue, etc.) as findings and strip them. The reader does not need to know where `run.sh` writes, what it names the artifact, or who reads it.
  - Test (apply to every sentence that mentions `run.sh`): if the sentence describes *what `run.sh` is doing under the hood* rather than *how the caller interacts with `run.sh`*, it is a leak. Rewrite or delete.
- **Forbidden** anywhere in the skill:
  - Process / daemon / plugin / module names.
  - Source-tree paths.
  - Protocol or wire-format details.
  - Build / install / packaging internals.
  - Internal symbol or variable names (including env-var naming conventions and state-key derivation rules).
  - Recital of what a wrapped script does internally — a SKILL.md call to `run.sh --foo` says only *what verb to call* and *how to react to each exit code*; the verb's internals are `run.sh`'s business.
  - State-management internals — `/tmp` file paths, file-naming conventions, lookup keys. If the model needs a value `run.sh` produces, `run.sh` provides a verb that returns it opaquely.
  - Exit-code descriptions that name an internal phase ("preflight failed") or internal symbol ("`$FOO` missing") instead of a user-facing condition ("setup failed irrecoverably", "invalid input").
  - `Why this lives here` content that explains *architecture* ("we use Y because mechanism Z") instead of *usage context* ("skipping causes CI to fail downstream").
  - Postmortems ("fixed in Y"), future-state speculation ("once X ships"), or reasoning about why the system is built the way it is.
- **Allowed**: commands to invoke, user-facing flags, behavior to expect, when to call which command.
- Heuristic: if the skill text would become inaccurate after a refactor that preserves user-facing CLI semantics, strip it.

### 14. Nested skill invocations via the Agent tool
- When a step needs to invoke another skill mid-flow, wrap the call in the `Agent` tool — don't invoke the sub-skill directly in the parent response stream.
- Subagent prompt: name the sub-skill, forbid further actions, specify a structured token response format.
- Parent step branches deterministically on the returned tokens.
- Skip the wrapper only when the sub-skill is trivial and is the last action of the parent flow.

---

## run.sh Conventions

If the skill has a `run.sh`:

- **Header comment**: documents every verb and every exit code.
- **`set -uo pipefail`** at the top.
- **Subcommand dispatch** via a top-level `case "${1:-}"` with `--flag`-style verbs.
- **Exit codes**: `0` success, `1` condition (not error), `2` script/usage error (use `die()`), `3+` domain-specific.
- **Helpers**: `die()` for fatal errors, `log()` for progress.
- **Default case**: print usage to stderr, exit 1.
- **State files**: `/tmp/<project>-<skill>-<identifier>` naming.
- **No interactive prompts.**

When in doubt, copy `check-cicd/run.sh`'s structure verbatim and adapt.

### When to extract logic into run.sh

| Condition | Reason |
|---|---|
| Shell logic >10 lines or has branching | Keep SKILL.md declarative. |
| Polling / retrying external tools | Deterministic logic belongs in a script. |
| State management across steps | Scripts manage state reliably. |
| Multiple exit codes convey different conditions | A dispatch table beats prose. |

---

## Steps

1. Parse arguments and determine mode.
  - Extract the mode (`new`, `audit`, or `update`) and the skill name.
  - **`new`**: continue to step 2.
  - **`audit`** / **`update`**: read `.claude/skills/<skill-name>/SKILL.md` (and `run.sh` if present). Continue to step 4.

2. Gather requirements (new mode only).
  - Ask the user (or infer from context):
    - **Name** — kebab-case, concise.
    - **Purpose** — one sentence covering what and when.
    - **`/iterate`-compatible?** — yes/no. Default: no.
    - **Complexity** — needs a `run.sh` driver, or is inline Bash sufficient?
    - **Allowed tools** — which Bash commands does it need?

3. Create the skill (new mode only).
  - Create `.claude/skills/<skill-name>/SKILL.md` (and `run.sh` if applicable) using `check-cicd` as the structural reference.
  - Mirror its section order, formatting, and tone.
  - Continue to step 4.

4. Audit the skill against every numbered convention (1–14).
  - For each convention, check whether the skill conforms; cite file:line for failures.
  - Also check `run.sh` against the `run.sh` Conventions when present.
  - **If the skill's name matches a family prefix in the Specializations section**, also audit it against that family's additional rules. Specialization failures are first-class audit findings, on equal footing with the numbered conventions.
  - Branch:
    - **`audit` mode**: emit the checklist results and stop. Modify no files.
    - **`update` mode**: emit the checklist results, then apply fixes for every failing check. Continue to step 5.
    - **`new` mode**: emit the checklist results. If any fail, apply fixes. Continue to step 5.

5. Verify (update and new modes only).
  - Re-read the final state of all skill files and confirm:
    - SKILL.md frontmatter is valid YAML.
    - Sections appear in the order: frontmatter → What This Skill Does → Arguments → (Iterate Signals if applicable) → Steps → Notes.
    - If `run.sh` exists: its header matches its actual verbs, and SKILL.md and `run.sh` agree on exit codes and stdout format.
    - No content was accidentally deleted or corrupted.

---

## Specializations

Specializations are named families of skills that share extra conventions on top of the general structure above. When the target skill's name matches a family's prefix, follow the family's conventions in addition to the general conventions. Specializations do **not** replace the general conventions — they layer on top.

### Position-analysis skills

- **Family prefix:** `analyze-position-*`.
- **Authoritative architectural docs:** `.claude/skills/analyze-position/MODEL.md` and `.claude/skills/analyze-position/taxonomy.mmd`.
- **Reference exemplars in the family:** `analyze-position-character`, `analyze-position-criticality`, `analyze-position-phase`.

In addition to the general conventions, every skill in this family follows these rules:

1. **Frontmatter is fixed.**
  - `argument-hint: "<prelim-json-path> [--defer-footer]"` — always.
  - `allowed-tools: Read, Bash(jq:*), Bash(gns cee:*)` — for leaf skills.
  - Parent skills (see #4) additionally need whatever tools they use to dispatch children (the `Agent` tool when dispatched via subagent).

2. **Arguments table is fixed.**
  - One row for `<prelim-json-path>` pointing at the `gns cee run prelim` response file written by `/analyze-position`'s setup step.
  - The row directs readers to `gns cee run prelim --help` for the response schema rather than re-documenting the schema inline.
  - One row for the optional `--defer-footer` flag that suppresses footer construction, directing readers to **Footer construction is opt-out** in `position-analysis-spec.md` rather than re-documenting the rule inline.

3. **Output Contract section is required**, replacing the freer `Steps` body the general conventions describe. It declares the response shape:
  - One **~50-sentence prose analysis**, no separate summary, no verdict line, no enumerated classification — the prose IS the entire output.
  - Every substantive claim is anchored on a specific move in **SAN** notation AND the specific **game-point handle** where it occurs -- this allows downstream agents (who are invoking the given position analysis skill) to guide their own analysis predicated on this information.
  - Thematic moves recurring across transpositions may cite several representative game points alongside the move instead of one.
  - **SAN only, never LAN, for string representation.** Where LAN is the only representation available, surface that fact explicitly rather than silently converting.

4. **Parent vs leaf is declared in the Output Contract.**
  - **Leaf analyst skills** analyze their aspect of the position directly and do not invoke other analyst skills, though they MAY invoke non-analysis presentation/utility skills (e.g. annotate-pgn) to render output.
  - **Parent analyst skills** dispatch one or more named child analyst skills via the `Agent` tool, then perform the MIXTURE from `MODEL.md` — synthesizing the children's prose AND gap-filling new analysis anchored on the children's SAN+gamepoint citations.
  - Parent skills name their permitted children explicitly in the Output Contract; the dispatcher does nothing else.
  - **Parent skills MUST dispatch every child with `--defer-footer`**, propagating the suppression transitively even when the parent itself received `--defer-footer`, so only the outermost caller renders a footer.

5. **Per-run memoization is the orchestrator's job, not the skill's.**
  - The skill body just runs its analysis whenever invoked.
  - The orchestrator (`/analyze-position` and the analysis-run controller) is responsible for ensuring each analyst skill executes at most once per analysis run, even when many parents depend on it.

6. **Notes section gains family-mandatory bullets:**
  - **CRITICAL NOTE: SAN only, never LAN.** Convert nothing; surface LAN-only cases explicitly.
  - **IMPORTANT NOTE: Always include the game point alongside the move** unless the move is genuinely thematic across multiple lines, in which case cite several example game points.
  - **IMPORTANT NOTE: Read the prelim file lazily via `jq`** for surgical field access. Do not load the whole file into context.
  - For **leaf** skills only: **CRITICAL NOTE: This skill is a leaf RPC in the position-analysis decomposition.** It delegates no further position analysis, so it MUST NOT invoke other analyst (`analyze-position-*`) skills, and MUST NOT edit files. It MAY invoke non-analysis presentation/utility skills (e.g. annotate-pgn) to render its output. External resources (CEE, and Wikipedia/Web where applicable) are not skills and are permitted.
  - For **parent** skills: omit the leaf-RPC restriction and instead add a CRITICAL note naming the exact set of permitted child skills.

---

## Notes

- **CRITICAL NOTE: Operate on exactly one skill at a time.** Do not read, modify, or create files outside `.claude/skills/<skill-name>/`.
- **CRITICAL NOTE: `audit` mode is read-only.** Report findings only — do not modify any files.
- **IMPORTANT NOTE: Do not execute the target skill.** This skill creates and evaluates skill definitions; it does not test them by running them.
- **IMPORTANT NOTE: Do not modify `AGENTS.md` or `CLAUDE.md`.** If the new skill needs to be referenced from project-level docs, note it in the report for the user to handle.
- **IMPORTANT NOTE: Do not create files outside the skill directory.** No temp files, no test files, no auxiliary artifacts elsewhere.
