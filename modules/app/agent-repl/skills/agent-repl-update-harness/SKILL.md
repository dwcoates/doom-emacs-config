---
name: agent-repl-update-harness
description: Change the agent harness agent-repl gives its agents — the metaprompt every session loads, a repository's AGENTS.md or CLAUDE.md, a skill, the daemon's prompt briefs, or any agent convention — by putting each instruction in its ONE right home, removing every restatement of it elsewhere, keeping a skill's availability (global, or this repository only) in step, and landing it through the merge queue. Use whenever any instruction to agents is added, changed, moved or retired, whenever the user asks to "update the metaprompt", "put this in AGENTS.md", "make a skill for this" or to codify a rule, whenever a rule turns out to be stated in more than one place, and whenever a skill should become available to other repositories or stop being so.
argument-hint: "<the instruction to add, change, move or retire>"
lineage_root: user.dodge.skills.agent-repl-update-harness
---

## What This Skill Does

Places one harness change in its one home and lands it. Every instruction to agents lives in exactly one place; every other file that needs it points there. The homes, and the test that picks each:

| Home | Holds | Test |
|---|---|---|
| A memory | a fact about the user, or one that holds only for a period | It would be wrong to tell every future agent as a standing rule. |
| The metaprompt (`metaprompt.md`) | how ANY agent working for the user behaves: their authority, the engineering values that hold everywhere, how responses address them, and how agents use agent-repl as an application on every task | It would still hold in an unrelated repository on an unrelated task. |
| A GLOBAL skill | the procedure for one kind of task or tool that agents need in ANY repository, agent-repl's application verbs included | It is needed only while doing that task, and outside agent-repl's own repository too. |
| A REPOSITORY skill | the procedure for one kind of task that only work on this repository's own code needs | It is needed only while doing that task, and only here. |
| A repository's `AGENTS.md` | the facts and contracts of that codebase: architecture, its invariants, how to build and test it, its conventions | It would still hold for a different person contributing to that repository. |

## Arguments

| Argument | Behaviour |
|---|---|
| `<the instruction to add, change, move or retire>` | The one harness change to place and land. |

## Steps

1. Classify the change.
  - a. Walk the home table top to bottom and take the FIRST home whose test the instruction passes.
  - b. When the instruction passes more than one test, take the narrowest home that is loaded everywhere the instruction applies.
  - c. CRITICAL: how agents behave when using agent-repl as an application, in any repository, NEVER goes in an `AGENTS.md`. It goes in the metaprompt when it applies to every task, and otherwise in a GLOBAL skill.
  - d. A memory is never written here: propose it in the response's memory branch, as the metaprompt's memory rule requires, and stop.

2. Find the instruction's current home and every restatement of it.
  - Search the metaprompt, every `AGENTS.md` and `CLAUDE.md` in the repository, every skill, and the daemon's prompt briefs for the rule's wording and its subject.
  - Record each hit as either the home (step 1's answer) or a restatement.

3. Write the instruction in its home, and only there.
  - a. A skill home is edited through `/create-or-update-skill`, dispatched in an `Agent` (`subagent_type: sonnet-medium`) that edits only that skill and returns `DONE` or `FAILED: <reason>` as its last line.
    - `DONE`: continue to step 3b.
    - `FAILED`: IMMEDIATELY stop and surface the reason.
  - b. Every restatement found in step 2 is replaced by a one-line pointer naming the home, or deleted where no pointer is needed.
  - c. NEVER restate the instruction in a second file "so it is loaded". A home that is not loaded where the instruction applies is the wrong home: return to step 1.

4. Keep skill availability in step (only when a skill is created, renamed, retired, or moves between GLOBAL and REPOSITORY).
  - a. A GLOBAL skill has a row in `/agent-repl-install`'s skill availability table AND an install requirement that links it into the user's skills directory.
  - b. A REPOSITORY skill has a link in this repository's own skills directory and NO global link.
  - c. A retired skill keeps a retired requirement, so a machine that linked it has the link removed.
  - d. The repository's tests check that the availability table and the requirements agree; run them with the touched suites.

5. Land the change.
  - Invoke `/agent-repl-merge-queue` and land the branch as an async merge, placement back.
  - *NOTE*: the metaprompt is read back by its own suites, so a metaprompt edit runs those suites at the gate like any code change.

## Notes

- CRITICAL: One instruction, one home. A second copy anywhere is a defect to remove, never a convenience to keep.
- CRITICAL: NEVER put agent-repl's application behavior in an `AGENTS.md`; only contributors to agent-repl's own code read it.
- IMPORTANT NOTE: A memory change is proposed, never written, by this skill.
- IMPORTANT NOTE: Editing a file outside the current repository still needs the user's per-use permission; this skill grants none.
- CRITICAL: NEVER merge by hand; the merge queue is the only path to the main branch.
