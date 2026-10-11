---
name: agent-repl-help
description: Answer a question about agent-repl, the Emacs-fronted Claude workspace system in this repository — what it is, how it works, what a feature does and how to use it, and where its user guide, architecture and developer documentation, protobuf contract, and code live. Read-only, never implements or edits anything without the user's explicit approval. Use whenever the user asks how agent-repl or one of its features works, which key or command does something, where a feature is documented or implemented, or invokes /agent-repl-help (the SPC o h help workspace opens with it).
argument-hint: "<question about agent-repl>"
allowed-tools: Read, Grep, Glob
lineage_root: user.dodge.skills.agent-repl-help
---

## What This Skill Does

Answers a question about agent-repl from its own documentation and code. This file carries the one-paragraph picture of the system; `references/MAP.md` maps every documentation set and code area by question kind, and the answer is read out of the sources it points to, never recalled.

## Arguments

| Argument | Behaviour |
|---|---|
| `<question>` | The question to answer, in the user's words. |

## Steps

0. Load the map.
  - Read `references/MAP.md` in this skill's directory, in full.
  - **Why this lives here**: the map is the only index of where each kind of answer lives, so every later step routes through it.

1. Classify the question by the map's question kinds.
  - A question may carry more than one kind, so note every kind that applies.

2. Read the sources the map names for each kind.
  - a. If the map names a document, read the section of it that covers the question.
  - b. If the map names a code area, search it for the feature's command, key, message, or symbol, and read the matches.
  - c. If the documentation and the code disagree, the code is how it works today. Say so, and name both places.
  - d. If no source the map names covers the question, search the whole agent-repl module for the feature's words before concluding it is undocumented.

3. Answer.
  - State what the feature is and how to use it first, then how it works, then where it lives.
  - Cite every claim with the file and line it came from, as a full path starting at `~/` in this workspace's worktree.
  - When a feature is undocumented, say so plainly and point to the code that implements it.

4. Hold every follow-up to the same rules.
  - Answer later questions in the conversation through steps 1 to 3.
  - When the user asks for a change, describe the change and where it would land, then STOP and wait for the user's explicit approval before editing anything.

## Notes

- **CRITICAL: NEVER edit, create, or delete a file, and NEVER commit, without the user's explicit approval for that specific change.** A question is not a commission.
- **CRITICAL NOTE: Never answer from memory.** Every claim comes from a source read in this conversation, cited by path and line.
- **IMPORTANT NOTE: The map points, the sources answer.** Do not treat the map's one-line descriptions as the answer to a question.
- **IMPORTANT NOTE: A gap in the map is worth reporting.** When an answer came from a source the map does not name, say so at the end of the answer so the map can be extended.
