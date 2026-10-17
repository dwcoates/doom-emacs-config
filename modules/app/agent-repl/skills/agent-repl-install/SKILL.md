---
name: install-agent-repl
description: Set up everything agent-repl needs on a computer — check every requirement, install what is missing or outdated (Emacs with xwidgets, toolchains, terminal-notifier, the store, sidecar and metrics-store services, skills, the git hook), walk the user through what only they can provide, and fix the requirement manifests generically when a run finds them inadequate. Use when setting up or bootstrapping agent-repl on a new machine, when an error points at the installer or a missing dependency, when the user asks whether this machine has everything agent-repl needs, or when invoked as /install-agent-repl. Update this skill whenever the installation process is expanded or changed.
argument-hint: "[check]"
allowed-tools: Bash(.claude/skills/install-agent-repl/run.sh:*)
lineage_root: user.dodge.skills.install-agent-repl
---

## What This Skill Does

Brings a machine to the point where every agent-repl requirement is met, and leaves the requirement manifests better than it found them.

## Arguments

| Argument | Behaviour |
|---|---|
| *(none)* | Check, install with the user's consent, walk the user through what remains, fix inadequate manifests, and finish with `doom doctor`. |
| `check` | Report every requirement's status and stop. Changes nothing. |

## Steps

0. Report the machine's current state.
  - Call `.claude/skills/install-agent-repl/run.sh --check`.
    - `EXIT CODE 0:` Every required requirement is met. Relay the report. If `check` was passed, STOP; otherwise go to step 5.
    - `EXIT CODE 1:` Some required requirement is unmet. Relay the report. If `check` was passed, STOP; otherwise continue to step 1.
    - `EXIT CODE 2:` The requirements could not be read. Go to step 4 with the printed error as the failure.
  - **Why this lives here**: the user sees exactly what will change before anything outside the repository is touched.

1. Get consent for changes outside the repository.
  - List every `missing` and `outdated` line from the step 0 report as a change install would make.
  - Ask the user to approve installing them.
    - a. On approval, continue to step 2.
    - b. On decline, STOP and leave the machine unchanged.
  - CRITICAL: NEVER run step 2 without this approval, because it installs packages, loads launchd services, and writes links under the home directory.

2. Install.
  - Call `.claude/skills/install-agent-repl/run.sh --install`.
    - `EXIT CODE 0:` Every required requirement is met. Go to step 5.
    - `EXIT CODE 1:` Some required requirement is still unmet. Continue to step 3.
    - `EXIT CODE 2:` The requirements could not be read. Go to step 4 with the printed error as the failure.

3. Walk the user through every remaining unmet requirement.
  - For each `needs-user` line, and each line whose install step needs the user:
    - a. Give the user the line's fix exactly as printed.
    - b. For an environment variable, have the user add it to their shell profile; it reaches Emacs at step 5.
    - c. For a notification permission, have the user allow the named program in System Settings → Notifications.
  - For each `failed` line, and each line whose install ran but left it unmet, go to step 4 with that line as the failure.
  - After the user reports each one done, return to step 0.

4. Fix the requirement manifests generically.
  - A failure the manifests should have prevented is fixed in the manifests or the runner, per the Requirements section of the agent-repl module's AGENTS.md, and NEVER only on this machine.
    - a. Make the fix hold for every machine and every person, and NEVER encode this user's paths, accounts, names or choices.
    - b. A per-person value becomes an environment-variable entry with its row in the user guide's Setup table.
    - c. Add or update the test that pins the fix.
  - Call `.claude/skills/install-agent-repl/run.sh --verify`.
    - `EXIT CODE 0:` The suites pass. Commit the fix, then return to step 0.
    - `EXIT CODE 1:` A suite failed. Fix the failure, then restart step 4's verification.
  - Ambiguity only the user can resolve goes in the response's dedicated setup branch, and is NEVER guessed.

5. Refresh Doom and confirm.
  - Call `.claude/skills/install-agent-repl/run.sh --finish`.
    - `EXIT CODE 0:` Doom is refreshed and the doctor found nothing. Report the machine ready.
    - `EXIT CODE 1:` The doctor reported findings. Relay them, then return to step 0.
    - `EXIT CODE 2:` Doom could not be refreshed. IMMEDIATELY terminate and surface the raw error.

## Notes

- **CRITICAL NOTE: Nothing outside the repository changes without the user's approval in step 1.**
- **CRITICAL NOTE: A fix is generic or it is not a fix.** Hand-repairing this machine past a failure leaves the next machine to fail the same way.
- **IMPORTANT NOTE: Never overwrite a file of the user's.** A `needs-user` line naming one is the user's to move.
- **CRITICAL NOTE: Do not read `run.sh` internals.** React only to the documented exit codes.
