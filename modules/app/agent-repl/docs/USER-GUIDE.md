# agent-repl user guide

How to operate agent-repl from the keyboard. Every new or changed control
lands with an update here (see `AGENTS.md`, "User controls land with the
user guide"). Controls that predate this guide are not all listed yet.

## Selecting a bubble

One bubble at a time can be selected in the feed. A selected bubble has the
selection border and is centered when the selection moves to it. Only one
kind of bubble is ever selected: selecting a prompt replaces a selected
response, and the other way round.

| Key (input window) | What it does |
| --- | --- |
| `C-p` / `C-n` (command mode) | Select the previous / next final response. From no selection, both start at the newest. Both wrap at the ends. |
| `C-S-p` / `C-S-n` | Select the previous / next prompt you can roll back to. From no selection, both start at the newest. Both wrap at the ends. |
| `ESC` `ESC` (command mode) | Clear the selection. The first escape only warns. The feed returns to the newest row. |

- Clicking the feed's background also clears the selection.
- **A selection ends when its bubble scrolls out of view.**
  - This applies once the bubble is entirely outside the feed, whether you scrolled or new output pushed it away.
  - The feed stays where you left it.
  - So a prompt never goes out as a reply to a response you can no longer see.
- Only the main agent's prompts in the current conversation can be selected.
  - Prompts before the last `/clear` or compaction can't be selected.
  - Neither can a subagent's prompts, or a prompt folded into a turn that was already running.

## Jumping to an entry

These clicks jump to an entry in the feed:

- a background agent, shell or monitor row in the expanded footer;
- a breadcrumb above a subagent's feed;
- the "gated:" link on a hook card.

- **A jump opens the entry and centers it.**
  - Any subagent bubble the entry sits inside is opened first.
  - The entry itself is then expanded: a subagent, shell or merge bubble opens its feed, and a tool card opens its output.
  - The feed scrolls so the expanded entry sits in the middle of the view. Near the top or bottom of the feed it goes as close to the middle as the feed allows.
  - The entry is briefly marked so your eye lands on it.
- **An entry the jump opened closes again once you scroll it out of view.**
  - It closes only when no part of it is visible any more.
  - Scrolling back to it then shows it closed.
  - Each jump's entry is watched on its own. A second jump never closes the first one's entry early.
- **An entry you opened yourself stays open when you scroll away from it.**
  - A jump to an entry that was already open leaves it as it is.
  - Clicking an entry the jump opened, to close or reopen it, makes it yours: the jump no longer closes it.
- **Scrolling back to the bottom of the feed closes every open entry.**
  - This happens when the newest entry comes back into view and the feed starts following new output again.
  - It closes the entries you opened and any a jump opened that are still open.
  - Switching to another window or application also closes open tool output and bubble text, as before.
- **If a subagent's feed cannot be opened, the jump still scrolls to its bubble.**
  - The failure is listed under the warning chip in the top bar.

## Background work in the expanded footer

The expanded footer lists the running background agents, shells and monitors, one row each.

- **Clicking anywhere on a row jumps to its entry in the feed.**
  - Hovering anywhere on the row highlights the whole row.
  - The jump works as described in "Jumping to an entry".
- **The agents list has columns.**
  - Its header line starts with the "stop all" button, which stops every running background agent.
  - The "tokens" and "duration" headers sit above their columns.
  - Every row's figures line up under those headers, so a changing clock never moves the token count.

## Replying to a response

Select a final response with `C-p` / `C-n` and send a prompt with `RET`. The
agent receives a copy of that response ahead of your prompt, so it knows what
you are replying to. Sending ends the selection.

## Rolling the conversation back

| Key (input window) | With a prompt selected | With nothing selected |
| --- | --- | --- |
| `C-c C-RET` | Roll the conversation back to just before the selected prompt. | Roll back the latest prompt: this cancels it. |
| `C-c M-RET` | The same, and also restore files to their state when that prompt was sent. | The same for the latest prompt, with files. |

- `RET` still sends, and `C-c C-k` still only interrupts.
- **Every rollback asks for confirmation in the minibuffer.**
  - It names the prompt and says whether files are restored.
  - It names the other key, so you can switch.
  - Side effects are listed in red:
    - a running turn is interrupted;
    - queued prompts sent after that prompt are dropped;
    - with `C-c M-RET`, background agents and shells that those prompts started are stopped. Monitors started then are stopped too.
- **After a rollback:**
  - the dropped prompts and everything after them are gone from the feed, for good;
  - the input window's previous contents are saved to its history;
  - the input window holds the rolled-back prompt's text and attachments, ready to edit and send again.
- **Limits of `C-c M-RET` (file restore):**
  - Only changes made by the agent's own file-editing tools are restored. Changes made by shell commands (`sed -i`, a build, `git checkout`) are not.
  - Git commits are not undone, so files and history can disagree after a restore.
  - Only prompts sent while file checkpointing was on can restore files. It is on for every session from this version on.
- **Refusals:**
  - The first prompt of a conversation cannot be rolled back. `/clear` starts over.
  - If anything changed between planning the rollback and confirming it (a prompt was sent or queued, a turn started or ended), the rollback is refused and nothing happens. Press the key again.
  - If the vendor refuses to cut the conversation, nothing is rolled back and its reason is shown.

## Changing the reasoning effort

The top bar's effort selector sits between the model selector and the permission-mode picker.

- **It shows the effort level the session runs at, as Claude itself reports it.**
  - Before the session first reports it, the selector shows the level your account's Claude `settings.json` (or `CLAUDE_CODE_EFFORT_LEVEL`) sets for the model.
  - It shows a dash only while nothing has stated a level yet.
- **Clicking it lists the levels the session's model accepts; clicking a level switches to it.**
  - The new level applies from the next turn on and holds for the rest of the session in this workspace, across hibernation.
  - Nothing is added to the feed for the switch.
  - Switching causes token cache misses, as switching the model does; both selectors say so on hover.
- **A model that takes no effort level shows a dash and offers no list.**

## When Claude does not start

The footer's `disconnected` status has three vendor substatuses for a Claude SDK that did not start. They are blue: the workspace is unusable until it is up.

| Footer shows | What it means | What to do |
| --- | --- | --- |
| `disconnected · vendor retry` | The Claude SDK did not start and is being retried on a backoff, for up to 10 minutes. The activity line shows the attempt and the cause. | Nothing. A transient failure usually clears by itself. |
| `disconnected · vendor rejection` | The Claude SDK refused to start for a reason retrying cannot change, such as a rejected credential or a missing model. It will not be retried. The activity line shows why and names the restart key. | Fix the cause, then `SPC o C-c`. |
| `disconnected · vendor failed` | Every retry failed for the whole 10 minutes, so the daemon gave up. The activity line reads "Claude SDK failed to start", with the restart key. | `SPC o C-c`. |

- **Prompts you send while the session is down are held, not lost.**
  - They wait in the tray with the badge "after reconnect".
  - They are delivered when a session next comes up on the workspace, however it comes up.
  - A failed start never drops them.

## Restarting a stuck workspace

`SPC o C-c` restarts the current workspace's backend and page. It is the way to unstick one workspace without rebooting everything.

- **What it bounces:**
  - The workspace's shim, rebuilt first if its build is stale, relaunched with the same session resumed.
  - The workspace's webapp page, reloaded.
  - Never the daemon, the store or the sidecar.
- **It is immediate and takes no prefix argument.**
  - There is no graceful mode: the running turn and all detached work are hard-stopped, because a workspace needs this when something is stuck.
  - Prompts sent meanwhile are held "after reconnect" and delivered once the session is back.
- **When to use it:**
  - A turn never ends.
  - The footer reads `vendor rejection` or `vendor failed`, or the Claude SDK is not responding.
  - The workspace runs a stale shim build.
  - The page is out of sync with the workspace.
- **When not to use it:**
  - The daemon is down: restart is a request to the daemon, so it cannot help.
  - The store or the sidecar has a problem.
  - You only want the page reloaded: use `SPC o l`.
- **It is not a routine action.** A freshly launched shim or page may speak a newer API than the daemon that is still running, so reach for it only when a workspace is stuck.
