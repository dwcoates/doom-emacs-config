# agent-repl user guide

How to operate agent-repl from the keyboard. Every new or changed control
lands with an update here (see `AGENTS.md`, "User controls land with the
user guide"). Controls that predate this guide are not all listed yet.

## Setup

agent-repl names no person and holds no person's values as defaults. The
values below are yours to set. A value that is missing or does not match
fails loudly, and the error points back to this section.

Environment variables go in your shell profile. Then run `doom env` so Emacs,
and the daemon Emacs starts, see them.

| Value | Where it is set | What reads it | When it is missing |
| --- | --- | --- | --- |
| Your work account | Sign in to Claude with `~/.claude-chesscom` as the config dir. Its email is read from `~/.claude-chesscom/.claude.json`. | Workspaces under `$MULTI_REPO_ROOT`, and the GNS login rule in `metaprompt.md`. | Those workspaces run logged out. An agent that needs to renew a GNS token stops and says so. |
| A Chrome profile per account | Sign each account's email in to its own Chrome profile. | Links clicked in a workspace open in the profile signed in as that workspace's account. A logged-out workspace opens links with no profile chosen. | The click fails with `launch_failed`, naming the email that no profile is signed in as. |
| `AGENT_REPL_PERSISTENT_WIFI_HOTSPOT` | Your phone hotspot's name, in your shell profile. Either apostrophe spelling joins. | Persistent-wifi mode, which joins the hotspot when it turns on and leaves it when it turns off. | Turning the mode on or off still changes the power settings. The hotspot step fails, naming the variable. |
| `AGENT_WORKSPACE_PREFIX` | Optional. A prefix for new workspace branches, such as your initials, in your shell profile. The older `CLAUDE_WORKSPACE_PREFIX` is still read when this one is unset. | Workspace creation, which names a branch `<prefix>/<name>`. | Branches get no prefix. |

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
  - A merge bubble is the exception: a jump that opened it leaves it open.
  - Scrolling back to it then shows it closed.
  - Each jump's entry is watched on its own. A second jump never closes the first one's entry early.
- **An entry you opened yourself stays open when you scroll away from it.**
  - A jump to an entry that was already open leaves it as it is.
  - Clicking an entry the jump opened, to close or reopen it, makes it yours: the jump no longer closes it.
- **Scrolling back to the bottom of the feed closes every open entry.**
  - This happens when the newest entry comes back into view and the feed starts following new output again.
  - It closes the entries you opened and any a jump opened that are still open.
  - A merge bubble is the exception: once you open it, it stays open until you close it.
  - Switching to another window or application also closes open tool output and bubble text, as before.

## The merge bubble

- **A merge bubble is open while the merge is queued, runs, waits on you, hits conflicts, fails or is abandoned.**
  - Click its head to close it; it stays closed until you click the head again.
- **A merge bubble never closes on its own, with one exception: a merge that succeeds closes its bubble.**
  - Scrolling back to the bottom of the feed, scrolling it out of view after a jump opened it, switching windows and new merge progress all leave it open.
  - A merge that lands closes its bubble once, and a landed merge's bubble is drawn closed after a reload or a restart too.
- **Your open or closed choice always wins, and survives a reload.**
  - Open a landed merge's bubble and it stays open, across a reload too; close a running merge's bubble and it stays closed, even when the merge later fails.
  - Reloading the page, restarting Emacs or restarting the daemon draws the bubble the way you left it.
  - If the daemon cannot record your choice, the failure is listed under the warning chip in the top bar, and the bubble stays the way you left it on this page.
- **A merge never opens the expanded footer.** The bubble's tests tab lists the suites, each with passed/failed/total counts on the right.
- **If a subagent's feed cannot be opened, the jump still scrolls to its bubble.**
  - The failure is listed under the warning chip in the top bar.

## File links in prompts and responses

Clicking a link to a file (not a web address) in a prompt or response bubble opens that file in Emacs, in the same popup a findings row or a plan's edit button opens.

- **The daemon decides which file a link means, trying these in order.**
  - An absolute path opens as written.
  - A path with a directory part opens relative to the workspace's worktree.
  - A bare file name opens from `modules/app/agent-repl/` in the worktree, then from the worktree's git root.
  - A `:<line>` suffix (`core.el:42`) lands on that line.
- **A link that leaves the worktree is refused.**
- **A bare name whose ending is also a web domain** (`.org`, `.md`, `.sh`, `.py`, `.rs`) is tried as a file first, and opens as a web address when no file matches, with nothing reported.
- **Any other link that names no file opens nothing.**
  - The footer shows "unknown file" with the name for a moment, and the status is unchanged.
  - The workspace's agent is asked, after its running turn and never interrupting it, which file it meant.
  - The question quotes the bubble you clicked in, as a reply to a selected bubble does.
  - The agent is also asked to propose an agent-repl change that avoids such links or teaches the resolver a further fallback.

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

## Outcome markers in the feed

An event that is not a message is drawn in the feed as an **outcome marker**: a small pill, placed where the event happened. A turn's ending sits after that turn's last output.

- **What a marker says:**
  - A glyph, a short label, and sometimes a detail: `◼ interrupted`, `◆ vendor error · rate limited`, `✕ agent-repl · process died`.
  - The glyph and the thin left edge carry the color: grey for your own acts and hooks, turquoise when the vendor ended or refused the work, blue when agent-repl's own machinery died.
  - The same marker stands for an interrupted turn, a Stop hook that ended the run, a permission you denied, a plan episode that broke, and a compaction that failed.
  - A failed compaction is shown only by its marker; the footer draws no line for it.
- **Opening a fault marker:**
  - Click a turquoise or blue marker (it draws a `›` chevron) to open its details in place; click it again to close them.
  - A grey marker has no chevron and does not open.
  - A vendor fault shows when it happened, the vendor's error type and message, the retries the vendor made, a countdown to its stated wait, and the model and account the turn ran on, each only when known.
  - An agent-repl fault shows when it happened, what died (the query or the agent process), what the query threw, and whether the session started again afterwards.
- **Its actions:**
  - **sign in** appears when the vendor rejected the credential or the organization; it opens the same login flow as the topbar's account cell.
  - **resend this prompt** appears when the turn produced nothing; it sends the same prompt again as a new turn.
  - A refused resend says why beside the button.
- **The workspace status carries the fault too.** Until the next turn starts, the footer reads `vendor fault · vendor error` (or the account block's own step) or `agent repl fault · turn died`, with the cause on its activity line, and the sidebar dot and the tab take the same color. The composer stays open, so the next prompt is what clears it.

## The editor popup

Every file or directory agent-repl shows you opens in **the editor popup**.

- **What it looks like:**
  - A popup on the right side of the frame, 40% of its width, with focus in it.
  - A file opens at the line given, or at its top when none is.
  - A directory opens in dired.
  - `q` in command mode closes it and kills its buffer, saving the file first.
- **What opens it:**
  - A plan bubble's edit button, and a findings row's location.
  - A link in a feed bubble: click it.
  - The notes file, and the paths in the worktree divider.
- **How a feed link finds its file:**
  1. An absolute path opens as it is.
  2. A relative path is taken from the worktree root.
  3. A bare name is looked for in `<worktree>/modules/app/agent-repl/<name>`, then in `<git root>/<name>`.
  4. If none exists, the footer briefly says "unknown file" with the name, and the agent is sent a follow-up question about which file you meant.
