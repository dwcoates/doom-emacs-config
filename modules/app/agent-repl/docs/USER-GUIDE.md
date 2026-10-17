# agent-repl user guide

How to operate agent-repl from the keyboard. Every new or changed control
lands with an update here (see `AGENTS.md`, "User controls land with the
user guide"). Controls that predate this guide are not all listed yet.

## Setup

Run `bash .claude/install.sh` from this repository, or ask an agent to run
`/install-agent-repl`. It installs everything agent-repl needs that is missing:
Emacs with xwidgets, the Go and Node toolchains, `terminal-notifier`,
`hunspell` and the input window's spelling dictionary, the Claude Code skills, the git hook, and the store and sidecar services. It then
lists what only you can provide. `bash .claude/install.sh check` reports the
same thing without changing anything, and `doom doctor` reports it too.

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
| `MULTI_REPO_ROOT` | Optional. The directory holding your work repositories, including the `explanation-engine` checkout, in your shell profile. | The installer, which links the `/create-or-update-workspace` and `/create-or-update-skill` skills from `$MULTI_REPO_ROOT/explanation-engine`, workspaces worked under the work account, and `SPC TAB O`, which makes its one-shot in `$MULTI_REPO_ROOT/explanation-engine`. | Those two skills are not linked, and the installer reports it. `SPC TAB O` refuses, naming the variable. |
| `AGENT_WORKSPACE_PREFIX` | Optional. A prefix for new workspace branches, such as your initials, in your shell profile. The older `CLAUDE_WORKSPACE_PREFIX` is still read when this one is unset. | Workspace creation, which names a branch `<prefix>/<name>`. | Branches get no prefix. |

## Asking about agent-repl

`SPC o h` asks a question about agent-repl in a new help workspace.

- It asks for the question alone, and the composer's text is the default.
- The workspace is made in the repository agent-repl's own source lives in, whichever workspace you ask from.
  - That repository must be on the roster. If it is not, the command says so. Add it with `SPC TAB C-n`.
- The agent answers with the `/agent-repl-help` skill, which maps agent-repl's user, architecture and developer documentation and its code.
- The agent changes nothing without your explicit approval, in that turn or any later one.
  - The help workspace is not a one-shot. Nothing merges when the answer is done, and you can keep asking.
- The opening instruction the agent receives is `agent-repl-help-preamble`. It is hidden from the drawn prompt.

## Selecting a bubble

One bubble at a time can be selected in the feed. A selected bubble has the
selection border and is centered when the selection moves to it. Only one
kind of bubble is ever selected: selecting a prompt replaces a selected
response, and the other way round.

| Key (input window) | What it does |
| --- | --- |
| `M-s-k` / `M-s-j` | Select the previous / next prompt or final response, moving up / down the feed. The walk covers the prompts you can roll back to and the final responses together, in feed order. From no selection, `M-s-k` starts at the newest and `M-s-j` at the oldest. `M-s-k` wraps past the oldest. `M-s-j` past the newest clears the selection and returns the feed to the newest row. |
| `RET` | With a prompt selected, roll the conversation back to just before it, keeping files (as `C-c C-RET`). With any other bubble selected, send the input window as a reply to it. |
| `C-s`, or `/` / `?` (command mode) | Search the selected bubble. It opens read-only in a popup, and the search runs there. |
| `ESC` `ESC` (command mode) | Clear the selection. The first escape only warns. The feed returns to the newest row. |

- Clicking the feed's background also clears the selection.
- Clicking the selected bubble clears the selection too, and the feed stays where it is.
- **A selection ends when its bubble scrolls out of view.**
  - This applies once the bubble is entirely outside the feed, whether you scrolled or new output pushed it away.
  - The feed stays where you left it.
  - So a prompt never goes out as a reply to a response you can no longer see.
- Only the main agent's prompts in the current conversation can be selected.
  - Prompts before the last `/clear` or compaction can't be selected.
  - Neither can a subagent's prompts, or a prompt folded into a turn that was already running.
- **A thinking bubble or an interim response is never selected.**
  - An interim response is one the agent wrote mid-turn, before the turn's answer.
  - Clicking one only opens it to its full text, and clicking it again closes it.
  - An open one has no selection fill or border, and the selection stays where it was.
  - A response you selected while it was the newest row stays selected once the agent moves on, and a reply still quotes it.
  - It closes again once it scrolls entirely out of view, or when you return to the newest row.

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

- **A merge's bubble appears the moment the merge is queued, and stays at the bottom of the feed until the merge lands, fails or is abandoned.**
  - Anything written while the merge waits in line or runs, such as a background agent's turn, lands above the bubble.
  - When the merge finishes, the bubble stays where it finished, and everything written afterward lands below it.
  - While it waits for your workspace, the footer reads `merging · waiting for` what is still running (the turn, subagents, shells), and the sidebar dot and the Emacs tab are purple with the waiting glyph.
  - Prompts you send meanwhile are held, as during the merge.
- **A merge bubble is open while the merge is queued, runs, waits on you, hits conflicts, fails or is abandoned.**
  - While it is pinned to the bottom of the feed it cannot be closed.
  - Once it stays where it finished, click its head to close it; it stays closed until you click the head again.
- **`C-c C-k` with no turn running, while your workspace's sync merge is queued, asks what to do with the merge.**
  - The question covers the pinned bubble's tabs, and its buttons take keyboard focus.
  - `Evict` takes the merge off the queue.
  - `Prioritize` moves it to the front of the merges waiting, so it runs next.
  - `Cancel` leaves the queue as it is.
  - Pressing `C-c C-k` changes nothing on the queue by itself, and offers no rollback.
- **A merge bubble never closes on its own, with one exception: a merge that succeeds closes its bubble.**
  - Scrolling back to the bottom of the feed, scrolling it out of view after a jump opened it, switching windows and new merge progress all leave it open.
  - A merge that lands closes its bubble once, and a landed merge's bubble is drawn closed after a reload or a restart too.
- **Your open or closed choice always wins, and survives a reload.**
  - Open a landed merge's bubble and it stays open, across a reload too; close a running merge's bubble and it stays closed, even when the merge later fails.
  - Reloading the page, restarting Emacs or restarting the daemon draws the bubble the way you left it.
  - If the daemon cannot record your choice, the failure is listed under the warning chip in the top bar, and the bubble stays the way you left it on this page.
- **A merge that lands on agent-repl's master shows the version it landed as on the bubble's head.**
  - Every merge an agent asks for onto agent-repl's master says whether it moves the version's minor or patch number, with a one-line changelog entry.
  - The merge queue tags the landed merge with the next version (for example `0.5.0`), and the changelog entry is that tag's annotation.
  - The head draws the version after the clock, the number the merge moved in green.
  - Only a person moves the major version, by tagging master by hand; the next merge counts up from that tag.
  - Push master to origin with `modules/app/agent-repl/bin/push-release.sh`, which tags master's tip `release-<version>` and pushes master and that tag together.
    - The `pre-push` hook refuses a master push whose tip has no version, and one that does not carry its `release-<version>` tag.
- **A merge never opens the expanded footer.**
- **The bubble has one rebasing tab and one tests tab, each with tabs of its own inside.**
  - The rebasing tab has a tab for each commit being replayed, named by its short hash and subject; commits the rebase has not reached yet show no clock.
  - A commit whose replay conflicted shows the conflicted files, and below them the agent's conflict resolution as it streams.
  - The tests tab has a tab for each full run of the suites (`run 1`, `run 2`, ...).
  - Only one merge of a repository runs its tests at a time; the merges ready to test take turns, first come first served.
    - A run waiting its turn shows a hollow `○`, keeps its clock ticking, and reads "waiting for the test turn: <workspace>'s tests run first".
    - While one merge's agent fixes a failed run, the other merge's tests run, and the fixed one runs again once they end.
  - Each run has an `output` tab listing the suites, each with passed/failed/total counts on the right, and the run's log; a failed run that the agent then fixed also has a `fixing` tab streaming that fix.
  - A merge that starts over because the target moved replays under the same rebasing tab, its commits marked `(2)`, and its runs keep counting under the same tests tab.
  - Each strip shows its most relevant tab: the one whose state changed last.
    - A tab that starts, or starts waiting on you, is shown.
    - When a tab finishes, the strip goes back to the tab still running, such as the queue tab of a merge that is waiting in line again after rebasing.
    - With nothing running, the tab that finished last stays shown.
  - Each strip keeps the tab you clicked until a tab in it starts, waits on you, or finishes.
- **If a subagent's feed cannot be opened, the jump still scrolls to its bubble.**
  - The failure is listed under the warning chip in the top bar.

## Sync and async merges

- **A merge is a ONE-SHOT or ASYNC.**
  - A one-shot is the only sync merge: it runs in your workspace, your agent resolves its conflicts and fixes its tests in the bubble's tabs, your prompts are held until it ends, and your workspace closes once it lands.
    - The prompts you queued before the agent asked for it are sent to the agent at once, in order, without interrupting it, so it answers them before the merge starts.
    - Prompts you send once the merge is running wait for it to end.
    - A one-shot that fails leaves your workspace open, and the agent is told why.
  - An async merge lands a named branch in a hidden merge workspace forked from yours, and your workspace stays free; the agent is told how it ended.
  - "Sync" and "in place" mean the same thing, and so do "async" and "detached".
- **An agent uses `/merge-queue one-shot` only when your workspace is done once the merge lands.**
  - The workspace is marked a one-shot at once, so its tab and its sidebar row show the one-shot mark (a red-and-white target) while the agent keeps working.
  - When the turn ends, the workspace's branch merges in place, and the workspace closes once the merge lands.
  - Work left uncommitted when the turn ends fails the merge, and the workspace stays open.
  - `SPC TAB M` and the sidebar's Merge are the same one-shot.
  - The main workspace (the repository's own checkout) never one-shots.
- **Every other merge is `/merge-queue async <name> <branch>`.**
  - It is for a fix beside the point of what remains, an attempt landed so you can test it on master, or the main workspace landing its subagents' branches.
  - The agent names the merge workspace; a name that is already a workspace, a branch or another merge's reservation is refused, and the agent asks again with another.
  - The branch is never the one checked out in the asking workspace; the daemon refuses it.
  - `instruction <text>` has the merge workspace prepare the branch first, for example rebasing it onto master or combining branches; that work is the bubble's first tab, `resolving`, and the merge joins the queue once it ends.
  - `close <worktree-dir>` closes another workspace once the merge lands, such as the subagent workspace the work came from.
  - `cleanup` removes the worktree that had the branch checked out once the merge lands; branches are kept.
  - `before-merge <prompt>` and `after-merge <prompt>` are prompts the merge workspace runs before the landing and after it.
    - A failed `before-merge` prompt fails the merge, and `after-merge` never does.
    - The merge workspace runs only the prompts its request names, never the ones the asking workspace was created with.
  - The main workspace can ask for an async merge; it lands in the repository's own checkout.
- **An async merge workspace is hidden from the tab bar and the sidebar's open rows.**
  - It is listed among the closed workspaces, and under Recently Merged once it lands.
  - The bubble is drawn only in your feed, placed where it was asked for rather than pinned, and its resolving, conflicts and fixes tabs stream the merge workspace's agent.
- **The merge workspace is revealed, and stays visible, when:**
  - the merge fails;
  - its agent asks a question or for a permission;
  - you press a running tab's `stop`;
  - you select it.
- **A revealed merge workspace is an ordinary workspace.**
  - Its feed has no merge bubble, and its footer shows its own statuses.
  - The bubble's header then carries an `open merge workspace` button.
- **A running agentic tab's `stop` stops only that merge's agent.**
  - `C-c C-k` in your own workspace never stops an async merge.
- **When an async merge ends, a badge in your feed says how (`merge of <branch> landed`), and your agent is told.**
  - A landed merge closes its merge workspace; its conversation stays there to read.
  - A failed merge leaves the merge workspace open where it stopped, for you to finish.
  - A merge you or an agent take off the queue on purpose ends its bubble, but your agent is not told: nothing failed.
- **A merge workspace nobody has used for 15 calendar days is removed.**
  - The bubble's button then reads `merge workspace removed` and does nothing.
- **Your own workspace is retired a day after its async merge lands, if nobody has used it since.**
  - Its session stops, its worktree is removed, and it moves to Recently Merged, as a sync merge leaves it.
  - Its branch is kept.
  - It is kept while it holds commits or changes the merge did not take, while anything runs or is held in it, or while another open workspace was cut from it.

## File links in prompts and responses

Clicking a link to a file (not a web address) in a prompt or response bubble opens that file in Emacs, in the same popup a findings row or a plan's edit button opens.

- **A path in inline code that starts at `~/` is a link.**
  - The whole code span must be the path, with an optional `:<line>` suffix (`` `~/.config/doom/config.el:12` ``).
  - It keeps its code styling.
  - Any other inline code (a bare file name, a relative path, a command) stays plain code.

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

## Answering a question or permission by number

While the agent waits on a question or a permission card, type the choice's number into the empty input window. The answer is sent at once, with no `RET`.

- **The numbers are the ones drawn on the card.**
  - A question's options are numbered from `1.`, in order.
  - A permission's buttons are numbered `1.` allow once, `2.` always allow when the card offers it, then deny.
- **On a single-select question, the number answers the question.**
- **On a multi-select question, the number ticks or unticks that option.**
  - `C-RET` in the empty input window confirms the ticks.
- **A number is typed as usual when it cannot be an answer.**
  - This covers text already in the input window, a number the card has no choice for, `0`, and no card waiting.
- **Typed text is still an answer.**
  - A sent sentence answers the shown question in your own words, or denies the permission with that sentence as the reason.
- **Every answer shows on the card at once.**
  - On a card of several questions, the card moves on to the next unanswered question.
  - Each answered question stays listed above it, with its header and its answer, typed words in italics.

## Replying to a response

Select a final response with `M-s-k` / `M-s-j` and send a prompt with `RET`. The
agent receives a copy of that response ahead of your prompt, so it knows what
you are replying to. Sending ends the selection.

## Rolling the conversation back

| Key (input window) | With a prompt selected | With nothing selected |
| --- | --- | --- |
| `RET` | Roll the conversation back to just before the selected prompt, keeping files. | Send. |
| `C-c C-RET` | Roll the conversation back to just before the selected prompt. | Roll back the latest prompt: this cancels it. |
| `C-c M-RET` | The same, and also restore files to their state when that prompt was sent. | The same for the latest prompt, with files. |

- **`C-c C-k` on a running prompt that nothing has answered yet stops the turn and rolls the prompt back, in one press.**
  - The prompt's words land in the input window, and the feed shows no "interrupted" marker.
  - A prompt counts as answered once a response bubble or a purple-titled card has landed after the prompt.
  - Once something has answered it, `C-c C-k` only stops the turn, as before.
  - With background agents running, `C-c C-k` only stops the turn, after asking about the agents.
  - Some prompts cannot be rolled back, such as the first prompt of a conversation. The turn is stopped anyway, the prompt stays, and the echo area says why.
- **`C-c C-k` pressed again, with no turn running, rolls back too.**
  - When nothing has answered the latest prompt in the feed yet, that prompt is rolled back at once, with no question, and its words land in the input window.
    - A prompt counts as answered once a response bubble or a purple-titled card has landed after the prompt.
  - When something has answered it, a selection list opens instead, in your completion framework.
    - It holds every prompt a rollback can reach, newest at the top.
    - Each line shows how long ago the prompt was sent and how full the context was just before it.
    - Picking one rolls back to just before it, after the usual confirmation, keeping files.
  - With no prompt to roll back, it says there is nothing to interrupt.
  - While your workspace's sync merge is queued, it asks about the merge instead (see the merge bubble section).
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

## Unsent input across a restart

When Emacs quits, each workspace's input window is saved to that workspace's prompt history, the same way `C-c C-c` saves it.

- When Emacs starts again, each input window holds what it held at shutdown.
  - Nothing is sent: the words are only put back in the input window.
  - An input window that already holds text is left as it is.
  - An empty input window at shutdown saves nothing.
- Text you cleared with `C-c C-c` and never sent comes back the same way.
  - It stops coming back once you send it, or once you send any other prompt after it.
  - Only the most recent such text comes back.

## Spellchecking the input window

The input window underlines misspelled words as you type.

- Misspellings are checked against SCOWL's size-80 American English dictionary.
  - It is the largest size SCOWL recommends for spellchecking.
  - The installer downloads it into `~/.cache/agent-repl/dictionaries`, and `hunspell` checks against it.
- To fix a misspelling, put point on it and press `z=` in normal state.
  - `C-.` also corrects the word at point.
  - `]s` and `[s` jump to the next and the previous misspelling.
  - `C-,` jumps to the next misspelling too.
- A word you save from the `z=` menu goes in hunspell's personal dictionary, `~/.hunspell_en_US-scowl80`.
- Text that is not prose is never checked.
  - That covers fenced code blocks, inline `code` spans, paths, URLs, slash commands, identifiers and email addresses.
- Pasted text, recalled history and restored input are checked as soon as they arrive.
- Setting `agent-repl-input-spellcheck` to `nil` turns spellchecking off for input windows opened after the change.
- If `hunspell` or the dictionary is missing, the input window is not spellchecked.
  - The agent-repl log names what is missing, once.
  - `bash .claude/install.sh` installs both.

## A one-shot that fixes the current workspace

`SPC TAB C-o` makes a one-shot about the workspace you press it in.

- Describe what is wrong with this workspace, for example a bug in its feed.
  - The prompt names the workspace, so you can see which one the commission is about.
- The daemon opens the one-shot's commission with a note naming this workspace.
  - The note gives its name, worktree and branch.
  - It tells the agent that the issue you describe is this workspace's, not the one-shot's own.
- Otherwise it is an ordinary `SPC TAB o` one-shot.
  - It is cut from the primary branch and merges back into it.
  - It copies nothing from this workspace's conversation or worktree.
  - `SPC u SPC TAB C-o` asks for the model too.

## A one-shot in explanation-engine

`SPC TAB O` makes a one-shot in the explanation-engine repository, whichever workspace you press it in.

- The repository is always `$MULTI_REPO_ROOT/explanation-engine` (see Setup).
  - It must be on the roster. If it is not, the command says so. Add it with `SPC TAB C-n`.
- Otherwise it is an ordinary `SPC TAB o` one-shot.
  - `SPC u SPC TAB O` asks for the model too.
- What happens when the work is done is explanation-engine's own policy, in `.agent-repl/prompts/` at its main checkout.
  - The policy merges the one-shot through the merge queue, then runs `/create-or-update-pr --add-to-merge-queue` once the merge lands.
- Re-opening a closed workspace is `SPC TAB C-r`.

## One-shot commissions across a crash

The one-shot commission prompt (`SPC TAB o` and `SPC TAB C-o`) saves what you type to its history on every keystroke.

- The prompt always starts empty.
  - Unlike the other creation prompts, it never copies in the text of your current workspace's input window.

- If Emacs crashes while you type, press `SPC TAB o` again, then the up arrow or `M-p`.
  - The text you were typing is the first history entry, in full.
- The text also stays in the history when you press `C-g`.
- The history is shared by every workspace, in `~/.claude-emacs/oneshot-commission-history.el`.

## Creating a workspace leaves you where you are

No way of creating a workspace switches you to it.

- This covers `SPC TAB n`, `SPC TAB N`, the forks (`SPC TAB c`, `SPC TAB C`, `SPC TAB f`, `SPC TAB F`), `SPC TAB o` and `SPC o h`.
  - The new workspace's tab appears on the bar, and you switch to it when you want.
- Nothing agent-repl does on its own takes you out of a minibuffer prompt.
  - While you type, a workspace's panels being built, a switch made from the sidebar or by an agent, and the landing on a registered or reopened workspace all wait.
  - They happen as soon as the prompt closes.

## When creating a workspace fails

A workspace that fails to be created is reported in two places.

- Emacs says "workspace creation FAILED:" in the minibuffer, with the reason.
- The repository's main-worktree workspace is asked to find out why.
  - It is opened first if it was closed.
  - The request names what the create asked for and the failure, and asks the agent to analyze it without changing anything until you answer.
  - It waits behind a turn the main worktree is already running, and never interrupts it.
- This holds however the workspace was asked for: an Emacs key, an agent's `/create-or-update-workspace`, or a support workspace.
- Nothing is reported for a create handed to a new daemon during a restart, or for the hidden workspace an async merge runs in.
  - The merge queue reports that one's failure back to the workspace that asked for the merge.
- The request's wording is `prompts/workspace-create-failed.md`.

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

## Zooming the feed text

| Key (input window or page) | Effect |
|---|---|
| `C-+` or `C-=` | Make the feed text one small step larger. |
| `C--` | Make the feed text one small step smaller. |

- Holding a key down keeps stepping, for fine adjustment.
- Only the feed text changes; Emacs's own font is untouched.
- The size is one setting for every workspace's page.
  - It is saved on each step, so it holds after Emacs or the daemon restarts.
  - The size stops at a smallest and a largest step.

## The sidebar's sections

- **The sidebar lists workspaces in three sections, top to bottom.**
  - The open workspaces section, headed "OPEN", holds the grouping you picked (repositories, tasks or stages).
  - "HIDDEN" holds the workspaces you hid.
  - "RECENTLY MERGED" holds the merges of the last week.
- **`SPC j R` sends every hidden workspace one keep-alive.**
  - A hidden workspace's cache is otherwise left to lapse, and its session stops when it does.
  - The keep-alive moves each lapse later, so the "HIDDEN" countdowns grow back.
  - The echo area counts the keep-alives sent and names each workspace that got none, with why.
- **Each heading folds its section with the chevron beside it.**
  - Every workspace's page shows the same folds.
- **"OPEN" shows how many workspaces it lists, folded or not.**
  - The number counts every row, children of other workspaces included, and the rows of a folded repository too.
- **The time at the right of an open workspace's row is its running turn's duration.**
  - It reads the same number as the footer's turn clock.
  - A workspace with no turn running reads `0s`.
  - A hidden row counts from its hide, and a merged row from its merge.
- **Resting the pointer on a workspace's name for three seconds shows its branch panel.**
  - The panel lists the branch, the branch it was cut from, its commit count and its last prompt.
  - This works in all three sections; the status dot does not open it.

## Searching the sidebar

- **The magnifier left of "Workspaces" opens a search bar under the sidebar's header.**
  - The bar slides down over the top of the list rather than pushing it down.
- **Typing narrows every section to the workspaces whose names match.**
  - The open workspaces section (under every grouping), Hidden Workspaces and Recently Merged are all narrowed.
  - A space matches anything, so `fix side` finds `fix-the-sidebar`.
  - The words must appear in the order typed, and case does not matter.
  - A section left with no match is hidden until the search changes.
- **Pressing the magnifier again, or `Escape` in the bar, closes it and shows everything again.**

## Switching workspaces from the keyboard

- **`SPC p p` lists the workspaces you are working with now.**
  - Every open workspace, and every hidden one.
  - The 25 most recently merged.
  - The 25 you most recently closed or killed yourself; one closed by an agent, a merge or agent-repl is not listed.
- **`SPC p P` lists every workspace agent-repl knows, however old.**
- **Both list the most recently updated workspace first.**
  - A turn finishing, opening, closing or killing a workspace, or a change of its status, brings it to the top.
  - Only switching to a workspace does not move it.
- **Each line shows the workspace's status mark as the sidebar draws it.**
  - A closed or merged workspace has the grey dot.
  - A hidden workspace has a dotted ring, `◌`, in its status color.
- **After the name, three columns.**
  - Whether it is closed, hidden, merged or not open in this Emacs.
  - How long ago its last turn ended.
  - How many commits its branch has past where it was cut.
- **Picking a workspace that is not open here opens it first.**
  - A merged workspace opens too, to revise what it landed (see "Reopening a merged workspace").

## Reopening a merged workspace

- **A merged workspace can be opened again, from the sidebar or from Emacs.**
  - Clicking its row under Recently Merged offers two choices, and either one opens it and switches to it.
  - **Reopen** brings it back as it was, to ask it about what it landed or to revise it.
  - **Reopen and revert the merge** also hands its agent a brief to revert the merge and land the revert; follow the revert from the workspace's feed.
  - `SPC p p`, `SPC p P` and the row menu's Open reopen it without reverting.
- **Reverting asks the agent to undo exactly what the merge brought in.**
  - The brief names the branch, the branch it landed on, and the commit the branch held when it landed.
  - The agent reverts on the workspace's own branch and lands the revert through the merge queue, like any other work.
  - When the reopened conversation is large enough to stop at the cold-cache choice, the brief waits there; the choice you make decides it, as it does for any prompt waiting at that choice.
- **It comes back as an ordinary open workspace.**
  - Its worktree is checked out again from its branch, and its conversation comes back.
  - It leaves Recently Merged and returns to its groupings and the tab bar.
- **Its branch moves up to where its parent branch is now.**
  - The work it merged is already there, so nothing is lost.
  - It stays where it was if its branch has commits its parent lacks, or the worktree has uncommitted changes.
- **Merging it again carries only the new work.**

## Hidden workspaces

- **Hiding a workspace keeps its conversation's cache warm for a while instead of stopping it at once.**
  - Its keep-alives stop, so the cache runs out on its own clock.
  - Showing it again before then brings it back with the cache still warm.
- **The Hidden Workspaces section counts down to the moment the cache runs out.**
  - The time reads like `12m 34s`, and `cold cache` once it has run out.
  - Its color moves from green, an hour or more out, to deep red when it runs out.
  - A row with no countdown is one whose cache was not being kept when it was hidden; it counts up from the hide instead.
- **When the cache runs out, the workspace's session is stopped, as idle workspaces are.**
  - Its conversation is kept.
  - A workspace whose conversation is large enough to need the cold-cache choice when it resumes has its name struck through.
  - Showing it again asks that choice, as any cold resume does.
- **A hidden workspace you are looking at runs like a shown one.**
  - Selecting it from a picker keeps its session up and its cache warm while you are on it.
  - Moving off it leaves its cache to run out again.
- **Answering a hidden workspace's cold-cache choice shows it again.**
  - It moves out of Hidden Workspaces the moment you click, whichever choice you made.

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
- **A prompt that is waiting on the cold gate shows the badge "awaiting cold gate".**
  - This happens when you send to a workspace with no live session and it comes back parked at its cold gate.
  - The prompt never answers the gate for you, because answering it spends money: pay, clear or compact in the gate card.
  - Your answer decides what happens to the held prompts, and only one answer sends them:
    - "clear context" discards them, so clearing spends nothing at all.
    - "pay and resume" and "compact first" keep them, with the badge "held for you", until you press Send now or Cancel.
    - "compact and resume" compacts, then sends them once the session is back.
      - The gate shows this button only while prompts are held at it.
- **A compaction you chose at the gate says what it cost in the footer.**
  - Once the summary is written, the footer's activity line ends with the summarizing model and the tokens it spent, e.g. "· claude-sonnet-5 spent 104.3k tokens".
  - The line stays on the "compacted and resumed" outcome after the session is back.
  - The token figure is colored with the same scale as the gate's own token figure, so the two read alike.
  - Send now is refused while the gate stands, and Cancel still drops the prompt.
- **A held prompt reads "classifying" only while the classifier is actually deciding it.**
  - A prompt that nothing is classifying and nothing holds reads "queued", and it is delivered in order.
- **A held prompt folded into the one above it is seen joining it.**
  - It happens when you press its "fold above" button, or when the classifier rules it should interrupt a prompt that has not started yet.
  - The prompt above flashes once.
  - The folded prompt's card stays a moment under it, badged "folding above", then fades away.

## Restarting a stuck workspace

`SPC o C-c` restarts the current workspace's backend and page. It is the way to unstick one workspace without rebooting everything.

- **What it bounces:**
  - The workspace's shim, rebuilt first if its build is stale, relaunched with the same session resumed.
  - The workspace's webapp page, reloaded.
  - Never the daemon, the store or the sidecar.
- **It is immediate and takes no prefix argument.**
  - There is no graceful mode: the running turn and all detached work are hard-stopped, because a workspace needs this when something is stuck.
  - Prompts sent meanwhile are held "after reconnect" and delivered once the session is back.
- **It shows nothing when it works.**
  - The restart simply happens: the fresh session and the reloaded page are the whole account of it.
  - If the relaunch fails, Emacs says "the restart failed:" with the reason, and the sidebar's Restart entry shows it too.
- **A merge waiting for the workspace does not block it.**
  - The merge waits through the restart and starts once the fresh session is free.
- **A merge in the middle of a step finishes that step first.**
  - A test run, a repair turn or a rebase in progress is never cut off: the restart happens as soon as that step ends.
  - The merge then goes on from the next step, after the restart.
  - Until the step ends, the restart has not happened yet, and the input stays held.
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
- **A turn the vendor gave up on is retried for you:**
  - When the vendor retried a failed call itself and then gave up, agent-repl asks the agent to continue every 5 minutes, for up to 2 hours from the vendor's first retry.
  - When your session or weekly usage allowance is exhausted, it waits instead until that allowance resets, as the footer's usage figures report it, and then asks the agent to continue. If the vendor still refuses, it asks again every 5 minutes for 30 minutes.
  - Each retry appears in the feed as a turn the agent started on its own.
  - Sending a prompt of your own stops the retrying.

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
