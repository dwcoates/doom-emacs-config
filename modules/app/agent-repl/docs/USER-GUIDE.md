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
