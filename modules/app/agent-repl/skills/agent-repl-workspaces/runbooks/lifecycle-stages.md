# Lifecycle: creation, finish, kinds and built-in defaults

- **Creation.** Prompt hooks' text is appended to the initial prompt, after
  the commission and before a one-shot's completion directive, costing no
  turn. A workspace created with no initial prompt runs none of them. Script
  hooks run once the worktree is registered, before the session's first
  prompt.
  - A failed creation script, or a creation prompt naming `{{slack_thread}}`
    for a workspace with no Slack origin, FAILS THE CREATE: it is undone and
    reported to the repository's main workspace (`creation_hook_failed`).
  - An async merge's hidden merge workspace runs no creation stage.
- **Finish.** Nothing is drawn as a bubble meanwhile. Prompt hooks are turns
  of the workspace, drawn in its own feed; scripts keep their log among the
  queue's logs. A failed hook withdraws the deploy before it is ever in line
  and hands the failure back to the workspace. It is durable across restarts.
  - An async merge has no finish stage.
  - A user-requested finish of a workspace whose kind has finish hooks waits
    for any turn in flight, then runs them.
- **TWO KINDS OF WORKSPACE, decided only by who created it**: `emoji_ack`
  (created by the Slack emoji-ack reaction system) and `user_or_workspace`
  (created by the user or another workspace). The creator states it with
  `create -originator`; the daemon never infers it from `-slack-thread`.
  - `creation`, `finish` and `post_deploy` each hold `emoji_ack: {hooks:
    [...]}` and/or `user_or_workspace: {hooks: [...]}`. `pre_deploy` holds
    the same two lists beside its `local_merge`. `deploy` holds `emoji_ack:`
    and/or `user_or_workspace:`, each ONE hook entry (`hook:` + `runs_for:`).
  - A workspace runs only its own kind's list.
  - A kind's list LEFT UNSET runs the stage's BUILT-IN DEFAULT for that kind;
    SET, even `hooks: []`, it replaces the default.
  - YAML anchors (`&x` / `*x`) let both kinds share one entry.
- **Built-in defaults exist only for `emoji_ack`:**
  - `creation`: a prompt asking the agent to post a brief reply in the
    originating thread, in its own words, that work started (the
    send-to-slack skill).
  - `finish`: a prompt asking the agent to post to the thread that its pull
    request was created, with a hyperlink, then store that post's permalink
    as an owned link announcing the pull request: `agent-repld workspace
    link here -url <permalink> -label <two to eight words> -owned -announces
    <PR url>`. It posts nothing if no pull request was opened.
  - `post_deploy`: the script `bin/hooks/slack-merged-reaction.sh` in the
    daemon's agent-repl checkout. It adds `:merged:` to every Slack message
    whose stored owned link announces a pull request GitHub reports merged,
    found through the workspace's links alone, never by searching the
    thread.
  - `builtin: {}` stands for the stage's default in an `emoji_ack` list of
    creation, finish or post_deploy, so a repository can keep the default
    beside its own hooks. It is refused anywhere else, and in a request's own
    `-pre-deploy-*` or `-post-deploy-*` hooks.
