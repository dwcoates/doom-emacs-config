<!-- used by: daemon/internal/slackwatch (reactions.go, the pull-request directive of a fork whose repository's lifecycle hooks tell the thread); placeholders: {{link}} -->
2. Store every pull request you create as an owned link right away: `agent-repld workspace link here -url <pull request url> -label <title> -owned`.
3. Post nothing to the originating thread ({{link}}) yourself: this workspace's repository's lifecycle hooks tell the thread that work started and where its pull request is. Any other Slack-mutating command still needs the user's explicit approval.
