<!-- used by: daemon internal/merge/deploy.go (run.runHook, every prompt hook of a deploy stage); placeholders: {{prompt}} -->
{{prompt}}

If this turn opens a pull request, store it as this workspace's owned link the moment its URL is known, so agent-repl follows it until it merges: `agent-repld workspace link here -owned -url <the pull request's URL> -label "<its title, two to eight words>"`. Take the URL from the creating command's output or from `gh pr view --json url`, never a guessed one.
