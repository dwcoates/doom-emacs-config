<!-- used by: daemon internal/addsupport/addsupport.go (Prompt); placeholders: {{command}}, {{config_root}} -->
The agent-repl GUI has no support for the `/{{command}}` slash command.

Running it in this non-interactive session answers only `/{{command}} isn't available in this environment.`, because the Claude Code CLI implements it as an interactive terminal panel and the GUI's session is headless. The feature is therefore unreachable from the GUI today.

Your job: investigate adding RICH GRAPHICAL support for `/{{command}}` inside the agent-repl GUI, so the GUI stops depending on a terminal panel it can never open.

Where the underlying data lives, in order of preference:
1. The Claude Code CLI itself. Work out what `/{{command}}` would render, then find whether any NON-interactive surface already exposes the same data (another subcommand, a flag, a machine-readable output mode, or an SDK/stream event the shim already receives). Prefer a supported surface over anything else.
2. The session's Claude config directory, at {{config_root}}. Inspect the files there BY HAND to find the state the command reports. Read them directly rather than assuming a schema.

Deliverable: the feature rendered richly in the agent-repl webapp, following the module's existing patterns rather than inventing new ones. Study how a comparable feature already flows end to end (shim event, daemon frame, webapp store, webapp render) and mirror it.

Constraints:
- Investigate FIRST and report what you found before building, since the right surface is the whole question here.
- If no supported surface exists and the config files are the only source, say so explicitly and explain what a config-file read would have to assume.
- Follow the repo's testing requirements in CLAUDE.md: one test file per source module, one edge case per test, and every test run and passing before you commit.
