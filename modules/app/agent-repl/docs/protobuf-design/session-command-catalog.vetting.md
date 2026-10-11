# Session command catalog — vetting register

## 1. `supportedCommands()` lists every user-added skill and command

- ASSUMPTION: the Agent SDK's `query.supportedCommands()`, with the shim's
  `settingSources: ["user", "project", "local"]`, returns user skills, project
  skills, plugin skills, custom commands and MCP prompts, each with its
  frontmatter description.
- AFFECTED: `conversation.v1.SessionCommandCatalog.commands` coverage; nothing in
  the shape changes if false, but the completion would miss commands.
- HOW TO VERIFY: in a live workspace with a known user skill
  (`~/.claude/skills/<name>/SKILL.md`), call `ListSlashCommands` (or read the
  shim's `shim.session.command_catalog` log record) and check the skill is
  present with its description.
- REVERSE PASS: compare the list against the CLI's own `/` menu in a terminal
  session with the same settings; anything in the CLI menu and not in the
  catalog is a silent omission.
- STATUS: OPEN.
