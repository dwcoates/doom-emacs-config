# Skill names link to their skill's file

The feed draws an inline code span that names one of the workspace's skills
(`ceac-resource-usage`) as a link reading `/ceac-resource-usage`, which opens
that skill's `SKILL.md` in the editor: the copy the workspace's own session
resolves the name to. The daemon draws the link, so it must know each skill's
file, and the session's command catalog carried names only.

## Assumptions (autonomous design)

- Objective: a skill named in a response or a prompt opens the file the
  session would actually run.
- No frontend contract change: the link is ordinary drawn markdown whose href
  is a file-link href, which the webapp already opens.
- Systems: the shim (states the files), the daemon (draws the links), the
  webapp (draws and opens them; no contract change).
- No non-additive change.

## Landed changes

### `conversation.v1.SessionCommandCatalogEntry.skill_file`

- What: an optional absolute path of the `SKILL.md` the runtime resolved the
  command to.
- Why: the daemon must link a skill name to the file the session runs, and the
  only authority for which source wins (project, account, plugin) is the
  runtime itself. A daemon-side directory scan would be a second, drifting
  lookup.
- Source: the vendor's `getContextUsage()` lists every loaded skill as
  `skills.skillFrontmatter[]` with `name` and `path`. The shim joins those
  paths onto the `supportedCommands()` rows by name, and restates the catalog
  whenever either reading changes.
- Evidence (observed, claude-agent-sdk 0.3.311, 2026-11-24): a probe session
  listed a project skill as `source: projectSettings` with the path of the
  worktree's `.claude/skills/<name>/SKILL.md`, and plugin skills as
  `gns-cowork:gns-bootstrap` with their plugin cache path. Both names are
  exactly the names `supportedCommands()` lists. Built-in skills carry no
  path. The vendor strips `path` only on remote and bridged connections, which
  agent-repl's local shim never uses.
- Consequences: the catalog's skill files arrive with the context reading,
  which can land after the catalog's first statement, so a catalog is first
  stated without files and restated with them. The daemon redraws its rows'
  skill links when a catalog is stated.
- Why not on `SessionContextSkillFrontmatter`: the file is a fact of the
  command a name resolves to, which is what the catalog answers; the context
  reading is a token breakdown. Putting it there would make the daemon join two
  readings that the shim already holds together.

Sweep: nothing obviated.
