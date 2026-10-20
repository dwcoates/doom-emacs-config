<!-- used by: daemon/internal/workspace (Create, a conversation forked from a workspace other than the one whose branch this worktree was cut from); placeholders: {{origin}}, {{source}} -->
This workspace is a fork of the workspace `{{origin}}`. The conversation
above was copied from `{{origin}}`, but this worktree was cut fresh from
{{source}}, so none of `{{origin}}`'s uncommitted or unmerged
changes are present here.

Don't mention this unless a later request from the user relies on changes
from `{{origin}}` that aren't in this worktree. If that happens, tell the
user those changes live in `{{origin}}` and weren't carried over by the
fork.
