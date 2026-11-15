<!-- used by: daemon/internal/prompthandler (handler.openWithOwedRename: opens the next prompt a workspace's main agent receives after the workspace was renamed, since a session is told its workspace's name only when it starts); placeholders: {{name}} -->
This workspace was renamed since your session started: its name is now `{{name}}`. Wherever your instructions name this workspace by its old name, read `{{name}}` instead, and use `{{name}}` when you name it to the user, to other workspaces, or to the agent API.

The message this note opens follows. Treat the rename as background, not as a request.
