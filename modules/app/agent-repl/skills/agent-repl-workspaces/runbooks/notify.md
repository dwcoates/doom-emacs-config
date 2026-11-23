# Notifying through a workspace

- `workspace notify WS TEXT` posts a desktop notification attributed to WS
  (`here` or any other open workspace): titled by WS's name, TEXT below it,
  and clicking it brings Emacs forward on WS. It follows the rule every
  banner does: while Emacs is focused none is posted, and the answer says so
  (`editorFocused`).
