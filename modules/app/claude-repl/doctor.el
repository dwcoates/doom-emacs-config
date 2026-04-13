;;; app/claude-repl/doctor.el -*- lexical-binding: t; -*-

(let* ((settings-file (expand-file-name "~/.claude/settings.json"))
       (json (when (file-exists-p settings-file)
               (condition-case nil
                   (json-read-file settings-file)
                 (error nil))))
       (hooks (cdr (assq 'hooks json))))
  (unless json
    (warn! "Cannot read ~/.claude/settings.json — hook checks skipped"))
  (when json
    (unless (cdr (assq 'Stop hooks))
      (warn! "No Stop hook configured in ~/.claude/settings.json — workspaces will get stuck in :thinking"))
    (unless (cdr (assq 'UserPromptSubmit hooks))
      (warn! "No UserPromptSubmit hook configured in ~/.claude/settings.json — :thinking state won't be set by hooks"))))
