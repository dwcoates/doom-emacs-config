;;; ui/treemacs/config.el -*- lexical-binding: t; -*-

(after! treemacs
  (setq treemacs-width 60  ; Increase this value to make it wider
        treemacs-position 'left
        treemacs-collapse-dirs 3
        treemacs-deferred-git-apply-delay 0.5
        treemacs-display-in-side-window t
        ;; agent-repl gives every workspace its own perspective, and the
        ;; treemacs-per-perspective scope persists a project pointing at that
        ;; workspace's git worktree.  Merging/closing a workspace removes the
        ;; worktree but leaves the persisted treemacs project behind, so on the
        ;; next perspective switch treemacs restores an unreadable project and
        ;; (with the default `ask') pops a blocking "Project <name> at <path>
        ;; cannot be read." minibuffer prompt on every switch.  `remove' prunes
        ;; the dead project silently instead of prompting, self-healing the
        ;; accumulated cruft from ephemeral worktrees.
        treemacs-missing-project-action 'remove)

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)

  (map! :leader
        (:prefix ("t" . "treemacs")
         :desc "Toggle treemacs" "t" #'treemacs
         :desc "Select treemacs window" "1" #'treemacs-select-window)))
