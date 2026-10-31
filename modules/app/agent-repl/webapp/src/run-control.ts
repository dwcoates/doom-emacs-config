/**
 * THE RUN CONTROL (owner request, 2026-10-30): the small `$` icon a shell code
 * block carries left of its copy control, which runs the block's command in
 * the editor's terminal popup (the one `SPC o t` toggles), closing the popup
 * once the command exits. The click is routed by link.ts's
 * `installRunControlRouting` as `OpenInEditorRequest.terminal_command`, which
 * the daemon relays to Emacs verbatim (lisp/terminal.el runs it).
 *
 * Only a block whose fence names a shell (`SHELL_LANGS`) carries one: the
 * metaprompt holds every agent to writing a shell command as one such block,
 * a single command a terminal runs whole. Like the copy control, it is an
 * `<ar-button>` (control.ts), so a click on it never selects, expands or
 * folds the bubble it sits in, and its icon holds no text, so the block's
 * text is still its code alone.
 */

/** The class every run control wears. */
export const RUN_CONTROL_CLASS = "run-control";

/** The class a code block's run control wears, which places it in the block. */
export const CODE_RUN_CLASS = "md-code-run";

/** The fence languages that mark a block as one shell command. */
export const SHELL_LANGS: ReadonlySet<string> = new Set(["bash", "sh", "shell", "zsh"]);

/** Whether a block fenced as LANG carries a run control. */
export function isShellLang(lang: string): boolean {
  return SHELL_LANGS.has(lang.toLowerCase());
}

/** A dollar sign: the shell prompt's mark. */
const RUN_ICON =
  `<svg class="run-control-icon" viewBox="0 0 16 16" aria-hidden="true">` +
  `<path d="M11 4.6c-.6-1-1.7-1.6-3-1.6c-1.7 0-3 .9-3 2.3c0 3 6 1.7 6 4.9c0 1.4-1.3 2.4-3 2.4c-1.4 0-2.6-.6-3.2-1.7"/>` +
  `<path d="M8 1.5v13"/>` +
  `</svg>`;

/** The label a run control shows on hover and gives assistive technology. */
export const RUN_CONTROL_LABEL = "Run in terminal";

/** The run control every shell code block carries. */
export const RUN_CONTROL_HTML =
  `<ar-button class="${RUN_CONTROL_CLASS} ${CODE_RUN_CLASS}" ` +
  `title="${RUN_CONTROL_LABEL}" aria-label="${RUN_CONTROL_LABEL}">${RUN_ICON}</ar-button>`;
