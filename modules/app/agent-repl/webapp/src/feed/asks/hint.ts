/**
 * hint — WHERE TO TYPE: the one line the standing permission card draws under
 * its controls to say that a deny's reason is typed in the Emacs composer and
 * sent, never on the card. (The question card draws none: owner ruling,
 * 2026-10-11.)
 */

/** The attribute the hint is found by. */
export const ASK_HINT_ATTRIBUTE = "data-ask-hint";

/** The hint line, saying TEXT verbatim. */
export function drawAskHint(text: string): HTMLElement {
  const hint = document.createElement("div");
  hint.className = "ask-hint";
  hint.setAttribute(ASK_HINT_ATTRIBUTE, "");
  hint.textContent = text;
  return hint;
}
