/**
 * tool-head — THE ONE TOOL CARD HEAD: the head row every `.tool-card` draws,
 * and the name the head leads with.
 *
 * A Bash call, a skill, a hook, a subagent bubble's head line, a question and a
 * consent card all head the same way (owner ruling, 2026-10-11: an ask card
 * looks like a tool call), so the head and its name are built here and nowhere
 * else. A card adds its own classes beside the shared one; the head's layout
 * and the name's weight and color are the stylesheet's `.tool-head` /
 * `.tool-name` rules.
 */

/** The class every tool card head wears. */
export const TOOL_HEAD_CLASS = "tool-head";

/** The class every tool card name wears. */
export const TOOL_NAME_CLASS = "tool-name";

/** A head row, wearing EXTRA classes beside the shared one. */
export function drawToolHead(...extra: string[]): HTMLElement {
  const head = document.createElement("div");
  head.classList.add(TOOL_HEAD_CLASS, ...extra);
  return head;
}

/** The head's name, verbatim, wearing EXTRA classes beside the shared one. */
export function drawToolName(text: string, ...extra: string[]): HTMLElement {
  const name = document.createElement("span");
  name.classList.add(TOOL_NAME_CLASS, ...extra);
  name.textContent = text;
  return name;
}
