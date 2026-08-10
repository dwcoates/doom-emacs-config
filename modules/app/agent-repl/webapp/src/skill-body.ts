/**
 * skill-body — THE rendering of a skill's own SKILL.md, for both places that
 * carry one.
 *
 * A skill's contents arrive at this frontend twice over, from two different
 * shapes of the same daemon fact: on the `skillBody` conversation arm, for an
 * invocation that opened no bubble, and on `AsyncSkillBubble.body`, for one
 * that did. What the reader sees must not depend on which of those delivered
 * it, so the markup is written once here and imported by both.
 *
 * IT RENDERS AS MARKDOWN because a skill IS a markdown document — the same
 * renderer that draws assistant prose and markdown Read previews.
 *
 * IT WEARS THE CAPPED-SECTION CLASSES (`skill-content`, a `CAPPED_CLASSES`
 * entry in expand.ts) so the body folds by default and expands on a click,
 * through the one capped-section mechanic every long card section uses.
 */

import { renderMarkdown } from "./markdown.js";

/**
 * A skill's SKILL.md as its capped, click-expandable section.
 *
 * EMPTY IS NOTHING. An unresolved body renders no section at all rather than
 * an empty box, because "the contents have not arrived" and "the skill file is
 * blank" are not worth distinguishing to a reader and a blank box reads as a
 * broken one.
 */
export function SkillBodySection(body: string): string {
  if (body === "") return "";
  return `<div class="tool-output skill-content skill-content-md">${renderMarkdown(body)}</div>`;
}
