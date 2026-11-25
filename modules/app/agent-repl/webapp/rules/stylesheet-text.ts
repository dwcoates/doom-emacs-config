/**
 * THE STYLESHEET READ AS TEXT, for the rules that hold `src/styles.css` to its
 * shape: the parsers of `test/css-text.ts`, handed the real stylesheet by
 * default, and a selector found by pattern. A behavior test that wants the
 * cascade installs the real stylesheet through `test/stylesheet.ts` instead;
 * reading the stylesheet's text is a rule, so these readers live in `rules/`.
 */
import stylesheet from "../src/styles.css?raw";
import { withoutBlockComments } from "../test/source-text.js";
import { keyframesOf as keyframesIn, mediaBlocksOf as mediaBlocksIn, type MediaBlock } from "../test/css-text.js";

export { isOwnRule, rulesOf, type CssRule, type MediaBlock } from "../test/css-text.js";

/** `test/css-text.ts`'s keyframesOf, over the real stylesheet by default. */
export function keyframesOf(name: string, css: string = stylesheet): string {
  return keyframesIn(name, css);
}

/** `test/css-text.ts`'s mediaBlocksOf, over the real stylesheet by default. */
export function mediaBlocksOf(query: string, css: string = stylesheet): MediaBlock[] {
  return mediaBlocksIn(query, css);
}

/**
 * The selector PATTERN's first group captures from the real stylesheet,
 * comments stripped, or a loud failure naming WHAT the test looked for.
 */
export function selectorOf(pattern: RegExp, what: string): string {
  const found = pattern.exec(withoutBlockComments(stylesheet))?.[1]?.trim();
  if (found === undefined) throw new Error(`the stylesheet has no ${what}`);
  return found;
}
