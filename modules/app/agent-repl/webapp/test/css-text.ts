/**
 * A STYLESHEET READ AS TEXT: its rules, a selector's own rule, a `@keyframes`
 * block, a `@media` block, each parsed out of the CSS it is handed. It reads
 * nothing itself, so a behavior test may parse CSS the app generates (the
 * vocabulary's token sheet); reading the webapp's own stylesheet to hold it
 * to a rule is `rules/stylesheet-text.ts`, which hands these the real one.
 */
import { withoutBlockComments } from "./source-text.js";

/** One `selector-list { declarations }` block of a stylesheet. */
export interface CssRule {
  selectors: string[];
  declarations: string;
}

/**
 * Every `selector { declarations }` pair in CSS, in source order, comments
 * stripped. A comment that mentions a selector is prose, never part of the
 * selector of the rule that follows it, so a scan that kept comments would
 * credit a rule with every class its preceding comment happens to name.
 */
export function rulesOf(css: string): CssRule[] {
  const text = withoutBlockComments(css);
  const rules: CssRule[] = [];
  const pattern = /([^{}]+)\{([^{}]*)\}/g;
  let match = pattern.exec(text);
  while (match !== null) {
    rules.push({
      selectors: (match[1] ?? "").split(",").map((one) => one.trim()).filter((one) => one !== ""),
      declarations: match[2] ?? "",
    });
    match = pattern.exec(text);
  }
  return rules;
}

/**
 * Whether RULE is SELECTOR's OWN rule: its selector list is SELECTOR alone, so
 * what it declares reaches nothing else. Every scan that asks for a selector's
 * own rule asks through here.
 */
export function isOwnRule(rule: CssRule, selector: string): boolean {
  return rule.selectors.length === 1 && rule.selectors[0] === selector;
}

/**
 * The body of the `@keyframes NAME` block in CSS: everything between its opening brace and the closing brace on its
 * own line, so a block of any number of stops comes back whole. Fails loudly
 * when the stylesheet has no such block.
 */
export function keyframesOf(name: string, css: string): string {
  const opener = `@keyframes ${name} {`;
  const start = css.indexOf(opener);
  if (start === -1) throw new Error(`the stylesheet has no @keyframes ${name}`);
  const bodyStart = start + opener.length;
  const end = css.indexOf("\n}", bodyStart);
  if (end === -1) throw new Error(`the stylesheet's @keyframes ${name} never closes`);
  return css.slice(bodyStart, end);
}

/** One `@media` block of a stylesheet, as `mediaBlocksOf` finds it. */
export interface MediaBlock {
  /** Where the block's `@media` starts. */
  start: number;
  /** Just past the block's closing brace. */
  end: number;
  /** Everything between the block's braces. */
  body: string;
}

/**
 * Every `@media (QUERY)` block in CSS, in source order, each matched brace by brace from its opening `{`: `rulesOf`'s
 * flat scan cannot tell a rule inside a media block from one outside it, and
 * the same selector often stands on both sides. Fails loudly on a block that
 * never closes.
 */
export function mediaBlocksOf(query: string, css: string): MediaBlock[] {
  const head = `@media (${query})`;
  const blocks: MediaBlock[] = [];
  for (let start = css.indexOf(head); start !== -1; start = css.indexOf(head, start + head.length)) {
    const open = css.indexOf("{", start);
    let depth = 0;
    let cursor = open;
    for (; cursor !== -1 && cursor < css.length; cursor += 1) {
      if (css[cursor] === "{") depth += 1;
      if (css[cursor] === "}") depth -= 1;
      if (depth === 0) break;
    }
    if (open === -1 || depth !== 0) throw new Error(`the stylesheet's ${head} block never closes`);
    blocks.push({ start, end: cursor + 1, body: css.slice(open + 1, cursor) });
  }
  return blocks;
}
