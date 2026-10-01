/**
 * THE REAL STYLESHEET, INSTALLED INTO A TEST'S DOCUMENT.
 *
 * WHY A TEST WOULD WANT IT. Almost every drawing test here asserts CLASSES,
 * which is the right unit: the class is what the drawing code decides. But a
 * class only means something if the stylesheet lets it win, and the cascade is
 * decided by the WHOLE file rather than by the rule an author was looking at.
 * A screenshot of the real topbar caught exactly that gap — the context figure
 * carried `.tone-yellow` and was painted grey, because a later single-class
 * rule set `color` on the same element and the earlier tone rule lost. The
 * class assertion passed the whole time.
 *
 * WHAT IT CAN AND CANNOT SAY. jsdom resolves the cascade — selector matching,
 * specificity and source order — so "which declaration wins" is answerable
 * here. It does NOT resolve custom properties, so the winning value comes back
 * as the literal `var(--async)` rather than a color. That is enough, and it is
 * also the honest assertion: the token is the vocabulary the app is written in,
 * and pinning the resolved hex would pin the theme instead of the register.
 */
import stylesheet from "../src/styles.css?raw";
import { withoutBlockComments } from "./source-text.js";

/**
 * Install `src/styles.css` into the current document, and answer a teardown
 * that removes it. Call it inside the test that needs it, never globally: a
 * suite that does not ask about paint should not pay to parse the file.
 */
export function installStylesheet(): () => void {
  const style = document.createElement("style");
  style.textContent = stylesheet;
  document.head.append(style);
  return () => style.remove();
}

/**
 * The declaration the cascade actually hands `property` on `el`, with the real
 * stylesheet installed. Custom properties are returned unresolved, by design.
 */
export function cascadedValue(el: Element, property: string): string {
  return window.getComputedStyle(el).getPropertyValue(property).trim();
}

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
 * The selector PATTERN's first group captures from the real stylesheet,
 * comments stripped, or a loud failure naming WHAT the test looked for.
 */
export function selectorOf(pattern: RegExp, what: string): string {
  const found = pattern.exec(withoutBlockComments(stylesheet))?.[1]?.trim();
  if (found === undefined) throw new Error(`the stylesheet has no ${what}`);
  return found;
}
