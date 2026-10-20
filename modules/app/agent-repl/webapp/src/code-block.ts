/**
 * THE ONE CODE BLOCK: every block of code the webapp draws, whether a
 * markdown fence, a fence inside a metaprompt tree, or a reply's quoted text,
 * is the `pre.md-code` drawn here, so the shape (and everything the
 * stylesheet hangs on it) cannot drift between them. A source scan
 * (test/code-block.test.ts) fails any other module that writes the shape
 * itself.
 */
import { escapeHtml, highlightCode } from "./highlight.js";

/**
 * BODY as the one code block: escaped, highlighted when LANG is a known
 * language (hljs escapes its own output), inside `pre.md-code`.
 */
export function codeBlockHtml(body: string, lang: string): string {
  const html = highlightCode(body, lang);
  const langClass = lang === "" ? "" : ` lang-${escapeHtml(lang)}`;
  return `<pre class="md-code"><code class="hljs${langClass}">${html}</code></pre>`;
}
