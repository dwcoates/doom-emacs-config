/**
 * THE ONE CODE BLOCK: every block of code the webapp draws, whether a
 * markdown fence, a fence inside a metaprompt tree, or a reply's quoted text,
 * is the `pre.md-code` drawn here, so the shape (and everything the
 * stylesheet hangs on it) cannot drift between them. A source scan
 * (test/code-block.test.ts) fails any other module that writes the shape
 * itself.
 *
 * EVERY CODE BLOCK CARRIES A COPY CONTROL (owner request, 2026-10-20): the one
 * copy control (copy-control.ts) of kind `code`, floated at the block's top
 * right, which copies the block's code (`codeOfCopyControl`).
 */
import { copyControlHtml, type CopiedText } from "./copy-control.js";
import { escapeHtml, highlightCode } from "./highlight.js";

/** The class a code block's copy control wears, which places it in the block. */
export const CODE_COPY_CLASS = "md-code-copy";

/** The copy control every code block carries. */
const COPY_CONTROL_HTML = copyControlHtml("code", CODE_COPY_CLASS, "Copy code");

/**
 * BODY as the one code block: escaped, highlighted when LANG is a known
 * language (hljs escapes its own output), inside `pre.md-code`, with the copy
 * control ahead of the code.
 */
export function codeBlockHtml(body: string, lang: string): string {
  const html = highlightCode(body, lang);
  const langClass = lang === "" ? "" : ` lang-${escapeHtml(lang)}`;
  return `<pre class="md-code">${COPY_CONTROL_HTML}<code class="hljs${langClass}">${html}</code></pre>`;
}

/**
 * The code CONTROL copies: the text of the code element of the block it sits
 * in, logged with the block's language ("" for none). A code copy control is
 * only ever drawn inside a code block ahead of its code (`codeBlockHtml`), so
 * a control outside one is a bug, and throws.
 */
export function codeOfCopyControl(control: Element): CopiedText {
  const code = control.closest("pre.md-code")?.querySelector(":scope > code");
  if (code === null || code === undefined) {
    throw new Error("a code block's copy control sits outside a code block's pre.md-code > code");
  }
  const langClass = [...code.classList].find((name) => name.startsWith("lang-"));
  return { text: code.textContent ?? "", context: { lang: langClass?.slice("lang-".length) ?? "" } };
}
