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
 * right, which copies the block's code (`codeOfBlockControl`).
 *
 * A SHELL BLOCK ALSO CARRIES A RUN CONTROL (owner request, 2026-10-30): the
 * one run control (run-control.ts), floated right after the copy control so
 * it sits left of it, which runs the block's code in the editor's terminal.
 * ONLY A RESPONSE'S SHELL BLOCK CARRIES ONE (owner request, 2026-11-10): the
 * drawer asks for it (`CodeBlockOptions.runControl`), and only a response
 * bubble's prose slot does (bubble/body.ts `offerShellRuns`), so a prompt's
 * shell block, and every other drawn block, carries none.
 */
import { copyControlHtml, type CopiedText } from "./copy-control.js";
import { escapeHtml, highlightCode } from "./highlight.js";
import { isShellLang, RUN_CONTROL_HTML } from "./run-control.js";

/** The class a code block's copy control wears, which places it in the block. */
export const CODE_COPY_CLASS = "md-code-copy";

/** The copy control every code block carries. */
const COPY_CONTROL_HTML = copyControlHtml("code", CODE_COPY_CLASS, "Copy code");

/** What a code block carries beside its copy control. */
export interface CodeBlockOptions {
  /** Whether a shell block carries a run control: false unless asked for. */
  readonly runControl?: boolean;
}

/**
 * BODY as the one code block: escaped, highlighted when LANG is a known
 * language (hljs escapes its own output), inside `pre.md-code`, with the copy
 * control, and a shell block's run control when OPTIONS ask for one, ahead of
 * the code.
 */
export function codeBlockHtml(body: string, lang: string, options: CodeBlockOptions = {}): string {
  const html = highlightCode(body, lang);
  const langClass = lang === "" ? "" : ` lang-${escapeHtml(lang)}`;
  const run = options.runControl === true && isShellLang(lang) ? RUN_CONTROL_HTML : "";
  return `<pre class="md-code">${COPY_CONTROL_HTML}${run}<code class="hljs${langClass}">${html}</code></pre>`;
}

/**
 * The code a block's CONTROL acts on (its copy control copies it, its run
 * control runs it): the text of the code element of the block it sits in,
 * with the block's language ("" for none). A code block's control is only
 * ever drawn inside a code block ahead of its code (`codeBlockHtml`), so a
 * control outside one is a bug, and throws.
 */
export function codeOfBlockControl(control: Element): CopiedText {
  const code = control.closest("pre.md-code")?.querySelector(":scope > code");
  if (code === null || code === undefined) {
    throw new Error("a code block's control sits outside a code block's pre.md-code > code");
  }
  const langClass = [...code.classList].find((name) => name.startsWith("lang-"));
  return { text: code.textContent ?? "", context: { lang: langClass?.slice("lang-".length) ?? "" } };
}
