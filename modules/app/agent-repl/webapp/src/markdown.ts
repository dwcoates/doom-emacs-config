/**
 * Markdown → HTML renderer for TextStream blocks, backed by markdown-it
 * with full GFM (tables, task lists, strikethrough, autolinked bare
 * URLs) on top of the CommonMark core.
 *
 * Safety (escape-first equivalent): the instance runs with `html: false`,
 * so raw HTML in model output is never passed through — markdown-it
 * escapes it and only ever emits the tags its own rules produce. That is
 * why the rendered string is injected into innerHTML with no separate
 * sanitizer. Link targets are further restricted to http/https via
 * `validateLink`, and every emitted link opens in a new tab with
 * `rel="noopener noreferrer"`.
 *
 * Fenced code blocks with a KNOWN language tag are syntax-highlighted via
 * the shared highlight.js helper (highlight.ts); hljs emits its own
 * escaped HTML. Unknown or absent language tags fall back to plain
 * escaped text — no auto-detection, so rendering stays deterministic and
 * cheap under per-delta re-renders. A metaprompt TLDR tree never reaches
 * this renderer: the daemon parses every drawn text and serves a tree, bare
 * or inside a fence, as a tree block of its own (prose.proto), which
 * metaprompt-tree.ts draws.
 *
 * TeX IS TYPESET (owner request, 2026-10-25): `$…$` and `\(…\)` inline,
 * `$$…$$`, `\[…\]` and a ```math fence as a display block, each through
 * KaTeX (markdown-it-texmath), drawn in the text's own color on no ground
 * (styles.css, "TeX"). A `$` beside a digit is money, never math (`$5 and
 * $10`). KaTeX's output is its own markup from the parsed TeX, never the
 * model's HTML (`trust` stays off), so the escape-first guarantee holds; a
 * formula KaTeX cannot parse draws its source, marked as KaTeX marks an
 * error, rather than failing the whole render.
 *
 * Streaming-safe: markdown-it parses partial input on every call without
 * throwing (an unterminated fence renders as a still-open code block, a
 * half-typed construct degrades to literal text), so it can re-render on
 * every text-delta.
 */

import katex from "katex";
import MarkdownIt from "markdown-it";
import taskLists from "markdown-it-task-lists";
import texmath from "markdown-it-texmath";
import { codeBlockHtml } from "./code-block.js";
import { escapeHtml } from "./highlight.js";
import {
  codeSpanFileHref,
  FILE_LINK_EXTENSIONS,
  fileLinkLabel,
  hasFileExtension,
  isFileLinkHref,
  isPathLinkText,
  isWebHref,
} from "./href.js";

/** Inline markup within one already-escaped line. Exported for the
 * metaprompt-tree renderer (and the question picker), which inject it
 * into content spans of text that is already HTML-escaped. Kept as a
 * standalone pass — independent of the markdown-it block pipeline —
 * because those callers hand it pre-escaped fragments, not documents. */
export function inline(escaped: string): string {
  // Lift code spans out first so emphasis/link rules cannot touch their
  // contents; NUL sentinels cannot occur in escaped text.
  const codeSpans: string[] = [];
  let out = escaped.replace(/`([^`]+)`/g, (_m, code: string) => {
    codeSpans.push(code);
    return `\u0000${codeSpans.length - 1}\u0000`;
  });
  // Links: [text](url) — a web URL or a file link (path or bare file name).
  // A code span in the link text is restored here as plain code: it is
  // already inside an anchor, so it must not become one of its own below.
  out = out.replace(/\[([^\]]+)\]\(([^)\s]+)\)/g, (whole: string, text: string, url: string) => {
    if (isWebHref(url)) return anchorHtml(url, null, restoreCodeSpans(text, codeSpans, false));
    if (!isFileLinkHref(url)) return whole;
    // eslint-disable-next-line no-control-regex -- the code-span sentinel, as below
    const span = /^\u0000(\d+)\u0000$/.exec(text);
    const shown = span === null ? text : codeSpans[Number(span[1])];
    if (!isPathLinkText(shown)) return anchorHtml(url, null, restoreCodeSpans(text, codeSpans, false));
    const label = fileLinkLabel(shown);
    return anchorHtml(url, shown, span === null ? label : `<code>${label}</code>`);
  });
  out = autolinkWebUrls(out);
  // Bold before italic so ** is not consumed as two *.
  out = out.replace(/\*\*([^*]+)\*\*/g, "<strong>$1</strong>");
  out = out.replace(/\*([^*]+)\*/g, "<em>$1</em>");
  // The NUL sentinel is the point: it is the one character that cannot survive HTML-escaping, so
  // a code span lifted out under it cannot be forged by the text being rendered. It is written as
  // an escape here rather than the literal byte it used to be, which was invisible in an editor.
  return restoreCodeSpans(out, codeSpans, true);
}

/**
 * Draw TEXT into EL as inline markdown: escaped first, then given `inline`'s
 * markup. The one way a short served string (a title, a question, an option
 * label) is drawn as a response bubble draws its inline markup.
 */
export function setInlineMarkdown(el: HTMLElement, text: string): void {
  el.innerHTML = inline(escapeHtml(text));
}

/**
 * Put the code spans `inline` lifted out back into TEXT. With LINK, a span
 * that is wholly a `~/` path (`codeSpanFileHref`) is wrapped in a file-link
 * anchor, as the markdown-it pipeline's `code-span-file-link` rule does.
 */
function restoreCodeSpans(text: string, codeSpans: readonly string[], link: boolean): string {
  // eslint-disable-next-line no-control-regex -- see `inline`
  return text.replace(/\u0000(\d+)\u0000/g, (_m, i: string) => {
    const code = codeSpans[Number(i)];
    const html = `<code>${code}</code>`;
    const href = link ? codeSpanFileHref(code) : null;
    return href === null ? html : anchorHtml(href, href, `<code>${fileLinkLabel(href)}</code>`);
  });
}

/** The four entities `escapeHtml` writes, each standing for one character. */
const ESCAPED_CHAR = /&(?:amp|lt|gt|quot);/y;

/**
 * A BARE WEB URL IN `inline`'S TEXT IS A LINK, as the markdown-it pipeline's
 * `linkify` makes it one in a block (owner request, 2026-10-26). A metaprompt
 * tree's bullets are drawn by `inline`, so a URL the model wrote bare in a
 * tree was plain text. Matching is the pipeline's own linkifier (`md.linkify`)
 * run over the UNESCAPED text, so `&quot;` around a URL never joins it; only a
 * match written with an `http(s)://` scheme links, never a fuzzy bare host or
 * file name. ESCAPED is `inline`'s working string: the anchors it already drew
 * are skipped, and every character outside a link keeps its escaped form.
 */
function autolinkWebUrls(escaped: string): string {
  return escaped
    .split(/(<a [^>]*>[\s\S]*?<\/a>)/)
    .map((part, i) => (i % 2 === 1 ? part : autolinkEscapedRun(part)))
    .join("");
}

/** `autolinkWebUrls` for one RUN of escaped text holding no anchor. */
function autolinkEscapedRun(run: string): string {
  // starts[k] is where plain character k begins in RUN; one past the end closes it.
  let plain = "";
  const starts: number[] = [];
  for (let at = 0; at < run.length; ) {
    starts.push(at);
    ESCAPED_CHAR.lastIndex = at;
    const entity = ESCAPED_CHAR.exec(run);
    plain += entity === null ? run[at] : unescapeEntity(entity[0]);
    at += entity === null ? 1 : entity[0].length;
  }
  starts.push(run.length);
  const matches = (md.linkify.match(plain) ?? []).filter((m) => isWebHref(m.raw));
  let out = "";
  let from = 0;
  for (const m of matches) {
    const [begin, end] = [starts[m.index], starts[m.lastIndex]];
    out += run.slice(from, begin) + anchorHtml(escapeHtml(m.url), null, run.slice(begin, end));
    from = end;
  }
  return out + run.slice(from);
}

/** The character one of `ESCAPED_CHAR`'s entities stands for. */
function unescapeEntity(entity: string): string {
  const char = UNESCAPED[entity];
  if (char === undefined) throw new Error(`unescapeEntity: ${entity} is not one of escapeHtml's entities`);
  return char;
}

const UNESCAPED: Readonly<Record<string, string>> = { "&amp;": "&", "&lt;": "<", "&gt;": ">", "&quot;": '"' };

/**
 * The anchor `inline` draws, attributes in the order the markdown-it pipeline
 * renders them so both draw the same link. TITLE, when set, is the full path a
 * file link's shortened text stands for (`file-link-label`), shown on hover.
 */
function anchorHtml(href: string, title: string | null, body: string): string {
  const titleAttr = title === null ? "" : ` title="${title}"`;
  return `<a href="${href}"${titleAttr} target="_blank" rel="noopener noreferrer">${body}</a>`;
}

// One shared instance. `html: false` is the security linchpin (see file
// header); `linkify` autolinks bare URLs; `breaks` renders a single
// newline as <br>, matching how model prose expects soft breaks to show.
const md = new MarkdownIt({ html: false, linkify: true, breaks: true });

// Restrict link targets to a web URL or a file link (a path or bare file
// name, which the daemon resolves on click) — stricter than markdown-it's
// default allow-list. A rejected URL renders as literal text with no anchor.
md.validateLink = (url: string): boolean => isWebHref(url) || isFileLinkHref(url);

// AN IMAGE IS NEVER A FILE LINK: validateLink gates images too, and a relative
// `<img src>` would make the webview request a path from the page's own origin.
// An image whose source is not a web URL draws its alt text instead.
const defaultImage =
  md.renderer.rules.image ??
  ((tokens, idx, options, _env, self) => self.renderToken(tokens, idx, options));
md.renderer.rules.image = (tokens, idx, options, env, self) => {
  const src = tokens[idx].attrGet("src") ?? "";
  if (!isWebHref(src)) return escapeHtml(tokens[idx].content);
  return defaultImage(tokens, idx, options, env, self);
};

md.use(taskLists);

/** KaTeX's options for every formula: a malformed one draws, never throws. */
const KATEX_OPTIONS = { throwOnError: false } as const;

// texmath writes into the options it is handed, so it is handed a copy.
md.use(texmath, { engine: katex, delimiters: ["dollars", "brackets"], katexOptions: { ...KATEX_OPTIONS } });

// A BARE FILE NAME IS A FILE, NOT A HOST. The fuzzy linkifier must first SEE
// `foo.ts` (most extensions are no TLD), then the rule below turns every
// scheme-less autolink ending in a file extension into a file link: its href
// is the bare label, which the click router sends to the daemon.
//
// WHICH EXTENSIONS ARE ALSO WEB DOMAINS is the linkifier's own knowledge, asked
// BEFORE the file extensions are added to it: if it already links `a.<ext>`,
// `<ext>` is a real domain ending and a bare name carrying it is ambiguous.
const DOMAIN_EXTENSIONS: ReadonlySet<string> = new Set(
  FILE_LINK_EXTENSIONS.filter((ext) => md.linkify.test(`a.${ext}`)),
);
md.linkify.tlds([...FILE_LINK_EXTENSIONS], true);
md.core.ruler.after("linkify", "file-linkify", (state) => {
  for (const block of state.tokens) {
    const children = block.children ?? [];
    children.forEach((token, index) => {
      if (token.type !== "link_open" || token.markup !== "linkify") return;
      const label = children[index + 1]?.content ?? "";
      if (/^[a-z][a-z0-9+.-]*:/i.test(label) || label.includes("@") || label.includes("/")) return;
      if (!hasFileExtension(label)) return;
      token.attrSet("href", label);
      // An ambiguous name (`wikipedia.org`, `README.md`) falls back to the web
      // when no file resolves; the click router reads this mark.
      if (DOMAIN_EXTENSIONS.has(label.slice(label.lastIndexOf(".") + 1).toLowerCase())) {
        token.attrSet("data-web-fallback", "");
      }
    });
  }
  return true;
});

// A CODE SPAN THAT IS WHOLLY A `~/` PATH IS A FILE LINK (`codeSpanFileHref`):
// the span keeps its code styling inside an anchor whose href is the path. A
// span already inside a link stays plain code, since anchors cannot nest.
md.core.ruler.after("file-linkify", "code-span-file-link", (state) => {
  for (const block of state.tokens) {
    if (!block.children) continue;
    let linkDepth = 0;
    block.children = block.children.flatMap((token) => {
      if (token.type === "link_open") linkDepth++;
      if (token.type === "link_close") linkDepth--;
      if (token.type !== "code_inline" || linkDepth > 0) return [token];
      const href = codeSpanFileHref(token.content);
      if (href === null) return [token];
      const open = new state.Token("link_open", "a", 1);
      open.attrSet("href", href);
      return [open, token, new state.Token("link_close", "a", -1)];
    });
  }
  return true;
});

// A FILE LINK DRAWS ITS FILE NAME, NOT ITS PATH (owner request, 2026-10-15).
// A file link whose whole text is one path (`isPathLinkText`), as plain text or
// as a code span, draws `fileLinkLabel` of it (`b.ts:12`) and carries the full
// path as its hover title. The href is untouched, so a click opens the path;
// the browser wraps the drawn name alone, and a tree measures it alone.
md.core.ruler.after("code-span-file-link", "file-link-label", (state) => {
  for (const block of state.tokens) {
    const children = block.children ?? [];
    children.forEach((token, index) => {
      if (token.type !== "link_open") return;
      const href = token.attrGet("href") ?? "";
      if (isWebHref(href) || !isFileLinkHref(href)) return;
      const text = children[index + 1];
      if (children[index + 2]?.type !== "link_close") return;
      if (text.type !== "text" && text.type !== "code_inline") return;
      if (!isPathLinkText(text.content)) return;
      token.attrSet("title", text.content);
      text.content = fileLinkLabel(text.content);
    });
  }
  return true;
});

/** The class of the bold words that open a list item (`- **Term**: …`). */
export const LEAD_CLASS = "md-lead";

// BOLD WORDS THAT OPEN A LIST ITEM ARE ITS LEAD: a `strong` that is the very
// first thing in the item's first paragraph carries LEAD_CLASS, so a page
// can draw an item's lead apart from bold words later in its text, which CSS
// alone cannot tell apart (`:first-child` passes over text before it).
md.core.ruler.after("file-link-label", "list-item-lead", (state) => {
  state.tokens.forEach((token, index) => {
    if (token.type !== "list_item_open") return;
    const first = state.tokens[index + 1]?.type === "paragraph_open" ? state.tokens[index + 2] : undefined;
    if (first?.type !== "inline") return;
    // markdown-it leaves an empty text token before a leading `**`.
    const opener = first.children?.find((child) => child.type !== "text" || child.content !== "");
    if (opener?.type === "strong_open") opener.attrJoin("class", LEAD_CLASS);
  });
  return true;
});

// Every emitted link opens in a new tab, safely.
const defaultLinkOpen =
  md.renderer.rules.link_open ??
  ((tokens, idx, options, _env, self) => self.renderToken(tokens, idx, options));
md.renderer.rules.link_open = (tokens, idx, options, env, self) => {
  tokens[idx].attrSet("target", "_blank");
  tokens[idx].attrSet("rel", "noopener noreferrer");
  return defaultLinkOpen(tokens, idx, options, env, self);
};

/** A fence token's language tag and its body, as the fence rule reads them. */
function fenceParts(token: { info: string; content: string }): { lang: string; body: string } {
  return { lang: token.info.trim().split(/\s+/)[0] ?? "", body: token.content.replace(/\n$/, "") };
}

// Fenced code: the md-code + hljs shape. highlightCode escapes in both
// branches (hljs escapes its own output), so the escape-first guarantee holds.
// A ```math fence is a display formula, drawn as `$$…$$` is.
md.renderer.rules.fence = (tokens, idx): string => {
  const { lang, body } = fenceParts(tokens[idx]);
  if (lang === "math") return `<section><eqn>${texmath.render(body, true, { ...KATEX_OPTIONS })}</eqn></section>`;
  return codeBlockHtml(body, lang);
};

/** Render markdown to HTML. */
export function renderMarkdown(src: string): string {
  return md.render(src);
}
