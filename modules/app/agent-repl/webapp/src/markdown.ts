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
 * cheap under per-delta re-renders. A plain (language-less) fence whose
 * body reads as a metaprompt TLDR tree renders as hanging-indent tree
 * lines instead of a code block — but ONLY for a caller that hands in a
 * measured column budget (`TreeCols`). A caller with no measured width gets
 * the fence as an ordinary code block, which wraps on its own; there is no
 * default tree width.
 *
 * Streaming-safe: markdown-it parses partial input on every call without
 * throwing (an unterminated fence renders as a still-open code block, a
 * half-typed construct degrades to literal text), so it can re-render on
 * every text-delta.
 */

import MarkdownIt from "markdown-it";
import taskLists from "markdown-it-task-lists";
import { escapeHtml, highlightCode } from "./highlight.js";
import { FILE_LINK_EXTENSIONS, hasFileExtension, isFileLinkHref, isWebHref } from "./href.js";
import { isMetapromptTree, renderTreeHtml } from "./metaprompt-tree.js";

/**
 * The column budget a metaprompt tree wraps to, asked for only when a tree is
 * actually drawn: measuring needs layout, and prose with no tree must not pay
 * for (or fail on) a measurement it never uses.
 */
export type TreeCols = () => number;

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
  out = out.replace(
    /\[([^\]]+)\]\(([^)\s]+)\)/g,
    (whole: string, text: string, url: string) =>
      isWebHref(url) || isFileLinkHref(url)
        ? `<a href="${url}" target="_blank" rel="noopener noreferrer">${text}</a>`
        : whole,
  );
  // Bold before italic so ** is not consumed as two *.
  out = out.replace(/\*\*([^*]+)\*\*/g, "<strong>$1</strong>");
  out = out.replace(/\*([^*]+)\*/g, "<em>$1</em>");
  // The NUL sentinel is the point: it is the one character that cannot survive HTML-escaping, so
  // a code span lifted out under it cannot be forged by the text being rendered. It is written as
  // an escape here rather than the literal byte it used to be, which was invisible in an editor.
  // eslint-disable-next-line no-control-regex -- see above
  return out.replace(/\u0000(\d+)\u0000/g, (_m, i: string) => `<code>${codeSpans[Number(i)]}</code>`);
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

/** Whether a fence is a metaprompt tree: language-less, tree-shaped. */
function isTreeFence(lang: string, body: string): boolean {
  return lang === "" && isMetapromptTree(body);
}

// Fenced code: keep the md-code + hljs shape, and divert a language-less
// metaprompt tree to the hanging-indent tree renderer when the caller measured
// a width for it. highlightCode escapes in both branches (hljs escapes its own
// output), so the escape-first guarantee holds.
md.renderer.rules.fence = (tokens, idx, _options, env): string => {
  const { lang, body } = fenceParts(tokens[idx]);
  // The wrap width rides the render env so a fenced tree re-flows to the same
  // live width as a bare one (see renderMarkdown's treeCols).
  const treeCols = (env as { treeCols?: TreeCols } | undefined)?.treeCols;
  if (treeCols !== undefined && isTreeFence(lang, body)) {
    return `<div class="mp-tree">${renderTreeHtml(body, inline, treeCols())}</div>`;
  }
  const html = highlightCode(body, lang);
  const langClass = lang === "" ? "" : ` lang-${escapeHtml(lang)}`;
  return `<pre class="md-code"><code class="hljs${langClass}">${html}</code></pre>`;
};

/**
 * Render markdown to HTML. TREECOLS, when given, is the measured column budget a
 * fenced metaprompt tree wraps to, threaded to the fence rule through the render
 * env; without it a fenced tree is drawn as the ordinary code block it is.
 */
export function renderMarkdown(src: string, treeCols?: TreeCols): string {
  return md.render(src, { treeCols });
}

/** Whether SRC holds a fenced metaprompt tree, which only a measured width can draw. */
export function hasFencedTree(src: string): boolean {
  return md.parse(src, {}).some((token) => {
    if (token.type !== "fence") return false;
    const { lang, body } = fenceParts(token);
    return isTreeFence(lang, body);
  });
}
