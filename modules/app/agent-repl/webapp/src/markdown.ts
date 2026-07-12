/**
 * Minimal markdown → HTML renderer for TextStream blocks. Escape-first:
 * every character of source text is HTML-escaped before any markup is
 * emitted, so model output can never inject markup. Supports the common
 * subset: headings, fenced code, inline code, bold, italic, links
 * (http/https only), unordered + ordered lists, blockquotes, horizontal
 * rules, paragraphs.
 *
 * Fenced code blocks with a KNOWN language tag are syntax-highlighted
 * via the shared highlight.js helper (highlight.ts); hljs emits its
 * own escaped HTML, so the escape-first guarantee holds. Unknown or
 * absent language tags fall back to plain escaped text — no
 * auto-detection, so rendering stays deterministic and cheap under
 * per-delta re-renders.
 *
 * Streaming-safe: unterminated constructs (open fence, half-typed
 * emphasis) degrade to literal text or a still-open code block rather
 * than erroring, so it can re-render on every text-delta.
 */

import { escapeHtml, highlightCode } from "./highlight.js";
import { isMetapromptTree, renderTreeHtml } from "./metaprompt-tree.js";

/** Inline markup within one already-escaped line. Exported for the
 * metaprompt-tree renderer, which injects it into content spans. */
export function inline(escaped: string): string {
  // Lift code spans out first so emphasis/link rules cannot touch their
  // contents; NUL sentinels cannot occur in escaped text.
  const codeSpans: string[] = [];
  let out = escaped.replace(/`([^`]+)`/g, (_m, code: string) => {
    codeSpans.push(code);
    return `\u0000${codeSpans.length - 1}\u0000`;
  });
  // Links: [text](http…) — scheme-restricted to http/https.
  out = out.replace(
    /\[([^\]]+)\]\((https?:\/\/[^)\s]+)\)/g,
    (_m, text: string, url: string) =>
      `<a href="${url}" target="_blank" rel="noopener noreferrer">${text}</a>`,
  );
  // Bold before italic so ** is not consumed as two *.
  out = out.replace(/\*\*([^*]+)\*\*/g, "<strong>$1</strong>");
  out = out.replace(/\*([^*]+)\*/g, "<em>$1</em>");
  return out.replace(/\u0000(\d+)\u0000/g, (_m, i: string) => `<code>${codeSpans[Number(i)]}</code>`);
}

interface ListState {
  ordered: boolean;
  items: string[];
}

export function renderMarkdown(src: string): string {
  const lines = src.split("\n");
  const out: string[] = [];
  let paragraph: string[] = [];
  let list: ListState | null = null;
  let quote: string[] = [];
  let fence: { lang: string; lines: string[] } | null = null;

  const flushParagraph = (): void => {
    if (paragraph.length > 0) {
      out.push(`<p>${paragraph.map((l) => inline(escapeHtml(l))).join("<br>")}</p>`);
      paragraph = [];
    }
  };
  const flushList = (): void => {
    if (list !== null) {
      const tag = list.ordered ? "ol" : "ul";
      out.push(`<${tag}>${list.items.map((i) => `<li>${i}</li>`).join("")}</${tag}>`);
      list = null;
    }
  };
  const flushQuote = (): void => {
    if (quote.length > 0) {
      out.push(`<blockquote>${quote.map((l) => inline(escapeHtml(l))).join("<br>")}</blockquote>`);
      quote = [];
    }
  };
  const flushAll = (): void => {
    flushParagraph();
    flushList();
    flushQuote();
  };
  const flushFence = (): void => {
    if (fence !== null) {
      const body = fence.lines.join("\n");
      // A plain fence carrying a metaprompt TLDR tree renders as
      // hanging-indent tree lines instead of a wrapping code block.
      if (fence.lang === "" && isMetapromptTree(body)) {
        out.push(`<div class="mp-tree">${renderTreeHtml(body, inline)}</div>`);
        fence = null;
        return;
      }
      // highlightCode escapes in both branches (hljs escapes its own
      // output), so the escape-first guarantee holds.
      const html = highlightCode(body, fence.lang);
      const langClass = fence.lang === "" ? "" : ` lang-${escapeHtml(fence.lang)}`;
      out.push(`<pre class="md-code"><code class="hljs${langClass}">${html}</code></pre>`);
      fence = null;
    }
  };

  for (const line of lines) {
    if (fence !== null) {
      if (/^```\s*$/.test(line)) {
        flushFence();
      } else {
        fence.lines.push(line);
      }
      continue;
    }
    const fenceOpen = line.match(/^```(\w*)\s*$/);
    if (fenceOpen) {
      flushAll();
      fence = { lang: fenceOpen[1], lines: [] };
      continue;
    }
    const heading = line.match(/^(#{1,6})\s+(.*)$/);
    if (heading) {
      flushAll();
      const level = heading[1].length;
      out.push(`<h${level}>${inline(escapeHtml(heading[2]))}</h${level}>`);
      continue;
    }
    if (/^(---+|\*\*\*+)\s*$/.test(line)) {
      flushAll();
      out.push("<hr>");
      continue;
    }
    const quoted = line.match(/^>\s?(.*)$/);
    if (quoted) {
      flushParagraph();
      flushList();
      quote.push(quoted[1]);
      continue;
    }
    const bullet = line.match(/^\s*[-*]\s+(.*)$/);
    const numbered = line.match(/^\s*\d+\.\s+(.*)$/);
    if (bullet || numbered) {
      flushParagraph();
      flushQuote();
      const ordered = !bullet;
      const item = inline(escapeHtml((bullet ?? numbered)![1]));
      if (list === null || list.ordered !== ordered) {
        flushList();
        list = { ordered, items: [] };
      }
      list.items.push(item);
      continue;
    }
    if (line.trim() === "") {
      flushAll();
      continue;
    }
    flushList();
    flushQuote();
    paragraph.push(line);
  }
  // Streaming tail: an unterminated fence still renders as a code block.
  flushFence();
  flushAll();
  return out.join("");
}
