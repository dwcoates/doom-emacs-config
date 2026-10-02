/**
 * href — what kind of destination a link's href names, decided in ONE place so
 * the markdown renderer (which decides what becomes an anchor) and the prose
 * click router (which decides what a click does) cannot disagree.
 *
 * THREE KINDS. A WEB href (`http(s)://`) opens externally. A FILE-LINK href is
 * a path or a bare file name, optionally carrying a `:<line>` suffix; the
 * daemon resolves it (`OpenInEditorFeedLink`). Everything else (`javascript:`,
 * `mailto:`, `data:`, a fragment, a protocol-relative `//host`) is neither and
 * is never made an anchor.
 */

/** `http://` or `https://`: the one scheme that opens in the browser. */
const WEB_HREF = /^https?:\/\//i;

/** Any `scheme:` prefix, as RFC 3986 spells one. */
const SCHEME = /^[a-z][a-z0-9+.-]*:/i;

/** A trailing `:<line>`, which a bare `name:12` would otherwise read as a scheme. */
const LINE_SUFFIX = /:\d+$/;

/** Whether HREF is a web URL. */
export function isWebHref(href: string): boolean {
  return WEB_HREF.test(href.trim());
}

/** Whether HREF is a path or bare file name the daemon can resolve. */
export function isFileLinkHref(href: string): boolean {
  const trimmed = href.trim();
  if (trimmed === "" || trimmed.startsWith("#") || trimmed.startsWith("//")) return false;
  return !SCHEME.test(trimmed.replace(LINE_SUFFIX, ""));
}

/**
 * THE EXTENSIONS A SCHEME-LESS NAME IS READ AS A FILE BY (owner request,
 * 2026-10-02). Markdown's fuzzy linkifier takes a bare `README.md` for a
 * hostname (`.md` is Moldova's TLD) and turns it into a web link; a bare name
 * ending in one of these is a source or doc file, and its anchor opens through
 * the daemon (`OpenInEditorFeedLink`) instead. Real domains (`example.com`,
 * `github.io`) end in none of them and stay web links.
 */
export const FILE_LINK_EXTENSIONS: readonly string[] = [
  "md", "ts", "tsx", "js", "mjs", "go", "el", "py", "sh", "json", "yaml", "yml",
  "toml", "proto", "txt", "css", "html", "rs", "c", "h", "m", "swift", "org",
];

/** Whether LABEL, a scheme-less autolinked name, ends in a file extension. */
export function hasFileExtension(label: string): boolean {
  const dot = label.lastIndexOf(".");
  if (dot <= 0) return false;
  return FILE_LINK_EXTENSIONS.includes(label.slice(dot + 1).toLowerCase());
}
