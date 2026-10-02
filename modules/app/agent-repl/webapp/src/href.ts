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
