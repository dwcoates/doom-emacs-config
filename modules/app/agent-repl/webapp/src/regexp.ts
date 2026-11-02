/**
 * regexp — text matched LITERALLY inside a regular expression. A name or a
 * search word the page did not write (a workspace's name, a typed query) is
 * escaped here before it joins a pattern, so a `.` or a `(` in it matches
 * itself and never changes what the pattern means.
 */

/** TEXT with every character a regular expression gives a meaning escaped. */
export function escapeRegExp(text: string): string {
  return text.replace(/[.*+?^${}()|[\]\\]/g, "\\$&");
}
