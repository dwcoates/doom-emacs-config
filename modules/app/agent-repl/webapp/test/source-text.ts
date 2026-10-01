/**
 * TEXT A TEST READS AS CODE, WITH THE PROSE TAKEN OUT.
 *
 * Source scans and stylesheet scans both read files whose comments are where
 * the reasoning lives, and a comment that names a selector or a call is prose,
 * not code. Every such scan strips comments through these two readers, so no
 * test can drift to a stripping rule of its own.
 */

/** TEXT with every block comment removed: the whole of a CSS file's comments. */
export function withoutBlockComments(text: string): string {
  return text.replace(/\/\*[\s\S]*?\*\//g, "");
}

/**
 * TypeScript SOURCE with its block comments and whole-line `//` comments
 * removed. A trailing `//` is kept, because the two characters also open every
 * URL inside a string literal on a line of code.
 */
export function codeOf(source: string): string {
  return withoutBlockComments(source).replace(/^\s*\/\/.*$/gm, "");
}
