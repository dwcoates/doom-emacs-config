/**
 * TEXT A TEST READS AS CODE, WITH THE PROSE TAKEN OUT.
 *
 * Source scans and stylesheet scans both read files whose comments are where
 * the reasoning lives, and a comment that names a selector or a call is prose,
 * not code. Every such scan strips comments through these two readers, so no
 * test can drift to a stripping rule of its own, and cuts a function out of
 * the code through `functionBody`, so none can drift to a rule of its own for
 * where a function ends.
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

/**
 * The text of the function NAME declared in CODE, from its `function NAME(`
 * to its closing brace at the start of a line. A CODE that declares no such
 * function fails the test that asked, naming it.
 */
export function functionBody(code: string, name: string): string {
  const start = code.indexOf(`function ${name}(`);
  if (start < 0) throw new Error(`the code declares no function ${name}`);
  const end = code.indexOf("\n}\n", start);
  if (end < 0) throw new Error(`the function ${name} has no closing brace at the start of a line`);
  return code.slice(start, end);
}
