/**
 * WHERE A TEST FILE READS THE WEBAPP'S OWN SOURCE FROM DISK.
 *
 * A test that reads the webapp's source (`src/`, its stylesheets, the page
 * shell, the app's own `.ts`, the test code itself) to hold it to a structural
 * rule is a RULE test, and rule tests live in `rules/` as the `webapp-rules`
 * suite, which every branch runs before its merge is admitted to the queue.
 * `sourceReads` finds such a read in one file's text, so the rule in
 * `source-reads.test.ts` can fail a source-reading test anywhere under
 * `test/`.
 *
 * WHAT COUNTS AS A READ:
 * - a call of a file-system reader (`readFileSync`, `readFile`, `readdirSync`,
 *   `readdir`, `globSync`, `opendir(Sync)`, `createReadStream`) whose
 *   statement, once the file's own `const` names in it are spelled out, names
 *   a source: a `src` path segment, a `.ts`/`.tsx`/`.css`/`.html`/`.mjs`
 *   file, or the test's own file or directory (`import.meta.url`,
 *   `__dirname`, `process.cwd()` and kin). A statement that names a fixture
 *   (a `.json`, `.svg` or log file, the vocabulary, the glyph pictures, a
 *   temporary directory, a build's output) is data, not source;
 * - `import.meta.glob(...)`, which only ever loads modules;
 * - an import of a `.ts`/`.tsx`/`.css`/`.html`/`.mjs` file as `?raw` text;
 * - an import of a helper in `rules/`, each of which reads source.
 */

/** One source read found in a file: its kind and the text that made it one. */
export interface SourceRead {
  kind: "reader call" | "module glob" | "raw import" | "rules helper";
  text: string;
}

/** A file-system reader's call, its name captured. */
const READER_CALL = /\b(readFileSync|readFile|readdirSync|readdir|globSync|opendirSync|opendir|createReadStream)\s*\(/g;

/** A read of the webapp's source: a `src` segment, a source file's suffix, or the test's own location. */
const SOURCE_MARK =
  /(^|[/"'`])src([/"'`]|$)|\.(?:ts|tsx|css|html|mjs)\b|import\.meta\.(?:url|dirname|filename)|__dirname|__filename|process\.cwd\(\)/;

/** A read of data rather than source: a fixture, the vocabulary, the glyph pictures, a temporary or build directory. */
const DATA_MARK = /\.(?:json|jsonl|svg|png|log|txt)\b|(^|[/"'`])(?:vocab|images|fixtures|testdata)([/"'`]|$)|\b(?:tmpdir|mkdtempSync|outDir)\b/;

/** An import of a source file as raw text, static or dynamic. */
const RAW_IMPORT = /(?:\bfrom\s*|\bimport\s*\(\s*)["'`]([^"'`]+\.(?:ts|tsx|css|html|mjs)\?raw)["'`]/g;

/** An import of a module under `rules/`, from anywhere outside it. */
const RULES_IMPORT = /(?:\bfrom\s*|\bimport\s*\(\s*)["'`]((?:\.\.\/)+rules\/[^"'`]+)["'`]/g;

/** `const NAME = EXPR` (or `let`), EXPR up to the statement's end. */
const DECLARATION = /\b(?:const|let)\s+([A-Za-z_$][\w$]*)(?:\s*:[^=;\n]+)?\s*=\s*([^;\n]+)/g;

/** How many times a name inside a resolved argument is spelled out in turn. */
const RESOLVE_DEPTH = 4;

/** Every source read TEXT (one file's whole text) makes, in source order of kind. */
export function sourceReads(text: string): SourceRead[] {
  const reads: SourceRead[] = [];
  const names = declarations(text);
  for (const call of text.matchAll(READER_CALL)) {
    const start = call.index ?? 0;
    const open = start + call[0].length - 1;
    const argument = balanced(text, open);
    const resolved = resolveNames(statementAround(text, start, open + argument.length + 2), names);
    if (SOURCE_MARK.test(resolved) && !DATA_MARK.test(resolved)) {
      reads.push({ kind: "reader call", text: `${call[1]}(${argument})` });
    }
  }
  for (const glob of text.matchAll(/\bimport\.meta\.glob\s*\(/g)) {
    reads.push({ kind: "module glob", text: `import.meta.glob(${balanced(text, (glob.index ?? 0) + glob[0].length - 1)})` });
  }
  for (const raw of text.matchAll(RAW_IMPORT)) reads.push({ kind: "raw import", text: raw[1] });
  for (const helper of text.matchAll(RULES_IMPORT)) reads.push({ kind: "rules helper", text: helper[1] });
  return reads;
}

/**
 * The statement of TEXT around the span START..END: back to the last `;`, `{`
 * or `}` before it and on to the first `;` after it. A reader's path is often
 * built beside its call rather than inside it (a list of paths mapped through
 * it, a directory filtered to `.ts` files after it), so the statement is what
 * says what the call reads.
 */
function statementAround(text: string, start: number, end: number): string {
  const before = Math.max(text.lastIndexOf(";", start), text.lastIndexOf("{", start), text.lastIndexOf("}", start));
  const after = text.indexOf(";", end);
  return text.slice(before + 1, after === -1 ? text.length : after);
}

/** Every `const`/`let` NAME's expression in TEXT, the first declaration of a name winning. */
function declarations(text: string): ReadonlyMap<string, string> {
  const names = new Map<string, string>();
  for (const [, name, expression] of text.matchAll(DECLARATION)) {
    if (!names.has(name)) names.set(name, expression);
  }
  return names;
}

/** ARGUMENT with each name NAMES declares spelled out beside it, RESOLVE_DEPTH times over. */
function resolveNames(argument: string, names: ReadonlyMap<string, string>): string {
  let resolved = argument;
  for (let depth = 0; depth < RESOLVE_DEPTH; depth += 1) {
    const spelled = resolved.replace(/[A-Za-z_$][\w$]*/g, (name) => {
      const expression = names.get(name);
      return expression === undefined ? name : `${name}(${expression})`;
    });
    if (spelled === resolved) break;
    resolved = spelled;
  }
  return resolved;
}

/** The text between TEXT's parenthesis at OPEN and the one that closes it (to the end, when none does). */
function balanced(text: string, open: number): string {
  let depth = 0;
  for (let cursor = open; cursor < text.length; cursor += 1) {
    if (text[cursor] === "(") depth += 1;
    if (text[cursor] === ")") depth -= 1;
    if (depth === 0) return text.slice(open + 1, cursor);
  }
  return text.slice(open + 1);
}
