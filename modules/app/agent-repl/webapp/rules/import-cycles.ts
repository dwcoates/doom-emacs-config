/**
 * THE WEBAPP'S SOURCE IMPORT GRAPH, as the import-cycle check reads it
 * (test/import-cycles.test.ts): every module under a directory and the
 * modules under it that it imports.
 *
 * IT READS EVERY STATIC IMPORT, TYPE-ONLY ONES INCLUDED. A type-only import is
 * erased from the built code, so it cannot itself read a binding too early;
 * but an import that names only types today is one value away from a runtime
 * edge, and a graph that is free of cycles with its type imports counted stays
 * free of them whatever a later change imports through those edges. A dynamic
 * `import()` loads lazily, after every module it could reach has evaluated, so
 * it is not an edge, as the build's layering does not count it either
 * (app-layers.ts).
 */
import { existsSync, readFileSync } from "node:fs";
import { join, relative, resolve, dirname } from "node:path";
import ts from "typescript";
import type { ImportingModule } from "../app-layers.js";
import { filesUnder } from "./files-under.js";

/** The source suffixes a module of the graph has. */
const SOURCE_SUFFIXES = [".ts", ".tsx"];

/**
 * Every module under ROOT (its `.ts` and `.tsx` files, declaration files
 * aside) with the modules under ROOT it statically imports or re-exports
 * from, each id its path relative to ROOT. A specifier that names no module
 * under ROOT (a package, a stylesheet, a generated file outside it) is no
 * edge of the graph.
 */
export function sourceImportGraph(root: string): ImportingModule[] {
  const files = filesUnder(root, SOURCE_SUFFIXES).filter((file) => !file.endsWith(".d.ts"));
  return files.map((file) => ({
    id: relative(root, file),
    imports: staticSpecifiers(file)
      .map((specifier) => resolveModule(root, file, specifier))
      .filter((id): id is string => id !== undefined),
  }));
}

/** The module specifiers FILE's top-level import and export-from declarations name. */
function staticSpecifiers(file: string): string[] {
  const source = ts.createSourceFile(file, readFileSync(file, "utf8"), ts.ScriptTarget.Latest, false);
  return source.statements.flatMap((statement) => {
    if (ts.isImportDeclaration(statement) || ts.isExportDeclaration(statement)) {
      const specifier = statement.moduleSpecifier;
      return specifier !== undefined && ts.isStringLiteral(specifier) ? [specifier.text] : [];
    }
    if (ts.isImportEqualsDeclaration(statement) && ts.isExternalModuleReference(statement.moduleReference)) {
      const specifier = statement.moduleReference.expression;
      return ts.isStringLiteral(specifier) ? [specifier.text] : [];
    }
    return [];
  });
}

/**
 * The id under ROOT of the module FROM's SPECIFIER names, resolved as the
 * bundler resolves a relative specifier (`./x.js` is `./x.ts` or `./x.tsx`, a
 * directory its `index`), or undefined: a bare specifier, a query (`?raw`),
 * or a file that is not a module under ROOT.
 */
function resolveModule(root: string, from: string, specifier: string): string | undefined {
  if (!specifier.startsWith(".") || specifier.includes("?")) return undefined;
  const base = resolve(dirname(from), specifier);
  const stem = base.replace(/\.(js|jsx|ts|tsx)$/, "");
  const candidates = [
    ...SOURCE_SUFFIXES.map((suffix) => stem + suffix),
    ...SOURCE_SUFFIXES.map((suffix) => join(base, `index${suffix}`)),
  ];
  const found = candidates.find((candidate) => existsSync(candidate));
  if (found === undefined) return undefined;
  const id = relative(root, found);
  return id.startsWith("..") ? undefined : id;
}
