/**
 * The shim's static scan of its log sites (logging-contract.md "Log sites"):
 * every log call in `src/`, read from source, with the scope its logger was
 * built with and the key the runtime derives for it, and every fault that
 * would make two calls one site or leave a call unkeyable.
 *
 * A call's scope is its logger's: the `bindLog("scope", ...)` its receiver is
 * bound to in the call's file (directly, through `.with(...)`, or imported
 * from the file that built it). A call through a logger the scan cannot place
 * (a parameter, a context's field) could be in any scope, so its template
 * must be unique in the whole shim. A `LOGGER.message("template")` is a site
 * of that logger's scope, whichever logger later writes it (`ShimLogger.log`).
 */
import ts from "typescript";
import { parseTemplate, placeholders, siteKey } from "../../../logging/ts/logsite.js";

/** The runtime every shim key is derived under (src/log.ts `SHIM_LOG_RUNTIME`). */
const RUNTIME = "shim";
/** The scope src/log.ts writes its own records under. */
const LOGGING_SCOPE = "shim.logging";
const METHODS = new Set(["debug", "info", "warn", "error", "logVerbose"]);

/** One source file the scan reads: its path (as faults name it) and text. */
export interface ScanSource {
  readonly file: string;
  readonly text: string;
}

/** One log call the scan found. */
export interface ScannedSite {
  /** "file:line" of the call. */
  readonly at: string;
  /** The logger's scope, or null when the scan cannot place the logger. */
  readonly scope: string | null;
  readonly template: string;
  /** The runtime's key for the call; null when its scope is unplaced. */
  readonly key: string | null;
}

/** What the scan read: every site, every logger scope, and every fault. */
export interface ScanResult {
  readonly sites: readonly ScannedSite[];
  readonly scopes: readonly { readonly at: string; readonly scope: string }[];
  readonly faults: readonly string[];
}

interface FileLoggers {
  /** Identifier bound in the file to a logger, and its scope. */
  readonly bound: Map<string, string>;
}

function lineOf(source: ts.SourceFile, node: ts.Node): number {
  return source.getLineAndCharacterOfPosition(node.getStart(source)).line + 1;
}

/** The literal templates EXPR can be: a literal, or a conditional of literals; null otherwise. */
function literalTemplates(expr: ts.Expression): string[] | null {
  if (ts.isStringLiteral(expr) || ts.isNoSubstitutionTemplateLiteral(expr)) return [expr.text];
  if (ts.isParenthesizedExpression(expr)) return literalTemplates(expr.expression);
  if (ts.isConditionalExpression(expr)) {
    const whenTrue = literalTemplates(expr.whenTrue);
    const whenFalse = literalTemplates(expr.whenFalse);
    return whenTrue === null || whenFalse === null ? null : [...whenTrue, ...whenFalse];
  }
  return null;
}

/** The keys an object literal states; null when it spreads or computes one. */
function literalKeys(expr: ts.Expression): string[] | null {
  if (!ts.isObjectLiteralExpression(expr)) return null;
  const keys: string[] = [];
  for (const property of expr.properties) {
    if (ts.isShorthandPropertyAssignment(property)) keys.push(property.name.text);
    else if (ts.isPropertyAssignment(property) && (ts.isIdentifier(property.name) || ts.isStringLiteral(property.name))) keys.push(property.name.text);
    else return null;
  }
  return keys;
}

/** The import specifier SPEC of FILE, resolved to a scanned file's path, or null. */
function resolveImport(file: string, spec: string, files: ReadonlySet<string>): string | null {
  if (!spec.startsWith(".")) return null;
  const parts = file.split("/").slice(0, -1);
  for (const segment of spec.replace(/\.js$/, ".ts").split("/")) {
    if (segment === "..") parts.pop();
    else if (segment !== ".") parts.push(segment);
  }
  const resolved = parts.join("/");
  return files.has(resolved) ? resolved : null;
}

/** The scope of logger expression EXPR in a file whose loggers are LOGGERS, or null. */
function scopeOf(expr: ts.Expression, loggers: FileLoggers): string | null {
  if (ts.isIdentifier(expr)) return loggers.bound.get(expr.text) ?? null;
  if (ts.isCallExpression(expr) && ts.isPropertyAccessExpression(expr.expression) && expr.expression.name.text === "with") {
    return scopeOf(expr.expression.expression, loggers);
  }
  if (ts.isCallExpression(expr) && ts.isIdentifier(expr.expression) && expr.expression.text === "bindLog") {
    const scope = expr.arguments[0];
    return scope !== undefined && ts.isStringLiteral(scope) ? scope.text : null;
  }
  return null;
}

/**
 * Scan SOURCES, the shim's `src/` files (src/log.ts among them). KEY derives
 * a site's key; a test of the collision check alone hands in another.
 */
export function scanLogSites(sources: readonly ScanSource[], key: (scope: string, template: string) => string = (scope, template) => siteKey(RUNTIME, scope, template)): ScanResult {
  const parsed = sources.map(({ file, text }) => ({ file, source: ts.createSourceFile(file, text, ts.ScriptTarget.Latest, true) }));
  const files = new Set(sources.map(({ file }) => file));
  const faults: string[] = [];
  const scopes: { at: string; scope: string }[] = [];
  const loggersOf = new Map<string, FileLoggers>();
  const exported = new Map<string, Map<string, string>>();

  // Pass 1: every logger each file builds, and the names it binds them to.
  for (const { file, source } of parsed) {
    const loggers: FileLoggers = { bound: new Map() };
    const exports = new Map<string, string>();
    const visit = (node: ts.Node): void => {
      if (ts.isCallExpression(node) && ts.isIdentifier(node.expression) && node.expression.text === "bindLog") {
        const scope = node.arguments[0];
        const at = `${file}:${lineOf(source, node)}`;
        if (scope === undefined || !ts.isStringLiteral(scope)) faults.push(`${at}: bindLog's scope is not a string literal`);
        else scopes.push({ at, scope: scope.text });
      }
      ts.forEachChild(node, visit);
    };
    visit(source);
    // Two rounds bind `const B = A.with(...)` after `const A = bindLog(...)` wherever each is declared.
    for (let round = 0; round < 2; round++) {
      const bind = (node: ts.Node): void => {
        if (ts.isVariableDeclaration(node) && ts.isIdentifier(node.name) && node.initializer !== undefined) {
          const scope = scopeOf(node.initializer, loggers);
          if (scope !== null) {
            loggers.bound.set(node.name.text, scope);
            const statement = node.parent.parent;
            if (ts.isVariableStatement(statement) && statement.modifiers?.some((m) => m.kind === ts.SyntaxKind.ExportKeyword)) {
              exports.set(node.name.text, scope);
            }
          }
        }
        ts.forEachChild(node, bind);
      };
      bind(source);
    }
    loggersOf.set(file, loggers);
    exported.set(file, exports);
  }
  // Pass 2: a logger imported by name from the file that built it.
  for (const { file, source } of parsed) {
    const loggers = loggersOf.get(file) as FileLoggers;
    for (const statement of source.statements) {
      if (!ts.isImportDeclaration(statement) || !ts.isStringLiteral(statement.moduleSpecifier)) continue;
      const from = resolveImport(file, statement.moduleSpecifier.text, files);
      const bindings = statement.importClause?.namedBindings;
      if (from === null || bindings === undefined || !ts.isNamedImports(bindings)) continue;
      for (const element of bindings.elements) {
        const scope = exported.get(from)?.get((element.propertyName ?? element.name).text);
        if (scope !== undefined) loggers.bound.set(element.name.text, scope);
      }
    }
  }

  // Pass 3: every call that writes a template.
  const sites: ScannedSite[] = [];
  const add = (at: string, scope: string | null, templates: string[], values: ts.Expression | undefined | null): void => {
    const names = new Set<string>();
    templates.forEach((template, i) => {
      let found: string[];
      try {
        parseTemplate(template);
        found = placeholders(template);
      } catch (err) {
        faults.push(`${at}: ${(err as Error).message}`);
        return;
      }
      const sorted = [...found].sort().join(",");
      if (i > 0 && sorted !== [...names].sort().join(",")) {
        faults.push(`${at}: the branches of a conditional template name different placeholders`);
      }
      if (i === 0) for (const name of found) names.add(name);
      sites.push({ at, scope, template, key: scope === null ? null : key(scope, template) });
    });
    // NULL: the values travel apart from the call (src/log.ts hands a site its values itself).
    if (values === null) return;
    const keys = values === undefined ? [] : literalKeys(values);
    if (keys === null) return;
    const missing = [...names].filter((name) => !keys.includes(name));
    const extra = keys.filter((key) => !names.has(key));
    if (missing.length > 0) faults.push(`${at}: the named values lack {${missing.join("}, {")}}`);
    if (extra.length > 0) faults.push(`${at}: the named values hold ${extra.join(", ")}, which the template names no placeholder for`);
  };
  for (const { file, source } of parsed) {
    const loggers = loggersOf.get(file) as FileLoggers;
    const isLogModule = file.endsWith("src/log.ts") || file === "log.ts";
    const visit = (node: ts.Node): void => {
      if (ts.isCallExpression(node)) {
        const at = `${file}:${lineOf(source, node)}`;
        const callee = node.expression;
        if (ts.isPropertyAccessExpression(callee) && METHODS.has(callee.name.text) && node.arguments.length >= 2) {
          const receiver = callee.expression;
          const scope = scopeOf(receiver, loggers);
          const templates = literalTemplates(node.arguments[1]);
          // A call whose second argument is no literal writes a LogMessage
          // (ShimLogMethod's second signature), whose site is the
          // `.message(...)` call that made it; the compiler refuses a string
          // that is not a literal.
          if (templates !== null && !(ts.isIdentifier(receiver) && receiver.text === "console")) {
            add(at, scope, templates, node.arguments[2]);
          }
        } else if (ts.isPropertyAccessExpression(callee) && callee.name.text === "message" && node.arguments.length >= 1) {
          const scope = scopeOf(callee.expression, loggers);
          const templates = literalTemplates(node.arguments[0]);
          if (templates !== null) add(at, scope, templates, node.arguments[1]);
          else if (scope !== null) faults.push(`${at}: the template is not a string literal`);
        } else if (isLogModule && ts.isIdentifier(callee) && callee.text === "internal" && node.arguments.length >= 3) {
          const templates = literalTemplates(node.arguments[2]);
          if (templates !== null) add(at, LOGGING_SCOPE, templates, node.arguments[3]);
        } else if (isLogModule && ts.isIdentifier(callee) && callee.text === "emergencyStderr" && node.arguments.length >= 1) {
          const templates = literalTemplates(node.arguments[0]);
          if (templates !== null) add(at, LOGGING_SCOPE, templates, node.arguments[1]);
        } else if (isLogModule && ts.isPropertyAccessExpression(callee) && callee.name.text === "site" && ts.isIdentifier(callee.expression) && callee.expression.text === "LOGGING_SITES") {
          const templates = literalTemplates(node.arguments[0]);
          if (templates !== null) add(at, LOGGING_SCOPE, templates, null);
        }
      }
      ts.forEachChild(node, visit);
    };
    visit(source);
  }

  // The faults that make two calls one site, or two sites one key.
  const seenScope = new Map<string, string>();
  for (const { at, scope } of scopes) {
    const first = seenScope.get(scope);
    if (first !== undefined) faults.push(`${at}: the scope ${JSON.stringify(scope)} is already built at ${first}`);
    else seenScope.set(scope, at);
  }
  const bySite = new Map<string, string>();
  const byKey = new Map<string, string>();
  const byTemplate = new Map<string, ScannedSite[]>();
  for (const site of sites) {
    byTemplate.set(site.template, [...(byTemplate.get(site.template) ?? []), site]);
    if (site.scope === null) continue;
    const id = `${site.scope}\u0000${site.template}`;
    const first = bySite.get(id);
    if (first !== undefined && first !== site.at) {
      faults.push(`${site.at}: writes the template ${JSON.stringify(site.template)} that ${first} already writes in the scope ${site.scope}; give each call its own wording`);
      continue;
    }
    bySite.set(id, site.at);
    const collided = byKey.get(site.key as string);
    if (collided !== undefined && collided !== id) faults.push(`${site.at}: its key ${site.key} collides with another site's`);
    byKey.set(site.key as string, id);
  }
  for (const site of sites) {
    if (site.scope !== null) continue;
    const others = (byTemplate.get(site.template) ?? []).filter((other) => other.at !== site.at);
    if (others.length > 0) {
      faults.push(`${site.at}: logs through a logger the scan cannot place, so its template ${JSON.stringify(site.template)} must be unique in the shim, and ${others.map((o) => o.at).join(", ")} writes it too`);
    }
  }
  return { sites, scopes, faults };
}
