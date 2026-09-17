import { readFileSync, readdirSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import ts from "typescript";
import { describe, expect, it } from "vitest";

type Level = "debug" | "info" | "warn" | "error" | "logVerbose";

interface ExpectedSite {
  readonly file: string;
  readonly message: string;
  readonly level: Level;
}

interface LogCall {
  readonly file: string;
  readonly text: string;
  readonly source: ts.SourceFile;
  readonly node: ts.CallExpression;
  readonly level: Level;
  readonly message?: string;
}

const RECLASSIFIED_SITES: readonly ExpectedSite[] = [
  { file: "store/reader.ts", message: "the store refused to open an agent's book", level: "debug" },
  { file: "engine/turn.ts", message: "ReadHistory refused", level: "debug" },
  { file: "engine/session.ts", message: "refused a cold resume because the caller named no remediation for its stated cost", level: "debug" },
  { file: "convert/attachments.ts", message: "no context-injection converter owns this attachment; it lands as residue", level: "debug" },
  { file: "convert/blocks.ts", message: "a tool result block's kind is not modelled; it is kept whole as unsupported", level: "debug" },
  { file: "convert/tools/read.ts", message: "a text read carried no content; no success frame is produced", level: "debug" },
  { file: "convert/tools/web-search.ts", message: "a settled web search stated no results array; the answer is recorded as empty", level: "debug" },
  { file: "fake/scenarios/lifecycle.ts", message: "the fake query ends with no result", level: "debug" },
  { file: "convert/detached.ts", message: "a detached run reached a failure terminal", level: "info" },
  { file: "convert/hooks.ts", message: "a hook was cancelled before it finished", level: "info" },
  { file: "convert/terminals.ts", message: "the turn ended on a recorded API failure", level: "info" },
  { file: "engine/identity.ts", message: "the vendor session id rotated; the main agent identity is unchanged", level: "info" },
  // THE KEEP-ALIVE REWIND IS VISIBLE (2026-09-14). The rewind is the one step
  // that can lose a real prompt, and its two dead anchors were diagnosable only
  // because the shim had said which uuid it resumed at.
  {
    file: "engine/session.ts",
    message:
      "REWINDING the vendor context past the trailing keep-alive turns before delivering a real prompt; the anchor is the assistant record of that turn",
    level: "info",
  },
  {
    file: "engine/session.ts",
    message: "the keep-alive rewind LANDED: the vendor resumed at the anchor and is answering the real prompt",
    level: "info",
  },
  // THE ROLLBACK NOW RUNS BETWEEN BEATS (2026-09-17). Each keep-alive rewinds
  // the prior one out, so the transcript never holds more than one; the two
  // messages below are the keep-alive-facing counterparts of the real-prompt
  // pair above, and stay at INFO for the same reason.
  {
    file: "engine/session.ts",
    message:
      "REWINDING the vendor context past the outstanding keep-alive turn before submitting the next keep-alive; at most one keep-alive is ever in the transcript",
    level: "info",
  },
  {
    file: "engine/session.ts",
    message:
      "the keep-alive rewind LANDED: the vendor resumed at the anchor and is answering the next keep-alive",
    level: "info",
  },
  {
    file: "engine/session.ts",
    message:
      "the prompt the refused rewind was carrying was RE-DELIVERED on a plain resume; it was not lost",
    level: "info",
  },
  {
    file: "engine/keepalive.ts",
    message:
      "the keep-alive rewind anchor is CLEARED: a uuid from before this boundary may not be resumable",
    level: "info",
  },
  {
    file: "engine/keepalive.ts",
    message:
      "no rewind anchor exists (none taken yet, or cleared at a boundary): the next real prompt proceeds WITHOUT a rewind and carries the keep-alive turns",
    level: "info",
  },
] as const;

const SOURCE_ROOT = fileURLToPath(new URL("../src/", import.meta.url));

function sourceFiles(directory = SOURCE_ROOT): string[] {
  return readdirSync(directory, { withFileTypes: true }).flatMap((entry) => {
    const target = path.join(directory, entry.name);
    if (entry.isDirectory()) return sourceFiles(target);
    return entry.isFile() && entry.name.endsWith(".ts") ? [target] : [];
  });
}

function collectLogCalls(): LogCall[] {
  const calls: LogCall[] = [];
  const levels = new Set<Level>(["debug", "info", "warn", "error", "logVerbose"]);
  for (const file of sourceFiles()) {
    const text = readFileSync(file, "utf8");
    const source = ts.createSourceFile(file, text, ts.ScriptTarget.Latest, true);
    const visit = (node: ts.Node): void => {
      if (ts.isCallExpression(node) && ts.isPropertyAccessExpression(node.expression)) {
        const level = node.expression.name.text as Level;
        const message = node.arguments[1];
        if (levels.has(level)) {
          calls.push({
            file: path.relative(SOURCE_ROOT, file),
            text,
            source,
            node,
            level,
            ...(message !== undefined && ts.isStringLiteralLike(message) ? { message: message.text } : {}),
          });
        }
      }
      ts.forEachChild(node, visit);
    };
    visit(source);
  }
  return calls;
}

const LOG_CALLS = collectLogCalls();

describe("shim log-level classification", () => {
  it.each(RECLASSIFIED_SITES)("keeps $file message at $level", (site) => {
    // Arrange.
    const expected = site.level;

    // Act.
    const actual = LOG_CALLS
      .filter((call) => call.file === site.file && call.message === site.message)
      .map((call) => call.level);

    // Assert.
    expect(actual).toEqual([expected]);
  });

  it("keeps every surviving warning adjacent to its defect or decision justification", () => {
    // Arrange.
    const unjustified: string[] = [];

    // Act.
    for (const call of LOG_CALLS.filter((candidate) => candidate.level === "warn")) {
      const line = call.source.getLineAndCharacterOfPosition(call.node.getStart(call.source)).line;
      const previous = call.text.split("\n")[line - 1]?.trim() ?? "";
      if (!/^\/\/ warn: (?:a defect|a decision) because \S/.test(previous)) {
        unjustified.push(`${call.file}:${line + 1}`);
      }
    }

    // Assert.
    expect(unjustified).toEqual([]);
  });

  it("keeps warnings below sixty sites", () => {
    // Arrange, Act.
    const warnings = LOG_CALLS.filter((call) => call.level === "warn");

    // Assert.
    expect(warnings.length).toBeLessThan(60);
  });

  it("grows the logging inventory beyond the audited baseline", () => {
    // Arrange, Act, Assert.
    expect(LOG_CALLS.length).toBeGreaterThan(636);
  });

  it("carries error text in structured context at every error site", () => {
    // Arrange.
    const missing: string[] = [];

    // Act.
    for (const call of LOG_CALLS.filter((candidate) => candidate.level === "error")) {
      const context = call.node.arguments[0];
      const fields = context !== undefined && ts.isObjectLiteralExpression(context)
        ? context.properties.flatMap((property) =>
            ts.isPropertyAssignment(property) || ts.isShorthandPropertyAssignment(property)
              ? [property.name.getText(call.source)]
              : [],
          )
        : [];
      if (!fields.some((field) => field === "cause" || field === "detail" || field === "parse_error")) {
        const line = call.source.getLineAndCharacterOfPosition(call.node.getStart(call.source)).line;
        missing.push(`${call.file}:${line + 1}`);
      }
    }

    // Assert.
    expect(missing).toEqual([]);
  });
});
