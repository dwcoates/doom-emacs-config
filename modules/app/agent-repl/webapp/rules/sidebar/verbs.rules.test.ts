/**
 * THE SIDEBAR'S SELECTS AND A PAGE'S SELECTION, read off src/: every select
 * goes through `runSelectVerb` and is spelled once, by the row, and a page
 * selects only through the one SelectInEditor request builder.
 */
import { readdirSync, readFileSync } from "node:fs";
import { join, relative } from "node:path";
import { describe, expect, it } from "vitest";
import { filesUnder } from "../files-under.js";
import { codeOf } from "../../test/source-text.js";

/** Every webapp source file under DIR, recursively, as paths relative to DIR. */
function sourcesUnder(dir: string): string[] {
  return filesUnder(dir).map((file) => relative(dir, file));
}

describe("the in-flight lockout", () => {
  // EVERY SELECT GOES THROUGH runSelectVerb: a select spelled through runVerb
  // would lock its line out again, the stale lockout this pins against.
  it("is skipped by every select the sidebar spells", () => {
    // Arrange
    const dir = join(__dirname, "..", "..", "src", "sidebar");
    const spellers: string[] = [];

    // Act
    for (const name of readdirSync(dir).filter((file) => file.endsWith(".ts"))) {
      const code = codeOf(readFileSync(join(dir, name), "utf8"));
      for (const match of code.matchAll(/rpc: "Select(?:InEditor|Workspace)"/g)) {
        const before = code.slice(0, match.index);
        const opener = [...before.matchAll(/\b(runVerb|runSelectVerb|fireVerb|sendHeld)\(/g)].at(-1)?.[1];
        spellers.push(`${name}:${opener ?? "none"}`);
      }
    }

    // Assert
    expect(spellers.filter((speller) => !speller.endsWith(":runSelectVerb"))).toEqual([]);
  });

  // THE MERGE QUEUE SELECTS THROUGH THE ROW'S SELECT (`selectWorkspace`), so
  // the rail's one select is spelled once, in row.ts.
  it("is spelled once in the sidebar, by the row", () => {
    const dir = join(__dirname, "..", "..", "src", "sidebar");
    const spelled = readdirSync(dir)
      .filter((file) => file.endsWith(".ts"))
      .filter((name) => /rpc: "Select(?:InEditor|Workspace)"/.test(codeOf(readFileSync(join(dir, name), "utf8"))));
    expect(spelled).toEqual(["row.ts"]);
  });
});

// SELECTING IS ONE ACT (AGENTS.md "Selecting a workspace is one act"): every
// selection a page makes is the editor's own switch, relayed by SelectInEditor.
// A page that sent SelectWorkspace left the frame to follow the roster later,
// and the rail drew the workspace selected while Emacs showed another
// (a merge queue entry's click, 2026-11-11).
describe("a page's selection", () => {
  const src = join(__dirname, "..", "..", "src");

  it("is never a SelectWorkspace", () => {
    const senders = sourcesUnder(src).filter((name) => {
      const code = codeOf(readFileSync(join(src, name), "utf8"));
      return /\.selectWorkspace\(|"SelectWorkspace"/.test(code);
    });
    expect(senders).toEqual([]);
  });

  it("is built by the one SelectInEditor request builder", () => {
    const builders = sourcesUnder(src).filter((name) =>
      /SelectInEditorRequestSchema\b/.test(codeOf(readFileSync(join(src, name), "utf8"))),
    );
    expect(builders).toEqual(["sidebar/verbs.ts"]);
  });

  it("is found by the scan in each place a page selects", () => {
    const callers = sourcesUnder(src).filter((name) =>
      /buildSelectInEditorRequest\(/.test(codeOf(readFileSync(join(src, name), "utf8"))),
    );
    expect(callers.filter((name) => name !== "sidebar/verbs.ts").sort()).toEqual([
      "feed/merge/jump.ts",
      "sidebar/row.ts",
      "sidebar/threads.ts",
    ]);
  });
});
