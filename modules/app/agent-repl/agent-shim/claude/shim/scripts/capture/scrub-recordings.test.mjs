/** Unit tests for the recording re-scrub. */
import { mkdirSync, mkdtempSync, readFileSync, readdirSync, realpathSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import path from "node:path";
import { describe, expect, it } from "vitest";

import { scrubTree } from "./scrub-recordings.mjs";

const personal = { home: "/Users/ann", emails: ["ann@host.test"], names: ["ann"] };

/** A recordings tree holding the given files (relative path -> text). */
function tree(files) {
  const root = realpathSync(mkdtempSync(path.join(tmpdir(), "scrub-recordings-")));
  for (const [rel, text] of Object.entries(files)) {
    mkdirSync(path.dirname(path.join(root, rel)), { recursive: true });
    writeFileSync(path.join(root, rel), text, "utf8");
  }
  return root;
}

describe("scrubTree", () => {
  it("rewrites contents as text, touching only the personal values", () => {
    const root = tree({ "s.jsonl": '{"cwd":"/Users/ann/p",  "n":1}\n' });
    scrubTree(root, personal);
    expect(readFileSync(path.join(root, "s.jsonl"), "utf8")).toBe('{"cwd":"${HOME}/p",  "n":1}\n');
  });

  it("renames a directory named after the home, after scrubbing what it holds", () => {
    const root = tree({ "projects/-Users-ann-p/s.jsonl": '{"who":"Ann"}\n' });
    scrubTree(root, personal);
    expect(readdirSync(path.join(root, "projects"))).toEqual(["--HOME--p"]);
    expect(readFileSync(path.join(root, "projects", "--HOME--p", "s.jsonl"), "utf8")).toBe('{"who":"Someone"}\n');
  });

  it("answers every changed path and leaves a clean file untouched", () => {
    const root = tree({ "clean.txt": "nothing here", "dirty.txt": "ann@host.test" });
    expect(scrubTree(root, personal)).toEqual([path.join(root, "dirty.txt")]);
    expect(readFileSync(path.join(root, "clean.txt"), "utf8")).toBe("nothing here");
  });
});
