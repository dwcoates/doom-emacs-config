/**
 * Every tool card that shows an exit code draws it through drawToolExitLine
 * (src/feed/cards/exit-line.ts).
 */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";

const SRC = path.join(path.dirname(fileURLToPath(import.meta.url)), "../../../src");

describe("drawToolExitLine", () => {
  it.each(["feed/cards/tool-call.ts", "feed/cards/hook.ts"])("is how %s draws its exit code", (file) => {
    expect(readFileSync(path.join(SRC, file), "utf8")).toContain("drawToolExitLine(");
  });
});
