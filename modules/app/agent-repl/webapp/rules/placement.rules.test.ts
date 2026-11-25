/** The placement sites share the helpers of src/placement.ts. */
import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";
import { filesUnder } from "./files-under.js";

const SRC = path.join(path.dirname(fileURLToPath(import.meta.url)), "../src");

/** Every TypeScript source under SRC, relative to it. */
const sources = (): string[] => filesUnder(SRC).map((file) => path.relative(SRC, file));

describe("the placement sites share the helpers", () => {
  it("places a box beside its anchor nowhere but placeBeside", () => {
    // A site that clamps beside and applies the result by hand is the drift
    // placeBeside exists to prevent.
    const handRolled = sources().filter((file) => {
      const text = readFileSync(path.join(SRC, file), "utf8");
      return file !== "placement.ts" && /clampBeside\(/.test(text) && /applyPlacement\(/.test(text);
    });
    expect(handRolled).toEqual([]);
  });

  it("reads the window's size nowhere but viewportOf", () => {
    const readers = sources().filter((file) => file !== "placement.ts" && /\.innerWidth\b/.test(readFileSync(path.join(SRC, file), "utf8")));
    expect(readers).toEqual([]);
  });

  it("applies a viewport placement's height cap nowhere but applyPlacement, the host-relative topbar reveal aside", () => {
    // The topbar's reveal is placed against its host's box, not the window
    // (topbar/reveal.ts), so its left is translated and it applies its own.
    const appliers = sources().filter(
      (file) => !["placement.ts", path.join("topbar", "reveal.ts")].includes(file) && /\.maxHeight\}px`/.test(readFileSync(path.join(SRC, file), "utf8")),
    );
    expect(appliers).toEqual([]);
  });
});
