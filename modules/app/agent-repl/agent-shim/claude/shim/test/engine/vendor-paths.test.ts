import { readdirSync, readFileSync, statSync } from "node:fs";
import path from "node:path";
import { describe, expect, it } from "vitest";
import { cwdSlug, projectDir, transcriptPath } from "../../src/engine/vendor-paths.js";

describe("cwdSlug", () => {
  it.each([
    { name: "replaces every slash and dot", dir: "/Users/x/.config/y", slug: "-Users-x--config-y" },
    { name: "replaces an underscore too", dir: "/private/var/folders/_m/x", slug: "-private-var-folders--m-x" },
    { name: "preserves case and existing dashes", dir: "/Users/ABC/doom-worktrees/Feature", slug: "-Users-ABC-doom-worktrees-Feature" },
  ])("$name", ({ dir, slug }) => {
    // Act / Assert
    expect(cwdSlug(dir)).toBe(slug);
  });
});

describe("projectDir", () => {
  it("names the slugged directory under the account's projects directory", () => {
    // Act / Assert
    expect(projectDir("/account", "/w/s")).toBe("/account/projects/-w-s");
  });
});

describe("transcriptPath", () => {
  it("names the session file inside the project directory", () => {
    // Act / Assert
    expect(transcriptPath("/account", "/w/s", "s-1")).toBe("/account/projects/-w-s/s-1.jsonl");
  });
});

describe("the vendor's layout has one home", () => {
  /** Every TypeScript file under DIR, recursively. */
  function sources(dir: string): string[] {
    return readdirSync(dir).flatMap((name) => {
      const full = path.join(dir, name);
      if (statSync(full).isDirectory()) return sources(full);
      return full.endsWith(".ts") ? [full] : [];
    });
  }

  it("leaves no hand-rolled slug rule or projects directory outside vendor-paths.ts", () => {
    // Arrange
    const root = new URL("../..", import.meta.url).pathname;
    // The home, and this guard, which names the patterns it looks for.
    const exempt = new Set([path.join(root, "src", "engine", "vendor-paths.ts"), new URL(import.meta.url).pathname]);
    const files = [...sources(path.join(root, "src")), ...sources(path.join(root, "test"))].filter((f) => !exempt.has(f));

    // Act
    const handRolled = files.filter((file) => {
      const source = readFileSync(file, "utf8");
      return source.includes("[^A-Za-z0-9]/g") || /"projects",\s*cwdSlug\(/.test(source);
    });

    // Assert
    expect(handRolled.map((file) => path.relative(root, file))).toEqual([]);
  });
});
