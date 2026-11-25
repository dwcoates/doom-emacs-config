import { readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { describe, expect, it } from "vitest";

// THE CALL SITES SHARE THE ONE DRAWER: a source scan, so a site that hand-rolls
// `innerHTML = inline(escapeHtml(...))` again fails here rather than drifting.
// THE CALL SITES SHARE THE ONE DRAWER: a source scan, so a site that hand-rolls
// `innerHTML = inline(escapeHtml(...))` again fails here rather than drifting.
describe("setInlineMarkdown's call sites", () => {
  const here = path.dirname(fileURLToPath(import.meta.url));
  it.each(["src/news-digest/news-digest.ts", "src/feed/asks/question.ts", "src/feed/tabs.ts"])(
    "%s draws inline markdown through setInlineMarkdown",
    (file) => {
      // Arrange
      const source = readFileSync(path.join(here, "..", file), "utf8");
      // Act / Assert
      expect([source.includes("setInlineMarkdown("), /innerHTML\s*=\s*inline\(/.test(source)]).toEqual([true, false]);
    },
  );
});
