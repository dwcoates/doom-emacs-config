// @vitest-environment jsdom
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { codeOf } from "../../source-text.js";
import { describe, expect, it } from "vitest";
import { drawHandbackBadge, type HandbackBadgeKind } from "../../../src/feed/rows/handback-badge.js";
import { captureLogRecords, forwardedRecord } from "../../log-capture.js";

/** A kind that is neither real one, so the parameters are seen to be used. */
const KIND: HandbackBadgeKind = { className: "test-handback-badge", operation: "feed.draw-test-handback", describe: "a test hand-back" };

describe("drawHandbackBadge", () => {
  it("draws the label verbatim as the badge's only content", () => {
    const el = drawHandbackBadge("work reported back", KIND);
    expect([el.textContent, el.children.length]).toEqual(["work reported back", 0]);
  });

  it("wears the shared pill and the kind's own class", () => {
    const el = drawHandbackBadge("work reported back", KIND);
    expect([el.classList.contains("badge"), el.classList.contains("test-handback-badge")]).toEqual([true, true]);
  });

  it("records the draw under the kind's operation", async () => {
    const capture = captureLogRecords();
    drawHandbackBadge("work reported back", KIND);
    const record = await forwardedRecord(capture, "feed.draw-test-handback");
    expect(record.level.case).toBe("info");
  });

  it("is what every handback badge row draws through", () => {
    for (const file of ["subagent-handback.ts"]) {
      const source = codeOf(readFileSync(join(process.cwd(), "src/feed/rows", file), "utf8"));
      expect(source.includes("drawHandbackBadge("), file).toBe(true);
      expect(source.includes("createElement("), file).toBe(false);
    }
  });
});
