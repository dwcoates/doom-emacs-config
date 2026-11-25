/**
 * EVERY STOP IS DRAWN THROUGH drawStopControl (src/feed/stop-control.ts): a
 * rule over the sources that draw one.
 */
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { describe, expect, it } from "vitest";

describe("the stop control's call sites", () => {
  it.each(["src/feed/rows/subagent.ts", "src/feed/merge/tab-panels.ts", "src/feed/cards/shell.ts"])(
    "%s draws its stop through drawStopControl rather than a control of its own",
    (file) => {
      const source = readFileSync(join(process.cwd(), file), "utf8");
      expect([source.includes("drawStopControl("), source.includes('client.interrupt(')]).toEqual([true, false]);
    },
  );
});
