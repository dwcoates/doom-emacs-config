/**
 * THE SUITES THAT RECORD A POPUP MIRROR OR HOLD A POPUP CHANNEL TAKE IT FROM
 * THE SHARED STAND-INS (`test/host/popup-channel.ts`), never their own.
 */
import { describe, expect, it } from "vitest";
import { testDir } from "../files-under.js";

describe("the suites that record a popup mirror", () => {
  it.each(["../sidebar/memory.test.ts", "../sidebar/row-menu.test.ts"])(
    "%s takes its recording mirror from the shared stand-ins, never its own",
    async (file) => {
      const fs = await import("node:fs");
      const path = await import("node:path");
      const source = fs.readFileSync(path.join(testDir, "host", file), "utf8");
      expect([source.includes("recordingPopupMirror"), /told\.push\(`show:/.test(source)]).toEqual([true, false]);
    },
  );
});

describe("the suites that hold a popup channel", () => {
  it.each(["sidebar-popups.test.ts", "sidebar-page.test.ts", "start.test.ts"])(
    "%s takes its channel from the shared stand-ins, never its own",
    async (file) => {
      const fs = await import("node:fs");
      const path = await import("node:path");
      const source = fs.readFileSync(path.join(testDir, "host", file), "utf8");
      expect([source.includes("./popup-channel.js"), /postMessage:\s*\(/.test(source)]).toEqual([true, false]);
    },
  );
});
