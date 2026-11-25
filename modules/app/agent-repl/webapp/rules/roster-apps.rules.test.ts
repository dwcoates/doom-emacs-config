/** rosterApps (test/roster-apps.ts) is how every test roster carries the app boxes. */
import { readFileSync } from "node:fs";
import path from "node:path";
import { describe, expect, it } from "vitest";
import { testDir } from "./files-under.js";

describe("rosterApps", () => {
  it.each(["sidebar/harness.ts", "integration/fixtures.ts"])("is how %s's roster carries the app boxes", (file) => {
    // Arrange
    const source = readFileSync(path.join(testDir, file), "utf8");

    // Assert
    expect(source).toContain("...rosterApps(");
    expect(source).not.toContain("appSwitcher: {");
  });
});
