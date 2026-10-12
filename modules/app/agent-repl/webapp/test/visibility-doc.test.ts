// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { visibilityDoc } from "./visibility-doc.js";

describe("the test-set visibility document", () => {
  it("reports the visibility it starts with", () => {
    // Arrange, Act
    const v = visibilityDoc("hidden");
    // Assert
    expect(v.doc.visibilityState).toBe("hidden");
  });

  it("reports a new visibility and fires the platform's event", () => {
    // Arrange
    const v = visibilityDoc("hidden");
    const heard: DocumentVisibilityState[] = [];
    v.doc.addEventListener("visibilitychange", () => heard.push(v.doc.visibilityState));
    // Act
    v.set("visible");
    // Assert
    expect(heard).toEqual(["visible"]);
  });

  it("is the one way the suites fake a page's visibility", () => {
    // Arrange
    const files = ["test/page-seen.test.ts", "test/sidebar/unseen.test.ts", "test/sidebar/sidebar.test.ts"];
    // Act
    const handRolled = files.filter((file) =>
      readFileSync(join(process.cwd(), file), "utf8").includes('defineProperty(doc, "visibilityState"'),
    );
    // Assert
    expect(handRolled).toEqual([]);
  });
});
