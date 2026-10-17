// The stops of the boot's page-lifetime installs. See src/page-teardown.ts.
import { describe, expect, it } from "vitest";

import { stopAtPageTeardown, tearDownPage } from "../src/page-teardown.js";

describe("tearDownPage", () => {
  it("runs every registered stop, the latest first", () => {
    // Arrange
    const ran: string[] = [];
    stopAtPageTeardown(() => ran.push("census"));
    stopAtPageTeardown(() => ran.push("probe"));
    // Act
    tearDownPage();
    // Assert
    expect(ran).toEqual(["probe", "census"]);
  });

  it("forgets the stops it ran, so a second teardown runs none", () => {
    // Arrange
    let runs = 0;
    stopAtPageTeardown(() => (runs += 1));
    tearDownPage();
    // Act
    tearDownPage();
    // Assert
    expect(runs).toBe(1);
  });

  it("does nothing when nothing was registered", () => {
    expect(() => tearDownPage()).not.toThrow();
  });
});
