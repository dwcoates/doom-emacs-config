// Which daemon a document dials, as a value it is told. See
// src/rpc/daemon-origin.ts.
import { describe, expect, it } from "vitest";

import { createDaemonOrigin, isDaemonOrigin } from "../../src/rpc/daemon-origin.js";

describe("createDaemonOrigin", () => {
  it("answers the origin it started at", () => {
    // Act
    const daemon = createDaemonOrigin("http://host.localhost:1");

    // Assert
    expect(daemon.current()).toBe("http://host.localhost:1");
  });

  it("moves to a new origin and says it moved", () => {
    // Arrange
    const daemon = createDaemonOrigin("http://host.localhost:1");

    // Act
    const moved = daemon.set("http://host.localhost:2");

    // Assert
    expect([moved, daemon.current()]).toEqual([true, "http://host.localhost:2"]);
  });

  it("says nothing moved when told the origin it holds", () => {
    // Arrange
    const daemon = createDaemonOrigin("http://host.localhost:1");

    // Act / Assert
    expect(daemon.set("http://host.localhost:1")).toBe(false);
  });
});

describe("isDaemonOrigin", () => {
  it.each([
    ["an http origin", "http://host.localhost:61048", true],
    ["an origin with a path", "http://host.localhost:61048/?host=1", false],
    ["an https origin", "https://host.localhost:61048", false],
    ["no URL", "host.localhost", false],
    ["not a string", 61048, false],
  ])("%s", (_name, value, want) => {
    expect(isDaemonOrigin(value)).toBe(want);
  });
});
