// Which daemon a document dials, as a value it is told. See
// src/rpc/daemon-origin.ts.
import { describe, expect, it } from "vitest";

import { createDaemonOrigin, isDaemonOrigin, webhostOriginAt } from "../../src/rpc/daemon-origin.js";

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

describe("createDaemonOrigin: onMoved", () => {
  it("tells a listener the origin moved, already pointing at the new origin", () => {
    // Arrange
    const daemon = createDaemonOrigin("http://host.localhost:1");
    const seen: string[] = [];
    daemon.onMoved(() => seen.push(daemon.current()));

    // Act
    daemon.set("http://host.localhost:2");

    // Assert
    expect(seen).toEqual(["http://host.localhost:2"]);
  });

  it("tells no listener when told the origin it holds", () => {
    // Arrange
    const daemon = createDaemonOrigin("http://host.localhost:1");
    let moves = 0;
    daemon.onMoved(() => (moves += 1));

    // Act
    daemon.set("http://host.localhost:1");

    // Assert
    expect(moves).toBe(0);
  });

  it("stops telling a listener that unsubscribed", () => {
    // Arrange
    const daemon = createDaemonOrigin("http://host.localhost:1");
    let moves = 0;
    const unsubscribe = daemon.onMoved(() => (moves += 1));
    unsubscribe();

    // Act
    daemon.set("http://host.localhost:2");

    // Assert
    expect(moves).toBe(0);
  });

  it("tells every listener, even when one unsubscribes itself as it runs", () => {
    // Arrange
    const daemon = createDaemonOrigin("http://host.localhost:1");
    const told: string[] = [];
    const unsubscribeFirst = daemon.onMoved(() => {
      told.push("first");
      unsubscribeFirst();
    });
    daemon.onMoved(() => told.push("second"));

    // Act
    daemon.set("http://host.localhost:2");

    // Assert
    expect(told).toEqual(["first", "second"]);
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

describe("webhostOriginAt", () => {
  it("names the host origin on the daemon's port", () => {
    expect(webhostOriginAt("127.0.0.1:6666")).toBe("http://host.localhost:6666");
  });

  it("throws on an address with no port", () => {
    expect(() => webhostOriginAt("127.0.0.1")).toThrow(/no port/);
  });
});
