/**
 * The session's vendor process as a standing level: the running arm's facts,
 * the ended arm, and which exit is allowed to end the level.
 */
import { describe, expect, it } from "vitest";
import {
  exitEndsLevel,
  vendorProcessEnded,
  vendorProcessRunning,
  vendorProcessUpdate,
} from "../../src/convert/vendor-process.js";

describe("vendorProcessRunning", () => {
  it("states the child's pid", () => {
    // Arrange, Act.
    const level = vendorProcessRunning(29021, 5_000);

    // Assert.
    expect(level.state.case === "running" ? level.state.value.pid : 0).toBe(29021);
  });

  it("states the instant the shim observed the child", () => {
    // Arrange, Act.
    const level = vendorProcessRunning(29021, 5_000);

    // Assert.
    expect(level.state.case === "running" ? level.state.value.startedAtMs : 0n).toBe(5_000n);
  });

  it("refuses a pid that is not positive, since no running process has one", () => {
    // Arrange, Act, Assert.
    expect(() => vendorProcessRunning(0, 5_000)).toThrow(/positive pid/);
  });
});

describe("vendorProcessEnded", () => {
  it("is the ended arm, carrying no pid", () => {
    // Arrange, Act.
    const level = vendorProcessEnded();

    // Assert.
    expect(level.state.case).toBe("ended");
  });
});

describe("exitEndsLevel", () => {
  it("ends the level on the running child's own exit", () => {
    // Arrange, Act, Assert.
    expect(exitEndsLevel(vendorProcessRunning(29021, 5_000), 29021)).toBe(true);
  });

  it("does not end the level on a replaced child's late exit", () => {
    // Arrange, Act, Assert.
    expect(exitEndsLevel(vendorProcessRunning(29021, 5_000), 28000)).toBe(false);
  });

  it("does not end the level on the exit of a child that never spawned", () => {
    // Arrange, Act, Assert.
    expect(exitEndsLevel(vendorProcessRunning(29021, 5_000), undefined)).toBe(false);
  });

  it("does not end a level that already ended", () => {
    // Arrange, Act, Assert.
    expect(exitEndsLevel(vendorProcessEnded(), 29021)).toBe(false);
  });

  it("does not end a level that was never stated", () => {
    // Arrange, Act, Assert.
    expect(exitEndsLevel(undefined, 29021)).toBe(false);
  });
});

describe("vendorProcessUpdate", () => {
  it("carries the level on the session update's vendor-process arm", () => {
    // Arrange.
    const level = vendorProcessEnded();

    // Act.
    const update = vendorProcessUpdate(level);

    // Assert.
    expect(update.update.case === "vendorProcess" ? update.update.value : undefined).toBe(level);
  });
});
