/** engine/in-flight.ts — the record of one piece of work in flight. */
import { describe, expect, it } from "vitest";
import { InFlight, type InFlightOutcome } from "../../src/engine/in-flight.js";
import { settleable, type Settleable } from "../../src/engine/settleable.js";

/** The outcome's kind, which is what every case's waiter is told. */
function kindOf(outcome: InFlightOutcome<unknown>): string {
  return outcome.kind;
}

describe("InFlight", () => {
  it("holds nothing in flight before any work runs", () => {
    // Arrange
    const record = new InFlight<string>();

    // Assert
    expect(record.settling).toBeUndefined();
  });

  it("is in flight already when the work's own first step runs", async () => {
    // Arrange
    const record = new InFlight<string>();
    let seenByWork: Promise<string> | undefined;

    // Act
    await record.run(() => {
      seenByWork = record.settling;
      return Promise.resolve("done");
    }, kindOf);

    // Assert
    expect(seenByWork).toBeDefined();
  });

  it("answers its caller with the work's own value", async () => {
    // Arrange
    const record = new InFlight<string>();

    // Act
    const value = await record.run(() => Promise.resolve(42), kindOf);

    // Assert
    expect(value).toBe(42);
  });

  it("answers its caller with the work's own rejection", async () => {
    // Arrange
    const record = new InFlight<string>();
    const cause = new Error("the work failed");

    // Act
    const running = record.run(() => Promise.reject(cause), kindOf);

    // Assert
    await expect(running).rejects.toBe(cause);
  });

  it.each([
    { name: "resolved", settle: (gate: Settleable<boolean>) => gate.resolve(true), want: "resolved" },
    { name: "rejected", settle: (gate: Settleable<boolean>) => gate.resolve(false), want: "rejected" },
  ])("tells a waiter the work $name, without rejecting", async ({ settle, want }) => {
    // Arrange
    const record = new InFlight<string>();
    const gate = settleable<boolean>();
    const running = record.run(async () => {
      if (!(await gate.promise)) throw new Error("the work failed");
    }, kindOf);
    running.catch(() => undefined);
    const waiting = record.settling;

    // Act
    settle(gate);

    // Assert
    await expect(waiting).resolves.toBe(want);
  });

  it("is cleared before a waiter resumes", async () => {
    // Arrange
    const record = new InFlight<string>();
    const gate = settleable();
    void record.run(() => gate.promise, kindOf);
    const waiting = record.settling;

    // Act
    gate.resolve();
    await waiting;

    // Assert
    expect(record.settling).toBeUndefined();
  });

  it("settles as rejected, with the thrown value, when the work throws before its first await", async () => {
    // Arrange
    const record = new InFlight<InFlightOutcome<unknown>>();
    const thrown = { not: "an Error" };

    // Act
    const running = record.run(() => {
      throw thrown;
    }, (outcome) => outcome);
    running.catch(() => undefined);
    const outcome = await record.settling;

    // Assert
    expect(outcome).toEqual({ kind: "rejected", error: thrown });
  });

  it("leaves a newer run's record in flight when the older work settles", async () => {
    // Arrange
    const record = new InFlight<string>();
    const older = settleable();
    const newer = settleable();
    const olderRun = record.run(() => older.promise, () => "older");
    void record.run(() => newer.promise, () => "newer");
    const newerSettling = record.settling;

    // Act
    older.resolve();
    await olderRun;

    // Assert
    expect(record.settling).toBe(newerSettling);
  });
});
