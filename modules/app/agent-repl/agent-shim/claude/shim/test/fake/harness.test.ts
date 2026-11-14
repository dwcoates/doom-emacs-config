/**
 * The scenario drive's own machinery.
 */
import { describe, expect, it } from "vitest";
import type { SdkMessage } from "../../src/sdk/types.js";
import { messageArrivals } from "./harness.js";

const result = { type: "result" } as SdkMessage;
const assistant = { type: "assistant" } as SdkMessage;
const isResult = (message: SdkMessage): boolean => message.type === "result";

describe("messageArrivals", () => {
  it("resolves at once when a matching message already arrived", async () => {
    // Arrange
    const arrivals = messageArrivals([result]);

    // Act / Assert
    await expect(arrivals.until("a result", isResult)).resolves.toBeUndefined();
  });

  it("resolves at the matching arrival, not at another", async () => {
    // Arrange
    const messages: SdkMessage[] = [];
    const arrivals = messageArrivals(messages);
    let done = false;
    const waiting = arrivals.until("a result", isResult).then(() => {
      done = true;
    });

    // Act
    messages.push(assistant);
    arrivals.arrived(assistant);
    await Promise.resolve();
    const afterOther = done;
    messages.push(result);
    arrivals.arrived(result);
    await waiting;

    // Assert
    expect([afterOther, done]).toEqual([false, true]);
  });

  it("rejects a pending wait at once when the drive ends first", async () => {
    // Arrange
    const arrivals = messageArrivals([]);
    const waiting = arrivals.until("a result", isResult);

    // Act
    arrivals.ended("it failed: boom");

    // Assert
    await expect(waiting).rejects.toThrow("waiting for a result: the drive ended first (it failed: boom)");
  });

  it("rejects a wait begun after the drive ended", async () => {
    // Arrange
    const arrivals = messageArrivals([]);
    arrivals.ended("its iterable closed");

    // Act / Assert
    await expect(arrivals.until("a result", isResult)).rejects.toThrow(
      "waiting for a result: the drive ended first (its iterable closed)",
    );
  });
});
