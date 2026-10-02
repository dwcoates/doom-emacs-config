/**
 * `catalogs.ts`'s announced-window bookkeeping: which emitted vendor messages
 * move the fake's account-usage answer, and to what figure.
 */
import { describe, expect, it } from "vitest";
import {
  FAKE_DEFAULT_EFFORT,
  fakeAccountUsage,
  fakeAppliedEffort,
  noteAnnouncedWindow,
  type AnnouncedWindows,
} from "../../src/fake/catalogs.js";

const event = (rateLimitType: unknown, utilization: unknown): Record<string, unknown> => ({
  type: "rate_limit_event",
  rate_limit_info: { status: "allowed_warning", rateLimitType, utilization },
});

describe("noteAnnouncedWindow", () => {
  it("files a sampled window's utilization as a percent", () => {
    // Arrange
    const announced: AnnouncedWindows = {};

    // Act
    noteAnnouncedWindow(announced, event("seven_day", 0.91));

    // Assert
    expect(announced).toEqual({ seven_day: 91 });
  });

  it("lets a later event for the same window replace the figure", () => {
    // Arrange
    const announced: AnnouncedWindows = {};
    noteAnnouncedWindow(announced, event("five_hour", 0.82));

    // Act
    noteAnnouncedWindow(announced, event("five_hour", 0.5));

    // Assert
    expect(announced).toEqual({ five_hour: 50 });
  });

  it("ignores a window the usage endpoint does not sample", () => {
    // Arrange
    const announced: AnnouncedWindows = {};

    // Act
    noteAnnouncedWindow(announced, event("overage", 0.79));

    // Assert
    expect(announced).toEqual({});
  });

  it("ignores an event that states no utilization", () => {
    // Arrange
    const announced: AnnouncedWindows = {};

    // Act
    noteAnnouncedWindow(announced, event("seven_day", undefined));

    // Assert
    expect(announced).toEqual({});
  });

  it("ignores every message that is not a rate-limit event", () => {
    // Arrange
    const announced: AnnouncedWindows = {};

    // Act
    noteAnnouncedWindow(announced, { type: "assistant", rate_limit_info: { rateLimitType: "seven_day", utilization: 0.9 } });

    // Assert
    expect(announced).toEqual({});
  });
});

describe("fakeAccountUsage with announced windows", () => {
  it("answers an announced window's figure in place of its own", () => {
    // Arrange + Act
    const usage = fakeAccountUsage("available", 0, { seven_day_opus: 88 });

    // Assert
    expect(usage.rate_limits?.seven_day_opus?.utilization).toBe(88);
  });

  it("answers its own figures when nothing was announced", () => {
    // Arrange + Act
    const usage = fakeAccountUsage("available", 0);

    // Assert
    expect(usage.rate_limits?.seven_day?.utilization).toBe(63);
  });
});

describe("fakeAppliedEffort", () => {
  it.each([
    ["the asked-for level the model accepts", "fake-sonnet-5", "low", "low"],
    ["the default for a level the model lacks", "fake-sonnet-5", "max", FAKE_DEFAULT_EFFORT],
    ["the default when nothing was asked", "fake-opus-4-8", undefined, FAKE_DEFAULT_EFFORT],
    ["no level for a model that takes none", "fake-haiku-4-5", "high", null],
    ["no level for a model the catalog does not carry", "not-a-model", "high", null],
  ] as const)("states %s", (_name, model, asked, want) => {
    // Arrange, Act, Assert
    expect(fakeAppliedEffort(model, asked)).toBe(want);
  });
});
