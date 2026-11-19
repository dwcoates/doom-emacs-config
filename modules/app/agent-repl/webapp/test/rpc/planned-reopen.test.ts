/**
 * planned-reopen.test.ts — the error a planned end of a stream is thrown as.
 *
 * What `watchStream` does with it is pinned in streams.test.ts, and who throws
 * it in page-streams.test.ts; this file pins the type itself, which those two
 * recognize it by.
 */
import { describe, expect, it } from "vitest";

import { PlannedReopen } from "../../src/rpc/planned-reopen.js";

describe("PlannedReopen", () => {
  it("is recognized by type as an Error", () => {
    // Arrange, Act
    const end: unknown = new PlannedReopen("the page's attachment is over");

    // Assert
    expect([end instanceof PlannedReopen, end instanceof Error]).toEqual([true, true]);
  });

  it("names itself, so a log line that prints it says which end it was", () => {
    // Arrange, Act
    const end = new PlannedReopen("the page's attachment is over");

    // Assert
    expect([end.name, end.message]).toEqual(["PlannedReopen", "the page's attachment is over"]);
  });
});
