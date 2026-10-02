import { afterEach, describe, expect, it } from "vitest";
import { create } from "@bufbuild/protobuf";
import { WorkspaceRosterSchema } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { resetLoggingForTests } from "../../src/log.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { createSelectionEdge } from "../../src/sidebar/selection-edge.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { roster } from "./harness.js";

/** An edge for the page whose workspace is `own`, counting arrivals. */
function edge(): { arrivals: () => number; observe: (current?: string) => void; reset: () => void } {
  let count = 0;
  const e = createSelectionEdge("own", () => {
    count += 1;
  });
  return {
    arrivals: () => count,
    observe: (current) => e.observe(roster(current === undefined ? {} : { current })),
    reset: () => e.reset(),
  };
}

describe("createSelectionEdge", () => {
  afterEach(() => {
    resetLoggingForTests();
  });

  it("fires when current moves from another workspace to this one", () => {
    // Arrange
    const e = edge();
    e.observe("other");
    // Act
    e.observe("own");
    // Assert
    expect(e.arrivals()).toBe(1);
  });

  it("fires when current moves from none to this one", () => {
    // Arrange
    const e = edge();
    e.observe();
    // Act
    e.observe("own");
    // Assert
    expect(e.arrivals()).toBe(1);
  });

  it("does not fire on the first push, which is a baseline", () => {
    // Arrange
    const e = edge();
    // Act
    e.observe("own");
    // Assert
    expect(e.arrivals()).toBe(0);
  });

  it("does not fire when current restates this workspace", () => {
    // Arrange
    const e = edge();
    e.observe("other");
    e.observe("own");
    // Act
    e.observe("own");
    // Assert
    expect(e.arrivals()).toBe(1);
  });

  it("does not fire when current moves to another workspace", () => {
    // Arrange
    const e = edge();
    e.observe("own");
    // Act
    e.observe("other");
    // Assert
    expect(e.arrivals()).toBe(0);
  });

  it("fires again on a later switch back", () => {
    // Arrange
    const e = edge();
    e.observe("other");
    e.observe("own");
    e.observe("other");
    // Act
    e.observe("own");
    // Assert
    expect(e.arrivals()).toBe(2);
  });

  it("treats the first push after a reset as a baseline", () => {
    // Arrange
    const e = edge();
    e.observe("other");
    e.reset();
    // Act
    e.observe("own");
    // Assert
    expect(e.arrivals()).toBe(0);
  });

  it("records the switch at INFO with where it came from", async () => {
    // Arrange
    const capture = captureLogRecords("info");
    const e = edge();
    e.observe("other");
    // Act
    e.observe("own");
    // Assert
    const record = await forwardedRecord(capture, "sidebar.workspace-selected");
    expect(record.context).toMatchObject({ workspace: "own", from: "other" });
  });

  it("refuses a current that names no workspace", () => {
    // Arrange
    const e = createSelectionEdge("own", () => undefined);
    const malformed = create(WorkspaceRosterSchema, { current: {} });
    // Act / Assert
    expect(() => e.observe(malformed)).toThrow(MalformedView);
  });
});
