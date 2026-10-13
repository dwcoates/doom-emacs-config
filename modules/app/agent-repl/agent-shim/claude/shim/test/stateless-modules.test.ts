import { afterEach, describe, expect, it, vi } from "vitest";
import * as proto from "../src/proto.js";
import { keepGeneratedProtosAcrossResets } from "./stateless-modules.js";

describe("keepGeneratedProtosAcrossResets", () => {
  afterEach(() => {
    vi.doUnmock("../src/proto.js");
    vi.resetModules();
  });

  it("answers the one evaluated instance after a module reset", async () => {
    // Arrange
    keepGeneratedProtosAcrossResets();

    // Act
    vi.resetModules();
    const reimported = await import("../src/proto.js");

    // Assert
    expect(reimported.shimv1.KillSessionRequestSchema).toBe(proto.shimv1.KillSessionRequestSchema);
  });

  it("still evaluates the rest of the graph afresh", async () => {
    // Arrange
    keepGeneratedProtosAcrossResets();
    vi.resetModules();
    const before = await import("../src/locks.js");

    // Act
    vi.resetModules();
    const after = await import("../src/locks.js");

    // Assert: only the protos are pinned; the isolation a reset buys holds.
    expect(after).not.toBe(before);
  });

  it("leaves an unpinned reset evaluating the protos afresh", async () => {
    // Act
    vi.resetModules();
    const reimported = await import("../src/proto.js");

    // Assert: the pin is opt-in, so the baseline really is a fresh instance.
    expect(reimported.shimv1.KillSessionRequestSchema).not.toBe(proto.shimv1.KillSessionRequestSchema);
  });
});
