/**
 * The stand-in engine main.ts wires before the real one exists.
 *
 * WHAT THIS GUARDS: that a shim which has bound its socket answers every verb
 * HONESTLY. The failure mode being excluded is an engine that answers an empty
 * success — the daemon would believe it had a session, start a turn against
 * nothing, and the defect would surface as missing conversation rather than as
 * a refused call.
 */
import { Code, ConnectError } from "@connectrpc/connect";
import { describe, expect, it } from "vitest";
import { NotImplementedEngine, type Engine } from "../../src/engine/engine.js";

const UNARY_VERBS = [
  "startSession",
  "setSessionModel",
  "setSessionPermissionMode",
  "hibernate",
  "killSession",
  "startTurn",
  "updateAgent",
  "killTurn",
  "stopBash",
  "detachForeground",
  "readHistory",
  "gatherTitleDigest",
] as const;

const STREAM_VERBS = ["watchSession", "watchAgent", "watchBash"] as const;

describe("NotImplementedEngine", () => {
  it.each(UNARY_VERBS)("refuses %s with Unimplemented rather than an empty success", async (verb) => {
    // Arrange.
    const engine: Engine = new NotImplementedEngine();

    // Act.
    const rejection = await engine[verb](undefined as never).then(
      () => null,
      (err: unknown) => ConnectError.from(err),
    );

    // Assert.
    expect(rejection?.code).toBe(Code.Unimplemented);
  });

  it.each(STREAM_VERBS)("refuses the %s open at the transport", async (verb) => {
    // Arrange.
    const engine: Engine = new NotImplementedEngine();

    // Act.
    const rejection = await (async (): Promise<ConnectError | null> => {
      try {
        for await (const _frame of engine[verb](undefined as never)) {
          return null;
        }
        return null;
      } catch (err) {
        return ConnectError.from(err);
      }
    })();

    // Assert.
    expect(rejection?.code).toBe(Code.Unimplemented);
  });

  it("names the rpc it refused so a log says which verb was called", async () => {
    // Arrange.
    const engine: Engine = new NotImplementedEngine();

    // Act.
    const rejection = await engine.startTurn(undefined as never).then(
      () => null,
      (err: unknown) => ConnectError.from(err),
    );

    // Assert.
    expect(rejection?.message).toContain("shim.v1.StartTurn");
  });

  it("stands down cleanly so a shim told to stop before it started exits 0", async () => {
    // Arrange.
    const engine: Engine = new NotImplementedEngine();

    // Act, Assert.
    await expect(engine.standDown("SIGTERM")).resolves.toBe(0);
  });
});
