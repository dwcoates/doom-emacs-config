/**
 * The lifecycle family: the two query deaths and the keep-alive-shaped turn.
 *
 * The interrupt-driven scenarios (`!hold`, `!interrupt`) are pinned in the
 * ENGINE suite, because what they assert is the engine's interrupt terminal
 * rather than anything the scenario itself says.
 */
import { describe, expect, it } from "vitest";

import { driveScenario, ofType, theResult } from "../harness.js";

describe("query death by EOF", () => {
  it("delivers NO result for the turn", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!query-eof"]);

    // Assert. The turn never terminated; that is the fact under test.
    expect(ofType(driven, "result")).toHaveLength(0);
  });

  it("ends the iterable rather than rejecting it", async () => {
    // Arrange + Act. The drive completes without the collector catching, which
    // it would have done had the iterable rejected.
    const driven = await driveScenario(["!query-eof"]);

    // Assert. Only the init got through.
    expect(driven.messages.map((m) => (m as { type: string }).type)).toEqual(["system"]);
  });

  it("still recorded the prompt before dying", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!query-eof"]);

    // Assert
    expect(driven.transcript().some((l) => l.promptSource === "sdk")).toBe(true);
  });
});

describe("query death by iterator failure", () => {
  it("delivers no result either", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!query-fail"]);

    // Assert
    expect(ofType(driven, "result")).toHaveLength(0);
  });

  it("REJECTS the iterable, which is the opposite fact from a clean EOF", async () => {
    // Arrange
    let rejected = false;

    // Act. Driving it directly, because the harness deliberately swallows the
    // rejection so a suite can still inspect what arrived before the death.
    const { createFakeQuery } = await import("../../../src/fake/index.js");
    const query = createFakeQuery(
      (async function* () {
        yield { type: "user", message: { role: "user", content: "!query-fail" }, parent_tool_use_id: null } as never;
      })(),
      (async (_n: string, input: Record<string, unknown>) => ({ behavior: "allow", updatedInput: input })) as never,
      { sessionId: "s", newUuid: () => `u${Math.random()}`, cwd: "/tmp/fake-lifecycle", configDir: "/tmp/fake-lifecycle-cfg", spoolRoot: "/tmp/fake-lifecycle-spool" },
    );
    try {
      for await (const _ of query) void _;
    } catch {
      rejected = true;
    }

    // Assert
    expect(rejected).toBe(true);
  });
});

describe("the keep-alive-shaped turn", () => {
  it("answers ok and nothing else", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!keepalive"]);

    // Assert
    expect(theResult(driven).result).toBe("ok");
  });

  it("neither adds nor strips the shim's keep-alive marker", async () => {
    // Arrange + Act. The marker is the SHIM's; a mock that touched it would let
    // a broken shim pass.
    const marker = "<!--agent-repl:keepalive-->";
    const driven = await driveScenario([`${marker}!keepalive`]);
    const prompt = driven.transcript().find((l) => l.promptSource === "sdk");

    // Assert
    expect(String((prompt?.message as { content: { text: string }[] }).content[0]?.text)).toContain(marker);
  });
});

describe("a turn the vendor runs on its own", () => {
  it("runs ahead of the next send's own turn", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!queue-vendor-turn", "next send"]);

    // Assert. Three results: the queuing turn, the vendor's own, the next send's.
    expect(ofType(driven, "result").map((line) => line.result)).toEqual([
      "ok",
      "A background task finished.",
      expect.any(String),
    ]);
  });

  it("states where it came from on its result", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!queue-vendor-turn", "next send"]);

    // Assert
    expect(ofType(driven, "result")[1]?.origin).toEqual({ kind: "task-notification" });
  });

  it("names no send anywhere, though the next send carried a client uuid", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!queue-vendor-turn", "next send"], {
      clientUuids: [undefined, "client-send-2"],
    });
    const vendorTurn = driven.messages.slice(
      driven.messages.indexOf(ofType(driven, "result")[0] as never) + 1,
      driven.messages.indexOf(ofType(driven, "result")[1] as never) + 1,
    ) as Record<string, unknown>[];

    // Assert
    expect(vendorTurn.filter((message) => "user_message_uuid" in message)).toEqual([]);
  });

  it("runs only once", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!queue-vendor-turn", "second", "third"]);

    // Assert
    expect(ofType(driven, "result")).toHaveLength(4);
  });
});

describe("query death with a permission ask left open", () => {
  it("opens the ask, then ends the iterable without resolving it itself", async () => {
    // Arrange + Act. The scenario never awaits the ask; only the shim's own
    // stand-down resolves it, so driving it bare is expected to leave the
    // callback pending -- this pins that the STREAM still ends cleanly.
    const driven = await driveScenario(["!query-eof-mid-ask"]);

    // Assert. No result: the query died before a turn could conclude.
    expect(ofType(driven, "result")).toHaveLength(0);
  });

  it("still recorded the tool_use before the death", async () => {
    // Arrange + Act
    const driven = await driveScenario(["!query-eof-mid-ask"]);

    // Assert
    expect(driven.messages.some((m) => (m as { type: string }).type === "assistant")).toBe(true);
  });
});
