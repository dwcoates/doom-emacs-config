/**
 * The Remote Control family: what reaches a bridged session from claude.ai
 * past the shim. The bridge is turned on through the query's own
 * `enableRemoteControl`, as the shim does, before the scenario runs.
 */
import { describe, expect, it } from "vitest";

import type { FakeQuery } from "../../../src/fake/index.js";
import type { CanUseToolLike, QueryLike } from "../../../src/sdk/types.js";
import { driveScenario, expectDroveCleanly, ofType, type Line, type PromptFeeder } from "../harness.js";

/** Let the mock run until it has emitted `count` results. */
async function untilResults(messages: Line[], count: number): Promise<void> {
  for (let tick = 0; tick < 2_000; tick++) {
    if (messages.filter((message) => message.type === "result").length >= count) return;
    await new Promise((resolve) => setImmediate(resolve));
  }
  throw new Error(`the mock emitted fewer than ${count} results`);
}

/** Drive `prompt` with the bridge on (or off), waiting for `results` results. */
function drive(prompt: string, options: { bridge: boolean; results: number; canUseTool?: CanUseToolLike }) {
  return driveScenario([], {
    opts: { replayUserMessages: true },
    ...(options.canUseTool === undefined ? {} : { canUseTool: options.canUseTool }),
    during: async (query: QueryLike, feeder: PromptFeeder, messages: Line[]) => {
      if (options.bridge) await query.enableRemoteControl(true, "parser-fix", { keepSessionOnExit: true });
      feeder.push(prompt);
      await untilResults(messages, options.results);
    },
  });
}

/** The echoes the drive emitted, as uuid and text. */
const echoes = (lines: Line[]): { uuid: unknown; text: unknown; origin: unknown }[] =>
  lines
    .filter((line) => line.type === "user" && line.isReplay === true)
    .map((line) => ({ uuid: line.uuid, text: (line.message as { content?: unknown }).content, origin: line.origin }));

describe("!remote-prompt", () => {
  it("runs the prompt typed on claude.ai as a turn of its own after this one", async () => {
    // Arrange, Act
    const driven = expectDroveCleanly(await drive("!remote-prompt run the tests", { bridge: true, results: 2 }));

    // Assert
    expect(ofType(driven, "result")).toHaveLength(2);
  });

  it("echoes the remote prompt's words under a uuid the host never sent, of human origin", async () => {
    // Arrange, Act
    const driven = expectDroveCleanly(await drive("!remote-prompt run the tests", { bridge: true, results: 2 }));

    // Assert
    expect(echoes(driven.messages as unknown as Line[]).at(-1)).toEqual(
      expect.objectContaining({ text: "run the tests", origin: { kind: "human" } }),
    );
  });

  it("stamps the remote turn's result with the remote prompt's uuid", async () => {
    // Arrange, Act
    const driven = expectDroveCleanly(await drive("!remote-prompt run the tests", { bridge: true, results: 2 }));

    // Assert
    const remote = echoes(driven.messages).at(-1)?.uuid;
    expect(ofType(driven, "result").at(-1)?.user_message_uuid).toBe(remote);
  });

  it("types a default prompt when it names none", async () => {
    // Arrange, Act
    const driven = expectDroveCleanly(await drive("!remote-prompt", { bridge: true, results: 2 }));

    // Assert
    expect(echoes(driven.messages as unknown as Line[]).at(-1)?.text).toBe("What changed since I left?");
  });

  it("runs nothing more, and says so, while the bridge is off", async () => {
    // Arrange, Act
    const driven = expectDroveCleanly(await drive("!remote-prompt run the tests", { bridge: false, results: 1 }));

    // Assert
    expect(ofType(driven, "result").map((result) => result.result)).toEqual([
      "Remote Control is off; nothing came from claude.ai.",
    ]);
  });
});

describe("!remote-interrupt", () => {
  it("ends its turn interrupted, inside the vendor, with no interrupt() asked of it", async () => {
    // Arrange, Act
    const driven = expectDroveCleanly(await drive("!remote-interrupt", { bridge: true, results: 1 }));

    // Assert
    expect(ofType(driven, "result")[0]?.terminal_reason).toBe("aborted_streaming");
  });
});

describe("!perm-answered-remotely", () => {
  /** A gate that records whether the vendor withdrew its ask, and never answers it. */
  function withdrawalSpy(): { canUseTool: CanUseToolLike; withdrawn: boolean[] } {
    const withdrawn: boolean[] = [];
    const canUseTool: CanUseToolLike = (_name, _input, options) =>
      new Promise((resolve) => {
        options.signal.addEventListener("abort", () => {
          withdrawn.push(true);
          resolve({ behavior: "deny", message: "withdrawn" });
        });
      });
    return { canUseTool, withdrawn };
  }

  it("withdraws its ask by aborting the signal handed to canUseTool", async () => {
    // Arrange
    const spy = withdrawalSpy();

    // Act
    await drive("!perm-answered-remotely", { bridge: true, results: 1, canUseTool: spy.canUseTool });

    // Assert
    expect(spy.withdrawn).toEqual([true]);
  });

  it("runs the gated call as claude.ai allowed it, with a result that is no error", async () => {
    // Arrange
    const spy = withdrawalSpy();

    // Act
    const driven = await drive("!perm-answered-remotely", { bridge: true, results: 1, canUseTool: spy.canUseTool });

    // Assert
    expect([ofType(driven, "result")[0]?.subtype, ofType(driven, "result")[0]?.result]).toEqual([
      "success",
      "Pushed, as allowed on claude.ai.",
    ]);
  });
});

describe("!perm-remote-interrupt", () => {
  it("withdraws its ask and ends the turn by a user stop", async () => {
    // Arrange
    const withdrawn: boolean[] = [];
    const canUseTool: CanUseToolLike = (_name, _input, options) =>
      new Promise((resolve) => {
        options.signal.addEventListener("abort", () => {
          withdrawn.push(true);
          resolve({ behavior: "deny", message: "withdrawn" });
        });
      });

    // Act
    const driven = await drive("!perm-remote-interrupt", { bridge: true, results: 1, canUseTool });

    // Assert
    expect([withdrawn, ofType(driven, "result")[0]?.terminal_reason]).toEqual([[true], "aborted_tools"]);
  });
});

describe("injectRemotePrompt, from a test", () => {
  it("is offered by the mocked vendor's query", async () => {
    // Arrange
    let uuid: string | undefined;

    // Act
    await driveScenario([], {
      during: async (query, _feeder, messages) => {
        await query.enableRemoteControl(true, "parser-fix");
        uuid = (query as FakeQuery).injectRemotePrompt("hello");
        await untilResults(messages, 1);
      },
    });

    // Assert
    expect(typeof uuid).toBe("string");
  });
});
