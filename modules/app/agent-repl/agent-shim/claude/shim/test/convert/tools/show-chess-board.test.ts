/**
 * The agent asking for a chess board. The call carries a CEE session and game,
 * and both settled arms restate them, so each arm and each way the input can
 * fail to name a session gets its own assertion.
 */
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { conversationv1 } from "../../../src/proto.js";
import { showChessBoardConverter } from "../../../src/convert/tools/show-chess-board.js";
import { TOOL_CONVERTERS } from "../../../src/convert/tools/registry.js";
import { dispositionOf } from "../../../src/convert/tool-calls.js";
import { toolResultText } from "../../../src/convert/entries.js";
import type { PendingCall, ToolOutcome } from "../../../src/convert/tool-calls.js";
import { SHOW_CHESS_BOARD_TOOL_NAME } from "../../../src/sdk/agent-repl-mcp.js";
import { logRecordsSince, logSinkMark } from "../../log-records.js";

const INPUT = { session_id: "agent-a", game_id: "g-1" };

function call(input: Record<string, unknown> = INPUT): PendingCall {
  return {
    toolUseId: "toolu_board",
    toolName: SHOW_CHESS_BOARD_TOOL_NAME,
    input,
    startedAtMs: 5_000,
    agentId: create(conversationv1.AgentIdSchema, { value: "main" }),
  };
}

function outcome(isError = false): ToolOutcome {
  return {
    content: toolResultText(isError ? "invalid input: session_id is required" : "shown"),
    isError,
    structured: undefined,
    settledAtMs: 11_000,
  };
}

function armOf(item: conversationv1.AgentActivity["item"] | undefined): conversationv1.AgentChessBoard["result"] {
  expect(item?.case).toBe("chessBoard");
  return (item?.value as conversationv1.AgentChessBoard).result;
}

function sessionPair(session: conversationv1.AgentChessBoardSession | undefined): [string, string] | undefined {
  return session === undefined ? undefined : [session.sessionId, session.gameId];
}

describe("the board tool's registration", () => {
  it("is modelled by the board converter, not the generic MCP card", () => {
    // Arrange, Act.
    const disposition = dispositionOf(TOOL_CONVERTERS, SHOW_CHESS_BOARD_TOOL_NAME);

    // Assert.
    expect(disposition).toEqual({ case: "modelled", converter: showChessBoardConverter });
  });
});

describe("showChessBoardConverter.start", () => {
  it("announces the session and game the call named", () => {
    // Arrange, Act.
    const arm = armOf(showChessBoardConverter.start(call()));

    // Assert.
    expect(sessionPair((arm.value as conversationv1.AgentChessBoardStart).session)).toEqual(["agent-a", "g-1"]);
  });

  it("stamps the instant the call was issued", () => {
    // Arrange, Act.
    const arm = armOf(showChessBoardConverter.start(call()));

    // Assert.
    expect((arm.value as conversationv1.AgentChessBoardStart).startedAt?.atMs).toBe(5_000n);
  });

  it("announces nothing when the input names no session", () => {
    // Arrange, Act, Assert.
    expect(showChessBoardConverter.start(call({ game_id: "g-1" }))).toBeUndefined();
  });

  it("announces nothing when the input names no game", () => {
    // Arrange, Act, Assert.
    expect(showChessBoardConverter.start(call({ session_id: "agent-a", game_id: "" }))).toBeUndefined();
  });
});

describe("showChessBoardConverter.settle", () => {
  it("restates the session and game on success", () => {
    // Arrange, Act.
    const arm = armOf(showChessBoardConverter.settle(call(), outcome()));

    // Assert.
    expect([arm.case, sessionPair((arm.value as conversationv1.AgentChessBoardSuccess).session)]).toEqual([
      "success",
      ["agent-a", "g-1"],
    ]);
  });

  it("stamps the settle instant on success", () => {
    // Arrange, Act.
    const arm = armOf(showChessBoardConverter.settle(call(), outcome()));

    // Assert.
    expect((arm.value as conversationv1.AgentChessBoardSuccess).settledAt?.atMs).toBe(11_000n);
  });

  it("settles a failed call as the failure arm, with what the call said", () => {
    // Arrange, Act.
    const arm = armOf(showChessBoardConverter.settle(call({}), outcome(true)));

    // Assert.
    expect(arm.case).toBe("failure");
  });

  it("leaves the failure's session unset when the input named none", () => {
    // Arrange, Act.
    const arm = armOf(showChessBoardConverter.settle(call({}), outcome(true)));

    // Assert.
    expect((arm.value as conversationv1.AgentChessBoardFailure).session).toBeUndefined();
  });

  it("restates the session on a failure whose input named one", () => {
    // Arrange, Act.
    const arm = armOf(showChessBoardConverter.settle(call(), outcome(true)));

    // Assert.
    expect(sessionPair((arm.value as conversationv1.AgentChessBoardFailure).session)).toEqual(["agent-a", "g-1"]);
  });

  it("produces no frame for a returned call whose input named no session", () => {
    // Arrange, Act, Assert.
    expect(showChessBoardConverter.settle(call({}), outcome())).toBeUndefined();
  });

  it("logs a returned call whose input named no session as an error", () => {
    // Arrange.
    const before = logSinkMark();

    // Act.
    showChessBoardConverter.settle(call({}), outcome());

    // Assert.
    const record = logRecordsSince(before).find((candidate) => candidate.message.includes("named no session"));
    expect([record?.level, record?.context.tool_use_id]).toEqual(["error", "toolu_board"]);
  });
});
