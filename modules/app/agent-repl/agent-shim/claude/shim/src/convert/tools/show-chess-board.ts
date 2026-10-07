/**
 * convert/tools/show-chess-board.ts — the agent asking agent-repl to show the
 * reader a chess board (sdk/agent-repl-mcp.ts's `show_chess_board`).
 *
 * The call names a CEE CLI session and the game loaded in it, and nothing else:
 * the daemon draws the board from that pair. Both settled arms restate the pair
 * the start carried, so a settled frame alone draws the board.
 *
 * A CALL WHOSE INPUT NAMES NO SESSION has nothing to announce, so it writes no
 * start; the SDK refuses such input before the handler runs, so its result is
 * an error and settles as the failure arm, with no session.
 */
import { create } from "@bufbuild/protobuf";
import { bindLog } from "../../log.js";
import { conversationv1 } from "../../proto.js";
import { settledAt, startedAt } from "../entries.js";
import type { PendingCall, ToolConverter, ToolOutcome } from "../tool-calls.js";
import { failureOf, str } from "./support.js";

const LOGGER = bindLog({
  component: "shim-convert-show-chess-board",
  operation: "shim.convert.show_chess_board",
});

/** One board item, whatever arm it carries. */
function boardItem(result: conversationv1.AgentChessBoard["result"]): conversationv1.AgentActivity["item"] {
  return {
    case: "chessBoard",
    value: create(conversationv1.AgentChessBoardSchema, { result }),
  };
}

/** The session the call named, or `undefined` when its input names none. */
function sessionOf(call: PendingCall): conversationv1.AgentChessBoardSession | undefined {
  const sessionId = str(call.input, "session_id");
  const gameId = str(call.input, "game_id");
  if (sessionId === undefined || sessionId === "" || gameId === undefined || gameId === "") return undefined;
  return create(conversationv1.AgentChessBoardSessionSchema, { sessionId, gameId });
}

/** The `show_chess_board` tool: a board the daemon draws from the session it names. */
export const showChessBoardConverter: ToolConverter = {
  kind: "chess_board",
  // `AgentChessBoard` declares no progress arm.
  carriesProgress: false,

  start(call) {
    const session = sessionOf(call);
    if (session === undefined) {
      LOGGER.logVerbose(
        { tool_use_id: call.toolUseId },
        "a board request named no session; it announces nothing and settles as a failure",
      );
      return undefined;
    }
    return boardItem({
      case: "start",
      value: create(conversationv1.AgentChessBoardStartSchema, {
        session,
        startedAt: startedAt(call.startedAtMs),
      }),
    });
  },

  settle(call: PendingCall, outcome: ToolOutcome) {
    const session = sessionOf(call);
    if (outcome.isError) {
      LOGGER.logVerbose({ tool_use_id: call.toolUseId }, "a board request failed");
      return boardItem({
        case: "failure",
        value: create(conversationv1.AgentChessBoardFailureSchema, {
          error: failureOf(call, outcome),
          session,
        }),
      });
    }
    if (session === undefined) {
      // The SDK validates the input before the handler runs, so a returned
      // call always named its session; one that did not is a vendor change.
      LOGGER.error(
        { tool_use_id: call.toolUseId },
        "a board request returned although its input named no session; no frame is produced",
      );
      return undefined;
    }
    return boardItem({
      case: "success",
      value: create(conversationv1.AgentChessBoardSuccessSchema, {
        session,
        settledAt: settledAt(outcome.settledAtMs, call.startedAtMs),
      }),
    });
  },
};
