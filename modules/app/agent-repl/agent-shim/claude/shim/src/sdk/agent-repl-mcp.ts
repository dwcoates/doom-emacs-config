/**
 * sdk/agent-repl-mcp.ts — agent-repl's OWN MCP server, hosted in the shim's
 * process and handed to every real session.
 *
 * It offers the agent the tools whose calls agent-repl itself draws: today one,
 * `show_chess_board`. A call is an ordinary tool call in the conversation
 * record, converted into its own `AgentActivity` arm
 * (convert/tools/show-chess-board.ts), so what the tool causes — a board in the
 * feed — is part of the conversation: ordered where the agent asked for it,
 * replayed with every page, and durable across daemon restarts.
 *
 * THE HANDLER DOES NOTHING BUT ANSWER. agent-repl reads no chess data, and the
 * board is drawn by the daemon from the call itself, so the handler only tells
 * the agent that the board is shown. The input is validated by the schema the
 * SDK enforces before the handler runs.
 *
 * THE TOOL IS PRE-ALLOWED ({@link AGENT_REPL_AUTO_ALLOWED_TOOLS}): it changes
 * nothing on the user's machine, so a permission prompt for it would only
 * interrupt the turn.
 */
import type { McpServerConfig } from "@anthropic-ai/claude-agent-sdk";
import { z } from "zod";
import { bindLog } from "../log.js";

const LOGGER = bindLog({ component: "shim-agent-repl-mcp", operation: "shim.sdk.agent_repl_mcp" });

/** The server's name, the middle segment of every one of its qualified tool names. */
export const AGENT_REPL_MCP_SERVER = "agent-repl";

/** The board tool's own name on the server. */
export const SHOW_CHESS_BOARD_TOOL = "show_chess_board";

/** The board tool as the agent and the transcript name it. */
export const SHOW_CHESS_BOARD_TOOL_NAME = `mcp__${AGENT_REPL_MCP_SERVER}__${SHOW_CHESS_BOARD_TOOL}`;

/** The server's tools a session runs without a permission prompt. */
export const AGENT_REPL_AUTO_ALLOWED_TOOLS: readonly string[] = [SHOW_CHESS_BOARD_TOOL_NAME];

/** What the agent is told the board tool does. */
export const SHOW_CHESS_BOARD_DESCRIPTION =
  "Show the reader an interactive chess board in the agent-repl feed, for the game loaded in a " +
  "CEE CLI session. The board is the CEE CLI webapp's own widget: the reader can step through " +
  "the game, read the engine's lines, and click pieces for the engine's account of a square. " +
  "Name the session by its id (`gns cee session list`) and the game loaded in it by its game " +
  "id (the session's poll reports it); the show-chess-game skill says how to find both. " +
  "Nothing about the game is passed here: the board is resolved from the session when it is drawn.";

/** The board tool's input: the CEE session and the game loaded in it. */
export const SHOW_CHESS_BOARD_INPUT = {
  session_id: z
    .string()
    .min(1)
    .describe("The CEE CLI daemon's session id, as `gns cee session list` names it."),
  game_id: z
    .string()
    .min(1)
    .describe("The id of the game loaded in that session, as the session's poll reports it."),
};

/** The board tool's parsed input. */
export interface ShowChessBoardArgs {
  readonly session_id: string;
  readonly game_id: string;
}

/**
 * What a tool handler answers, in the MCP result shape the SDK forwards. The
 * index signature is the MCP result type's own: a result may carry fields
 * beyond `content`.
 */
export interface AgentReplToolResult {
  [field: string]: unknown;
  content: { type: "text"; text: string }[];
}

/** The board tool's handler: the board is drawn from the call, so it only answers. */
export async function showChessBoard(args: ShowChessBoardArgs): Promise<AgentReplToolResult> {
  LOGGER.info(
    { cee_session_id: args.session_id, cee_game_id: args.game_id },
    "the agent asked for a chess board; the daemon draws it from the call",
  );
  return {
    content: [
      {
        type: "text",
        text:
          `The chess board for CEE session ${args.session_id} (game ${args.game_id}) is shown ` +
          "to the reader in the feed. If the session or its game no longer exists, the board " +
          "says so where it is drawn.",
      },
    ],
  };
}

/** The slice of the SDK this module builds its server with. */
export interface McpServerFactory {
  createSdkMcpServer(options: {
    name: string;
    version?: string;
    tools?: unknown[];
  }): McpServerConfig;
  tool(
    name: string,
    description: string,
    inputSchema: typeof SHOW_CHESS_BOARD_INPUT,
    handler: (args: ShowChessBoardArgs, extra: unknown) => Promise<AgentReplToolResult>,
  ): unknown;
}

/**
 * agent-repl's MCP servers, keyed by the name a session registers each under.
 * Built from the live SDK's own factories, which is why the SDK is passed in.
 */
export function agentReplMcpServers(sdk: McpServerFactory): Record<string, McpServerConfig> {
  return {
    [AGENT_REPL_MCP_SERVER]: sdk.createSdkMcpServer({
      name: AGENT_REPL_MCP_SERVER,
      version: "1.0.0",
      tools: [
        sdk.tool(SHOW_CHESS_BOARD_TOOL, SHOW_CHESS_BOARD_DESCRIPTION, SHOW_CHESS_BOARD_INPUT, showChessBoard),
      ],
    }),
  };
}
