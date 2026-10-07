/**
 * agent-repl's own MCP server: the board tool's name, schema and answer. The
 * server is built from injected SDK factories, so the suite asserts exactly
 * what reaches the SDK without the live vendor.
 */
import { describe, expect, it } from "vitest";
import { z } from "zod";
import { logRecordsSince, logSinkMark } from "../log-records.js";
import {
  AGENT_REPL_AUTO_ALLOWED_TOOLS,
  AGENT_REPL_MCP_SERVER,
  SHOW_CHESS_BOARD_DESCRIPTION,
  SHOW_CHESS_BOARD_INPUT,
  SHOW_CHESS_BOARD_TOOL,
  SHOW_CHESS_BOARD_TOOL_NAME,
  agentReplMcpServers,
  showChessBoard,
  type McpServerFactory,
} from "../../src/sdk/agent-repl-mcp.js";

/** A factory that records what the module hands the SDK. */
function recordingFactory(): {
  factory: McpServerFactory;
  servers: { name: string; version?: string; tools?: unknown[] }[];
  tools: { name: string; description: string; inputSchema: unknown; handler: unknown }[];
} {
  const servers: { name: string; version?: string; tools?: unknown[] }[] = [];
  const tools: { name: string; description: string; inputSchema: unknown; handler: unknown }[] = [];
  const factory: McpServerFactory = {
    createSdkMcpServer(options) {
      servers.push(options);
      return { type: "sdk", name: options.name } as never;
    },
    tool(name, description, inputSchema, handler) {
      const definition = { name, description, inputSchema, handler };
      tools.push(definition);
      return definition;
    },
  };
  return { factory, servers, tools };
}

describe("SHOW_CHESS_BOARD_TOOL_NAME", () => {
  it("is the qualified name the vendor gives the server's board tool", () => {
    // Arrange, Act, Assert.
    expect(SHOW_CHESS_BOARD_TOOL_NAME).toBe(`mcp__${AGENT_REPL_MCP_SERVER}__${SHOW_CHESS_BOARD_TOOL}`);
  });
});

describe("AGENT_REPL_AUTO_ALLOWED_TOOLS", () => {
  it("pre-allows the board tool", () => {
    // Arrange, Act, Assert.
    expect(AGENT_REPL_AUTO_ALLOWED_TOOLS).toEqual([SHOW_CHESS_BOARD_TOOL_NAME]);
  });
});

describe("agentReplMcpServers", () => {
  it("registers one server under the agent-repl name", () => {
    // Arrange.
    const { factory } = recordingFactory();

    // Act.
    const servers = agentReplMcpServers(factory);

    // Assert.
    expect(Object.keys(servers)).toEqual([AGENT_REPL_MCP_SERVER]);
  });

  it("names the server agent-repl", () => {
    // Arrange.
    const { factory, servers } = recordingFactory();

    // Act.
    agentReplMcpServers(factory);

    // Assert.
    expect(servers.map((server) => server.name)).toEqual([AGENT_REPL_MCP_SERVER]);
  });

  it("offers the board tool with its description, schema and handler", () => {
    // Arrange.
    const { factory, tools } = recordingFactory();

    // Act.
    agentReplMcpServers(factory);

    // Assert.
    expect(tools).toEqual([
      {
        name: SHOW_CHESS_BOARD_TOOL,
        description: SHOW_CHESS_BOARD_DESCRIPTION,
        inputSchema: SHOW_CHESS_BOARD_INPUT,
        handler: showChessBoard,
      },
    ]);
  });

  it("puts the board tool on the server it registers", () => {
    // Arrange.
    const { factory, servers, tools } = recordingFactory();

    // Act.
    agentReplMcpServers(factory);

    // Assert.
    expect(servers[0]?.tools).toEqual(tools);
  });
});

describe("SHOW_CHESS_BOARD_INPUT", () => {
  const schema = z.object(SHOW_CHESS_BOARD_INPUT);

  it("accepts a session id and a game id", () => {
    // Arrange, Act.
    const parsed = schema.safeParse({ session_id: "agent-a", game_id: "g-1" });

    // Assert.
    expect(parsed.success).toBe(true);
  });

  it("refuses an empty session id", () => {
    // Arrange, Act.
    const parsed = schema.safeParse({ session_id: "", game_id: "g-1" });

    // Assert.
    expect(parsed.success).toBe(false);
  });

  it("refuses a missing game id", () => {
    // Arrange, Act.
    const parsed = schema.safeParse({ session_id: "agent-a" });

    // Assert.
    expect(parsed.success).toBe(false);
  });
});

describe("showChessBoard", () => {
  it("tells the agent the board for its session and game is shown in the feed", async () => {
    // Arrange, Act.
    const result = await showChessBoard({ session_id: "agent-a", game_id: "g-1" });

    // Assert.
    expect(result.content).toEqual([
      {
        type: "text",
        text:
          "The chess board for CEE session agent-a (game g-1) is shown to the reader in the feed. " +
          "If the session or its game no longer exists, the board says so where it is drawn.",
      },
    ]);
  });

  it("logs the request with the session and game it named", async () => {
    // Arrange.
    const before = logSinkMark();

    // Act.
    await showChessBoard({ session_id: "agent-a", game_id: "g-1" });

    // Assert.
    const record = logRecordsSince(before).find((candidate) => candidate.message.includes("asked for a chess board"));
    expect([record?.level, record?.context.cee_session_id, record?.context.cee_game_id]).toEqual([
      "info",
      "agent-a",
      "g-1",
    ]);
  });
});
