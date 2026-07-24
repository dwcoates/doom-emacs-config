/**
 * The /status panel: the GUI's rich, non-interactive replacement for the CLI
 * status command it can never open. Data is the SDK's system:init snapshot,
 * PUSHED as a frontend.v1 sessionInit frame, plus the account block the init
 * omits, merged with the live model/permission mode the store already tracks.
 */
import { describe, expect, it } from "vitest";

import {
  apiKeySourceWord,
  authLabel,
  fastModeLabel,
  pluginLabels,
  statusPanelHtml,
  statusRows,
  statusSnapshotFromInit,
  StatusSnapshot,
} from "../src/status.js";
import { Account } from "../src/account.js";

const account: Account = { config_dir: "", email: "dodge@chess.com" };

describe("apiKeySourceWord", () => {
  it("reduces the protojson enum name to the bare source word", () => {
    // Arrange + Act
    const word = apiKeySourceWord("API_KEY_SOURCE_PROJECT");
    // Assert
    expect(word).toBe("project");
  });

  it("treats UNSPECIFIED as unset so the label falls to a subscription", () => {
    // Arrange + Act
    const word = apiKeySourceWord("API_KEY_SOURCE_UNSPECIFIED");
    // Assert
    expect(word).toBeUndefined();
  });

  it("passes through a value that is already a bare word", () => {
    // Arrange + Act
    const word = apiKeySourceWord("none");
    // Assert
    expect(word).toBe("none");
  });

  it("leaves an absent source unset", () => {
    // Arrange + Act
    const word = apiKeySourceWord(undefined);
    // Assert
    expect(word).toBeUndefined();
  });
});

describe("statusSnapshotFromInit", () => {
  it("has no snapshot before any init has been pushed", () => {
    // Arrange + Act
    const snap = statusSnapshotFromInit(null);
    // Assert
    expect(snap).toBeNull();
  });

  it("reads the init's lowerCamel protojson field names", () => {
    // Arrange
    const init = { claudeCodeVersion: "1.2.3", fastModeState: "on", outputStyle: "concise" };
    // Act
    const snap = statusSnapshotFromInit(init);
    // Assert
    expect(snap).toMatchObject({
      claude_code_version: "1.2.3",
      fast_mode_state: "on",
      output_style: "concise",
    });
  });

  it("normalizes the auth source enum onto the snapshot", () => {
    // Arrange
    const init = { apiKeySource: "API_KEY_SOURCE_ORG" };
    // Act
    const snap = statusSnapshotFromInit(init);
    // Assert
    expect(snap?.apiKeySource).toBe("org");
  });

  it("carries the repeated rosters through as arrays", () => {
    // Arrange
    const init = { mcpServers: [{ name: "gns" }], skills: ["a", "b"], agents: ["x"] };
    // Act
    const snap = statusSnapshotFromInit(init);
    // Assert
    expect([snap?.mcp_servers?.length, snap?.skills?.length, snap?.agents?.length]).toEqual([
      1, 2, 1,
    ]);
  });

  it("carries the memory-path map through", () => {
    // Arrange
    const init = { memoryPaths: { auto: "/m/CLAUDE.md" } };
    // Act
    const snap = statusSnapshotFromInit(init);
    // Assert
    expect(snap?.memory_paths).toEqual({ auto: "/m/CLAUDE.md" });
  });

  it("leaves a field the init never carried unset rather than guessing", () => {
    // Arrange — an init from a release that reports none of these.
    const init = { cwd: "/w" };
    // Act
    const snap = statusSnapshotFromInit(init);
    // Assert
    expect(snap?.claude_code_version).toBeUndefined();
  });

  it("ignores a field whose type is not the one the panel reads", () => {
    // Arrange — a scalar where the panel expects a roster.
    const init = { skills: "not-an-array" };
    // Act
    const snap = statusSnapshotFromInit(init);
    // Assert
    expect(snap?.skills).toBeUndefined();
  });
});

describe("authLabel", () => {
  it("reads a `none` source as a Claude subscription", () => {
    // Arrange + Act + Assert — `none` means an OAuth login, not unauthenticated.
    expect(authLabel("none")).toBe("Claude subscription");
  });

  it("passes a real API-key source through", () => {
    // Arrange + Act + Assert
    expect(authLabel("ANTHROPIC_API_KEY")).toBe("ANTHROPIC_API_KEY");
  });
});

describe("fastModeLabel", () => {
  it("reports on when the toggle is on", () => {
    // Arrange + Act + Assert
    expect(fastModeLabel("on")).toBe("on");
  });

  it("defaults an absent toggle to off", () => {
    // Arrange + Act + Assert
    expect(fastModeLabel(undefined)).toBe("off");
  });
});

describe("pluginLabels", () => {
  it("pairs each plugin name with its version", () => {
    // Arrange + Act
    const labels = pluginLabels([{ name: "typescript-lsp", version: "1.0.0" }]);
    // Assert
    expect(labels).toEqual(["typescript-lsp 1.0.0"]);
  });

  it("renders a name alone when it has no version", () => {
    // Arrange + Act
    const labels = pluginLabels([{ name: "gns-cowork" }]);
    // Assert
    expect(labels).toEqual(["gns-cowork"]);
  });
});

describe("statusRows", () => {
  it("prefers the live model over the snapshot", () => {
    // Arrange — a mid-session switch moves the store's model ahead of a probe.
    const snapshot: StatusSnapshot = { cwd: "/w" };
    // Act
    const rows = statusRows(snapshot, account, "claude-opus-4-8", "default");
    // Assert
    expect(rows.find((r) => r.label === "Model")?.value).toBe("claude-opus-4-8");
  });

  it("omits the snapshot rows when there is no snapshot yet", () => {
    // Arrange — before the first probe lands, only account/model/mode show.
    // Act
    const rows = statusRows(null, account, "m", "plan");
    // Assert
    expect(rows.map((r) => r.label)).toEqual(["Account", "Model", "Permission mode"]);
  });

  it("labels a logged-out account rather than blanking it", () => {
    // Arrange
    const loggedOut: Account = { config_dir: "", email: "" };
    // Act
    const rows = statusRows(null, loggedOut, "m", "default");
    // Assert
    expect(rows.find((r) => r.label === "Account")?.value).toBe("logged out");
  });

  it("counts the MCP servers", () => {
    // Arrange
    const snapshot: StatusSnapshot = { mcp_servers: [{}, {}] };
    // Act
    const rows = statusRows(snapshot, account, "m", "default");
    // Assert
    expect(rows.find((r) => r.label === "MCP servers")?.value).toBe("2");
  });
});

describe("statusPanelHtml", () => {
  it("renders the rows as label/value cells", () => {
    // Arrange
    const snapshot: StatusSnapshot = { claude_code_version: "2.1.215" };
    // Act
    const html = statusPanelHtml({
      snapshot,
      account,
      model: "m",
      permissionMode: "default",
      loading: false,
    });
    // Assert
    expect(html).toContain("2.1.215");
    expect(html).toContain("status-grid");
  });

  it("shows a loading note while the first snapshot is in flight", () => {
    // Arrange — a null snapshot with loading true is the pre-probe state.
    // Act
    const html = statusPanelHtml({
      snapshot: null,
      account,
      model: "m",
      permissionMode: "default",
      loading: true,
    });
    // Assert
    expect(html).toContain("Resolving the rest of the session status");
  });

  it("renders an error state rather than an empty panel", () => {
    // Arrange + Act
    const html = statusPanelHtml({
      snapshot: null,
      account: null,
      model: "m",
      permissionMode: "default",
      loading: false,
      error: "boom",
    });
    // Assert
    expect(html).toContain("Status lookup failed: boom");
  });

  it("escapes a snapshot value rather than injecting it raw", () => {
    // Arrange — the cwd is host data, so a path with markup must not render live.
    const snapshot: StatusSnapshot = { cwd: "/w/<img>" };
    // Act
    const html = statusPanelHtml({
      snapshot,
      account,
      model: "m",
      permissionMode: "default",
      loading: false,
    });
    // Assert
    expect(html).toContain("&lt;img&gt;");
    expect(html).not.toContain("<img>");
  });
});
