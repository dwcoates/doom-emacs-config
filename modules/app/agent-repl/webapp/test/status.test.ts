/**
 * The /status panel: the GUI's rich, non-interactive replacement for the CLI
 * status command it can never open. Data is the SDK's system:init snapshot
 * (re-probed on demand) plus the account block the init omits, merged with the
 * live model/permission mode the store already tracks.
 */
import { describe, expect, it } from "vitest";

import {
  authLabel,
  fastModeLabel,
  fetchStatus,
  pluginLabels,
  refreshStatus,
  statusPanelHtml,
  statusRows,
  StatusSnapshot,
} from "../src/status.js";
import { Account } from "../src/account.js";

/** A fetch capturing the URL/method and answering with the given response. */
function fakeFetch(resp: { ok: boolean; status?: number; body?: unknown }): {
  fetchFn: typeof fetch;
  calls: Array<{ url: string; method: string }>;
} {
  const calls: Array<{ url: string; method: string }> = [];
  const fetchFn = (async (url: string, init?: { method?: string }) => {
    calls.push({ url, method: init?.method ?? "GET" });
    return {
      ok: resp.ok,
      status: resp.status ?? 200,
      json: async () => resp.body ?? { snapshot: null, account: { config_dir: "", email: "" } },
      text: async () => "boom",
    };
  }) as unknown as typeof fetch;
  return { fetchFn, calls };
}

const account: Account = { config_dir: "", email: "dodge@chess.com" };

describe("fetchStatus", () => {
  it("gets the session's own status route", async () => {
    // Arrange
    const { fetchFn, calls } = fakeFetch({ ok: true });
    // Act
    await fetchStatus("http://d", "s_1", fetchFn);
    // Assert
    expect(calls[0].url).toBe("http://d/sessions/s_1/status");
  });

  it("returns the snapshot and account together", async () => {
    // Arrange
    const { fetchFn } = fakeFetch({
      ok: true,
      body: { snapshot: { fast_mode_state: "on" }, account },
    });
    // Act
    const res = await fetchStatus("http://d", "s_1", fetchFn);
    // Assert
    expect(res.snapshot?.fast_mode_state).toBe("on");
    expect(res.account.email).toBe("dodge@chess.com");
  });

  it("rejects on a non-2xx", async () => {
    // Arrange
    const { fetchFn } = fakeFetch({ ok: false, status: 500 });
    // Act + Assert
    await expect(fetchStatus("http://d", "s_1", fetchFn)).rejects.toThrow(/500/);
  });
});

describe("refreshStatus", () => {
  it("posts the session's status refresh route", async () => {
    // Arrange
    const { fetchFn, calls } = fakeFetch({ ok: true, status: 202 });
    // Act
    await refreshStatus("http://d", "s_1", fetchFn);
    // Assert
    expect(calls[0]).toEqual({ url: "http://d/sessions/s_1/status/refresh", method: "POST" });
  });

  it("rejects on a non-2xx", async () => {
    // Arrange
    const { fetchFn } = fakeFetch({ ok: false, status: 409 });
    // Act + Assert
    await expect(refreshStatus("http://d", "s_1", fetchFn)).rejects.toThrow(/409/);
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
