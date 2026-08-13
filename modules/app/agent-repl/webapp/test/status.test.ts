/**
 * The /status panel: the GUI's rich, non-interactive replacement for the CLI
 * status command it can never open.
 *
 * THE PANEL DERIVES NOTHING. The daemon resolves and stringifies every row it
 * owns and pushes them on the `sessionInit` frame; the panel prints those
 * verbatim, splicing ahead of them the three rows that come from sources
 * moving independently of an init — the account, the live model, and the live
 * permission mode.
 */
import { describe, expect, it } from "vitest";

import { statusPanelHtml, statusRows, type StatusRow } from "../src/status.js";
import { Account } from "../src/account.js";

const account: Account = { config_dir: "", email: "dodge@chess.com" };

describe("statusRows", () => {
  it("draws only the rows it owns when the daemon has sent none", () => {
    // Arrange — EMPTY rows is the daemon saying no init has landed yet.
    const rows: StatusRow[] = [];
    // Act
    const built = statusRows(rows, account, "m", "plan");
    // Assert — its own three, and nothing standing in for the absent rest.
    expect(built.map((r) => r.label)).toEqual(["Account", "Model", "Permission mode"]);
  });

  it("puts its own rows AHEAD of the daemon's", () => {
    // Arrange
    const rows: StatusRow[] = [{ label: "Version", value: "2.1.215" }];
    // Act
    const built = statusRows(rows, account, "m", "default");
    // Assert
    expect(built.map((r) => r.label)).toEqual([
      "Account",
      "Model",
      "Permission mode",
      "Version",
    ]);
  });

  it("carries a daemon row through verbatim rather than reformatting it", () => {
    // Arrange — the value is a string ON PURPOSE, so nothing here restyles it.
    const rows: StatusRow[] = [{ label: "MCP servers", value: "2" }];
    // Act
    const built = statusRows(rows, account, "m", "default");
    // Assert
    expect(built.at(-1)).toEqual({ label: "MCP servers", value: "2" });
  });

  it("labels a logged-out account rather than blanking it", () => {
    // Arrange
    const loggedOut: Account = { config_dir: "", email: "" };
    // Act
    const built = statusRows([], loggedOut, "m", "default");
    // Assert
    expect(built.find((r) => r.label === "Account")?.value).toBe("logged out");
  });

  it("omits its own row when it has no value for it", () => {
    // Arrange — the account has not loaded, so there is no account row.
    // Act
    const built = statusRows([], null, "m", "default");
    // Assert — absence renders as absence, never a placeholder.
    expect(built.map((r) => r.label)).toEqual(["Model", "Permission mode"]);
  });
});

describe("statusPanelHtml", () => {
  it("renders a daemon row as label and value cells", () => {
    // Arrange
    const rows: StatusRow[] = [{ label: "Version", value: "2.1.215" }];
    // Act
    const html = statusPanelHtml({ rows, account, model: "m", permissionMode: "default" });
    // Assert
    expect(html).toContain(
      `<span class="status-label">Version</span><span class="status-value">2.1.215</span>`,
    );
  });

  it("draws no rows beyond its own when the daemon has sent none", () => {
    // Arrange / Act
    const html = statusPanelHtml({ rows: [], account, model: "m", permissionMode: "default" });
    // Assert — exactly the three the panel owns; no spinner, no hole.
    expect(html.match(/class="status-row"/g)).toHaveLength(3);
  });

  it("escapes a daemon row's value rather than injecting it raw", () => {
    // Arrange — a value is host data, so markup in it must not render live.
    const rows: StatusRow[] = [{ label: "Working directory", value: "/w/<img>" }];
    // Act
    const html = statusPanelHtml({ rows, account, model: "m", permissionMode: "default" });
    // Assert
    expect(html).toContain("&lt;img&gt;");
    expect(html).not.toContain("<img>");
  });

  it("escapes a daemon row's label too", () => {
    // Arrange
    const rows: StatusRow[] = [{ label: "<b>Auth</b>", value: "Claude subscription" }];
    // Act
    const html = statusPanelHtml({ rows, account, model: "m", permissionMode: "default" });
    // Assert
    expect(html).toContain("&lt;b&gt;Auth&lt;/b&gt;");
  });

  it("renders an error state rather than an empty panel", () => {
    // Arrange / Act
    const html = statusPanelHtml({
      rows: [],
      account: null,
      model: "m",
      permissionMode: "default",
      error: "boom",
    });
    // Assert
    expect(html).toContain("Status lookup failed: boom");
  });
});
