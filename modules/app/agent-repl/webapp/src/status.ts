/**
 * The `/status` panel: what the CLI's interactive status command would show,
 * rendered graphically for the GUI that can never open that terminal panel.
 *
 * The data is the SDK's `system:init` snapshot — model, version, cwd, auth
 * source, MCP/plugin/skill rosters, memory paths — which the daemon retains
 * per session and PUSHES as a `frontend.v1` `sessionInit` frame (and inside
 * `StateSnapshot.inits` on attach). The panel therefore reads live state off
 * the push plane; the old `GET /sessions/{id}/status` and its
 * `POST /status/refresh` re-probe are both gone, the latter because there is
 * nothing staler than the last pushed init left to refresh.
 *
 * The one section the init omits is the logged-in account, which the daemon
 * reads fresh from the config dir; that stays an HTTP read on the sanctioned
 * account endpoint.
 *
 * Model and permission mode are NOT read off the snapshot here: the store
 * already tracks them live from their own frames, so the panel is handed the
 * fresher ones and overlays them onto the snapshot.
 */

import { Account } from "./account.js";
import { escapeHtml } from "./highlight.js";

/**
 * The subset of the SDK's `system:init` the panel reads. Every field is
 * optional: the payload is the SDK's to define and grows per release, so a
 * field the panel does not find renders as "unknown" rather than breaking it.
 */
export interface StatusSnapshot {
  cwd?: string;
  apiKeySource?: string;
  claude_code_version?: string;
  output_style?: string;
  fast_mode_state?: string;
  mcp_servers?: unknown[];
  plugins?: Array<{ name?: string; version?: string }>;
  skills?: unknown[];
  agents?: unknown[];
  memory_paths?: Record<string, string>;
}

/** The panel's inputs: the init-derived snapshot plus the account block. */
export interface StatusResponse {
  /** The `system:init` snapshot, or null before any init has landed. */
  snapshot: StatusSnapshot | null;
  /** The logged-in account, read fresh from the session's config dir. */
  account: Account;
}

/** Read one string field, treating an absent or non-string value as unset. */
function str(init: Record<string, unknown>, key: string): string | undefined {
  const v = init[key];
  return typeof v === "string" && v !== "" ? v : undefined;
}

/** Read one repeated field, treating an absent or non-array value as unset. */
function arr(init: Record<string, unknown>, key: string): unknown[] | undefined {
  const v = init[key];
  return Array.isArray(v) ? v : undefined;
}

/**
 * Normalize the `data.v1.ApiKeySource` enum to the bare source word the panel
 * labels. protojson spells an enum as its NAME, so the wire carries
 * `API_KEY_SOURCE_NONE` where the panel wants `none`.
 *
 * `UNSPECIFIED` maps to unset — the daemon does not know the source, which is
 * exactly the case the label already renders as a subscription login.
 */
export function apiKeySourceWord(raw: string | undefined): string | undefined {
  if (raw === undefined) return undefined;
  const prefix = "API_KEY_SOURCE_";
  if (!raw.startsWith(prefix)) return raw;
  const word = raw.slice(prefix.length).toLowerCase();
  return word === "unspecified" ? undefined : word;
}

/**
 * Project a pushed `data.v1.SystemInit` (protojson, lowerCamel field names)
 * onto the panel's snapshot view-model.
 *
 * This replaces the old `GET /sessions/{id}/status`: the daemon retains the
 * session's SystemInit and pushes it as a `sessionInit` frame (and in
 * `StateSnapshot.inits`), so the panel reads live state off the push plane
 * instead of a round trip. It is also why the old `/status/refresh` re-probe
 * is gone — there is nothing staler than the last pushed init to refresh.
 *
 * Read leniently by design: the payload is the SDK's to define and grows per
 * release, so a field that is absent or of an unexpected type renders as
 * "unknown" rather than breaking the panel.
 */
export function statusSnapshotFromInit(
  init: Record<string, unknown> | null,
): StatusSnapshot | null {
  if (init === null) return null;
  const memory = init["memoryPaths"];
  return {
    cwd: str(init, "cwd"),
    apiKeySource: apiKeySourceWord(str(init, "apiKeySource")),
    claude_code_version: str(init, "claudeCodeVersion"),
    output_style: str(init, "outputStyle"),
    fast_mode_state: str(init, "fastModeState"),
    mcp_servers: arr(init, "mcpServers"),
    plugins: arr(init, "plugins") as StatusSnapshot["plugins"],
    skills: arr(init, "skills"),
    agents: arr(init, "agents"),
    memory_paths:
      typeof memory === "object" && memory !== null && !Array.isArray(memory)
        ? (memory as Record<string, string>)
        : undefined,
  };
}

/** Human label for the auth source. `none` means an OAuth/subscription login,
 * not "unauthenticated" — the CLI reports no API-key source in that case. */
export function authLabel(apiKeySource: string | undefined): string {
  if (apiKeySource === undefined || apiKeySource === "" || apiKeySource === "none") {
    return "Claude subscription";
  }
  return apiKeySource;
}

/** Human label for the fast-mode toggle, defaulting an absent value to off. */
export function fastModeLabel(fastModeState: string | undefined): string {
  return fastModeState === "on" ? "on" : "off";
}

/** `name version` for each installed plugin, in report order. */
export function pluginLabels(plugins: StatusSnapshot["plugins"]): string[] {
  if (!plugins) return [];
  return plugins.map((p) => {
    const name = p.name ?? "unknown";
    return p.version !== undefined && p.version !== "" ? `${name} ${p.version}` : name;
  });
}

/** One rendered row of the panel. */
export interface StatusRow {
  label: string;
  value: string;
}

/**
 * The ordered rows the panel renders, merging the snapshot with the live
 * model/permission mode the store holds. `model` and `permissionMode` come
 * from the store rather than the snapshot because a mid-session switch moves
 * them on their own frames, ahead of any status re-probe.
 *
 * A null snapshot yields only the rows that do not depend on it (model,
 * permission mode, account), so the panel is never empty while the first
 * probe is still in flight.
 */
export function statusRows(
  snapshot: StatusSnapshot | null,
  account: Account | null,
  model: string,
  permissionMode: string,
): StatusRow[] {
  const rows: StatusRow[] = [];
  const push = (label: string, value: string | undefined): void => {
    if (value !== undefined && value !== "") rows.push({ label, value });
  };

  push("Account", account === null ? undefined : account.email === "" ? "logged out" : account.email);
  push("Model", model);
  push("Permission mode", permissionMode);

  if (snapshot !== null) {
    push("Version", snapshot.claude_code_version);
    push("Working directory", snapshot.cwd);
    push("Auth", authLabel(snapshot.apiKeySource));
    push("Output style", snapshot.output_style);
    push("Fast mode", fastModeLabel(snapshot.fast_mode_state));
    push("MCP servers", snapshot.mcp_servers ? String(snapshot.mcp_servers.length) : undefined);
    const plugins = pluginLabels(snapshot.plugins);
    push("Plugins", plugins.length > 0 ? plugins.join(", ") : undefined);
    push("Skills", snapshot.skills ? String(snapshot.skills.length) : undefined);
    push("Agents", snapshot.agents ? String(snapshot.agents.length) : undefined);
    const memory = snapshot.memory_paths ? Object.values(snapshot.memory_paths) : [];
    push("Memory", memory.length > 0 ? memory.join(", ") : undefined);
  }
  return rows;
}

/**
 * The `/status` panel, rendering in place of the generic
 * "unsupported command" card once the GUI has replaced that refusal with a
 * real feature.
 *
 * `loading` is true while the first snapshot is still in flight (the GET
 * returned a null snapshot and no `status` frame has landed): the panel then
 * shows the rows it already has (model, mode, account) and a note that the
 * rest is resolving, rather than a bare spinner.
 */
export function statusPanelHtml(args: {
  snapshot: StatusSnapshot | null;
  account: Account | null;
  model: string;
  permissionMode: string;
  loading: boolean;
  error?: string;
}): string {
  const head = `<div class="perm-head">Status <span class="badge ok">/status</span></div>`;
  if (args.error !== undefined && args.error !== "") {
    return `
      <div class="permission resolved status-panel">
        ${head}
        <div class="q-text unsupported-err">Status lookup failed: ${escapeHtml(args.error)}</div>
      </div>`;
  }
  const rows = statusRows(args.snapshot, args.account, args.model, args.permissionMode);
  const rowsHtml = rows
    .map(
      (r) =>
        `<div class="status-row"><span class="status-label">${escapeHtml(
          r.label,
        )}</span><span class="status-value">${escapeHtml(r.value)}</span></div>`,
    )
    .join("");
  const note =
    args.loading && args.snapshot === null
      ? `<div class="q-text status-loading">Resolving the rest of the session status…</div>`
      : "";
  return `
    <div class="permission resolved status-panel">
      ${head}
      <div class="status-grid">${rowsHtml}</div>
      ${note}
    </div>`;
}
