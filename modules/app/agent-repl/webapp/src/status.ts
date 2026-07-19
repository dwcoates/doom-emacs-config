/**
 * The `/status` panel: what the CLI's interactive status command would show,
 * rendered graphically for the GUI that can never open that terminal panel.
 *
 * The data is the SDK's `system:init` snapshot — model, version, cwd, auth
 * source, MCP/plugin/skill rosters, memory paths — which the daemon caches
 * from the live init and re-reads on demand off a throwaway `refresh-status`
 * probe (so a mid-session `/fast` toggle or config edit is reflected, not the
 * value frozen at session start). The one section the init omits is the
 * logged-in account, which the daemon reads fresh from the config dir and
 * bundles into the same GET.
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

/** GET /sessions/{id}/status: the cached snapshot plus the account block. */
export interface StatusResponse {
  /** The `system:init` snapshot, or null before any init has landed. */
  snapshot: StatusSnapshot | null;
  /** The logged-in account, read fresh from the session's config dir. */
  account: Account;
}

/**
 * Fetch the session's `/status` snapshot and account.
 *
 * Rejects on any non-2xx. A session whose init has not landed is NOT an
 * error — it returns 200 with a null snapshot, which the panel renders as a
 * loading state until the refresh's push frame arrives.
 */
export async function fetchStatus(
  httpBase: string,
  sessionId: string,
  fetchFn: typeof fetch = fetch,
): Promise<StatusResponse> {
  const resp = await fetchFn(`${httpBase}/sessions/${sessionId}/status`);
  if (!resp.ok) {
    throw new Error(
      `GET /sessions/${sessionId}/status: ${resp.status} ${await resp.text()}`,
    );
  }
  return (await resp.json()) as StatusResponse;
}

/**
 * Ask the daemon to re-probe the snapshot. Fire-and-forget: the daemon
 * answers 202 immediately and the fresh snapshot arrives later as a `status`
 * frame, which the store adopts and the open panel re-renders on.
 */
export async function refreshStatus(
  httpBase: string,
  sessionId: string,
  fetchFn: typeof fetch = fetch,
): Promise<void> {
  const resp = await fetchFn(`${httpBase}/sessions/${sessionId}/status/refresh`, {
    method: "POST",
  });
  if (!resp.ok) {
    throw new Error(
      `POST /sessions/${sessionId}/status/refresh: ${resp.status} ${await resp.text()}`,
    );
  }
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
