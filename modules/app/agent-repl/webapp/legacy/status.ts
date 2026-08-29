/**
 * The `/status` panel: what the CLI's interactive status command would show,
 * rendered graphically for the GUI that can never open that terminal panel.
 *
 * THE PANEL DERIVES NOTHING. The daemon resolves the panel's rows — label and
 * stringified value, in render order — and pushes them on
 * `frontend.v1.SessionInitView.rows`; this module prints them verbatim
 * (escaped) and nothing else.
 *
 * What that replaced: a `data.v1.SystemInit` carried whole to this end, out of
 * which every value the user actually saw was computed here — the auth word
 * from an enum name, the fast-mode word from a state string, plugin labels
 * from name+version pairs, three rows from `.length` of an array, and the
 * memory row from joining a map's values. Five derivations in the renderer is
 * five places the panel's answer can disagree with the daemon's. The vendor
 * payload is RESERVED on the wire now, name and number, so there is nothing
 * left here to derive from.
 *
 * EMPTY ROWS MEANS NO INIT HAS LANDED YET, and the panel then draws the rows
 * it owns and nothing more — absence rendering absence, never a spinner and
 * never a placeholder value.
 *
 * The panel splices its OWN three rows ahead of the daemon's: the account (read
 * fresh from the session's config dir on the sanctioned account endpoint), the
 * model, and the permission mode. Those come from sources that move
 * independently of an init and are already resolved elsewhere, so the store
 * holds the fresher ones.
 */

import { Account } from "./account.js";
import { escapeHtml } from "./highlight.js";

/** The panel's HTTP-sourced input: the account block, and only that. */
export interface StatusResponse {
  /** The logged-in account, read fresh from the session's config dir. */
  account: Account;
}

/**
 * One rendered row of the panel: a label and the value beside it.
 *
 * Structurally the daemon's `frontend.v1.SessionInitRow` — a STRING value on
 * purpose, so no renderer reformats what the daemon decided it says.
 */
export interface StatusRow {
  label: string;
  value: string;
}

/**
 * The ordered rows the panel renders: the panel's own three, then the daemon's
 * verbatim.
 *
 * The panel's own rows lead because they come from sources that move
 * independently of an init — a mid-session model or permission-mode switch
 * arrives on its own frame — and the account is not part of an init at all.
 *
 * A row the panel owns but has no value for is OMITTED, the same rule the
 * daemon follows for its own: absence renders as absence rather than as a
 * blank or an "unknown".
 */
export function statusRows(
  rows: readonly StatusRow[],
  account: Account | null,
  model: string,
  permissionMode: string,
): StatusRow[] {
  const own: StatusRow[] = [];
  const push = (label: string, value: string | undefined): void => {
    if (value !== undefined && value !== "") own.push({ label, value });
  };

  push("Account", account === null ? undefined : account.email === "" ? "logged out" : account.email);
  push("Model", model);
  push("Permission mode", permissionMode);

  return [...own, ...rows];
}

/**
 * The `/status` panel, rendering in place of the generic
 * "unsupported command" card once the GUI has replaced that refusal with a
 * real feature.
 *
 * There is NO loading state: an empty `rows` is the daemon saying no init has
 * landed, which the panel draws as the rows it owns and nothing more.
 */
export function statusPanelHtml(args: {
  rows: readonly StatusRow[];
  account: Account | null;
  model: string;
  permissionMode: string;
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
  const rowsHtml = statusRows(args.rows, args.account, args.model, args.permissionMode)
    .map(
      (r) =>
        `<div class="status-row"><span class="status-label">${escapeHtml(
          r.label,
        )}</span><span class="status-value">${escapeHtml(r.value)}</span></div>`,
    )
    .join("");
  return `
    <div class="permission resolved status-panel">
      ${head}
      <div class="status-grid">${rowsHtml}</div>
    </div>`;
}
