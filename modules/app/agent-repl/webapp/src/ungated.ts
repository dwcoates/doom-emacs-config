/**
 * The UNGATED-SESSION indicator: the one place the webapp says out loud that
 * this session is running with NO permission gate at all.
 *
 * WHY THIS EXISTS (verified against @anthropic-ai/claude-agent-sdk 0.3.220,
 * empirically, not from the docs alone):
 *
 *   Under `permissionMode: "bypassPermissions"` the SDK auto-approves EVERY
 *   tool call BEFORE `canUseTool` is consulted. The callback is never invoked.
 *   The SDK itself says so at query construction, via a Node process warning
 *   coded `CLAUDE_SDK_CAN_USE_TOOL_SHADOWED`: "canUseTool will not be invoked:
 *   permissionMode 'bypassPermissions' auto-approves every tool call (except
 *   explicit deny rules) before the callback is consulted."
 *
 *   `canUseTool` is the shim's ONLY hook into tool approval, and the whole
 *   daemon permission round-trip (PermissionRequest -> the permission card ->
 *   PermissionResponse, plus the pending-permission accounting) hangs off it.
 *   So in this mode there is no daemon-side gate whatsoever: not a permissive
 *   gate, not an auto-answering gate — none. A deny-everything callback was
 *   run against a real session and the tool still executed.
 *
 *   Every other mode leaves the callback in the loop for the ASK path:
 *   `default`/`acceptEdits`/`plan`/`auto` all reach it, and `dontAsk`
 *   short-circuits only in the DENY direction (fail-closed, so no privilege is
 *   granted behind the gate's back). `bypassPermissions` is the sole
 *   fail-OPEN mode, which is why it is the sole member of
 *   {@link UNGATED_PERMISSION_MODES}.
 *
 * WHAT THIS IS NOT: it is not a gate, and nothing here re-checks a tool. The
 * SDK has already approved the call by the time anything in this process could
 * look at it, so a client-side "gate" would be theater. The honest response to
 * an ungated session is to make the ungating IMPOSSIBLE TO MISS, which is what
 * this module does and all it does.
 */

/**
 * The permission modes under which no daemon-side gate can engage in the
 * fail-OPEN direction — i.e. tools RUN without any approval round-trip.
 *
 * `dontAsk` is deliberately absent: it also bypasses `canUseTool`, but it
 * bypasses it by DENYING, so an ungated-privilege warning would be a lie.
 */
export const UNGATED_PERMISSION_MODES: readonly string[] = ["bypassPermissions"];

/** Whether MODE names a session running with no permission gate. */
export function isUngatedMode(mode: string): boolean {
  return UNGATED_PERMISSION_MODES.includes(mode);
}

/**
 * The session's ungated verdict, read from BOTH mode sources the store holds.
 *
 * - `requested` is `SessionView.permission_mode`, which the daemon fills from
 *   the registry record — the mode the session was LAUNCHED with.
 * - `effective` is `SystemInit.permissionMode`, the mode the CLI itself
 *   reports back in its init handshake.
 *
 * The two can disagree. A requested mode the CLI declines is downgraded
 * silently (a session asked for `auto` was observed reporting `default`), and
 * conversely a `permissions.defaultMode` in the user's own settings.json can
 * escalate a session the daemon believes is plain `default` — the shim loads
 * `settingSources: ["user", "project", "local"]`, so settings-borne modes are
 * in play and the registry never sees them.
 *
 * Either source claiming `bypassPermissions` therefore means ungated. This is
 * an OR and not a precedence rule on purpose: the failure that matters is
 * showing a gate that is not there, so the verdict never lets one source's
 * silence suppress the other's warning.
 */
export function ungatedModeOf(args: {
  requestedMode: string;
  systemInit: Record<string, unknown> | null;
}): string {
  if (isUngatedMode(args.requestedMode)) return args.requestedMode;
  const effective = effectiveMode(args.systemInit);
  return isUngatedMode(effective) ? effective : "";
}

/** Whether the session runs with no permission gate. See {@link ungatedModeOf}. */
export function isUngatedSession(args: {
  requestedMode: string;
  systemInit: Record<string, unknown> | null;
}): boolean {
  return ungatedModeOf(args) !== "";
}

/**
 * The permission mode the CLI reported in its `system:init` handshake, or ""
 * when no init has landed (or it carried no mode). protojson camelCase, which
 * is the shape the store adopts verbatim.
 */
export function effectiveMode(systemInit: Record<string, unknown> | null): string {
  if (systemInit === null) return "";
  const v = systemInit.permissionMode;
  return typeof v === "string" ? v : "";
}

/**
 * The banner's text for an ungated session, or "" when the session is gated.
 *
 * Exported separately from {@link ungatedBannerHtml} so the wording is
 * assertable without parsing markup.
 */
export function ungatedBannerText(mode: string): string {
  if (!isUngatedMode(mode)) return "";
  return `NO PERMISSION GATE — ${mode}: every tool call runs immediately, with no approval prompt`;
}

/**
 * The banner element's inner HTML. Empty string for a gated session, which
 * collapses the slot via `:empty`.
 *
 * There is no dismiss control, by design: the banner is a function of the live
 * mode and is re-rendered from state on every chrome frame, so it cannot be
 * closed while the mode is in force. A dismissible warning about a permanently
 * open door is a worse lie than no warning at all.
 */
export function ungatedBannerHtml(mode: string): string {
  const text = ungatedBannerText(mode);
  if (text === "") return "";
  // The mode string is a fixed enum member (isUngatedMode gated it), so the
  // interpolation carries no caller-controlled text.
  return `<span class="ungated-mark" aria-hidden="true">!</span><span class="ungated-text">${text}</span>`;
}

/**
 * The `<option>` list a permission-mode picker should carry, given the modes
 * it natively offers and the mode actually in force.
 *
 * The picker offers only SWITCHABLE modes, and `bypassPermissions` is
 * launch-only, so an ungated session's live mode matches no option and the
 * `<select>` renders BLANK — the one presentation strictly worse than showing
 * the dangerous mode, because a blank picker reads as "nothing special here".
 * This appends a DISABLED option carrying the live mode so the picker always
 * names what is running, while still refusing to offer a switch the CLI would
 * reject.
 *
 * Returns "" when the live mode is already among `offered`, so callers can
 * skip the DOM write entirely.
 */
export function unswitchableModeOptionHtml(
  offered: readonly string[],
  liveMode: string,
): string {
  if (liveMode === "" || offered.includes(liveMode)) return "";
  return `<option value="${escapeAttr(liveMode)}" disabled>${escapeText(liveMode)}</option>`;
}

function escapeAttr(s: string): string {
  return escapeText(s).replace(/"/g, "&quot;");
}

function escapeText(s: string): string {
  return s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
}
