/**
 * "Session gone" remediation: the browser can only observe that its
 * session has vanished from the daemon, so it hands the loss back to the
 * daemon (POST /remediation), which dispatches a headless Opus analyst to
 * diagnose the termination and open a workspace that engineers the failure
 * class out of the system.
 *
 * The daemon is by construction reachable at this point: "gone" means the
 * existence probe got a 200 from GET /sessions whose listing lacked our id,
 * not that the daemon was unreachable.
 */

/** Notice shown while the analyst works. */
export const REMEDIATION_DEVISING = "devising remediation plan";

/** Notice shown when the analyst could not be dispatched at all. */
export const REMEDIATION_FAILED = "remediation dispatch failed";

/** Notice shown when an analyst was already working on this session. */
export const REMEDIATION_ALREADY_UNDERWAY = "remediation already underway";

/** Lifecycle of the remediation the frontend requested. */
export type RemediationPhase = "devising" | "underway" | "failed";

/** Topbar text for a remediation phase. */
export function remediationNotice(phase: RemediationPhase): string {
  switch (phase) {
    case "devising":
      return REMEDIATION_DEVISING;
    case "underway":
      return REMEDIATION_ALREADY_UNDERWAY;
    case "failed":
      return REMEDIATION_FAILED;
  }
}

/**
 * Ask the daemon to remediate a session it no longer knows.
 *
 * Resolves "devising" when this call dispatched the analyst and "underway"
 * when one was already running for the session (a second tab, or an earlier
 * reconnect, got there first). Rejects on any non-2xx: a remediation that
 * never launched must be surfaced, never silently read as underway.
 */
export async function requestRemediation(
  httpBase: string,
  sessionId: string,
  fetchFn: typeof fetch = fetch,
): Promise<RemediationPhase> {
  const resp = await fetchFn(`${httpBase}/remediation`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ session_id: sessionId }),
  });
  if (!resp.ok) {
    throw new Error(`POST /remediation: ${resp.status} ${await resp.text()}`);
  }
  const body = (await resp.json()) as { started: boolean };
  return body.started ? "devising" : "underway";
}
