/**
 * withheld — the one reader of a refusal the daemon makes because it serves no
 * page of a workspace that is not visible.
 *
 * `WatchWebWorkspace` refuses a workspace that is not visible (`not_visible`),
 * and the editor releases the page of such a workspace. A page that meets the
 * refusal is one whose workspace's visibility changed while it loaded: planned,
 * never an outage. The daemon marks it by type, on a dedicated stream's error
 * metadata (`WITHHELD_HEADER`) and on a page subscription's failure
 * (`agentrepl.v1.PageSubscriptionFailed.withheld`, which page-streams.ts carries
 * into the same header), so every layer asks this one reader and files no
 * failure for it.
 */
import { ConnectError } from "@connectrpc/connect";

/** The daemon's metadata marking a refusal withheld (daemon internal/server WithheldHeader). */
export const WITHHELD_HEADER = "Agent-Repl-Withheld";

/** Whether ERR is a refusal withheld because the workspace is not visible. */
export function isWithheld(err: unknown): boolean {
  if (!(err instanceof ConnectError)) return false;
  const marked = err.metadata.get(WITHHELD_HEADER);
  return marked !== null && marked !== "";
}
