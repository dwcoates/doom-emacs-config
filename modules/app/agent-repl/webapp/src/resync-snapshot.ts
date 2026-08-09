/**
 * The immutable WorkspaceState facts that make one `ResyncCmd` conditional.
 *
 * A replay request must retain the exact FENCE that was authoritative when its
 * sender decided to issue it.  Reading a later state when the transport sends
 * would silently rebind a delayed client to a newer controller generation.
 *
 * The fence is the workspace's own entry in the store's fence map — the same
 * answer inbound fenced pushes are measured against (see `fence.ts`), so the
 * question "which generation is this end reading" has one answer in both
 * directions.  A workspace with NO fence yet yields `""`, which the daemon
 * refuses; that is the honest outcome, because nothing has yet established what
 * current means for this workspace and a replay could only be against a guess.
 */
import type { ResyncSnapshot } from "./connect-resync.js";

/** The revisioned workspace facts every resync sender reads from the store. */
export interface ResyncSnapshotSource {
  cwd: string;
  fences: ReadonlyMap<string, string>;
}

/** Copy the current WorkspaceState fence into a dispatch-ready request. */
export function captureResyncSnapshot(
  state: ResyncSnapshotSource,
  fromSeq: number,
): ResyncSnapshot {
  return {
    workspace: state.cwd,
    fromSeq,
    fence: state.fences.get(state.cwd) ?? "",
  };
}
