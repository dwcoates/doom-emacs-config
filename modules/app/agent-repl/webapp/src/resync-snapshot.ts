/**
 * The immutable WorkspaceState facts that make one `ResyncCmd` conditional.
 *
 * A replay request must retain the exact session/controller identity that was
 * authoritative when its sender decided to issue it.  Reading a later state
 * when the transport sends would silently rebind a delayed client to a newer
 * controller generation.
 */
import type { ResyncSnapshot } from "./connect-resync.js";

/** The revisioned workspace facts every resync sender reads from the store. */
export interface ResyncSnapshotSource {
  cwd: string;
  sessionId: string;
  controllerGenerationId: string;
}

/** Copy the current WorkspaceState identity into a dispatch-ready request. */
export function captureResyncSnapshot(
  state: ResyncSnapshotSource,
  fromSeq: number,
): ResyncSnapshot {
  return {
    workspace: state.cwd,
    fromSeq,
    sessionId: state.sessionId,
    controllerGenerationId: state.controllerGenerationId,
  };
}
