/**
 * merge-queue-move — WHERE A DRAGGED WAITING MERGE GOES, the one reading
 * both queues a reader drags share: the sidebar's Merge Queue
 * (`sidebar/merge-queue-view.ts`) and a merge bubble's queue tab
 * (`feed/merge/queue.ts`). A drop is named by the waiting merge it now stands
 * beside, never by an index, so a queue that moved meanwhile cannot misplace
 * it (`agentrepl.v1.UpdateMergeQueueMoveTo`).
 */
import type { UpdateMergeQueueMoveTo } from "../../proto/gen/ts/agentrepl/v1/endpoint_update_merge_queue_pb";
import type { WorkspaceRef } from "../../proto/gen/ts/workspace/v1/workspace_pb";

/**
 * Where a merge dropped just before BEFORE (null: after every waiting merge)
 * goes, named by the waiting merge it now stands beside: ahead of BEFORE, or
 * behind the last of WAITING when it was dropped at the end. REFS names each
 * waiting merge's workspace.
 */
export function placeBeside(
  waiting: readonly HTMLElement[],
  item: HTMLElement,
  before: HTMLElement | null,
  refs: ReadonlyMap<HTMLElement, WorkspaceRef>,
): UpdateMergeQueueMoveTo["place"] {
  const anchorOf = (el: HTMLElement): WorkspaceRef => {
    const ref = refs.get(el);
    if (ref === undefined) throw new Error("invariant: a waiting merge's anchor names its workspace");
    return ref;
  };
  if (before !== null) return { case: "aheadOf", value: anchorOf(before) };
  const others = waiting.filter((el) => el !== item);
  const last = others[others.length - 1];
  if (last === undefined) throw new Error("invariant: a merge dropped elsewhere than it started has a waiting merge beside it");
  return { case: "behind", value: anchorOf(last) };
}

/** The `UpdateMergeQueue` action that moves WORKSPACE's waiting merge to PLACE. */
export function moveToAction(workspace: WorkspaceRef, place: UpdateMergeQueueMoveTo["place"]) {
  return { case: "moveTo" as const, value: { workspace, place } };
}
