/**
 * fence — THE staleness gate, and the single choke point every fenced
 * component view passes through before one byte of it reaches the store.
 *
 * WHY A CHOKE POINT AND NOT A CHECK PER CALL SITE. The daemon publishes a
 * workspace's chrome as several independent resolved views (topbar, token
 * breakdown, revival gate), each stamped with the workspace's staleness FENCE
 * at the moment it was produced. A view that was produced before the
 * workspace's owning session rotated is STALE: it describes a session that is
 * gone. Adopting even part of one is how a composer ends up locked against a
 * workspace that is already awake, or a topbar names a model nothing is running.
 *
 * A per-view check would be three checks, then four, then five — each one an
 * opportunity to forget, and each one a second place where "what does stale
 * mean" is decided. So there is exactly ONE function here, {@link admitFenced},
 * and the store has exactly one ingestion case for fenced views that calls it.
 * A new resolved view joins the {@link FencedView} union and is gated by
 * construction; it cannot reach the store any other way, because the store
 * exposes no other entry that writes a view slice.
 *
 * THE RULE, in full:
 *
 * - The fence is compared BYTE-WISE against the store's current
 *   `WorkspaceState.fence`. It is never parsed, split, prefixed-matched or
 *   interpreted. Its composition is the daemon's and is free to change, which
 *   is only true for as long as nothing here reads structure into it.
 * - Equal means CURRENT: the view is adopted whole.
 * - Different means STALE: the view is discarded WHOLE. There is no partial
 *   adoption — not "take the title but not the gate", not "take it if the store
 *   has nothing yet". Half a stale view is a view that disagrees with itself.
 * - A store holding NO fence yet is not a match. Nothing has established what
 *   current means for this workspace, so no view can be shown to be current.
 * - Every discard is REPORTED, once per discarded push, through the client's
 *   canonical log channel (`ClientLogCmd`) with structured context: which view,
 *   which workspace, the fence the push carried and the fence it was measured
 *   against. A silently dropped frame is indistinguishable from a daemon that
 *   never sent one.
 */

import type {
  TokenBreakdownView,
  TopbarView,
  WorkspaceGateView,
} from "./frontend-proto.js";
import type { ClientLogContext } from "./protocol.js";

/**
 * ONE fenced resolved view, as the union the choke point accepts.
 *
 * Adding an arm here is what makes a new view fenced. It is deliberately a
 * closed union rather than a structural `{ fence: string }` constraint: the arm
 * name is what the discard record reports, and a structural constraint would
 * let any object with a `fence` field slip through unnamed.
 */
export type FencedView =
  | { case: "topbar"; value: TopbarView }
  | { case: "tokenBreakdown"; value: TokenBreakdownView }
  | { case: "workspaceGate"; value: WorkspaceGateView };

/** The workspace a fenced view describes, whichever arm it is. */
export function fencedWorkspace(view: FencedView): string {
  return view.value.workspace;
}

/** The fence a push carried, whichever arm it is. */
export function fencedFence(view: FencedView): string {
  return view.value.fence;
}

/**
 * A stale push, as the record the caller writes to `ClientLogCmd`.
 *
 * Returned rather than logged here so the one canonical logging API stays the
 * caller's (the same discipline `hibernationBlockedLog` follows), and so the
 * wording and the structured context are assertable without a logger double.
 */
export interface StaleFenceReport {
  message: string;
  context: ClientLogContext;
}

/**
 * The verdict on ONE fenced push.
 *
 * `adopt` carries the view back rather than a bare boolean, so the caller
 * cannot adopt a view the gate did not return — the only reference it has to an
 * admitted view comes out of this function.
 */
export type FenceVerdict =
  | { kind: "adopt"; view: FencedView }
  | { kind: "discard"; report: StaleFenceReport };

/**
 * THE fence gate. Every fenced view passes through here, exactly once, before
 * any store mutation.
 *
 * CURRENTFENCE is the store's current `WorkspaceState.fence` for the workspace
 * — the authoritative answer — and `""` means the store holds no ruling yet.
 */
export function admitFenced(view: FencedView, currentFence: string): FenceVerdict {
  const pushed = fencedFence(view);
  if (currentFence !== "" && pushed === currentFence) return { kind: "adopt", view };
  return { kind: "discard", report: staleFenceReport(view, currentFence) };
}

/**
 * The record for one discarded push.
 *
 * The two fences are carried VERBATIM and side by side: the whole diagnostic
 * value is being able to see that they differ and by what, and a report that
 * summarized them ("stale") would leave a reader unable to tell a rotation from
 * a client that never adopted a WorkspaceState at all.
 */
function staleFenceReport(view: FencedView, currentFence: string): StaleFenceReport {
  const workspace = fencedWorkspace(view);
  const pushed = fencedFence(view);
  const held = currentFence === "" ? "none" : currentFence;
  return {
    message:
      `stale fenced view discarded whole: ${view.case} for workspace=${workspace} ` +
      `carried fence=${pushed} while the workspace's current fence is ${held} ` +
      `(no part of it was adopted)`,
    context: {
      operation: "fence.stale-view",
      workspace,
      view: view.case,
      pushed_fence: pushed,
      current_fence: currentFence,
      branch: currentFence === "" ? "no_current_fence" : "fence_mismatch",
      cause: "a fenced push must match the workspace's current WorkspaceState fence byte-wise",
    },
  };
}
