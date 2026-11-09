/**
 * selection-paint — WHEN THE SIDEBAR FIRST PAINTED A SELECTION.
 *
 * WHY IT EXISTS (2026-11-09). A switch painted the page first and the sidebar
 * 70-270ms later, and the logs could say when the page painted
 * (`page.shown-frame`) but not when the sidebar did: `sidebar.roster-draw`
 * records every draw, keyed to no switch. The daemon now names each select
 * on the roster's selection (frontend.v1.RosterCurrentWorkspace.selection_id,
 * beside its own `selected the workspace` record), and this records the
 * first frame the rail painted each one in (`sidebar.selection-painted`), so
 * the gap between the page's paint and the sidebar's is read from the logs.
 *
 * THE PAINT IS THE FRAME AFTER THE DRAW. A roster is drawn inside an
 * animation frame (sidebar.ts `drawRoster`), so the frame it painted in has
 * been shown when the next one runs, which is when this records.
 *
 * THE SWITCH'S LATENCY RIDES THE SAME EDGE. When the select carried the
 * instant Emacs took the keystroke (RosterCurrentWorkspace
 * .switch_started_at_ms), the time from it to the paint is the
 * `switch_sidebar` latency (proto/vocab/latencies.json), measured on the
 * host's wall clock the keystroke was stamped with (wall-latency.ts).
 */
import { ReportedLatencyKind } from "../../../proto/gen/ts/agentrepl/v1/endpoint_report_latencies_pb";
import type { RosterCurrentWorkspace } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import type { WorkspaceRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { log } from "../log.js";
import { recordWallLatency } from "../wall-latency.js";

/** The clock and frames a selection's paint is timed with. */
export interface SelectionPaintEnv {
  /** Wall-clock Unix milliseconds, the clock Emacs stamps a switch with. */
  readonly wallNow: () => number;
  /** Run FN on the next animation frame. */
  readonly frame: (fn: () => void) => void;
}

/** Where a sidebar latency goes: the workspace it happened in, its kind and its length. */
export type RecordSidebarLatency = (workspace: WorkspaceRef, kind: ReportedLatencyKind, seconds: number) => void;

/** The watcher one rail keeps. */
export interface SelectionPaint {
  /** The rail has just drawn a roster whose selection is CURRENT (absent when none). */
  drawn(current: RosterCurrentWorkspace | undefined): void;
}

/** Record the first paint of each select's selection; RECORD takes its latency when given. */
export function createSelectionPaint(env: SelectionPaintEnv, record?: RecordSidebarLatency): SelectionPaint {
  /** The select whose selection was last drawn, so each is recorded once. */
  let drawnSelection = "";
  return {
    drawn(current) {
      const selection = current?.selectionId ?? "";
      // A SELECTION NO SELECT NAMED (a boot's registry, another daemon's
      // stored selection) has no switch to key a paint to.
      if (current === undefined || selection === "" || selection === drawnSelection) return;
      drawnSelection = selection;
      const workspace = current.workspace;
      if (workspace === undefined) {
        throw new Error(`sidebar: the roster's selection ${selection} names no workspace`);
      }
      const startedAtMs = current.switchStartedAtMs === undefined ? null : Number(current.switchStartedAtMs);
      env.frame(() => {
        const paintedAtMs = env.wallNow();
        const context: Record<string, unknown> = { workspace: workspace.id, selection_id: selection };
        if (startedAtMs !== null) context.ms_since_switch = paintedAtMs - startedAtMs;
        log.info("the sidebar painted the selection a select made", { operation: "sidebar.selection-painted", context });
        if (startedAtMs !== null && record !== undefined) {
          recordWallLatency(ReportedLatencyKind.SWITCH_SIDEBAR, startedAtMs, paintedAtMs, (kind, seconds) =>
            record(workspace, kind, seconds),
          );
        }
      });
    },
  };
}
