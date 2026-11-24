/**
 * cold-gate-name — THE ONE COLD-GATE TREATMENT OF A NAME IN THE RAIL.
 *
 * A workspace parked at its cold gate draws its name struck through (owner
 * ruling, 2026-10-13): an Open row's in its status tone until the row is
 * viewed and in a grey dimmer than muted once it is (owner request,
 * 2026-11-23), a merge queue line's always in that grey. The Open section's rows draw it
 * from their `coldGate` arm (`row.ts`), and the Merge Queue section's lines
 * from the roster's standing cold gates (`merge-queue-view.ts`, owner request
 * 2026-11-17), both through `markColdGatedName`, so the stylesheet's one rule
 * on COLD_GATED_NAME_CLASS draws both. A merge queue line draws no status mark,
 * so it shows the strike without the Open section's snowflake.
 */
import type { RosterColdGate } from "../../../proto/gen/ts/frontend/v1/sidebar_pb";
import { requireMessage } from "../rpc/strict.js";

/** The class a cold-gated workspace's name wears, in every section of the rail. */
export const COLD_GATED_NAME_CLASS = "cold-gated";

/** Draw NAME as a cold-gated workspace's name. */
export function markColdGatedName(name: HTMLElement): void {
  name.classList.add(COLD_GATED_NAME_CLASS);
}

/** The workspaces whose sessions stand at their cold gates, by `WorkspaceRef.id`. */
export type ColdGated = ReadonlySet<string>;

/** No workspace stands at its cold gate. */
export const NONE_COLD_GATED: ColdGated = new Set();

/**
 * The workspaces GATES stand on (`frontend.v1.WorkspaceRoster.cold_gates`),
 * the standing the daemon resolved: the sidebar reads a cold gate from it,
 * never from any text it draws.
 */
export function coldGatedOf(gates: readonly RosterColdGate[], path: string): ColdGated {
  return new Set(gates.map((gate, index) => requireMessage(gate.workspace, `${path}[${index}].workspace`).id));
}

/** Whether A and B name the same workspaces, in whatever order. */
export function sameColdGated(a: ColdGated, b: ColdGated): boolean {
  return a.size === b.size && [...a].every((id) => b.has(id));
}
