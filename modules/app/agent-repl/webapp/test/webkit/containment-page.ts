/**
 * THE PAGE THE WEBKIT CONTAINMENT TEST DRIVES (containment.webkit.test.ts).
 *
 * Owner rule (2026-10-06): NOTHING IN THE FEED SPANS PAST THE FEED STREAM
 * AREA. This page mounts the REAL feed (`mountFeed`, the real renderers, the
 * real stylesheet the test injects) over a scripted daemon whose first page
 * holds one row of EVERY kind the contract declares — every FeedRow arm and
 * every FeedTurnActivity unit, in each of their states — with every text field
 * stretched into a long unbroken run, the input most likely to push a box past
 * its column. The test then reads, in real WebKit layout, whether any row
 * spills past the feed's stream area.
 */
import type { DescMessage, Message } from "@bufbuild/protobuf";
import { FeedRowSchema, type FeedRow } from "../../../proto/gen/ts/frontend/v1/feed_pb";
import { mountFeed } from "../../src/feed/feed.js";
import { createRowRenderers } from "../../src/feed/renderers.js";
import { ForwardingLogger, bindLogContext, setLogger } from "../../src/log.js";
import { orderFor } from "../feed-order.js";
import { harness, openSuccess, page } from "../feed/harness.js";
import {
  ARTIFACT_STATES,
  COLD_GATE_CHOICES,
  FEED_COMMAND_PANEL_ARMS,
  HOOK_OUTCOMES,
  MERGE_TAB_KINDS,
  PERMISSION_ANSWERS,
  PLAN_STATES,
  QUESTION_STATES,
  RESPONSE_STATES,
  SEPARATION_ARMS,
  SHELL_OUTCOMES,
  SKILL_OUTCOMES,
  SUBAGENT_OUTCOMES,
  TOOL_OUTPUT_FORMS,
  TURN_ERROR_ARMS,
  activityRow,
  agentPromptRow,
  artifactUnit,
  coldGateResolvedRow,
  coldGateStandingRow,
  commandPanelRow,
  commandRefusedRow,
  detachedShellRow,
  detachedSubagentRow,
  feedId,
  findingsUnit,
  hookUnit,
  mcpToolCallUnit,
  mergeTabRow,
  mergeUnit,
  MERGE_RESULTS,
  monitorCallUnit,
  peerMessageRow,
  permissionRow,
  planUnit,
  questionRow,
  responseNoticeUnit,
  responseUnit,
  separationRow,
  skillUnit,
  subagentHandbackRow,
  subagentResultUnit,
  subagentUnit,
  toolCallDeniedUnit,
  toolCallReturnedUnit,
  toolCallRunningUnit,
  turnEndedConcludedRow,
  turnEndedErroredRow,
  turnEndedInterruptedRow,
  userPromptRow,
  worktreeRemovedRow,
} from "../integration/fixtures.js";

/** The long unbroken run every stretched text field ends with. */
const RUN = `${"unbroken".repeat(60)}`;

/**
 * The text fields a row DRAWS as words. Only these are stretched: an id, a
 * path, a url, a paint class or a token is handed back or interpreted, and
 * stretching it would break the row rather than test its layout.
 */
const DRAWN_TEXT_FIELDS = new Set(["text", "markdown", "body", "label", "sender", "summary", "detail", "email", "name", "reason", "error", "status", "heading"]);

/** Stretch every drawn text field of MSG, recursively, in place. */
function stretch(msg: Message, desc: DescMessage): void {
  for (const field of desc.fields) {
    const record = msg as unknown as Record<string, unknown>;
    const value = record[field.localName];
    if (field.fieldKind === "scalar" && typeof value === "string" && DRAWN_TEXT_FIELDS.has(field.name) && value !== "") {
      record[field.localName] = `${value} ${RUN}`;
    } else if (field.fieldKind === "message" && value !== undefined && value !== null) {
      stretch(value as Message, field.message);
    } else if (field.fieldKind === "list" && field.listKind === "message" && Array.isArray(value)) {
      for (const item of value) stretch(item as Message, field.message);
    }
  }
  for (const oneof of desc.oneofs) {
    const record = msg as unknown as Record<string, { case?: string; value?: unknown }>;
    const chosen = record[oneof.localName];
    const field = oneof.fields.find((f) => f.localName === chosen?.case);
    if (field === undefined || chosen === undefined) continue;
    if (field.fieldKind === "message" && chosen.value !== undefined) {
      stretch(chosen.value as Message, field.message);
    } else if (field.fieldKind === "scalar" && typeof chosen.value === "string" && DRAWN_TEXT_FIELDS.has(field.name)) {
      chosen.value = `${chosen.value} ${RUN}`;
    }
  }
}

/** One of every row, ids made unique so no two collapse onto one row. */
export function everyRow(): FeedRow[] {
  const units = [
    ...RESPONSE_STATES.map((state) => responseUnit(state, `a long answer ${RUN}`)),
    responseNoticeUnit(),
    toolCallRunningUnit(),
    ...TOOL_OUTPUT_FORMS.map((form) => toolCallReturnedUnit(form)),
    toolCallDeniedUnit(),
    mcpToolCallUnit(),
    monitorCallUnit(),
    ...SKILL_OUTCOMES.map((outcome) => skillUnit(outcome)),
    ...HOOK_OUTCOMES.map((outcome) => hookUnit(outcome)),
    ...ARTIFACT_STATES.map((state) => artifactUnit(state)),
    ...PLAN_STATES.map((state) => planUnit(state)),
    findingsUnit(),
    subagentResultUnit(),
    subagentUnit("live"),
    ...SUBAGENT_OUTCOMES.map((outcome) => subagentUnit(outcome)),
    ...MERGE_RESULTS.map((result) => mergeUnit(result)),
  ];
  const rows: FeedRow[] = [
    userPromptRow(`a long prompt ${RUN}`),
    agentPromptRow(`a long agent prompt ${RUN}`),
    peerMessageRow(),
    subagentHandbackRow(),
    ...units.map((unit) => activityRow(unit)),
    turnEndedConcludedRow(feedId("nowhere")),
    ...TURN_ERROR_ARMS.map((arm) => turnEndedErroredRow(arm, { message: `the vendor's long account ${RUN}` })),
    turnEndedInterruptedRow(),
    detachedSubagentRow("live"),
    detachedShellRow("live"),
    ...SHELL_OUTCOMES.map((outcome) => detachedShellRow(outcome)),
    ...(["open", "abandoned", ...PERMISSION_ANSWERS] as const).map((state) => permissionRow(state)),
    ...QUESTION_STATES.map((state) => questionRow(state)),
    ...SEPARATION_ARMS.map((arm) => separationRow(arm)),
    worktreeRemovedRow(),
    coldGateStandingRow(),
    ...COLD_GATE_CHOICES.map((choice) => coldGateResolvedRow(choice)),
    ...MERGE_TAB_KINDS.map((kind) => mergeTabRow(kind)),
    ...FEED_COMMAND_PANEL_ARMS.map((arm) => commandPanelRow(arm)),
    commandRefusedRow({ reason: `a long reason ${RUN}` }),
  ];
  return rows.map((row, i) => {
    const id = `contained-${i.toString().padStart(3, "0")}`;
    row.id = feedId(id);
    row.order = orderFor(id);
    stretch(row, FeedRowSchema);
    return row;
  });
}

/** What the test calls, on `window.containment`. */
export interface ContainmentPage {
  /** Mount the feed over every row, and resolve once it drew them. */
  mount(): Promise<number>;
  /** The feed rows that spill past the stream area, by id and by how far. */
  spills(): { id: string; kind: string; over: number; culprit: string }[];
  /** The kinds of row the page served, as `row` or `activity.<unit>`. */
  kinds(): string[];
  /** Every record the page's logger has written, as `level operation`. */
  records(): string[];
}

declare global {
  interface Window {
    containment: ContainmentPage;
  }
}

const written: string[] = [];
setLogger(
  new ForwardingLogger(
    () => Promise.resolve("accepted"),
    (level, line) => {
      const { operation } = JSON.parse(line) as { operation: string };
      written.push(`${level} ${operation}`);
    },
    {},
    "debug",
  ),
);
bindLogContext({ connection_id: "webkit-containment-page" });

let served: FeedRow[] = [];

/** The outermost element under ROOT whose box reaches past AREA, as its class path. */
function culprit(root: HTMLElement, area: DOMRect): string {
  for (const el of root.querySelectorAll<HTMLElement>("*")) {
    const r = el.getBoundingClientRect();
    if (r.width > 0 && r.right > area.right + 1) {
      const path: string[] = [];
      for (let at: HTMLElement | null = el; at !== null && at !== root; at = at.parentElement) {
        path.unshift(`${at.localName}${at.className ? "." + String(at.className).trim().split(/\s+/).join(".") : ""}`);
      }
      return path.join(" > ");
    }
  }
  return "";
}

window.containment = {
  async mount(): Promise<number> {
    document.body.innerHTML = `
      <div id="main-col">
        <div id="feed-scroll" class="scroll-zone">
          <main id="feed" data-feed="root"></main>
        </div>
      </div>`;
    const host = document.getElementById("feed");
    const box = document.getElementById("feed-scroll");
    if (host === null || box === null) throw new Error("the page's shell did not mount");
    served = everyRow();
    const h = harness({ openFeed: () => openSuccess(page(served), "tok:root") });
    mountFeed(host, h.ctx, { renderers: createRowRenderers(h.ctx), scrollBox: box });
    for (let i = 0; i < 200 && host.querySelectorAll("[data-feed-row]").length < served.length; i++) {
      await new Promise((resolve) => setTimeout(resolve, 10));
    }
    // Every row laid out, as a reader scrolling the whole feed would see it.
    for (const item of host.querySelectorAll<HTMLElement>(".feed-item")) item.style.contentVisibility = "visible";
    // Every outcome marker OPEN: its expansion must stay inside the column too.
    for (const pill of host.querySelectorAll<HTMLElement>('.outcome-marker-pill[aria-expanded="false"]')) pill.click();
    await new Promise((resolve) => requestAnimationFrame(() => resolve(null)));
    return host.querySelectorAll("[data-feed-row]").length;
  },
  spills() {
    const host = document.getElementById("feed");
    const box = document.getElementById("feed-scroll");
    if (host === null || box === null) throw new Error("not mounted");
    const area = host.getBoundingClientRect();
    const out: { id: string; kind: string; over: number; culprit: string }[] = [];
    if (box.scrollWidth > box.clientWidth + 1) {
      out.push({ id: "#feed-scroll", kind: "the stream scrolls sideways", over: box.scrollWidth - box.clientWidth, culprit: "" });
    }
    for (const item of host.querySelectorAll<HTMLElement>("[data-feed-row]")) {
      const r = item.getBoundingClientRect();
      const over = Math.max(r.right - area.right, area.left - r.left, item.scrollWidth - item.clientWidth);
      if (over > 1) {
        out.push({
          id: item.getAttribute("data-feed-row") ?? "?",
          kind: item.getAttribute("data-row-kind") ?? item.className,
          over: Math.round(over),
          culprit: culprit(item, area),
        });
      }
    }
    return out;
  },
  kinds() {
    return served.map((row) =>
      row.row.case === "activity" ? `activity.${row.row.value.unit.case ?? "unset"}` : (row.row.case ?? "unset"),
    );
  },
  records() {
    return [...written];
  },
};
