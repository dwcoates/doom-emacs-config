/**
 * Streaming-response-element catalogue — a browsable mock gallery of every
 * element in the taxonomy under deliberation: any response element (bubble
 * or badge) that can be expanded to show a continuous stream of update
 * information specific to an asynchronous process.
 *
 * The point of the page is FIDELITY: each example is a mock item list fed
 * through the REAL projection pipeline (`gnsFolds` → `partitionFeed` →
 * `asyncMembersByBubble` → `finalResponses`) and painted by the REAL
 * `renderItem` under the real stylesheet, so what the gallery shows is what
 * the GUI draws — only the DATA is fabricated. Open it via the Vite dev
 * server at /catalogue.html.
 *
 * Taxonomy numbers in the scenario titles refer to the deliberation's
 * enumeration: 2.x are the agreed members, 3.x the boundary cases.
 */
import "./styles.css";
import { parseJournal } from "./async-stream.js";
import { CLICK_THROUGH_SELECTOR, PANEL_CLASS } from "./expand.js";
import { gnsFolds } from "./gns.js";
import { escapeHtml } from "./highlight.js";
import { partitionFeed } from "./partition.js";
import {
  PanelContext,
  activityTicker,
  asyncMembersByBubble,
  finalResponses,
  panelToggleTarget,
  renderItem,
} from "./render.js";
import {
  ConversationItem,
  PermissionItem,
  ResultItem,
  TextItem,
  ThinkingItem,
  ToolItem,
  UserTurnItem,
} from "./store.js";
import { agentTopbarHtml } from "./topbar.js";
import { TaskTail } from "./watcher-poll.js";

/** One catalogue entry: a mock feed plus the folds its expanded pane opens. */
export interface Scenario {
  /** Taxonomy number from the deliberation ("2.1", "3.3", …). */
  taxonomy: string;
  slug: string;
  title: string;
  /** What the example demonstrates, rendered as the section's caption. */
  blurb: string;
  items: ConversationItem[];
  /** Poller tails by source id, standing in for the daemon. */
  tails?: Record<string, TaskTail>;
  /** Fold ids pre-opened in the expanded variant; empty = one variant only. */
  openIds: string[];
}

// --- mock item constructors ---------------------------------------------------

const TS = "2026-07-19T12:00:00.000Z";

function userTurn(requestId: string, text: string): UserTurnItem {
  return { kind: "user-turn", requestId, content: [{ type: "text", text }], ts: TS };
}

function text(
  blockId: string,
  body: string,
  opts: { parent?: string; done?: boolean } = {},
): TextItem {
  return {
    kind: "text",
    blockId,
    messageId: `m-${blockId}`,
    parentToolUseId: opts.parent,
    text: body,
    done: opts.done ?? true,
    ts: TS,
  };
}

function thinking(
  blockId: string,
  body: string,
  opts: { parent?: string; done?: boolean } = {},
): ThinkingItem {
  return {
    kind: "thinking",
    blockId,
    messageId: `m-${blockId}`,
    parentToolUseId: opts.parent,
    text: body,
    done: opts.done ?? true,
  };
}

function tool(
  toolUseId: string,
  toolName: string,
  input: Record<string, unknown>,
  opts: Partial<ToolItem> = {},
): ToolItem {
  return {
    kind: "tool",
    toolUseId,
    toolName,
    messageId: `m-${toolUseId}`,
    ts: TS,
    input,
    inputJson: JSON.stringify(input, null, 2),
    inputDone: true,
    ...opts,
  };
}

function ok(content: string): ToolItem["result"] {
  return { isError: false, content };
}

function pendingPermission(
  requestId: string,
  toolUseId: string,
  toolName: string,
  input: unknown,
): PermissionItem {
  return { kind: "permission", requestId, toolUseId, toolName, input };
}

function successResult(): ResultItem {
  return {
    kind: "result",
    subtype: "success",
    durationMs: 5230,
    sincePrevFinalMs: 5230,
    numTurns: 3,
    totalCostUsd: 0.12,
    usage: { input_tokens: 1200, output_tokens: 300 },
    isError: false,
    context: { total: 54_000, delta: 1_800 },
  };
}

// --- mock stream payloads -----------------------------------------------------

/** One assistant transcript entry carrying BLOCKS, as the daemon spools it. */
function assistantLine(...blocks: Record<string, unknown>[]): string {
  return JSON.stringify({
    type: "assistant",
    timestamp: TS,
    message: { id: "m-transcript", content: blocks },
  });
}

/** One tool_result transcript entry answering TOOLUSEID. */
function toolResultLine(toolUseId: string, content: string): string {
  return JSON.stringify({
    type: "user",
    message: {
      id: "m-transcript",
      content: [{ type: "tool_result", tool_use_id: toolUseId, content }],
    },
  });
}

/** A detached agent's JSONL transcript, with a depth-two spawn inside it. */
const MIGRATION_TRANSCRIPT = [
  assistantLine({
    type: "thinking",
    thinking:
      "Three files import oldApi; batching by directory keeps the diffs reviewable.",
  }),
  assistantLine({ type: "text", text: "Migrating the first batch now." }),
  assistantLine({
    type: "tool_use",
    id: "s5-t1",
    name: "Bash",
    input: { command: "rg -l oldApi src/" },
  }),
  toolResultLine("s5-t1", "src/a.ts\nsrc/b.ts\nsrc/c.ts"),
  assistantLine({
    type: "tool_use",
    id: "s5-t2",
    name: "TaskCreate",
    input: { subject: "migrate src/b.ts" },
  }),
  toolResultLine("s5-t2", "Task #7 created successfully: migrate src/b.ts"),
  assistantLine({
    type: "tool_use",
    id: "s5-t3",
    name: "TaskUpdate",
    input: { taskId: "7", status: "completed" },
  }),
  toolResultLine("s5-t3", "Updated task #7"),
  assistantLine({
    type: "tool_use",
    id: "s5-t4",
    name: "Bash",
    input: { command: "./scripts/watch-migration.sh", run_in_background: true },
  }),
  toolResultLine("s5-t4", "Command running in background with ID: bg-nested-9"),
  assistantLine({ type: "text", text: "First batch migrated; the watcher is armed." }),
].join("\n");

/** A short transcript for the badge-expanded watcher example. */
const BADGE_TRANSCRIPT = [
  assistantLine({ type: "text", text: "Halfway through; two files remain." }),
  assistantLine({
    type: "tool_use",
    id: "s6-t1",
    name: "Edit",
    input: { file_path: "src/b.ts" },
  }),
  toolResultLine("s6-t1", "ok"),
].join("\n");

/** A workflow run's journal.jsonl rows: done, running, and errored. */
const REVIEW_JOURNAL = [
  JSON.stringify({ label: "review:bugs", prompt: "Find bugs", result: "3 findings" }),
  JSON.stringify({ label: "review:perf", prompt: "Find perf issues" }),
  JSON.stringify({ label: "verify:store.ts", error: "agent exhausted retries" }),
].join("\n");

// --- the catalogue ------------------------------------------------------------

export const scenarios: Scenario[] = [
  {
    taxonomy: "2.1",
    slug: "agent-activity",
    title: "activity fold on an inline subagent card",
    blurb:
      "An Agent card whose child feed streams in as parent-tagged frames. The collapsed face is the step ticker; expanded, the panel nests the child feed through renderItem — including a grandchild Agent card with its own fold, a pending permission (the loud badge), and the agent-scoped topbar strip.",
    items: [
      userTurn("cat-u1", "Audit the parser's quote handling"),
      tool("cat-agent-1", "Agent", {
        description: "audit parser edge cases",
        subagent_type: "general-purpose",
        prompt: "Audit quote handling in parser.ts",
      }),
      thinking(
        "s1-th1",
        "Single quotes nest inside double quotes here, so the tokenizer needs a stack rather than a flag.",
        { parent: "cat-agent-1" },
      ),
      text("s1-tx1", "Scanning the tokenizer for quote-state transitions.", {
        parent: "cat-agent-1",
      }),
      tool(
        "s1-read1",
        "Read",
        { file_path: "src/parser.ts" },
        { parentToolUseId: "cat-agent-1", result: ok("(412 lines)") },
      ),
      tool(
        "s1-agent2",
        "Agent",
        { description: "verify fix on the mac runner", subagent_type: "general-purpose" },
        { parentToolUseId: "cat-agent-1", result: ok("Verified: 34 tests pass.") },
      ),
      text("s1-tx2", "The mac runner agrees; drafting the fix now.", {
        parent: "s1-agent2",
      }),
      tool(
        "s1-bash1",
        "Bash",
        { command: "rg -n 'unquote' src/" },
        { parentToolUseId: "cat-agent-1" },
      ),
      pendingPermission("s1-perm1", "s1-bash1", "Bash", {
        command: "rg -n 'unquote' src/",
      }),
    ],
    openIds: ["cat-agent-1", "s1-agent2"],
  },
  {
    taxonomy: "2.1 + 2.2",
    slug: "workflow-journal",
    title: "workflow card: activity fold plus journal-format async fold",
    blurb:
      "A Workflow card carrying BOTH element kinds at once: the activity fold (its inline child agents) and the async fold (the run's journal, rendered as rows rather than bubbles because a journal is a record log, not a conversation).",
    items: [
      userTurn("cat-u2", "ultracode: review the branch"),
      tool(
        "cat-wf-1",
        "Workflow",
        { description: "review changes across dimensions" },
        {
          result: ok("Workflow launched in background with ID: wf-review-1"),
          asyncSource: {
            source_id: "wf-review-1",
            kind: "workflow",
            label: "review-changes",
            status: "running",
            stream: { transport: "poll", format: "jsonl-journal" },
          },
          taskOutput: REVIEW_JOURNAL,
        },
      ),
      text("s2-tx1", "Fanning out the review dimensions.", { parent: "cat-wf-1" }),
      tool(
        "s2-a1",
        "Agent",
        { description: "review: bugs" },
        { parentToolUseId: "cat-wf-1", result: ok("3 findings") },
      ),
      tool(
        "s2-a2",
        "Agent",
        { description: "review: performance" },
        { parentToolUseId: "cat-wf-1" },
      ),
    ],
    openIds: ["cat-wf-1", "async:cat-wf-1"],
  },
  {
    taxonomy: "3.1",
    slug: "taskcreate-history",
    title: "boundary: TaskCreate card streaming its update history",
    blurb:
      "The task's TaskUpdate calls are claimed by task id (parsed from the create's result text) and nest as one-line transition bubbles in the create card's activity fold. Boundary case: a continuous stream of updates, but of harness state the agent itself drives, not of a detached process.",
    items: [
      tool(
        "cat-task-1",
        "TaskCreate",
        { subject: "Fix the flaky watcher test" },
        { result: ok("Task #1 created successfully: Fix the flaky watcher test") },
      ),
      tool(
        "s3-up1",
        "TaskUpdate",
        { taskId: "1", status: "in_progress" },
        { result: ok("Updated task #1") },
      ),
      tool(
        "s3-up2",
        "TaskUpdate",
        {
          taskId: "1",
          status: "completed",
          subject: "Fix the flaky watcher test (deflaked)",
        },
        { result: ok("Updated task #1") },
      ),
    ],
    openIds: ["cat-task-1"],
  },
  {
    taxonomy: "2.2",
    slug: "shell-fold",
    title: "async fold on a backgrounded shell (text format)",
    blurb:
      "A Bash card that spawned detached work the daemon classified as a shell stream. The collapsed face is the kind · label · status pill with the live arc; expanded, the tail is a raw <pre> — bytes are bytes, no bubbles are invented for them.",
    items: [
      userTurn("cat-u4", "Soak the daemon in the background"),
      tool(
        "cat-shell-1",
        "Bash",
        { command: "./scripts/soak.sh --hours 4", run_in_background: true },
        {
          result: ok("Command running in background with ID: bg-soak-1"),
          asyncSource: {
            source_id: "bg-soak-1",
            kind: "shell",
            label: "./scripts/soak.sh --hours 4",
            status: "running",
            stream: { transport: "ws", format: "text" },
          },
          taskOutput:
            "soak: iteration 1 ok (1.2s)\nsoak: iteration 2 ok (1.1s)\nsoak: iteration 3 ok (1.3s)",
        },
      ),
    ],
    openIds: ["async:cat-shell-1"],
  },
  {
    taxonomy: "2.2",
    slug: "agent-transcript",
    title: "async fold on a detached agent (transcript format), nesting to depth two",
    blurb:
      "A background Agent whose polled JSONL transcript renders as nested bubbles through the same renderItem the top feed uses. Inside it: a TaskCreate whose TaskUpdate folds under it (the transcript's own partition), and a nested backgrounded shell whose synthesized source folds at depth two.",
    items: [
      userTurn("cat-u5", "Migrate the call sites in the background"),
      tool(
        "cat-agentbg-1",
        "Agent",
        { description: "migrate oldApi call sites", run_in_background: true },
        {
          result: ok("Async agent launched. agentId: ag-42"),
          asyncSource: {
            source_id: "ag-42",
            kind: "agent",
            label: "migrate oldApi call sites",
            status: "running",
            stream: { transport: "poll", format: "jsonl-transcript" },
          },
          taskOutput: MIGRATION_TRANSCRIPT,
        },
      ),
    ],
    tails: {
      "bg-nested-9": {
        text: "watch: src/b.ts clean\nwatch: src/c.ts clean",
        offset: 64,
        done: false,
        elapsedMs: 42_000,
      },
    },
    openIds: ["async:cat-agentbg-1", "s5-t2", "async:s5-t4"],
  },
  {
    taxonomy: "2.3",
    slug: "host-catalog",
    title: "host bubble's async catalog: badges expanding to watcher rows",
    blurb:
      "A final response hosting three members its turn armed: two live (amber badges, amber bubble border outranking the green final-response) and one settled (grey badge). Expanded, the agent member's badge opens its WatcherRow — the same transcript-as-bubbles dispatch, plus stop control and composer.",
    items: [
      userTurn("cat-u6", "Kick off the soak and the migration, then summarize"),
      tool(
        "cat-w1",
        "Bash",
        { command: "./scripts/soak.sh", run_in_background: true },
        {
          result: ok("Command running in background with ID: bg-7"),
          asyncSource: {
            source_id: "bg-7",
            kind: "shell",
            label: "./scripts/soak.sh",
            status: "running",
            stream: { transport: "ws", format: "text" },
          },
          taskOutput: "soak: iteration 1 ok",
        },
      ),
      tool(
        "cat-w2",
        "Agent",
        { description: "migrate oldApi call sites", run_in_background: true },
        {
          result: ok("Async agent launched. agentId: ag-9"),
          asyncSource: {
            source_id: "ag-9",
            kind: "agent",
            label: "migrate oldApi call sites",
            status: "running",
            stream: { transport: "poll", format: "jsonl-transcript" },
          },
        },
      ),
      tool(
        "cat-w3",
        "Bash",
        { command: "./scripts/prefetch.sh", run_in_background: true },
        {
          result: ok("Command running in background with ID: bg-old"),
          notification: { taskId: "bg-old", status: "completed", text: "prefetch done" },
        },
      ),
      text(
        "cat-final-6",
        "Both jobs are running; the prefetch already finished. I'll report when the soak and the migration land.",
      ),
      successResult(),
    ],
    tails: {
      "ag-9": { text: BADGE_TRANSCRIPT, offset: 512, done: false, elapsedMs: 96_000 },
    },
    openIds: ["member:cat-final-6:cat-w2"],
  },
  {
    taxonomy: "2.3",
    slug: "prompt-host",
    title: "prompt bubble as fallback host (tools-only turn)",
    blurb:
      "A turn that armed background work but wrote no answer: the catalog lands on the user's own prompt bubble, which goes amber and enumerates the work exactly as a final response would.",
    items: [
      userTurn("cat-u7", "Run the soak in the background and keep it running"),
      tool(
        "cat-w4",
        "Bash",
        { command: "./scripts/soak.sh", run_in_background: true },
        {
          result: ok("Command running in background with ID: bg-11"),
          asyncSource: {
            source_id: "bg-11",
            kind: "shell",
            label: "./scripts/soak.sh",
            status: "running",
            stream: { transport: "ws", format: "text" },
          },
          taskOutput: "soak: iteration 1 ok\nsoak: iteration 2 ok",
        },
      ),
      successResult(),
    ],
    openIds: ["member:cat-u7:cat-w4"],
  },
  {
    taxonomy: "3.2",
    slug: "gns-fold",
    title: "boundary: gns-sockets fold on a final response",
    blurb:
      "Stop-hook bridge upkeep (the sockets-listener respawn and its acknowledgment) folds out of the feed into the answer above it, which keeps the green border — and the bridge spawn doubles as an async member, so the same bubble also wears the catalog badge. Boundary case: the fold's body is folded past turns, not a continuous live tail.",
    items: [
      userTurn("cat-u8", "What changed in the publish pipeline?"),
      text(
        "cat-final-8",
        "The publish pipeline now dedupes retried events before the fan-out, so replays cannot double-publish.",
      ),
      tool(
        "cat-br-1",
        "Agent",
        { description: "respawn the slack bridge", subagent_type: "sockets-listener" },
        { result: ok("Async agent launched. agentId: br-1") },
      ),
      text("s8-ack", "Bridge listener respawned for the quiet window."),
      successResult(),
    ],
    openIds: ["gns:cat-final-8"],
  },
  {
    taxonomy: "3.3",
    slug: "inline-shell",
    title: "boundary: announcement-less tail, inline with no expansion",
    blurb:
      "A streamed tail on a call whose result announced NO task id: with nothing to synthesize a source from (a result that does announce one grows a fold even without daemon classification), the tail paints zero-click into the card as a plain <pre>. Boundary case: a continuous stream of an async process that is NOT expandable — it fails the definition today, yet is arguably the same element wanting the same dress.",
    items: [
      userTurn("cat-u9", "Keep the build watcher running"),
      tool(
        "cat-plainshell-1",
        "Bash",
        { command: "make watch" },
        {
          result: ok("(watching; output streams below)"),
          taskOutput: "watch: build ok (3.2s)\nwatch: build ok (2.9s)",
        },
      ),
    ],
    openIds: [],
  },
  {
    taxonomy: "3.4",
    slug: "thinking",
    title: "boundary: thinking disclosure",
    blurb:
      "A thinking block streams continuously and expands via a native <details>, but the stream is the model's own turn, not a detached asynchronous process — the proposed exclusion. Shown streaming (spinner, held open) and settled (closed, reopenable).",
    items: [
      thinking(
        "cat-think-1",
        "Weighing the two consolidation seams: the fold skeleton is shared, but the member surfaces around it are not…",
        { done: false },
      ),
      thinking("cat-think-2", "A settled thought reads back from its disclosure.", {
        done: true,
      }),
    ],
    openIds: [],
  },
];

// --- rendering ----------------------------------------------------------------

/**
 * One scenario's feed HTML through the real projection pipeline — the same
 * wiring FeedRenderer.renderRestored applies, minus renderer-owned state
 * (drafts, selections) the catalogue does not exercise.
 */
export function renderScenarioHtml(sc: Scenario, isOpen: (id: string) => boolean): string {
  const gns = gnsFolds(sc.items);
  const visible = sc.items.filter((i) => !gns.folded.has(i));
  const part = partitionFeed(sc.items);
  const top = part.top.filter((i) => !gns.folded.has(i));
  const watchers = asyncMembersByBubble(visible, gns.byBubble);
  const finals = finalResponses(visible);
  const panels: PanelContext = {
    children: part.children,
    isOpen,
    watchers,
    gnsFolds: gns.byBubble,
    taskTail: (id) => sc.tails?.[id],
    agentTopbar: (agent) =>
      agentTopbarHtml(
        sc.items,
        agent,
        { agentsOpen: false, tasksOpen: false, tokensOpen: false },
        Date.now(),
      ),
  };
  return top
    .map((item) => `<div class="feed-item">${renderItem(item, undefined, finals, panels)}</div>`)
    .join("");
}

// --- dual-body comparison (deliberation aid, taxonomy 3.2) --------------------

/** The scenario registered under SLUG; throws when the catalogue lost it. */
function mustScenario(slug: string): Scenario {
  const sc = scenarios.find((s) => s.slug === slug);
  if (!sc) throw new Error(`catalogue scenario missing: ${slug}`);
  return sc;
}

/**
 * The workflow scenario stripped to its card, so the two shape renders
 * differ only in fold structure, never in surrounding feed chrome.
 */
function dualBodyItems(): ConversationItem[] {
  return mustScenario("workflow-journal").items.filter((i) => i.kind !== "user-turn");
}

/** Shape A — today's real render: two stacked folds, both open. */
export function dualBodyShapeAHtml(): string {
  const base = mustScenario("workflow-journal");
  const sc: Scenario = { ...base, items: dualBodyItems() };
  const open = new Set(base.openIds);
  return renderScenarioHtml(sc, (id) => open.has(id));
}

/**
 * Shape B — the PROPOSED merged panel, hand-composed from the same CSS
 * vocabulary and the same child renderers, since no production code draws
 * it yet: one fold whose sectioned body stacks the child feed above the
 * journal, under one combined ticker.
 */
export function dualBodyShapeBHtml(): string {
  const items = dualBodyItems();
  const part = partitionFeed(items);
  const panels: PanelContext = { children: part.children, isOpen: () => false };
  const children = part.children.get("cat-wf-1") ?? [];
  const childFeed = children
    .map((c) => `<div class="feed-child">${renderItem(c, undefined, undefined, panels)}</div>`)
    .join("");
  const wf = items.find((i): i is ToolItem => i.kind === "tool" && i.toolUseId === "cat-wf-1");
  if (!wf) throw new Error("workflow card missing from scenario");
  const journal = parseJournal(wf.taskOutput ?? "")
    .rows.map(
      (r) =>
        `<div class="stream-row"><span class="agent-dot agent-${r.status}" aria-hidden="true">●</span> <span class="tool-name">${escapeHtml(
          r.label,
        )}</span><span class="stream-detail">${escapeHtml(r.detail)}</span></div>`,
    )
    .join("");
  const ticker = `${activityTicker(children)} · workflow · review-changes · running`;
  return `<div class="feed-item">
    <div class="tool-card tool-generic">
      <div class="tool-head"><span class="tool-name">Workflow</span><span class="badge run"><span class="tool-spinner" aria-hidden="true"></span>running…</span></div>
      <div class="agent-activity open" data-panel-toggle="cat-dual-b">
        <div class="agent-ticker">${escapeHtml(ticker)} <span class="agent-caret" aria-hidden="true">▴</span></div>
        <div class="agent-panel">
          <div class="cat-panel-heading">activity</div>
          ${childFeed}
          <div class="cat-panel-heading">journal · wf-review-1</div>
          ${journal}
        </div>
      </div>
    </div>
  </div>`;
}

/** The 3.2 comparison section's inner HTML: the two shapes side by side. */
export function dualBodySectionHtml(): string {
  return `<h2 class="cat-title">3.2 · dual-body shapes compared</h2><p class="cat-blurb">${escapeHtml(
    "The same Workflow call rendered both ways, frozen open. Shape A stacks the activity fold and the journal fold: two chrome rows, two clicks, each ticker truthful to its own body. Shape B merges them into one panel: one click and one background, but section headings inside and a two-summaries-in-one ticker.",
  )}</p><div class="cat-row"><figure class="cat-variant"><figcaption>shape A — two stacked panels (today)</figcaption><div class="cat-feed">${dualBodyShapeAHtml()}</div></figure><figure class="cat-variant"><figcaption>shape B — one merged panel (proposal)</figcaption><div class="cat-feed">${dualBodyShapeBHtml()}</div></figure></div>`;
}

// --- interactive mounting -----------------------------------------------------

/**
 * Mount SC into HOST with its own open-fold state, wiring the same
 * delegated toggle the FeedRenderer uses (panelToggleTarget plus the
 * click-through guard), so every fold and badge behaves as it does live.
 */
export function mountScenario(
  host: HTMLElement,
  sc: Scenario,
  initialOpen: readonly string[],
): void {
  const open = new Set(initialOpen);
  const draw = (): void => {
    host.innerHTML = renderScenarioHtml(sc, (id) => open.has(id));
  };
  host.addEventListener("click", (e) => {
    const target = e.target as HTMLElement;
    const toggle = panelToggleTarget(
      target.closest("[data-panel-toggle]"),
      target.closest(`.${PANEL_CLASS}`),
    );
    if (!toggle || target.closest(CLICK_THROUGH_SELECTOR) !== null) return;
    const id = toggle.getAttribute("data-panel-toggle") ?? "";
    if (open.has(id)) {
      open.delete(id);
    } else {
      open.add(id);
    }
    draw();
  });
  draw();
}

/** Build the whole catalogue into ROOT: one section per scenario. */
export function buildCatalogue(root: HTMLElement): void {
  for (const sc of scenarios) {
    const section = document.createElement("section");
    section.className = "cat-section";
    section.id = `cat-${sc.slug}`;
    const variants: Array<{ label: string; open: readonly string[] }> =
      sc.openIds.length > 0
        ? [
            { label: "collapsed", open: [] },
            { label: "expanded", open: sc.openIds },
          ]
        : [{ label: "as rendered", open: [] }];
    section.innerHTML = `<h2 class="cat-title">${escapeHtml(sc.taxonomy)} · ${escapeHtml(
      sc.title,
    )}</h2><p class="cat-blurb">${escapeHtml(sc.blurb)}</p><div class="cat-row">${variants
      .map(
        (v) =>
          `<figure class="cat-variant"><figcaption>${escapeHtml(
            v.label,
          )}</figcaption><div class="cat-feed" data-variant="${escapeHtml(v.label)}"></div></figure>`,
      )
      .join("")}</div>`;
    root.appendChild(section);
    const feeds = section.querySelectorAll<HTMLElement>(".cat-feed");
    variants.forEach((v, i) => mountScenario(feeds[i], sc, v.open));
  }
  // The 3.2 dual-body comparison rides after the taxonomy sections: both
  // shapes render frozen open (the comparison is structural, not
  // interactive), so no scenario mounting applies.
  const dual = document.createElement("section");
  dual.className = "cat-section";
  dual.id = "cat-dual-body";
  dual.innerHTML = dualBodySectionHtml();
  root.appendChild(dual);
}

// Auto-mount when loaded as the catalogue page's entry (never in tests,
// which import the exports above without a #feed to build into).
if (typeof document !== "undefined") {
  const feed = document.getElementById("feed");
  if (feed !== null && document.title.includes("catalogue")) {
    buildCatalogue(feed);
  }
}
