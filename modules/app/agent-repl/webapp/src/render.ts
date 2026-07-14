/**
 * DOM renderers for conversation items. Component naming mirrors the
 * spec (§2.4–2.7): TextStream, Thinking, ToolCard/<Name>,
 * PermissionPrompt. The feed renderer reuses one element per item key
 * so streaming updates do not rebuild the whole list.
 */
import { SUBAGENT_TOOLS, SubagentEntry, agentsMenuHtml } from "./agents.js";
import { formatDuration, formatDurationCeil } from "./duration.js";
import { applyExpanded, expandedKeys, sectionsIn } from "./expand.js";
import { escapeHtml, highlightCode, languageForPath } from "./highlight.js";
import { inline, renderMarkdown } from "./markdown.js";
import { isMetapromptTree, renderTreeHtml } from "./metaprompt-tree.js";
import { ModelInfo, QueuedItem } from "./protocol.js";
import { isPinnedToBottom, parkAtTail } from "./scroll.js";
import { IDLE_LABEL, TIMER_SLOT } from "./timer.js";
import { blocksToText, isClearTurn, userTurnText } from "./turn.js";
import {
  CompactBoundaryItem,
  ConversationItem,
  ErrorItem,
  PermissionItem,
  ResultItem,
  RetryItem,
  StoreState,
  SystemItem,
  TextItem,
  ThinkingItem,
  ToolItem,
  UserTurnItem,
} from "./store.js";

export interface Actions {
  decidePermission(requestId: string, behavior: "allow" | "deny"): void;
  /**
   * Answer an AskUserQuestion permission. UPDATED-INPUT is the tool
   * input echoed back with the `answers` record filled in (question
   * text → answer string, multi-select comma-separated) — the exact
   * contract the CLI's permission component expects.
   */
  answerQuestions(requestId: string, updatedInput: unknown): void;
  /** Remove a parked queued message without ever running it (§2.13). */
  cancelQueued(queueId: string): void;
  /** Escalate a parked queued message to preempt the running turn (§2.13). */
  runQueuedNow(queueId: string): void;
}

/** One AskUserQuestion entry as carried in the tool input. */
export interface AskQuestion {
  question: string;
  header: string;
  multiSelect?: boolean;
  options: Array<{ label: string; description: string }>;
}

/** Selected option labels per question, keyed `${requestId} ${qIdx}`. */
export type QuestionSelections = ReadonlyMap<string, ReadonlySet<string>>;

/** The item's questions when it is an AskUserQuestion prompt, else null. */
export function askQuestions(item: PermissionItem): AskQuestion[] | null {
  if (item.toolName !== "AskUserQuestion") return null;
  const input = item.input as { questions?: AskQuestion[] } | null;
  return input && Array.isArray(input.questions) && input.questions.length > 0
    ? input.questions
    : null;
}

/** Tool names the SPA renders specially (§2.6); others use Generic. */
const SPECIAL_TOOLS = new Set([
  "Bash",
  "Read",
  "Edit",
  "Write",
  "Grep",
  "Skill",
  ...SUBAGENT_TOOLS,
]);

/**
 * Tool names whose cards are suppressed entirely: AskUserQuestion's UI
 * IS the permission picker card, ToolSearch is deferred-tool schema
 * plumbing, and TaskUpdate is task-list bookkeeping — all feed noise.
 */
const SUPPRESSED_TOOLS = new Set(["AskUserQuestion", "ToolSearch", "TaskUpdate"]);

function contentToText(
  content: string | Array<{ type: string; text?: string }>,
): string {
  if (typeof content === "string") return content;
  return content
    .filter((b) => b.type === "text")
    .map((b) => b.text ?? "")
    .join("\n");
}

// --- topbar chrome ------------------------------------------------------------

/** Token counts as the topbar and the result chip both write them: `300,000`. */
function formatTokens(n: number): string {
  return n.toLocaleString("en-US");
}

/**
 * Topbar session datapoints: `parent workspace: <ws> · time: <elapsed> ·
 * tokens: <n> · <k> agents ▾` (the vterm modeline's context mirror). The
 * parent workspace entry is omitted entirely when PARENT-WS is absent or
 * empty, as is the agents chip before the session spawns one; each value
 * gets its own color via the info-* classes.
 *
 * The model is NOT here. It moved into the #model-select picker, which
 * both names the live model and switches it — printing it again as text
 * immediately left of a dropdown showing the same thing is noise.
 *
 * AGENTS-OPEN is the caller's disclosure state for the agents chip, which
 * drops the subagent roster as an overlay.
 *
 * TIMER-LABEL is how long the running task has taken so far (`IDLE_LABEL`
 * when none is). Its span is marked so the caller's once-a-second tick can
 * repaint that one value without rewriting the whole strip — the two paint
 * paths agree because the tick writes exactly what this would have.
 *
 * CONTEXT-TOKENS is the conversation's CURRENT size (what the next request
 * will carry), never the session's cumulative spend. `null` means the size
 * is genuinely unknown — a `/clear` and a compaction each leave one behind
 * without reporting it — and prints as a dash rather than a lying `0`.
 */
export function sessionInfoHtml(
  parentWs: string | null,
  contextTokens: number | null,
  agents: readonly SubagentEntry[] = [],
  agentsOpen = false,
  timerLabel: string = IDLE_LABEL,
): string {
  const parts: string[] = [];
  if (parentWs) {
    parts.push(`parent workspace: <span class="info-ws">${escapeHtml(parentWs)}</span>`);
  }
  parts.push(
    `time: <span class="info-time" ${TIMER_SLOT}>${escapeHtml(timerLabel)}</span>`,
  );
  const tokens = contextTokens === null ? "—" : formatTokens(contextTokens);
  parts.push(`tokens: <span class="info-tokens">${tokens}</span>`);
  const menu = agentsMenuHtml(agents, agentsOpen);
  if (menu !== "") parts.push(menu);
  return parts.join(" · ");
}

/**
 * The compaction-in-progress banner: shown while the SDK is compacting the
 * conversation (a `compact-status` frame opened the window and the
 * `compact-boundary` closes it). The SDK reports no progress percentage, so
 * the bar is INDETERMINATE — a looping sweep, mirroring the CLI's own
 * "Compacting conversation…" spinner rather than pretending to a fraction it
 * cannot know.
 *
 * Returns "" when no compaction is running, so the caller can blindly write
 * the result into the banner slot and let an empty string collapse it.
 */
export function compactionBannerHtml(compacting: boolean): string {
  if (!compacting) return "";
  return (
    `<div class="compact-progress" role="status" aria-live="polite">` +
    `<span class="compact-progress-label">Compacting conversation…</span>` +
    `<span class="compact-progress-track"><span class="compact-progress-bar"></span></span>` +
    `</div>`
  );
}

/**
 * The #model-select options: the live model SELECTED, every alternative
 * the daemon offers, and nothing invented.
 *
 * Two cases the picker must not lie about:
 * - No model known yet (pre-hello): a disabled placeholder is selected,
 *   rather than letting the browser auto-select the first option and
 *   claim a model the session is not on.
 * - The live model is not in the menu (an id the CLI accepts but does not
 *   advertise): it is prepended as its own option, so the picker still
 *   names what the session is ACTUALLY running.
 */
export function modelOptionsHtml(
  models: readonly ModelInfo[],
  current: string,
): string {
  const opts: string[] = [];
  if (current === "") {
    opts.push(`<option value="" disabled selected>model…</option>`);
  } else if (!models.some((m) => m.value === current)) {
    opts.push(
      `<option value="${escapeHtml(current)}" selected>${escapeHtml(current)}</option>`,
    );
  }
  for (const m of models) {
    const selected = m.value === current ? " selected" : "";
    opts.push(
      `<option value="${escapeHtml(m.value)}"${selected} title="${escapeHtml(m.description)}">${escapeHtml(m.displayName)}</option>`,
    );
  }
  return opts.join("");
}

// --- per-item components ------------------------------------------------------

/**
 * Envelope ts (ISO8601, §2.1) as the local wall-clock time the user sees
 * on their own machine, zero-padded 24-hour `HH:MM` — the whole clock
 * face would drown out the prompt it labels.
 */
export function formatTurnTime(ts: string): string {
  const at = new Date(ts);
  const hh = String(at.getHours()).padStart(2, "0");
  const mm = String(at.getMinutes()).padStart(2, "0");
  return `${hh}:${mm}`;
}

/**
 * The one bubble shape, shared by the user's prompt and the agent's
 * response: a body column, then the turn's time stamped into the
 * top-right corner. CSS pins and shrinks the stamp (see `.turn-ts`); the
 * markup only has to hand it the same corner in both bubbles.
 *
 * The stamp holds its own flex column rather than floating over the body,
 * so a full-width response line never runs beneath it.
 */
function Bubble(cls: string, body: string, ts: string): string {
  return `<div class="${cls}"><div class="bubble-body">${body}</div><span class="turn-ts">${escapeHtml(
    formatTurnTime(ts),
  )}</span></div>`;
}

/**
 * The prompt bubble, breathing when PULSE-ID marks it the newest thing the
 * feed has drawn (see `pulseTarget`): a prompt whose turn has produced
 * nothing visible yet is the one moment the UI has no other motion to offer,
 * so the prompt itself carries the beat until something supersedes it.
 */
function UserTurn(item: UserTurnItem, isPulsing = false): string {
  const cls = `bubble user${isPulsing ? " pulsing" : ""}`;
  const divider = isClearTurn(item)
    ? `<div class="clear-divider" role="separator" aria-label="context cleared"></div>`
    : "";
  return `${Bubble(cls, `<pre>${escapeHtml(userTurnText(item))}</pre>`, item.ts)}${divider}`;
}

/**
 * §2.13 status/verdict badge for a queued item. `classifying` is the
 * pre-verdict state; `waiting` parks the message to drain in FIFO order;
 * `interrupt` escalates it to preempt the running turn.
 */
function queuedBadge(item: QueuedItem): { label: string; cls: string } {
  switch (item.status) {
    case "waiting":
      return { label: "queued — waiting", cls: "waiting" };
    case "interrupt":
      return { label: "interrupting…", cls: "interrupt" };
    case "classifying":
    default:
      return { label: "queued — classifying…", cls: "classifying" };
  }
}

/**
 * A parked message in the in-flight queue (§2.13). It is deliberately NOT
 * a `bubble user`: a subdued card, so the reader SEES the message is queued
 * for later and is explicitly not interrupting the running turn unless
 * escalated. It carries the prompt text (injected spans stripped, exactly
 * as a user turn's are), a status/verdict badge, the classifier's one-line
 * reason once known, and a Cancel / Run-now pair. Each button carries the
 * `queue_id` in a data attribute for delegated click handling.
 */
export function QueuedCard(item: QueuedItem): string {
  const badge = queuedBadge(item);
  const qid = escapeHtml(item.queue_id);
  const reason = item.reason
    ? `<div class="queued-reason">${escapeHtml(item.reason)}</div>`
    : "";
  return `
    <div class="queued-card">
      <div class="queued-head">
        <span class="queued-badge ${badge.cls}">${escapeHtml(badge.label)}</span>
      </div>
      <pre class="queued-content">${escapeHtml(blocksToText(item.content))}</pre>
      ${reason}
      <div class="queued-actions">
        <button data-queue-cancel="${qid}">Cancel</button>
        <button data-queue-run-now="${qid}">Run now</button>
      </div>
    </div>`;
}

/** Key identifying one queued card's DOM node across renders (§2.13). */
export function queuedCardKey(item: QueuedItem): string {
  return `queued:${item.queue_id}`;
}

/**
 * A turn's FINAL response gets the green border (§2.4): the answer the
 * turn actually landed on, set apart from the running commentary the
 * agent emits between its tool calls.
 *
 * CHIP is the closing chip of the turn that bubble answers, and it rides
 * INSIDE the bubble, along its bottom edge — the turn's ending belongs to
 * the answer it ended on rather than to a pill floating in the feed
 * beneath it. Only a final response carries one, so CHIP is null for the
 * commentary bubbles above it (see `finalResponses`).
 *
 * The working frontier gets the pulse instead (see `pulseTarget`). The
 * two never land on the same bubble: a final response only exists once the
 * turn has ended, and the pulse only runs while it has not.
 *
 * The bubble is stamped with the time the agent OPENED the block (the
 * `text-start` envelope), not the time it closed it: the stamp then dates
 * the response the same way the user bubble's dates the prompt, and it
 * does not jump while the block streams.
 */
function TextStream(item: TextItem, chip: ResultItem | null, isPulsing = false): string {
  const cursor = item.done ? "" : `<span class="cursor">▍</span>`;
  const cls = `bubble assistant md${chip ? " final-response" : ""}${
    isPulsing ? " pulsing" : ""
  }`;
  // A turn that closed in under a second reports no time worth a chip, so the
  // final-response badge is dropped entirely below that floor rather than
  // rounding a negligible span up to a `1s` the reader never asked for. The
  // bubble keeps its `final-response` wash regardless, since it is the answer
  // either way — only the timing pill is conditional.
  const closer =
    chip && chip.sincePrevFinalMs >= 1000
      ? ResultChip(chip, chip.sincePrevFinalMs, true)
      : "";
  // A bare metaprompt TLDR tree (no code fence — fenced trees are the
  // markdown fence handler's job) renders as hanging-indent tree lines;
  // the markdown pipeline would shear its wrapped branches to column 0.
  if (!item.text.includes("```") && isMetapromptTree(item.text)) {
    return Bubble(
      cls,
      `<div class="mp-tree">${renderTreeHtml(item.text, inline)}</div>${cursor}${closer}`,
      item.ts,
    );
  }
  return Bubble(cls, `${renderMarkdown(item.text)}${cursor}${closer}`, item.ts);
}

function Thinking(item: ThinkingItem): string {
  // Adaptive-thinking models withhold the thinking text: the block streams
  // a signature and no thinking_delta, so item.text stays empty. A
  // disclosure triangle over an empty <pre> unfolds to nothing, so a
  // textless block gets a spinner while it is open and, once it closes,
  // disappears entirely (`rendersEmpty`).
  if (item.text === "") {
    return `<div class="thinking-pending"><span class="thinking-spinner" aria-hidden="true"></span> thinking</div>`;
  }
  const state = item.done ? "" : " (thinking…)";
  return `
    <details class="thinking"${item.done ? "" : " open"}>
      <summary>Thinking${state}</summary>
      <pre>${escapeHtml(item.text)}</pre>
    </details>`;
}

// IS-PULSING breathes the card while it is the running frontier (see
// `pulseTarget`): a slow wash under the head's fast arc, so a call in flight
// is never a wholly still card during the second the arc stays hidden. The
// two motions live on different channels — the arc in the badge, the breath
// on the card background — so they read as one signal, not two competing ones.
function ToolCard(item: ToolItem, isPulsing = false): string {
  const variant = SPECIAL_TOOLS.has(item.toolName) ? item.toolName : "Generic";
  // In-flight is ONE look, whichever phase the call is in: the orange run
  // badge carrying the same arc the thinking indicator spins, held
  // invisible for its first second by CSS (see .tool-spinner) so the
  // sub-second tools — Edit, Read, most Bash — never flash it. Only the
  // badge's label distinguishes the two phases; a settled card drops the
  // badge for done/error. The card body carries no indicator of its own,
  // so the arc is the single place motion lives.
  const status = item.result
    ? item.result.isError
      ? `<span class="badge err">error</span>`
      : `<span class="badge ok">done</span>`
    : `<span class="badge run"><span class="tool-spinner" aria-hidden="true"></span>${
        item.inputDone ? "running…" : "streaming input…"
      }</span>`;
  const progress = item.progress
    ? `<div class="tool-progress">${escapeHtml(item.progress)}</div>`
    : "";
  return `
    <div class="tool-card tool-${variant.toLowerCase()}${isPulsing ? " pulsing" : ""}">
      <div class="tool-head"><span class="tool-name">${escapeHtml(item.toolName)}</span>${status}</div>
      ${toolInput(item)}
      ${progress}
      ${toolResult(item)}
    </div>`;
}

function toolInput(item: ToolItem): string {
  // While the input is still streaming, item.input is unparsed and the
  // only material is the accumulating RAW partial JSON — flashing that
  // before the pretty per-tool form renders reads as a glitch. The body
  // stays empty until tool-use-input-end lands: the head's running badge
  // (see ToolCard) is the sole in-progress indicator, so an in-body pulse
  // would only double up on the beat the arc already marks.
  if (!item.inputDone) return "";
  if (item.toolName === "Bash" && item.input && typeof item.input.command === "string") {
    // .bash-input caps the visible command at 5 lines, scrollable
    // independently of the output's own 5-line cap.
    return `<pre class="cmd bash-input">$ ${escapeHtml(item.input.command)}</pre>`;
  }
  if (
    (item.toolName === "Read" || item.toolName === "Write" || item.toolName === "Edit") &&
    item.input &&
    typeof item.input.file_path === "string"
  ) {
    return `<div class="file-path">${escapeHtml(item.input.file_path)}</div>`;
  }
  if (item.toolName === "Grep" && item.input && typeof item.input.pattern === "string") {
    return `<pre class="cmd">grep: ${escapeHtml(item.input.pattern)}</pre>`;
  }
  // A subagent call reads as its description alone; the prompt, model and
  // agent type stay folded into the JSON one click away. The description
  // class is .agent-input-desc, NOT the .agent-desc the topbar roster
  // already owns for its own rows — two different components.
  if (
    SUBAGENT_TOOLS.has(item.toolName) &&
    item.input &&
    typeof item.input.description === "string"
  ) {
    return `<div class="tool-input agent-input"><div class="file-path agent-input-desc">${escapeHtml(
      item.input.description,
    )}</div><pre class="agent-json">${escapeHtml(item.inputJson)}</pre></div>`;
  }
  // SendMessage renders summary-only: the UI-preview summary (plus the
  // recipient), never the full message body.
  if (item.toolName === "SendMessage" && item.input && typeof item.input.summary === "string") {
    const to = typeof item.input.to === "string" ? `→ ${item.input.to}: ` : "";
    return `<div class="file-path">${escapeHtml(`${to}${item.input.summary}`)}</div>`;
  }
  // Skill announces the launch and nothing else: the input's `args` is the
  // prompt the skill is handed, which the feed already carries as the user
  // turn or the agent's own text.
  if (item.toolName === "Skill" && item.input && typeof item.input.skill === "string") {
    return `<div class="skill-launch">Launching skill: ${escapeHtml(item.input.skill)}</div>`;
  }
  return `<pre class="tool-input">${escapeHtml(item.inputJson)}</pre>`;
}

function toolResult(item: ToolItem): string {
  if (!item.result) return "";
  const r = item.result.render;
  if (r) {
    switch (r.kind) {
      case "bash":
        // .bash-output caps the visible output at 5 lines, scrollable
        // independently of the command's own 5-line cap.
        return `<pre class="tool-output bash-output">${escapeHtml(r.stdout)}${
          r.stderr ? `\n<span class="stderr">${escapeHtml(r.stderr)}</span>` : ""
        }</pre>`;
      case "diff":
        // .diff-output caps the visible diff at 10 lines (the Read
        // preview's cap), scrollable for the rest.
        return `<pre class="diff diff-output">${diffHtml(r.unified_diff)}</pre>`;
      case "grep":
        return `<pre class="tool-output">${r.matches
          .map((m) => `${escapeHtml(m.file)}:${m.line}: ${escapeHtml(m.text)}`)
          .join("\n")}</pre>`;
      case "task":
        return `<pre class="tool-output">${escapeHtml(r.summary)}</pre>`;
    }
  }
  if (item.toolName === "Read" && !item.result.isError) {
    return readResultHtml(item, contentToText(item.result.content));
  }
  // SendMessage is summary-only: the successful delivery echo adds
  // nothing over the summary line. Errors still fall through below so
  // failures stay loud.
  if (item.toolName === "SendMessage" && !item.result.isError) {
    return "";
  }
  // Skill's success result is the very "Launching skill: <name>" line the
  // card's input already renders. Errors fall through below so a skill that
  // failed to launch stays loud.
  if (item.toolName === "Skill" && !item.result.isError) {
    return "";
  }
  return `<pre class="tool-output${item.result.isError ? " stderr" : ""}">${escapeHtml(
    contentToText(item.result.content),
  )}</pre>`;
}

/**
 * Read result preview. cat -n numbered output has each line's number
 * prefix lifted into a muted .line-no span so only the code text is
 * syntax-highlighted (language from the file_path extension, plain
 * escaped text when it is unknown); non-numbered content is
 * highlighted whole. Markdown files render as formatted markdown
 * instead of highlighted source — the number gutter is dropped there,
 * since rendered blocks have no line correspondence. In every form
 * .tool-read-output caps the visible height — the rest stays
 * reachable by scrolling.
 */
function readResultHtml(item: ToolItem, text: string): string {
  const path =
    item.input && typeof item.input.file_path === "string" ? item.input.file_path : "";
  const lang = languageForPath(path) ?? "";
  const lines = text.split("\n");
  const parts = lines.map((l) => l.match(/^(\s*\d+\t)(.*)$/));
  // Numbered when every line carries the cat -n prefix (a blank
  // trailing line from a final newline doesn't break the format).
  const numbered =
    parts.some((p) => p !== null) && parts.every((p, i) => p !== null || lines[i] === "");
  if (lang === "markdown") {
    const md = numbered ? parts.map((p, i) => (p ? p[2] : lines[i])).join("\n") : text;
    return `<div class="tool-output tool-read-output tool-read-md">${renderMarkdown(md)}</div>`;
  }
  if (!numbered) {
    return `<pre class="tool-output tool-read-output"><code class="hljs">${highlightCode(text, lang)}</code></pre>`;
  }
  const code = parts.map((p, i) => (p ? p[2] : lines[i])).join("\n");
  // hljs preserves newlines, so the highlighted HTML splits back into
  // the same number of lines; if that invariant ever breaks, keep the
  // prefixes but fall back to unhighlighted code.
  let codeLines = highlightCode(code, lang).split("\n");
  if (codeLines.length !== lines.length) {
    codeLines = parts.map((p, i) => escapeHtml(p ? p[2] : lines[i]));
  }
  const body = codeLines
    .map((h, i) => {
      const p = parts[i];
      return p ? `<span class="line-no">${escapeHtml(p[1])}</span>${h}` : h;
    })
    .join("\n");
  return `<pre class="tool-output tool-read-output"><code class="hljs">${body}</code></pre>`;
}

export function diffHtml(unifiedDiff: string): string {
  return unifiedDiff
    .split("\n")
    .map((line) => {
      const esc = escapeHtml(line);
      if (line.startsWith("+")) return `<span class="add">${esc}</span>`;
      if (line.startsWith("-")) return `<span class="del">${esc}</span>`;
      if (line.startsWith("@@")) return `<span class="hunk">${esc}</span>`;
      return esc;
    })
    .join("\n");
}

function PermissionPrompt(item: PermissionItem, selections?: QuestionSelections): string {
  const questions = askQuestions(item);
  if (questions) return QuestionPrompt(item, questions, selections);
  const preview = permissionPreviewHtml(item);
  if (item.resolution) {
    const label =
      item.resolution.decision === "cancel"
        ? `cancelled${item.resolution.message ? ` — ${escapeHtml(item.resolution.message)}` : ""}`
        : `${item.resolution.decision}ed`;
    return `
      <div class="permission resolved">
        <div class="perm-head">Permission: ${escapeHtml(item.toolName)} <span class="badge ${
          item.resolution.decision === "allow" ? "ok" : "err"
        }">${label}</span></div>
        ${preview}
      </div>`;
  }
  return `
    <div class="permission pending">
      <div class="perm-head">Allow ${escapeHtml(item.toolName)}?</div>
      ${preview}
      <div class="perm-actions">
        <button data-perm-allow="${escapeHtml(item.requestId)}">Allow</button>
        <button data-perm-deny="${escapeHtml(item.requestId)}">Deny</button>
      </div>
    </div>`;
}

/**
 * AskUserQuestion picker: option chips per question (single-select
 * replaces the pick, multiSelect toggles), a submit enabled once every
 * question has a selection, and a decline path. Selection state lives
 * in the RENDERER, not the DOM, so it survives per-delta re-renders.
 */
function QuestionPrompt(
  item: PermissionItem,
  questions: AskQuestion[],
  selections?: QuestionSelections,
): string {
  if (item.resolution) {
    const label =
      item.resolution.decision === "allow"
        ? "answered"
        : item.resolution.decision === "cancel"
          ? "cancelled"
          : "declined";
    return `
      <div class="permission resolved question">
        <div class="perm-head">Question <span class="badge ${
          item.resolution.decision === "allow" ? "ok" : "err"
        }">${label}</span></div>
        ${questions.map((q) => `<div class="q-text">${escapeHtml(q.question)}</div>`).join("")}
      </div>`;
  }
  const rid = escapeHtml(item.requestId);
  const blocks = questions.map((q, qi) => {
    const picked = selections?.get(`${item.requestId} ${qi}`);
    const opts = q.options
      .map(
        (o, oi) =>
          `<button class="q-opt${picked?.has(o.label) ? " selected" : ""}" data-q-req="${rid}" data-q-idx="${qi}" data-q-opt="${oi}" title="${escapeHtml(
            o.description,
          )}">${escapeHtml(o.label)}</button>`,
      )
      .join("");
    return `
      <div class="q-block">
        <span class="badge q-chip">${escapeHtml(q.header)}</span>
        <div class="q-text">${escapeHtml(q.question)}${
          q.multiSelect ? ` <span class="q-multi">(select all that apply)</span>` : ""
        }</div>
        <div class="q-opts">${opts}</div>
      </div>`;
  });
  const complete = questions.every(
    (_q, qi) => (selections?.get(`${item.requestId} ${qi}`)?.size ?? 0) > 0,
  );
  return `
    <div class="permission pending question">
      ${blocks.join("")}
      <div class="perm-actions">
        <button data-q-submit="${rid}"${complete ? "" : " disabled"}>Answer</button>
        <button data-perm-deny="${rid}">Decline</button>
      </div>
    </div>`;
}

function permissionPreviewHtml(item: PermissionItem): string {
  const p = item.preview;
  if (!p) return "";
  switch (p.kind) {
    case "bash":
      return `<pre class="cmd">$ ${escapeHtml(p.command)}</pre>`;
    case "diff":
      return `<div class="file-path">${escapeHtml(p.file_path)}</div><pre class="diff">${diffHtml(p.unified_diff)}</pre>`;
    case "write":
      return `<div class="file-path">${escapeHtml(p.file_path)} (${p.bytes} bytes)</div><pre class="tool-output">${escapeHtml(p.preview)}</pre>`;
    case "generic":
      return `<pre class="tool-output">${escapeHtml(p.summary)}</pre>`;
  }
}

/**
 * Whether a result ends its turn normally: an aborted or errored turn was
 * cut off partway, so it never reached the answer it was working toward.
 */
function isTurnComplete(item: ResultItem): boolean {
  return item.subtype === "success";
}

/**
 * The feed's final responses, seen from both ends of the pairing they
 * create between a turn's answer and the chip that closes the turn.
 */
export interface FinalResponses {
  /** Final-response block id → the closing chip that bubble swallows. */
  readonly chips: ReadonlyMap<string, ResultItem>;
  /** The results a bubble swallowed, which the feed no longer prints itself. */
  readonly swallowed: ReadonlySet<ResultItem>;
}

/**
 * The text bubbles that CONCLUDE a completed turn — the agent's final
 * response to a prompt, as against the commentary it emits between tool
 * calls — each paired with the `result` that closes its turn. A turn's
 * final response is its last text block before the `result`, and only a
 * turn that ran to completion has one: an aborted or errored turn's last
 * text is a severed thought, not an answer. A turn still streaming has no
 * final response either, since its next block could always continue it.
 *
 * The pairing is what nests a completed turn's chip inside its answer.
 * A completed turn that wrote no answer to nest it in (one that only ran
 * tools) pairs with nothing, so its chip keeps printing standalone.
 */
export function finalResponses(items: readonly ConversationItem[]): FinalResponses {
  const chips = new Map<string, ResultItem>();
  const swallowed = new Set<ResultItem>();
  let lastText: string | null = null;
  for (const item of items) {
    if (item.kind === "user-turn") {
      lastText = null;
    } else if (item.kind === "text") {
      lastText = item.blockId;
    } else if (item.kind === "result") {
      if (lastText !== null && isTurnComplete(item)) {
        chips.set(lastText, item);
        swallowed.add(item);
      }
      lastText = null;
    }
  }
  return { chips, swallowed };
}

/**
 * Whether an item draws NOTHING into the feed.
 *
 * The renderer and the pulse both need this same answer, from opposite ends:
 * `renderItem` returns the empty node, and `pulseTarget` scans straight past
 * it, since a thing the feed never drew cannot be the progress that
 * supersedes a breathing bubble. Keeping the two on one predicate is what
 * stops a suppressed tool from silently stilling a pulse it never replaced.
 *
 * FINALS is needed only for a result, whose chip rides inside the response
 * bubble that swallowed it rather than standing alone in the feed.
 */
export function rendersEmpty(
  item: ConversationItem,
  finals?: FinalResponses,
): boolean {
  switch (item.kind) {
    // A turn that was nothing BUT injected spans has no bubble at all.
    case "user-turn":
      return userTurnText(item) === "";
    // A textless thinking block leaves nothing behind once it closes.
    case "thinking":
      return item.done && item.text === "";
    // AskUserQuestion's UI IS the permission picker card — the generic tool
    // card would just dump the questions JSON (input) and the "User has
    // answered…" echo (result) alongside it. ToolSearch is deferred-tool
    // schema plumbing and TaskUpdate is task-list bookkeeping: both pure
    // feed noise.
    case "tool":
      return SUPPRESSED_TOOLS.has(item.toolName);
    case "result":
      return finals?.swallowed.has(item) ?? false;
    default:
      return false;
  }
}

/** The section that BREATHES, or null when none does. */
export type PulseTarget =
  | { kind: "user-turn"; requestId: string }
  | { kind: "text"; blockId: string }
  | { kind: "tool"; toolUseId: string }
  | null;

/**
 * The one section that breathes: the newest thing the UI is waiting on, so a
 * running session always shows a single slow beat somewhere at its tail.
 *
 * Exactly one of these takes the breath, in tail-first precedence:
 * - a RUNNING TOOL CARD at the tail breathes until its badge flips to
 *   done/error, covering the second its arc spends deliberately hidden (see
 *   `.tool-spinner`) so a call in flight is never a wholly still card.
 * - the WORKING FRONTIER (the last response a still-running turn has
 *   finished) breathes once the agent has written something, so a reader who
 *   has caught up sees it is still writing rather than that it went quiet.
 * - the PROMPT breathes from the moment it is sent until the turn draws its
 *   first visible thing, so a send is never answered by a still feed.
 *
 * The states that forfeit the pulse entirely, each because something else
 * already carries the beat (or because there is no beat to carry):
 * - the turn is not in flight, so nothing more is coming at all;
 * - a response is streaming, and its own cursor is the live signal;
 * - a thinking indicator is running, and its spinner is the live signal;
 * - a settled section (a done tool, a retry, an error) followed the prompt
 *   before the agent wrote a word, and IT is now the progress on show.
 *
 * The scan stops at the newest user turn: the previous turn's answer belongs
 * to a question already answered, so a fresh prompt never breathes life back
 * into it. What the feed never drew (`rendersEmpty`) is skipped outright — a
 * pulse is stilled only by progress the user can actually see.
 */
export function pulseTarget(
  items: readonly ConversationItem[],
  turnInFlight: boolean,
  finals?: FinalResponses,
): PulseTarget {
  if (!turnInFlight) return null;
  // Whether the cursor is still at the feed's visible tail, i.e. nothing
  // drawn has been passed on the way back to the item under inspection.
  let atTail = true;
  for (let i = items.length - 1; i >= 0; i--) {
    const item = items[i];
    if (rendersEmpty(item, finals)) continue;
    if (item.kind === "user-turn") {
      return atTail ? { kind: "user-turn", requestId: item.requestId } : null;
    }
    if (item.kind === "thinking" && !item.done) return null;
    if (item.kind === "text") {
      return item.done ? { kind: "text", blockId: item.blockId } : null;
    }
    // A running tool card (no result badge yet) at the tail IS the frontier:
    // it breathes until its badge flips to done/error, filling the second its
    // arc stays hidden. A card the agent has moved past (something visible
    // follows it) is history, so the frontier falls back to the text above.
    if (item.kind === "tool" && atTail && !item.result) {
      return { kind: "tool", toolUseId: item.toolUseId };
    }
    atTail = false;
  }
  return null;
}

/** Whether PULSE names this item as the section that breathes. */
export function isPulsed(item: ConversationItem, pulse: PulseTarget): boolean {
  if (!pulse) return false;
  if (pulse.kind === "text") {
    return item.kind === "text" && item.blockId === pulse.blockId;
  }
  if (pulse.kind === "tool") {
    return item.kind === "tool" && item.toolUseId === pulse.toolUseId;
  }
  return item.kind === "user-turn" && item.requestId === pulse.requestId;
}

/** A context increase as a signed figure: `+100,000`, `-40,000`, `+0`. */
function formatTokenDelta(n: number): string {
  return `${n < 0 ? "" : "+"}${formatTokens(n)}`;
}

/**
 * A turn's closing chip: how long the turn took, then where the session's
 * input tokens now stand and how far this turn moved them.
 *
 * DURATION-MS is the span the chip reports, which differs by where the chip
 * is drawn: a final-response chip reads the turn's elapsed time SINCE THE
 * PREVIOUS final response (`sincePrevFinalMs`), while a standalone chip
 * reads the SDK's whole-task figure (`durationMs`). The caller passes
 * whichever it means, so this renderer never has to know which it is.
 *
 * A completed turn is labelled by the chip's own wash rather than by a
 * word, so only a turn that ended some OTHER way (aborted, errored) names
 * its subtype. A turn that ended with the context size unknown (a
 * `/clear` or a `/compact`) reports the duration alone, since the figure
 * it would otherwise print is the one the turn just invalidated.
 *
 * A completed turn's chip is drawn INSIDE the final response it closes
 * (see `TextStream`), so the feed prints the chip standalone only when no
 * bubble swallowed it: an aborted or errored turn, or a completed turn
 * that wrote no answer at all.
 *
 * SECOND-RESOLUTION rounds the duration up to whole seconds (`6s`, not
 * `5s 984ms`): the nested final-response badge reports a settled turn to a
 * reader who never needs the millisecond digit, whereas the standalone chip
 * keeps the SDK's exact figure so a sub-second abort still reads its span.
 */
function ResultChip(
  item: ResultItem,
  durationMs: number,
  secondResolution = false,
): string {
  const done = isTurnComplete(item);
  const parts = done ? [] : [escapeHtml(item.subtype)];
  parts.push(
    secondResolution ? formatDurationCeil(durationMs) : formatDuration(durationMs),
  );
  if (item.context) {
    parts.push(`${formatTokens(item.context.total)} in`);
    parts.push(formatTokenDelta(item.context.delta));
  }
  return `
    <div class="result ${item.isError ? "err" : "ok"}${done ? " done" : ""}">
      ${parts.join(" · ")}
    </div>`;
}

function CompactDivider(item: CompactBoundaryItem): string {
  return `<div class="compact-divider">— context compacted (${escapeHtml(item.trigger)}, ${item.preTokens} tokens before) —</div>`;
}

function ErrorBanner(item: ErrorItem): string {
  return `<div class="error-banner">[${escapeHtml(item.code)}] ${escapeHtml(item.message)}${
    item.recoverable ? "" : " (fatal)"
  }</div>`;
}

function RetryBadge(item: RetryItem): string {
  return `<div class="retry-badge">retrying (attempt ${item.attempt}): ${escapeHtml(item.reason)}</div>`;
}

function SystemNote(item: SystemItem): string {
  return `<div class="system-note">system: ${escapeHtml(item.subtype)}</div>`;
}

/**
 * One item's HTML, or nothing at all for the items the feed draws no node
 * for (`rendersEmpty`). FINALS pairs the feed's answers with the chips that
 * close their turns: a text block it names renders as a final response
 * carrying its chip, and the paired result renders as nothing, since the
 * bubble above it has already drawn it.
 *
 * IS-PULSING marks the one section that breathes (`pulseTarget`), which is
 * the prompt, a finished response, or a running tool card.
 */
export function renderItem(
  item: ConversationItem,
  selections?: QuestionSelections,
  finals?: FinalResponses,
  isPulsing = false,
): string {
  if (rendersEmpty(item, finals)) return "";
  switch (item.kind) {
    case "user-turn":
      return UserTurn(item, isPulsing);
    case "text":
      return TextStream(item, finals?.chips.get(item.blockId) ?? null, isPulsing);
    case "thinking":
      return Thinking(item);
    case "tool":
      return ToolCard(item, isPulsing);
    case "permission":
      return PermissionPrompt(item, selections);
    case "result":
      return ResultChip(item, item.durationMs);
    case "compact-boundary":
      return CompactDivider(item);
    case "error":
      return ErrorBanner(item);
    case "retry":
      return RetryBadge(item);
    case "system":
      return SystemNote(item);
  }
}

/** Key identifying one item's DOM node across renders. */
export function itemKey(item: ConversationItem, index: number): string {
  switch (item.kind) {
    case "text":
    case "thinking":
      return `${item.kind}:${item.blockId}`;
    case "tool":
      return `tool:${item.toolUseId}`;
    case "permission":
      return `perm:${item.requestId}`;
    default:
      return `${item.kind}:${index}`;
  }
}

/**
 * Request id of the newest user turn in the feed, or null when there is
 * none. A change in this id between renders means a prompt was just
 * sent — from the webapp composer or from the Emacs host's input buffer
 * alike, since both reach the feed as the daemon's `user-turn`
 * broadcast — which is what re-pins a scrolled-up feed to its tail.
 */
export function lastUserTurnId(items: readonly ConversationItem[]): string | null {
  for (let i = items.length - 1; i >= 0; i--) {
    const item = items[i];
    if (item.kind === "user-turn") return item.requestId;
  }
  return null;
}

/**
 * Whether a render must land the feed on its tail.
 *
 * A feed already parked at the bottom keeps following new content, as
 * ever. The addition: a user turn the previous render had not seen means
 * a prompt was JUST sent, and a sender wants to watch the answer — so the
 * feed jumps to the tail even from a scrolled-up position.
 */
export function repinsToTail(opts: {
  prevTurnId: string | null;
  nextTurnId: string | null;
  pinned: boolean;
}): boolean {
  if (opts.nextTurnId !== null && opts.nextTurnId !== opts.prevTurnId) return true;
  return opts.pinned;
}

/** Items filled per backfill step during a restored-session render. */
export const BACKFILL_CHUNK = 40;

/**
 * Tail-first fill order for a restored session: index groups from the
 * last item backwards, so the newest content renders first and older
 * items backfill upwards. Indices within a group stay ascending (fill
 * order inside one synchronous chunk is invisible).
 */
export function backfillChunks(count: number, chunkSize: number): number[][] {
  const chunks: number[][] = [];
  for (let end = count; end > 0; end -= chunkSize) {
    const start = Math.max(0, end - chunkSize);
    const chunk: number[] = [];
    for (let i = start; i < end; i++) chunk.push(i);
    chunks.push(chunk);
  }
  return chunks;
}

/**
 * Feed renderer: reconciles the item list into `container`, reusing
 * nodes by key and only rewriting nodes whose HTML changed.
 */
export class FeedRenderer {
  private container: HTMLElement;
  private actions: Actions;
  private nodes = new Map<string, { el: HTMLElement; html: string }>();
  /** AskUserQuestion picks (renderer-owned so re-renders keep them). */
  private questionSelections = new Map<string, Set<string>>();
  private lastState: StoreState | null = null;
  /** Pending bottom-up fill steps from renderRestored, oldest last. */
  private backfillQueue: Array<() => void> = [];
  /** Newest user turn seen by a render, so the next one spots a fresh send. */
  private lastUserTurn: string | null = null;

  constructor(container: HTMLElement, actions: Actions) {
    this.container = container;
    this.actions = actions;
    container.addEventListener("click", (e) => {
      const target = e.target as HTMLElement;
      const allow = target.getAttribute("data-perm-allow");
      const deny = target.getAttribute("data-perm-deny");
      if (allow) this.actions.decidePermission(allow, "allow");
      if (deny) this.actions.decidePermission(deny, "deny");
      const qReq = target.getAttribute("data-q-req");
      if (qReq !== null) {
        this.toggleQuestionOption(
          qReq,
          Number(target.getAttribute("data-q-idx")),
          Number(target.getAttribute("data-q-opt")),
        );
      }
      const submit = target.getAttribute("data-q-submit");
      if (submit !== null) this.submitQuestionAnswers(submit);
      // §2.13 queued-card controls: cancel drops the parked message,
      // run-now escalates it to preempt the running turn.
      const cancelQ = target.getAttribute("data-queue-cancel");
      if (cancelQ !== null) this.actions.cancelQueued(cancelQ);
      const runNowQ = target.getAttribute("data-queue-run-now");
      if (runNowQ !== null) this.actions.runQueuedNow(runNowQ);
    });
  }

  private pendingQuestionItem(requestId: string): { item: PermissionItem; questions: AskQuestion[] } | null {
    const item = this.lastState?.items.find(
      (it): it is PermissionItem => it.kind === "permission" && it.requestId === requestId,
    );
    if (!item || item.resolution) return null;
    const questions = askQuestions(item);
    return questions ? { item, questions } : null;
  }

  private toggleQuestionOption(requestId: string, qIdx: number, optIdx: number): void {
    const found = this.pendingQuestionItem(requestId);
    const opt = found?.questions[qIdx]?.options[optIdx];
    if (!found || !opt) return;
    const key = `${requestId} ${qIdx}`;
    const set = this.questionSelections.get(key) ?? new Set<string>();
    if (found.questions[qIdx].multiSelect) {
      if (set.has(opt.label)) {
        set.delete(opt.label);
      } else {
        set.add(opt.label);
      }
    } else {
      set.clear();
      set.add(opt.label);
    }
    this.questionSelections.set(key, set);
    if (this.lastState) this.render(this.lastState);
  }

  /**
   * One item's HTML: green-bordered and chip-bearing when FINALS marks it a
   * turn's answer, and breathing when PULSE names it.
   */
  private itemHtml(
    item: ConversationItem,
    finals: FinalResponses,
    pulse: PulseTarget,
  ): string {
    return renderItem(item, this.questionSelections, finals, isPulsed(item, pulse));
  }

  private submitQuestionAnswers(requestId: string): void {
    const found = this.pendingQuestionItem(requestId);
    if (!found) return;
    const answers: Record<string, string> = {};
    for (let qi = 0; qi < found.questions.length; qi++) {
      const set = this.questionSelections.get(`${requestId} ${qi}`);
      if (!set || set.size === 0) return; // incomplete (button disabled)
      answers[found.questions[qi].question] = [...set].join(", ");
    }
    for (let qi = 0; qi < found.questions.length; qi++) {
      this.questionSelections.delete(`${requestId} ${qi}`);
    }
    this.actions.answerQuestions(requestId, {
      questions: found.questions,
      answers,
    });
  }

  /**
   * Restored-session render (§2.10 fresh-join replay): builds every
   * item's shell, fills the NEWEST chunk synchronously, jumps straight
   * to the bottom, and backfills older items upwards across animation
   * frames — the latest message is visible immediately with no
   * top-down build crawl and no scroll animation. Scroll position is
   * compensated per chunk so the view never moves off the tail.
   */
  renderRestored(state: StoreState): void {
    this.lastState = state;
    // Replayed history's newest prompt is old news: banking it here keeps
    // the next render from reading it as a fresh send and yanking a feed
    // the user has meanwhile scrolled up.
    this.lastUserTurn = lastUserTurnId(state.items);
    this.backfillQueue = [];
    this.container.innerHTML = "";
    this.nodes.clear();
    const finals = finalResponses(state.items);
    const pulse = pulseTarget(state.items, state.turnInFlight, finals);
    const shells: Array<{ el: HTMLElement; item: ConversationItem }> = [];
    state.items.forEach((item, i) => {
      const key = itemKey(item, i);
      const el = document.createElement("div");
      el.className = "feed-item";
      el.dataset.key = key;
      this.container.appendChild(el);
      this.nodes.set(key, { el, html: "" });
      shells.push({ el, item });
    });
    // The parked queue (§2.13) renders in full at the tail — a fresh join
    // with a pending queue must show it, and the cards are cheap enough to
    // skip the tail-first backfill the history items get.
    state.queued.forEach((q) => {
      const key = queuedCardKey(q);
      const el = document.createElement("div");
      el.className = "feed-item";
      el.dataset.key = key;
      const html = QueuedCard(q);
      el.innerHTML = html;
      this.container.appendChild(el);
      this.nodes.set(key, { el, html });
    });
    const fillChunk = (indexes: number[]): void => {
      const before = this.container.scrollHeight;
      for (const i of indexes) {
        const { el, item } = shells[i];
        const html = this.itemHtml(item, finals, pulse);
        el.innerHTML = html;
        const entry = this.nodes.get(el.dataset.key ?? "");
        if (entry) entry.html = html;
      }
      // Content added above the viewport grows scrollHeight; shift
      // scrollTop by the growth so the tail stays in view.
      this.container.scrollTop += this.container.scrollHeight - before;
    };
    const chunks = backfillChunks(shells.length, BACKFILL_CHUNK);
    if (chunks.length > 0) fillChunk(chunks[0]);
    parkAtTail(this.container);
    this.backfillQueue = chunks.slice(1).map((c) => () => fillChunk(c));
    this.scheduleBackfill();
  }

  private scheduleBackfill(): void {
    if (this.backfillQueue.length === 0) return;
    requestAnimationFrame(() => {
      const step = this.backfillQueue.shift();
      if (step) step();
      this.scheduleBackfill();
    });
  }

  /**
   * Complete any in-flight backfill synchronously. render() must
   * reconcile against fully-materialized nodes — an empty shell's
   * cached "" html would otherwise re-fill mid-reconcile anyway, just
   * unpredictably interleaved.
   */
  private flushBackfill(): void {
    while (this.backfillQueue.length > 0) {
      const step = this.backfillQueue.shift();
      if (step) step();
    }
  }

  render(state: StoreState): void {
    this.flushBackfill();
    this.lastState = state;
    const turnId = lastUserTurnId(state.items);
    const toTail = repinsToTail({
      prevTurnId: this.lastUserTurn,
      nextTurnId: turnId,
      pinned: isPinnedToBottom(this.container),
    });
    this.lastUserTurn = turnId;
    const finals = finalResponses(state.items);
    const pulse = pulseTarget(state.items, state.turnInFlight, finals);
    const seen = new Set<string>();
    state.items.forEach((item, i) => {
      const key = itemKey(item, i);
      seen.add(key);
      const html = this.itemHtml(item, finals, pulse);
      let entry = this.nodes.get(key);
      if (!entry) {
        const el = document.createElement("div");
        el.className = "feed-item";
        el.dataset.key = key;
        this.container.appendChild(el);
        entry = { el, html: "" };
        this.nodes.set(key, entry);
      }
      if (entry.html !== html) {
        // A section the user clicked open outlives the re-render of the
        // item that carries it: a running tool card rewrites its whole
        // body when its result lands, which would otherwise re-cap a
        // command the user had just expanded.
        const open = expandedKeys(sectionsIn(entry.el));
        entry.el.innerHTML = html;
        applyExpanded(sectionsIn(entry.el), open);
        entry.html = html;
      }
    });
    // The in-flight queue (§2.13) is a subdued section at the tail, after
    // every conversation item. Each card is re-appended so a live item node
    // appended above (the agent's streaming response during the running
    // turn) never lands beneath a parked message.
    state.queued.forEach((q) => {
      const key = queuedCardKey(q);
      seen.add(key);
      const html = QueuedCard(q);
      let entry = this.nodes.get(key);
      if (!entry) {
        const el = document.createElement("div");
        el.className = "feed-item";
        el.dataset.key = key;
        entry = { el, html: "" };
        this.nodes.set(key, entry);
      }
      if (entry.html !== html) {
        entry.el.innerHTML = html;
        entry.html = html;
      }
      this.container.appendChild(entry.el);
    });
    for (const [key, entry] of this.nodes) {
      if (!seen.has(key)) {
        entry.el.remove();
        this.nodes.delete(key);
      }
    }
    if (toTail) {
      parkAtTail(this.container);
    }
  }
}
