/**
 * DOM renderers for conversation items. Component naming mirrors the
 * spec (§2.4–2.7): TextStream, Thinking, ToolCard/<Name>,
 * PermissionPrompt. The feed renderer reuses one element per item key
 * so streaming updates do not rebuild the whole list.
 */
import { applyExpanded, expandedIndexes, sectionsIn } from "./expand.js";
import { escapeHtml, highlightCode, languageForPath } from "./highlight.js";
import { inline, renderMarkdown } from "./markdown.js";
import { stripMetaSpans } from "./meta.js";
import { isMetapromptTree, renderTreeHtml } from "./metaprompt-tree.js";
import { Usage } from "./protocol.js";
import { isPinnedToBottom, parkAtTail } from "./scroll.js";
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
const SPECIAL_TOOLS = new Set(["Bash", "Read", "Edit", "Write", "Grep", "Task"]);

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

/** Context usage for the topbar: input + cache read + cache creation. */
function contextTokens(usage: Usage | null): number {
  if (!usage) return 0;
  return (
    usage.input_tokens +
    (usage.cache_read_input_tokens ?? 0) +
    (usage.cache_creation_input_tokens ?? 0)
  );
}

/**
 * Topbar session datapoints: `parent workspace: <ws> · model: <m> ·
 * tokens: <n>` (the vterm modeline's context mirror). The parent
 * workspace entry is omitted entirely when PARENT-WS is absent or
 * empty, as is model before hello delivers one; each value gets its
 * own color via the info-* classes.
 */
export function sessionInfoHtml(
  parentWs: string | null,
  model: string,
  usage: Usage | null,
): string {
  const parts: string[] = [];
  if (parentWs) {
    parts.push(`parent workspace: <span class="info-ws">${escapeHtml(parentWs)}</span>`);
  }
  if (model !== "") {
    parts.push(`model: <span class="info-model">${escapeHtml(model)}</span>`);
  }
  parts.push(
    `tokens: <span class="info-tokens">${contextTokens(usage).toLocaleString("en-US")}</span>`,
  );
  return parts.join(" · ");
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
 * A user turn's prompt text, non-text blocks standing in as `[kind]`.
 *
 * The host's injected spans (the metaprompt read-directive, the
 * workspace-generation preamble and wrap-up gate) are marked at their
 * injection site and dropped here: a user turn reads as the user's own
 * words, both in the bubble and to `isClearTurn`.
 */
function userTurnText(item: UserTurnItem): string {
  return stripMetaSpans(
    item.content
      .map((b) => (b.type === "text" ? String((b as { text: string }).text) : `[${b.type}]`))
      .join("\n"),
  );
}

/**
 * Whether a user turn is the `/clear` command — the prompt that drops the
 * CLI's context and re-inits the session. Its bubble carries the context
 * boundary rule beneath it, splitting the discarded conversation above from
 * the re-initialized one (`system: init`, then a contextless reply) below.
 */
export function isClearTurn(item: UserTurnItem): boolean {
  return userTurnText(item).trim() === "/clear";
}

function UserTurn(item: UserTurnItem): string {
  // A turn that was nothing BUT injected spans has no bubble at all.
  const text = userTurnText(item);
  if (text === "") return "";
  const divider = isClearTurn(item)
    ? `<div class="clear-divider" role="separator" aria-label="context cleared"></div>`
    : "";
  return `<div class="bubble user"><pre>${escapeHtml(text)}</pre><span class="turn-ts">${escapeHtml(
    formatTurnTime(item.ts),
  )}</span></div>${divider}`;
}

/**
 * A turn's FINAL response gets the green border (§2.4): the answer the
 * turn actually landed on, set apart from the running commentary the
 * agent emits between its tool calls.
 */
function TextStream(item: TextItem, isFinal: boolean): string {
  const cursor = item.done ? "" : `<span class="cursor">▍</span>`;
  const cls = `bubble assistant md${isFinal ? " final-response" : ""}`;
  // A bare metaprompt TLDR tree (no code fence — fenced trees are the
  // markdown fence handler's job) renders as hanging-indent tree lines;
  // the markdown pipeline would shear its wrapped branches to column 0.
  if (!item.text.includes("```") && isMetapromptTree(item.text)) {
    return `<div class="${cls}"><div class="mp-tree">${renderTreeHtml(
      item.text,
      inline,
    )}</div>${cursor}</div>`;
  }
  return `<div class="${cls}">${renderMarkdown(item.text)}${cursor}</div>`;
}

function Thinking(item: ThinkingItem): string {
  // Adaptive-thinking models withhold the thinking text: the block streams
  // a signature and no thinking_delta, so item.text stays empty. A
  // disclosure triangle over an empty <pre> unfolds to nothing, so a
  // textless block gets a spinner while it is open and disappears once it
  // closes.
  if (item.text === "") {
    return item.done
      ? ""
      : `<div class="thinking-pending"><span class="thinking-spinner" aria-hidden="true"></span> thinking</div>`;
  }
  const state = item.done ? "" : " (thinking…)";
  return `
    <details class="thinking"${item.done ? "" : " open"}>
      <summary>Thinking${state}</summary>
      <pre>${escapeHtml(item.text)}</pre>
    </details>`;
}

function ToolCard(item: ToolItem): string {
  const variant = SPECIAL_TOOLS.has(item.toolName) ? item.toolName : "Generic";
  // A call whose input has landed but whose result has not is the one card
  // state with no motion of its own: the input phase already pulses •••,
  // and a settled card carries done/error. So the running badge grows the
  // same arc the thinking indicator spins, held invisible for its first
  // second by CSS (see .tool-spinner), which keeps the sub-second tools —
  // Edit, Read, most Bash — from flashing it.
  const status = item.result
    ? item.result.isError
      ? `<span class="badge err">error</span>`
      : `<span class="badge ok">done</span>`
    : item.inputDone
      ? `<span class="badge run"><span class="tool-spinner" aria-hidden="true"></span>running…</span>`
      : `<span class="badge run">streaming input…</span>`;
  const progress = item.progress
    ? `<div class="tool-progress">${escapeHtml(item.progress)}</div>`
    : "";
  return `
    <div class="tool-card tool-${variant.toLowerCase()}">
      <div class="tool-head"><span class="tool-name">${escapeHtml(item.toolName)}</span>${status}</div>
      ${toolInput(item)}
      ${progress}
      ${toolResult(item)}
    </div>`;
}

function toolInput(item: ToolItem): string {
  // While the input is still streaming, item.input is unparsed and the
  // only material is the accumulating RAW partial JSON — flashing that
  // before the pretty per-tool form renders reads as a glitch. Show a
  // quiet preparing indicator until tool-use-input-end lands.
  if (!item.inputDone) {
    return `<div class="tool-input-pending"><span class="pulse">•••</span></div>`;
  }
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
  if (item.toolName === "Task" && item.input && typeof item.input.description === "string") {
    return `<div class="file-path">${escapeHtml(item.input.description)}</div>`;
  }
  // SendMessage renders summary-only: the UI-preview summary (plus the
  // recipient), never the full message body.
  if (item.toolName === "SendMessage" && item.input && typeof item.input.summary === "string") {
    const to = typeof item.input.to === "string" ? `→ ${item.input.to}: ` : "";
    return `<div class="file-path">${escapeHtml(`${to}${item.input.summary}`)}</div>`;
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
 * Block ids of the text bubbles that CONCLUDE a completed turn — the
 * agent's final response to a prompt, as against the commentary it emits
 * between tool calls. A turn's final response is its last text block
 * before the `result`, and only a turn that ran to completion has one: an
 * aborted or errored turn's last text is a severed thought, not an answer.
 * A turn still streaming has no final response either, since its next
 * block could always continue it.
 */
export function finalResponseBlockIds(items: readonly ConversationItem[]): Set<string> {
  const finals = new Set<string>();
  let lastText: string | null = null;
  for (const item of items) {
    if (item.kind === "user-turn") {
      lastText = null;
    } else if (item.kind === "text") {
      lastText = item.blockId;
    } else if (item.kind === "result") {
      if (lastText !== null && isTurnComplete(item)) finals.add(lastText);
      lastText = null;
    }
  }
  return finals;
}

/**
 * Duration units, coarsest-last. MAX is the exclusive ceiling on the
 * unit's rendered magnitude: once a value rounds up to it, the next unit
 * carries the duration instead.
 */
const DURATION_UNITS: ReadonlyArray<{ suffix: string; scale: number; max: number }> = [
  { suffix: "ms", scale: 1, max: 1000 },
  { suffix: "s", scale: 1000, max: 60 },
  { suffix: "m", scale: 60_000, max: 60 },
  { suffix: "h", scale: 3_600_000, max: Infinity },
];

/** VALUE at three significant digits, trailing zeros and dot trimmed. */
function threeSigFigs(value: number): string {
  const decimals = value >= 100 ? 0 : value >= 10 ? 1 : 2;
  const text = value.toFixed(decimals);
  return text.includes(".") ? text.replace(/0+$/, "").replace(/\.$/, "") : text;
}

/**
 * A duration in its most compact reading: the coarsest unit whose
 * magnitude still reaches 1, carried to three significant digits
 * (1033ms → `1.03s`, 12300ms → `12.3s`, 120000ms → `2m`).
 *
 * Sub-second durations stay whole milliseconds, there being no finer
 * unit to promote a fraction into.
 */
export function formatDuration(ms: number): string {
  for (const unit of DURATION_UNITS) {
    const value = ms / unit.scale;
    const text = unit.suffix === "ms" ? String(Math.round(value)) : threeSigFigs(value);
    if (Number(text) < unit.max) return `${text}${unit.suffix}`;
  }
  throw new Error(`unreachable: no unit carried ${ms}ms`);
}

function ResultChip(item: ResultItem): string {
  const done = isTurnComplete(item);
  const label = done ? "turn complete" : item.subtype;
  return `
    <div class="result ${item.isError ? "err" : "ok"}${done ? " done" : ""}">
      ${escapeHtml(label)} · ${formatDuration(item.durationMs)} ·
      ${item.usage.input_tokens}in/${item.usage.output_tokens}out ·
      $${item.totalCostUsd.toFixed(4)}
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

export function renderItem(
  item: ConversationItem,
  selections?: QuestionSelections,
  isFinal = false,
): string {
  switch (item.kind) {
    case "user-turn":
      return UserTurn(item);
    case "text":
      return TextStream(item, isFinal);
    case "thinking":
      return Thinking(item);
    case "tool":
      // AskUserQuestion's UI IS the permission picker card — the
      // generic tool card would just dump the questions JSON (input)
      // and the "User has answered…" echo (result) alongside it.
      // ToolSearch is deferred-tool schema plumbing and TaskUpdate is
      // task-list bookkeeping: both pure feed noise.
      return SUPPRESSED_TOOLS.has(item.toolName) ? "" : ToolCard(item);
    case "permission":
      return PermissionPrompt(item, selections);
    case "result":
      return ResultChip(item);
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

  /** One item's HTML, green-bordered when FINALS marks it a turn's answer. */
  private itemHtml(item: ConversationItem, finals: ReadonlySet<string>): string {
    return renderItem(
      item,
      this.questionSelections,
      item.kind === "text" && finals.has(item.blockId),
    );
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
    const finals = finalResponseBlockIds(state.items);
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
    const fillChunk = (indexes: number[]): void => {
      const before = this.container.scrollHeight;
      for (const i of indexes) {
        const { el, item } = shells[i];
        const html = this.itemHtml(item, finals);
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
    const finals = finalResponseBlockIds(state.items);
    const seen = new Set<string>();
    state.items.forEach((item, i) => {
      const key = itemKey(item, i);
      seen.add(key);
      const html = this.itemHtml(item, finals);
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
        const open = expandedIndexes(sectionsIn(entry.el));
        entry.el.innerHTML = html;
        applyExpanded(sectionsIn(entry.el), open);
        entry.html = html;
      }
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
