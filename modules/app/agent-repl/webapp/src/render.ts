/**
 * DOM renderers for conversation items. Component naming mirrors the
 * spec (§2.4–2.7): TextStream, Thinking, ToolCard/<Name>,
 * PermissionPrompt. The feed renderer reuses one element per item key
 * so streaming updates do not rebuild the whole list.
 */
import { renderMarkdown } from "./markdown.js";
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
}

/** Tool names the SPA renders specially (§2.6); others use Generic. */
const SPECIAL_TOOLS = new Set(["Bash", "Read", "Edit", "Write", "Grep", "Task"]);

export function escapeHtml(s: string): string {
  return s
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll('"', "&quot;");
}

function contentToText(
  content: string | Array<{ type: string; text?: string }>,
): string {
  if (typeof content === "string") return content;
  return content
    .filter((b) => b.type === "text")
    .map((b) => b.text ?? "")
    .join("\n");
}

// --- per-item components ------------------------------------------------------

function UserTurn(item: UserTurnItem): string {
  const text = item.content
    .map((b) => (b.type === "text" ? String((b as { text: string }).text) : `[${b.type}]`))
    .join("\n");
  return `<div class="bubble user"><pre>${escapeHtml(text)}</pre></div>`;
}

function TextStream(item: TextItem): string {
  const cursor = item.done ? "" : `<span class="cursor">▍</span>`;
  return `<div class="bubble assistant md">${renderMarkdown(item.text)}${cursor}</div>`;
}

function Thinking(item: ThinkingItem): string {
  const state = item.done ? "" : " (thinking…)";
  return `
    <details class="thinking"${item.done ? "" : " open"}>
      <summary>Thinking${state}</summary>
      <pre>${escapeHtml(item.text)}</pre>
    </details>`;
}

function ToolCard(item: ToolItem): string {
  const variant = SPECIAL_TOOLS.has(item.toolName) ? item.toolName : "Generic";
  const status = item.result
    ? item.result.isError
      ? `<span class="badge err">error</span>`
      : `<span class="badge ok">done</span>`
    : item.inputDone
      ? `<span class="badge run">running…</span>`
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
  if (item.toolName === "Bash" && item.input && typeof item.input.command === "string") {
    return `<pre class="cmd">$ ${escapeHtml(item.input.command)}</pre>`;
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
  return `<pre class="tool-input">${escapeHtml(item.inputJson)}</pre>`;
}

function toolResult(item: ToolItem): string {
  if (!item.result) return "";
  const r = item.result.render;
  if (r) {
    switch (r.kind) {
      case "bash":
        return `<pre class="tool-output">${escapeHtml(r.stdout)}${
          r.stderr ? `\n<span class="stderr">${escapeHtml(r.stderr)}</span>` : ""
        }</pre>`;
      case "diff":
        return `<pre class="diff">${diffHtml(r.unified_diff)}</pre>`;
      case "grep":
        return `<pre class="tool-output">${r.matches
          .map((m) => `${escapeHtml(m.file)}:${m.line}: ${escapeHtml(m.text)}`)
          .join("\n")}</pre>`;
      case "task":
        return `<pre class="tool-output">${escapeHtml(r.summary)}</pre>`;
    }
  }
  return `<pre class="tool-output${item.result.isError ? " stderr" : ""}">${escapeHtml(
    contentToText(item.result.content),
  )}</pre>`;
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

function PermissionPrompt(item: PermissionItem): string {
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

function ResultChip(item: ResultItem): string {
  const label = item.subtype === "success" ? "turn complete" : item.subtype;
  return `
    <div class="result ${item.isError ? "err" : "ok"}">
      ${escapeHtml(label)} · ${item.durationMs}ms ·
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

export function renderItem(item: ConversationItem): string {
  switch (item.kind) {
    case "user-turn":
      return UserTurn(item);
    case "text":
      return TextStream(item);
    case "thinking":
      return Thinking(item);
    case "tool":
      return ToolCard(item);
    case "permission":
      return PermissionPrompt(item);
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
 * Feed renderer: reconciles the item list into `container`, reusing
 * nodes by key and only rewriting nodes whose HTML changed.
 */
export class FeedRenderer {
  private container: HTMLElement;
  private actions: Actions;
  private nodes = new Map<string, { el: HTMLElement; html: string }>();

  constructor(container: HTMLElement, actions: Actions) {
    this.container = container;
    this.actions = actions;
    container.addEventListener("click", (e) => {
      const target = e.target as HTMLElement;
      const allow = target.getAttribute("data-perm-allow");
      const deny = target.getAttribute("data-perm-deny");
      if (allow) this.actions.decidePermission(allow, "allow");
      if (deny) this.actions.decidePermission(deny, "deny");
    });
  }

  render(state: StoreState): void {
    const pinned =
      this.container.scrollHeight - this.container.scrollTop -
        this.container.clientHeight < 40;
    const seen = new Set<string>();
    state.items.forEach((item, i) => {
      const key = itemKey(item, i);
      seen.add(key);
      const html = renderItem(item);
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
        entry.el.innerHTML = html;
        entry.html = html;
      }
    });
    for (const [key, entry] of this.nodes) {
      if (!seen.has(key)) {
        entry.el.remove();
        this.nodes.delete(key);
      }
    }
    if (pinned) {
      this.container.scrollTop = this.container.scrollHeight;
    }
  }
}
