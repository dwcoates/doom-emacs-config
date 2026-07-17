/**
 * Subfeed projection — the shared shape behind every nesting fold.
 *
 * A fold's body is always a CHILD FEED: conversation items rendered
 * through the very renderItem the top level uses. Depth one gets that
 * feed from the live store (partitionFeed over parent-tagged frames);
 * deeper levels get it from a detached stream's parsed transcript. This
 * module is the seam that makes the two the SAME thing: a parsed
 * transcript is partitioned exactly like the live item list, so a task's
 * update history nests inside a watcher's transcript just as it does at
 * the top level, and the renderer recurses on one representation
 * instead of growing a bespoke one per depth.
 */
import { ParsedTranscript, STREAM_ITEM_CAP, parseTranscript } from "./async-stream.js";
import { partitionFeed, spawnedTaskIds } from "./partition.js";
import { AsyncSource } from "./protocol.js";
import { ConversationItem, ToolItem } from "./store.js";

/** A parsed transcript, partitioned like the live feed. */
export interface TranscriptFeed {
  /** Top-level items of the transcript, in stream order. */
  top: ConversationItem[];
  /** Child items per owning tool_use_id, as partitionFeed confines them. */
  children: ReadonlyMap<string, ConversationItem[]>;
  /** Entries dropped by the parse cap; 0 when the whole stream fit. */
  dropped: number;
}

/**
 * Parse and partition a detached stream's transcript in one step: the
 * same confinement the live feed gets (a TaskUpdate folds into its
 * TaskCreate's panel, a task poll into its spawner's) applied to the
 * parsed items — which keep their real, globally-unique tool-use ids, so
 * the partition's claims resolve unchanged.
 */
export function transcriptFeed(text: string, cap = STREAM_ITEM_CAP): TranscriptFeed {
  const { items, dropped }: ParsedTranscript = parseTranscript(text, cap);
  const { top, children } = partitionFeed(items);
  return { top, children, dropped };
}

/**
 * BASE's child map extended with EXTRA's: what a fold rendering a parsed
 * transcript hands the items it nests, so a nested card resolves its own
 * children from the very stream that carries them while everything the
 * live feed confined stays resolvable. Tool-use ids are globally unique,
 * so a collision names the same call — EXTRA wins, being the fresher
 * parse.
 */
export function mergeChildren(
  base: ReadonlyMap<string, readonly ConversationItem[]>,
  extra: ReadonlyMap<string, readonly ConversationItem[]>,
): ReadonlyMap<string, readonly ConversationItem[]> {
  if (extra.size === 0) return base;
  const merged = new Map<string, readonly ConversationItem[]>(base);
  for (const [id, list] of extra) merged.set(id, list);
  return merged;
}

/** The stream shape a spawn's tool implies, mirroring the daemon's classifier. */
function syntheticShape(toolName: string): {
  kind: AsyncSource["kind"];
  format: "jsonl-transcript" | "jsonl-journal" | "text";
} {
  switch (toolName) {
    case "Agent":
      return { kind: "agent", format: "jsonl-transcript" };
    case "Workflow":
      return { kind: "workflow", format: "jsonl-journal" };
    default:
      return { kind: "shell", format: "text" };
  }
}

/**
 * The async source ITEM effectively owns: the daemon-classified one when
 * present, else one SYNTHESIZED from the spawn announcement in the
 * item's own result. Live store cards always carry the classification
 * (the async-source frame lands on them), so synthesis is the
 * depth-two-and-deeper case — a transcript-parsed spawn card the daemon's
 * frames never reach. Undefined when the item announced nothing
 * detached, which is what "renders no fold at all" keys off.
 */
export function effectiveAsyncSource(item: ToolItem): AsyncSource | undefined {
  if (item.asyncSource) return item.asyncSource;
  const ids = spawnedTaskIds(item);
  if (ids.length === 0) return undefined;
  const { kind, format } = syntheticShape(item.toolName);
  const description = item.input?.description;
  return {
    source_id: item.notification?.taskId ?? ids[0],
    kind,
    label: typeof description === "string" ? description : undefined,
    status: item.notification ? "done" : "running",
    stream: { transport: "poll", format },
  };
}

/**
 * Most fold levels a feed may nest below the top; past this the fold
 * simply stops appearing. Recursion this deep is a runaway rather than
 * information — every level is one more transcript parsed per render.
 */
export const SUBFEED_DEPTH_CAP = 5;

/**
 * Whether a fold for SOURCEID may nest at DEPTH given the source ids
 * already rendering above it. A transcript that announces an ancestor's
 * own id must not recurse into itself, and depth past the cap is cut
 * rather than drawn.
 */
export function mayNest(
  depth: number,
  seen: ReadonlySet<string> | undefined,
  sourceId: string,
): boolean {
  return depth < SUBFEED_DEPTH_CAP && !(seen?.has(sourceId) ?? false);
}

/**
 * Every source id the poller should poll: the ids behind OPEN folds,
 * collected RECURSIVELY so polling mirrors exactly what the renderer
 * draws at every depth.
 *
 * Two roots, matching the two fold surfaces. A spawning card's async
 * fold (`async:<toolUseId>`) contributes its poll source; a watcher fold
 * (`watchers:<blockId>`) contributes every id its watchers announced,
 * since a watcher row renders its tail inline the moment the fold opens.
 * From either root the walk descends into a transcript source's
 * accumulated tail — TAILTEXT, the poller's own state — because that
 * parse is where the nested spawn cards live: opening depth N's fold
 * polls its tail, whose parse reveals depth N+1's cards, whose folds
 * contribute in turn once opened. The descent honors the renderer's own
 * guards (`mayNest`, closed activity panels), so nothing polls that the
 * feed would not draw.
 */
export function openSubfeedSourceIds(opts: {
  items: readonly ConversationItem[];
  watchers: ReadonlyMap<string, readonly ToolItem[]>;
  isOpen: (foldId: string) => boolean;
  tailText: (sourceId: string) => string | undefined;
}): Set<string> {
  const ids = new Set<string>();

  const walkCard = (item: ToolItem, depth: number, seen: ReadonlySet<string>): void => {
    const source = effectiveAsyncSource(item);
    // A ws source's tail already arrives as task-output-delta frames;
    // polling it would be a round trip for bytes in hand.
    if (!source || source.stream?.transport !== "poll") return;
    if (!mayNest(depth, seen, source.source_id)) return;
    if (!opts.isOpen(`async:${item.toolUseId}`)) return;
    ids.add(source.source_id);
    descendStream(item, source, depth, seen);
  };

  const descendStream = (
    item: ToolItem,
    source: AsyncSource,
    depth: number,
    seen: ReadonlySet<string>,
  ): void => {
    if (source.stream?.format !== "jsonl-transcript") return;
    const text =
      item.taskOutput !== undefined && item.taskOutput !== ""
        ? item.taskOutput
        : opts.tailText(source.source_id);
    if (text === undefined || text === "") return;
    const feed = transcriptFeed(text);
    const nextSeen = new Set([...seen, source.source_id]);
    for (const t of feed.top) {
      if (t.kind === "tool") walkCard(t, depth + 1, nextSeen);
    }
    for (const [parent, children] of feed.children) {
      // A child renders inside its parent's activity panel, so a closed
      // panel keeps its cards — and their folds — out of the walk.
      if (!opts.isOpen(parent)) continue;
      for (const t of children) {
        if (t.kind === "tool") walkCard(t, depth + 1, nextSeen);
      }
    }
  };

  for (const item of opts.items) {
    if (item.kind === "tool") walkCard(item, 0, new Set());
  }
  for (const [blockId, list] of opts.watchers) {
    if (!opts.isOpen(`watchers:${blockId}`)) continue;
    for (const w of list) {
      for (const id of spawnedTaskIds(w)) ids.add(id);
      const source = effectiveAsyncSource(w);
      if (source) descendStream(w, source, 0, new Set([source.source_id]));
    }
  }
  return ids;
}
