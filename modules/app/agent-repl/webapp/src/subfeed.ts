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
import { partitionFeed } from "./partition.js";
import { ConversationItem } from "./store.js";

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
