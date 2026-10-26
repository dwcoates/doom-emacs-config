/**
 * editor-ingress — the editor's own list of its prompts waiting in the
 * held-prompt ingress, as the host last handed it (host/protocol.ts
 * `heldIngress`).
 *
 * The page cannot read the ingress directory; Emacs can, and it pushes the
 * whole list for this workspace every time it changes, through the host, which
 * replays it to a page that says hello. The held section draws it only while
 * its root feed is out of reach of the daemon (held/section.ts).
 */
import type { UserSaid } from "../../../proto/gen/ts/conversation/v1/user_pb";
import { log } from "../log.js";
import { createListeners } from "../listeners.js";

/** One prompt the editor wrote to the ingress for this workspace. */
export interface EditorIngressEntry {
  /** The entry's idempotency key: its identity until a daemon accepts it. */
  readonly key: string;
  /** When the editor wrote it, epoch milliseconds. */
  readonly queuedAtMs: number;
  /** What the user said, whole. */
  readonly said: UserSaid;
}

/** The editor's list as the page last heard it. */
export interface EditorIngressView {
  /** The list now, in the order the editor wrote the entries. */
  entries(): readonly EditorIngressEntry[];
  /** Replace the whole list with ENTRIES, as the editor last stated it. */
  set(entries: readonly EditorIngressEntry[]): void;
  /** Be told every change of the list; answers the unsubscribe. */
  subscribe(listener: () => void): () => void;
}

/** An empty list, until the editor states one. */
export function createEditorIngressView(): EditorIngressView {
  let standing: readonly EditorIngressEntry[] = [];
  const listeners = createListeners();
  return {
    entries: () => standing,
    set(entries) {
      log.info("the editor's held-prompt ingress entries changed", {
        operation: "held.editor-ingress",
        context: { from: standing.length, to: entries.length, keys: entries.map((e) => e.key) },
      });
      standing = entries;
      listeners.notify();
    },
    subscribe(listener) {
      return listeners.add(listener);
    },
  };
}
