/**
 * workspace-mentions — A WORKSPACE'S NAME IN A RESPONSE'S OR A PROMPT'S TEXT IS
 * A LINK that switches to that workspace (owner requests, 2026-11-01 and
 * 2026-11-19).
 *
 * THE DAEMON DECIDES WHICH NAMES ARE LINKS. The page subscribes to its
 * mention targets (`WatchWorkspaceMentions`, frontend.v1.WorkspaceMentionTargets),
 * the very set the daemon serves this workspace's composer as the names it
 * completes, so a name the composer completes is always a link here. The
 * page never decides on its own that a word names a workspace; it only finds
 * the served names in what it drew.
 *
 * A MENTION IS A WHOLE OCCURRENCE of a served name anywhere in a mention
 * scope's text (`MENTION_TEXT_CLASS`: a response's prose, a prompt's text
 * block, an agent prompt's peer name): a code span whose whole text is the
 * name (agents write a workspace's name as inline code, and the span's code
 * markers are stripped when it is linked), or the name in plain text with no
 * name character (`NAME_CHAR`) on either side, so `alpha` never links inside
 * `alpha-beta`. Text already inside a link, a code span that is more than the
 * name, a code block, and a tree's rails and labels are never searched.
 *
 * LINKED ON EVERY PAINT AND ON EVERY PUSH. A scope is linked on each paint
 * (`linkWorkspaceMentionsInText`, a slot mark, so a repaint keeps the links),
 * and each push re-links every scope already drawn, so a workspace opened
 * after a response was written gains its links and a closed one loses them.
 * Re-linking first unwraps every mention, so it is idempotent.
 *
 * THE CLICK IS AN ORDINARY WORKSPACE SWITCH: `SelectInEditor`, through the
 * feed's one jump (feed/merge/jump.ts), delegated from the feed's scroll zone
 * so a mention drawn after the install is handled too. A mention is an anchor
 * with NO href, so neither the prose link router nor the navigation guard
 * (link.ts) ever takes its click.
 */
import { create } from "@bufbuild/protobuf";
import {
  WatchWorkspaceMentionsResponseSchema,
  type WatchWorkspaceMentionsResponse,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_workspace_mentions_pb";
import type { WorkspaceMentionTargets } from "../../../proto/gen/ts/frontend/v1/workspace_mention_pb";
import { WorkspaceRefSchema, type WorkspaceRef } from "../../../proto/gen/ts/workspace/v1/workspace_pb";
import { queryAll } from "../dom.js";
import { log } from "../log.js";
import { escapeRegExp } from "../regexp.js";
import { claimsClick } from "../link.js";
import type { AppContext } from "../rpc/context.js";
import { guardMalformed } from "../rpc/guard.js";
import { MalformedView } from "../rpc/malformed.js";
import { requireCase, requireMessage, unreachableArm } from "../rpc/strict.js";
import { watchStream, type StreamHandle } from "../rpc/streams.js";
import { TREE_LABEL_CLASS, TREE_RAILS_CLASS } from "../metaprompt-tree.js";
import { jumpToWorkspace, type JumpSite } from "./merge/jump.js";

/** The class every mention wears. */
export const WORKSPACE_MENTION_CLASS = "workspace-mention";

/**
 * The class of a MENTION SCOPE, a text slot searched whole: a response's
 * prose, a prompt bubble's text block, an agent prompt's peer name. A tree
 * drawn inside one keeps its rails and labels unsearched.
 */
export const MENTION_TEXT_CLASS = "mention-text";

/** The attribute naming the `WorkspaceRef.id` a mention switches to. */
export const WORKSPACE_MENTION_ATTRIBUTE = "data-workspace-mention";

/** Where a mention's jump is recorded and its refusal drawn. */
const MENTION_JUMP: JumpSite = {
  describe: "a workspace mention",
  operation: "feed.workspace-mention",
  refusalClass: "workspace-mention-refusal",
};

/** A character a workspace name is made of: a plain-text mention has none on either side. */
const NAME_CHAR = String.raw`[\p{L}\p{N}_-]`;

/** The served targets, by name and by workspace id. */
interface Targets {
  readonly byName: ReadonlyMap<string, WorkspaceRef>;
  readonly byId: ReadonlyMap<string, WorkspaceRef>;
  /** Every name in plain text, longest first so a longer name wins, or null for none. */
  readonly pattern: RegExp | null;
}

const NO_TARGETS: Targets = { byName: new Map(), byId: new Map(), pattern: null };

/** This page's targets: none until the first push. */
let current: Targets = NO_TARGETS;

/**
 * The served TARGETS, read to the page's lookup. A target with no name or no
 * workspace, or a name served twice, is malformed: the daemon serves distinct
 * names only.
 */
export function readMentionTargets(targets: WorkspaceMentionTargets): Targets {
  const byName = new Map<string, WorkspaceRef>();
  const byId = new Map<string, WorkspaceRef>();
  targets.targets.forEach((target, i) => {
    const path = `WorkspaceMentionTargets.targets[${i}]`;
    const name = requireMessage(target.name, `${path}.name`).text;
    const workspace = requireMessage(target.workspace, `${path}.workspace`);
    if (name === "") throw new MalformedView(`${path}.name.text`, "a mention target names nothing");
    if (workspace.id === "") throw new MalformedView(`${path}.workspace.id`, "a mention target names no workspace");
    if (byName.has(name)) throw new MalformedView(`${path}.name.text`, `the name ${JSON.stringify(name)} is served twice`);
    byName.set(name, workspace);
    byId.set(workspace.id, workspace);
  });
  const names = [...byName.keys()].sort((a, b) => b.length - a.length).map(escapeRegExp);
  const pattern =
    names.length === 0 ? null : new RegExp(`(?<!${NAME_CHAR})(?:${names.join("|")})(?!${NAME_CHAR})`, "gu");
  return { byName, byId, pattern };
}

/** A mention of WORKSPACE holding NODES, as every mention is drawn. */
function mentionAnchor(doc: Document, workspace: WorkspaceRef, name: string, nodes: Node[]): HTMLAnchorElement {
  const anchor = doc.createElement("a");
  anchor.className = WORKSPACE_MENTION_CLASS;
  anchor.setAttribute(WORKSPACE_MENTION_ATTRIBUTE, workspace.id);
  anchor.title = `switch to ${name}`;
  anchor.append(...nodes);
  return anchor;
}

/** Whether NODE sits where a mention is never searched for, below SCOPE. */
function unsearched(node: Node, scope: Element): boolean {
  for (let at = node.parentElement; at !== null && at !== scope; at = at.parentElement) {
    if (at.tagName === "A" || at.tagName === "CODE" || at.tagName === "PRE") return true;
    if (at.classList.contains(TREE_RAILS_CLASS) || at.classList.contains(TREE_LABEL_CLASS)) return true;
  }
  return false;
}

/** Take every mention in SCOPE back to the text it wrapped. */
function unlinkScope(scope: Element): void {
  const mentions = queryAll<HTMLElement>(scope, `.${WORKSPACE_MENTION_CLASS}`);
  for (const mention of mentions) mention.replaceWith(...mention.childNodes);
  // The text a plain-text mention split is joined back into one node.
  if (mentions.length > 0) scope.normalize();
}

/** Link every mention of TARGETS in SCOPE; answers how many were linked. */
function linkScope(scope: Element, targets: Targets): number {
  const doc = scope.ownerDocument;
  let linked = 0;
  for (const code of queryAll<HTMLElement>(scope, "code")) {
    if (unsearched(code, scope)) continue;
    const name = code.textContent ?? "";
    const workspace = targets.byName.get(name);
    if (workspace === undefined) continue;
    // The code markers are stripped: the mention is plain purple text, as in a prompt.
    code.replaceWith(mentionAnchor(doc, workspace, name, [doc.createTextNode(name)]));
    linked++;
  }
  const pattern = targets.pattern;
  if (pattern === null) return linked;
  const texts: Text[] = [];
  const walker = doc.createTreeWalker(scope, NodeFilter.SHOW_TEXT);
  for (let node = walker.nextNode(); node !== null; node = walker.nextNode()) {
    if (!unsearched(node, scope)) texts.push(node as Text);
  }
  for (const text of texts) {
    const value = text.data;
    const parts: Node[] = [];
    let from = 0;
    for (const match of value.matchAll(pattern)) {
      const name = match[0];
      const workspace = targets.byName.get(name);
      if (workspace === undefined) throw new Error(`linkScope: the pattern matched ${JSON.stringify(name)}, which is no target`);
      if (match.index > from) parts.push(doc.createTextNode(value.slice(from, match.index)));
      parts.push(mentionAnchor(doc, workspace, name, [doc.createTextNode(name)]));
      from = match.index + name.length;
      linked++;
    }
    if (parts.length === 0) continue;
    if (from < value.length) parts.push(doc.createTextNode(value.slice(from)));
    text.replaceWith(...parts);
  }
  return linked;
}

/**
 * Link every mention in the whole of ROOT, a mention scope's rendered text:
 * the mark a response's prose and a prompt's text block run on every paint.
 * Mentions already drawn are re-linked from scratch.
 */
export function linkWorkspaceMentionsInText(root: Element): void {
  unlinkScope(root);
  linkScope(root, current);
}

/**
 * Re-link every mention scope (`MENTION_TEXT_CLASS`) in ROOT, ROOT itself
 * included when it is one, with TARGETS; answers how many mentions were
 * linked.
 */
function relink(root: ParentNode, targets: Targets): number {
  let linked = 0;
  const scopes = queryAll(root, `.${MENTION_TEXT_CLASS}`);
  if (root instanceof Element && root.classList.contains(MENTION_TEXT_CLASS)) scopes.unshift(root);
  for (const scope of scopes) {
    unlinkScope(scope);
    linked += linkScope(scope, targets);
  }
  return linked;
}

/**
 * Take a push's TARGETS as the page's, and re-link every scope LIVE answers:
 * the mention scopes already drawn.
 */
export function applyMentionTargets(targets: WorkspaceMentionTargets, live: () => Iterable<ParentNode>): void {
  current = readMentionTargets(targets);
  let linked = 0;
  for (const root of live()) linked += relink(root, current);
  log.debug("the page's workspace mention targets changed; its drawn mention scopes were re-linked", {
    operation: "feed.workspace-mentions.applied",
    context: { targets: current.byName.size, linked },
  });
}

/** Forget the page's targets (a test's teardown). */
export function resetMentionTargets(): void {
  current = NO_TARGETS;
}

/**
 * Switch to the workspace a click on a mention names. Delegated from ROOT, in
 * the capture phase so a bubble's own click affordances never see it.
 */
export function installWorkspaceMentionClicks(ctx: AppContext, root: HTMLElement): () => void {
  const onClick = (event: MouseEvent): void => {
    if (!(event.target instanceof Element)) return;
    const mention = event.target.closest<HTMLElement>(`.${WORKSPACE_MENTION_CLASS}`);
    if (mention === null || !root.contains(mention)) return;
    if (!claimsClick(event)) return;
    event.preventDefault();
    event.stopPropagation();
    const id = mention.getAttribute(WORKSPACE_MENTION_ATTRIBUTE) ?? "";
    // A mention is only ever drawn from the current targets, and every push
    // re-links every drawn scope, so a drawn mention always names one.
    const workspace = current.byId.get(id);
    if (workspace === undefined) {
      log.error("a drawn workspace mention names no current target", {
        operation: "feed.workspace-mention.stale",
        context: { workspace: id, targets: current.byId.size },
      });
      throw new Error(`a drawn workspace mention names ${JSON.stringify(id)}, which is no current target`);
    }
    void guardMalformed(
      ctx,
      MENTION_JUMP.operation,
      jumpToWorkspace(mention, ctx, create(WorkspaceRefSchema, { id: workspace.id, dir: workspace.dir }), MENTION_JUMP),
    );
  };
  root.addEventListener("click", onClick, true);
  return () => root.removeEventListener("click", onClick, true);
}

/**
 * Watch this page's mention targets, re-linking LIVE's scopes on every push.
 * The page's own workspace is the viewer the daemon leaves out.
 */
export function watchWorkspaceMentions(ctx: AppContext, live: () => Iterable<ParentNode>): StreamHandle {
  return watchStream<WatchWorkspaceMentionsResponse>(ctx, {
    name: "WatchWorkspaceMentions",
    schema: WatchWorkspaceMentionsResponseSchema,
    open: (_client, signal) => ctx.streams.watch("workspaceMentions", { viewer: ctx.workspace }, signal),
    plannedEnding: (res) => res.push.case === "ending",
    onPush: (res) => {
      const push = requireCase(res.push, "WatchWorkspaceMentionsResponse.push");
      switch (push.case) {
        case "targets":
          applyMentionTargets(push.value, live);
          return;
        case "ending":
          return;
        default: {
          const other: { case: string } = push;
          return unreachableArm("WatchWorkspaceMentionsResponse.push", other.case);
        }
      }
    },
  });
}
