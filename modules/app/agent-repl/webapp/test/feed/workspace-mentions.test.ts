// @vitest-environment jsdom
import { afterEach, describe, expect, it, vi } from "vitest";
import { create } from "@bufbuild/protobuf";
import {
  SelectWorkspaceResponseSchema,
  type SelectWorkspaceRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_select_workspace_pb";
import {
  WatchWorkspaceMentionsResponseSchema,
  type WatchWorkspaceMentionsRequest,
} from "../../../proto/gen/ts/agentrepl/v1/endpoint_watch_workspace_mentions_pb";
import {
  WorkspaceMentionTargetsSchema,
  type WorkspaceMentionTargets,
} from "../../../proto/gen/ts/frontend/v1/workspace_mention_pb";
import {
  MENTION_TEXT_CLASS,
  WORKSPACE_MENTION_ATTRIBUTE,
  WORKSPACE_MENTION_CLASS,
  applyMentionTargets,
  installWorkspaceMentionClicks,
  linkWorkspaceMentions,
  readMentionTargets,
  resetMentionTargets,
  watchWorkspaceMentions,
} from "../../src/feed/workspace-mentions.js";
import { MalformedView } from "../../src/rpc/malformed.js";
import { scriptedAppContext } from "../rpc/app-context.js";
import { captureLogRecords, forwardedRecord } from "../log-capture.js";
import { RecordingSink, WORKSPACE } from "./harness.js";

afterEach(() => {
  resetMentionTargets();
  document.body.replaceChildren();
});

/** Targets naming each of NAMES, its workspace id `ws-<name>`. */
function targets(...names: string[]): WorkspaceMentionTargets {
  return create(WorkspaceMentionTargetsSchema, {
    targets: names.map((name) => ({ name: { text: name }, workspace: { id: `ws-${name}`, dir: `/w/${name}` } })),
  });
}

/** A drawn tree whose one line's text is BODY (HTML), under a rail and a label. */
function tree(body: string): HTMLElement {
  const root = document.createElement("div");
  root.innerHTML = `<div class="mp-tree"><div class="mp-line"><span class="mp-connectors">├── </span><span class="mp-label">1</span> ${body}</div></div>`;
  document.body.append(root);
  return root;
}

/** Every mention drawn in ROOT, as "text=id". */
function mentions(root: ParentNode): string[] {
  return [...root.querySelectorAll(`.${WORKSPACE_MENTION_CLASS}`)].map(
    (el) => `${el.textContent ?? ""}=${el.getAttribute(WORKSPACE_MENTION_ATTRIBUTE) ?? ""}`,
  );
}

describe("linkWorkspaceMentions", () => {
  const cases: Array<{ name: string; served: string[]; body: string; want: string[] }> = [
    { name: "links a code span that is wholly a name", served: ["alpha"], body: "see <code>alpha</code>", want: ["alpha=ws-alpha"] },
    { name: "links a name in plain text", served: ["alpha"], body: "see alpha now", want: ["alpha=ws-alpha"] },
    { name: "links a name ending a sentence", served: ["alpha"], body: "see alpha.", want: ["alpha=ws-alpha"] },
    { name: "never links a name inside a longer name", served: ["alpha"], body: "see alpha-beta and xalpha", want: [] },
    { name: "the longer of two overlapping names wins", served: ["alpha", "alpha-beta"], body: "see alpha-beta", want: ["alpha-beta=ws-alpha-beta"] },
    { name: "never links a code span that is more than a name", served: ["alpha"], body: "<code>alpha --flag</code>", want: [] },
    { name: "never links inside a link", served: ["alpha"], body: '<a href="https://x.test">alpha</a>', want: [] },
    { name: "links every mention on the line", served: ["alpha", "beta"], body: "alpha then <code>beta</code> then alpha", want: ["alpha=ws-alpha", "beta=ws-beta", "alpha=ws-alpha"] },
    { name: "links nothing a target does not name", served: ["alpha"], body: "<code>gamma</code> gamma", want: [] },
    { name: "links nothing with no targets", served: [], body: "<code>alpha</code> alpha", want: [] },
  ];
  for (const tc of cases) {
    it(tc.name, () => {
      // ARRANGE
      applyMentionTargets(targets(...tc.served), () => []);
      const root = tree(tc.body);
      // ACT
      linkWorkspaceMentions(root);
      // ASSERT
      expect(mentions(root)).toEqual(tc.want);
    });
  }

  it("never searches a tree's rails or labels", () => {
    // ARRANGE
    applyMentionTargets(targets("1"), () => []);
    const root = tree("text");
    // ACT
    linkWorkspaceMentions(root);
    // ASSERT
    expect(mentions(root)).toEqual([]);
  });

  it("never searches prose outside a tree", () => {
    // ARRANGE
    applyMentionTargets(targets("alpha"), () => []);
    const root = document.createElement("div");
    root.innerHTML = "<p>see <code>alpha</code></p>";
    // ACT
    linkWorkspaceMentions(root);
    // ASSERT
    expect(mentions(root)).toEqual([]);
  });

  it("leaves the line's text unchanged when it links", () => {
    // ARRANGE
    applyMentionTargets(targets("alpha"), () => []);
    const root = tree("see alpha and <code>alpha</code>.");
    const before = root.textContent;
    // ACT
    linkWorkspaceMentions(root);
    // ASSERT
    expect(root.textContent).toBe(before);
  });

  it("is idempotent: linking twice draws each mention once", () => {
    // ARRANGE
    applyMentionTargets(targets("alpha"), () => []);
    const root = tree("see alpha and <code>alpha</code>");
    // ACT
    linkWorkspaceMentions(root);
    linkWorkspaceMentions(root);
    // ASSERT
    expect(mentions(root)).toEqual(["alpha=ws-alpha", "alpha=ws-alpha"]);
    expect(root.querySelectorAll(`.${WORKSPACE_MENTION_CLASS} .${WORKSPACE_MENTION_CLASS}`)).toHaveLength(0);
  });
});

/** A prompt's text slot whose one paragraph is BODY (HTML). */
function promptText(body: string): HTMLElement {
  const slot = document.createElement("div");
  slot.className = `prompt-block-text ${MENTION_TEXT_CLASS}`;
  slot.innerHTML = `<p>${body}</p>`;
  document.body.append(slot);
  return slot;
}

describe("a whole-text slot (a prompt's text)", () => {
  it("links a name in plain text with no tree around it", () => {
    // ARRANGE
    applyMentionTargets(targets("alpha"), () => []);
    const slot = promptText("see alpha now");
    // ACT
    linkWorkspaceMentions(slot);
    // ASSERT
    expect(mentions(slot)).toEqual(["alpha=ws-alpha"]);
  });

  it("links a code span that is wholly a name", () => {
    // ARRANGE
    applyMentionTargets(targets("alpha"), () => []);
    const slot = promptText("see <code>alpha</code>");
    // ACT
    linkWorkspaceMentions(slot);
    // ASSERT
    expect(mentions(slot)).toEqual(["alpha=ws-alpha"]);
  });

  it("never links a name inside a longer name", () => {
    // ARRANGE
    applyMentionTargets(targets("alpha"), () => []);
    const slot = promptText("see alpha-beta");
    // ACT
    linkWorkspaceMentions(slot);
    // ASSERT
    expect(mentions(slot)).toEqual([]);
  });

  it("is re-linked by a push when the slot is itself the live root", () => {
    // ARRANGE
    applyMentionTargets(targets("alpha"), () => []);
    const slot = promptText("alpha and beta");
    linkWorkspaceMentions(slot);
    // ACT
    applyMentionTargets(targets("beta"), () => [slot]);
    // ASSERT
    expect(mentions(slot)).toEqual(["beta=ws-beta"]);
    expect(slot.textContent).toBe("alpha and beta");
  });
});

describe("applyMentionTargets", () => {
  it("re-links the live trees: a target gained links, a target lost unlinks", () => {
    // ARRANGE
    applyMentionTargets(targets("alpha"), () => []);
    const root = tree("alpha and <code>beta</code>");
    linkWorkspaceMentions(root);
    // ACT
    applyMentionTargets(targets("beta"), () => [root]);
    // ASSERT
    expect(mentions(root)).toEqual(["beta=ws-beta"]);
    expect(root.textContent).toContain("alpha and beta");
  });

  it("records the change with how many mentions it linked", async () => {
    // ARRANGE
    const capture = captureLogRecords("debug");
    const root = tree("alpha and alpha");
    // ACT
    applyMentionTargets(targets("alpha"), () => [root]);
    // ASSERT
    const record = await forwardedRecord(capture, "feed.workspace-mentions.applied");
    expect(record.context).toMatchObject({ targets: 1, linked: 2 });
  });
});

describe("readMentionTargets", () => {
  const malformed: Array<{ name: string; served: WorkspaceMentionTargets }> = [
    { name: "a target with no name", served: create(WorkspaceMentionTargetsSchema, { targets: [{ workspace: { id: "a" } }] }) },
    { name: "a target with an empty name", served: create(WorkspaceMentionTargetsSchema, { targets: [{ name: { text: "" }, workspace: { id: "a" } }] }) },
    { name: "a target with no workspace", served: create(WorkspaceMentionTargetsSchema, { targets: [{ name: { text: "a" } }] }) },
    { name: "a target whose workspace has no id", served: create(WorkspaceMentionTargetsSchema, { targets: [{ name: { text: "a" }, workspace: { id: "" } }] }) },
    { name: "a name served twice", served: targets("a", "a") },
  ];
  for (const tc of malformed) {
    it(`refuses ${tc.name} as malformed`, () => {
      // ACT + ASSERT
      expect(() => readMentionTargets(tc.served)).toThrow(MalformedView);
    });
  }

  it("leaves the page's targets as they were when a push is malformed", () => {
    // ARRANGE
    applyMentionTargets(targets("alpha"), () => []);
    const root = tree("alpha");
    // ACT
    expect(() => applyMentionTargets(targets("a", "a"), () => [root])).toThrow(MalformedView);
    linkWorkspaceMentions(root);
    // ASSERT
    expect(mentions(root)).toEqual(["alpha=ws-alpha"]);
  });
});

describe("installWorkspaceMentionClicks", () => {
  /** A page whose SelectWorkspace records its requests, with ROOT's clicks installed. */
  function page(root: HTMLElement) {
    const calls: SelectWorkspaceRequest[] = [];
    const ctx = scriptedAppContext(
      {
        selectWorkspace: (req) => {
          calls.push(req);
          return create(SelectWorkspaceResponseSchema, { result: { case: "success", value: {} } });
        },
      },
      { workspace: WORKSPACE, failures: new RecordingSink() },
    );
    const uninstall = installWorkspaceMentionClicks(ctx, root);
    return { calls, uninstall };
  }

  it("selects the mentioned workspace, echoed verbatim", async () => {
    // ARRANGE
    applyMentionTargets(targets("alpha"), () => []);
    const root = tree("<code>alpha</code>");
    linkWorkspaceMentions(root);
    const { calls, uninstall } = page(root);
    // ACT
    root.querySelector<HTMLElement>(`.${WORKSPACE_MENTION_CLASS} code`)?.click();
    // ASSERT
    await vi.waitFor(() => expect(calls).toHaveLength(1));
    expect(calls[0].workspace?.id).toBe("ws-alpha");
    expect(calls[0].workspace?.dir).toBe("/w/alpha");
    uninstall();
  });

  it("claims the click so nothing beneath the feed sees it", () => {
    // ARRANGE
    applyMentionTargets(targets("alpha"), () => []);
    const root = tree("alpha");
    linkWorkspaceMentions(root);
    const { uninstall } = page(root);
    const beneath = vi.fn();
    root.querySelector(".mp-line")?.addEventListener("click", beneath);
    // ACT
    const event = new MouseEvent("click", { bubbles: true, cancelable: true, button: 0 });
    root.querySelector(`.${WORKSPACE_MENTION_CLASS}`)?.dispatchEvent(event);
    // ASSERT
    expect(event.defaultPrevented).toBe(true);
    expect(beneath).not.toHaveBeenCalled();
    uninstall();
  });

  it("leaves a modified click alone", () => {
    // ARRANGE
    applyMentionTargets(targets("alpha"), () => []);
    const root = tree("alpha");
    linkWorkspaceMentions(root);
    const { calls, uninstall } = page(root);
    // ACT
    const event = new MouseEvent("click", { bubbles: true, cancelable: true, button: 0, metaKey: true });
    root.querySelector(`.${WORKSPACE_MENTION_CLASS}`)?.dispatchEvent(event);
    // ASSERT
    expect(event.defaultPrevented).toBe(false);
    expect(calls).toHaveLength(0);
    uninstall();
  });

  it("fails loudly, recorded at ERROR, on a mention naming no current target", async () => {
    // ARRANGE
    const capture = captureLogRecords();
    const root = tree(`<a class="${WORKSPACE_MENTION_CLASS}" ${WORKSPACE_MENTION_ATTRIBUTE}="ws-gone">gone</a>`);
    const calls: SelectWorkspaceRequest[] = [];
    const ctx = scriptedAppContext(
      { selectWorkspace: (req) => (calls.push(req), create(SelectWorkspaceResponseSchema, {})) },
      { workspace: WORKSPACE, failures: new RecordingSink() },
    );
    const uninstall = installWorkspaceMentionClicks(ctx, root);
    const errors: unknown[] = [];
    const onError = (event: ErrorEvent): void => {
      errors.push(event.error);
      event.preventDefault();
    };
    window.addEventListener("error", onError);
    // ACT
    root.querySelector(`.${WORKSPACE_MENTION_CLASS}`)?.dispatchEvent(new MouseEvent("click", { bubbles: true, cancelable: true }));
    // ASSERT
    window.removeEventListener("error", onError);
    expect(errors).toHaveLength(1);
    expect(calls).toHaveLength(0);
    const record = await forwardedRecord(capture, "feed.workspace-mention.stale");
    expect(record.level.case).toBe("error");
    expect(record.context).toMatchObject({ workspace: "ws-gone", targets: 0 });
    uninstall();
  });
});

describe("watchWorkspaceMentions", () => {
  it("subscribes as the page's own workspace and re-links the live trees on a push", async () => {
    // ARRANGE
    const requests: WatchWorkspaceMentionsRequest[] = [];
    const ctx = scriptedAppContext(
      {
        watchWorkspaceMentions: async function* (req) {
          requests.push(req);
          yield create(WatchWorkspaceMentionsResponseSchema, { push: { case: "targets", value: targets("alpha") } });
          await new Promise<never>(() => undefined);
        },
      },
      { workspace: WORKSPACE, failures: new RecordingSink() },
    );
    const root = tree("<code>alpha</code>");
    // ACT
    const stream = watchWorkspaceMentions(ctx, () => [root]);
    // ASSERT
    await vi.waitFor(() => expect(mentions(root)).toEqual(["alpha=ws-alpha"]));
    expect(requests[0].viewer?.id).toBe(WORKSPACE.id);
    stream.cancel();
  });
});
