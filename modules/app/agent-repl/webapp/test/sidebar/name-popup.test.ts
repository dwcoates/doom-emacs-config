// @vitest-environment jsdom
import { readdirSync, readFileSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { afterEach, describe, expect, it, vi } from "vitest";
import { NAME_POPUP_CLASS, openNamePopup, type NamePopupSpec } from "../../src/sidebar/name-popup.js";
import { log } from "../../src/log.js";
import { sidebarContext } from "./harness.js";

/** Every context a test opened a popup in, whose popups end with the test. */
const contexts: Array<ReturnType<typeof sidebarContext>> = [];

afterEach(() => {
  for (const sc of contexts.splice(0)) sc.dropdowns.dismissAll("the test ended");
  vi.restoreAllMocks();
  document.body.replaceChildren();
});

/** Open a popup naming SUBJECT under a fresh anchor, recording every send. */
function opened(subject: string, answer = true) {
  const sc = sidebarContext();
  contexts.push(sc);
  const anchor = document.createElement("span");
  document.body.appendChild(anchor);
  const sent: Array<{ name: string; control: HTMLElement }> = [];
  const spec: NamePopupSpec = {
    subject,
    placeholder: "a name",
    value: "old",
    submitLabel: "Rename",
    send: (name, control) => {
      sent.push({ name, control });
      return Promise.resolve(answer);
    },
  };
  const popup = openNamePopup(sc, anchor, spec);
  const input = popup.querySelector(`[data-${subject}-name]`) as HTMLInputElement;
  const submit = popup.querySelector(`[data-${subject}-submit]`) as HTMLElement;
  return { sc, popup, input, submit, sent };
}

/** Let a send's promise settle. */
const settled = () => new Promise((resolve) => globalThis.setTimeout(resolve, 0));

describe("openNamePopup", () => {
  it("names its data attributes for its subject and opens on the rail's layer with the value", () => {
    const { sc, popup, input, submit } = opened("workspace");
    expect([
      popup.className,
      popup.hasAttribute("data-workspace-popup"),
      input.value,
      submit.textContent,
      sc.arrange.layer.contains(popup),
    ]).toEqual([NAME_POPUP_CLASS, true, "old", "Rename", true]);
  });

  it("sends the trimmed name with its button as the refusal's control, and closes once taken", async () => {
    const { sc, input, submit, sent } = opened("workspace");
    input.value = "  better-name ";
    input.dispatchEvent(new KeyboardEvent("keydown", { key: "Enter" }));
    await settled();
    expect([sent.map((s) => s.name), sent[0]?.control === submit, sc.arrange.layer.querySelector("[data-workspace-popup]")]).toEqual([
      ["better-name"],
      true,
      null,
    ]);
  });

  it("keeps waiting rather than send a blank name", async () => {
    const { sc, input, submit, sent } = opened("workspace");
    input.value = "   ";
    submit.click();
    await settled();
    expect([sent.length, sc.arrange.layer.querySelector("[data-workspace-popup]") !== null]).toEqual([0, true]);
  });

  it("stays open when the send is refused", async () => {
    const { sc, submit, sent } = opened("workspace", false);
    submit.click();
    await settled();
    expect([sent.length, sc.arrange.layer.querySelector("[data-workspace-popup]") !== null]).toEqual([1, true]);
  });

  it("closes on Escape and on a press outside it, recording why under its subject", () => {
    const debug = vi.spyOn(log, "debug");
    const first = opened("workspace");
    first.input.dispatchEvent(new KeyboardEvent("keydown", { key: "Escape" }));
    const second = opened("workspace");
    document.body.dispatchEvent(new PointerEvent("pointerdown", { bubbles: true }));
    expect([first.popup.isConnected, second.popup.isConnected]).toEqual([false, false]);
    expect(debug.mock.calls.filter(([, fields]) => fields?.operation === "sidebar.workspace.popup-close").map(([, fields]) => fields?.context)).toEqual([
      { why: "Escape" },
      { why: "a press outside it" },
    ]);
  });

  it("records the name it sends under its subject", async () => {
    const info = vi.spyOn(log, "info");
    const { input } = opened("workspace");
    input.value = "abc";
    input.dispatchEvent(new KeyboardEvent("keydown", { key: "Enter" }));
    await settled();
    expect(info).toHaveBeenCalledWith("naming a sidebar workspace", { operation: "sidebar.workspace.name", context: { length: 3 } });
  });
});

describe("the sidebar's name popups", () => {
  // EVERY NAME THE SIDEBAR ASKS FOR IS ASKED THROUGH openNamePopup: the
  // popup's class, input and button are spelled in name-popup.ts alone, so a
  // hand-rolled second popup fails here rather than drifting from the first.
  it("are all drawn by openNamePopup", () => {
    const dir = path.join(path.dirname(fileURLToPath(import.meta.url)), "../../src/sidebar");
    const spelled = readdirSync(dir)
      .filter((file) => file.endsWith(".ts"))
      .filter((file) => /sb-subsection-popup|sb-subsection-name/.test(readFileSync(path.join(dir, file), "utf8")));
    const openers = readdirSync(dir)
      .filter((file) => file.endsWith(".ts") && file !== "name-popup.ts")
      .filter((file) => /openNamePopup\(/.test(readFileSync(path.join(dir, file), "utf8")));
    expect([spelled, openers.sort()]).toEqual([["name-popup.ts"], ["rename.ts", "subsections.ts"]]);
  });
});
