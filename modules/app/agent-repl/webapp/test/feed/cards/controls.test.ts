// @vitest-environment jsdom
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import {
  agenticBubble,
  clearRefusals,
  foldSection,
  initialFold,
  refusal,
  release,
  whileInFlight,
} from "../../../src/feed/cards/controls.js";
import type { RowContext } from "../../../src/feed/renderers.js";
import { harness, rowContext } from "../harness.js";
import { create } from "@bufbuild/protobuf";
import { FeedRowSchema } from "../../../../proto/gen/ts/frontend/v1/feed_pb";

/** A row context whose only interesting part is `previous`. */
function rc(previous?: HTMLElement): RowContext {
  return rowContext(harness().ctx, create(FeedRowSchema, {}), { previous });
}

/** A fold section over a marked body. */
function fold(folded: boolean, previous?: HTMLElement): HTMLElement {
  const body = document.createElement("div");
  body.className = "the-body";
  return foldSection({ name: "doc", label: "document", body, folded, rc: rc(previous) });
}

/** A disabled-state reading of BUTTONS, for the in-flight assertions. */
function button(): HTMLButtonElement {
  const el = document.createElement("button");
  el.type = "button";
  return el;
}

beforeEach(() => {
  vi.useFakeTimers();
});

// The fake clock is this file's own; hand the real one back so a
// later file sharing this worker never inherits a frozen timer.
afterEach(() => {
  vi.useRealTimers();
});

describe("refusal", () => {
  it("carries the error's own arm name", () => {
    expect(refusal("confirmRequired", "no").getAttribute("data-arm")).toBe("confirmRequired");
  });

  it("draws the sentence it was given", () => {
    expect(refusal("transport", "the daemon could not be reached").textContent).toBe(
      "the daemon could not be reached",
    );
  });

  it("wears the shared refusal class the integration suite targets", () => {
    expect(refusal("transport", "no").className).toBe("refusal");
  });
});

describe("clearRefusals", () => {
  it("removes a refusal a previous click left behind", () => {
    const host = document.createElement("div");
    host.append(refusal("transport", "no"));
    clearRefusals(host);
    expect(host.querySelector(".refusal")).toBeNull();
  });

  it("leaves everything that is not a refusal alone", () => {
    const host = document.createElement("div");
    host.append(button(), refusal("transport", "no"));
    clearRefusals(host);
    expect(host.querySelector("button")).not.toBeNull();
  });
});

describe("foldSection", () => {
  it("starts folded when the wire said folded", () => {
    expect(fold(true).querySelector<HTMLElement>(".the-body")?.hidden).toBe(true);
  });

  it("starts open when the wire said open", () => {
    expect(fold(false).querySelector<HTMLElement>(".the-body")?.hidden).toBe(false);
  });

  it("draws the closed caret while folded", () => {
    expect(fold(true).querySelector("[data-fold]")?.textContent).toBe("▸ document");
  });

  it("draws the open caret while unfolded", () => {
    expect(fold(false).querySelector("[data-fold]")?.textContent).toBe("▾ document");
  });

  it("names the fold so a redraw can find its state", () => {
    expect(fold(true).querySelector("[data-fold]")?.getAttribute("data-fold")).toBe("doc");
  });

  it("opens on the reader's click", () => {
    const el = fold(true);
    el.querySelector<HTMLButtonElement>("[data-fold]")?.click();
    expect(el.querySelector<HTMLElement>(".the-body")?.hidden).toBe(false);
  });

  it("closes again on a second click", () => {
    const el = fold(true);
    const toggle = el.querySelector<HTMLButtonElement>("[data-fold]");
    toggle?.click();
    toggle?.click();
    expect(el.querySelector<HTMLElement>(".the-body")?.hidden).toBe(true);
  });

  it("keeps the body in the DOM while folded, so its ticking survives", () => {
    expect(fold(true).querySelector(".the-body")).not.toBeNull();
  });
});

describe("initialFold", () => {
  const cases = [
    { name: "the wire decides the first draw", previous: null, wire: true, want: true },
    { name: "the wire's open state decides the first draw", previous: null, wire: false, want: false },
    { name: "the reader's open fold survives a push that said folded", previous: "false", wire: true, want: false },
    { name: "the reader's closed fold survives a push that said open", previous: "true", wire: false, want: true },
  ] as const;

  for (const c of cases) {
    it(c.name, () => {
      let previous: HTMLElement | undefined;
      if (c.previous !== null) {
        previous = document.createElement("div");
        const toggle = document.createElement("button");
        toggle.setAttribute("data-fold", "doc");
        toggle.setAttribute("data-folded", c.previous);
        previous.append(toggle);
      }
      expect(initialFold("doc", c.wire, rc(previous))).toBe(c.want);
    });
  }

  it("ignores a previous element that carried some OTHER card's fold", () => {
    const previous = document.createElement("div");
    const toggle = document.createElement("button");
    toggle.setAttribute("data-fold", "scenario");
    toggle.setAttribute("data-folded", "false");
    previous.append(toggle);
    expect(initialFold("doc", true, rc(previous))).toBe(true);
  });
});

describe("whileInFlight", () => {
  it("latches the controls inert for the duration of the call", async () => {
    const b = button();
    let seen = false;
    await whileInFlight([b], async () => {
      seen = b.disabled;
    });
    expect(seen).toBe(true);
  });

  it("hands back what the call answered", async () => {
    const answered = await whileInFlight([button()], async () => 7);
    expect(answered).toEqual({ value: 7 });
  });

  it("keeps the controls inert after an answer that landed", async () => {
    const b = button();
    await whileInFlight([b], async () => 7);
    expect(b.disabled).toBe(true);
  });

  it("gives the controls back when the call threw", async () => {
    const b = button();
    await whileInFlight([b], async () => {
      throw new Error("boom");
    });
    expect(b.disabled).toBe(false);
  });

  it("reports the failure rather than swallowing it", async () => {
    const err = new Error("boom");
    const answered = await whileInFlight([button()], async () => {
      throw err;
    });
    expect(answered).toEqual({ failed: err });
  });
});

describe("release", () => {
  it("gives a latched control back", () => {
    const b = button();
    b.disabled = true;
    release([b]);
    expect(b.disabled).toBe(false);
  });
});

describe("agenticBubble", () => {
  it("is the response bubble plus the one purple accent", () => {
    expect(agenticBubble({ state: "published", content: [] }).className).toBe(
      "bubble md assistant agentic",
    );
  });

  it("carries the message's own arm as its state", () => {
    expect(agenticBubble({ state: "publishing", content: [] }).getAttribute("data-state")).toBe(
      "publishing",
    );
  });

  it("draws the heading verbatim, favicon emoji and all", () => {
    const bubble = agenticBubble({ state: "published", heading: "📊 Merge Queue Report", content: [] });
    expect(bubble.querySelector(".agentic-heading")?.textContent).toBe("📊 Merge Queue Report");
  });

  it("draws no heading element for a message that has none", () => {
    expect(agenticBubble({ state: "planning", content: [] }).querySelector(".agentic-heading")).toBeNull();
  });
});
