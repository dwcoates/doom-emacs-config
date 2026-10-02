// @vitest-environment jsdom
/** The one control, and the proof nothing in src builds a `<button>` itself. */
import { readdirSync, readFileSync, statSync } from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { afterEach, describe, expect, it } from "vitest";
import { CONTROL_SELECTOR, armButtonRole, createControl, isControl } from "../src/control.js";
import { captureLogRecords, forwardedRecord } from "./log-capture.js";
import { codeOf, withoutBlockComments } from "./source-text.js";

const here = path.dirname(fileURLToPath(import.meta.url));
const SRC = path.join(here, "../src");

afterEach(() => {
  document.body.innerHTML = "";
});

/** Every source file under DIR with one of EXTENSIONS, recursively. */
function sources(dir: string, extensions: readonly string[]): string[] {
  return readdirSync(dir).flatMap((name) => {
    const full = path.join(dir, name);
    if (statSync(full).isDirectory()) return sources(full, extensions);
    return extensions.some((ext) => full.endsWith(ext)) ? [full] : [];
  });
}

/** A mounted control, counting the clicks that reach its own handler. */
function mounted(): { control: ReturnType<typeof createControl>; clicks: () => number } {
  const control = createControl();
  document.body.append(control);
  let clicks = 0;
  control.addEventListener("click", () => clicks++);
  return { control, clicks: () => clicks };
}

/** Press KEY on EL: its keydown, then its keyup. */
function press(el: HTMLElement, key: string): void {
  el.dispatchEvent(new KeyboardEvent("keydown", { key, bubbles: true, cancelable: true }));
  el.dispatchEvent(new KeyboardEvent("keyup", { key, bubbles: true, cancelable: true }));
}

describe("createControl", () => {
  it("is an ar-button element, never a button", () => {
    expect(createControl().localName).toBe("ar-button");
  });

  it("carries the button role", () => {
    expect(createControl().getAttribute("role")).toBe("button");
  });

  it("is focusable", () => {
    expect(createControl().tabIndex).toBe(0);
  });

  it("starts enabled", () => {
    expect(createControl().disabled).toBe(false);
  });

  it("reflects disabled as aria-disabled", () => {
    // Arrange
    const control = createControl();
    // Act
    control.disabled = true;
    // Assert
    expect(control.getAttribute("aria-disabled")).toBe("true");
  });

  it("leaves the tab order while disabled", () => {
    // Arrange
    const control = createControl();
    // Act
    control.disabled = true;
    // Assert
    expect(control.tabIndex).toBe(-1);
  });

  it("drops aria-disabled and rejoins the tab order when re-enabled", () => {
    // Arrange
    const control = createControl();
    control.disabled = true;
    // Act
    control.disabled = false;
    // Assert
    expect([control.hasAttribute("aria-disabled"), control.tabIndex]).toEqual([false, 0]);
  });

  it("reads disabled off the attribute, so a morphed attribute is the state", () => {
    // Arrange
    const control = createControl();
    // Act
    control.setAttribute("aria-disabled", "true");
    // Assert
    expect(control.disabled).toBe(true);
  });

  it("activates on Enter", () => {
    // Arrange
    const { control, clicks } = mounted();
    // Act
    control.dispatchEvent(new KeyboardEvent("keydown", { key: "Enter", bubbles: true, cancelable: true }));
    // Assert
    expect(clicks()).toBe(1);
  });

  it("activates on Space when the key comes up", () => {
    // Arrange
    const { control, clicks } = mounted();
    // Act
    press(control, " ");
    // Assert
    expect(clicks()).toBe(1);
  });

  it("does not activate on Space's keydown alone", () => {
    // Arrange
    const { control, clicks } = mounted();
    // Act
    control.dispatchEvent(new KeyboardEvent("keydown", { key: " ", bubbles: true, cancelable: true }));
    // Assert
    expect(clicks()).toBe(0);
  });

  it("keeps Space from scrolling the page", () => {
    // Arrange
    const { control } = mounted();
    const event = new KeyboardEvent("keydown", { key: " ", bubbles: true, cancelable: true });
    // Act
    control.dispatchEvent(event);
    // Assert
    expect(event.defaultPrevented).toBe(true);
  });

  it("ignores other keys", () => {
    // Arrange
    const { control, clicks } = mounted();
    // Act
    press(control, "a");
    // Assert
    expect(clicks()).toBe(0);
  });

  it("activates on a click while enabled", () => {
    // Arrange
    const { control, clicks } = mounted();
    // Act
    control.click();
    // Assert
    expect(clicks()).toBe(1);
  });

  it("refuses a click while disabled", () => {
    // Arrange
    const { control, clicks } = mounted();
    control.disabled = true;
    // Act
    control.click();
    // Assert
    expect(clicks()).toBe(0);
  });

  it("refuses Enter while disabled", () => {
    // Arrange
    const { control, clicks } = mounted();
    control.disabled = true;
    // Act
    press(control, "Enter");
    // Assert
    expect(clicks()).toBe(0);
  });

  it("keeps a refused click from reaching its ancestors", () => {
    // Arrange
    const { control } = mounted();
    let reached = 0;
    document.body.addEventListener("click", () => reached++);
    control.disabled = true;
    // Act
    control.click();
    // Assert
    expect(reached).toBe(0);
  });

  it("records a refused click at debug", async () => {
    // Arrange
    const capture = captureLogRecords("debug");
    const { control } = mounted();
    control.className = "perm-button";
    control.disabled = true;
    // Act
    control.click();
    // Assert
    const record = await forwardedRecord(capture, "control.click-refused");
    expect([record.level.case, record.context]).toEqual([
      "debug",
      expect.objectContaining({ classes: "perm-button" }),
    ]);
  });
});

describe("armButtonRole", () => {
  /** A plain div, armed, counting the clicks that reach it. */
  function armed(): { el: HTMLElement; clicks: () => number } {
    const el = document.createElement("div");
    armButtonRole(el);
    document.body.append(el);
    let clicks = 0;
    el.addEventListener("click", () => clicks++);
    return { el, clicks: () => clicks };
  }

  it("gives an element the button role", () => {
    expect(armed().el.getAttribute("role")).toBe("button");
  });

  it("makes an element focusable", () => {
    expect(armed().el.tabIndex).toBe(0);
  });

  it("leaves an element already aria-disabled out of the tab order", () => {
    // Arrange
    const el = document.createElement("div");
    el.setAttribute("aria-disabled", "true");
    // Act
    armButtonRole(el);
    // Assert
    expect(el.tabIndex).toBe(-1);
  });

  it("activates an armed element on Enter", () => {
    // Arrange
    const { el, clicks } = armed();
    // Act
    el.dispatchEvent(new KeyboardEvent("keydown", { key: "Enter", bubbles: true, cancelable: true }));
    // Assert
    expect(clicks()).toBe(1);
  });

  it("leaves a key pressed on something inside the element to that thing", () => {
    // Arrange
    const { el, clicks } = armed();
    const inner = document.createElement("a");
    el.append(inner);
    // Act
    inner.dispatchEvent(new KeyboardEvent("keydown", { key: "Enter", bubbles: true, cancelable: true }));
    // Assert
    expect(clicks()).toBe(0);
  });

  it("refuses a click on an armed element while it is aria-disabled", () => {
    // Arrange
    const { el, clicks } = armed();
    el.setAttribute("aria-disabled", "true");
    // Act
    el.click();
    // Assert
    expect(clicks()).toBe(0);
  });
});

describe("isControl", () => {
  it("recognizes a control", () => {
    expect(isControl(createControl())).toBe(true);
  });

  it("refuses a native button", () => {
    expect(isControl(document.createElement("button"))).toBe(false);
  });

  it("refuses a non-element", () => {
    expect(isControl("ar-button")).toBe(false);
  });
});

describe("the one control", () => {
  it("is what the control selector finds", () => {
    // Arrange
    document.body.append(createControl());
    // Act / Assert
    expect(document.querySelectorAll(CONTROL_SELECTOR)).toHaveLength(1);
  });

  it("is the only way src builds a clickable control: no module creates a <button>", () => {
    // Arrange
    const files = sources(SRC, [".ts", ".html"]).concat([path.join(here, "../index.html")]);
    // Act
    const handRolled = files
      .filter((file) => {
        const code = file.endsWith(".ts") ? codeOf(readFileSync(file, "utf8")) : readFileSync(file, "utf8");
        return /createElement\(\s*["'`]button["'`]\s*\)|<button[\s>]/i.test(code);
      })
      .map((file) => path.relative(path.join(here, ".."), file));
    // Assert
    expect(handRolled).toEqual([]);
  });

  it("styles no native button: the stylesheet names ar-button, never button", () => {
    // Arrange
    const css = withoutBlockComments(readFileSync(path.join(SRC, "styles.css"), "utf8"));
    // Act
    const selectors = [...css.matchAll(/([^{}]+)\{/g)].map((m) => m[1]);
    const native = selectors.filter((sel) => /(?<![-\w.#:])button(?![-\w])/.test(sel));
    // Assert
    expect(native).toEqual([]);
  });

  it("styles no :disabled control: a control's disabled state is aria-disabled", () => {
    // Arrange
    const css = withoutBlockComments(readFileSync(path.join(SRC, "styles.css"), "utf8"));
    // Act / Assert
    expect(css.includes(":disabled")).toBe(false);
  });
});
