// The stops of the boot's page-lifetime installs. See src/page-teardown.ts.
import { afterEach, describe, expect, it } from "vitest";

import {
  abandonedAtPageEnd,
  beginPage,
  endPage,
  endPageAtPagehide,
  pageEnded,
  pageEnding,
  stopAtPageTeardown,
  tearDownPage,
} from "../src/page-teardown.js";
import { settledByNextTask } from "./settled.js";
import path from "node:path";
import { codeUnder } from "./files-under.js";

afterEach(() => beginPage());

describe("tearDownPage", () => {
  it("runs every registered stop, the latest first", () => {
    // Arrange
    const ran: string[] = [];
    stopAtPageTeardown(() => ran.push("census"));
    stopAtPageTeardown(() => ran.push("probe"));
    // Act
    tearDownPage();
    // Assert
    expect(ran).toEqual(["probe", "census"]);
  });

  it("forgets the stops it ran, so a second teardown runs none", () => {
    // Arrange
    let runs = 0;
    stopAtPageTeardown(() => (runs += 1));
    tearDownPage();
    // Act
    tearDownPage();
    // Assert
    expect(runs).toBe(1);
  });

  it("does nothing when nothing was registered", () => {
    expect(() => tearDownPage()).not.toThrow();
  });
});

describe("the page's end", () => {
  it("aborts the signal every request of the page carries", () => {
    // Arrange
    beginPage();
    const signal = pageEnding();
    // Act
    endPage();
    // Assert
    expect([signal.aborted, pageEnded()]).toEqual([true, true]);
  });

  it("runs the page's stops", () => {
    // Arrange
    beginPage();
    let ran = false;
    stopAtPageTeardown(() => (ran = true));
    // Act
    endPage();
    // Assert
    expect(ran).toBe(true);
  });

  it("begins the next page with a signal of its own", () => {
    // Arrange
    beginPage();
    const ended = pageEnding();
    endPage();
    // Act
    beginPage();
    // Assert
    expect([ended.aborted, pageEnding().aborted, pageEnded()]).toEqual([true, false, false]);
  });

  it.each([
    { persisted: false, ends: true },
    { persisted: true, ends: false },
  ])("ends at a pagehide unless the page is kept to come back (persisted $persisted)", ({ persisted, ends }) => {
    // Arrange
    beginPage();
    const win = new EventTarget() as Window;
    endPageAtPagehide(win);
    const pagehide = Object.assign(new Event("pagehide"), { persisted });
    // Act
    win.dispatchEvent(pagehide);
    // Assert
    expect(pageEnded()).toBe(ends);
  });

  it("stops ending the page at a pagehide once the page is torn down", () => {
    // Arrange
    beginPage();
    const win = new EventTarget() as Window;
    endPageAtPagehide(win);
    tearDownPage();
    // Act
    win.dispatchEvent(Object.assign(new Event("pagehide"), { persisted: false }));
    // Assert
    expect(pageEnded()).toBe(false);
  });

  it("answers a cut-off call with a promise that never settles", async () => {
    // Act
    const settled = await settledByNextTask(abandonedAtPageEnd<number>());
    // Assert
    expect(settled).toBe(false);
  });
});

describe("who ends a document at its unload", () => {
  it("is page-teardown.ts alone, besides the observers dom.ts disconnects", () => {
    // Arrange
    const owners = new Set(["page-teardown.ts", "dom.ts"]);

    // Act
    const others = [...codeUnder(path.join(__dirname, "..", "src"))]
      .filter(([file, code]) => !owners.has(file) && code.includes('"pagehide"'))
      .map(([file]) => file);

    // Assert
    expect(others).toEqual([]);
  });
});
