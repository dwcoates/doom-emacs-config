// @vitest-environment jsdom
// The sidebar's own warnings and errors, behind its version. See
// src/sidebar/notices.ts.
import { afterEach, describe, expect, it } from "vitest";

import { createLocalFailures, daemonUnreachable } from "../../src/failure/local.js";
import { createDropdowns } from "../../src/sidebar/dropdowns.js";
import {
  NOTICE_LIMIT,
  createSidebarNotices,
  installVersionNotices,
  noticesToneClass,
  type VersionNotices,
} from "../../src/sidebar/notices.js";

/** A record's JSON line, as the logger writes it. */
function line(message: string, operation = "test.op", timestamp = "2026-10-31T12:00:00.000Z"): string {
  return JSON.stringify({ timestamp, operation, message, level: "warn" });
}

let installed: VersionNotices | null = null;

afterEach(() => {
  installed?.dispose();
  installed = null;
  document.body.replaceChildren();
});

/** A version in a rail head, with notices installed on it. */
function install() {
  const rail = document.createElement("div");
  const version = document.createElement("span");
  rail.appendChild(version);
  document.body.appendChild(rail);
  const notices = createSidebarNotices();
  const failures = createLocalFailures();
  const dropdowns = createDropdowns();
  installed = installVersionNotices(version, rail, failures, notices, dropdowns);
  const panel = (): HTMLElement => rail.querySelector<HTMLElement>(".sb-notices")!;
  return { rail, version, notices, failures, dropdowns, panel };
}

describe("createSidebarNotices", () => {
  it("is green with nothing recorded", () => {
    expect(createSidebarNotices().tone()).toBe("ok");
  });

  it("is yellow after a warning", () => {
    // Arrange
    const notices = createSidebarNotices();

    // Act
    notices.record("warn", line("careful"));

    // Assert
    expect(notices.tone()).toBe("warn");
  });

  it("is red after an error, whatever warnings came with it", () => {
    // Arrange
    const notices = createSidebarNotices();
    notices.record("warn", line("careful"));

    // Act
    notices.record("error", line("broken"));

    // Assert
    expect(notices.tone()).toBe("error");
  });

  it("ignores records below warn", () => {
    // Arrange
    const notices = createSidebarNotices();

    // Act
    notices.record("info", line("fine"));
    notices.record("debug", line("fine"));

    // Assert
    expect([notices.tone(), notices.entries()]).toEqual(["ok", []]);
  });

  it("keeps a record's level, timestamp, operation and message", () => {
    // Arrange
    const notices = createSidebarNotices();

    // Act
    notices.record("error", line("broken", "sidebar.roster", "2026-10-31T01:02:03.000Z"));

    // Assert
    expect(notices.entries()).toEqual([
      { level: "error", timestamp: "2026-10-31T01:02:03.000Z", operation: "sidebar.roster", message: "broken" },
    ]);
  });

  it("keeps only the newest records past its limit", () => {
    // Arrange
    const notices = createSidebarNotices();

    // Act
    for (let i = 0; i <= NOTICE_LIMIT; i += 1) notices.record("warn", line(`w${i}`));

    // Assert
    expect([notices.entries().length, notices.entries()[0]?.message]).toEqual([NOTICE_LIMIT, "w1"]);
  });

  it("stays red once the error has scrolled out of the list", () => {
    // Arrange
    const notices = createSidebarNotices();
    notices.record("error", line("broken"));

    // Act
    for (let i = 0; i < NOTICE_LIMIT; i += 1) notices.record("warn", line(`w${i}`));

    // Assert
    expect(notices.tone()).toBe("error");
  });

  it("tells its listeners every record it takes", () => {
    // Arrange
    const notices = createSidebarNotices();
    let told = 0;
    notices.subscribe(() => (told += 1));

    // Act
    notices.record("warn", line("careful"));
    notices.record("info", line("fine"));

    // Assert
    expect(told).toBe(1);
  });
});

describe("installVersionNotices", () => {
  it("tints the version green with nothing recorded", () => {
    // Act
    const { version } = install();

    // Assert
    expect(version.classList.contains(noticesToneClass("ok"))).toBe(true);
  });

  it("re-tints the version as records arrive", () => {
    // Arrange
    const { version, notices } = install();

    // Act
    notices.record("error", line("broken"));

    // Assert
    expect([version.classList.contains("notices-error"), version.classList.contains("notices-ok")]).toEqual([true, false]);
  });

  it("opens the list on a click on the version", () => {
    // Arrange
    const { version, panel } = install();

    // Act
    version.click();

    // Assert
    expect(panel().classList.contains("open")).toBe(true);
  });

  it("says there is nothing to show when nothing was recorded", () => {
    // Arrange
    const { version, panel } = install();

    // Act
    version.click();

    // Assert
    expect(panel().querySelector(".sb-notices-none")).not.toBeNull();
  });

  it("lists the records newest first", () => {
    // Arrange
    const { version, notices, panel } = install();
    notices.record("warn", line("first"));
    notices.record("error", line("second"));

    // Act
    version.click();

    // Assert
    expect([...panel().querySelectorAll(".sb-notices-list li span")].map((e) => e.textContent)).toEqual(["second", "first"]);
  });

  it("lists the failures standing now above the records", () => {
    // Arrange
    const { version, failures, panel } = install();
    failures.report(daemonUnreachable(1006, "gone"));

    // Act
    version.click();

    // Assert
    expect(panel().querySelectorAll(".sb-notices-standing li").length).toBe(1);
  });

  it("refills the open list as records arrive", () => {
    // Arrange
    const { version, notices, panel } = install();
    version.click();

    // Act
    notices.record("warn", line("late"));

    // Assert
    expect(panel().querySelector(".sb-notices-list li span")?.textContent).toBe("late");
  });

  it("closes the list on a second click on the version", () => {
    // Arrange
    const { version, panel } = install();
    version.click();

    // Act
    version.click();

    // Assert
    expect(panel().classList.contains("open")).toBe(false);
  });

  it("closes the list on a click elsewhere in the rail", () => {
    // Arrange
    const { version, dropdowns, rail, panel } = install();
    version.click();

    // Act
    dropdowns.dismissOutside(rail);

    // Assert
    expect(panel().classList.contains("open")).toBe(false);
  });

  it("removes its panel when disposed", () => {
    // Arrange
    const { rail } = install();

    // Act
    installed?.dispose();
    installed = null;

    // Assert
    expect(rail.querySelector(".sb-notices")).toBeNull();
  });
});
