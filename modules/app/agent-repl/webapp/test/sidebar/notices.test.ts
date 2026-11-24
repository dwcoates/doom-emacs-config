// @vitest-environment jsdom
// The sidebar's own warnings and errors, which tint its version. See
// src/sidebar/notices.ts.
import { afterEach, describe, expect, it } from "vitest";

import { NOTICE_LIMIT, createSidebarNotices, installVersionTint, noticesToneClass } from "../../src/sidebar/notices.js";

/** A record's JSON line, as the logger writes it. */
function line(message: string, operation = "test.op", timestamp = "2026-10-31T12:00:00.000Z"): string {
  return JSON.stringify({ timestamp, operation, message, level: "warn" });
}

let uninstall: (() => void) | null = null;

afterEach(() => {
  uninstall?.();
  uninstall = null;
  document.body.replaceChildren();
});

/** A version with its tint installed. */
function install() {
  const version = document.createElement("span");
  document.body.appendChild(version);
  const notices = createSidebarNotices();
  uninstall = installVersionTint(version, notices);
  return { version, notices };
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

describe("installVersionTint", () => {
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

  it("opens nothing on a click on the version", () => {
    // Arrange
    const { version, notices } = install();
    notices.record("warn", line("careful"));

    // Act
    version.click();

    // Assert
    expect(document.body.innerHTML).toBe(version.outerHTML);
  });

  it("stops re-tinting once uninstalled", () => {
    // Arrange
    const { version, notices } = install();
    uninstall?.();
    uninstall = null;

    // Act
    notices.record("error", line("broken"));

    // Assert
    expect(version.classList.contains("notices-ok")).toBe(true);
  });
});
