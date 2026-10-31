// @vitest-environment jsdom
/** The run control a shell code block carries, and which fences carry one. */
import { afterEach, describe, expect, it } from "vitest";
import { CODE_RUN_CLASS, isShellLang, RUN_CONTROL_CLASS, RUN_CONTROL_HTML, RUN_CONTROL_LABEL } from "../src/run-control.js";

afterEach(() => {
  document.body.innerHTML = "";
});

describe("isShellLang", () => {
  it.each(["bash", "sh", "shell", "zsh", "Bash", "SHELL"])("marks a %s fence as a shell command", (lang) => {
    // Act / Assert
    expect(isShellLang(lang)).toBe(true);
  });

  it.each(["", "go", "elisp", "console", "text", "bashrc"])("leaves a %j fence without a run control", (lang) => {
    // Act / Assert
    expect(isShellLang(lang)).toBe(false);
  });
});

describe("RUN_CONTROL_HTML", () => {
  it("is an ar-button wearing the run control's classes", () => {
    // Arrange
    document.body.innerHTML = RUN_CONTROL_HTML;
    // Act
    const control = document.body.firstElementChild;
    // Assert
    expect([control?.localName, control?.classList.contains(RUN_CONTROL_CLASS), control?.classList.contains(CODE_RUN_CLASS)]).toEqual([
      "ar-button",
      true,
      true,
    ]);
  });

  it("labels itself on hover and for assistive technology", () => {
    // Arrange
    document.body.innerHTML = RUN_CONTROL_HTML;
    // Act
    const control = document.body.firstElementChild;
    // Assert
    expect([control?.getAttribute("title"), control?.getAttribute("aria-label")]).toEqual([RUN_CONTROL_LABEL, RUN_CONTROL_LABEL]);
  });

  it("holds no text, so the block's text stays its code alone", () => {
    // Arrange
    document.body.innerHTML = RUN_CONTROL_HTML;
    // Act / Assert
    expect(document.body.textContent).toBe("");
  });
});
