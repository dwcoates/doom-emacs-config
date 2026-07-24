import { describe, it, expect } from "vitest";
import { previewFromInput } from "../src/permission-preview.js";

describe("previewFromInput", () => {
  it("shows the command a Bash permission would run", () => {
    // Arrange + Act
    const preview = previewFromInput("Bash", { command: "rm -rf /tmp/x" });
    // Assert
    expect(preview).toEqual({ kind: "bash", command: "rm -rf /tmp/x" });
  });

  it("shows the path and size a Write permission would create", () => {
    // Arrange + Act
    const preview = previewFromInput("Write", { file_path: "/w/a.ts", content: "hello" });
    // Assert
    expect(preview).toEqual({
      kind: "write",
      file_path: "/w/a.ts",
      bytes: 5,
      preview: "hello",
    });
  });

  it("truncates a large written body rather than pasting the whole file", () => {
    // Arrange
    const content = "x".repeat(5000);
    // Act
    const preview = previewFromInput("Write", { file_path: "/w/a.ts", content });
    // Assert
    expect(preview).toMatchObject({ bytes: 5000, preview: "x".repeat(2000) });
  });

  it("names the file an Edit would change without claiming a diff it lacks", () => {
    // Arrange + Act
    const preview = previewFromInput("Edit", { file_path: "/w/a.ts", old_string: "a" });
    // Assert
    expect(preview).toEqual({ kind: "generic", summary: "Edit: /w/a.ts" });
  });

  it("falls back to the arguments themselves for an unknown tool", () => {
    // Arrange + Act
    const preview = previewFromInput("Mystery", { pattern: "*.ts" });
    // Assert
    expect(preview).toEqual({ kind: "generic", summary: '{\n  "pattern": "*.ts"\n}' });
  });

  it("shows nothing for an argument-less tool rather than an empty box", () => {
    // Arrange + Act
    const preview = previewFromInput("Mystery", {});
    // Assert
    expect(preview).toBeUndefined();
  });

  it("shows nothing when the input is not an argument object at all", () => {
    // Arrange + Act
    const preview = previewFromInput("Mystery", "not-an-object");
    // Assert
    expect(preview).toBeUndefined();
  });

  it("does not treat a non-Bash tool's command field as a shell command", () => {
    // Arrange — only Bash actually runs its `command` in a shell.
    const preview = previewFromInput("Task", { command: "look-like-bash" });
    // Assert
    expect(preview?.kind).toBe("generic");
  });
});
