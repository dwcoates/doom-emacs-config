import { describe, expect, it } from "vitest";
import {
  codeSpanFileHref,
  FILE_LINK_EXTENSIONS,
  fileLinkLabel,
  hasFileExtension,
  isFileLinkHref,
  isPathLinkText,
  isWebHref,
} from "../src/href.js";

describe("isWebHref", () => {
  it.each([
    ["https://a.test/x", true],
    ["HTTP://a.test", true],
    ["a.go", false],
    ["file:///a", false],
  ])("%s -> %s", (href, want) => {
    expect(isWebHref(href)).toBe(want);
  });
});

describe("isFileLinkHref", () => {
  it.each([
    ["a.go", true],
    ["lisp/a.el", true],
    ["/abs/a.el", true],
    ["~/a.el", true],
    ["a.el:12", true],
    ["javascript:alert(1)", false],
    ["mailto:a@b.test", false],
    ["https://a.test", false],
    ["file:///a", false],
    ["#frag", false],
    ["//host/x", false],
    ["", false],
  ])("%s -> %s", (href, want) => {
    expect(isFileLinkHref(href)).toBe(want);
  });
});

describe("hasFileExtension", () => {
  it.each([
    ["README.md", true],
    ["FOO.TS", true],
    ["a.b.go", true],
    ["example.com", false],
    ["github.io", false],
    ["wikipedia.org", true],
    ["noext", false],
    [".md", false],
  ])("%s -> %s", (label, want) => {
    expect(hasFileExtension(label)).toBe(want);
  });

  it("names the extensions in one constant", () => {
    expect(FILE_LINK_EXTENSIONS).toContain("proto");
  });
});

describe("codeSpanFileHref", () => {
  it.each([
    { name: "a home-relative file", code: "~/.config/doom/a.el", want: "~/.config/doom/a.el" },
    { name: "a home-relative file with a line", code: "~/a/b.go:193", want: "~/a/b.go:193" },
    { name: "a home-relative directory", code: "~/.config/emacs/.local/", want: "~/.config/emacs/.local/" },
    { name: "a bare file name", code: "README.md", want: null },
    { name: "a relative path", code: "lisp/status.el", want: null },
    { name: "an absolute path", code: "/Users/u/a.el", want: null },
    { name: "another user's home", code: "~user/a.el", want: null },
    { name: "a path followed by more words", code: "~/a.el foo", want: null },
    { name: "a bare tilde-slash", code: "~/", want: null },
    { name: "an identifier", code: "agent-repl-send", want: null },
  ])("answers $want for $name", ({ code, want }) => {
    // Act + Assert
    expect(codeSpanFileHref(code)).toBe(want);
  });
});

describe("isPathLinkText", () => {
  it.each([
    { name: "a home-relative path", text: "~/a/b.ts", want: true },
    { name: "a path with a line", text: "~/a/b.ts:12", want: true },
    { name: "a relative path", text: "lisp/status.el", want: true },
    { name: "an absolute path", text: "/Users/u/a.el", want: true },
    { name: "a bare file name", text: "README.md", want: false },
    { name: "prose", text: "the config file", want: false },
    { name: "a path with more words", text: "~/a.el foo", want: false },
    { name: "a web URL", text: "https://x.test/a", want: false },
  ])("answers $want for $name", ({ text, want }) => {
    expect(isPathLinkText(text)).toBe(want);
  });
});

describe("fileLinkLabel", () => {
  it.each([
    { name: "a file", path: "~/a/b.ts", want: "b.ts" },
    { name: "a file with a line", path: "~/a/b.go:193", want: "b.go:193" },
    { name: "a directory", path: "~/a/b/", want: "b/" },
    { name: "a relative path", path: "lisp/status.el", want: "status.el" },
    { name: "a bare name", path: "README.md", want: "README.md" },
    { name: "the root", path: "/", want: "/" },
  ])("draws $want for $name", ({ path, want }) => {
    expect(fileLinkLabel(path)).toBe(want);
  });
});
