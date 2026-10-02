import { describe, expect, it } from "vitest";
import { FILE_LINK_EXTENSIONS, hasFileExtension, isFileLinkHref, isWebHref } from "../src/href.js";

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
    ["noext", false],
    [".md", false],
  ])("%s -> %s", (label, want) => {
    expect(hasFileExtension(label)).toBe(want);
  });

  it("names the extensions in one constant", () => {
    expect(FILE_LINK_EXTENSIONS).toContain("proto");
  });
});
