// @vitest-environment jsdom
import { describe, expect, it } from "vitest";
import { readWebappBuild, WebappBuildUnknown } from "../src/webapp-build.js";
import { captureLogRecords, forwardedRecord } from "./log-capture.js";

/** A document carrying zero or more `<script>` tags, in `<head>`. */
function docWithScripts(
  scripts: ReadonlyArray<{ type?: string; src?: string }>,
): Document {
  const doc = document.implementation.createHTMLDocument("webapp-build");
  for (const spec of scripts) {
    const element = doc.createElement("script");
    if (spec.type !== undefined) element.setAttribute("type", spec.type);
    if (spec.src !== undefined) element.setAttribute("src", spec.src);
    doc.head.append(element);
  }
  return doc;
}

describe("readWebappBuild", () => {
  const cases: ReadonlyArray<{
    name: string;
    scripts: ReadonlyArray<{ type?: string; src?: string }>;
    expected: string;
  }> = [
    {
      name: "reads the hash out of the built entry script",
      scripts: [{ type: "module", src: "/assets/index-aBc123_-.js" }],
      expected: "aBc123_-",
    },
    {
      name: "finds the entry tag among other module scripts",
      scripts: [
        { type: "module", src: "/assets/vendor-xyz.js" },
        { type: "module", src: "/assets/index-def456.js" },
      ],
      expected: "def456",
    },
  ];

  it.each(cases)("$name", ({ scripts, expected }) => {
    // Arrange
    const doc = docWithScripts(scripts);
    // Act
    const build = readWebappBuild(doc);
    // Assert
    expect(build).toBe(expected);
  });

  it("fails loudly, through the canonical logger, when the page has no built entry tag", async () => {
    // Arrange
    const capture = captureLogRecords("error");
    const doc = docWithScripts([{ type: "module", src: "/src/main.ts" }]);
    // Act + Assert: the missing-build failure throws rather than answering empty.
    expect(() => readWebappBuild(doc)).toThrow(WebappBuildUnknown);
    const record = await forwardedRecord(capture, "webapp-build.missing");
    expect(record.level.case).toBe("error");
  });

  it("fails loudly when the page carries no script tags at all", () => {
    // Arrange
    const doc = docWithScripts([]);
    // Act + Assert
    expect(() => readWebappBuild(doc)).toThrow(WebappBuildUnknown);
  });
});
