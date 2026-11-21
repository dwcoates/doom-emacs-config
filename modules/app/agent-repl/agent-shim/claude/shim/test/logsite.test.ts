/**
 * agent-shim/logging/ts/logsite.ts, the site key and template every
 * TypeScript runtime derives its records' sites with. Its key is pinned to
 * the value agent-shim/logging/go/logsite pins, so every runtime derives one.
 */
import { createHash } from "node:crypto";
import { describe, expect, it } from "vitest";
import {
  parseTemplate,
  placeholders,
  renderSite,
  renderTemplate,
  sha256Hex,
  siteKey,
  SiteTable,
  validSiteKey,
  valueText,
  type Literal,
  type LogValuesArg,
  type Placeholders,
} from "../../../logging/ts/logsite.js";

describe("siteKey", () => {
  it("is the value Go's logsite.Key pins for one input", () => {
    // Arrange
    // printf 'daemon\0pkg.F\0hello {name}' | shasum -a 256, the Go test's pin.
    const [runtime, scope, template] = ["daemon", "pkg.F", "hello {name}"];

    // Act
    const key = siteKey(runtime, scope, template);

    // Assert
    expect(key).toBe("074892b7d933");
  });

  it.each([
    ["runtime", siteKey("store", "pkg.F", "t")],
    ["scope", siteKey("daemon", "pkg.G", "t")],
    ["template", siteKey("daemon", "pkg.F", "u")],
    ["a boundary moved between inputs", siteKey("daemon", "pkg.Ft", "")],
  ])("differs when the %s changes", (_input, key) => {
    // Act / Assert
    expect(key).not.toBe(siteKey("daemon", "pkg.F", "t"));
  });

  it.each([
    ["a key", "074892b7d933", true],
    ["too short", "074892b7d93", false],
    ["uppercase", "074892B7D933", false],
    ["not hex", "074892b7d93g", false],
  ])("validSiteKey reads %s", (_case, key, valid) => {
    // Act / Assert
    expect(validSiteKey(key)).toBe(valid);
  });
});

describe("sha256Hex", () => {
  it.each([
    ["empty", ""],
    ["one block", "abc"],
    ["the last length that pads in its own block", "x".repeat(55)],
    ["the first length that pads into a second block", "x".repeat(56)],
    ["a whole block", "x".repeat(64)],
    ["multi-byte UTF-8", "é☃".repeat(40)],
  ])("agrees with node's sha256 on %s", (_case, text) => {
    // Act / Assert
    expect(sha256Hex(text)).toBe(createHash("sha256").update(text).digest("hex"));
  });
});

describe("parseTemplate", () => {
  it("answers literal runs and placeholders in order", () => {
    // Act / Assert
    expect(parseTemplate("shim {shim} after {elapsed}ms")).toEqual([
      { literal: "shim " }, { name: "shim" }, { literal: " after " }, { name: "elapsed" }, { literal: "ms" },
    ]);
  });

  it("reads a doubled brace as a literal brace", () => {
    // Act / Assert
    expect(parseTemplate("a {{b}} c")).toEqual([{ literal: "a {b} c" }]);
  });

  it.each([
    ["an unclosed placeholder", "opens {never", "never closes"],
    ["an empty placeholder", "empty {} here", "not a lowercase identifier"],
    ["an uppercase name", "a {Name}", "not a lowercase identifier"],
    ["a name with a space", "a {two words}", "not a lowercase identifier"],
    ["a lone closing brace", "a } here", "lone closing brace"],
  ])("refuses %s", (_case, template, fault) => {
    // Act / Assert
    expect(() => parseTemplate(template)).toThrow(fault);
  });
});

describe("placeholders", () => {
  it.each([
    ["none", "plain text", []],
    ["two", "shim {shim} after {elapsed}", ["shim", "elapsed"]],
    ["a repeat once", "{a} and {a}", ["a"]],
    ["none inside literal braces", "{{x}} {y}", ["y"]],
  ])("answers %s", (_case, template, names) => {
    // Act / Assert
    expect(placeholders(template)).toEqual(names);
  });
});

describe("valueText", () => {
  it.each([
    ["a string as it is", "words", "words"],
    ["a number as String spells it", 1.5, "1.5"],
    ["a boolean", false, "false"],
    ["undefined", undefined, "undefined"],
    ["null", null, "null"],
    ["an error by its message", new Error("boom"), "boom"],
    ["an array as JSON", ["a", 1], '["a",1]'],
    ["an object as JSON", { a: 1 }, '{"a":1}'],
  ])("reads %s", (_case, value, text) => {
    // Act / Assert
    expect(valueText(value)).toBe(text);
  });
});

describe("SiteTable", () => {
  it("answers one site per template, derived once", () => {
    // Arrange
    const table = new SiteTable("webapp", "webapp.test");

    // Act
    const first = table.site("a {b}");
    const again = table.site("a {b}");

    // Assert
    expect(again).toBe(first);
    expect(first).toMatchObject({ key: siteKey("webapp", "webapp.test", "a {b}"), template: "a {b}", names: ["b"] });
  });

  it.each([["no dot", "webapp"], ["uppercase", "Webapp.test"], ["a space", "webapp.a b"]])("refuses a scope with %s", (_case, scope) => {
    // Act / Assert
    expect(() => new SiteTable("webapp", scope)).toThrow("dotted lowercase");
  });
});

describe("renderSite", () => {
  const table = new SiteTable("shim", "shim.test");

  it("fills each placeholder with its value's text", () => {
    // Act / Assert
    expect(renderSite(table.site("{n} rows in {place}"), { n: 2, place: "the tail" })).toBe("2 rows in the tail");
  });

  it("refuses a placeholder with no value", () => {
    // Act / Assert
    expect(() => renderSite(table.site("{n} rows"), {})).toThrow("names {n}, which the call's values do not hold");
  });

  it("refuses a value no placeholder names", () => {
    // Act / Assert
    expect(() => renderSite(table.site("rows"), { n: 1 })).toThrow("names no {n}");
  });

  it("renders a message outside a site with the same checks", () => {
    // Act / Assert
    expect(renderTemplate("{{literal}} {n}", { n: 1 })).toBe("{literal} 1");
  });
});

describe("the template types", () => {
  /** A log method's shape, as both runtimes declare theirs. */
  function method<T extends string>(template: Literal<T>, ...values: LogValuesArg<T>): string {
    return renderTemplate(template, values[0] ?? {});
  }

  it("reads the placeholder names out of a literal type", () => {
    // Arrange
    const names: Placeholders<"x {a} {{b}} {c}"> = "c";

    // Act / Assert
    // @ts-expect-error {{b}} is a literal brace, not a placeholder
    const literal: Placeholders<"x {a} {{b}} {c}"> = "b";
    expect([names, literal]).toEqual(["c", "b"]);
  });

  it("accepts a literal template with exactly its values", () => {
    // Act / Assert
    expect(method("a {x} b {y}", { x: 1, y: 2 })).toBe("a 1 b 2");
  });

  it("refuses a template that is not a literal", () => {
    // Arrange
    const built: string = "built";

    // Act / Assert: the directive fails the typecheck if the call compiles.
    // @ts-expect-error a template must be a string literal
    expect(method(built)).toBe("built");
  });

  it("refuses a value the template does not name", () => {
    // Act / Assert
    // @ts-expect-error the template names {x}, not {y}
    expect(() => method("a {x}", { y: 1 })).toThrow();
  });

  it("refuses an extra value beside the template's", () => {
    // Act / Assert
    // @ts-expect-error {y} is no placeholder of the template
    expect(() => method("a {x}", { x: 1, y: 2 })).toThrow("names no {y}");
  });

  it("refuses values for a template that names none", () => {
    // Act / Assert
    // @ts-expect-error the template names no placeholder
    expect(() => method("plain", { x: 1 })).toThrow("names no {x}");
  });
});
