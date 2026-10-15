import { readFileSync } from "node:fs";
import { relative } from "node:path";
import { create } from "@bufbuild/protobuf";
import { describe, expect, it } from "vitest";
import { FailureKindSchema } from "../../proto/gen/ts/frontend/v1/failure_pb";
import { filesUnder } from "./files-under.js";
import { recordingFailures } from "./failure-sink.js";

const undecodable = () => create(FailureKindSchema, { kind: { case: "frameUndecodable", value: { cause: "c", frameHead: "h" } } });

describe("recordingFailures", () => {
  it("keeps every kind filed, in order", () => {
    // Arrange
    const sink = recordingFailures();
    const first = undecodable();
    const second = undecodable();

    // Act
    sink.report(first);
    sink.report(second);

    // Assert
    expect(sink.reports).toEqual([first, second]);
  });

  it("keeps the arm of every kind filed", () => {
    // Arrange
    const sink = recordingFailures();

    // Act
    sink.report(undecodable());

    // Assert
    expect(sink.cases).toEqual(["frameUndecodable"]);
  });

  it("names a kind with no arm unset", () => {
    // Arrange
    const sink = recordingFailures();

    // Act
    sink.report(create(FailureKindSchema, {}));

    // Assert
    expect(sink.cases).toEqual(["unset"]);
  });

  it("forgets nothing on a retraction", () => {
    // Arrange
    const sink = recordingFailures();
    sink.report(undecodable());

    // Act
    sink.retract("frameUndecodable");

    // Assert
    expect(sink.cases).toEqual(["frameUndecodable"]);
  });
});

describe("the tests' recording failure sinks", () => {
  it("are all recordingFailures rather than a hand-rolled push", () => {
    // Arrange
    const root = new URL(".", import.meta.url).pathname;
    const handRolled = /report: \(kind\) => (?:void )?\w+\.push\(/;

    // Act
    const offenders = filesUnder(root)
      .filter((path) => handRolled.test(readFileSync(path, "utf8")))
      .map((path) => relative(root, path));

    // Assert
    expect(offenders).toEqual([]);
  });
});
