import { readFileSync } from "node:fs";
import { resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { describe, expect, it } from "vitest";

import {
  NOTICE_SUBJECT_BY_CLASS,
  noticeSubjectFromWords,
  noticeSubjectOf,
  synthesizedNotice,
  type NoticeSubject,
} from "../../src/convert/notice-subject.js";

/** One case of the fixture both planes read (testdata/vendor-notices/subjects.json). */
interface SharedCase {
  readonly name: string;
  readonly class: string | null;
  readonly text: string;
  readonly subject: string;
}

const FIXTURE = resolve(fileURLToPath(new URL(".", import.meta.url)), "../../../../../testdata/vendor-notices/subjects.json");
const SHARED = (JSON.parse(readFileSync(FIXTURE, "utf8")) as { cases: SharedCase[] }).cases;

/** The fixture's snake_case subject as this plane's protobuf case name. */
function caseName(subject: string): NoticeSubject {
  return subject.replace(/_([a-z])/g, (_m, c: string) => c.toUpperCase()) as NoticeSubject;
}

describe("the reading the stream and the transcript share", () => {
  it.each(SHARED.map((c) => [c.name, c] as const))("reads %s as the fixture says", (_name, c) => {
    expect(noticeSubjectOf(c.class ?? undefined, c.text)).toBe(caseName(c.subject));
  });
});

describe("the declared class decides", () => {
  it.each(Object.entries(NOTICE_SUBJECT_BY_CLASS))("classes %s as %s whatever the words", (cls, subject) => {
    expect(noticeSubjectOf(cls, "API Error: 529 overloaded")).toBe(subject);
  });

  it("reads an unknown class by its words", () => {
    expect(noticeSubjectOf("unknown", "API Error: 529 overloaded")).toBe("vendorTransient");
  });

  it("reads a class it does not know by its words", () => {
    expect(noticeSubjectOf("a_class_from_a_newer_vendor", "Login expired")).toBe("account");
  });
});

describe("the words, where no class decided", () => {
  it.each([
    ["an account", "Failed to authenticate", "account"],
    ["an account before an allowance", "OAuth: usage limit", "account"],
    ["an exhausted allowance", "You've hit your monthly spend limit", "usageLimit"],
    ["an approaching allowance", "API Error: approaching your limit", "usageWarning"],
    ["a changed window", "Your allowance resets at 5pm", "usageTransition"],
    ["a transient status", "API Error: 503 service unavailable", "vendorTransient"],
    ["a network code alone", "EAI_AGAIN", "vendorTransient"],
    ["nothing recognizable", "Claude's response was interrupted", "unclassified"],
  ])("reads %s", (_name, text, subject) => {
    expect(noticeSubjectFromWords(text)).toBe(subject);
  });
});

describe("the notice message", () => {
  it.each(["account", "usageLimit", "usageWarning", "usageTransition", "vendorTransient", "unclassified"] as const)(
    "sets the %s arm and no status",
    (subject) => {
      const notice = synthesizedNotice(subject);
      expect([notice.subject.case, notice.httpStatus]).toEqual([subject, undefined]);
    },
  );
});
