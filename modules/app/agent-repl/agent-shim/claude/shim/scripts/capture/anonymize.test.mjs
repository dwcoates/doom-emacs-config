/**
 * Unit tests for the corpus anonymizer.
 *
 * The rules under test are `testdata/corpus/MANIFEST.md`'s, one test per rule
 * and one test per edge case, so a failure names the rule that broke.
 */
import { readFileSync } from "node:fs";
import { describe, expect, it } from "vitest";

import {
  BLOB_PREFIX_CHARS,
  MAX_STRING_CHARS,
  REDACTED,
  TRUNCATE_TO_CHARS,
  anonymize,
  anonymizeJsonl,
  anonymizePlainText,
  anonymizeString,
  isBlobKey,
  isSecretKey,
  redactSecretRuns,
  truncateBlob,
  truncateLong,
  NO_PERSONAL_VALUES,
  expandHome,
  pathSlug,
  scrubPersonal,
  scrubbedEmail,
} from "./anonymize.mjs";

describe("isSecretKey", () => {
  const cases = [
    { name: "authorization", key: "authorization", want: true },
    { name: "api-key with a hyphen", key: "api-key", want: true },
    { name: "api_key with an underscore", key: "api_key", want: true },
    { name: "apiKey in camel case", key: "apiKey", want: true },
    { name: "Authorization capitalized", key: "Authorization", want: true },
    { name: "password", key: "password", want: true },
    { name: "secret", key: "secret", want: true },
    { name: "clientSecret", key: "clientSecret", want: true },
    { name: "a structural session id", key: "sessionId", want: false },
    { name: "a structural uuid", key: "uuid", want: false },
    { name: "a structural tool use id", key: "tool_use_id", want: false },
    { name: "a path", key: "cwd", want: false },
  ];
  for (const { name, key, want } of cases) {
    it(`${want ? "matches" : "does not match"} ${name}`, () => {
      expect(isSecretKey(key)).toBe(want);
    });
  }
});

describe("isBlobKey", () => {
  it("matches a thinking block's signature", () => {
    expect(isBlobKey("signature")).toBe(true);
  });

  it("matches a base64 image payload", () => {
    expect(isBlobKey("data")).toBe(true);
  });

  it("does not match an ordinary field", () => {
    expect(isBlobKey("text")).toBe(false);
  });
});

describe("redactSecretRuns", () => {
  const cases = [
    { name: "an Anthropic key", input: "sk-ant-api03-AAAABBBBCCCCDDDDEEEE1234" },
    { name: "a GitHub PAT", input: "ghp_AAAABBBBCCCCDDDDEEEEFFFF11112222" },
    { name: "a fine-grained GitHub PAT", input: "github_pat_AAAABBBBCCCCDDDDEEEEFF" },
    { name: "an AWS access key id", input: "AKIAIOSFODNN7EXAMPLE" },
    { name: "an AWS session key id", input: "ASIAIOSFODNN7EXAMPLE" },
    { name: "a Slack bot token", input: "xoxb-1234567890-abcdefghijkl" },
    { name: "a Bearer header value", input: "Bearer abcdefghijklmnopqrstuvwxyz" },
    { name: "a JWT", input: "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIn0.SflKxwRJSMeKKF2QT4fwpMeJf36POk6yJV_adQssw5c" },
  ];
  for (const { name, input } of cases) {
    it(`redacts ${name}`, () => {
      expect(redactSecretRuns(`prefix ${input} suffix`)).toBe(`prefix ${REDACTED} suffix`);
    });
  }

  it("leaves a JWT-shaped string with too short a header alone (the length floor keeps ordinary base64 prose out)", () => {
    const notAJwt = "eyJhbGci.eyJzdWIiOjEy.SflKxwRJSMeKKF2QT4";
    expect(redactSecretRuns(notAJwt)).toBe(notAJwt);
  });

  it("leaves a uuid alone", () => {
    const uuid = "f2c3c473-defc-4d8c-91de-d27177b2568e";
    expect(redactSecretRuns(uuid)).toBe(uuid);
  });

  it("leaves a vendor tool_use id alone", () => {
    const id = "toolu_01HmeTAn4c5saajyR8e8M7fu";
    expect(redactSecretRuns(id)).toBe(id);
  });

  it("leaves an absolute path alone", () => {
    const p = "/Users/x/.config/doom/modules/app/agent-repl";
    expect(redactSecretRuns(p)).toBe(p);
  });

  it("redacts every occurrence, not just the first", () => {
    const key = "AKIAIOSFODNN7EXAMPLE";
    expect(redactSecretRuns(`${key} and ${key}`)).toBe(`${REDACTED} and ${REDACTED}`);
  });

  it("is stateless across calls despite the module-level /g patterns", () => {
    const key = "AKIAIOSFODNN7EXAMPLE";
    expect(redactSecretRuns(key)).toBe(REDACTED);
    expect(redactSecretRuns(key)).toBe(REDACTED);
  });
});

describe("truncateBlob", () => {
  it("leaves a short blob untouched", () => {
    expect(truncateBlob("abc")).toBe("abc");
  });

  it("truncates a long blob to the prefix plus a count", () => {
    const blob = "z".repeat(BLOB_PREFIX_CHARS + 10);
    expect(truncateBlob(blob)).toBe(`${"z".repeat(BLOB_PREFIX_CHARS)}…[TRUNCATED 10 chars]`);
  });
});

describe("truncateLong", () => {
  it("leaves a string at the limit untouched", () => {
    const text = "a".repeat(MAX_STRING_CHARS);
    expect(truncateLong(text)).toBe(text);
  });

  it("truncates a string past the limit, reporting the dropped count", () => {
    const text = "a".repeat(MAX_STRING_CHARS + 1);
    const dropped = MAX_STRING_CHARS + 1 - TRUNCATE_TO_CHARS;
    expect(truncateLong(text)).toBe(
      `${"a".repeat(TRUNCATE_TO_CHARS)}…[TRUNCATED ${dropped} chars for corpus]`,
    );
  });
});

describe("anonymizeString rule ordering", () => {
  it("redacts a secret KEY before any truncation could leak a prefix", () => {
    expect(anonymizeString("x".repeat(2000), "authorization")).toBe(REDACTED);
  });

  it("redacts a secret VALUE before the blob rule truncates it", () => {
    expect(anonymizeString("AKIAIOSFODNN7EXAMPLE", "signature")).toBe(REDACTED);
  });

  it("truncates a blob shorter than the general length limit", () => {
    const blob = "s".repeat(200);
    expect(anonymizeString(blob, "signature")).toBe(
      `${"s".repeat(BLOB_PREFIX_CHARS)}…[TRUNCATED ${200 - BLOB_PREFIX_CHARS} chars]`,
    );
  });

  it("applies the length rule to an ordinary long string", () => {
    const text = "t".repeat(1000);
    expect(anonymizeString(text, "text")).toContain("[TRUNCATED 600 chars for corpus]");
  });

  it("applies the value rules to a string with no key at all", () => {
    expect(anonymizeString("AKIAIOSFODNN7EXAMPLE", undefined)).toBe(REDACTED);
  });
});

describe("anonymize", () => {
  it("preserves object keys verbatim", () => {
    const out = anonymize({ toolUseResult: { backgroundTaskId: "b1f2" } });
    expect(Object.keys(out)).toEqual(["toolUseResult"]);
    expect(Object.keys(out.toolUseResult)).toEqual(["backgroundTaskId"]);
  });

  it("preserves structural values verbatim", () => {
    const line = {
      uuid: "0b24530b-c53c-47ea-9b70-c3c1a1b7bc6a",
      sessionId: "f2c3c473-defc-4d8c-91de-d27177b2568e",
      timestamp: "2026-07-20T21:21:09.847Z",
      cwd: "/Users/x/.config/doom",
      tool_use_id: "toolu_01HmeTAn4c5saajyR8e8M7fu",
    };
    expect(anonymize(line)).toEqual(line);
  });

  it("preserves numbers, booleans and null", () => {
    const value = { n: 42, f: 1.5, t: true, z: null };
    expect(anonymize(value)).toEqual(value);
  });

  it("preserves array order", () => {
    expect(anonymize(["a", "b", "c"])).toEqual(["a", "b", "c"]);
  });

  it("redacts a nested credential", () => {
    const out = anonymize({ headers: { authorization: "Bearer abcdefghijklmnopqrst" } });
    expect(out.headers.authorization).toBe(REDACTED);
  });

  it("truncates a nested thinking signature", () => {
    const out = anonymize({ content: [{ type: "thinking", signature: "q".repeat(500) }] });
    expect(out.content[0].signature).toContain("[TRUNCATED");
    expect(out.content[0].type).toBe("thinking");
  });

  it("does not mutate its input", () => {
    const input = { secret: "hunter2" };
    anonymize(input);
    expect(input.secret).toBe("hunter2");
  });

  it("applies the key rule to every element of an array under that key", () => {
    const out = anonymize({ password: ["one", "two"] });
    expect(out.password).toEqual([REDACTED, REDACTED]);
  });
});

describe("anonymizeJsonl", () => {
  it("anonymizes each line independently and keeps line count", () => {
    const text = '{"a":"AKIAIOSFODNN7EXAMPLE"}\n{"b":"ok"}\n';
    const out = anonymizeJsonl(text);
    expect(out.split("\n")).toHaveLength(3);
    expect(JSON.parse(out.split("\n")[0]).a).toBe(REDACTED);
    expect(JSON.parse(out.split("\n")[1]).b).toBe("ok");
  });

  it("reports an unparsed line instead of dropping it", () => {
    const seen = [];
    const out = anonymizeJsonl("{not json\n", (lineNo) => seen.push(lineNo));
    expect(seen).toEqual([1]);
    expect(out.startsWith("{not json")).toBe(true);
  });

  it("still redacts credentials inside an unparsed line", () => {
    const out = anonymizeJsonl("{broken AKIAIOSFODNN7EXAMPLE\n");
    expect(out).toContain(REDACTED);
  });
});

describe("anonymizePlainText", () => {
  it("redacts a credential in a spool", () => {
    expect(anonymizePlainText("export TOKEN=ghp_AAAABBBBCCCCDDDDEEEEFFFF11112222\n")).toContain(
      REDACTED,
    );
  });

  it("does NOT truncate a long spool", () => {
    const spool = `${"tick\n".repeat(5000)}EXIT=0\n`;
    expect(anonymizePlainText(spool)).toBe(spool);
  });
});

describe("scrubPersonal", () => {
  const personal = {
    home: "/Users/annexample",
    emails: ["Ann.Example@mail.test", "ann@work.test"],
    names: ["ann", "annexample", "Example"],
  };
  const cases = [
    { name: "the home directory becomes the home token", text: "/Users/annexample/.config/x", want: "${HOME}/.config/x" },
    { name: "the home's slug becomes the token's slug", text: "-Users-annexample--config-x", want: "--HOME---config-x" },
    { name: "an email becomes a numbered example address", text: "to ann@work.test.", want: `to ${scrubbedEmail(1)}.` },
    { name: "an email matches case-insensitively", text: "ann.example@MAIL.test", want: scrubbedEmail(0) },
    { name: "a capitalized name keeps its capital", text: "Ann's notes", want: "Someone's notes" },
    { name: "a lowercase name stays lowercase", text: "github.com/annexample", want: "github.com/someone" },
    { name: "a name inside a longer word is left alone", text: "annual", want: "annual" },
    { name: "text with no personal value is unchanged", text: "plain text", want: "plain text" },
  ];
  for (const tc of cases) {
    it(tc.name, () => {
      expect(scrubPersonal(tc.text, personal)).toBe(tc.want);
    });
  }

  it("scrubs nothing with no personal values", () => {
    expect(scrubPersonal("/Users/annexample Ann", NO_PERSONAL_VALUES)).toBe("/Users/annexample Ann");
  });

  it("keeps a recorded path and its project directory in agreement", () => {
    const cwd = "/Users/annexample/proj";
    expect(pathSlug(scrubPersonal(cwd, personal))).toBe(scrubPersonal(pathSlug(cwd), personal));
  });
});

describe("expandHome", () => {
  // The vectors are shared with the sidecar's recorded.Expand, so the two
  // expansions cannot drift apart.
  const shared = JSON.parse(readFileSync(new URL("./home-token-vectors.json", import.meta.url), "utf8"));
  for (const tc of shared.vectors) {
    it(tc.name, () => {
      expect(expandHome(tc.text, shared.home)).toBe(tc.want);
    });
  }

  it("undoes the home rule of scrubPersonal", () => {
    const personal = { home: "/Users/bo", emails: [], names: [] };
    const text = '{"cwd":"/Users/bo/p","dir":"-Users-bo-p"}';
    expect(expandHome(scrubPersonal(text, personal), "/Users/bo")).toBe(text);
  });
});

describe("the anonymizer applies scrubPersonal", () => {
  const personal = { home: "/Users/ann", emails: [], names: ["ann"] };

  it("to string values and to object keys", () => {
    expect(anonymize({ "/Users/ann/p": { who: "Ann" } }, undefined, personal)).toEqual({
      "${HOME}/p": { who: "Someone" },
    });
  });

  it("to every JSONL line", () => {
    expect(anonymizeJsonl('{"cwd":"/Users/ann"}\n', () => {}, personal)).toBe('{"cwd":"${HOME}"}\n');
  });

  it("to plain text", () => {
    expect(anonymizePlainText("cd /Users/ann && echo Ann", personal)).toBe("cd ${HOME} && echo Someone");
  });

  it("before truncation, so the truncated prefix names nobody", () => {
    const text = `/Users/ann/${"x".repeat(MAX_STRING_CHARS)}`;
    expect(anonymizeString(text, "content", personal).startsWith("${HOME}/x")).toBe(true);
  });
});
