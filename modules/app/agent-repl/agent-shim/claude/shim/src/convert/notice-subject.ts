/**
 * What a vendor-synthesized notice is ABOUT (`conversation.v1.
 * AgentResponseSynthesizedNotice.subject`), read from the vendor's declared
 * error class first and from its words only where the class says nothing.
 *
 * ONE READING ON BOTH PLANES. The sidecar gives the transcript's copy of the
 * same notice the same reading (shim-sidecar/internal/convert/
 * notice_subject.go), and a shared fixture (testdata/vendor-notices/
 * subjects.json) pins that the two agree, so a notice the stream drew and the
 * transcript restates is the same subject either way.
 */

import { create } from "@bufbuild/protobuf";

import { networkCodeIn } from "../engine/network-resume.js";
import { conversationv1 } from "../proto.js";

/** The subject arms a notice can take, by their protobuf case names. */
export type NoticeSubject = NonNullable<conversationv1.AgentResponseSynthesizedNotice["subject"]["case"]>;

/**
 * The subject each declared vendor error class (`SDKAssistantMessageError` in
 * `@anthropic-ai/claude-agent-sdk`) names. `unknown` is absent on purpose: the
 * vendor declares it when it did not classify the failure, so its notice is
 * read by its words. A request the vendor refused (`invalid_request`,
 * `model_not_found`, `max_output_tokens`) is `unclassified`: nothing later
 * answers it.
 */
export const NOTICE_SUBJECT_BY_CLASS: Readonly<Record<string, NoticeSubject>> = {
  authentication_failed: "account",
  oauth_org_not_allowed: "account",
  account_on_hold: "account",
  verification_required: "account",
  cloud_credential_error: "account",
  rate_limit: "usageLimit",
  billing_error: "usageLimit",
  server_error: "vendorTransient",
  overloaded: "vendorTransient",
  invalid_request: "unclassified",
  model_not_found: "unclassified",
  max_output_tokens: "unclassified",
};

/** The vendor's words for an account it refused, when no class was declared. */
const ACCOUNT_WORDS = [
  "failed to authenticate",
  "authentication_error",
  "invalid authentication",
  "login expired",
  "/login",
  "oauth",
];

/** The vendor's words for an exhausted allowance. */
const USAGE_LIMIT_WORDS = ["limit reached", "spend limit", "usage limit", "hit your"];

/** The vendor's words for an allowance being approached. */
const USAGE_WARNING_WORDS = ["approaching"];

/** The vendor's words for an allowance window or tier that changed. */
const USAGE_TRANSITION_WORDS = ["switched to", "resets at"];

/** A status a vendor notice states that waiting cures: a timeout, a rate limit or a 5xx. */
const TRANSIENT_STATUS = /\bAPI Error:?\s*(408|429|5\d\d)\b/i;

/** The vendor's words for trouble waiting cures that state no status. */
const TRANSIENT_WORDS = [
  "overloaded",
  "timed out",
  "timeout",
  "connection error",
  "connection closed",
  "connection dropped",
  "connection refused",
  "connection reset",
  "unable to connect",
  "can't reach",
  "cannot reach",
  "could not reach",
  "socket hang up",
  "fetch failed",
];

/** Whether TEXT holds any of WORDS, ignoring case. */
function saysAny(text: string, words: readonly string[]): boolean {
  const folded = text.toLowerCase();
  return words.some((word) => folded.includes(word));
}

/** The subject the notice's WORDS name, read only when no class decided. */
export function noticeSubjectFromWords(text: string): NoticeSubject {
  if (saysAny(text, ACCOUNT_WORDS)) return "account";
  if (saysAny(text, USAGE_LIMIT_WORDS)) return "usageLimit";
  if (saysAny(text, USAGE_WARNING_WORDS)) return "usageWarning";
  if (saysAny(text, USAGE_TRANSITION_WORDS)) return "usageTransition";
  if (TRANSIENT_STATUS.test(text) || saysAny(text, TRANSIENT_WORDS) || networkCodeIn(text) !== undefined) {
    return "vendorTransient";
  }
  return "unclassified";
}

/** The subject of a notice whose vendor declared ERRORCLASS (or none) and said TEXT. */
export function noticeSubjectOf(errorClass: string | undefined, text: string): NoticeSubject {
  return (errorClass === undefined ? undefined : NOTICE_SUBJECT_BY_CLASS[errorClass]) ?? noticeSubjectFromWords(text);
}

/** The notice message whose subject is SUBJECT. */
export function synthesizedNotice(subject: NoticeSubject): conversationv1.AgentResponseSynthesizedNotice {
  const arm = ((): conversationv1.AgentResponseSynthesizedNotice["subject"] => {
    switch (subject) {
      case "account":
        return { case: "account", value: create(conversationv1.AgentNoticeAccountSchema, {}) };
      case "usageLimit":
        return { case: "usageLimit", value: create(conversationv1.AgentNoticeUsageLimitSchema, {}) };
      case "usageWarning":
        return { case: "usageWarning", value: create(conversationv1.AgentNoticeUsageWarningSchema, {}) };
      case "usageTransition":
        return { case: "usageTransition", value: create(conversationv1.AgentNoticeUsageTransitionSchema, {}) };
      case "vendorTransient":
        return { case: "vendorTransient", value: create(conversationv1.AgentNoticeVendorTransientSchema, {}) };
      case "unclassified":
        return { case: "unclassified", value: create(conversationv1.AgentNoticeUnclassifiedSchema, {}) };
    }
  })();
  // The vendor attaches no HTTP status to an assistant record (the status
  // rides the RESULT, `api_error_status`), so it stays unset here.
  return create(conversationv1.AgentResponseSynthesizedNoticeSchema, { subject: arm });
}
