package convert

import (
	"regexp"

	conversationv1 "agentrepl/proto/conversation/v1"
)

// WHAT A VENDOR NOTICE IS ABOUT (conversation.v1.AgentResponseSynthesizedNotice.
// subject), read from the vendor's declared error class first and from its
// words only where the transcript record carries no class the table names.
//
// ONE READING ON BOTH PLANES. The shim gives the stream's copy of the same
// notice the same reading (shim/src/convert/notice-subject.ts), and a shared
// fixture (testdata/vendor-notices/subjects.json) pins that the two agree, so
// a notice the stream drew and the transcript restates is one subject either
// way.

// noticeSubject names a subject arm, as the shared fixture spells it.
type noticeSubject string

const (
	subjectAccount         noticeSubject = "account"
	subjectUsageLimit      noticeSubject = "usage_limit"
	subjectUsageWarning    noticeSubject = "usage_warning"
	subjectUsageTransition noticeSubject = "usage_transition"
	subjectVendorTransient noticeSubject = "vendor_transient"
	subjectUnclassified    noticeSubject = "unclassified"
)

// noticeSubjectByClass is the subject each declared vendor error class
// (SDKAssistantMessageError in @anthropic-ai/claude-agent-sdk) names.
// `unknown` is absent on purpose: the vendor declares it when it did not
// classify the failure, so its notice is read by its words. A request the
// vendor refused is unclassified: nothing later answers it.
var noticeSubjectByClass = map[string]noticeSubject{
	"authentication_failed":  subjectAccount,
	"oauth_org_not_allowed":  subjectAccount,
	"account_on_hold":        subjectAccount,
	"verification_required":  subjectAccount,
	"cloud_credential_error": subjectAccount,
	"rate_limit":             subjectUsageLimit,
	"billing_error":          subjectUsageLimit,
	"server_error":           subjectVendorTransient,
	"overloaded":             subjectVendorTransient,
	"invalid_request":        subjectUnclassified,
	"model_not_found":        subjectUnclassified,
	"max_output_tokens":      subjectUnclassified,
}

// The vendor's words for each subject, read only where no class decided.
var (
	accountWords         = []string{"failed to authenticate", "authentication_error", "invalid authentication", "login expired", "/login", "oauth"}
	usageLimitWords      = []string{"limit reached", "spend limit", "usage limit", "hit your"}
	usageWarningWords    = []string{"approaching"}
	usageTransitionWords = []string{"switched to", "resets at"}
	// transientWords are trouble waiting cures that states no status: an
	// overloaded API, a timeout, a dropped connection, a socket error code.
	transientWords = []string{
		"overloaded", "timed out", "timeout", "connection error", "connection closed", "connection dropped",
		"connection refused", "connection reset", "unable to connect", "can't reach", "cannot reach",
		"could not reach", "socket hang up", "fetch failed",
		"econnreset", "econnrefused", "etimedout", "enotfound", "eai_again", "enetunreach", "ehostunreach", "enetdown",
	}
)

// transientStatus reads a status a vendor notice states that waiting cures: a
// timeout (408), a rate limit (429) or a server-side failure (5xx).
var transientStatus = regexp.MustCompile(`(?i)\bAPI Error:?\s*(408|429|5\d\d)\b`)

// saysAny reports whether TEXT holds any of WORDS, ignoring case.
func saysAny(text string, words []string) bool {
	for _, word := range words {
		if containsFold(text, word) {
			return true
		}
	}
	return false
}

// noticeSubjectFromWords is the subject the notice's words name.
func noticeSubjectFromWords(text string) noticeSubject {
	switch {
	case saysAny(text, accountWords):
		return subjectAccount
	case saysAny(text, usageLimitWords):
		return subjectUsageLimit
	case saysAny(text, usageWarningWords):
		return subjectUsageWarning
	case saysAny(text, usageTransitionWords):
		return subjectUsageTransition
	case transientStatus.MatchString(text), saysAny(text, transientWords):
		return subjectVendorTransient
	}
	return subjectUnclassified
}

// noticeSubjectOf is the subject of a notice whose record declared CLASS
// (empty when it declared none) and whose words are TEXT.
func noticeSubjectOf(class, text string) noticeSubject {
	if subject, named := noticeSubjectByClass[class]; named {
		return subject
	}
	return noticeSubjectFromWords(text)
}

// noticeOfSubject is the notice message whose subject is SUBJECT. The vendor
// attaches no HTTP status to an assistant record, so none is set.
func noticeOfSubject(subject noticeSubject) *conversationv1.AgentResponseSynthesizedNotice {
	notice := &conversationv1.AgentResponseSynthesizedNotice{}
	switch subject {
	case subjectAccount:
		notice.Subject = &conversationv1.AgentResponseSynthesizedNotice_Account{Account: &conversationv1.AgentNoticeAccount{}}
	case subjectUsageLimit:
		notice.Subject = &conversationv1.AgentResponseSynthesizedNotice_UsageLimit{UsageLimit: &conversationv1.AgentNoticeUsageLimit{}}
	case subjectUsageWarning:
		notice.Subject = &conversationv1.AgentResponseSynthesizedNotice_UsageWarning{UsageWarning: &conversationv1.AgentNoticeUsageWarning{}}
	case subjectUsageTransition:
		notice.Subject = &conversationv1.AgentResponseSynthesizedNotice_UsageTransition{UsageTransition: &conversationv1.AgentNoticeUsageTransition{}}
	case subjectVendorTransient:
		notice.Subject = &conversationv1.AgentResponseSynthesizedNotice_VendorTransient{VendorTransient: &conversationv1.AgentNoticeVendorTransient{}}
	case subjectUnclassified:
		notice.Subject = &conversationv1.AgentResponseSynthesizedNotice_Unclassified{Unclassified: &conversationv1.AgentNoticeUnclassified{}}
	default:
		panic("convert: a notice subject outside the table: " + string(subject))
	}
	return notice
}
