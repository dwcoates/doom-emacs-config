// Package ratelimittest builds the conversation.v1.SessionRateLimitStatus a
// shim relays for the vendor's rate-limit event, for the tests of the views
// that take it (the footer, the roster) and the ladder that decides its
// block.
//
// A rate-limit verdict is about ONE allowance window, and the views file it
// under that window alone (ladder.RateLimitRejections, the footer's
// allowances). So every fixture names its window through these builders
// rather than spelling the oneof by hand, and a test file that hand-rolls a
// window is refused (TestNoTestHandRollsARateLimitWindow).
package ratelimittest

import (
	conversationv1 "agentrepl/proto/conversation/v1"
)

// FiveHour is the rolling five-hour (session) window.
func FiveHour() *conversationv1.SessionRateLimitType {
	return &conversationv1.SessionRateLimitType{Window: &conversationv1.SessionRateLimitType_FiveHour{
		FiveHour: &conversationv1.SessionRateLimitWindowFiveHour{}}}
}

// SevenDay is the account-wide seven-day (weekly) window.
func SevenDay() *conversationv1.SessionRateLimitType {
	return &conversationv1.SessionRateLimitType{Window: &conversationv1.SessionRateLimitType_SevenDay{
		SevenDay: &conversationv1.SessionRateLimitWindowSevenDay{}}}
}

// SevenDayOpus is the seven-day window for the Opus model family.
func SevenDayOpus() *conversationv1.SessionRateLimitType {
	return &conversationv1.SessionRateLimitType{Window: &conversationv1.SessionRateLimitType_SevenDayOpus{
		SevenDayOpus: &conversationv1.SessionRateLimitWindowSevenDayOpus{}}}
}

// SevenDaySonnet is the seven-day window for the Sonnet model family.
func SevenDaySonnet() *conversationv1.SessionRateLimitType {
	return &conversationv1.SessionRateLimitType{Window: &conversationv1.SessionRateLimitType_SevenDaySonnet{
		SevenDaySonnet: &conversationv1.SessionRateLimitWindowSevenDaySonnet{}}}
}

// SevenDayOverageIncluded is the seven-day window with overage included.
func SevenDayOverageIncluded() *conversationv1.SessionRateLimitType {
	return &conversationv1.SessionRateLimitType{Window: &conversationv1.SessionRateLimitType_SevenDayOverageIncluded{
		SevenDayOverageIncluded: &conversationv1.SessionRateLimitWindowSevenDayOverageIncluded{}}}
}

// Overage is the vendor's overage window.
func Overage() *conversationv1.SessionRateLimitType {
	return &conversationv1.SessionRateLimitType{Window: &conversationv1.SessionRateLimitType_Overage{
		Overage: &conversationv1.SessionRateLimitWindowOverage{}}}
}

// Allowed is an allowed verdict on WINDOW (nil for a windowless event).
func Allowed(window *conversationv1.SessionRateLimitType) *conversationv1.SessionRateLimitStatus {
	return &conversationv1.SessionRateLimitStatus{RateLimitType: window,
		Status: &conversationv1.SessionRateLimitStatus_Allowed{Allowed: &conversationv1.SessionRateLimitAllowed{}}}
}

// AllowedWarning is an allowed-with-warning verdict on WINDOW.
func AllowedWarning(window *conversationv1.SessionRateLimitType) *conversationv1.SessionRateLimitStatus {
	return &conversationv1.SessionRateLimitStatus{RateLimitType: window,
		Status: &conversationv1.SessionRateLimitStatus_AllowedWarning{AllowedWarning: &conversationv1.SessionRateLimitAllowedWarning{}}}
}

// Rejected is a rejected verdict on WINDOW.
func Rejected(window *conversationv1.SessionRateLimitType) *conversationv1.SessionRateLimitStatus {
	return &conversationv1.SessionRateLimitStatus{RateLimitType: window,
		Status: &conversationv1.SessionRateLimitStatus_Rejected{Rejected: &conversationv1.SessionRateLimitRejected{}}}
}

// Statusless is an event about WINDOW that carries no verdict.
func Statusless(window *conversationv1.SessionRateLimitType) *conversationv1.SessionRateLimitStatus {
	return &conversationv1.SessionRateLimitStatus{RateLimitType: window}
}

// Update wraps STATUS as the session update the shim pushes.
func Update(status *conversationv1.SessionRateLimitStatus) *conversationv1.SessionUpdate {
	return &conversationv1.SessionUpdate{Update: &conversationv1.SessionUpdate_RateLimitStatus{RateLimitStatus: status}}
}
