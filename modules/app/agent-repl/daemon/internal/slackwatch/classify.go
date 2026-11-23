package slackwatch

import (
	"regexp"
	"strings"
)

// urgentPatterns are what makes a thread's message URGENT: its digest is
// delivered after the short quiet period, as an ordinary prompt the running
// turn's classifier may interject. They are the gns-sockets plugin's
// RegexClassifier rules, tuned for precision over recall: a false urgent
// interrupts the agent for nothing, while a missed one only waits for the
// longer quiet period. Each is matched against the lower-cased, trimmed text.
var urgentPatterns = []*regexp.Regexp{
	// An imperative opening the message.
	regexp.MustCompile(`^(stop|halt|pause|abort|cancel|wait|hold[\s_-]?(on|up))\b`),
	// A pause or alarm emoji anywhere.
	regexp.MustCompile(`:(pause|stop|no_entry|warning|rotating_light|hand|raised_hand):`),
	// A direct address opening the message.
	regexp.MustCompile(`^@(me|agent|bot|claude)\b`),
}

// Urgent reports whether TEXT reads as urgent (urgentPatterns).
func Urgent(text string) bool {
	text = strings.ToLower(strings.TrimSpace(text))
	for _, p := range urgentPatterns {
		if p.MatchString(text) {
			return true
		}
	}
	return false
}
