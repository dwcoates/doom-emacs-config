package slackwatch

import "regexp"

// secretPattern is one shape of secret a post may never carry, and what it
// reads as.
type secretPattern struct {
	kind string
	re   *regexp.Regexp
}

// secretPatterns are the gns-sockets plugin's outbound scrub, unchanged: a
// post matching any is refused and never sent.
var secretPatterns = []secretPattern{
	{"AWS access key ID", regexp.MustCompile(`AKIA[0-9A-Z]{16}`)},
	{"GitHub personal access token", regexp.MustCompile(`gh[psoru]_[A-Za-z0-9]{30,}`)},
	{"Slack token", regexp.MustCompile(`xox[abprs]-[A-Za-z0-9-]{10,}`)},
	{"Bearer token in Authorization header", regexp.MustCompile(`(?i)Bearer\s+[A-Za-z0-9._\-]{20,}`)},
	{"JWT-shaped token", regexp.MustCompile(`eyJ[A-Za-z0-9_\-]{20,}\.eyJ[A-Za-z0-9_\-]{20,}\.[A-Za-z0-9_\-]{10,}`)},
	{"PEM-encoded private key", regexp.MustCompile(`-----BEGIN [A-Z ]+PRIVATE KEY-----`)},
	{".env-style secret assignment", regexp.MustCompile(`(?m)\b[A-Z0-9_]*(SECRET|TOKEN|API_KEY|PRIVATE_KEY|PASSWORD)[A-Z0-9_]*\s*=\s*[^\s'"#]{16,}`)},
}

// Secrets answers what each secret TEXT carries reads as, one per match, in
// pattern order; none for a clean text. The matched text is never answered.
func Secrets(text string) []string {
	var kinds []string
	for _, p := range secretPatterns {
		for range p.re.FindAllStringIndex(text, -1) {
			kinds = append(kinds, p.kind)
		}
	}
	return kinds
}
