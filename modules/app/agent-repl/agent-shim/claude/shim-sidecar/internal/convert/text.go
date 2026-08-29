package convert

// text.go — flattening the vendor's result content into the plain text a few
// settled arms carry as one string (a search's rendered lines, a subagent's
// report). Everything richer keeps its block structure; this is only for the
// arms the schema declares AS a string.

import "strings"

// flattenResultText renders a tool result's content as the text it carried.
func flattenResultText(raw any) string {
	switch value := raw.(type) {
	case string:
		return value
	case []any:
		var parts []string
		for _, el := range value {
			block := obj(el)
			if block == nil {
				continue
			}
			if str(block["type"]) == "text" {
				parts = append(parts, str(block["text"]))
			}
		}
		return strings.Join(parts, "\n")
	default:
		return ""
	}
}

// nonEmptyLines splits rendered output into its lines, dropping blanks. Used
// where the vendor returns a list AS text (a filenames-mode search, a glob).
func nonEmptyLines(text string) []string {
	var out []string
	for _, line := range strings.Split(text, "\n") {
		line = strings.TrimSpace(line)
		if line != "" {
			out = append(out, line)
		}
	}
	return out
}

// countMatches reads a count-mode search's rendered total. The vendor renders
// per-file counts; their sum is the total the count arm states.
func countMatches(text string) int {
	total := 0
	for _, line := range nonEmptyLines(text) {
		idx := strings.LastIndex(line, ":")
		if idx < 0 {
			total += atoiSafe(line)
			continue
		}
		total += atoiSafe(strings.TrimSpace(line[idx+1:]))
	}
	return total
}

// atoiSafe reads a non-negative decimal, answering 0 for anything else. A
// malformed count is not an error the schema can express — the arm holds a
// number — so it is read as none rather than fabricated.
func atoiSafe(s string) int {
	if s == "" {
		return 0
	}
	n := 0
	for i := 0; i < len(s); i++ {
		if s[i] < '0' || s[i] > '9' {
			return 0
		}
		n = n*10 + int(s[i]-'0')
	}
	return n
}
