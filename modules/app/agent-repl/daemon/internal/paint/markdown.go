package paint

import "strings"

// highlightMarkdown is markdown's own pass: markdown is line-oriented rather
// than token-oriented, so it does not go through the generic lexer.
func (p *painter) highlightMarkdown(code string) (Spans, error) {
	spans := Spans{}
	var err error
	add := func(text, class string) bool {
		spans, err = p.emit(spans, text, class)
		return err == nil
	}

	inFence := false
	for _, line := range splitKeepingNewlines(code) {
		body, newline := line, ""
		if strings.HasSuffix(body, "\n") {
			body, newline = body[:len(body)-1], "\n"
		}
		trimmed := strings.TrimLeft(body, " \t")
		indent := body[:len(body)-len(trimmed)]

		switch {
		case strings.HasPrefix(trimmed, "```") || strings.HasPrefix(trimmed, "~~~"):
			inFence = !inFence
			if !add(indent, "") || !add(trimmed, "punctuation") {
				return nil, err
			}
		case inFence:
			if !add(body, "") {
				return nil, err
			}
		case strings.HasPrefix(trimmed, "#"):
			if !add(indent, "") || !add(trimmed, "heading") {
				return nil, err
			}
		case strings.HasPrefix(trimmed, ">"):
			if !add(indent, "") || !add(trimmed, "comment") {
				return nil, err
			}
		default:
			if !add(indent, "") {
				return nil, err
			}
			marker, rest := listMarker(trimmed)
			if marker != "" && !add(marker, "punctuation") {
				return nil, err
			}
			next, e := p.inlineMarkdown(spans, rest)
			if e != nil {
				return nil, e
			}
			spans = next
		}
		if !add(newline, "") {
			return nil, err
		}
	}
	return spans, nil
}

// listMarker splits a leading list bullet off a line, so the bullet paints as
// punctuation and the item's text still gets inline treatment.
func listMarker(trimmed string) (marker, rest string) {
	for _, bullet := range []string{"- ", "* ", "+ "} {
		if strings.HasPrefix(trimmed, bullet) {
			return trimmed[:2], trimmed[2:]
		}
	}
	i := 0
	for i < len(trimmed) && isDigit(trimmed[i]) {
		i++
	}
	if i > 0 && i+1 < len(trimmed) && (trimmed[i] == '.' || trimmed[i] == ')') && trimmed[i+1] == ' ' {
		return trimmed[:i+2], trimmed[i+2:]
	}
	return "", trimmed
}

// inlineMarkdown paints one line's inline constructs: code spans, strong,
// emphasis and links.
func (p *painter) inlineMarkdown(spans Spans, line string) (Spans, error) {
	var err error
	add := func(text, class string) bool {
		spans, err = p.emit(spans, text, class)
		return err == nil
	}
	for i := 0; i < len(line); {
		switch {
		case line[i] == '`':
			if end := strings.IndexByte(line[i+1:], '`'); end >= 0 {
				if !add(line[i:i+1+end+1], "string") {
					return nil, err
				}
				i += 1 + end + 1
				continue
			}
		case strings.HasPrefix(line[i:], "**") || strings.HasPrefix(line[i:], "__"):
			marker := line[i : i+2]
			if end := strings.Index(line[i+2:], marker); end >= 0 {
				if !add(line[i:i+2+end+2], "strong") {
					return nil, err
				}
				i += 2 + end + 2
				continue
			}
		case line[i] == '*' || line[i] == '_':
			marker := line[i]
			if end := strings.IndexByte(line[i+1:], marker); end >= 0 {
				if !add(line[i:i+1+end+1], "emphasis") {
					return nil, err
				}
				i += 1 + end + 1
				continue
			}
		case line[i] == '[':
			if close := strings.IndexByte(line[i:], ']'); close > 0 {
				after := i + close + 1
				if after < len(line) && line[after] == '(' {
					if paren := strings.IndexByte(line[after:], ')'); paren >= 0 {
						if !add(line[i:after+paren+1], "link") {
							return nil, err
						}
						i = after + paren + 1
						continue
					}
				}
			}
		}
		if !add(line[i:i+1], "") {
			return nil, err
		}
		i++
	}
	return spans, nil
}

// splitKeepingNewlines splits into lines with each line's terminator kept, so
// concatenating the spans reproduces the input byte for byte.
func splitKeepingNewlines(code string) []string {
	var lines []string
	start := 0
	for i := 0; i < len(code); i++ {
		if code[i] == '\n' {
			lines = append(lines, code[start:i+1])
			start = i + 1
		}
	}
	if start < len(code) {
		lines = append(lines, code[start:])
	}
	return lines
}
