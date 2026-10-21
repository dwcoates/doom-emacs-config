package pyedit

import (
	"fmt"
	"regexp"
	"strings"
)

// heredoc is one here-document a command line opens: the simple command it
// feeds and the body the shell hands that command on stdin.
type heredoc struct {
	command string
	body    string
}

// pendingHeredoc is a here-document whose operator has been read and whose
// body starts after the logical line ends.
type pendingHeredoc struct {
	command   string
	tag       string
	quoted    bool
	stripTabs bool
}

// pythonCommand is a simple command that runs python on its stdin and nothing
// else: optional environment assignments, a python interpreter, and an
// optional "-". Anything more (a script path, -c, arguments) and the
// here-document is not the whole program.
var pythonCommand = regexp.MustCompile(`^(?:[A-Za-z_][A-Za-z0-9_]*=\S*\s+)*(?:\S*/)?python(?:3(?:\.\d+)?)?(?:\s+-)?$`)

// mentionsPython is any word naming a python interpreter.
var mentionsPython = regexp.MustCompile(`(?:^|[\s/;&|(])python(?:3(?:\.\d+)?)?(?:\s|$|;|&|\|)`)

// pythonScripts finds the python programs a command line runs from
// here-documents, in order. It answers an error when the line runs python in
// any way it cannot read whole — a here-document it cannot delimit, a python
// invocation that is not a bare here-document's — because the edits it states
// would then be incomplete.
func pythonScripts(line string) ([]string, error) {
	docs, shellText, err := heredocs(line)
	if err != nil {
		return nil, err
	}
	var scripts []string
	pythonDocs := 0
	for _, doc := range docs {
		if pythonCommand.MatchString(doc.command) {
			scripts = append(scripts, doc.body)
			pythonDocs++
		}
	}
	// Every python word outside the bodies must be one of those heredocs'.
	if n := len(mentionsPython.FindAllString(shellText, -1)); n != pythonDocs {
		return nil, fmt.Errorf("the command runs python %d time(s) but only %d as a bare here-document", n, pythonDocs)
	}
	return scripts, nil
}

// heredocs reads a command line's here-documents, and the shell text outside
// their bodies.
//
// The shell text is lexed for quotes, backslashes and comments so that a
// "<<" inside a string or a comment opens nothing. A body runs from the line
// after the logical line that opened it to its delimiter line. An unquoted
// delimiter subjects the body to expansion, so a body that would expand
// anything ($, a backquote, a backslash) is refused rather than guessed at.
func heredocs(line string) ([]heredoc, string, error) {
	lines := strings.Split(line, "\n")
	var docs []heredoc
	var shell strings.Builder
	var pending []pendingHeredoc
	inSingle, inDouble := false, false
	segStart := 0
	for i := 0; i < len(lines); i++ {
		text := lines[i]
		shell.WriteString(text)
		shell.WriteByte('\n')
		continued := false
		comment := false
		segStart = 0
		for j := 0; j < len(text); j++ {
			c := text[j]
			switch {
			case inSingle:
				if c == '\'' {
					inSingle = false
				}
			case inDouble:
				if c == '\\' {
					j++
				} else if c == '"' {
					inDouble = false
				}
			case c == '\\':
				if j == len(text)-1 {
					continued = true
				}
				j++
			case c == '\'':
				if j > 0 && text[j-1] == '$' {
					return nil, "", fmt.Errorf("ANSI-C quoting at line %d", i+1)
				}
				inSingle = true
			case c == '"':
				inDouble = true
			case c == '#' && (j == 0 || strings.ContainsRune(" \t;&|()", rune(text[j-1]))):
				comment = true
			case c == ';' || c == '|' || c == '&' || c == '(' || c == ')' || c == '{' || c == '}':
				segStart = j + 1
			case c == '<' && strings.HasPrefix(text[j:], "<<<"):
				// A here-string: its word is on this line, and no body follows.
				j += 2
			case c == '<' && strings.HasPrefix(text[j:], "<<"):
				doc, next, err := heredocOperator(text, j)
				if err != nil {
					return nil, "", fmt.Errorf("line %d: %w", i+1, err)
				}
				doc.command = strings.TrimSpace(text[segStart:j])
				pending = append(pending, doc)
				j = next - 1
			}
			if comment {
				break
			}
		}
		if continued || inSingle || inDouble {
			if len(pending) > 0 && !continued {
				return nil, "", fmt.Errorf("a quote spans the line that opens a here-document (line %d)", i+1)
			}
			continue
		}
		for _, doc := range pending {
			body, end, err := heredocBody(lines, i+1, doc)
			if err != nil {
				return nil, "", err
			}
			docs = append(docs, heredoc{command: doc.command, body: body})
			i = end
		}
		pending = nil
	}
	if inSingle || inDouble {
		return nil, "", fmt.Errorf("an unterminated quote")
	}
	if len(pending) > 0 {
		return nil, "", fmt.Errorf("a here-document opened on the last line")
	}
	return docs, shell.String(), nil
}

// heredocOperator reads the "<<" operator at text[at:] and its delimiter
// word, answering the pending document and the index just past the word.
func heredocOperator(text string, at int) (pendingHeredoc, int, error) {
	j := at + 2
	doc := pendingHeredoc{}
	if j < len(text) && text[j] == '-' {
		doc.stripTabs = true
		j++
	}
	for j < len(text) && (text[j] == ' ' || text[j] == '\t') {
		j++
	}
	var tag strings.Builder
	for j < len(text) {
		c := text[j]
		switch {
		case c == '\'' || c == '"':
			end := strings.IndexByte(text[j+1:], c)
			if end < 0 {
				return doc, 0, fmt.Errorf("an unterminated here-document delimiter")
			}
			tag.WriteString(text[j+1 : j+1+end])
			doc.quoted = true
			j += end + 2
		case c == '\\' && j+1 < len(text):
			tag.WriteByte(text[j+1])
			doc.quoted = true
			j += 2
		case c == '_' || c == '-' || c == '.' || isAlnum(c):
			tag.WriteByte(c)
			j++
		default:
			goto done
		}
	}
done:
	if tag.Len() == 0 {
		return doc, 0, fmt.Errorf("a here-document with no delimiter")
	}
	doc.tag = tag.String()
	return doc, j, nil
}

// heredocBody collects a document's body from lines[from:], answering it and
// the index of its delimiter line.
func heredocBody(lines []string, from int, doc pendingHeredoc) (string, int, error) {
	var body strings.Builder
	for k := from; k < len(lines); k++ {
		line := lines[k]
		if doc.stripTabs {
			line = strings.TrimLeft(line, "\t")
		}
		if line == doc.tag {
			text := body.String()
			if !doc.quoted && strings.ContainsAny(text, "$`\\") {
				return "", 0, fmt.Errorf("the unquoted here-document %q would expand its body", doc.tag)
			}
			return text, k, nil
		}
		body.WriteString(line)
		body.WriteByte('\n')
	}
	return "", 0, fmt.Errorf("the here-document %q is never closed", doc.tag)
}

func isAlnum(c byte) bool {
	return c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9'
}
