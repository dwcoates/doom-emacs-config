// Package prompts reads the prompts/ directory: the briefs the daemon splices
// and sends as prompts.
//
// Briefs are read AT USE TIME, never cached across a use, so editing one takes
// effect without a daemon bounce. A missing or malformed brief is LOUD: the
// caller refuses rather than sending an unspliced or partial prompt. See
// ARCHITECTURE.md "merge (internal/merge)" and the RequestCommandSupport
// ruling (`prompts/add-support-slash-command.md`).
package prompts

import (
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
)

// Suffix is the extension every brief carries. Load takes a name without it.
const Suffix = ".md"

// HeaderUsedBy and HeaderPlaceholders are the header keys prompts/README.md
// pins: the call site and the declared placeholder list.
const (
	HeaderUsedBy       = "used by"
	HeaderPlaceholders = "placeholders"
)

// HeaderNone is the placeholders value a brief with no placeholders declares.
// It is a declaration, not an absence.
const HeaderNone = "none"

// placeholderPattern matches one `{{name}}` placeholder. Names are the
// lowercase snake_case the briefs use; anything else is not a placeholder this
// package will splice.
var placeholderPattern = regexp.MustCompile(`\{\{([a-z0-9_]+)\}\}`)

// anyBracePattern matches any `{{...}}` token, so a misspelled or malformed
// one is caught rather than shipped to an agent as prose.
var anyBracePattern = regexp.MustCompile(`\{\{[^{}]*\}\}`)

// Prompt is one loaded brief: its parsed header and its body.
type Prompt struct {
	// Name is the brief's file name without the .md suffix.
	Name string
	// Header is the brief's front-matter keys, as parsed.
	Header map[string]string
	// Body is the brief's text, placeholders unspliced.
	Body string
	// Placeholders are the placeholder names the body uses, in first-use
	// order. Splice refuses a values map that does not cover them exactly.
	Placeholders []string
}

// Load reads one brief by name from dir. A brief that is absent, unreadable,
// or whose header does not parse is an error the caller surfaces; nothing is
// defaulted.
func Load(dir, name string) (Prompt, error) {
	if name == "" {
		return Prompt{}, fmt.Errorf("prompts: no brief name given")
	}
	path := Path(dir, name)
	raw, err := os.ReadFile(path)
	if err != nil {
		return Prompt{}, fmt.Errorf("prompts: reading %s: %w", path, err)
	}
	return Parse(name, path, string(raw))
}

// Path is where the brief NAME lives in dir.
func Path(dir, name string) string {
	return filepath.Join(dir, name+Suffix)
}

// Parse reads one brief's whole file CONTENT, exactly as Load reads it off
// disk; WHERE names the content in every error. It is Load's parse alone, so
// content that is not on disk yet — a rewrite about to replace a brief — is
// held to the same rules as a brief that is.
func Parse(name, where, content string) (Prompt, error) {
	if strings.TrimSpace(content) == "" {
		return Prompt{}, fmt.Errorf("prompts: %s is empty", where)
	}
	headerLine, body, found := strings.Cut(content, "\n")
	if !found {
		return Prompt{}, fmt.Errorf("prompts: %s has a header and no body", where)
	}
	header, declared, err := parseHeader(headerLine)
	if err != nil {
		return Prompt{}, fmt.Errorf("prompts: %s: %w", where, err)
	}
	// A file's final newline is the editor's line terminator and is dropped;
	// everything before it is the prompt verbatim.
	body = strings.TrimSuffix(body, "\n")
	used, err := bodyPlaceholders(body)
	if err != nil {
		return Prompt{}, fmt.Errorf("prompts: %s: %w", where, err)
	}
	if err := sameSet("placeholders", declared, used); err != nil {
		return Prompt{}, fmt.Errorf("prompts: %s: %w", where, err)
	}
	return Prompt{Name: name, Header: header, Body: body, Placeholders: used}, nil
}

// parseHeader reads the first line: `<!-- used by: <site>; placeholders: {{a}}, {{b}} -->`.
// It answers the header keys and the DECLARED placeholder names.
func parseHeader(line string) (map[string]string, []string, error) {
	trimmed := strings.TrimSpace(strings.TrimSuffix(strings.TrimSpace(line), "\r"))
	if !strings.HasPrefix(trimmed, "<!--") || !strings.HasSuffix(trimmed, "-->") {
		return nil, nil, fmt.Errorf("the first line is not the header comment")
	}
	inner := strings.TrimSpace(trimmed[len("<!--") : len(trimmed)-len("-->")])
	if inner == "" {
		return nil, nil, fmt.Errorf("the header comment is empty")
	}
	header := map[string]string{}
	for _, field := range strings.Split(inner, ";") {
		key, value, found := strings.Cut(field, ":")
		if !found {
			return nil, nil, fmt.Errorf("header field %q is not `key: value`", strings.TrimSpace(field))
		}
		key = strings.TrimSpace(key)
		if key == "" {
			return nil, nil, fmt.Errorf("a header field has an empty key")
		}
		if _, seen := header[key]; seen {
			return nil, nil, fmt.Errorf("header key %q appears twice", key)
		}
		header[key] = strings.TrimSpace(value)
	}
	if header[HeaderUsedBy] == "" {
		return nil, nil, fmt.Errorf("the header declares no %q call site", HeaderUsedBy)
	}
	declaredValue, ok := header[HeaderPlaceholders]
	if !ok {
		return nil, nil, fmt.Errorf("the header declares no %q list", HeaderPlaceholders)
	}
	if declaredValue == "" {
		return nil, nil, fmt.Errorf("the %q list is empty; %q is how a brief declares it has none", HeaderPlaceholders, HeaderNone)
	}
	if declaredValue == HeaderNone {
		return header, nil, nil
	}
	declared, err := extractPlaceholders(declaredValue)
	if err != nil {
		return nil, nil, fmt.Errorf("the %q list: %w", HeaderPlaceholders, err)
	}
	if len(declared) == 0 {
		return nil, nil, fmt.Errorf("the %q list %q names no placeholder", HeaderPlaceholders, declaredValue)
	}
	return header, declared, nil
}

// bodyPlaceholders answers the placeholder names the body uses, in first-use
// order.
func bodyPlaceholders(body string) ([]string, error) {
	return extractPlaceholders(body)
}

// extractPlaceholders answers every placeholder in text, in first-use order. A
// `{{...}}` token that is not a well-formed placeholder name is an error: a
// misspelled one would otherwise ship to an agent as prose.
func extractPlaceholders(text string) ([]string, error) {
	for _, token := range anyBracePattern.FindAllString(text, -1) {
		if !placeholderPattern.MatchString(token) {
			return nil, fmt.Errorf("%q is not a well-formed placeholder", token)
		}
	}
	var names []string
	seen := map[string]bool{}
	for _, m := range placeholderPattern.FindAllStringSubmatch(text, -1) {
		if seen[m[1]] {
			continue
		}
		seen[m[1]] = true
		names = append(names, m[1])
	}
	return names, nil
}

// sameSet reports whether two name lists cover each other exactly.
func sameSet(what string, declared, used []string) error {
	have := map[string]bool{}
	for _, name := range used {
		have[name] = true
	}
	var undeclared, unused []string
	for _, name := range used {
		if !contains(declared, name) {
			undeclared = append(undeclared, name)
		}
	}
	for _, name := range declared {
		if !have[name] {
			unused = append(unused, name)
		}
	}
	if len(undeclared) == 0 && len(unused) == 0 {
		return nil
	}
	sort.Strings(undeclared)
	sort.Strings(unused)
	return fmt.Errorf("the header's %s diverge from the body: undeclared %v, declared but unused %v", what, undeclared, unused)
}

func contains(haystack []string, needle string) bool {
	for _, s := range haystack {
		if s == needle {
			return true
		}
	}
	return false
}

// Splice substitutes values into the body's placeholders. A value for an
// unknown placeholder, or a placeholder with no value, is an error: a brief is
// never sent with a hole in it.
func (p Prompt) Splice(values map[string]string) (string, error) {
	var missing, unknown []string
	for _, name := range p.Placeholders {
		if _, ok := values[name]; !ok {
			missing = append(missing, name)
		}
	}
	for name := range values {
		if !contains(p.Placeholders, name) {
			unknown = append(unknown, name)
		}
	}
	if len(missing) > 0 || len(unknown) > 0 {
		sort.Strings(missing)
		sort.Strings(unknown)
		return "", fmt.Errorf("prompts: %s: values do not cover the placeholders: missing %v, unknown %v", p.Name, missing, unknown)
	}
	out := placeholderPattern.ReplaceAllStringFunc(p.Body, func(token string) string {
		return values[placeholderPattern.FindStringSubmatch(token)[1]]
	})
	if leftover := anyBracePattern.FindString(out); leftover != "" {
		return "", fmt.Errorf("prompts: %s: %q survived the splice", p.Name, leftover)
	}
	return out, nil
}
