// Package logsite derives a log statement's SITE: the key that names one log
// call across every record it writes and every commit that keeps it, and the
// template that doubles as its summary (logging-contract.md "Log sites").
//
// A SITE KEY is the first 12 hex characters of the sha256 of the record's
// runtime, the function the call is written in, and the call's template,
// joined by NUL bytes. Nothing dynamic takes part in it: a template is a
// string literal whose placeholders ({name}) are filled from the record's
// named values only when the message is rendered. So every record one call
// writes carries one key, whatever values it carries, and the key changes
// only when the template is reworded or its function is renamed or moved.
//
// Every Go runtime's logging helper derives its keys here, and every
// runtime's static scan test (Scan) derives them the same way from source,
// so the two can never disagree.
package logsite

import (
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"regexp"
	"runtime"
	"strings"
	"sync"
)

// KeyLength is how many hex characters of the sha256 a key keeps.
const KeyLength = 12

// Key is the site key of the log call in FUNCTION, written in template
// TEMPLATE, of runtime RUNTIME. FUNCTION is a normalized function name
// (NormalizeFunction).
func Key(runtime, function, template string) string {
	sum := sha256.Sum256([]byte(runtime + "\x00" + function + "\x00" + template))
	return hex.EncodeToString(sum[:])[:KeyLength]
}

// placeholderName is a placeholder's name: a lowercase identifier.
var placeholderName = regexp.MustCompile(`^[a-z_][a-z0-9_]*$`)

// Placeholders answers TEMPLATE's placeholder names, in order of first
// appearance, each once. A literal brace is written doubled ({{ or }}). A
// template whose braces do not parse is an error naming the fault: an
// unclosed or empty placeholder, a name that is not a lowercase identifier,
// or a lone closing brace.
func Placeholders(template string) ([]string, error) {
	var names []string
	seen := map[string]bool{}
	err := walk(template, func(literal string) {}, func(name string) {
		if !seen[name] {
			seen[name] = true
			names = append(names, name)
		}
	})
	return names, err
}

// Render answers TEMPLATE with each placeholder replaced by its value, as
// VALUE answers it for the placeholder's name. A value VALUE has none for is
// an error naming the placeholder, as is a template that does not parse.
func Render(template string, value func(name string) (string, bool)) (string, error) {
	var b strings.Builder
	var missing []string
	err := walk(template, func(literal string) { b.WriteString(literal) }, func(name string) {
		v, ok := value(name)
		if !ok {
			missing = append(missing, name)
			return
		}
		b.WriteString(v)
	})
	if err != nil {
		return "", err
	}
	if len(missing) > 0 {
		return "", fmt.Errorf("logsite: the template %q names {%s}, which the record's values do not hold", template, strings.Join(missing, "}, {"))
	}
	return b.String(), nil
}

// walk reads TEMPLATE, calling LITERAL with each run of literal text (a
// doubled brace already undoubled) and PLACEHOLDER with each placeholder's
// name, in order.
func walk(template string, literal func(string), placeholder func(string)) error {
	var run strings.Builder
	flush := func() {
		if run.Len() > 0 {
			literal(run.String())
			run.Reset()
		}
	}
	for i := 0; i < len(template); i++ {
		c := template[i]
		switch {
		case c == '{' && i+1 < len(template) && template[i+1] == '{':
			run.WriteByte('{')
			i++
		case c == '}' && i+1 < len(template) && template[i+1] == '}':
			run.WriteByte('}')
			i++
		case c == '{':
			end := strings.IndexByte(template[i+1:], '}')
			if end < 0 {
				return fmt.Errorf("logsite: the template %q opens a placeholder at byte %d that never closes (a literal brace is written {{)", template, i)
			}
			name := template[i+1 : i+1+end]
			if !placeholderName.MatchString(name) {
				return fmt.Errorf("logsite: the template %q names the placeholder {%s}, which is not a lowercase identifier (a literal brace is written {{)", template, name)
			}
			flush()
			placeholder(name)
			i += end + 1
		case c == '}':
			return fmt.Errorf("logsite: the template %q has a lone closing brace at byte %d (a literal brace is written }})", template, i)
		default:
			run.WriteByte(c)
		}
	}
	flush()
	return nil
}

// closureSegment is one segment of a function name the compiler adds for a
// closure or a wrapper inside the function a call is written in.
var closureSegment = regexp.MustCompile(`^(func\d+|\d+|deferwrap\d+|gowrap\d+)$`)

// NormalizeFunction answers the function a call written in FULL is keyed by
// (runtime.Frame.Function's spelling, or Scan's): the enclosing named
// function or method, with every closure and wrapper segment and every
// generic instantiation dropped, and a package variable's initializer named
// as the package's init. So renumbering closures, or instantiating a
// generic, never changes a key, and a call in a closure is keyed by the
// function it is written in.
func NormalizeFunction(full string) string {
	full = stripGenerics(full)
	// The package path may hold dots (a domain); the function's segments
	// start after its last slash.
	slash := strings.LastIndexByte(full, '/')
	pkgEnd := slash + 1 + strings.IndexByte(full[slash+1:], '.')
	if pkgEnd <= slash {
		return full
	}
	pkg, rest := full[:pkgEnd], full[pkgEnd+1:]
	if strings.HasPrefix(rest, "glob.") {
		return pkg + ".init"
	}
	segments := splitSegments(rest)
	kept := segments[:0]
	for i, s := range segments {
		if i > 0 && closureSegment.MatchString(s) {
			break
		}
		kept = append(kept, s)
	}
	return pkg + "." + strings.Join(kept, ".")
}

// splitSegments splits a function name's dotted segments, keeping a
// parenthesized receiver ("(*T)") whole.
func splitSegments(rest string) []string {
	var out []string
	depth, start := 0, 0
	for i := 0; i < len(rest); i++ {
		switch rest[i] {
		case '(':
			depth++
		case ')':
			depth--
		case '.':
			if depth == 0 {
				out = append(out, rest[start:i])
				start = i + 1
			}
		}
	}
	return append(out, rest[start:])
}

// stripGenerics drops every bracketed instantiation from a function name.
func stripGenerics(name string) string {
	if !strings.Contains(name, "[") {
		return name
	}
	var b strings.Builder
	depth := 0
	for _, r := range name {
		switch {
		case r == '[':
			depth++
		case r == ']':
			depth--
		case depth == 0:
			b.WriteRune(r)
		}
	}
	return b.String()
}

// Callers resolves the function a log call is written in from its program
// counter, once per call site: the frames above a logging helper's own
// package are walked until the first that is not the helper's, and the
// answer is cached by that frame's program counter.
type Callers struct {
	// helpers are the helper packages' import paths, each with its trailing
	// dot ("pkg."), whose frames are skipped.
	helpers []string
	cache   sync.Map // uintptr -> string
}

// NewCallers answers a resolver that skips the frames of every package in
// HELPERS (import paths), the logging helper's own and any it wraps.
func NewCallers(helpers ...string) *Callers {
	prefixes := make([]string, len(helpers))
	for i, h := range helpers {
		prefixes[i] = h + "."
	}
	return &Callers{helpers: prefixes}
}

// Function answers the normalized function of the first frame above the
// caller of Function that lies outside the helper packages. A stack with no
// such frame is a helper calling itself, which never happens: it panics.
func (c *Callers) Function() string {
	var pcs [32]uintptr
	n := runtime.Callers(2, pcs[:])
	frames := runtime.CallersFrames(pcs[:n])
	for {
		frame, more := frames.Next()
		if !c.isHelper(frame.Function) {
			if cached, ok := c.cache.Load(frame.PC); ok {
				return cached.(string)
			}
			fn := NormalizeFunction(frame.Function)
			c.cache.Store(frame.PC, fn)
			return fn
		}
		if !more {
			panic("logsite: every frame of the log call lies in the logging helper; no call site to key")
		}
	}
}

// isHelper reports whether FUNCTION lies in one of the helper packages.
func (c *Callers) isHelper(function string) bool {
	for _, p := range c.helpers {
		if strings.HasPrefix(function, p) {
			return true
		}
	}
	return false
}

// template is a log message's template as Msg takes it: a string constant,
// never a value built at run time. It is unexported so no other package can
// convert a run-time string to it; an untyped string constant converts
// implicitly. See Msg.
type template string

// Message is a log message a helper FORWARDS to a logger on its caller's
// behalf: its template, and the function the template is written in. A
// helper that takes the message its caller logs (a failure reporter, a
// shared note) takes a Message, so the record is keyed by the caller that
// wrote the template, never by the helper.
type Message struct {
	template string
	function string
}

// msgCallers resolves Msg's caller.
var msgCallers = NewCallers("agentrepl/logging/logsite")

// Msg answers the message written as TEMPLATE in the function that calls it.
//
// TEMPLATE MUST BE A STRING LITERAL (or a constant): its parameter's type is
// unexported, so a fmt.Sprintf result or a string variable does not compile.
// A template is the site's summary and part of its key, so a value built at
// run time would make every record its own site (logging-contract.md "Log
// sites"). Put each dynamic value in the record's context under a name, and
// name it in the template as {name}.
func Msg(t template) Message {
	return Message{template: string(t), function: msgCallers.Function()}
}

// Template is the message's template.
func (m Message) Template() string { return m.template }

// Function is the normalized function the message was written in.
func (m Message) Function() string { return m.function }

// IsZero reports whether the message was never made by Msg.
func (m Message) IsZero() bool { return m.function == "" }

// RenderFields renders TEMPLATE from FIELDS, a record's named values: each
// placeholder takes the field of its name, as Text spells it.
func RenderFields(template string, fields map[string]any) (string, error) {
	return Render(template, func(name string) (string, bool) {
		v, ok := fields[name]
		if !ok {
			return "", false
		}
		return Text(v), true
	})
}

// Text is how a named value reads inside a rendered message: a string as it
// is, an error by its Error, a Stringer by its String, and anything else as
// fmt.Sprint prints it.
func Text(v any) string {
	switch t := v.(type) {
	case string:
		return t
	case error:
		return t.Error()
	case fmt.Stringer:
		return t.String()
	}
	return fmt.Sprint(v)
}
