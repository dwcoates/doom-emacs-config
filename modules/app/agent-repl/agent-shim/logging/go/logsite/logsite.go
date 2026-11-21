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

// ValidKey reports whether KEY has a site key's form: KeyLength lowercase hex
// characters.
func ValidKey(key string) bool {
	if len(key) != KeyLength {
		return false
	}
	for i := 0; i < len(key); i++ {
		c := key[i]
		if !(c >= '0' && c <= '9' || c >= 'a' && c <= 'f') {
			return false
		}
	}
	return true
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

// callerCache maps a log call's program counter to the normalized function
// it is written in, so each call site walks its frames once.
var callerCache sync.Map // uintptr -> string

// Caller answers the normalized function that called the logging helper
// entry point calling Caller: the function a log call is written in. Every
// entry point (a logger's Debug, Info, Warn and Error, and Msg) calls it
// directly, so the call site is always exactly one frame above that entry
// point, whatever package it lies in: a log call written in the helper's own
// package (its own records, its tests) is keyed by its own function. A
// wrapper that forwards to another logger resolves its caller at its own
// entry point and forwards the function with the template, so the wrapped
// logger never resolves it again.
//
// The answer is cached by the call site's program counter.
func Caller() string {
	var pcs [8]uintptr
	// Skip runtime.Callers and Caller; frames then start at the entry point.
	n := runtime.Callers(2, pcs[:])
	frames := runtime.CallersFrames(pcs[:n])
	if _, more := frames.Next(); !more {
		panic("logsite: Caller was called with no entry point above it")
	}
	site, _ := frames.Next()
	if site.Function == "" || strings.HasPrefix(site.Function, "runtime.") {
		// A log entry point is never a goroutine's root function: Caller was
		// called from something that is not an entry point.
		panic(fmt.Sprintf("logsite: Caller's entry point was called from %q, which is no log call", site.Function))
	}
	if cached, ok := callerCache.Load(site.PC); ok {
		return cached.(string)
	}
	fn := NormalizeFunction(site.Function)
	callerCache.Store(site.PC, fn)
	return fn
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

// Msg answers the message written as TEMPLATE in the function that calls it.
//
// TEMPLATE MUST BE A STRING LITERAL (or a constant): its parameter's type is
// unexported, so a fmt.Sprintf result or a string variable does not compile.
// A template is the site's summary and part of its key, so a value built at
// run time would make every record its own site (logging-contract.md "Log
// sites"). Put each dynamic value in the record's context under a name, and
// name it in the template as {name}.
func Msg(t template) Message {
	return Message{template: string(t), function: Caller()}
}

// Template is the message's template.
func (m Message) Template() string { return m.template }

// Function is the normalized function the message was written in.
func (m Message) Function() string { return m.function }

// IsZero reports whether the message was never made by Msg.
func (m Message) IsZero() bool { return m.function == "" }

// Forwarded answers the function the message was written in, for a logger
// recording it on its caller's behalf. A Message no Msg made names no call,
// which is a defect of the helper that forwarded it: it panics.
func (m Message) Forwarded() string {
	if m.IsZero() {
		panic("logsite: a forwarded message must be made by logsite.Msg")
	}
	return m.function
}

// MustRender is a logger's record of OPERATION rendered: TEMPLATE filled
// from FIELDS, the record's named values. A template naming a value the
// record does not hold, or one whose braces do not parse, is a defect of the
// call that wrote it, which every runtime's static scan test exists to
// catch: it panics, naming the operation and the fault.
func MustRender(operation, template string, fields map[string]any) string {
	text, err := RenderFields(template, fields)
	if err != nil {
		panic(fmt.Sprintf("logsite: the record of operation %s cannot be rendered: %v", operation, err))
	}
	return text
}

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
