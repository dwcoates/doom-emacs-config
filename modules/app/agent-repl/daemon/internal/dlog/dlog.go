// Package dlog holds the daemon's logging combinators: small wrappers
// over the printf-style logf closure the whole codebase already
// threads, so instrumentation can be added liberally without a
// structural migration.
//
// Two conventions these helpers encode:
//
//   - Context rides in a trailing " {k=v ...}" block, mirroring the
//     Emacs module's doom-agent-repl.log format, so both logs grep the
//     same way. Tag/TagFunc build loggers that append the block to
//     every line.
//
//   - External model/subprocess calls (the classifier, the summarizer,
//     the remediation analyst, the shim itself) log both edges: Call
//     writes the start line and returns a completion func that writes
//     duration, clamped output, and error. A call that logs only its
//     failure is unattributable; a call that logs nothing is invisible.
package dlog

import (
	"fmt"
	"strings"
	"time"
)

// Logf is the printf-style logging closure threaded through every
// daemon component (ultimately log.Printf, teed to stderr + disk).
type Logf func(format string, args ...any)

// OutputClampLen bounds the response/output text a Call completion
// writes, so one giant model response cannot balloon the disk log.
const OutputClampLen = 2000

// Clamp truncates s to at most n bytes, marking the cut.
func Clamp(s string, n int) string {
	if len(s) <= n {
		return s
	}
	return s[:n] + " …[clamped]"
}

// kvBlock renders pairs to "k=v k2=v2". An odd trailing key gets the
// explicit marker value so a mis-paired call site is visible in the
// log rather than silently shifting every following pair.
func kvBlock(kv []any) string {
	var b strings.Builder
	for i := 0; i < len(kv); i += 2 {
		if b.Len() > 0 {
			b.WriteByte(' ')
		}
		if i+1 < len(kv) {
			fmt.Fprintf(&b, "%v=%v", kv[i], kv[i+1])
		} else {
			fmt.Fprintf(&b, "%v=!MISSING", kv[i])
		}
	}
	return b.String()
}

// Tag returns a Logf that appends " {k=v ...}" to every line, so a
// component logs its identity (session, cwd) once at construction
// instead of hand-threading it into every format string.
func Tag(logf Logf, kv ...any) Logf {
	block := kvBlock(kv)
	if block == "" {
		return logf
	}
	return func(format string, args ...any) {
		logf("%s {%s}", fmt.Sprintf(format, args...), block)
	}
}

// TagFunc is Tag with the pairs resolved at call time, for context
// that is not known at construction (a cwd learned from the init
// event). TAGS must be safe to call from any goroutine that logs.
func TagFunc(logf Logf, tags func() []any) Logf {
	return func(format string, args ...any) {
		block := kvBlock(tags())
		if block == "" {
			logf(format, args...)
			return
		}
		logf("%s {%s}", fmt.Sprintf(format, args...), block)
	}
}

// Call logs the start of an external model/subprocess call and returns
// the completion func. NAME is the call's short identity ("classifier",
// "summarizer"); KV carries the differentiating source info (session,
// cwd, model, action). The completion func logs duration plus the
// clamped output on success, or the error (with any output tail) on
// failure.
func Call(logf Logf, name string, kv ...any) func(output string, err error) {
	block := kvBlock(kv)
	suffix := ""
	if block != "" {
		suffix = " {" + block + "}"
	}
	logf("%s: call start%s", name, suffix)
	start := time.Now()
	return func(output string, err error) {
		elapsed := time.Since(start).Round(time.Millisecond)
		if err != nil {
			logf("%s: call FAILED after %s%s: %v (output: %s)",
				name, elapsed, suffix, err, Clamp(output, OutputClampLen))
			return
		}
		logf("%s: call ok in %s%s: %s", name, elapsed, suffix, Clamp(output, OutputClampLen))
	}
}
