package dlog

import (
	"fmt"

	"agentrepl/logging/logsite"
)

// A record's SITE (logging-contract.md "Log sites"): the key of the log call
// that wrote it, logsite.Key over the runtime, the function the call is
// written in and its template, and the template rendered into its message
// from the record's named values.

// mustRender answers TEMPLATE rendered from the record's named values
// FIELDS. A template naming a value the record does not hold, or one whose
// braces do not parse, is a defect of the call that wrote it, which the
// static scan test (logsite_scan_test.go) exists to catch: it panics.
func mustRender(operation, template string, fields Context) string {
	text, err := logsite.RenderFields(template, fields)
	if err != nil {
		panic(fmt.Sprintf("dlog: the record of operation %s cannot be rendered: %v", operation, err))
	}
	return text
}

// forwarded answers the function MESSAGE was written in. A Message no Msg
// made names no call: it panics.
func forwarded(message logsite.Message) string {
	if message.IsZero() {
		panic("dlog: a forwarded message must be made by logsite.Msg")
	}
	return message.Function()
}
