// Package outtail is the one bounded tail of a process's output that testrun
// quotes: a failed unit's event, and a module build's compiler output.
package outtail

// Bytes bounds a tail.
const Bytes = 4000

// Of is the end of output, at most Bytes, cut at a line start where one falls
// inside the window.
func Of(output []byte) string {
	if len(output) <= Bytes {
		return string(output)
	}
	cut := output[len(output)-Bytes:]
	for i, b := range cut {
		if b == '\n' && i+1 < len(cut) {
			return string(cut[i+1:])
		}
	}
	return string(cut)
}
