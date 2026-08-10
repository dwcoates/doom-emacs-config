//go:build race

package ssm

// raceDetector reports whether this binary was built with -race. The timing
// test at fleet scale asserts a SQL cost, and the race detector multiplies
// every cgo SQLite call by an order of magnitude, so the number it would
// measure is about the instrumentation rather than about the query plan.
const raceDetector = true
