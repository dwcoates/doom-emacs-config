//go:build !race

package ssm

// raceDetector reports whether this binary was built with -race. See the
// build-tagged twin of this file.
const raceDetector = false
