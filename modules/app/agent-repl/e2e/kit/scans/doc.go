// Package scans holds the source scans of the e2e package: the checks that
// keep its scenario inventory whole (every step in exactly one scenario,
// every world built inside one, every scenario and step naming its seam), the
// scenario matrix (SCENARIO-MATRIX.md, checked against the sources it
// summarizes), and the lints that pin each of the harness's shared helpers as
// the one place its shape is spelled.
//
// They read the e2e package's files as data and build no world, so they live
// here, outside the e2e suite, which is never reused: this package is its own
// unit, keyed on the trees it reads (testrun/roster, the e2e-kit suite). Every
// test runs with the e2e package's directory as its working directory
// (TestMain), so a scan spells the files it reads as the e2e package does.
package scans
