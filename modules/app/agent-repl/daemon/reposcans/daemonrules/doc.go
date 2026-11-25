// Package daemonrules holds every rule test of the daemon module: the tests
// that read the daemon's Go sources and hold them to a structural rule (one
// home per pattern, every X goes through Y, no Z outside its package), one
// file per daemon package they hold (<package path>_rules_test.go), plus the
// rules that span packages.
//
// They live here, in reposcans, rather than beside the packages they hold,
// because the rule suites run on every branch before its merge is admitted to
// the queue (testrun/roster's Rules suites, which `workspace async-merge` and
// `workspace finished` run in the maker's workspace): a rule test beside its
// package ran only in the merge gate, so no workspace ever saw it fail before
// landing. Each test runs in the directory of the package it holds
// (sourcescan.InPackage), so the paths it reads keep their meaning, and
// TestNoDaemonCodeOutsideReposcansReadsGoSource fails a source-reading test
// anywhere else in the daemon module.
package daemonrules
