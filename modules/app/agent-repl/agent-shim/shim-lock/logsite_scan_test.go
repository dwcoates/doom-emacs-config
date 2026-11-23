package main

import (
	"testing"

	"agentrepl/logging/logsite/scan"
	"agentrepl/shim-lock/internal/logging"
)

// logCalls are the shim-lock logger's call shapes.
var logCalls = scan.Calls(3, 1, 2, "Info", "Error")

// TestEveryLogSiteIsSound scans the module's source for every log call and
// fails on each fault scan.Check finds: a template that does not parse, a
// placeholder its call's own context literal does not hold, two calls of one
// function writing one template, two sites sharing a key
// (logging-contract.md "Log sites").
func TestEveryLogSiteIsSound(t *testing.T) {
	// Arrange
	spec := scan.Spec{Runtime: logging.Runtime, Module: "agentrepl/shim-lock", Root: ".", Calls: logCalls}

	// Act and Assert
	scan.Require(t, spec)
}
