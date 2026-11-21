package main

import (
	"testing"

	"agentrepl/logging/logsite/scan"
	"agentrepl/zettel/internal/zlog"
)

// logCalls are recordlog.Logger's call shapes: a level method's template
// and context, and the Msg a forwarded message is made with.
var logCalls = scan.Calls(3, 1, 2, "Debug", "Info", "Warn", "Error")

// TestEveryLogSiteIsSound scans the module's source for every log call and
// fails on each fault scan.Check finds: a template that does not parse, a
// placeholder its call's own context literal does not hold, two calls of one
// function writing one template, two sites sharing a key
// (logging-contract.md "Log sites").
func TestEveryLogSiteIsSound(t *testing.T) {
	// Arrange
	spec := scan.Spec{Runtime: zlog.Runtime, Module: "agentrepl/zettel", Root: ".", Calls: logCalls}

	// Act and Assert
	scan.Require(t, spec)
}
