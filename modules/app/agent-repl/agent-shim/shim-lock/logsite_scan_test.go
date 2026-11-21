package main

import (
	"testing"

	"agentrepl/logging/logsite/scan"
	"agentrepl/shim-lock/internal/logging"
)

// logCalls are the shim-lock logger's call shapes.
var logCalls = []scan.Call{
	{Selector: "Info", Args: 3, Template: 1, Context: 2},
	{Selector: "Error", Args: 3, Template: 1, Context: 2},
	scan.MsgCall,
}

// TestEveryLogSiteIsSound scans the module's source for every log call and
// fails on each fault scan.Check finds: a template that does not parse, a
// placeholder its call's own context literal does not hold, two calls of one
// function writing one template, two sites sharing a key
// (logging-contract.md "Log sites").
func TestEveryLogSiteIsSound(t *testing.T) {
	// Arrange
	spec := scan.Spec{Runtime: logging.Runtime, Module: "agentrepl/shim-lock", Root: ".", Calls: logCalls}

	// Act
	sites, err := scan.Scan(spec)

	// Assert
	if err != nil {
		t.Fatalf("scan: %v", err)
	}
	if len(sites) == 0 {
		t.Fatal("the scan found no log call; it read no source")
	}
	for _, fault := range scan.Check(sites) {
		t.Error(fault)
	}
}
