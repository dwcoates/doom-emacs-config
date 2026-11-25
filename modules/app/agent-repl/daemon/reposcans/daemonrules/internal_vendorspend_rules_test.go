package daemonrules

import (
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalVendorspendDir is the package these rules hold, under the daemon module's root.
const internalVendorspendDir = "internal/vendorspend"

func TestTheCachePricesHaveOneDefinition(t *testing.T) {
	sourcescan.InPackage(t, internalVendorspendDir)
	// Act
	sites := sourcescan.Sites(t, internalVendorspendDir, "PriceTenths  = ")
	// Assert: the run cap reads them through cachelapse's alias, which
	// names this package's constants, never a figure of its own.
	if len(sites) != 1 || sites[0] != "vendorspend.go" {
		t.Fatalf("the cache prices are defined in %v, want vendorspend.go alone", sites)
	}
}
