package envc_test

import (
	"errors"
	"strings"
	"testing"

	"claude-repld/internal/envc"
)

func TestVendorGuardCheckPermitted(t *testing.T) {
	// Arrange.
	t.Setenv(envc.EnvForbidVendorCalls, "")
	guard := envc.NewVendorGuard(envc.Load())

	// Act.
	err := guard.Check("classifier")

	// Assert.
	if err != nil {
		t.Fatalf("Check() = %v, want nil", err)
	}
}

func TestVendorGuardCheckForbidden(t *testing.T) {
	// Arrange.
	t.Setenv(envc.EnvForbidVendorCalls, "1")
	guard := envc.NewVendorGuard(envc.Load())

	// Act.
	err := guard.Check("classifier")

	// Assert.
	var forbidden *envc.ForbiddenError
	if !errors.As(err, &forbidden) {
		t.Fatalf("Check() = %v, want *envc.ForbiddenError", err)
	}
}

func TestVendorGuardForbiddenErrorNamesSite(t *testing.T) {
	// Arrange.
	t.Setenv(envc.EnvForbidVendorCalls, "1")
	guard := envc.NewVendorGuard(envc.Load())

	// Act.
	err := guard.Check("merge-brief")

	// Assert.
	if !strings.Contains(err.Error(), "merge-brief") {
		t.Fatalf("Error() = %q, want it to name the site", err.Error())
	}
}
