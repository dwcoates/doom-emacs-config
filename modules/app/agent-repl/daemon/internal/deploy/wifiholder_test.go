package deploy

import (
	"context"
	"errors"
	"fmt"
	"path/filepath"
	"strings"
	"testing"
)

// wifiHeld makes the harness's persistent-wifi service loaded from its
// definition, and its socket one the declared holder "wifi-holder" holds.
func wifiHeld(t *testing.T, h *restarterHarness) {
	t.Helper()
	writeFile(t, filepath.Join(h.services.dir, WifiLabel+".plist"), "<plist/>")
	h.r.WifiHolder = "wifi-holder"
}

func TestThePersistentWifiServiceIsRestartedAfterItsSocketHolderIsEnsured(t *testing.T) {
	// Arrange
	h := newRestarter(t)
	wifiHeld(t, h)

	// Act
	err := h.r.RestartStandalone(context.Background(), Wifi)

	// Assert
	if want := []string{"ensure-holder wifi-holder", "define " + WifiLabel, "restart " + WifiLabel}; err != nil || fmt.Sprint(h.acts()) != fmt.Sprint(want) {
		t.Fatalf("RestartStandalone = %v, acts %v; want %v", err, h.acts(), want)
	}
	if fmt.Sprint(h.ensuredHolders) != "[wifi-holder]" {
		t.Fatalf("ensured holders %v, want the service's own holder once", h.ensuredHolders)
	}
}

func TestThePersistentWifiServiceIsNotRestartedWhenItsHolderCannotBeEnsured(t *testing.T) {
	// Arrange
	h := newRestarter(t)
	wifiHeld(t, h)
	h.holderErr = errors.New("no holder serves")

	// Act
	err := h.r.RestartStandalone(context.Background(), Wifi)

	// Assert: the refusal names the service and its holder, and nothing was started or restarted.
	if err == nil || !strings.Contains(err.Error(), "wifi-holder") || !strings.Contains(err.Error(), Wifi.Display) {
		t.Fatalf("RestartStandalone = %v, want the holder's failure naming the service and the holder", err)
	}
	if fmt.Sprint(h.acts()) != "[ensure-holder wifi-holder]" {
		t.Fatalf("acts %v, want only the holder's failed ensure, and no service act", h.acts())
	}
}

func TestEnsuringThePersistentWifiServiceLoadedEnsuresItsHolderFirst(t *testing.T) {
	// Arrange: the service is unloaded, its definition written.
	h := newRestarter(t)
	wifiHeld(t, h)
	h.services.loaded[WifiLabel] = false

	// Act
	err := h.r.EnsureStandaloneLoaded(context.Background(), Wifi)

	// Assert
	if err != nil || fmt.Sprint(h.ensuredHolders) != "[wifi-holder]" {
		t.Fatalf("EnsureStandaloneLoaded = %v, holders %v; want the holder ensured", err, h.ensuredHolders)
	}
}

func TestAStandaloneWithNoHeldSocketEnsuresNoHolder(t *testing.T) {
	// Arrange: the test service takes no holder's socket.
	h := newRestarter(t)
	testsLoaded(t, h)
	h.r.WifiHolder = "wifi-holder"

	// Act
	err := h.r.RestartStandalone(context.Background(), Tests)

	// Assert
	if err != nil || len(h.ensuredHolders) != 0 {
		t.Fatalf("RestartStandalone = %v, holders %v; want no holder ensured for a service that is not Held", err, h.ensuredHolders)
	}
}

func TestAPersistentWifiServiceWhoseSocketNoDeclaredHolderHoldsEnsuresNoHolder(t *testing.T) {
	// Arrange: a harness's own socket, no declared holder.
	h := newRestarter(t)
	wifiHeld(t, h)
	h.r.WifiHolder = ""

	// Act
	err := h.r.RestartStandalone(context.Background(), Wifi)

	// Assert
	if err != nil || len(h.ensuredHolders) != 0 {
		t.Fatalf("RestartStandalone = %v, holders %v; want no holder ensured", err, h.ensuredHolders)
	}
}
