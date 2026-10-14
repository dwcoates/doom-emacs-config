package cpugov

import (
	"os"
	"path/filepath"
	"testing"
)

// supply writes one power supply's attributes under root.
func supply(t *testing.T, root, name string, attrs map[string]string) {
	t.Helper()
	dir := filepath.Join(root, name)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		t.Fatal(err)
	}
	for k, v := range attrs {
		if err := os.WriteFile(filepath.Join(dir, k), []byte(v+"\n"), 0o644); err != nil {
			t.Fatal(err)
		}
	}
}

func TestPowerFromSysfs(t *testing.T) {
	tests := []struct {
		name     string
		supplies map[string]map[string]string
		missing  bool
		want     Power
	}{
		{name: "no class directory", missing: true, want: PowerAC},
		{name: "no supplies", want: PowerAC},
		{name: "mains online", supplies: map[string]map[string]string{
			"AC":   {"type": "Mains", "online": "1"},
			"BAT0": {"type": "Battery", "status": "Charging"}}, want: PowerAC},
		{name: "mains offline", supplies: map[string]map[string]string{
			"AC":   {"type": "Mains", "online": "0"},
			"BAT0": {"type": "Battery", "status": "Discharging"}}, want: PowerBattery},
		{name: "battery discharging, no mains", supplies: map[string]map[string]string{
			"BAT0": {"type": "Battery", "status": "Discharging"}}, want: PowerBattery},
		{name: "battery full, no mains", supplies: map[string]map[string]string{
			"BAT0": {"type": "Battery", "status": "Full"}}, want: PowerAC},
		{name: "other supply only", supplies: map[string]map[string]string{
			"hid-mouse": {"type": "USB"}}, want: PowerAC},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange.
			root := filepath.Join(t.TempDir(), "power_supply")
			if !tt.missing {
				if err := os.MkdirAll(root, 0o755); err != nil {
					t.Fatal(err)
				}
			}
			for name, attrs := range tt.supplies {
				supply(t, root, name, attrs)
			}

			// Act.
			got, err := powerFromSysfs(root)

			// Assert.
			if err != nil || got != tt.want {
				t.Fatalf("powerFromSysfs = %v, %v; want %v", got, err, tt.want)
			}
		})
	}
}

func TestPowerFromSysfsRefusesAnUnreadableSupply(t *testing.T) {
	// Arrange: a mains supply whose online attribute is missing.
	root := t.TempDir()
	supply(t, root, "AC", map[string]string{"type": "Mains"})

	// Act.
	got, err := powerFromSysfs(root)

	// Assert.
	if err == nil || got != PowerUnknown {
		t.Fatalf("powerFromSysfs = %v, %v; want an error and an unknown source", got, err)
	}
}
