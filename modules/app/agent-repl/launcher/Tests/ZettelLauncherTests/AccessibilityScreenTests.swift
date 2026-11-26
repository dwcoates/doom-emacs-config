import Foundation
import XCTest
@testable import ZettelLauncher

/// An Accessibility element of a scripted tree.
struct FakeNode: AccessibilityNode {
    var role: String?
    var subrole: String?
    var identifier: String?
    var nodeDescription: String?
    var rawActionNames: [String] = []
    var children: [FakeNode] = []
}

let closeRaw = "Name:Close\nTarget:0x0\nSelector:(null)"

/// A notification's group as macOS 26.2 draws it (measured 2026-11-25).
func notificationGroup(_ identifier: String?, _ description: String, subrole: String = "AXNotificationCenterBanner") -> FakeNode {
    FakeNode(role: "AXGroup", subrole: subrole, identifier: identifier, nodeDescription: description,
             rawActionNames: ["AXPress", "Name:Show Details\nTarget:0x0\nSelector:(null)", closeRaw],
             children: [FakeNode(role: "AXStaticText", identifier: "title"), FakeNode(role: "AXStaticText", identifier: "body")])
}

/// A window on screen: Notification Center's hosting groups around a scroll
/// area holding the notifications directly.
func bannerWindow(_ groups: [FakeNode]) -> FakeNode {
    FakeNode(role: "AXWindow", subrole: "AXSystemDialog", children: [
        FakeNode(role: "AXGroup", subrole: "AXHostingView", children: [
            FakeNode(role: "AXGroup", children: [FakeNode(role: "AXScrollArea", children: groups)]),
            FakeNode(role: "AXGroup", identifier: "widgets-overlay-view"),
        ]),
    ])
}

/// The panel the user opens from the menu bar: its history sits under the
/// notification list group, beside the widgets.
func panelWindow(_ groups: [FakeNode]) -> FakeNode {
    FakeNode(role: "AXWindow", subrole: "AXSystemDialog", children: [
        FakeNode(role: "AXGroup", subrole: "AXHostingView", children: [
            FakeNode(role: "AXGroup", children: [
                FakeNode(role: "AXScrollArea", children: [
                    FakeNode(role: "AXGroup", identifier: notificationListIdentifier, children: groups),
                    FakeNode(role: "AXOpaqueProviderGroup", subrole: "AXOpaqueProviderGrid"),
                ]),
            ]),
        ]),
    ])
}

final class ScanWindowsTests: XCTestCase {
    func testANotificationOnScreenIsFoundWithItsActions() {
        // Arrange
        let windows = [(key: "w1", node: bannerWindow([notificationGroup("A", "Slack, New message, hi")]))]

        // Act
        let result = scanWindows(windows)

        // Assert
        XCTAssertEqual(result.scan, ScreenScan(notifications: [
            ShownNotification(identifier: "A", description: "Slack, New message, hi", actions: ["AXPress", "Show Details", "Close"]),
        ]))
        XCTAssertEqual(result.elements["A"]?.nodeDescription, "Slack, New message, hi")
    }

    func testThePanelsHistoryIsNeverPartOfTheScan() {
        // Arrange
        let windows = [(key: "p", node: panelWindow([notificationGroup("H", "Slack, Old, message")]))]

        // Act
        let result = scanWindows(windows)

        // Assert
        XCTAssertEqual(result.scan, ScreenScan(notifications: []))
        XCTAssertTrue(result.elements.isEmpty)
    }

    func testAStackIsOneNotification() {
        // Arrange
        let stack = notificationGroup("S", "Slack, New message, hi, stacked", subrole: "AXNotificationCenterBannerStack")
        let windows = [(key: "w1", node: bannerWindow([stack]))]

        // Act
        let result = scanWindows(windows)

        // Assert
        XCTAssertEqual(result.scan.notifications.map(\.identifier), ["S"])
    }

    func testEveryWindowsNotificationsAreFound() {
        // Arrange
        let windows = [
            (key: "w1", node: bannerWindow([notificationGroup("A", "Slack, a, b")])),
            (key: "w2", node: bannerWindow([notificationGroup("B", "Messages, c, d")])),
        ]

        // Act
        let result = scanWindows(windows)

        // Assert
        XCTAssertEqual(result.scan.notifications.map(\.identifier), ["A", "B"])
        XCTAssertTrue(result.scan.unrecognizedWindows.isEmpty)
    }

    func testAWindowWithNoNotificationIsUnrecognized() {
        // Arrange: a group whose subrole no longer names a notification.
        let renamed = FakeNode(role: "AXGroup", subrole: "AXSomethingNew", identifier: "A", nodeDescription: "Slack, a, b")
        let windows = [(key: "w1", node: bannerWindow([renamed]))]

        // Act
        let result = scanWindows(windows)

        // Assert
        XCTAssertEqual(result.scan, ScreenScan(notifications: [], unrecognizedWindows: ["w1"]))
    }

    func testANotificationWithNoIdentifierMakesItsWindowUnrecognized() {
        // Arrange
        let windows = [(key: "w1", node: bannerWindow([notificationGroup(nil, "Slack, a, b"), notificationGroup("B", "Slack, c, d")]))]

        // Act
        let result = scanWindows(windows)

        // Assert
        XCTAssertEqual(result.scan.notifications.map(\.identifier), ["B"])
        XCTAssertEqual(result.scan.unrecognizedWindows, ["w1"])
    }
}

final class ActionNameTests: XCTestCase {
    func testACustomActionIsItsName() {
        XCTAssertEqual(actionName(closeRaw), "Close")
    }

    func testAStandardActionIsItself() {
        XCTAssertEqual(actionName("AXPress"), "AXPress")
    }
}

final class AccessibilityReportTests: XCTestCase {
    func testATrustedAppMayUseAccessibility() {
        XCTAssertEqual(accessibilityReport(trusted: true).line, "Zettel may use Accessibility")
        XCTAssertTrue(accessibilityReport(trusted: true).allowed)
    }

    func testAnUntrustedAppIsToldWhereToAllowIt() {
        XCTAssertFalse(accessibilityReport(trusted: false).allowed)
        XCTAssertTrue(accessibilityReport(trusted: false).line.contains("Privacy & Security → Accessibility"))
    }
}
