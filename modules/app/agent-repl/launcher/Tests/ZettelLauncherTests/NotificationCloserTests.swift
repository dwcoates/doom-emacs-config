import Foundation
import XCTest
@testable import ZettelLauncher

/// A NotificationScreen showing a scripted scan and recording each action.
final class FakeScreen: NotificationScreen {
    var shown = ScreenScan(notifications: [])
    var scanError: Error?
    var performError: Error?
    private(set) var performed: [(action: String, identifier: String)] = []

    func scan() throws -> ScreenScan {
        if let scanError { throw scanError }
        return shown
    }

    func perform(_ action: String, on identifier: String) throws {
        performed.append((action, identifier))
        if let performError { throw performError }
    }
}

/// A DeadlineScheduler on a clock the test moves, firing nothing on its own.
final class FakeScheduler: DeadlineScheduler {
    final class Call: ScheduledCall {
        let date: Date
        let fire: () -> Void
        var cancelled = false
        init(date: Date, fire: @escaping () -> Void) { self.date = date; self.fire = fire }
        func cancel() { cancelled = true }
    }

    var clock = Date(timeIntervalSince1970: 1_000)
    private(set) var calls: [Call] = []

    func now() -> Date { clock }

    func schedule(at date: Date, _ fire: @escaping () -> Void) -> ScheduledCall {
        let call = Call(date: date, fire: fire)
        calls.append(call)
        return call
    }

    /// The one call still armed, if any.
    var armed: Call? {
        let live = calls.filter { !$0.cancelled }
        precondition(live.count <= 1, "the closer armed \(live.count) timers at once")
        return live.first
    }

    /// Moves the clock to the armed call's date and fires it.
    func fireArmed() {
        guard let call = armed else { return XCTFail("no deadline is armed") }
        clock = call.date
        call.cancelled = true
        call.fire()
    }
}

struct Failure: Error, CustomStringConvertible {
    let description: String
}

func slack(_ identifier: String, actions: [String] = ["Show Details", "Reply", "Close"]) -> ShownNotification {
    ShownNotification(identifier: identifier, description: "Slack, New message in #general, hello", actions: actions)
}

func zettel(_ identifier: String) -> ShownNotification {
    ShownNotification(identifier: identifier, description: "Zettel, footer-activity-updates, finished its turn", actions: ["Show Details", "Close"])
}

final class NotificationCloserTests: XCTestCase {
    var screen: FakeScreen!
    var scheduler: FakeScheduler!
    var log: RecordingLogger!
    var duration: NotificationDuration = .seconds(15)
    var closer: NotificationCloser!

    override func setUp() {
        screen = FakeScreen()
        scheduler = FakeScheduler()
        log = RecordingLogger()
        duration = .seconds(15)
        closer = NotificationCloser(screen: screen, scheduler: scheduler, duration: { [unowned self] in self.duration }, log: log)
    }

    func testASlackNotificationIsClosedAfterItsTime() {
        // Arrange
        screen.shown = ScreenScan(notifications: [slack("A")])

        // Act
        closer.screenChanged()
        scheduler.fireArmed()

        // Assert
        XCTAssertEqual(scheduler.calls.first?.date, Date(timeIntervalSince1970: 1_015))
        XCTAssertEqual(screen.performed.map(\.action), ["Close"])
        XCTAssertEqual(screen.performed.map(\.identifier), ["A"])
        XCTAssertEqual(log.first(.info, "closed a notification after its time on screen")?.context["identifier"], "A")
        XCTAssertTrue(closer.pending.isEmpty)
    }

    func testAZettelNotificationIsClosedAfterItsTime() {
        // Arrange
        screen.shown = ScreenScan(notifications: [zettel("Z")])

        // Act
        closer.screenChanged()
        scheduler.fireArmed()

        // Assert
        XCTAssertEqual(screen.performed.map(\.identifier), ["Z"])
        XCTAssertEqual(log.first(.info, "closed a notification after its time on screen")?.context["app"], "Zettel")
        XCTAssertTrue(closer.pending.isEmpty)
    }

    func testEachRecordNamesTheNotificationsApp() {
        // Arrange
        screen.shown = ScreenScan(notifications: [slack("A")])

        // Act
        closer.screenChanged()

        // Assert
        XCTAssertEqual(log.first(.info, "a notification appeared; it closes after its time on screen")?.context["app"], "Slack")
    }

    func testAnotherAppsNotificationIsLeftAlone() {
        // Arrange
        screen.shown = ScreenScan(notifications: [ShownNotification(identifier: "M", description: "Messages, Vivian, hi", actions: ["Close"])])

        // Act
        closer.screenChanged()

        // Assert
        XCTAssertTrue(closer.pending.isEmpty)
        XCTAssertNil(scheduler.armed)
    }

    func testASlackNotificationSeenAgainKeepsItsFirstDeadline() {
        // Arrange
        screen.shown = ScreenScan(notifications: [slack("A")])
        closer.screenChanged()
        scheduler.clock = Date(timeIntervalSince1970: 1_010)

        // Act
        closer.screenChanged()

        // Assert
        XCTAssertEqual(closer.pending["A"], Date(timeIntervalSince1970: 1_015))
        XCTAssertEqual(scheduler.armed?.date, Date(timeIntervalSince1970: 1_015))
    }

    func testEachNotificationClosesAtItsOwnDeadline() {
        // Arrange
        screen.shown = ScreenScan(notifications: [slack("A")])
        closer.screenChanged()
        scheduler.clock = Date(timeIntervalSince1970: 1_005)
        screen.shown = ScreenScan(notifications: [slack("A"), slack("B")])
        closer.screenChanged()

        // Act
        scheduler.fireArmed()

        // Assert: only A's time is up; B's deadline is armed next.
        XCTAssertEqual(screen.performed.map(\.identifier), ["A"])
        XCTAssertEqual(scheduler.armed?.date, Date(timeIntervalSince1970: 1_020))
    }

    func testANotificationGoneBeforeItsTimeIsForgotten() {
        // Arrange
        screen.shown = ScreenScan(notifications: [slack("A")])
        closer.screenChanged()
        screen.shown = ScreenScan(notifications: [])

        // Act
        closer.screenChanged()

        // Assert
        XCTAssertTrue(closer.pending.isEmpty)
        XCTAssertNil(scheduler.armed)
        XCTAssertNotNil(log.first(.debug, "a notification left the screen before its time"))
    }

    func testANotificationGoneAtItsDeadlineIsNotActedOn() {
        // Arrange
        screen.shown = ScreenScan(notifications: [slack("A")])
        closer.screenChanged()
        screen.shown = ScreenScan(notifications: [])

        // Act: the deadline falls due before any change reached the closer.
        scheduler.fireArmed()

        // Assert
        XCTAssertTrue(screen.performed.isEmpty)
        XCTAssertTrue(closer.pending.isEmpty)
    }

    func testTheConfiguredDurationIsUsed() {
        // Arrange
        duration = .seconds(40)
        screen.shown = ScreenScan(notifications: [slack("A")])

        // Act
        closer.screenChanged()

        // Assert
        XCTAssertEqual(closer.pending["A"], Date(timeIntervalSince1970: 1_040))
        XCTAssertEqual(log.first(.info, "a notification appeared; it closes after its time on screen")?.context["seconds"], "40")
    }

    func testAnInvalidDurationLeavesTheNotificationUpAndIsRecordedOnce() {
        // Arrange
        duration = .invalid("soon")
        screen.shown = ScreenScan(notifications: [slack("A")])

        // Act
        closer.screenChanged()
        closer.screenChanged()

        // Assert
        let refusals = log.records.filter { $0.level == .error && $0.message.hasPrefix("NotificationSeconds is not") }
        XCTAssertEqual(refusals.count, 1)
        XCTAssertEqual(refusals.first?.context["value"], "soon")
        XCTAssertEqual(refusals.first?.context["identifier"], "A")
        XCTAssertTrue(closer.pending.isEmpty)
        XCTAssertNil(scheduler.armed)
    }

    func testANotificationWithoutCloseIsRecordedAndLeft() {
        // Arrange
        screen.shown = ScreenScan(notifications: [slack("A", actions: ["Show Details"])])
        closer.screenChanged()

        // Act
        scheduler.fireArmed()

        // Assert
        let record = log.first(.error, "a notification offers no Close action, so it stays on screen; macOS may have changed its notifications' actions")
        XCTAssertEqual(record?.context["actions"], "Show Details")
        XCTAssertEqual(record?.context["identifier"], "A")
        XCTAssertTrue(screen.performed.isEmpty)
        XCTAssertTrue(closer.pending.isEmpty)
    }

    func testAFailedCloseIsRecorded() {
        // Arrange
        screen.shown = ScreenScan(notifications: [slack("A")])
        screen.performError = Failure(description: "AXError -25205")
        closer.screenChanged()

        // Act
        scheduler.fireArmed()

        // Assert
        let record = log.first(.error, "closing a notification failed, so it stays on screen")
        XCTAssertEqual(record?.context["cause"], "AXError -25205")
        XCTAssertEqual(record?.context["identifier"], "A")
        XCTAssertTrue(closer.pending.isEmpty)
        XCTAssertNil(scheduler.armed)
    }

    func testAFailedScanIsRecordedAndChangesNothing() {
        // Arrange
        screen.shown = ScreenScan(notifications: [slack("A")])
        closer.screenChanged()
        screen.scanError = Failure(description: "AXError -25204")

        // Act
        closer.screenChanged()

        // Assert
        XCTAssertEqual(log.first(.error, "reading Notification Center's windows failed, so notifications stay on screen")?.context["cause"], "AXError -25204")
        XCTAssertEqual(closer.pending["A"], Date(timeIntervalSince1970: 1_015))
    }

    func testANotificationNamingNoAppIsWarnedOnce() {
        // Arrange
        screen.shown = ScreenScan(notifications: [ShownNotification(identifier: "X", description: nil, actions: ["Close"])])

        // Act
        closer.screenChanged()
        closer.screenChanged()

        // Assert
        let warnings = log.records.filter { $0.level == .warn && $0.message.hasPrefix("a notification on screen names no app") }
        XCTAssertEqual(warnings.count, 1)
        XCTAssertEqual(warnings.first?.context["description"], "<none>")
        XCTAssertTrue(closer.pending.isEmpty)
    }

    func testAnUnrecognizedWindowIsWarnedOncePerWindow() {
        // Arrange
        screen.shown = ScreenScan(notifications: [], unrecognizedWindows: ["w1"])

        // Act
        closer.screenChanged()
        closer.screenChanged()
        screen.shown = ScreenScan(notifications: [], unrecognizedWindows: ["w1", "w2"])
        closer.screenChanged()

        // Assert
        let warnings = log.records.filter { $0.level == .warn && $0.message.hasPrefix("a Notification Center window holds no notification") }
        XCTAssertEqual(warnings.map { $0.context["window"] }, ["w1", "w2"])
    }
}

final class NotificationDurationTests: XCTestCase {
    var defaults: UserDefaults!
    let suite = "zettel-tests-\(UUID().uuidString)"

    override func setUp() {
        defaults = UserDefaults(suiteName: suite)
    }

    override func tearDown() {
        defaults.removePersistentDomain(forName: suite)
    }

    func testUnsetIsTheDefault() {
        XCTAssertEqual(notificationDuration(defaults), .seconds(15))
    }

    func testAWholeNumberIsThatManySeconds() {
        defaults.set(30, forKey: notificationSecondsKey)
        XCTAssertEqual(notificationDuration(defaults), .seconds(30))
    }

    func testANumberWrittenAsTextIsThatManySeconds() {
        defaults.set("20", forKey: notificationSecondsKey)
        XCTAssertEqual(notificationDuration(defaults), .seconds(20))
    }

    func testZeroIsInvalid() {
        defaults.set(0, forKey: notificationSecondsKey)
        XCTAssertEqual(notificationDuration(defaults), .invalid("0"))
    }

    func testANegativeNumberIsInvalid() {
        defaults.set(-5, forKey: notificationSecondsKey)
        XCTAssertEqual(notificationDuration(defaults), .invalid("-5"))
    }

    func testAFractionIsInvalid() {
        defaults.set(2.5, forKey: notificationSecondsKey)
        XCTAssertEqual(notificationDuration(defaults), .invalid("2.5"))
    }

    func testABooleanIsInvalid() {
        defaults.set(true, forKey: notificationSecondsKey)
        XCTAssertEqual(notificationDuration(defaults), .invalid("1"))
    }

    func testTextThatIsNoNumberIsInvalid() {
        defaults.set("soon", forKey: notificationSecondsKey)
        XCTAssertEqual(notificationDuration(defaults), .invalid("soon"))
    }
}

final class ClosedAppsTests: XCTestCase {
    func testTheClosedAppsAreSlackAndZettel() {
        XCTAssertEqual(closedApps, ["Slack", "Zettel"])
    }

    func testZettelsNameIsItsBundlesName() throws {
        // Arrange: macOS names Zettel's banners by the bundle's CFBundleName.
        let plist = URL(fileURLWithPath: #filePath)
            .deletingLastPathComponent().deletingLastPathComponent().deletingLastPathComponent()
            .appendingPathComponent("Bundle/Info.plist")

        // Act
        let info = try PropertyListSerialization.propertyList(from: Data(contentsOf: plist), format: nil) as? [String: Any]

        // Assert
        XCTAssertEqual(info?["CFBundleName"] as? String, zettelAppName)
    }
}

final class ShownNotificationTests: XCTestCase {
    func testTheAppIsTheDescriptionsFirstPart() {
        XCTAssertEqual(slack("A").app, "Slack")
    }

    func testADescriptionWithoutASeparatorNamesNoApp() {
        XCTAssertNil(ShownNotification(identifier: "A", description: "Slack", actions: []).app)
    }

    func testNoDescriptionNamesNoApp() {
        XCTAssertNil(ShownNotification(identifier: "A", description: nil, actions: []).app)
    }
}
