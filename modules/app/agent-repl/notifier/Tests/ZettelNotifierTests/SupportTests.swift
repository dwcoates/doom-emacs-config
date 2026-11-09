import Foundation
import UserNotifications
import XCTest
import ZettelProto
@testable import ZettelNotifier

/// The repository's agent-repl root, from this file's place in it.
let agentReplRoot = URL(fileURLWithPath: #filePath)
    .deletingLastPathComponent().deletingLastPathComponent().deletingLastPathComponent().deletingLastPathComponent()

final class LogTests: XCTestCase {
    /// The cross-language contract (proto/vocab/log-timestamp.json) holds
    /// this renderer to the same answer as Go's, TypeScript's and elisp's.
    func testTheTimestampMatchesTheSharedContract() throws {
        // Arrange
        let vocab = try JSONSerialization.jsonObject(
            with: Data(contentsOf: agentReplRoot.appendingPathComponent("proto/vocab/log-timestamp.json"))) as! [String: Any]
        let example = vocab["example"] as! [String: Any]
        let instant = ISO8601DateFormatter.withFractionalSeconds.date(from: example["instant"] as! String)!

        // Act
        let rendered = logTimestamp(instant, zone: TimeZone(identifier: "UTC")!)

        // Assert
        XCTAssertEqual(rendered, example["rendered_in_utc"] as! String)
        let pattern = try NSRegularExpression(pattern: vocab["pattern"] as! String)
        let local = logTimestamp(Date())
        XCTAssertNotNil(pattern.firstMatch(in: local, range: NSRange(local.startIndex..., in: local)), "\(local) breaks the pattern")
    }

    func testTheExecutablesBuildIsItsContentHash() throws {
        // Arrange
        let file = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
        try Data("abc".utf8).write(to: file)

        // Act
        let build = try executableBuild(at: file)

        // Assert: SHA-256("abc"), the value Go's buildreport.HashFile answers.
        XCTAssertEqual(build, "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
    }

    func testANegativeOffsetIsSigned() {
        // Act
        let rendered = logTimestamp(Date(timeIntervalSince1970: 0), zone: TimeZone(secondsFromGMT: -5 * 3600)!)

        // Assert
        XCTAssertEqual(rendered, "1969-12-31T19:00:00.000000-05:00")
    }

    func testALineCarriesEveryRequiredField() throws {
        // Act
        let line = logLine(level: .error, operation: "notifier.x", message: "m", context: ["post": UInt64(7), "cause": "c"],
                           at: Date(timeIntervalSince1970: 0), pid: 9, zone: TimeZone(identifier: "UTC")!)

        // Assert
        let object = try JSONSerialization.jsonObject(with: line) as! [String: Any]
        XCTAssertEqual(object["runtime"] as? String, "notifier")
        XCTAssertEqual(object["level"] as? String, "error")
        XCTAssertEqual(object["verbosity"] as? String, "normal")
        XCTAssertEqual(object["operation"] as? String, "notifier.x")
        XCTAssertEqual(object["message"] as? String, "m")
        XCTAssertEqual(object["pid"] as? Int, 9)
        XCTAssertEqual(object["timestamp"] as? String, "1970-01-01T00:00:00.000000+00:00")
        let context = object["context"] as! [String: Any]
        XCTAssertEqual(context["post"] as? Int, 7)
        XCTAssertEqual(context["cause"] as? String, "c")
    }

    func testTheFileLoggerAppendsOneLinePerRecord() throws {
        // Arrange
        let path = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString).appendingPathComponent("notifier.log")
        let logger = FileLogger(path: path)

        // Act
        logger.info("notifier.x", "one")
        logger.info("notifier.x", "two")

        // Assert
        let lines = try String(contentsOf: path, encoding: .utf8).split(separator: "\n")
        XCTAssertEqual(lines.count, 2)
        XCTAssertTrue(lines[1].contains("\"message\":\"two\""))
    }

    func testTheFileLoggerRotatesBeforeCrossingItsCap() throws {
        // Arrange
        let dir = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
        let path = dir.appendingPathComponent("notifier.log")
        let logger = FileLogger(path: path, cap: 300)

        // Act
        for n in 0..<12 { logger.info("notifier.x", "record \(n)") }

        // Assert
        let current = try String(contentsOf: path, encoding: .utf8)
        XCTAssertTrue(current.contains("record 11"))
        XCTAssertLessThanOrEqual(try Data(contentsOf: path).count, 300)
        XCTAssertTrue(FileManager.default.fileExists(atPath: path.path + ".1"))
        XCTAssertFalse(FileManager.default.fileExists(atPath: path.path + ".6"), "more than five generations were kept")
    }
}

extension ISO8601DateFormatter {
    static let withFractionalSeconds: ISO8601DateFormatter = {
        let f = ISO8601DateFormatter()
        f.formatOptions = [.withInternetDateTime, .withFractionalSeconds]
        return f
    }()
}

final class DaemonAddressTests: XCTestCase {
    func testAnAdvertisementNamesItsAddressAndPid() {
        XCTAssertEqual(parseAdvertisement("127.0.0.1:5000\npid=42\n"), DaemonAdvertisement(address: "127.0.0.1:5000", pid: 42))
    }

    func testALegacyAdvertisementNamesNoPid() {
        XCTAssertEqual(parseAdvertisement("127.0.0.1:5000\n"), DaemonAdvertisement(address: "127.0.0.1:5000", pid: nil))
    }

    func testAnEmptyAdvertisementNamesNothing() {
        XCTAssertNil(parseAdvertisement(""))
        XCTAssertNil(parseAdvertisement("\n\n"))
    }

    func testAMissingFileIsNoAdvertisement() {
        XCTAssertNil(readAdvertisement(stateDir: URL(fileURLWithPath: "/nonexistent-\(UUID().uuidString)")))
    }

    func testTheWatcherReportsTheStandingAndEveryChangedAdvertisement() async throws {
        // Arrange
        let dir = FileManager.default.temporaryDirectory.appendingPathComponent(UUID().uuidString)
        try FileManager.default.createDirectory(at: dir, withIntermediateDirectories: true)
        try "127.0.0.1:1\npid=1\n".write(to: advertisementPath(stateDir: dir), atomically: true, encoding: .utf8)
        let seen = AdvertisementLog()
        let watcher = AdvertisementWatcher(stateDir: dir) { ad in Task { await seen.add(ad) } }

        // Act
        try watcher.start()
        await seen.wait(count: 1)
        try "127.0.0.1:2\npid=2\n".write(to: advertisementPath(stateDir: dir), atomically: true, encoding: .utf8)
        await seen.wait(count: 2)
        try FileManager.default.removeItem(at: advertisementPath(stateDir: dir))
        await seen.wait(count: 3)

        // Assert
        let all = await seen.all
        XCTAssertEqual(all, [DaemonAdvertisement(address: "127.0.0.1:1", pid: 1), DaemonAdvertisement(address: "127.0.0.1:2", pid: 2), nil])
    }

    actor AdvertisementLog {
        private(set) var all: [DaemonAdvertisement?] = []
        private var waiters: [(Int, CheckedContinuation<Void, Never>)] = []
        func add(_ ad: DaemonAdvertisement?) {
            all.append(ad)
            let ready = waiters.filter { all.count >= $0.0 }
            waiters.removeAll { all.count >= $0.0 }
            ready.forEach { $0.1.resume() }
        }
        func wait(count: Int) async {
            if all.count >= count { return }
            await withCheckedContinuation { waiters.append((count, $0)) }
        }
    }
}

final class ConnectTests: XCTestCase {
    func testAnEnvelopeRoundTrips() {
        // Arrange
        var buffer = envelope(Data([1, 2, 3])) + envelope(Data("{}".utf8), flags: 0x02)

        // Act
        let frames = takeEnvelopes(&buffer)

        // Assert
        XCTAssertEqual(frames, [.message(Data([1, 2, 3])), .end(Data("{}".utf8))])
        XCTAssertTrue(buffer.isEmpty)
    }

    func testAPartialEnvelopeWaitsForMoreBytes() {
        // Arrange
        let whole = envelope(Data([9, 9, 9, 9]))
        var buffer = whole.prefix(6)

        // Act
        let none = takeEnvelopes(&buffer)
        buffer.append(whole.suffix(from: 6))
        let one = takeEnvelopes(&buffer)

        // Assert
        XCTAssertEqual(none, [])
        XCTAssertEqual(one, [.message(Data([9, 9, 9, 9]))])
    }

    func testTheProtocolsErrorIsReadBareOrUnderError() {
        XCTAssertEqual(connectError(fromJSON: Data(#"{"code":"invalid_argument","message":"no"}"#.utf8)),
                       ConnectError(code: "invalid_argument", message: "no"))
        XCTAssertEqual(connectError(fromJSON: Data(#"{"error":{"code":"aborted","message":"x"}}"#.utf8)),
                       ConnectError(code: "aborted", message: "x"))
        XCTAssertNil(connectError(fromJSON: Data("{}".utf8)), "a clean end-of-stream frame read as an error")
        XCTAssertEqual(connectError(fromJSON: Data("not json".utf8))?.code, "unknown")
    }
}

final class OptionsTests: XCTestCase {
    let home = URL(fileURLWithPath: "/Users/someone")

    func testTheDefaultsAreTheDaemonsStateRootAndTheCacheLog() throws {
        let options = try parseOptions([], environment: [:], home: home)
        XCTAssertEqual(options, Options(stateDir: URL(fileURLWithPath: "/Users/someone/.claude-emacs"),
                                        logPath: URL(fileURLWithPath: "/Users/someone/.cache/agent-repl/log/notifier.log"),
                                        resident: false))
    }

    func testTheStateRootFollowsTheDaemonsEnvironment() throws {
        let options = try parseOptions([], environment: ["AGENT_REPL_STATE_DIR": "/tmp/state"], home: home)
        XCTAssertEqual(options.stateDir, URL(fileURLWithPath: "/tmp/state"))
    }

    func testEveryFlagIsRead() throws {
        let options = try parseOptions(["--resident", "--state-dir", "/s", "--log", "/l", "-psn_0_1"], environment: [:], home: home)
        XCTAssertEqual(options, Options(stateDir: URL(fileURLWithPath: "/s"), logPath: URL(fileURLWithPath: "/l"), resident: true))
    }

    func testThePermissionModesAreRead() throws {
        XCTAssertEqual(try parseOptions(["--check-notifications"], environment: [:], home: home).mode, .checkNotifications)
        XCTAssertEqual(try parseOptions(["--request-notifications"], environment: [:], home: home).mode, .requestNotifications)
        XCTAssertEqual(try parseOptions([], environment: [:], home: home).mode, .run)
    }

    func testAFlagMissingItsValueIsRefused() {
        XCTAssertThrowsError(try parseOptions(["--log"], environment: [:], home: home)) {
            XCTAssertEqual($0 as? OptionsError, OptionsError(description: "--log needs a path"))
        }
    }

    func testAnUnknownArgumentIsRefused() {
        XCTAssertThrowsError(try parseOptions(["--wat"], environment: [:], home: home)) {
            XCTAssertEqual($0 as? OptionsError, OptionsError(description: "unknown argument --wat"))
        }
    }
}

final class NotificationCenterTests: XCTestCase {
    func testThePostResultOfEachAnswer() {
        XCTAssertEqual(postResult(for: nil), .posted)
        XCTAssertEqual(postResult(for: NSError(domain: UNErrorDomain, code: UNError.Code.notificationsNotAllowed.rawValue)), .notAllowed)
        if case .failed(let text) = postResult(for: NSError(domain: "X", code: 3, userInfo: [NSLocalizedDescriptionKey: "bad"])) {
            XCTAssertEqual(text, "X 3: bad")
        } else {
            XCTFail("another error was not a failure")
        }
    }

    func testABannersContentCarriesItsTextAndClickTokenAndNoSound() throws {
        // Arrange
        let banner = bannerFrame(post: 1).banner

        // Act
        let content = try notificationContent(for: banner)

        // Assert
        XCTAssertEqual(content.title, "✅ ws")
        XCTAssertEqual(content.body, "done")
        XCTAssertNil(content.sound)
        let token = try XCTUnwrap(clickToken(from: content.userInfo))
        XCTAssertEqual(try Agentrepl_V1_DesktopBannerClickToken(serializedBytes: token), banner.click)
    }

    func testEachAuthorizationIsReported() {
        XCTAssertTrue(authorizationReport(.authorized).allowed)
        XCTAssertTrue(authorizationReport(.provisional).allowed)
        XCTAssertFalse(authorizationReport(.denied).allowed)
        XCTAssertTrue(authorizationReport(.denied).line.contains("System Settings"))
        XCTAssertFalse(authorizationReport(.notDetermined).allowed)
    }

    func testANotificationWithNoTokenHasNone() {
        XCTAssertNil(clickToken(from: [:]))
    }
}
