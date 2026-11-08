import Foundation
import XCTest
import ZettelProto
@testable import ZettelNotifier

final class RelayTests: XCTestCase {
    var dialer: FakeDialer!
    var center: FakeCenter!
    var log: RecordingLogger!
    var supersededCalls: SupersededCounter!
    var relay: Relay!

    final class SupersededCounter: @unchecked Sendable {
        let signals = Signals()
        func call() { Task { await signals.raise("superseded") } }
    }

    override func setUp() { reset() }

    /// A fresh relay over fresh fakes.
    func reset() {
        dialer = FakeDialer()
        center = FakeCenter()
        log = RecordingLogger()
        supersededCalls = SupersededCounter()
        relay = Relay(dial: dialer.dial, center: center, log: log, pid: 4242, superseded: { [supersededCalls] in supersededCalls!.call() })
    }

    /// Advertises `address` and waits for its stream to open.
    func attach(_ address: String = "127.0.0.1:1") async -> FakeDaemon {
        await relay.advertised(DaemonAdvertisement(address: address, pid: 1))
        await dialer.signals.wait("opened \(address)")
        return dialer.daemon(address)
    }

    func testAnAdvertisementOpensTheStreamWithThisProcess() async {
        // Act
        let daemon = await attach()

        // Assert
        XCTAssertEqual(daemon.opens.map(\.process.pid), [4242])
        let serving = await relay.servingAddress
        XCTAssertEqual(serving, "127.0.0.1:1")
    }

    func testABannerIsPostedAndAnsweredPosted() async {
        // Arrange
        let daemon = await attach()

        // Act
        daemon.send(bannerFrame(post: 7))
        await dialer.signals.wait("reported")

        // Assert
        XCTAssertEqual(center.posted.map(\.postID.value), [7])
        XCTAssertEqual(daemon.posts.count, 1)
        XCTAssertEqual(daemon.posts[0].postID.value, 7)
        XCTAssertNotNil(daemon.posts[0].outcome.posted)
        await log.signals.wait("posted a banner")
        XCTAssertEqual(log.first(.info, "posted a banner")?.context["post"], "7")
    }

    func testARefusedBannerIsAnsweredWithItsRefusalAndRecordedAsAnError() async {
        let cases: [(PostResult, (Agentrepl_V1_DesktopBannerPostOutcome) -> Bool, String)] = [
            (.notAllowed, { $0.notAllowed != nil }, "macOS refused the banner: Zettel is not allowed to notify (System Settings → Notifications → Zettel)"),
            (.failed("boom"), { $0.failed.platformError == "boom" }, "macOS refused the banner"),
        ]
        for (result, check, message) in cases {
            // Arrange
            reset()
            center.result = result
            let daemon = await attach()

            // Act
            daemon.send(bannerFrame(post: 1))
            await dialer.signals.wait("reported")

            // Assert
            XCTAssertTrue(check(daemon.posts[0].outcome), "\(result): outcome \(daemon.posts[0].outcome)")
            await log.signals.wait(message)
            XCTAssertNotNil(log.first(.error, message), "\(result) was not recorded as an error")
        }
    }

    func testAnAnswerTheDaemonNoLongerAwaitsIsRecorded() async {
        // Arrange
        let daemon = await attach()
        daemon.postedAnswer = .with { $0.error.unknownPost = .init() }

        // Act
        daemon.send(bannerFrame(post: 3))

        // Assert
        await log.signals.wait("the daemon no longer awaited the post's answer")
        XCTAssertEqual(log.first(.info, "the daemon no longer awaited the post's answer")?.context["post"], "3")
    }

    func testSupersededEndsTheAppAndLetsTheStreamGo() async {
        // Arrange
        let daemon = await attach()

        // Act
        daemon.send(.with { $0.superseded = .init() })

        // Assert
        await supersededCalls.signals.wait("superseded")
        await log.signals.wait("another Zettel instance took the banner stream; this one exits")
        let serving = await relay.servingAddress
        XCTAssertNil(serving)
    }

    func testAPlannedEndingIsNoError() async {
        // Arrange
        let daemon = await attach()

        // Act
        daemon.send(.with { $0.ending = .init() })
        daemon.finish()

        // Assert
        await log.signals.wait("the daemon is standing down; awaiting the next advertised daemon")
        XCTAssertFalse(log.records.contains { $0.level == .error }, "a planned ending recorded an error: \(log.records)")
    }

    func testAnUnplannedEndIsAnError() async {
        // Arrange
        let daemon = await attach()

        // Act
        daemon.finish()

        // Assert
        let message = "the banner stream ended without the daemon's planned ending; no banners post until a daemon is advertised again"
        await log.signals.wait(message)
        XCTAssertNotNil(log.first(.error, message))
    }

    func testAFailedStreamIsAnErrorWithItsCause() async {
        // Arrange
        let daemon = await attach()

        // Act
        daemon.finish(throwing: ConnectError(code: "unavailable", message: "gone"))

        // Assert
        let message = "the banner stream failed; no banners post until a daemon is advertised again"
        await log.signals.wait(message)
        XCTAssertEqual(log.first(.error, message)?.context["cause"], "connect unavailable: gone")
    }

    func testTheSameAdvertisementAgainDialsNothing() async {
        // Arrange
        _ = await attach()

        // Act
        await relay.advertised(DaemonAdvertisement(address: "127.0.0.1:1", pid: 1))

        // Assert
        XCTAssertEqual(dialer.dialCount, 1)
    }

    func testANewAdvertisementReplacesTheStream() async {
        // Arrange
        _ = await attach("127.0.0.1:1")

        // Act
        _ = await attach("127.0.0.1:2")

        // Assert
        let serving = await relay.servingAddress
        XCTAssertEqual(serving, "127.0.0.1:2")
        await log.signals.wait("the banner stream was replaced")
        XCTAssertFalse(log.records.contains { $0.level == .error }, "a replaced stream recorded an error: \(log.records)")
    }

    func testAWithdrawnAdvertisementDropsTheStream() async {
        // Arrange
        _ = await attach()

        // Act
        await relay.advertised(nil)

        // Assert
        let serving = await relay.servingAddress
        XCTAssertNil(serving)
        XCTAssertNotNil(log.first(.info, "the daemon withdrew its advertisement; dropping its banner stream"))
    }

    func token(_ workspace: String = "ws1") -> Data {
        try! Agentrepl_V1_DesktopBannerClickToken.with { $0.workspace.id = workspace; $0.workspace.dir = "/work/\(workspace)" }.serializedData()
    }

    func testAClickIsReportedToTheAdvertisedDaemon() async {
        // Arrange
        let daemon = await attach()

        // Act
        await relay.clicked(token: token())

        // Assert
        XCTAssertEqual(daemon.clicks.map(\.click.workspace.id), ["ws1"])
        XCTAssertNotNil(log.first(.info, "reported a banner's click; Emacs selects its workspace"))
    }

    func testAClickTheDaemonHandedOnIsReportedToItsSuccessorOnce() async {
        // Arrange
        let first = await attach("127.0.0.1:1")
        first.clickAnswer = .with { $0.error.transferringAway.address = "127.0.0.1:2" }
        let second = dialer.daemon("127.0.0.1:2")
        second.clickAnswer = .with { $0.error.transferringAway.address = "127.0.0.1:3" }

        // Act
        await relay.clicked(token: token())

        // Assert
        XCTAssertEqual(first.clicks.count, 1)
        XCTAssertEqual(second.clicks.count, 1)
        XCTAssertEqual(dialer.daemon("127.0.0.1:3").clicks.count, 0, "a redirect was followed twice")
        XCTAssertNotNil(log.first(.error, "the daemon refused the click; it selects nothing"))
    }

    func testAClickOnAWorkspaceNotYetAdoptedIsReportedToItsServingDaemon() async {
        // Arrange
        let first = await attach("127.0.0.1:1")
        first.clickAnswer = .with { $0.error.notYetAdopted.address = "127.0.0.1:2" }

        // Act
        await relay.clicked(token: token())

        // Assert
        XCTAssertEqual(dialer.daemon("127.0.0.1:2").clicks.count, 1)
    }

    func testARefusedClickIsAnError() async {
        // Arrange
        let daemon = await attach()
        daemon.clickAnswer = .with { $0.error.unknownWorkspace = .init() }

        // Act
        await relay.clicked(token: token())

        // Assert
        XCTAssertNotNil(log.first(.error, "the daemon refused the click; it selects nothing"))
    }

    func testAClickThatCannotReachTheDaemonIsAnError() async {
        // Arrange
        let daemon = await attach()
        daemon.clickError = ConnectError(code: "unavailable", message: "gone")

        // Act
        await relay.clicked(token: token())

        // Assert
        XCTAssertEqual(log.first(.error, "the click did not reach the daemon")?.context["cause"], "connect unavailable: gone")
    }

    func testAClickWithNoDaemonAdvertisedSelectsNothing() async {
        // Act
        await relay.clicked(token: token())

        // Assert
        XCTAssertEqual(dialer.dialCount, 0)
        XCTAssertNotNil(log.first(.error, "a banner was clicked while no daemon is advertised; it selects nothing"))
    }

    func testAClickWithAnUnreadableTokenSelectsNothing() async {
        // Arrange
        _ = await attach()

        // Act
        await relay.clicked(token: Data([0xFF, 0xFF, 0xFF]))

        // Assert
        XCTAssertNotNil(log.first(.error, "a clicked banner carried no readable click token"))
    }
}
