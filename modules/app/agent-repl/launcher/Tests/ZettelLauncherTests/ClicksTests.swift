import Foundation
import XCTest
import ZettelProto
@testable import ZettelLauncher

final class ClicksTests: XCTestCase {
    var dialer: FakeDialer!
    var log: RecordingLogger!
    var clicks: ClickReporter!

    override func setUp() {
        dialer = FakeDialer()
        log = RecordingLogger()
        clicks = ClickReporter(dial: dialer.dial, log: log)
    }

    /// Advertises `address`, answering the daemon a click there reaches.
    func attach(_ address: String = "127.0.0.1:1") async -> FakeDaemon {
        await clicks.advertised(DaemonAdvertisement(address: address, pid: 1))
        return dialer.daemon(address)
    }

    func token(_ workspace: String = "ws1") -> Data {
        try! Agentrepl_V1_DesktopBannerClickToken.with { $0.workspace.id = workspace; $0.workspace.dir = "/work/\(workspace)" }.serializedData()
    }

    func testAClickIsReportedToTheAdvertisedDaemon() async {
        // Arrange
        let daemon = await attach()

        // Act
        await clicks.clicked(token: token())

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
        await clicks.clicked(token: token())

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
        await clicks.clicked(token: token())

        // Assert
        XCTAssertEqual(dialer.daemon("127.0.0.1:2").clicks.count, 1)
    }

    func testARefusedClickIsAnError() async {
        // Arrange
        let daemon = await attach()
        daemon.clickAnswer = .with { $0.error.unknownWorkspace = .init() }

        // Act
        await clicks.clicked(token: token())

        // Assert
        XCTAssertNotNil(log.first(.error, "the daemon refused the click; it selects nothing"))
    }

    func testAClickThatCannotReachTheDaemonIsAnError() async {
        // Arrange
        let daemon = await attach()
        daemon.clickError = ConnectError(code: "unavailable", message: "gone")

        // Act
        await clicks.clicked(token: token())

        // Assert
        XCTAssertEqual(log.first(.error, "the click did not reach the daemon")?.context["cause"], "connect unavailable: gone")
    }

    func testAClickWithNoDaemonAdvertisedSelectsNothing() async {
        // Act
        await clicks.clicked(token: token())

        // Assert
        XCTAssertEqual(dialer.dialCount, 0)
        XCTAssertNotNil(log.first(.error, "a banner was clicked while no daemon is advertised; it selects nothing"))
    }

    func testAWithdrawnAdvertisementLeavesAClickNowhereToGo() async {
        // Arrange
        _ = await attach()
        await clicks.advertised(nil)

        // Act
        await clicks.clicked(token: token())

        // Assert
        XCTAssertEqual(dialer.dialCount, 0)
        XCTAssertNotNil(log.first(.info, "the daemon withdrew its advertisement; a click selects nothing until one is advertised again"))
        XCTAssertNotNil(log.first(.error, "a banner was clicked while no daemon is advertised; it selects nothing"))
    }

    func testAClickWithAnUnreadableTokenSelectsNothing() async {
        // Arrange
        _ = await attach()

        // Act
        await clicks.clicked(token: Data([0xFF, 0xFF, 0xFF]))

        // Assert
        XCTAssertNotNil(log.first(.error, "a clicked banner carried no readable click token"))
    }
}
