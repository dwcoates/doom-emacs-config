import Foundation
import SwiftProtobuf
import ZettelProto

/// The one procedure the launcher calls on a daemon.
public protocol Daemon: Sendable {
    /// Reports one click.
    func reportClicked(_ request: Agentrepl_V1_ReportDesktopBannerClickedRequest) async throws -> Agentrepl_V1_ReportDesktopBannerClickedResponse
}

/// Builds the Daemon at one address.
public typealias DaemonDialer = @Sendable (String) -> Daemon

/// The Daemon over Connect at one address.
public struct ConnectDaemon: Daemon {
    let client: ConnectClient
    /// How long a click's report may take: the daemon answers in milliseconds.
    static let reportTimeout: TimeInterval = 10

    public init(address: String) { client = ConnectClient(address: address) }

    public func reportClicked(_ request: Agentrepl_V1_ReportDesktopBannerClickedRequest) async throws -> Agentrepl_V1_ReportDesktopBannerClickedResponse {
        try await client.unary("ReportDesktopBannerClicked", request, timeout: Self.reportTimeout)
    }
}

/// The operation the click records carry.
enum ClickOp {
    static let click = "launcher.clicks.click"
    static let advertisement = "launcher.clicks.advertisement"
}

/// Reports each banner click to the daemon `daemon.addr` advertises at the
/// moment of the click: a click may land long after its banner was posted, on
/// a daemon that never posted it, and its token names a workspace rather than
/// a post, so whichever daemon serves then selects it.
public actor ClickReporter {
    private let dial: DaemonDialer
    private let log: Logger
    private var advertisement: DaemonAdvertisement?

    public init(dial: @escaping DaemonDialer, log: Logger) {
        self.dial = dial
        self.log = log
    }

    /// Takes the daemon's current advertisement, nil when none stands.
    public func advertised(_ next: DaemonAdvertisement?) {
        if next == nil, advertisement != nil {
            log.info(ClickOp.advertisement, "the daemon withdrew its advertisement; a click selects nothing until one is advertised again", [:])
        }
        advertisement = next
    }

    /// Reports one click. `token` is the click token's serialized bytes, as
    /// stored on the banner. The click is reported to the advertised daemon,
    /// and to the address it names when it has handed the workspace on.
    public func clicked(token: Data) async {
        let click: Agentrepl_V1_DesktopBannerClickToken
        do {
            click = try Agentrepl_V1_DesktopBannerClickToken(serializedBytes: token)
        } catch {
            log.error(ClickOp.click, "a clicked banner carried no readable click token", ["cause": "\(error)"])
            return
        }
        let workspace = click.workspace.id
        guard let address = advertisement?.address else {
            log.error(ClickOp.click, "a banner was clicked while no daemon is advertised; it selects nothing", ["workspace": workspace])
            return
        }
        var request = Agentrepl_V1_ReportDesktopBannerClickedRequest()
        request.click = click
        await report(request, to: address, workspace: workspace, redirected: false)
    }

    private func report(_ request: Agentrepl_V1_ReportDesktopBannerClickedRequest, to address: String, workspace: String, redirected: Bool) async {
        let response: Agentrepl_V1_ReportDesktopBannerClickedResponse
        do {
            response = try await dial(address).reportClicked(request)
        } catch {
            log.error(ClickOp.click, "the click did not reach the daemon", ["workspace": workspace, "address": address, "cause": "\(error)"])
            return
        }
        switch response.result {
        case .success:
            log.info(ClickOp.click, "reported a banner's click; Emacs selects its workspace", ["workspace": workspace, "address": address])
        case .error(let refusal):
            switch refusal.cause {
            case .transferringAway(let away) where !redirected:
                log.info(ClickOp.click, "the daemon handed the workspace on; reporting the click to its successor", ["workspace": workspace, "address": away.address])
                await report(request, to: away.address, workspace: workspace, redirected: true)
            case .notYetAdopted(let pending) where !redirected && pending.hasAddress:
                log.info(ClickOp.click, "the workspace is not yet adopted; reporting the click to the daemon serving it", ["workspace": workspace, "address": pending.address])
                await report(request, to: pending.address, workspace: workspace, redirected: true)
            default:
                log.error(ClickOp.click, "the daemon refused the click; it selects nothing", ["workspace": workspace, "address": address, "refusal": "\(refusal)"])
            }
        case nil:
            log.error(ClickOp.click, "the daemon answered the click with nothing this app knows", ["workspace": workspace])
        }
    }
}
