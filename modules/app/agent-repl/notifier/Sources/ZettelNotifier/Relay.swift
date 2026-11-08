import Foundation
import SwiftProtobuf
import ZettelProto

/// The three procedures the app calls, on one daemon.
public protocol Daemon: Sendable {
    /// Opens the banner stream.
    func watchBanners(_ request: Agentrepl_V1_WatchDesktopBannersRequest) -> AsyncThrowingStream<Agentrepl_V1_WatchDesktopBannersResponse, Error>
    /// Answers one post.
    func reportPosted(_ request: Agentrepl_V1_ReportDesktopBannerPostedRequest) async throws -> Agentrepl_V1_ReportDesktopBannerPostedResponse
    /// Reports one click.
    func reportClicked(_ request: Agentrepl_V1_ReportDesktopBannerClickedRequest) async throws -> Agentrepl_V1_ReportDesktopBannerClickedResponse
}

/// Builds the Daemon at one address.
public typealias DaemonDialer = @Sendable (String) -> Daemon

/// What the platform's notification center did with one banner.
public enum PostResult: Equatable, Sendable {
    case posted
    case notAllowed
    case failed(String)
}

/// The platform's notification center, as the relay posts through it.
public protocol BannerCenter: Sendable {
    /// Posts one banner, storing its click token for the click to return.
    func post(_ banner: Agentrepl_V1_DesktopBanner) async -> PostResult
}

/// The Daemon over Connect at one address.
public struct ConnectDaemon: Daemon {
    let client: ConnectClient
    /// How long a unary report may take: the daemon answers in milliseconds.
    static let reportTimeout: TimeInterval = 10

    public init(address: String) { client = ConnectClient(address: address) }

    public func watchBanners(_ request: Agentrepl_V1_WatchDesktopBannersRequest) -> AsyncThrowingStream<Agentrepl_V1_WatchDesktopBannersResponse, Error> {
        client.serverStream("WatchDesktopBanners", request)
    }
    public func reportPosted(_ request: Agentrepl_V1_ReportDesktopBannerPostedRequest) async throws -> Agentrepl_V1_ReportDesktopBannerPostedResponse {
        try await client.unary("ReportDesktopBannerPosted", request, timeout: Self.reportTimeout)
    }
    public func reportClicked(_ request: Agentrepl_V1_ReportDesktopBannerClickedRequest) async throws -> Agentrepl_V1_ReportDesktopBannerClickedResponse {
        try await client.unary("ReportDesktopBannerClicked", request, timeout: Self.reportTimeout)
    }
}

/// The operations the relay's records carry.
enum Op {
    static let stream = "notifier.relay.stream"
    static let post = "notifier.relay.post"
    static let click = "notifier.relay.click"
}

/// The app's whole job: hold the banner stream of whichever daemon is
/// advertised, post every banner it hands over and answer each, and report
/// each click.
///
/// ONE STREAM AT A TIME, TO THE ADVERTISED DAEMON. A new advertisement
/// replaces the stream; the same advertisement again changes nothing; no
/// advertisement holds none. A stream that ends is not redialed on its own: a
/// planned end (`ending`) is followed by the next daemon's advertisement, and
/// an unplanned one is an error the record states, recovered by the next
/// advertisement rather than by retrying an address that just failed.
public actor Relay {
    private let dial: DaemonDialer
    private let center: BannerCenter
    private let log: Logger
    private let pid: Int32
    private let superseded: @Sendable () -> Void

    private var advertisement: DaemonAdvertisement?
    private var serving: (address: String, task: Task<Void, Never>)?

    /// `superseded` is called when another app instance's stream took this
    /// one's place: the app exits rather than reconnecting.
    public init(dial: @escaping DaemonDialer, center: BannerCenter, log: Logger, pid: Int32, superseded: @escaping @Sendable () -> Void) {
        self.dial = dial
        self.center = center
        self.log = log
        self.pid = pid
        self.superseded = superseded
    }

    /// The address the relay holds a stream to now, if any.
    public var servingAddress: String? { serving?.address }

    /// Takes a new advertisement: opens the stream to it, replacing any other.
    public func advertised(_ next: DaemonAdvertisement?) {
        advertisement = next
        guard let next else {
            if let serving {
                log.info(Op.stream, "the daemon withdrew its advertisement; dropping its banner stream", ["address": serving.address])
                serving.task.cancel()
                self.serving = nil
            } else {
                log.info(Op.stream, "no daemon is advertised; waiting for one", [:])
            }
            return
        }
        if serving?.address == next.address {
            return
        }
        serving?.task.cancel()
        let daemon = dial(next.address)
        let address = next.address
        let task = Task { await self.serve(daemon, address: address) }
        serving = (address, task)
    }

    /// Serves one stream until it ends.
    private func serve(_ daemon: Daemon, address: String) async {
        var request = Agentrepl_V1_WatchDesktopBannersRequest()
        request.process.pid = pid
        log.info(Op.stream, "opening the banner stream", ["address": address])
        var planned = false
        do {
            for try await frame in daemon.watchBanners(request) {
                switch frame.frame {
                case .attached:
                    log.info(Op.stream, "the daemon attached the banner stream; banners post from here on", ["address": address])
                case .banner(let banner):
                    await post(banner, to: daemon)
                case .superseded:
                    log.info(Op.stream, "another Zettel instance took the banner stream; this one exits", ["address": address])
                    ended(address)
                    superseded()
                    return
                case .ending:
                    planned = true
                    log.info(Op.stream, "the daemon is standing down; awaiting the next advertised daemon", ["address": address])
                case nil:
                    log.error(Op.stream, "the daemon sent a frame this app does not know; it was skipped", ["address": address])
                }
            }
            // A REPLACED STREAM ENDS QUIETLY: cancelling its task finishes the
            // iteration with no frame and no error, which is not the daemon
            // ending it.
            if Task.isCancelled {
                log.debug(Op.stream, "the banner stream was replaced", ["address": address])
            } else if !planned {
                log.error(Op.stream, "the banner stream ended without the daemon's planned ending; no banners post until a daemon is advertised again", ["address": address])
            }
        } catch is CancellationError {
            log.debug(Op.stream, "the banner stream was replaced", ["address": address])
        } catch {
            if Task.isCancelled {
                log.debug(Op.stream, "the banner stream was replaced", ["address": address])
            } else {
                log.error(Op.stream, "the banner stream failed; no banners post until a daemon is advertised again", ["address": address, "cause": "\(error)"])
            }
        }
        ended(address)
    }

    /// Forgets the stream to `address` once it ends, unless another replaced it.
    private func ended(_ address: String) {
        if serving?.address == address {
            serving = nil
        }
    }

    /// Posts one banner and answers it.
    private func post(_ banner: Agentrepl_V1_DesktopBanner, to daemon: Daemon) async {
        let id = banner.postID.value
        let result = await center.post(banner)
        var request = Agentrepl_V1_ReportDesktopBannerPostedRequest()
        request.postID = banner.postID
        switch result {
        case .posted:
            request.outcome.posted = Agentrepl_V1_DesktopBannerPostOutcomePosted()
            log.info(Op.post, "posted a banner", ["post": id, "group": banner.group.id])
        case .notAllowed:
            request.outcome.notAllowed = Agentrepl_V1_DesktopBannerPostOutcomeNotAllowed()
            log.error(Op.post, "macOS refused the banner: Zettel is not allowed to notify (System Settings → Notifications → Zettel)", ["post": id])
        case .failed(let platformError):
            request.outcome.failed.platformError = platformError
            log.error(Op.post, "macOS refused the banner", ["post": id, "cause": platformError])
        }
        do {
            let response = try await daemon.reportPosted(request)
            switch response.result {
            case .success:
                return
            case .error(let refusal) where refusal.cause == .unknownPost(.init()):
                log.info(Op.post, "the daemon no longer awaited the post's answer", ["post": id])
            default:
                log.error(Op.post, "the daemon answered the post's report with nothing this app knows", ["post": id])
            }
        } catch {
            log.error(Op.post, "the post's answer did not reach the daemon", ["post": id, "cause": "\(error)"])
        }
    }

    /// Reports one click. `token` is the click token's serialized bytes, as
    /// stored on the banner. The click is reported to the advertised daemon,
    /// and to the address it names when it has handed the workspace on.
    public func clicked(token: Data) async {
        let click: Agentrepl_V1_DesktopBannerClickToken
        do {
            click = try Agentrepl_V1_DesktopBannerClickToken(serializedBytes: token)
        } catch {
            log.error(Op.click, "a clicked banner carried no readable click token", ["cause": "\(error)"])
            return
        }
        let workspace = click.workspace.id
        guard let address = advertisement?.address else {
            log.error(Op.click, "a banner was clicked while no daemon is advertised; it selects nothing", ["workspace": workspace])
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
            log.error(Op.click, "the click did not reach the daemon", ["workspace": workspace, "address": address, "cause": "\(error)"])
            return
        }
        switch response.result {
        case .success:
            log.info(Op.click, "reported a banner's click; Emacs selects its workspace", ["workspace": workspace, "address": address])
        case .error(let refusal):
            switch refusal.cause {
            case .transferringAway(let away) where !redirected:
                log.info(Op.click, "the daemon handed the workspace on; reporting the click to its successor", ["workspace": workspace, "address": away.address])
                await report(request, to: away.address, workspace: workspace, redirected: true)
            case .notYetAdopted(let pending) where !redirected && pending.hasAddress:
                log.info(Op.click, "the workspace is not yet adopted; reporting the click to the daemon serving it", ["workspace": workspace, "address": pending.address])
                await report(request, to: pending.address, workspace: workspace, redirected: true)
            default:
                log.error(Op.click, "the daemon refused the click; it selects nothing", ["workspace": workspace, "address": address, "refusal": "\(refusal)"])
            }
        case nil:
            log.error(Op.click, "the daemon answered the click with nothing this app knows", ["workspace": workspace])
        }
    }
}
