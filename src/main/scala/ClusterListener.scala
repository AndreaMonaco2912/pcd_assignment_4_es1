package assignment4

import org.apache.pekko.actor.typed.*
import org.apache.pekko.actor.typed.scaladsl.*
import org.apache.pekko.cluster.ClusterEvent.*
import org.apache.pekko.cluster.typed.Subscribe
import org.apache.pekko.cluster.typed.Cluster

object ClusterListener:
  enum Event:
    case ReachabilityChange(event: ReachabilityEvent)
    case MemberChange(event: MemberEvent)

  export Event.*

  def apply(): Behavior[Event] = Behaviors.setup: ctx =>
    val memberEventAdapter = ctx.messageAdapter(MemberChange.apply)
    val cluster = Cluster(ctx.system)
    cluster.subscriptions ! Subscribe(memberEventAdapter, classOf[MemberEvent])
    val reachabilityEventAdapter = ctx.messageAdapter(ReachabilityChange.apply)
    cluster.subscriptions ! Subscribe(reachabilityEventAdapter, classOf[ReachabilityEvent])
    Behaviors.receiveMessage: message =>
      message match
        case ReachabilityChange(event) =>
          event match
            case UnreachableMember(member) =>
              ctx.log.info(s"Member ${member.address} is unreachable")
            case ReachableMember(member) =>
              ctx.log.info(s"Member ${member.address} is reachable again")
        case MemberChange(event) =>
          event match
            case MemberJoined(member) =>
              ctx.log.info(s"Member ${member.address} joined the cluster")
            case MemberUp(member) =>
              ctx.log.info(s"Member ${member.address} is up")
            case MemberRemoved(member, previousStatus) =>
              ctx.log.info(s"Member ${member.address} removed from cluster, previous status was $previousStatus")
            case MemberExited(member) =>
              ctx.log.info(s"Member ${member.address} exited the cluster")
            case MemberDowned(member) =>
              ctx.log.info(s"Member ${member.address} is down")
            case _ => ctx.log.info(s"Member event: $event")
      Behaviors.same
