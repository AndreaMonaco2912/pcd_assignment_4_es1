package assignment4

import Command.ExitTimer

import com.typesafe.config.ConfigFactory
import org.apache.pekko.actor.typed.*
import org.apache.pekko.actor.typed.receptionist.{Receptionist, ServiceKey}
import org.apache.pekko.actor.typed.scaladsl.*

import scala.concurrent.duration.DurationInt

enum Command extends CborSerializable:
  case EnterPin(pin: String)
  case ArmingPin(pin: String, zones: Set[String])
  case SensorDetection(zone: String)
  case ExitTimer
  case EnterTimer
  case AcuResolved(listing: Receptionist.Listing)

enum SensorType:
  case Door
  case Window

object Sensor:

  import Command.{AcuResolved, SensorDetection}

  def apply(zone: String): Behavior[Command] = Behaviors.setup: ctx =>
    val alarmControlUnitServiceKey = SmartAlarmControlUnit.getServiceKeyForZone(zone)
    val listingAdapter = ctx.messageAdapter[Receptionist.Listing](AcuResolved.apply)
    ctx.system.receptionist ! Receptionist.Subscribe(alarmControlUnitServiceKey, listingAdapter)
    resolve(ctx, zone)

  private def resolve(ctx: ActorContext[Command], zone: String): Behavior[Command] =
    Behaviors.receiveMessagePartial:
      case AcuResolved(listing) =>
        listing.serviceInstances(SmartAlarmControlUnit.getServiceKeyForZone(zone)).headOption match
          case Some(acu) => detection(ctx, acu, zone)
          case None => Behaviors.same

  private def detection(ctx: ActorContext[Command], acu: ActorRef[Command], zone: String): Behavior[Command] =
    Behaviors.withTimers: timers =>
      val exitDuration = scala.util.Random.nextInt(20).seconds
      timers.startSingleTimer(ExitTimer, exitDuration)
      exitTimer(ctx, acu, zone)

  private def exitTimer(ctx: ActorContext[Command], acu: ActorRef[Command], zone: String): Behavior[Command] = Behaviors.receiveMessagePartial:
    case Command.ExitTimer =>
      ctx.log.info(s"Sending detection for zone $zone to ACU $acu")
      acu ! SensorDetection(zone)
      detection(ctx, acu, zone)


object SmartAlarmControlUnit:

  export Command.*

  val correctPin = "1234"
  private val exitDuration = 1.seconds
  private val enterDuration = 3.seconds

  private val allZones = Set("Hall", "Kitchen", "BedRoom")
  private var armedZones = Set.empty[String]

  def apply(zones: Set[String]): Behavior[Command] =
    Behaviors.setup: ctx =>
      zones.foreach: zone =>
        ctx.system.receptionist ! Receptionist.Register(
          SmartAlarmControlUnit.getServiceKeyForZone(zone),
          ctx.self
        )
      disarmed(ctx)

  def getServiceKeyForZone(zone: String): ServiceKey[Command] =
    ServiceKey(s"alarm-control-unit-zone-$zone")

  private def disarmed(ctx: ActorContext[Command]): Behavior[Command] = Behaviors.receiveMessagePartial:
    case Command.EnterPin(pin) if pin == correctPin =>
      ctx.log.info("D-Correct pin, all zones are armed. You have: " + exitDuration + " to leave your home.")
      armedZones = allZones
      Behaviors.withTimers: timers =>
        timers.startSingleTimer(ExitTimer, exitDuration)
        exitTimer(ctx)
    case Command.ArmingPin(pin, zones) if pin == correctPin =>
      ctx.log.info("D-Correct pin, armed zones are: " + zones + ".")
      armedZones = zones
      Behaviors.withTimers: timers =>
        timers.startSingleTimer(ExitTimer, exitDuration)
        exitTimer(ctx)
    case Command.EnterPin(_) | Command.ArmingPin(_, _) =>
      ctx.log.info("D-Wrong pin.")
      Behaviors.same
    case _ =>
      Behaviors.same

  private def exitTimer(ctx: ActorContext[Command]): Behavior[Command] = Behaviors.receiveMessagePartial:
    case Command.EnterPin(pin) if pin == correctPin =>
      ctx.log.info("EXT-System disarmed successfully.")
      disarmed(ctx)
    case Command.EnterPin(_) =>
      ctx.log.info("EXT-Wrong pin.")
      Behaviors.same
    case Command.ExitTimer =>
      ctx.log.info("EXT-Now actually armed.")
      armed(ctx)
    case _ =>
      Behaviors.same

  private def armed(ctx: ActorContext[Command]): Behavior[Command] = Behaviors.receiveMessagePartial:
    case Command.EnterPin(pin) if pin == correctPin =>
      ctx.log.info("A-Correct pin. Or from disabled zone or some sensor could be broken.")
      disarmed(ctx)
    case Command.EnterPin(_) =>
      ctx.log.info("A-Wrong pin. No detection, so some sensor could be broken.")
      alarm(ctx)
    case Command.SensorDetection(zone) if armedZones.contains(zone) =>
      ctx.log.info("A-Movement detected.")
      Behaviors.withTimers: timers =>
        timers.cancelAll()
        timers.startSingleTimer(EnterTimer, enterDuration)
        enterTimer(ctx)
    case Command.SensorDetection(_) =>
      ctx.log.info("A-Movement detected from no armed zone. No intrusion.")
      Behaviors.same
    case _ =>
      Behaviors.same

  private def enterTimer(ctx: ActorContext[Command]): Behavior[Command] = Behaviors.receiveMessagePartial:
    case Command.EnterPin(pin) if pin == correctPin =>
      ctx.log.info("ENT-Correct pin. Alarm disabled.")
      disarmed(ctx)
    case Command.EnterPin(_) =>
      ctx.log.info("ENT-Wrong pin.")
      Behaviors.same
    case Command.EnterTimer =>
      ctx.log.info("ENT-ALARM.")
      alarm(ctx)
    case _ =>
      Behaviors.same

  private def alarm(ctx: ActorContext[Command]): Behavior[Command] = Behaviors.receiveMessagePartial:
    case Command.EnterPin(pin) if pin == correctPin =>
      ctx.log.info("AL-Alarm disabled.")
      disarmed(ctx)
    case Command.EnterPin(_) =>
      ctx.log.info("AL-Wrong pin. Keep ALARM.")
      Behaviors.same
    case _ =>
      Behaviors.same

@main def spawnAlarmControlUnit(): Unit =
  val config = ConfigFactory.load("application.conf")
  val zones: Set[String] = sys.env("ZONES").split(",").toSet
  val _ = ActorSystem[Command](SmartAlarmControlUnit(zones), "ClusterSystem", config)

@main def spawnSensor(): Unit =
  val config = ConfigFactory.load("application.conf")
  val zone: String = sys.env("ZONE")
  val _ = ActorSystem[Command](Sensor(zone), "ClusterSystem", config)