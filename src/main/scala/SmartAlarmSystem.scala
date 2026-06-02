package assignment4

import SmartAlarmSystem.{Command, correctPin}

import SmartAlarmSystem.Command.{ArmingPin, EnterPin, SensorDetection}
import org.apache.pekko.actor.typed.*
import org.apache.pekko.actor.typed.scaladsl.*

import scala.concurrent.duration.DurationInt

@main
def main(): Unit =
  val wrongPin = "2222"
  val system = ActorSystem(SmartAlarmSystem(), "SmartAlarmSystem")

  system ! ArmingPin(correctPin, Set("Kitchen"))
  Thread.sleep(2000)
  //No armed zone
  system ! SensorDetection("Bedroom")
  //Armed zone
  system ! SensorDetection("Kitchen")
  system ! EnterPin(correctPin)
  Thread.sleep(2000)

  // Now arm all house
  system ! EnterPin(correctPin)
  Thread.sleep(2000)
  system ! SensorDetection("Kitchen")
  Thread.sleep(5000)
  system ! EnterPin(wrongPin)
  system ! EnterPin(correctPin)

object SmartAlarmSystem:
  enum Command:
    case EnterPin(pin: String)
    case ArmingPin(pin: String, zones: Set[String])
    case SensorDetection(zone: String)
    case ExitTimer
    case EnterTimer

  export Command.*

  val correctPin = "1234"
  private val exitDuration = 1.seconds
  private val enterDuration = 3.seconds

  private val allZones = Set("Hall", "Kitchen", "BedRoom")
  private var armedZones = Set.empty[String]

  def apply(): Behavior[Command] = Behaviors.setup: ctx =>
    disarmed(ctx)

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