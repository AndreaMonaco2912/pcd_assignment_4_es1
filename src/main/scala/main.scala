package assignment3

import SmartAlarmSystem.Command

import org.apache.pekko.actor.typed.*
import org.apache.pekko.actor.typed.scaladsl.*

import scala.concurrent.duration.DurationInt

@main
def main(): Unit =
  val system = ActorSystem(SmartAlarmSystem(), "SmartAlarmSystem")
  system ! Command.SensorDetection("Hall")
  Thread.sleep(1000)

  system ! Command.EnterPin("222")
  Thread.sleep(1000)

  system ! Command.EnterPin("1234")// exit house arming (timer started)
  system ! Command.EnterPin("1234")//disarmed immediately
  system ! Command.EnterPin("1234")//rearmed
  Thread.sleep(1200)

  system ! Command.SensorDetection("Hall")

  system ! Command.EnterPin("222")// typo in pin
  system ! Command.EnterPin("1234")// correct pin

  Thread.sleep(500)

  system ! Command.EnterPin("1234") //exit again
  Thread.sleep(1500) //system armed

  system ! Command.SensorDetection("Hall")
  Thread.sleep(2000)
  system ! Command.EnterPin("222") //should not nino
  Thread.sleep(2000) // should nino
  system ! Command.EnterPin("222")
  system ! Command.EnterPin("1234")

object SmartAlarmSystem:
  enum Command:
    case EnterPin(pin: String)
    case ArmingPin(pin: String, zones: Set[String])
    case SensorDetection(zone: String)
    case ExitTimer
    case EnterTimer

  export Command.*

  val correctPin = "1234"
  val exitDuration = 1.seconds
  val enterDuration = 3.seconds

  val allZones = Set("Hall", "Kitchen", "BedRoom")
  var armedZones = Set.empty[String]

  def apply(): Behavior[Command] = Behaviors.setup: ctx =>
    disarmed(ctx)

  def disarmed(ctx: ActorContext[Command]): Behavior[Command] = Behaviors.receiveMessagePartial:
    case Command.EnterPin(pin) if pin == correctPin =>
      ctx.log.info("Correct pin, fully armed House! You have: " + exitDuration + " to leave your home")
      armedZones = allZones
      Behaviors.withTimers: timers =>
        timers.startSingleTimer(ExitTimer, exitDuration)
        exitTimer(ctx)
    case Command.ArmingPin(pin, zones) if pin == correctPin =>
      ctx.log.info("Correct pin, armed zones are: " + zones)
      armedZones = zones
      Behaviors.withTimers: timers =>
        timers.startSingleTimer(ExitTimer, exitDuration)
        exitTimer(ctx)
    case Command.EnterPin(_) | Command.ArmingPin(_, _) =>
      ctx.log.info("WROOOOONG")
      Behaviors.same
    case _ =>
      Behaviors.same

  def exitTimer(ctx: ActorContext[Command]): Behavior[Command] = Behaviors.receiveMessagePartial:
    case Command.EnterPin(pin) if pin == correctPin =>
      ctx.log.info("System disarmed successfully!")
      disarmed(ctx)
    case Command.EnterPin(_) =>
      ctx.log.info("WROOOOONG")
      Behaviors.same
    case Command.ExitTimer =>
      ctx.log.info("House armed and Dangerous")
      armed(ctx)
    case _ =>
      Behaviors.same

  def armed(ctx: ActorContext[Command]): Behavior[Command] = Behaviors.receiveMessagePartial:
    case Command.EnterPin(pin) if pin == correctPin =>
      ctx.log.info("Correct pin from disabled Zone!!!")
      disarmed(ctx)
    case Command.EnterPin(_) =>
      ctx.log.info("Wrong Pin Fast to Alarm!!")
      alarm(ctx)
    case Command.SensorDetection(zone) if armedZones.contains(zone) =>
      ctx.log.info("Movement detected!!!")
      Behaviors.withTimers: timers =>
        timers.cancelAll()
        timers.startSingleTimer(EnterTimer, enterDuration)
        enterTimer(ctx)
    case _ =>
      Behaviors.same

  def enterTimer(ctx: ActorContext[Command]): Behavior[Command] = Behaviors.receiveMessagePartial:
    case Command.EnterPin(pin) if pin == correctPin =>
      ctx.log.info("Correct Pin alarm disabled")
      //stop timer
      disarmed(ctx)
    case Command.EnterPin(_) =>
      ctx.log.info("WROOOOONG")
      Behaviors.same
    case Command.EnterTimer =>
      ctx.log.info("NINONINONINONINO")
      alarm(ctx)
    case _ =>
      Behaviors.same

  def alarm(ctx: ActorContext[Command]): Behavior[Command] = Behaviors.receiveMessagePartial:
    case Command.EnterPin(pin) if pin == correctPin =>
      ctx.log.info("Welcome Back, alarm disabled, as you can here")
      disarmed(ctx)
    case Command.EnterPin(_) =>
      ctx.log.info("WROOOOOOOOONG ULTRA MEGA SUPER NINONINONINONINO")
      Behaviors.same
    case _ =>
      Behaviors.same