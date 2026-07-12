package assignment4

import org.apache.pekko.actor.testkit.typed.scaladsl.{LoggingTestKit, ScalaTestWithActorTestKit}
import org.scalatest.wordspec.AnyWordSpecLike

class SmartAlarmSystemTest extends ScalaTestWithActorTestKit with AnyWordSpecLike {

  import SmartAlarmControlUnit.*

  "The SmartAlarmSystem" should {

    "reject an incorrect PIN when disarmed" in {
      val alarm = spawn(SmartAlarmControlUnit())
      val wrongPin = "2222"
      LoggingTestKit.info("D-Wrong pin.").expect {
        alarm ! EnterPin(wrongPin)
      }
    }

    "arm successfully and activate sensors after ExitTimer" in {
      val alarm = spawn(SmartAlarmControlUnit())

      LoggingTestKit.info("D-Correct pin, all zones are armed. You have: 1 second to leave your home.").expect {
        alarm ! EnterPin(correctPin)
      }

      alarm ! ExitTimer

      LoggingTestKit.info("A-Movement detected.").expect {
        alarm ! SensorDetection("Hall")
      }
    }

    "ignore sensor detections during the exit delay phase" in {
      val alarm = spawn(SmartAlarmControlUnit())
      alarm ! EnterPin(correctPin)

      alarm ! SensorDetection("Hall")

      LoggingTestKit.info("EXT-System disarmed successfully.").expect {
        alarm ! EnterPin(correctPin)
      }
    }

    "allow disarming during the Entry Delay" in {
      val alarm = spawn(SmartAlarmControlUnit())
      alarm ! EnterPin(correctPin)
      alarm ! ExitTimer

      alarm ! SensorDetection("BedRoom")

      LoggingTestKit.info("ENT-Correct pin. Alarm disabled.").expect {
        alarm ! EnterPin(correctPin)
      }
    }

    "sound the alarm if PIN is not entered before EnterTimer expires" in {
      val alarm = spawn(SmartAlarmControlUnit())
      val wrongPin = "2222"

      alarm ! EnterPin(correctPin)
      alarm ! ExitTimer

      alarm ! SensorDetection("Kitchen")

      LoggingTestKit.info("ENT-ALARM.").expect {
        alarm ! EnterTimer
      }

      LoggingTestKit.info("AL-Wrong pin. Keep ALARM.").expect {
        alarm ! EnterPin(wrongPin)
      }
    }

    "support partial arming by ignoring inactive zones" in {
      val alarm = spawn(SmartAlarmControlUnit())

      // Arm only the "Hall"
      LoggingTestKit.info("D-Correct pin, armed zones are: Set(Hall).").expect {
        alarm ! ArmingPin(correctPin, Set("Hall"))
      }

      alarm ! ExitTimer

      LoggingTestKit.info("A-Movement detected from no armed zone. No intrusion.").expect {
        alarm ! SensorDetection("BedRoom")
      }
    }
  }
}