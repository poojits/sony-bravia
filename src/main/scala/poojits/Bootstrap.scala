package poojits

import wol._

object Bootstrap extends App {
  //TODO: Add auto discovery
  val macString = "FC:F1:52:E6:93:5F"
  WakeOnLAN.wake(macString)
}
