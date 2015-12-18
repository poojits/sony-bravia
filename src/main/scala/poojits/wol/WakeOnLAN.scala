package poojits.wol

import java.net.{DatagramPacket, DatagramSocket, InetAddress, NetworkInterface}
import scala.collection.JavaConversions._

object WakeOnLAN {
  val WOL_PORT = 9

  def hex2bytes(hexText: String): Array[Byte] = hexText.sliding(2, 2).map(Integer.parseInt(_, 16).toByte).toArray

  def bytes2hex(bytes: Array[Byte]): String = bytes.map(b => f"$b%02X").mkString(":")

  def getMacAddress(macText: String): Option[Array[Byte]] = {
    hex2bytes( """([\dA-F]{2})""".r.findAllMatchIn(macText.toUpperCase).map(_.group(1)).mkString("")) match {
      case macBytes if macBytes.length == 6 => Some(macBytes)
      case _ => None
    }
  }

  def getBroadcastAddressList = {
    def getNicList = {
      def getSubNicList(nic: NetworkInterface): List[NetworkInterface] = {
        val subNicList = nic.getSubInterfaces.toList
        subNicList ::: subNicList.map(getSubNicList).flatten
      }
      val nicList = NetworkInterface.getNetworkInterfaces.toList
      nicList ::: nicList.map(getSubNicList).flatten
    }

    getNicList.filterNot(_.isLoopback).map(_.getInterfaceAddresses.map(_.getBroadcast).filterNot(_ == null)).flatten
  }

  def sendMagicPacket(macBytes: Array[Byte], address: InetAddress) {
    def generateMagicBytes(macBytes: Array[Byte]) = (List.fill(6)(0xFF.toByte) ::: List.fill(16)(macBytes.toList).flatten).toArray[Byte]

    def sendDatagramPacket(data: Array[Byte], address: InetAddress, port: Int) {
      val dPacket = new DatagramPacket(data, data.length, address, port)
      new DatagramSocket().send(dPacket)
    }

    val magicBytes = generateMagicBytes(macBytes)
    sendDatagramPacket(magicBytes, address, WOL_PORT)
  }

  def wake(macString: String) {
    val macBytesOption = getMacAddress(macString)
    macBytesOption match {
      case Some(mac) => {
        getBroadcastAddressList.map(address => {
          println(s"Broadcasting a magic packet: ${bytes2hex(mac)}$address")
          sendMagicPacket(mac, address)
        })
      }
      case None => {
        throw new IllegalArgumentException("Invalid MAC Address")
      }
    }
  }
}
