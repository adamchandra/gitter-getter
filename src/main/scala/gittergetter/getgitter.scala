package gittergetter

import io.circe, circe._
import circe.syntax._

import ammonite.{ops => fs}
import fs._


import RoomManifest._
import scala.annotation.tailrec
import java.time.Duration
import Formatting._

class LocalPaths(root: Path) {

  def roomDir(roomName: String): Path = {
    root / fs.RelPath(roomName
      .replaceAll("/", "__")
      .replaceAll(" ", "_")
    )
  }

  def jsonArchiveDir(roomName: String): fs.Path = {
    roomDir(roomName) / "json-archive"
  }
  def jsonArchiveFiles(room: String): Seq[Path] = {
    // Format is "0001-messages.json"
    ls(jsonArchiveDir(room))
      .filter(f => f.name.endsWith("messages.json"))
  }

  def fileNumber(p: Path): Int = {
    p.name.take(4).toInt
  }

  def getLatestArchiveFile(room: String): Option[Path] = {
    jsonArchiveFiles(room)
      .sortBy { fileNumber }
      .lastOption
  }

  def makeNextArchiveFile(room: String): Path = {
    val nextNumber = getLatestArchiveFile(room)
      .map{ p =>
        val next = fileNumber(p) + 1
        "%04d".format(next)
      }
      .getOrElse{ "0000" }
    jsonArchiveDir(room) / s"${nextNumber}-messages.json"
  }

  def addJsonContent(room: String, j: Json): fs.Path = {
    val file = makeNextArchiveFile(room)
    write(file, j.spaces2)
    file
  }

  def manifestFile(roomName: String): fs.Path = {
    roomDir(roomName) / "manifest.txt"
  }

  def updateManifest(roomName: String, manifest: RoomManifest): fs.Path = {
    val p = manifestFile(roomName)
    if (exists(p)) rm(p)
    fs.write(p, manifest.asJson.spaces4)
    p
  }

  def ensureRoomDirectories(roomSchema: RoomSchema): Unit = {
    val name = roomSchema.name
    val roomd = roomDir(name)
    if (!fs.exists(roomd)) {
      fs.mkdir(roomd)
      fs.mkdir(jsonArchiveDir(name))
    }
  }

  def getRoomManifest(room: RoomSchema): Option[RoomManifest] = {
    val p = manifestFile(room.name)
    if (exists(p)) {
      val str = fs.read(p)
      str.asJson.as[RoomManifest]
        .left.map { f => sys.error(s"Error decoding RoomManifest: ${f}\nsrc was:\n ${str}") }
        .toOption
    } else {
      None
    }
  }
}

import java.time.{ Instant }
object getGitter extends App with AppMainBasics {
  val PrettyPrint2Spaces = circe.Printer.spaces2

  val root: Path = fs.pwd / "gitter-output.d"
  val localPaths = new LocalPaths(root)

  val EarliestMessage = Instant.now().minus(Duration.ofDays(100))


  @tailrec
  def fetchBackwards(room: RoomSchema, fromMsg: MessageSchema): Unit = {
    if (fromMsg.sent.isBefore(EarliestMessage)) {
      println(s"Earliest fetch cutoff ${formatInstant(EarliestMessage)} exceeded.")
    } else {
      println(s"Fetching messages from ${room.name} prior to ${formatInstant(fromMsg.sent)}")
      val (fetched, sourceJson) = RemoteFetch.roomMessages(room.name, room.id, Some(fromMsg.id), fetchForward=false, 100)
      if (fetched.nonEmpty) {
        val first = fetched.sortBy(_.sent).head
        if (first.sent.isBefore(fromMsg.sent)) {
          localPaths.addJsonContent(room.name, sourceJson)
          fetchBackwards(room, first)
        }
      }
    }
  }

  @tailrec
  def fetchForwards(room: RoomSchema, fromMsg: MessageSchema): Unit = {
    println(s"Fetching messages from ${room.name} after ${formatInstant(fromMsg.sent)}")
    val (fetched, sourceJson) = RemoteFetch.roomMessages(room.name, room.id, Some(fromMsg.id), fetchForward=true, 100)
    if (fetched.nonEmpty) {
      val last = fetched.sortBy(_.sent).last
      if (last.sent.isAfter(fromMsg.sent)) {
        localPaths.addJsonContent(room.name, sourceJson)
        fetchForwards(room, last)
      }
    }
  }


  def updateRoomJsons(room: RoomSchema): Unit = {
    println(s"Updating room ${room.name}")

    findArchivedMessagesRange(room) match {

      case Some( (earliest, latest) ) =>
        if (earliest.sent.isAfter(EarliestMessage)) {
          fetchBackwards(room, earliest)
        }
        fetchForwards(room, latest)


      case None =>
        val (messages, sourceJson) = RemoteFetch.roomMessages(room.name, room.id, None, fetchForward=false, 100)
        if (messages.nonEmpty) {
          localPaths.addJsonContent(room.name, sourceJson)
          updateRoomJsons(room)
        }
    }
  }


  // def readArchivedMessages(room: RoomSchema): Seq[MessageSchema] = {
  def readArchivedMessages(roomName: String): Seq[MessageSchema] = {
    val jsonArchives = localPaths.jsonArchiveDir(roomName)

    val messagesWithTimestamps = ls(jsonArchives)
      .filter( _.name.endsWith(".json") )
      .map{ file =>
        val existingFile = fs.read(file)
        val prevJsons = JsonSchema.getOrDie[List[Json]](existingFile)
        prevJsons.map{ js => JsonSchema.fromJsonOrDie[MessageSchema](js) }
      }

    messagesWithTimestamps.flatten.sortBy(_.sent)
  }

  def sortMessageArchives(room: RoomSchema): Unit = {

    val jsonArchives = localPaths.jsonArchiveDir(room.name)

    val messagesWithTimestamps = ls(jsonArchives)
      .filter( _.name.endsWith(".json") )
      .map{ file =>
        val existingFile = fs.read(file)
        val prevJsons = JsonSchema.getOrDie[List[Json]](existingFile)
        val prev = prevJsons.map{ js => JsonSchema.fromJsonOrDie[MessageSchema](js) }.headOption
        prev.map{ m => (m.sent, file) }
      }

    val sorted = messagesWithTimestamps.flatten.sortBy(_._1)

    val tmpSortDir = localPaths.roomDir(room.name) / "tmp-sort"
    mkdir(tmpSortDir)
    sorted.zipWithIndex.foreach { case ((sent, file), i) =>
      val nexti = "%04d".format(i)
      val f = tmpSortDir / s"${nexti}-messages.json"
      mv(file, f)
    }

    rm(jsonArchives)

    mv(tmpSortDir, jsonArchives)
  }

  def findArchivedMessagesRange(room: RoomSchema): Option[(MessageSchema, MessageSchema)] = {
    val sorted = readArchivedMessages(room.name)

    if (sorted.isEmpty) None else {
      val first = sorted.head
      val last = sorted.last
      val from = formatInstant(first.sent)
      val to =  formatInstant(last.sent)
      println(s"Known messages: From ${from} to ${to} ")
      Some((first, last))
    }
  }

  val TB = TextBoxing

  def run(args: Array[String]): Unit = {
    val argMap = argsToMap(args.toArray)

    val doUpdate = hasArg("update", argMap)
    val doFormat = hasArg("format", argMap)

    if (doUpdate) {
      val rooms = RemoteFetch.roomSchemas()
      rooms.foreach { roomSchema =>
        val roomName = roomSchema.name
        val roomArchivePath = localPaths.roomDir(roomName)
        if (fs.exists(roomArchivePath)) {
          println(s"-----> ${roomName}")
        } else {
          println(s"(new)> ${roomName}")
          rooms.foreach{ r =>  localPaths.ensureRoomDirectories(roomSchema) }
        }
      }

      rooms.foreach{ room =>
        updateRoomJsons(room)
      }

    }

    if (doFormat) {
      println(s"starting format")

      val formatOneRoom = hasArg("room", argMap)
      if (formatOneRoom) {
        val room = getArg("room", argMap)
        println(s"Formatting ${room}")

        val messages = readArchivedMessages(room)
        val formattedMessages = messages.map{ messageSchema =>
          Formatting.formatMessage(messageSchema)
        }

        val formatOutput = TB.vjoins(
          formattedMessages
        )

        val formatted = localPaths.roomDir(room) / "formatted.txt"
        if (fs.exists(formatted)) {
          fs.rm(formatted)
        }

        fs.write(formatted, formatOutput.toString())

      }

    }

  }

  run(args)

}
