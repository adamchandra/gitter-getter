package gittergetter

import io.circe, circe._
import circe.syntax._
import java.time.{ Instant, ZoneId }

import ammonite.{ops => fs}
import fs._


import RoomManifest._
import java.time.format.{ DateTimeFormatter, FormatStyle }
import java.util.Locale
import scala.annotation.tailrec
import java.time.Duration


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

object getGitter extends App {
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

    readArchivedMessagesRange(room) match {

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


  def formatInstant(i: Instant): String = {
    val formatter =
      DateTimeFormatter.ofLocalizedDateTime( FormatStyle.SHORT )
        .withLocale( Locale.US )
        .withZone( ZoneId.systemDefault() );
    formatter.format(i)
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

  def readArchivedMessagesRange(room: RoomSchema): Option[(MessageSchema, MessageSchema)] = {
    val jsonArchives = localPaths.jsonArchiveDir(room.name)

    val messagesWithTimestamps = ls(jsonArchives)
      .filter( _.name.endsWith(".json") )
      .map{ file =>
        val existingFile = fs.read(file)
        val prevJsons = JsonSchema.getOrDie[List[Json]](existingFile)
        prevJsons.map{ js => JsonSchema.fromJsonOrDie[MessageSchema](js) }
      }

    val sorted = messagesWithTimestamps.flatten.sortBy(_.sent)

    if (sorted.isEmpty) None else {
      val first = sorted.head
      val last = sorted.last
      val from = formatInstant(first.sent)
      val to =  formatInstant(last.sent)
      println(s"Known messages: From ${from} to ${to} ")
      Some((first, last))
    }
  }

  def main(): Unit = {

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

  main()

}



  // def findMessagesRange(m: Seq[MessageSummary]): Option[(MessageSummary, MessageSummary)]  = {
  //   if (m.isEmpty) None else {
  //     val sorted = m.sortBy(_.sent)
  //     val first = sorted.head
  //     val last = sorted.last
  //     val len = sorted.length
  //     val from = formatInstant(first.sent)
  //     val to =  formatInstant(last.sent)
  //     // first.sent.formatted(fmtstr: String)
  //     println(s"Message (${len}): From ${from} to ${to} ")
  //     Some((first, last))
  //   }
  // }

  // def findMessagesRange(m: Seq[MessageSchema]): Option[(MessageSchema, MessageSchema)]  = {
  //   if (m.isEmpty) None else {
  //     val sorted = m.sortBy(_.sent)
  //     val first = sorted.head
  //     val last = sorted.last
  //     val len = sorted.length
  //     val from = formatInstant(first.sent)
  //     val to =  formatInstant(last.sent)
  //     // first.sent.formatted(fmtstr: String)
  //     println(s"Message (${len}): From ${from} to ${to} ")
  //     Some((first, last))
  //   }
  // }
