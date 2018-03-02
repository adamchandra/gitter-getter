package gittergetter

import io.circe, circe._
import circe.syntax._
import circe.generic.auto._
import java.time.Instant

import ammonite.{ops => fs}
import fs._

import InstantEncoders._
object TagTypes {
  import scalaz._
  // import Scalaz._

  type @@[A, B] = scalaz.@@[A, B]

  sealed trait MessageID
  val MessageID = Tag.of[MessageID]
}
import TagTypes._



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
    write(file, j.spaces4)
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


  // val initManifest = RoomManifest(
  //   None, None, Instant.now()
  // )

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



  def fetchNextMessageBatch(room: RoomSchema, startingId: String, fetchUntil: Instant): Option[String] = {

    val (messages, srcJson) = RemoteFetch.roomMessages(room.name, room.id, Some(startingId), 100)

    val sortedMessages = messages
      .sortBy{ m => m.sent.toEpochMilli() }
      .filter(m => m.sent.isAfter(fetchUntil))

    if (sortedMessages.nonEmpty) {
      localPaths.addJsonContent(room.name, srcJson)
      // Sorted from earliest to latest
      sortedMessages.foreach { m =>
        println(s"Sent: ${m.sent}: ${m.text.slice(0, 10)}...")
      }
      // sortedMessages.headOption.map { m =>
      //   localPaths.updateManifest(room.name,
      //     roomManifest.copy(
      //       earliestMessageTime = Some(m.sent),
      //       earliestMessageId = Some(m.id),
      //       lastUpdate =  Instant.now()
      //     )
      //   )
      // }
      sortedMessages.headOption.map { _.id }
    } else {
      None
    }

  }

  def updateRoomJsons(room: RoomSchema): Unit = {
    println(s"Updating room ${room.name}")

    // readArchivedJsons(room)

    val (mostRecentMessageList, sourceJson) = RemoteFetch.roomMessages(room.name, room.id, None, 1)

    mostRecentMessageList.headOption.foreach { mostRecentMessage =>

      val roomManifest = localPaths.getRoomManifest(room).getOrElse {
        val sent = mostRecentMessage.sent
        val id = mostRecentMessage.id
        localPaths.addJsonContent(room.name, sourceJson)
        localPaths.updateManifest(room.name,
          RoomManifest(
            earliestMessageTime = sent,
            earliestMessageId   = id,
            latestMessageTime   = sent,
            latestMessageId     = id,
            lastUpdate          = Instant.MIN
          )
        )
      }


      while (earliestFetched.isDefined) {
        earliestFetched = fetchNextMessageBatch(room, manifest.latestMessageId, manifest.latestMessageTime)
      }
    }

  }




  def readArchivedJsons(room: RoomSchema): Seq[(String, Instant)] = {
    val jsonArchives = localPaths.jsonArchiveDir(room.name)
    val messagesWithTimestamps = ls(jsonArchives)
      .filter( _.name.endsWith(".json") )
      .map{ file =>
        val existingFile = fs.read(file)

        println("Parsing Previous Jsons")
        val prevJsons = JsonSchema.getOrDie[List[Json]](existingFile)

        println("Parsing Previous Messages")

        prevJsons
          .map{ js => JsonSchema.fromJsonOrDie[MessageSchema](js) }
          .map{ messageSchema =>
            (messageSchema.id, messageSchema.sent)
          }
      }
    for {
      msgs <- messagesWithTimestamps
      (m, t) <- msgs
    } {
      println(s"${t}  Message ${m}")
    }

    messagesWithTimestamps.flatten
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

