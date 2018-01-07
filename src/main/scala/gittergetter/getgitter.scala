package gittergetter

import scalaj.http._

import io.circe, circe._
import circe.syntax._
import circe.generic.auto._
import circe.{parser => CirceParser}
// import circe.parser.decode
// import java.util.Date
import scala.collection.mutable
import ammonite.{ops => fs}

case class UserSchema(
  displayName     : String, // "Pelle Kr\u00f8gholt",
  id              : String, // "566fce4016b6c7089cbebd83",
  username        : String, // "pellekrogholt",
  // avatarUrl       : String, // "https://avatars-05.gitter.im/gh/uv/4/pellekrogholt",
  // avatarUrlMedium : String, // "https://avatars1.githubusercontent.com/u/62456?v=4&s=128",
  // avatarUrlSmall  : String, // "https://avatars1.githubusercontent.com/u/62456?v=4&s=60",
  // gv              : Int,
  // url             : String, // "/pellekrogholt",
  // v               : Int

)
case class UrlSchema(
)
case class IssueSchema(
  number: Int
)

case class MentionSchema(
  screenName: String,
  userId: Option[String],
  userIds: List[String]
)

case class MessageSchema(
  fromUser   : UserSchema,
  id         : String,
  issues     : Option[List[IssueSchema]],
  mentions   : List[MentionSchema],
  sent       : String,
  text       : String,
  urls       : List[UrlSchema]

  // highlights: List[String],
  // html       : String,
  // meta       : List[String],
  // readBy     : Int,
  // unread     : Boolean,
  // v          : Int
)

// [{
//   "id": "53307860c3599d1de448e19d",
//   "name": "Andrew Newdigate",
//   "topic": "",
//   "oneToOne": true,
//   "user": {
//     "id": "53307831c3599d1de448e19a",
//     "username": "suprememoocow",
//     "displayName": "Andrew Newdigate",
//     "url": "/suprememoocow",
//     "avatarUrlSmall": "https://avatars.githubusercontent.com/u/594566?",
//     "avatarUrlMedium": "https://avatars.githubusercontent.com/u/594566?"
//   },
//   "unreadItems": 0,
//   "mentions": 0,
//   "lurk": false,
//   "url": "/suprememoocow",
//   "githubType": "ONETOONE"
// }, {
case class RoomSchema(
  id          : String,  // 53307860c3599d1de448e19d,
  name        : String,  // Andrew Newdigate,
  topic       : String,  // ,
  oneToOne    : Boolean, // true,
  unreadItems : Int,     // 0,
  mentions    : Int,     // 0,
  lurk        : Boolean, // false,
  url         : String,  // /suprememoocow,
  githubType  : String,  // ONETOONE
)




object getGitter extends App {
  val PrettyPrint2Spaces = circe.Printer.spaces2

  val outputRoot = fs.pwd / "gitter-output.d"
  val roomsFile = outputRoot / "rooms.json"
  def roomDir(roomName: String): String = {
    roomName.replaceAll("/", "__")
  }

  def getMessagesFrom(roomName: String, roomId: String, from: Option[String], count: Int): List[MessageSchema] = {
    println(s"Getting messages from ${from} len:${count}")
    val uri = from.map{ f =>
      apiCall(s"rooms/${roomId}/chatMessages?beforeId=${f}&limit=${count}")
    } getOrElse {
      apiCall(s"rooms/${roomId}/chatMessages?limit=${count}")
    }

    val resp = Http(uri)
      .headers(Seq("Authorization" -> s"Bearer $token"))
      .asString.body


    getOrDie[List[MessageSchema]](resp)
  }

  def getOrDie[A: Decoder](astr: String): A = {
    CirceParser.parse(astr).fold(err => {
      sys.error(s"Json parse err: ${err}")
    }, succ => {
      succ.as[A].fold(err => {
        val jsStr = succ.pretty(PrettyPrint2Spaces)
        sys.error(s"Decode Err: ${err}, json was: \n${jsStr}")
      }, succ => succ)
    })
  }
  def fromJsonOrDie[A: Decoder](js: Json): A = {
    js.as[A].fold(err => {
      sys.error(s"Decode Err: ${err}, json was: \n${js}")
    }, succ => succ)
  }
  def getJsonOrDie(astr: String): Json = {
    CirceParser.parse(astr).fold(err => {
      sys.error(s"Json parse err: ${err}")
    }, succ => succ)
  }

  def apiCall(path: String): String = {
    val call = s"https://api.gitter.im/v1/${path}"
    println(s"API Call:  ${call}")
    call
  }

  val authKeyVal = fs.read(fs.pwd / "secrets.conf").split("\n")
    .map(_.trim)
    .filter(_.startsWith("authkey"))
    .headOption.getOrElse {
      sys.error("no authkey found in secrets.conf")
    }


  val token = authKeyVal.split('=')(1).trim()

  def fetchAllRooms(): Unit = {

    val response: HttpResponse[String] =
      Http("https://api.gitter.im/v1/rooms")
        .headers(Seq("Authorization" -> s"Bearer $token"))
        .asString

    val rooms = getOrDie[List[RoomSchema]](response.body)

    val roomNames = rooms.zipWithIndex.map { case (r, i) =>
      s"${i} ${r.name}"
    }.mkString("{\n  ", "\n  ", "\n}")

    println(s"Room Count = ${rooms.length}")
    println(roomNames)

    rooms.drop(16).take(1).foreach{ room =>
      fetchRoomMessages(room)
    }
  }

  def fetchRoomMessages(room: RoomSchema): Unit = {
    val roomd = outputRoot / roomDir(room.name)
    if (!fs.exists(roomd)) {
      fs.mkdir(roomd)
    }

    println(s"Fetching Room ${room.name} into ${roomd}")

    val msgFile = roomd / s"${roomDir(room.name)}-messages.json"
    val msgTextFile = roomd / s"${roomDir(room.name)}-messages.txt"

    val roomMessages = mutable.ArrayBuffer[MessageSchema]()
    val roomJsonMessages = mutable.ArrayBuffer[Json]()

    if (fs.exists(msgFile)) {
      val existingFile = fs.read(msgFile)
      // val prevJson = existingFile.asJson

      println("Parsing Previous Jsons")
      val prevJsons = getOrDie[List[Json]](existingFile)

      println("Parsing Previous Messages")
      val prevMsgs = prevJsons.map{ js =>
        fromJsonOrDie[MessageSchema](js)
      }
      roomMessages.appendAll(prevMsgs)
      roomJsonMessages.appendAll(prevJsons)
      println(s"Recovered ${roomJsonMessages.length} previous messages")
      println(s"Recovered ${roomMessages.length} previous messages")
    }

    def writeCurrentMessages(): Unit = {
      if (fs.exists(msgFile)) {
        fs.rm(msgFile)
      }
      val allJson =roomJsonMessages.toSeq.asJson
      fs.write(msgFile, allJson.pretty(PrettyPrint2Spaces) )
    }

    def loop(): Unit = {
      val lastMsgId = roomMessages.lastOption.map(_.id).get
      val uri = apiCall(s"rooms/${room.id}/chatMessages?beforeId=${lastMsgId}&limit=100")

      val resp = Http(uri).headers(Seq("Authorization" -> s"Bearer $token")).asString.body

      val jsons = getOrDie[List[Json]](resp)
      val msgs = jsons.map(fromJsonOrDie[MessageSchema](_))
      val msgsSent = msgs.reverse.map{  m =>
        s"${m.sent}: ${m.text.slice(0, 20)}"
      }.mkString("\n  ", "\n  ", "\n")

      println("Fetched")
      println(msgsSent)

      roomJsonMessages.appendAll(jsons.reverse)
      roomMessages.appendAll(msgs.reverse)

      writeCurrentMessages()
      Thread.sleep(1000)


      if (!msgs.isEmpty) loop()
    }

    if (roomMessages.isEmpty) {
      getMessagesFrom(room.name, room.id, None, 1).headOption.foreach{ m0 =>
        roomMessages.append(m0)
        loop()
      }
    } else loop()

    writeCurrentMessages()

    val msgChron = roomMessages.reverse.map{ msg =>
      val u = msg.fromUser.displayName + " @" + msg.fromUser.username
      val t = msg.sent
      val txt = msg.text
      (" "*150) + s"${u}@${t}\n" + s"${txt}\n"
    }.mkString("\n")

    if (fs.exists(msgTextFile)) {
      fs.rm(msgTextFile)
    }

    fs.write(msgTextFile, msgChron)

  }


  fetchAllRooms()

}
