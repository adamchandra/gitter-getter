package gittergetter

import io.circe, circe._
import circe.generic.auto._

import ammonite.{ops => fs}

import scalaj.http._
import InstantEncoders._


object RemoteFetch {

  def apiCall(path: String): String = {
    s"https://api.gitter.im/v1/${path}"
  }


  def getToken() = {
    val authKeyVal = fs.read(fs.pwd / "secrets.conf").split("\n")
      .map(_.trim)
      .filter(_.startsWith("authkey"))
      .headOption.getOrElse {
        sys.error("no authkey found in secrets.conf")
      }

    authKeyVal.split('=')(1).trim()
  }

  lazy val token = getToken()

  def roomMessages(roomName: String, roomId: String, from: Option[String], count: Int): (List[MessageSchema], Json) = {
    println(s"Getting messages from ${from} len:${count}")
    val uri = from.map{ f =>
      apiCall(s"rooms/${roomId}/chatMessages?beforeId=${f}&limit=${count}")
    } getOrElse {
      apiCall(s"rooms/${roomId}/chatMessages?limit=${count}")
    }

    val resp = Http(uri)
      .headers(Seq("Authorization" -> s"Bearer $token"))
      .asString.body

    val asJson = JsonSchema.getJsonOrDie(resp)
    (JsonSchema.fromJsonOrDie[List[MessageSchema]](asJson), asJson)
  }

  def roomSchemas(): List[RoomSchema] = {
    val response: HttpResponse[String] =
      Http("https://api.gitter.im/v1/rooms")
        .headers(Seq("Authorization" -> s"Bearer $token"))
        .asString

    JsonSchema.getOrDie[List[RoomSchema]](response.body)
  }
}
