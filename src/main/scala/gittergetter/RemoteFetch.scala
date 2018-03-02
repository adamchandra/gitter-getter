package gittergetter

import io.circe, circe._
import circe.generic.auto._

import ammonite.{ops => fs}

import scalaj.http._

import RoomManifest._

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

  def roomMessages(
    roomName: String,
    roomId: String,
    from: Option[String],
    fetchForward: Boolean,
    count: Int
  ): (List[MessageSchema], Json) = {

    Thread.sleep(2000)

    val dirParam = if (fetchForward) "afterId" else "beforeId"
    val dir = if (fetchForward) "forward" else "backward"

    println(s"Fetching ${count} messages (${dir}) from ${roomName}")
    val uri = from.map{ f =>
      apiCall(s"rooms/${roomId}/chatMessages?${dirParam}=${f}&limit=${count}")
    } getOrElse {
      apiCall(s"rooms/${roomId}/chatMessages?limit=${count}")
    }

    val resp = Http(uri)
      .headers(Seq("Authorization" -> s"Bearer $token"))
      .asString.body

    val asJson = JsonSchema.getJsonOrDie(resp)
    val prevJsons = JsonSchema.getOrDie[List[Json]](resp)
    val parsed = prevJsons.map{ js =>
      JsonSchema.fromJsonOrDie[MessageSchema](js)
    }
    // (JsonSchema.fromJsonOrDie[List[MessageSchema]](asJson), asJson)
    (parsed, asJson)
  }

  def roomSchemas(): List[RoomSchema] = {
    val response: HttpResponse[String] =
      Http("https://api.gitter.im/v1/rooms")
        .headers(Seq("Authorization" -> s"Bearer $token"))
        .asString

    JsonSchema.getOrDie[List[RoomSchema]](response.body)
  }
}
