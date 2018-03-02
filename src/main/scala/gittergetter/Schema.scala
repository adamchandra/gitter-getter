package gittergetter

import java.time.Instant
import io.circe, circe._
import circe.generic.semiauto._
import circe.generic.auto._
import circe.{parser => CirceParser}



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
  sent       : Instant,
  text       : String,
  urls       : List[UrlSchema]
)

// case class MessageSummary(
//   id         : String,
//   sent       : Instant
// )

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


case class RoomManifest(
  earliestMessage: MessageSchema,
  latestMessage: MessageSchema
)


object RoomManifest {

  implicit def Encode_Instant: Encoder[Instant] =
    Encoder[String].contramap { _.toString }

  implicit def Decode_Instant: Decoder[Instant] =
    Decoder[String].map { Instant.parse(_) }

  // implicit def Encode_MessageSummary: Encoder[MessageSummary] = deriveEncoder
  // implicit def Decode_MessageSummary: Decoder[MessageSummary] = deriveDecoder
  implicit def Encode_MessageSchema: Encoder[MessageSchema] = deriveEncoder
  implicit def Decode_MessageSchema: Decoder[MessageSchema] = deriveDecoder

  implicit def Encode_RoomManifest: Encoder[RoomManifest] = deriveEncoder
  implicit def Decode_RoomManifest: Decoder[RoomManifest] = deriveDecoder


}

object JsonSchema {
  def getOrDie[A: Decoder](astr: String): A = {
    CirceParser.parse(astr).fold(err => {
      sys.error(s"Json parse err: ${err}")
    }, succ => {
      succ.as[A].fold(err => {
        val jsStr = succ.spaces2
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

}
