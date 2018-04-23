package gittergetter


object Formatting {
  val TB = TextBoxing
  import TB._

  import java.time.{ Instant, ZoneId }
  import java.time.format.{ DateTimeFormatter, FormatStyle }
  import java.util.Locale

  def formatInstant(i: Instant): String = {
    val formatter =
      DateTimeFormatter.ofLocalizedDateTime( FormatStyle.SHORT )
        .withLocale( Locale.US )
        .withZone( ZoneId.systemDefault() );
    formatter.format(i)
  }

  def formatMessage(messageSchema: MessageSchema): TB.Box = {
    val textBox = messageSchema.text.mbox
    val sent =  formatInstant(messageSchema.sent)
    vjoin(
      hjoin(">", hspace(1), textBox),
      vspace(1),
      indent(
        hjoin("- ", messageSchema.fromUser.username, " @", sent)
      ),
      vspace(2)
    )

  }


}
