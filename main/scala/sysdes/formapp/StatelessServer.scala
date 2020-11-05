package sysdes.formapp

import java.net.Socket
import java.net.URLDecoder
import sysdes.formapp.server.{Handler, Server}

object StatelessServer extends Server(8001) {
  override def getHandler(socket: Socket) = new StatelessServerHandler(socket)
}

class StatelessServerHandler(socket: Socket) extends Handler(socket) {

  import sysdes.formapp.server.{NotFound, Ok, Request, Response}

  override def handle(request: Request): Response = request match {
    case Request("GET", "/", _, _, _) => index()
    case Request("POST", "/form", _, _, _) => registerName()
    case Request("POST", "/name", _, _, _) => registerGender(request.body: Option[String])
    case Request("POST", "/gender", _, _, _) => registerMessage(request.body: Option[String])
    case Request("POST", "/message", _, _, _) => confirm(request.body: Option[String])
    case Request("POST", "/confirm", _, _, _) => index()
    case _                            => NotFound(s"Requested resource '${request.path}' for ${request.method} is not found.")
  }

  def index(): Response = {
      //<input type="submit" value="start" />
    Ok("""<html>
        |<body>
        |    <form action="/form" method="post">
        |        <p>アンケート開始</p>
        |        <p><input type="submit" value="start"></p>
        |    </form>
        |</body>
        |</html>""".stripMargin)
  }

  def registerName(): Response = {
    Ok("""<html>
         |<body>
         |    <form method="post" action="/name">
         |      <p>名前：<input type="text" name="name" required>　<strong>必須</strong></p>
         |      <p><input type="submit" value="next"></p>
         |    </form>
         |</body>
         |</html>""".stripMargin)
  }

  def registerGender(body: Option[String]): Response = {
    val info : String = body.getOrElse("0")
    val decodeInfo = URLDecoder.decode(info, "UTF-8").replace("name=", "")
    Ok(s"""<html>
         |<body>
         |    <form method="post" action="/gender">
         |      <p>性別：<input type="radio" name="gender" value="male" required>男性
         |           <input type="radio" name="gender" value="female">女性
         |           <input type="radio" name="gender" value="Other">その他　<strong>必須</strong></p>
         |      <p><input type="submit" value="next"></p>
         |      <input type="hidden" name="name" value=$decodeInfo>
         |    </form>
         |</body>
         |</html>""".stripMargin)
  }

  def registerMessage(body: Option[String]) : Response = {
    val info : String = body.getOrElse("0")
    val decodeInfo = URLDecoder.decode(info, "UTF-8").replace("gender=", "")
    Ok(s"""<html>
          |<body>
          |    <form method="post" action="/message">
          |      <p>メッセージ：<strong>必須</strong></p>
          |      <p><textarea name="message" cols="30" rows="5" required></textarea></p>
          |      <p><input type="submit" value="next"></p>
          |      <input type="hidden" name="gender" value=$decodeInfo>
          |    </form>
          |</body>
          |</html>""".stripMargin)
  }

  def confirm(body: Option[String]) : Response = {
    val info : String = body.getOrElse("0")
    val decodeInfo = URLDecoder.decode(info, "UTF-8")
    val info1 = decodeInfo.replace("message=", "")
    val info2 = info1.replace("&gender=", ",")
    val info3 = info2.replace("&name=", ",")
    val infoFinal : Array[String] = info3.split(",")
    val name = infoFinal(2)
    val gender = infoFinal(1)
    val message = infoFinal(0)
    Ok(s"""<html>
          |<body>
          |    <form method="post" action="/confirm">
          |      <p>名前：$name</p>
          |      <p>性別：$gender</p>
          |      <p>メッセージ</p>
          |      <p><textarea name="message" cols="30" rows="5"> $message </textarea></p>
          |      <p><input type="submit" value="submit"></p>
          |    </form>
          |</body>
          |</html>""".stripMargin)

  }
}
