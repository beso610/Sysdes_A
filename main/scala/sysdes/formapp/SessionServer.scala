package sysdes.formapp

import java.net.{Socket, URLDecoder}

import sysdes.formapp.server.{Handler, Server}

import scala.collection.mutable.HashMap
import java.util.UUID

//import sysdes.formapp.SessionServerHandler.{sessionID, states}
import sysdes.formapp.SessionServerHandler.states
import sysdes.formapp.http.Status

class State(var name: String, var gender: String, var message: String)

object SessionServer extends Server(8002) {
  override def getHandler(socket: Socket) = new SessionServerHandler(socket)
}

object SessionServerHandler {
  // インスタンス間で共有する内部状態に関する変数・関数はこの中に記述
  val states = HashMap[String, State]()
}


class SessionServerHandler(socket: Socket) extends Handler(socket) {
  import sysdes.formapp.server.{NotFound, Ok, Request, Response}

  def handle(request: Request): Response = request match {
    case Request("GET", "/", _, _, _) => index()
    case Request("POST", "/form", _, _, _) => registerName()
    case Request("POST", "/name", _, _, _) => registerGender(request)
    case Request("POST", "/gender", _, _, _) => registerMessage(request)
    case Request("POST", "/message", _, _, _) => confirm(request)
    case Request("POST", "/confirm", _, _, _) => index()
    case _                            => NotFound(s"Requested resource '${request.path}' for ${request.method} is not found.")
  }

  def index(): Response = {
    val sessionID = UUID.randomUUID()
    //<input type="submit" value="start" />
    val res = Ok("""<html>
         |<body>
         |    <form action="/form" method="post">
         |        <p>アンケート開始</p>
         |        <p><input type="submit" value="start"></p>
         |    </form>
         |</body>
         |</html>""".stripMargin)
    res.addHeader("Set-Cookie", s"session-id=${sessionID}")
    res
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

  def registerGender(request: Request): Response = {
    val name : String = request.body.getOrElse("0")
    val decodeName = URLDecoder.decode(name, "UTF-8").replace("name=", "")
    val id = request.headers.get("Cookie").getOrElse("0")
    val s = new State(decodeName, "", "")
    states put (id, s)
    Ok(s"""<html>
          |<body>
          |    <form method="post" action="/gender">
          |      <p>性別：<input type="radio" name="gender" value="male" required>男性
          |           <input type="radio" name="gender" value="female">女性
          |           <input type="radio" name="gender" value="Other">その他　<strong>必須</strong></p>
          |
          |      <p><input type="submit" value="next"></p>
          |    </form>
          |</body>
          |</html>""".stripMargin)
  }

  def registerMessage(request: Request) : Response = {
    val gender : String = request.body.getOrElse("0")
    val decodeGender = URLDecoder.decode(gender, "UTF-8").replace("gender=", "")
    val id = request.headers.get("Cookie").getOrElse("0")
    val s = states get(id)
    val tmpState = new State("","", "")
    val ss = s.getOrElse(tmpState)
    ss.gender = decodeGender
    states put (id, ss)

    Ok(s"""<html>
          |<body>
          |    <form method="post" action="/message">
          |      <p>メッセージ：<strong>必須</strong></p>
          |      <p><textarea name="message" cols="30" rows="5" required></textarea></p>
          |      <p><input type="submit" value="next"></p>
          |    </form>
          |</body>
          |</html>""".stripMargin)
  }

  def confirm(request: Request) : Response = {
    val message : String = request.body.getOrElse("0")
    val decodeMessage = URLDecoder.decode(message, "UTF-8").replace("message=", "")
    val id = request.headers.get("Cookie").getOrElse("0")
    val s = states get(id)
    val tmpState = new State("","", "")
    val ss = s.getOrElse(tmpState)
    val name = ss.name
    val gender = ss.gender
    Ok(s"""<html>
          |<body>
          |    <form method="post" action="/confirm">
          |      <p>名前：$name</p>
          |      <p>性別：$gender</p>
          |      <p>メッセージ</p>
          |      <p><textarea name="message" cols="30" rows="5">$decodeMessage</textarea></p>
          |      <p><input type="submit" value="submit"></p>
          |    </form>
          |</body>
          |</html>""".stripMargin)

  }


}
