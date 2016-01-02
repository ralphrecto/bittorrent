import akka.actor.Actor

class Tracker extends Actor {
  override def receive: Receive = {
    case "hi" => println("hi")
  }
}
