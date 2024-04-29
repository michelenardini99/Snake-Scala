
import SnakeFx.Direction.*
import SnakeFx.Direction
import scalafx.application.{JFXApp3, Platform}
import scalafx.beans.property.{IntegerProperty, ObjectProperty}
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color.{Green, Red, White}
import scalafx.scene.shape.Rectangle

import scala.concurrent.Future
import scala.util.Random

object SnakeFx extends JFXApp3{

  type Snake = List[(Int, Int)]

  object Snake{
    def apply(value: List[(Int, Int)]): Snake = value
  }

  type Food = (Int, Int)

  object Food {
    def apply(value: (Int, Int)): Food = value
  }

  sealed trait Direction

  object Direction{
    case object North extends Direction
    case object South extends Direction
    case object East extends Direction
    case object West extends Direction
    def defaultDirection: Direction = West
  }

  val initialSnake = List(
    (200, 200),
    (225, 200),
    (250, 200)
  )

  import scala.concurrent.ExecutionContext.Implicits.global
  def gameLoop(update: () => Unit): Unit = {
    Future{
      update()
      Thread.sleep(1000/25 * 2)
    }.flatMap(_ => Future(gameLoop(update)))
  }

  case class State(snake: Snake)(food: Food){
    def newState(dir: Direction): State = {
      val (x, y) = snake.head
      val (newX, newY) = dir match
        case North => (x, y - 25)
        case South => (x, y + 25)
        case East => (x + 25, y)
        case West => (x - 25, y)
        case _ => (x, y)
      val newSnake = {
        if(newX < 0 || newX >= 600 || newY < 0 || newY >= 600 || snake.contains((newX, newY)))
          initialSnake
        else if((newX, newY) == food)
          food :: snake
        else
          (newX, newY) :: snake.init
      }
      val newFood = {
        if ((newX, newY) == food)
          randomFood()
        else
          food
      }
      State(newSnake)(newFood)
    }
    def rectangles: List[Rectangle] = rect(food._1, food._2, Red) :: snake.map{
      case (x, y) => rect(x, y, Green)
    }
  }

  def randomFood(): Food = (Random.nextInt(24) * 25, Random.nextInt(24) * 25)

  def rect(xr: Int, yr: Int, color: Color) = new Rectangle{
    x = xr
    y = yr
    fill = color
    width = 25
    height = 25
  }

  override def start(): Unit = {
    val state = ObjectProperty(State(initialSnake)(randomFood()))
    val frame = IntegerProperty(0)
    val direction = ObjectProperty(defaultDirection)

    frame.onChange{
      state.update(state.value.newState(direction.value))
    }
    stage = new JFXApp3.PrimaryStage{
      width = 600
      height = 600
      scene = new Scene{
        fill = White
        content = state.value.rectangles
        onKeyPressed = key => key.getText match
          case "w" => direction.value = North
          case "s" => direction.value = South
          case "d" => direction.value = East
          case "a" => direction.value = West
        frame.onChange {  Platform.runLater{
            content = state.value.rectangles
          }
        }
      }

    }
    gameLoop(() => frame.update(frame.value + 1))
  }
}
