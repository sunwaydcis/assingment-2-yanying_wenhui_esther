import com.github.tototoshi.csv._
import java.io.File

object HotelReport:

  def loadData(): List[Map[String, String]] =
    val file = new File("Hotel_Dataset.csv")
    val reader = CSVReader.open(file)
    val data = reader.allWithHeaders()
    reader.close()
    data

//Trait
trait BookingAnalysis:
  def analyze(data: List[Map[String, String]]): Unit

//Question 1: Highest Booking Country
class PopularCountryAnalysis extends BookingAnalysis:
  override def analyze(data: List[Map[String,String]]): Unit =
    val grouped = data.groupBy(_("Destination Country"))
    val top = grouped.maxByOption(_._2.size)

    top.foreach { case (country,rows) =>
      println("\n1: Highest Bookings Country")
      println(s"Country        : $country")
      println(s"Total Bookings : ${rows.size}")
    }

//Main Program
object Main:
  def main(args: Array[String]): Unit =
    val data = HotelReport.loadData()

    val analyses: List[BookingAnalysis] = List(
      new PopularCountryAnalysis
    )

    analyses.foreach(_.analyze(data))
