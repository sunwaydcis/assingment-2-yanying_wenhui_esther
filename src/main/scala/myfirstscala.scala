import com.github.tototoshi.csv._
import java.io.File

object HotelReport:
  def loadData(): List[Map[String, String]] =
    val file = new File("Hotel_Dataset.csv")
    val reader = CSVReader.open(file)
    val data = reader.allWithHeaders()
    reader.close()
    data

object Utils:
  def safeDouble(s: String): Double =
    val cleaned = s.replace("%", "").trim
    try cleaned.toDouble catch
      case _: Throwable => 0.0

  def safeInt(s: String): Int =
    try s.trim.toInt catch
      case _: Throwable => 0

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

//Question 2: Most Economical Hotel
class EconomicalHotelAnalysis extends BookingAnalysis:
  import Utils.*
  //Calculate nights stayed
  private def nightsStayed(start: String, end: String): Int =
    import java.time.LocalDate
    import java.time.format.DateTimeFormatter

    val formatter = DateTimeFormatter.ofPattern("M/d/yyyy")
    val day1 = LocalDate.parse(start, formatter)
    val day2 = LocalDate.parse(end, formatter)
    java.time.temporal.ChronoUnit.DAYS.between(day1, day2).toInt.max(1)

  override def analyze(data: List[Map[String,String]]): Unit =
    val grouped = data.groupBy { row =>
      (
        row("Destination Country"),
        row("Destination City"),
        row("Hotel Name")
      )
    }
    val hotelStats = grouped.map {
      case ((country, city, hotel) , rows) =>
        // Average cost per night for each hotel
        val priceRows = rows.map { r =>
          val price = safeDouble(r("Booking Price[SGD]"))
          val rooms = safeInt(r("Rooms")).max(1)
          val nightsCount = nightsStayed(r("Date of Booking"), r("Check-Out Date"))
          price / rooms / nightsCount
        }
        val avgPrice = priceRows.sum / priceRows.size

        // Average discount
        val avgDiscount = rows.map(r => safeDouble(r("Discount"))).sum / rows.size

        // Average profit margin
        val avgProfit = rows.map(r => safeDouble(r("Profit Margin"))).sum / rows.size
        (country, city, hotel, avgPrice, avgDiscount, avgProfit)
    }.toList

    // Find min max ranges for normalization
    val minPrice = hotelStats.map(_._4).min
    val maxPrice = hotelStats.map(_._4).max

    val minDiscount = hotelStats.map(_._5).min
    val maxDiscount = hotelStats.map(_._5).max

    val minProfit = hotelStats.map(_._6).min
    val maxProfit = hotelStats.map(_._6).max

    val priceRange = (maxPrice - minPrice).max(0.00001)
    val discountRange = (maxDiscount - minDiscount).max(0.00001)
    val profitRange = (maxProfit - minProfit).max(0.00001)

    // Normalize
    val scored = hotelStats.map {
      case (country, city, hotel, avgPrice, avgDiscount, avgProfit) =>
        // LOWER = BETTER
        val priceScore =
          1 - ((avgPrice - minPrice) / priceRange)
        // HIGHER = BETTER
        val discountScore =
          (avgDiscount - minDiscount) / discountRange
        // LOWER = BETTER
        val profitScore =
          1 - ((avgProfit - minProfit) / profitRange)

        val finalScore = (priceScore + discountScore + profitScore) / 3
        (country, city, hotel, priceScore, discountScore, profitScore, finalScore)
    }
    val best = scored.sortBy(-_._7).head

    println("\n2: Most Economical Hotel")
    println("+--------------------------+------------------------------+")
    println(f"| Destination Country      | ${best._1}%-28s |")
    println(f"| Destination City         | ${best._2}%-28s |")
    println(f"| Hotel Name               | ${best._3}%-28s |")
    println(f"| Booking Price Score      | ${best._4}%-28.4f |")
    println(f"| Discount Score           | ${best._5}%-28.4f |")
    println(f"| Profit Margin Score      | ${best._6}%-28.4f |")
    println("+--------------------------+------------------------------+")
    println(f"| FINAL SCORE              | ${best._7}%-28.4f |")
    println("+--------------------------+------------------------------+")


// Question 3 : Most Profitable Hotel
class MostProfitableHotelAnalysis extends BookingAnalysis:
  import Utils.*

  override def analyze(data: List[Map[String,String]]): Unit =
    val stats = data.map {row =>
      val hotel = row("Hotel Name")
      val rooms = safeInt(row("Rooms"))
      val profit = safeDouble(row("Profit Margin"))
      (hotel, rooms * profit)
    }

    val grouped = stats.groupBy(_._1)
    val totals = grouped.map { case (hotel, rows) =>
       val totalProfit = rows.map(_._2).sum
       (hotel, totalProfit)
    }.toList

    val best = totals.sortBy(-_._2).head

    println("\n3: Most Profitable Hotel")
    println(s"Hotel Name    : ${best._1}")
    println(f"Total Score   : ${best._2}%.2f")

//Main Program
object Main:
  def main(args: Array[String]): Unit =
    val data = HotelReport.loadData()

    val analyses: List[BookingAnalysis] = List(
      new PopularCountryAnalysis,
      new EconomicalHotelAnalysis,
      new MostProfitableHotelAnalysis
    )

    analyses.foreach(_.analyze(data))
