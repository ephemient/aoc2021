#!/usr/bin/env kotlin

@file:DependsOn("info.picocli:picocli:4.6.2")

import java.io.File
import java.net.HttpURLConnection
import java.net.URL
import java.time.Duration
import java.time.Instant
import java.time.LocalDate
import java.time.LocalTime
import java.time.OffsetDateTime
import java.time.Year
import java.time.ZoneOffset
import java.time.temporal.ChronoUnit
import java.util.concurrent.Callable
import kotlin.system.exitProcess
import picocli.CommandLine

@CommandLine.Command(
    name = "get-inputs",
    description = ["Retrieves Advent of Code inputs"],
)
class GetInputs : Callable<Int> {
    @CommandLine.ArgGroup
    var mode: Mode? = null
    class Mode {
        @CommandLine.Option(names = ["-n", "--dry-run"], description = ["Dry run"])
        var dryRun = false
        @CommandLine.Option(names = ["-o", "--overwrite"], description = ["Re-download even if files already exist"])
        var force = false
    }

    @CommandLine.ArgGroup
    var session: Session? = null
    class Session {
        @CommandLine.Option(names = ["-s", "--session"], description = ["SESSION cookie"], paramLabel = "SESSION")
        var session: String? = null

        @CommandLine.Option(
            names = ["-S", "--session-file"],
            description = ["SESSION cookie file"],
            paramLabel = "FILE"
        )
        var sessionFile: File? = null
    }

    @CommandLine.Option(names = ["-y", "--year"], description = ["year"], defaultValue = "2021")
    lateinit var year: Year

    @CommandLine.Parameters(paramLabel = "DAY", description = ["days to fetch"])
    var days: IntArray? = null

    @CommandLine.Option(names = ["-h", "--help"], description = ["Show usage help"], usageHelp = true)
    var help = false

    override fun call(): Int {
        val session = session?.session
            ?: session?.sessionFile?.readText()?.trimEnd('\n')
            ?: System.getenv("SESSION")?.ifEmpty { null }
            ?: File(File(System.getProperty("user.home")), ".aocrc").readText().trimEnd('\n')
        val base = OffsetDateTime.of(LocalDate.of(year.value, 12, 1), LocalTime.MIDNIGHT, zoneOffset)
        val days = days?.map { it.coerceIn(1..25) }?.toSortedSet() ?: Instant.now().let { now ->
            List(25) { it + 1 }
                .takeWhile { base.plusDays(it - 1L).toInstant().isBefore(now) }
                .ifEmpty { listOf(1) }
        }
        loop@for (day in days) {
            val file = File("day$day.txt")
            while (true) {
                if (mode?.force != true && file.exists()) {
                    println("$file already exists")
                    continue@loop
                }
                val target = base.plusDays(day - 1L)
                val now = Instant.now()
                if (!now.isBefore(target.toInstant())) break
                val delta = Duration.between(now, target)
                sequence {
                    units.fold(now.atOffset(zoneOffset)) { from, unit ->
                        val count = unit.between(from, target)
                        yield(count to unit)
                        from.plus(count, unit)
                    }
                }
                    .dropWhile { (count, _) -> count == 0L }
                    .ifEmpty { sequenceOf(delta.toMillis() to ChronoUnit.MILLIS) }
                    .joinToString(prefix = "$file available in ") { (count, unit) -> "$count $unit" }
                    .also(::println)
                if (mode?.dryRun == true) break
                Thread.sleep(delta.toMillis() + 1)
            }
            if (mode?.force != true && file.exists()) {
                println("$file already exists")
                continue@loop
            }
            val url = URL("https://adventofcode.com/${base.year}/day/$day/input")
            println("$file = $url")
            if (mode?.dryRun == true) {
                continue@loop
            }
            val conn = url.openConnection() as HttpURLConnection
            conn.setRequestProperty("Cookie", "session=$session")
            try {
                conn.connect()
                check(conn.responseCode == 200) { "${conn.responseCode} ${conn.responseMessage}" }
                conn.inputStream.use { input ->
                    file.outputStream().use { output ->
                        input.copyTo(output)
                    }
                }
            } finally {
                conn.disconnect()
            }
        }
        return 0
    }

    companion object {
        val zoneOffset: ZoneOffset = ZoneOffset.ofHours(-5)

        val units = listOf(
            ChronoUnit.YEARS,
            ChronoUnit.MONTHS,
            ChronoUnit.WEEKS,
            ChronoUnit.DAYS,
            ChronoUnit.HOURS,
            ChronoUnit.MINUTES,
            ChronoUnit.SECONDS,
        )
    }
}

exitProcess(CommandLine(GetInputs()).execute(*args))
