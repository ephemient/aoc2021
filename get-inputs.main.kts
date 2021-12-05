#!/usr/bin/env kotlin

@file:Repository("https://repo.gradle.org/gradle/libs-releases/")
@file:DependsOn("info.picocli:picocli:4.6.2")
@file:DependsOn("net.rubygrapefruit:native-platform:0.22-milestone-21")

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
import java.time.temporal.TemporalUnit
import java.util.concurrent.Callable
import kotlin.system.exitProcess
import net.rubygrapefruit.platform.Native
import net.rubygrapefruit.platform.terminal.Terminals
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
        val terminals: Terminals = Native.get(Terminals::class.java)
        val terminalOutput = when {
            terminals.isTerminal(Terminals.Output.Stdout) -> terminals.getTerminal(Terminals.Output.Stdout)
            terminals.isTerminal(Terminals.Output.Stderr) -> terminals.getTerminal(Terminals.Output.Stderr)
            else -> null
        }

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
            if (mode?.force != true && file.exists()) {
                println("$file already exists")
                continue@loop
            }
            if (mode?.dryRun != true) {
                val target = base.plusDays(day - 1L)
                var delta = Duration.between(Instant.now(), target)
                while (delta > Duration.ZERO) {
                    val message = "$file available in $delta"
                    val sleep = if (terminalOutput == null) {
                        println(message)
                        delta
                    } else {
                        terminalOutput.write(message)
                        when {
                             delta > Duration.ofHours(2) -> delta % ChronoUnit.HOURS
                             delta > Duration.ofMinutes(2) -> delta % ChronoUnit.MINUTES
                            delta > Duration.ofSeconds(2) -> delta % ChronoUnit.SECONDS
                            delta > Duration.ofMillis(20) -> delta % Duration.of(10, ChronoUnit.MILLIS)
                            else -> delta
                        }
                    }
                    Thread.sleep(sleep.toMillis(), (sleep - sleep.truncatedTo(ChronoUnit.MILLIS)).toNanosPart())
                    terminalOutput?.write("\u001b[2K\r")
                    if (mode?.force != true && file.exists()) {
                        println("$file already exists")
                        continue@loop
                    }
                    delta = Duration.between(Instant.now(), target)
                }
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

        operator fun Duration.rem(duration: Duration): Duration = this - duration.multipliedBy(dividedBy(duration))

        operator fun Duration.rem(unit: TemporalUnit): Duration = this % Duration.of(1, unit)
    }
}

exitProcess(CommandLine(GetInputs()).execute(*args))
