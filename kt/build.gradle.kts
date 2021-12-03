import io.gitlab.arturbosch.detekt.Detekt
import org.gradle.api.plugins.ApplicationPlugin.APPLICATION_GROUP
import org.gradle.language.base.plugins.LifecycleBasePlugin.VERIFICATION_GROUP

plugins {
    kotlin("multiplatform") version libs.versions.kotlin.get()
    kotlin("plugin.allopen") version libs.versions.kotlin.get()
    alias(libs.plugins.dependency.updates)
    alias(libs.plugins.detekt)
    alias(libs.plugins.dokka)
    alias(libs.plugins.kotlinx.benchmark)
    distribution
}

dependencies {
    detektPlugins(libs.detekt.formatting)
}

val jvmResources by tasks.registering(Sync::class) {
    from(rootDir.parentFile)
    into(layout.buildDirectory.dir("generated/source/$name"))
    include("day*.txt")
}

val nonJvmSources by tasks.registering {
    inputs.files(fileTree(rootDir.parent).matching { include("day*.txt") })
    outputs.dir(layout.buildDirectory.dir("generated/source/$name"))

    doLast {
        val outputDir = outputs.files.singleFile
        delete(outputDir.listFiles())
        File(outputDir, "Resources.kt").bufferedWriter().use { out ->
            out.write("package com.github.ephemient.aoc2021\n\n")
            out.write("actual fun getInput(day: Int): List<String> = when (day) {\n")
            for (
                (day, file) in inputs.files.files
                    .mapNotNull { file ->
                        val day = file.nameWithoutExtension.removePrefix("day").toIntOrNull()
                            ?: return@mapNotNull null
                        day to file
                    }
                    .sortedBy { it.first }
            ) {
                out.write("    $day -> \"\"\"\n")
                file.useLines { lines ->
                    for (line in lines) {
                        out.write("        |")
                        out.write(line.replace("$", "\${'$'}").replace("\"\"\"", "\"\"\${'\"'}"))
                        out.write("\n")
                    }
                }
                out.write("    \"\"\".trimMargin()\n\n")
            }
            out.write("    else -> throw IllegalArgumentException(\"No data for day \$day\")\n")
            out.write("}.lines()\n")
        }
    }
}

kotlin {
    jvm {
        compilations.create("bench")
    }
    // js(IR) // https://github.com/Kotlin/kotlinx-benchmark/issues/30
    js {
        binaries.executable()
        nodejs {
            testTask {
                useMocha()
            }
        }
        compilations.create("bench")
    }
    linuxX64 {
        binaries.executable {
            entryPoint("com.github.ephemient.aoc2021.main")
        }
        compilations.create("bench")
    }

    sourceSets {
        val commonMain by getting

        val jvmMain by getting {
            resources.srcDir(jvmResources)
        }

        val nonJvmMain by creating {
            kotlin.srcDir(nonJvmSources)
        }

        val jsMain by getting {
            dependsOn(nonJvmMain)
        }

        val linuxX64Main by getting {
            dependsOn(nonJvmMain)
        }

        getByName("commonTest") {
            dependencies {
                implementation(kotlin("test-common"))
                implementation(kotlin("test-annotations-common"))
            }
        }

        getByName("jvmTest") {
            dependencies {
                implementation(kotlin("test-junit5"))
                implementation(libs.junit.jupiter.api)
                runtimeOnly(libs.junit.jupiter.engine)
            }
        }

        getByName("jsTest") {
            dependencies {
                implementation(kotlin("test-js"))
            }
        }

        val commonBench by creating {
            dependsOn(commonMain)
            dependencies {
                implementation(libs.kotlinx.benchmark.runtime)
            }
        }

        getByName("jvmBench") {
            dependsOn(commonBench)
            dependsOn(jvmMain)
        }

        getByName("jsBench") {
            dependsOn(commonBench)
            dependsOn(jsMain)
        }

        getByName("linuxX64Bench") {
            dependsOn(commonBench)
            dependsOn(linuxX64Main)
        }
    }
}

allOpen {
    annotation("org.openjdk.jmh.annotations.State")
}

benchmark {
    targets {
        register("jvmBench")
        register("jsBench")
        // register("linuxX64Bench") // https://github.com/Kotlin/kotlinx-benchmark/issues/67
    }

    configurations {
        named("main") {
            warmups = 1
            iterationTime = 1
            project.findProperty("benchmarkInclude")?.let { include(it.toString()) }
            project.findProperty("benchmarkExclude")?.let { exclude(it.toString()) }
        }
    }
}

val jvmJar by tasks.existing
val jvmRuntimeClasspath by configurations.existing

tasks.register<JavaExec>("jvmRun") {
    description = "Runs this project as a JVM application"
    group = APPLICATION_GROUP
    classpath(jvmJar, jvmRuntimeClasspath)
    mainClass.set("com.github.ephemient.aoc2021.MainKt")
}

val jvmStartScripts by tasks.registering(CreateStartScripts::class) {
    description = "Creates OS specific scripts to run the project as a JVM application."
    classpath = files(jvmJar, jvmRuntimeClasspath)
    applicationName = project.name
    outputDir = File(buildDir, "scripts")
    mainClass.set("com.github.ephemient.aoc2021.MainKt")
}

distributions {
    main {
        contents.with(
            copySpec {
                from("src/dist")
                with(
                    copySpec {
                        from(jvmJar, jvmRuntimeClasspath)
                        into("lib")
                    }
                )
                with(
                    copySpec {
                        from(jvmStartScripts)
                        into("bin")
                        fileMode = "755".toInt(8)
                    }
                )
            }
        )
    }
}

tasks.named<Test>("jvmTest") {
    useJUnitPlatform()
}

tasks.withType<org.gradle.jvm.tasks.Jar>().matching { it.name.endsWith("BenchmarkJar") }.configureEach {
    // https://github.com/Kotlin/kotlinx-benchmark/issues/68
    duplicatesStrategy = DuplicatesStrategy.WARN
}

val detektKotlinScripts by tasks.registering(Detekt::class) {
    group = VERIFICATION_GROUP
    description = "Run detekt analysis for Kotlin scripts"
    source(files().apply { from(layout.projectDirectory.asFileTree.matching { include("*.kts") }) })
}

tasks.withType<Detekt>().configureEach {
    config.from("detekt.yml")
    buildUponDefaultConfig = true
    autoCorrect = !System.getenv("CI").isNullOrEmpty()
    exclude { it.file.toPath().startsWith(buildDir.toPath()) }
}
tasks.register("detektAll") { dependsOn(tasks.withType<Detekt>()) }
tasks.check { dependsOn(tasks.withType<Detekt>()) }

tasks.dependencyUpdates {
    revision = "release"
    gradleReleaseChannel = "current"
}
