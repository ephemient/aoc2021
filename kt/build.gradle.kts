import io.gitlab.arturbosch.detekt.Detekt
import org.gradle.api.plugins.ApplicationPlugin.APPLICATION_GROUP
import org.gradle.language.base.plugins.LifecycleBasePlugin.VERIFICATION_GROUP
import org.jetbrains.kotlin.gradle.plugin.KotlinTargetPreset
import org.jetbrains.kotlin.gradle.plugin.mpp.KotlinNativeTarget

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

val commonSources by tasks.registering {
    outputs.dir(layout.buildDirectory.dir("generated/source/$name"))

    doFirst {
        delete(outputs.files.singleFile.listFiles())
    }
    doLast {
        val d = 9
        val base = LongArray(d * d).apply {
            repeat(8) { this[(d + 1) * it + 1] = 1 }
            this[d * 6] = 1
            this[d * 8] = 1
        }
        fun LongArray.pow(n: Int): LongArray {
            if (n == 1) return this
            val double = LongArray(d * d) {
                val i = it / d
                val j = it % d
                (0 until d).sumOf { k -> this[d * i + k] * this[d * k + j] }
            }.pow(n / 2)
            return if (n % 2 == 0) double else LongArray(d * d) {
                val i = it / d
                val j = it % d
                (0 until d).sumOf { k -> double[d * i + k] * this[d * k + j] }
            }
        }
        fun LongArray.rowSums() = LongArray(d) { j -> (0 until d).sumOf { i -> this[d * i + j] } }
        File(outputs.files.singleFile, "Day6Constants.kt").bufferedWriter().use { out ->
            out.write("package com.github.ephemient.aoc2021\n\n")
            out.write("object Day6Constants {\n")
            base.pow(80).rowSums().joinTo(out, prefix = "    internal val matrix80 = longArrayOf(", postfix = ")\n")
            base.pow(256).rowSums().joinTo(out, prefix = "    internal val matrix256 = longArrayOf(", postfix = ")\n")
            out.write("}\n")
        }
    }
}

val jvmResources by tasks.registering(Sync::class) {
    from(rootDir.parentFile)
    into(layout.buildDirectory.dir("generated/source/$name"))
    include("day*.txt")
}

val nonJvmSources by tasks.registering {
    inputs.files(fileTree(rootDir.parent).matching { include("day*.txt") })
    outputs.dir(layout.buildDirectory.dir("generated/source/$name"))

    doFirst {
        delete(outputs.files.singleFile.listFiles())
    }
    doLast {
        File(outputs.files.singleFile, "Resources.kt").bufferedWriter().use { out ->
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

val nativeTargets = setOf("linuxX64", "linuxArm64", "mingwX86", "mingwX64", "macosX64", "macosArm64", "wasm32")

kotlin {
    jvm {
        compilations.create("bench")
    }
    js("jsIr", IR) {
        binaries.executable()
        nodejs {
            testTask {
                useMocha()
            }
        }
        compilations.create("bench")
    }
    js("jsLegacy", LEGACY) {
        binaries.executable()
        nodejs {
            testTask {
                useMocha()
            }
        }
        compilations.create("bench")
    }
    for (nativeTarget in nativeTargets) {
        @Suppress("UNCHECKED_CAST")
        targetFromPreset(presets[nativeTarget] as KotlinTargetPreset<KotlinNativeTarget>) {
            binaries.executable {
                entryPoint("com.github.ephemient.aoc2021.main")
            }
            compilations.create("bench")
        }
    }
    targets.all {
        compilations.all {
            kotlinOptions {
                freeCompilerArgs += "-Xopt-in=kotlin.RequiresOptIn"
            }
        }
    }

    sourceSets {
        val commonMain by getting {
            kotlin.srcDir(commonSources)
        }
        getByName("commonTest") {
            dependencies {
                implementation(kotlin("test-common"))
                implementation(kotlin("test-annotations-common"))
            }
        }
        val commonBench by creating {
            dependsOn(commonMain)
            dependencies {
                implementation(libs.kotlinx.benchmark.runtime)
            }
        }

        val jvmMain by getting {
            resources.srcDir(jvmResources)
        }
        getByName("jvmTest") {
            dependencies {
                implementation(kotlin("test-junit5"))
                implementation(libs.junit.jupiter.api)
                runtimeOnly(libs.junit.jupiter.engine)
            }
        }
        getByName("jvmBench") {
            dependsOn(commonBench)
            dependsOn(jvmMain)
        }

        for ((compilation, parentCompilation) in mapOf("Main" to null, "Test" to "Main", "Bench" to "Main")) {
            val nonJvm = create("nonJvm$compilation") {
                dependsOn(getByName("common$compilation"))
                parentCompilation?.also { dependsOn(getByName("common$it")) }
            }
            val js = create("js$compilation") {
                dependsOn(nonJvm)
                parentCompilation?.also { dependsOn(getByName("nonJvm$it")) }
            }
            getByName("jsIr$compilation") {
                dependsOn(js)
                parentCompilation?.also { dependsOn(getByName("jsIr$it")) }
            }
            getByName("jsLegacy$compilation") {
                dependsOn(js)
                parentCompilation?.also { dependsOn(getByName("jsLegacy$it")) }
            }
            val native = create("native$compilation") {
                dependsOn(nonJvm)
                parentCompilation?.also { dependsOn(getByName("native$it")) }
            }
            val parents = nativeTargets.groupingBy { nativeTarget -> nativeTarget.takeWhile { it.isLowerCase() } }
                .eachCountTo(mutableMapOf())
                .apply { values.retainAll { it > 1 } }
                .mapValues { (nativeParent, _) ->
                    create("$nativeParent$compilation") {
                        dependsOn(native)
                        parentCompilation?.also { dependsOn(getByName("$nativeParent$it")) }
                    }
                }
            for (nativeTarget in nativeTargets) {
                getByName("$nativeTarget$compilation") {
                    dependsOn(parents[nativeTarget.takeWhile { it.isLowerCase() }] ?: native)
                    parentCompilation?.also { dependsOn(getByName("$nativeTarget$it")) }
                }
            }
        }

        getByName("nonJvmMain") {
            kotlin.srcDir(nonJvmSources)
        }

        getByName("jsTest") {
            dependencies {
                implementation(kotlin("test-js"))
            }
        }
    }
}

allOpen {
    annotation("org.openjdk.jmh.annotations.State")
}

benchmark {
    targets {
        register("jvmBench")
        // register("jsIrBench") // https://github.com/Kotlin/kotlinx-benchmark/issues/30
        // register("jsLegacyBench")
        // for (nativeTarget in nativeTargets) register("${nativeTarget}Bench") // https://github.com/Kotlin/kotlinx-benchmark/issues/67
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

val detektKotlinScripts by tasks.registering(Detekt::class) {
    group = VERIFICATION_GROUP
    description = "Run detekt analysis for Kotlin scripts"
    source(files().apply { from(layout.projectDirectory.asFileTree.matching { include("*.kts") }) })
}

tasks.withType<Detekt>().configureEach {
    config.from("detekt.yml")
    buildUponDefaultConfig = true
    autoCorrect = System.getenv("CI").isNullOrEmpty()
    exclude { it.file.toPath().startsWith(buildDir.toPath()) }
}
tasks.register("detektAll") { dependsOn(tasks.withType<Detekt>()) }
tasks.check { dependsOn(tasks.withType<Detekt>()) }

tasks.dependencyUpdates {
    revision = "release"
    gradleReleaseChannel = "current"
}
