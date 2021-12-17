plugins {
    application
    alias(libs.plugins.native.image)
}

application {
    mainClass.set("com.github.ephemient.aoc2021.MainKt")
}

graalvmNative.binaries.named("main") {
    imageName.set(rootProject.name)
    buildArgs.add("-H:IncludeResources=day.*\\.txt")
}

dependencies {
    implementation(rootProject)
}
