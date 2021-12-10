plugins {
    application
    alias(libs.plugins.native.image)
}

application {
    mainClass.set("com.github.ephemient.aoc2021.MainKt")
}

nativeBuild {
    imageName.set(rootProject.name)
    buildArgs.add("-H:IncludeResources=day.*\\.txt")
}

dependencies {
    implementation(rootProject)
}
