data class Doc(val head: Head?)
data class Head(val title: String?)
data class Summary(
    val title: String?,
    val ok: Boolean,
)

fun readDoc(url: String): Result<Doc> = when {
    "fail" in url -> Result.failure(Exception("Bad read of $url"))
    else -> Result.success(
        when {
            "head-missing" in url -> Doc(head = null)
            "title-missing" in url -> Doc(head = Head(title = null))
            "title-empty" in url -> Doc(head = Head(title = ""))
            else -> Doc(head = Head(title = "Title of $url"))
        }
    )
}

fun buildSummary(doc: Doc) = Summary(title = doc.head?.title, ok = true)

fun readAndBuildSummary(url: String): Summary {
    return buildSummary(
        readDoc(url).getOrElse { return Summary(title = null, ok = false) }
    )
}

// fun readAndBuildSummary(url: String): Summary = readDoc(url).fold(
//     onSuccess = { buildSummary(it) },
//     onFailure = { Summary(title = null, ok = false) },
// )

fun isTitleNonEmpty(doc: Doc): Boolean? = doc.head?.title?.isNotEmpty()

fun readWhetherTitleNonEmpty(url: String): Result<Boolean?> =
    // Result.success(
    //     isTitleNonEmpty(readDoc(url).getOrElse { return Result.failure(it) })
    // )
    readDoc(url).map { isTitleNonEmpty(it) }

fun main() {
    val urls = listOf(
        "good", "title-empty", "title-missing", "head-missing", "fail"
    )
    for (url in urls) {
        println("""Checking "https://$url/":""")
        // Summary.
        val summary = readAndBuildSummary(url)
        println("  Summary: $summary")
        println("  Title: ${summary.title ?: ""}")
        // Has title.
        val hasTitle = readWhetherTitleNonEmpty(url)
        val hasTitleSure = hasTitle.getOrDefault(false) ?: false
        println("  Has title: $hasTitle vs $hasTitleSure")
    }
}

main()
