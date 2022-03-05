data class Doc(val head: Head?)
data class Head(val title: String?)
data class Summary(
    val title: String?,
    val ok: Boolean,
)

fun readDoc(url: String): Doc = when {
    // `never` vs `!` (never) vs `noreturn` vs `Nothing`
    "fail" in url -> throw Exception("Bad read of $url")
    "head-missing" in url -> Doc(head = null)
    "title-missing" in url -> Doc(head = Head(title = null))
    "title-empty" in url -> Doc(head = Head(title = ""))
    else -> Doc(head = Head(title = "Title of $url"))
}

fun buildSummary(doc: Doc) = Summary(title = doc.head?.title, ok = true)

fun readAndBuildSummary(url: String): Summary {
    return buildSummary(
        try { readDoc(url) } catch (err: Exception) {
            return Summary(title = null, ok = false)
        }
    )
}

fun isTitleNonEmpty(doc: Doc): Boolean? = doc.head?.title?.isNotEmpty()

fun readWhetherTitleNonEmpty(url: String): Boolean? =
    isTitleNonEmpty(readDoc(url))

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
        val hasTitle = try {
            readWhetherTitleNonEmpty(url)
        } catch (err: Exception) { err }
        val hasTitleSure = when (hasTitle) {
            is Exception -> false
            else -> hasTitle ?: false
        }
        println("  Has title: $hasTitle vs $hasTitleSure")
    }
}

main()
