data class Doc(val head: Head?)
data class Head(val title: String?)
data class DocReport(val title: String?, val ok: Boolean)

fun readDoc(url: String): Result<Doc> = when {
    "fail" in url -> Result.failure(Exception("Failed to read document"))
    else -> Result.success(
        when {
            "head-missing" in url -> Doc(head = null)
            "title-missing" in url -> Doc(head = Head(title = null))
            "title-empty" in url -> Doc(head = Head(title = ""))
            else -> Doc(head = Head(title = "Title of $url"))
        }
    )
}

fun buildDocReport(doc: Doc) = DocReport(title = doc.head?.title, ok = true)

fun readAndBuildDocReport(url: String): DocReport {
    return buildDocReport(
        readDoc(url).getOrElse { return DocReport(title = null, ok = false) }
    )
}

fun isTitleNonEmpty(doc: Doc): Boolean? = doc.head?.title?.isNotEmpty()

fun readWhetherTitleNonEmpty(url: String): Result<Boolean?> =
    readDoc(url).map { isTitleNonEmpty(it) }

fun main() {
    val urls = listOf(
        "good", "title-empty", "title-missing", "head-missing", "fail"
    )
    for (url in urls) {
        println("""Checking "https://$url/":""")
        println("  Report: ${readAndBuildDocReport(url)}")
        val hasTitle = readWhetherTitleNonEmpty(url);
        println("  Has title: $hasTitle vs ${
            hasTitle.getOrDefault(false) ?: false
        }");
    }
}

main()
