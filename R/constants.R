NULL

SEALED <- FALSE

PKGUTILS_OPTIONS <- new.env(parent = emptyenv())
PKGUTILS_OPTIONS$logfile <- ""

DIRS <- new.env(parent = emptyenv())
DIRS$WD_INDEX <- 0L

SPECIAL_PAPER_SIZES <- structure(
  .Data = c(216, 432, 559, 864, 279, 457, 108, 381, 445, 572, 584, 184, 216,
    216, 210, 216, 216, 203, 140, 140, 127, 419, 279, 215.9, 215.9, 457, 140,
    184, 70, 279, 140, 216, 394, 889, 229, 508, 140, 330, 279, 279, 279, 559,
    864, 1118, 432, 610, 171, 508, 572, 889, 711, 267, 304, 330, 330, 304, 330,
    267, 216, 216, 203.2, 533, 432, 355.6, 279.4, 584, 216, 267, 127, 432, 216,
    279, 489, 1143, 279, 635, 216, 483, 432, 377),
  .Dim = c(40L, 2L),
  .Dimnames = list(c("ansi a", "ansi c", "ansi d", "ansi e", "bible",
    "broadsheet", "compact", "crown", "demy", "double demy", "elephant",
    "executive", "fanfold", "folio", "foolscap", "german std fanfold",
    "government legal", "government letter", "half letter", "jepps",
    "junior legal", "large post", "ledger", "legal", "letter", "medium",
    "memo", "monarch", "organizer j", "organizer k", "organizer l",
    "organizer m", "post", "quad demy", "quarto", "royal", "statement",
    "super b", "tabloid", "us std fanfold"), c("width", "height"))
)

QUOTED <- "(%s(\\\\\"|[^\"])*%s|%s[^`]+%s|%s(\'|[^'])*%s)"

QUOTED_END <- sprintf(QUOTED, '"', "$", "`", "$", "'", "$")

QUOTED_BEGIN <- sprintf(QUOTED, "^", '"', "^", "`", "^", "'")

QUOTED <- sprintf(QUOTED, '"', '"', "`", "`", "'", "'")

OPS_LEFT <- paste("[^\\s]([~/^]|%%)", "[^\\s*][*]", "[^\\s<]<",
  "[^\\s[(:][+]", "[^\\s<[(:]-", "[^\\s>-]>", "[^\\s<>=!]=", "[^\\s&]&",
  "[^\\s|]\\|", sep = "|")

OPS_RIGHT <- paste("([~/^]|%%)[^\\s]", "[*][^\\s*]", "<[^\\s<=-]",
  ">[^\\s>=]", "=[^\\s=,]", "&[^\\s&]", "\\|[^\\s|]", sep = "|")

