#!/usr/bin/env Rscript
# ==============================================================================
# Build the journal-neutral replication package for deposit.
#
# The package is assembled from `git archive HEAD`, so it contains exactly the
# tracked, vetted file set and nothing from the working tree. Three categories
# are then removed, because they are correspondence or working files rather than
# replication material, and because the deposit should not be tied to any
# particular journal:
#
#   - cover letters, presubmission inquiries, and the submission-form helper;
#   - journal-specific manuscript and supplementary variants;
#   - repository plumbing (.gitignore, the renv bootstrap directory).
#
# The current manuscript and supplementary sources are then written back under
# neutral names. Their analysis chunks are identical across variants, so any one
# of them reproduces the reported results; CANONICAL below picks which is used.
#
# Run from the repository root. Writes a dated zip to the project root.
# ==============================================================================

CANONICAL_MS <- "paper/manuscript_OE.qmd"      # most recent; chunks identical across variants
CANONICAL_SI <- "paper/supplementary_OE.qmd"

stopifnot(file.exists(CANONICAL_MS), file.exists(CANONICAL_SI))

version   <- "v1.1"
pkg_name  <- paste0("BiodivFinancePreferences_replication_", version)
staging   <- file.path(tempdir(), pkg_name)
unlink(staging, recursive = TRUE); dir.create(staging, recursive = TRUE)

# Export the tracked file set, not the working tree
tar <- file.path(tempdir(), "archive.tar")
system2("git", c("archive", "--format=tar", "-o", shQuote(tar), "HEAD"))
system2("tar", c("-x", "-f", shQuote(tar), "-C", shQuote(staging)))

drop <- c(
  # correspondence and submission scaffolding
  "paper/cover_letter_NEE.md", "paper/cover_letter_NS.md",
  "paper/Cover_ND.docx", "paper/Cover_NS.docx",
  "paper/presubmission_OneEarth.md", "paper/si_description.md",
  # journal-specific variants, superseded by the neutral pair written below
  "paper/manuscript_NEE.qmd", "paper/manuscript_NEE_anon.qmd",
  "paper/manuscript_NS.qmd",  "paper/manuscript_OE.qmd",
  "paper/supplementary_NEE.qmd", "paper/supplementary_NS.qmd",
  "paper/supplementary_OE.qmd",
  # presentation artefact for a specific journal's submission format, not a
  # reported result; the package is meant to stay journal-neutral
  "analysis/graphical_abstract.R",
  # repository plumbing
  ".gitignore"
)
unlink(file.path(staging, drop))
unlink(file.path(staging, "renv"), recursive = TRUE)

file.copy(CANONICAL_MS, file.path(staging, "paper", "manuscript.qmd"))
file.copy(CANONICAL_SI, file.path(staging, "paper", "supplementary.qmd"))

# The supplementary is rendered as a sibling of the manuscript, so its own
# subtitle is the only place a title appears; keep both journal-neutral.
for (f in file.path(staging, "paper", c("manuscript.qmd", "supplementary.qmd"))) {
  txt <- readLines(f, warn = FALSE)
  writeLines(txt, f)
}

out <- normalizePath(file.path(getwd(), paste0(pkg_name, ".zip")), mustWork = FALSE)
unlink(out)
owd <- setwd(dirname(staging)); on.exit(setwd(owd))
system2("zip", c("-qr", shQuote(out), shQuote(basename(staging)), "-x", "*.DS_Store"))
setwd(owd)

cat("Wrote", out, "\n")
cat("Files:", length(list.files(staging, recursive = TRUE)), "\n")
