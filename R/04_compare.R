# 04_compare.R -- put every published value next to its reproduced value.
# Output: output/comparison.csv (+ a console summary).

pub <- read.csv(file.path(ROOT, "published_values.csv"), stringsAsFactors = FALSE, check.names = FALSE)
rep <- RESULTS$tab
stopifnot(!anyDuplicated(rep$id))

pub$lookup <- ifelse(pub$repro_id == "" | is.na(pub$repro_id), pub$id, pub$repro_id)
cmp <- merge(pub, rep, by.x = "lookup", by.y = "id", all.x = TRUE, sort = FALSE)
cmp <- cmp[match(pub$id, cmp$id), ]
missing_ids <- cmp$id[is.na(cmp$value)]
if (length(missing_ids)) stop("No reproduced value for: ", paste(missing_ids, collapse = ", "))

# Round half up (R's round() is half-even and would hide x.5 ties).
rhu <- function(x, d) floor(x * 10^d + 0.5 + 1e-9) / 10^d
compare <- function(value, published, digits, type) {
  switch(type,
         eq = rhu(value, digits) == published,
         lt = value < published,
         gt = value > published,
         stop("unknown type ", type))
}
cmp$status <- mapply(function(v, p, dg, ty) if (compare(v, p, dg, ty)) "match" else "differs",
                     cmp$value, cmp$published, cmp$digits, cmp$type)
cmp$alt_status <- mapply(function(v, p, dg, ty) if (is.na(v)) NA_character_ else if (compare(v, p, dg, ty)) "match" else "differs",
                         cmp$alt, cmp$published, cmp$digits, cmp$type)
# distance in units of the last printed digit (eq type only)
cmp$diff_last_digit_units <- ifelse(cmp$type == "eq",
                                    round((rhu(cmp$value, cmp$digits) - cmp$published) * 10^cmp$digits), NA)

# Human explanations, matched by regex on id (first match wins).
nt <- read.csv(file.path(ROOT, "discrepancy_notes.csv"), stringsAsFactors = FALSE)
cmp$explanation <- ""; cmp$category <- ""
for (i in seq_len(nrow(cmp))) {
  if (cmp$status[i] == "differs") {
    hit <- which(vapply(nt$id_regex, function(rx) grepl(rx, cmp$id[i]), logical(1)))
    if (length(hit)) { cmp$explanation[i] <- nt$explanation[hit[1]]; cmp$category[i] <- nt$category[hit[1]] }
    else { cmp$explanation[i] <- "UNEXPLAINED"; cmp$category[i] <- "unknown" }
  }
}
cmp$table <- sub("[.].*$", "", cmp$id)
out <- cmp[, c("id", "table", "source", "item", "published", "digits", "type", "value", "alt", "status",
               "diff_last_digit_units", "alt_status", "category", "explanation")]
names(out)[names(out) == "value"] <- "reproduced"
names(out)[names(out) == "alt"] <- "alt_reproduced"
out$reproduced <- signif(out$reproduced, 6); out$alt_reproduced <- signif(out$alt_reproduced, 6)
write.csv(out, file.path(OUT, "comparison.csv"), row.names = FALSE, na = "")

# Reproduced statistics the paper never prints (regression n, LSD count, ...)
extra <- rep[!(rep$id %in% pub$lookup), c("id", "value", "note")]
write.csv(extra, file.path(OUT, "reproduced_not_published.csv"), row.names = FALSE)

summ <- as.data.frame.matrix(table(out$table, out$status))
summ$total <- rowSums(summ)
write.csv(cbind(table = rownames(summ), summ), file.path(OUT, "comparison_summary.csv"), row.names = FALSE)
cat("\n==== Comparison summary (primary reproduction) ====\n"); print(summ)
cat(sprintf("\nTOTAL: %d published values compared; %d match, %d differ.\n",
            nrow(out), sum(out$status == "match"), sum(out$status == "differs")))
dd <- out[out$status == "differs", c("id", "published", "reproduced", "alt_reproduced", "category")]
cat("\n==== Differences ====\n"); print(dd, row.names = FALSE)
