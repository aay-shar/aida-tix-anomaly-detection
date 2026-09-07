# =============================================================================
# Pipeline schematic (base ggplot2 only — no extra packages, square corners)
#
#   raw tables -> clean/guard -> feature engineering -> matrix
#              -> AIDA -> ranked claims + TIX
#   rules table branches off as a HELD-OUT benchmark (answer-key leakage guard)
#
# Boxes = geom_rect, arrows = geom_segment, all text = annotate().
# Exports vector PDF + high-res PNG sized for an A0 poster column.
# Tested against stable base ggplot2 API only.
# =============================================================================

library(ggplot2)
library(grid)   # unit(), arrow()  — ships with base R, no install needed

# ---- palette 
P <- list(
  blue_fill   = "#DCE9F8", blue_line   = "#3B6BB0", blue_text   = "#1F4E79",
  teal_fill   = "#D6EDE4", teal_line   = "#2F8C6E", teal_text   = "#1D6B51",
  purple_fill = "#E6E2F5", purple_line = "#6B5FB0", purple_text = "#3D2F7A",
  coral_fill  = "#F8D6CE", coral_line  = "#D85A30", coral_text  = "#993C1D",
  amber_fill  = "#FBE9CE", amber_line  = "#BA7517", amber_text  = "#7A5410",
  gray_fill   = "#ECEAE4", gray_line   = "#888780", gray_text   = "#4A4A45",
  arrow_col   = "#7A7A72"
)

# ---- box geometry -----------------------------------------------------------
# x, y = CENTRE of each box.  w, h = HALF width / HALF height.
# Coordinate space is arbitrary (0-100). Square corners (base geom_rect).
boxes <- data.frame(
  id   = c("claims","history","parts","labours","rules",
           "clean","feateng","benchmark","matrix","aida","output","eval"),
  x    = c( 16, 16, 16, 16, 16,   49, 82, 49, 82, 49, 49, 73),
  y    = c( 88, 78, 68, 58, 48,   72, 72, 49, 40, 32, 21, 12),
  w    = c(  9,  9,  9,  9,  9,   13, 14, 14, 15, 13, 14, 13),
  h    = c(  3,  3,  3,  3,  3,    7, 11,  4,  4,  4,  4,  4),
  fill = c(rep(P$blue_fill,4), P$amber_fill,
           P$teal_fill, P$teal_fill, P$amber_fill,
           P$purple_fill, P$coral_fill, P$coral_fill, P$gray_fill),
  line = c(rep(P$blue_line,4), P$amber_line,
           P$teal_line, P$teal_line, P$amber_line,
           P$purple_line, P$coral_line, P$coral_line, P$gray_line),
  tcol = c(rep(P$blue_text,4), P$amber_text,
           P$teal_text, P$teal_text, P$amber_text,
           P$purple_text, P$coral_text, P$coral_text, P$gray_text),
  stringsAsFactors = FALSE
)
boxes$xmin <- boxes$x - boxes$w; boxes$xmax <- boxes$x + boxes$w
boxes$ymin <- boxes$y - boxes$h; boxes$ymax <- boxes$y + boxes$h

gx <- function(id) boxes$x[boxes$id == id]   # helper: centre-x of a box
gy <- function(id) boxes$y[boxes$id == id]   # helper: centre-y of a box

# ---- solid pipeline arrows --------------------------------------------------
# Edge coordinates derived from box geometry so arrows touch borders cleanly.
arrows_df <- data.frame(
  x = c(25, 25, 25, 25,                       # 4 input tables -> clean (fan-in)
        gx("clean")+13,                        # clean -> feateng
        25,                                    # rules -> benchmark
        gx("feateng"),                         # feateng -> matrix (vertical)
        gx("matrix")-14,                       # matrix -> aida (leftward)
        gx("aida"),                            # aida -> output (vertical)
        gx("output")+12),                      # output -> eval (diagonal)
  y = c(88, 78, 68, 58,
        gy("clean"),
        48,
        gy("feateng")-11,
        gy("matrix"),
        gy("aida")-4,
        gy("output")),
  xend = c(rep(gx("clean")-13, 4),
           gx("feateng")-13,
           gx("benchmark")-13,
           gx("matrix"),
           gx("aida")+12,
           gx("output"),
           gx("eval")),
  yend = c(74, 73, 71, 70,
           gy("clean"),
           48,
           gy("matrix")+4,
           gy("matrix"),
           gy("output")+4,
           gy("eval")),
  stringsAsFactors = FALSE
)

# ---- dashed benchmark -> evaluation leader (elbow = two segments) -----------
dash_df <- data.frame(
  x    = c(gx("benchmark"),            gx("benchmark")),
  y    = c(gy("benchmark")-4,          gy("eval")),
  xend = c(gx("benchmark"),            gx("eval")-13),
  yend = c(gy("eval"),                 gy("eval")),
  arrowed = c(FALSE, TRUE),
  stringsAsFactors = FALSE
)

arr <- arrow(length = unit(0.18, "cm"), type = "closed")

# ---- text content (built as data frames, drawn with one geom_text each) -----
# Titles (bold). y-offset puts the title near the top of tall boxes,
# centred for short ones.
title_df <- data.frame(
  id    = boxes$id,
  label = c("claims","history","parts","labours","rules",
            "Clean + guard","Feature engineering","Held-out benchmark",
            "Analysis-ready matrix","AIDA detector","Ranked claims + TIX",
            "Evaluation"),
  size  = c(rep(5,5), 5.2, 5.2, 5, 5, 5.1, 5.1, 5),
  stringsAsFactors = FALSE
)
title_df$x <- vapply(title_df$id, gx, numeric(1))
# vertical placement of titles
title_yoff <- c(claims=0,history=0,parts=0,labours=0,rules=0,
                clean=2.4, feateng=7.6, benchmark=1.4, matrix=1.4,
                aida=1.4, output=1.4, eval=1.4)
title_df$y    <- vapply(title_df$id, gy, numeric(1)) + title_yoff[title_df$id]
title_df$tcol <- boxes$tcol[match(title_df$id, boxes$id)]

# Subtitles (smaller). One row per box that has one.
sub_df <- data.frame(
  id = c("clean","benchmark","matrix","aida","output","eval"),
  label = c("collapse to latest snapshot\ndrop PII + outcomes",
            "never a model input",
            "claims × encoded features",
            "isolation + distance score",
            "feature-level explanation",
            "scores vs. held-out rules"),
  stringsAsFactors = FALSE
)
sub_df$x    <- vapply(sub_df$id, gx, numeric(1))
sub_yoff    <- c(clean=-1.8, benchmark=-1.6, matrix=-1.6,
                 aida=-1.6, output=-1.6, eval=-1.6)
sub_df$y    <- vapply(sub_df$id, gy, numeric(1)) + sub_yoff[sub_df$id]
sub_df$tcol <- boxes$tcol[match(sub_df$id, boxes$id)]

# Feature-engineering bullet list (single centred multi-line block)
feat_df <- data.frame(
  x = gx("feateng"), y = gy("feateng") - 1.5,
  label = paste("revision patterns","parts discrepancies","history lookback",
                "robust scaling","missingness flags","categorical encoding",
                sep = "\n"),
  tcol = boxes$tcol[boxes$id == "feateng"],
  stringsAsFactors = FALSE
)

# ---- assemble ---------------------------------------------------------------
p <- ggplot() +
  geom_rect(data = boxes,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = boxes$fill, colour = boxes$line, linewidth = 0.6) +
  geom_segment(data = arrows_df,
               aes(x = x, y = y, xend = xend, yend = yend),
               colour = P$arrow_col, linewidth = 0.5,
               arrow = arr, lineend = "round") +
  # dashed elbow: first segment no head, second segment with head
  geom_segment(data = dash_df[!dash_df$arrowed, ],
               aes(x = x, y = y, xend = xend, yend = yend),
               colour = P$gray_line, linewidth = 0.45, linetype = "dashed") +
  geom_segment(data = dash_df[dash_df$arrowed, ],
               aes(x = x, y = y, xend = xend, yend = yend),
               colour = P$gray_line, linewidth = 0.45, linetype = "dashed",
               arrow = arr) +
  geom_text(data = title_df, aes(x = x, y = y, label = label),
            fontface = "bold", colour = title_df$tcol, size = title_df$size) +
  geom_text(data = sub_df, aes(x = x, y = y, label = label),
            colour = sub_df$tcol, size = 3.5, lineheight = 0.9) +
  geom_text(data = feat_df, aes(x = x, y = y, label = label),
            colour = feat_df$tcol, size = 3.6, lineheight = 1.15) +
  # section header (top-left)
  annotate("text", x = 7, y = 94, hjust = 0, fontface = "bold",
           label = "Raw relational tables", size = 5.4, colour = "#222222") +
  # legend swatch + caption (bottom)
  annotate("rect", xmin = 7, xmax = 9.4, ymin = 6.4, ymax = 8.4,
           fill = P$amber_fill, colour = P$amber_line, linewidth = 0.6) +
  annotate("text", x = 10.2, y = 7.4, hjust = 0, size = 3.6, colour = "#333333",
           label = "Held-out benchmark — excluded from model input to avoid answer-key leakage") +
  coord_equal(xlim = c(5, 98), ylim = c(4, 96), expand = FALSE) +
  theme_void()

# ---- export -----------------------------------------------------------------
ggsave("pipeline_diagram.pdf", p, width = 11, height = 8.5, units = "in")
ggsave("pipeline_diagram.png", p, width = 11, height = 8.5, units = "in", dpi = 300)
# For SVG: install.packages("svglite"), then:
# ggsave("pipeline_diagram.svg", p, width = 11, height = 8.5, units = "in")

cat("Done -> pipeline_diagram.{pdf,png}\n")

# =============================================================================
# Figure 3.2 — Data-schema / entity-relationship diagram (base ggplot2 only)
#
#   Five tables. claims is the PARENT (keyed on File / CaseID).
#   parts, labours, history, rules are CHILDREN — one-to-many from claims.
#   Each child is aggregated, then LEFT-JOINED onto the claims base.
#
#   Layout: four children stacked on the LEFT (with vertical gaps),
#   claims base on the RIGHT. Crow's-foot marks the "one" (claims, bar)
#   and "many" (child, fork) ends. Join arrows labelled.
#
#   Edit the `cols`/`keys` vectors to match your real schema.
# =============================================================================

library(ggplot2)
library(grid)

# ---- palette ----------------------------------------------------------------
P <- list(
  hdr_parts="#3B6BB0", hdr_labours="#2F8C6E", hdr_history="#6B5FB0",
  hdr_rules="#BA7517",  hdr_claims="#993C1D",
  body_fill="#FFFFFF", body_line="#9A958B", row_alt="#F4F2EC",
  hdr_text="#FFFFFF", col_text="#2A2A2A", key_text="#111111",
  arrow_col="#6E6E66", join_lbl="#444444")

# ---- geometry constants -----------------------------------------------------
TBL_W <- 19        # half-width of each table
ROW_H <- 3.2       # column-row height
HDR_H <- 4.0       # header-band height
# children given explicit TOP y's with gaps so boxes never touch:
tables <- list(
  parts = list(header="parts", col=P$hdr_parts, cx=21, top=96,
               cols=c("File*","CaseID*","part_code","part_price","part_qty"),
               keys=c(TRUE,TRUE,FALSE,FALSE,FALSE)),
  labours = list(header="labours", col=P$hdr_labours, cx=21, top=73,
                 cols=c("File*","CaseID*","labour_code","labour_hours","labour_rate"),
                 keys=c(TRUE,TRUE,FALSE,FALSE,FALSE)),
  history = list(header="history", col=P$hdr_history, cx=21, top=50,
                 cols=c("File*","CaseID*","stage","snapshot_date","status"),
                 keys=c(TRUE,TRUE,FALSE,FALSE,FALSE)),
  rules = list(header="rules", col=P$hdr_rules, cx=21, top=27,
               cols=c("File*","CaseID*","rule_id","rule_flag"),
               keys=c(TRUE,TRUE,FALSE,FALSE)),
  claims = list(header="claims  (base)", col=P$hdr_claims, cx=78, top=72,
                cols=c("File*  (PK)","CaseID*  (PK)","manufacturer","vehicle_value",
                       "odometer","cause_of_damage"),
                keys=c(TRUE,TRUE,FALSE,FALSE,FALSE,FALSE)))

# ---- build rect/text frames -------------------------------------------------
hdr_rects<-data.frame(); body_rects<-data.frame()
hdr_txt<-data.frame();   col_txt<-data.frame()
geom_cache<-list()
for(nm in names(tables)){
  t<-tables[[nm]]; n<-length(t$cols)
  xL<-t$cx-TBL_W; xR<-t$cx+TBL_W
  yHtop<-t$top; yHbot<-t$top-HDR_H
  hdr_rects<-rbind(hdr_rects,data.frame(xmin=xL,xmax=xR,ymin=yHbot,ymax=yHtop,fill=t$col))
  hdr_txt<-rbind(hdr_txt,data.frame(x=t$cx,y=(yHtop+yHbot)/2,label=t$header))
  for(i in seq_len(n)){
    rtop<-yHbot-(i-1)*ROW_H; rbot<-rtop-ROW_H
    body_rects<-rbind(body_rects,data.frame(xmin=xL,xmax=xR,ymin=rbot,ymax=rtop,
                                            fill=ifelse(i%%2==0,P$row_alt,P$body_fill)))
    col_txt<-rbind(col_txt,data.frame(x=xL+1.4,y=(rtop+rbot)/2,
                                      label=t$cols[i],key=t$keys[i]))
  }
  yBbot<-yHbot-n*ROW_H
  geom_cache[[nm]]<-list(xL=xL,xR=xR,yTop=yHtop,yBot=yBbot,yMid=(yHtop+yBbot)/2)
}

# ---- relationship arrows: child right edge -> claims left edge ---------------
cl<-geom_cache[["claims"]]
rel<-data.frame()
for(nm in c("parts","labours","history","rules")){
  g<-geom_cache[[nm]]
  rel<-rbind(rel,data.frame(child=nm,
                            cx_from=g$xR, cy_from=g$yMid, cx_to=cl$xL, cy_to=cl$yMid))
}
arr<-arrow(length=unit(0.16,"cm"),type="closed")

# Crow's-foot glyphs.
#   MANY (fork) sits flush on the CHILD right edge, opening leftward INTO table.
#   ONE  (bar)  sits flush on the CLAIMS left edge.
FORK<-1.6; BAR<-1.5
cf<-data.frame()
for(i in seq_len(nrow(rel))){
  r<-rel[i,]
  # fork: three prongs from a point just outside child edge back to the edge
  px<-r$cx_from + FORK            # prong apex (toward claims)
  ex<-r$cx_from                   # child edge
  cf<-rbind(cf,
            data.frame(x=px,y=r$cy_from, xend=ex,yend=r$cy_from+1.5),
            data.frame(x=px,y=r$cy_from, xend=ex,yend=r$cy_from),
            data.frame(x=px,y=r$cy_from, xend=ex,yend=r$cy_from-1.5))
  # bar: single vertical tick just outside claims edge
  bx<-r$cx_to - BAR
  cf<-rbind(cf,data.frame(x=bx,y=r$cy_to-1.5,xend=bx,yend=r$cy_to+1.5))
}
# arrow shaft runs between fork apex and bar tick (so it doesn't overdraw glyphs)
rel$sx_from<-rel$cx_from+FORK
rel$sx_to  <-rel$cx_to  -BAR
rel$lx<-(rel$sx_from+rel$sx_to)/2
rel$ly<-(rel$cy_from+rel$cy_to)/2 + 1.5

# ---- assemble ---------------------------------------------------------------
p<-ggplot()+
  geom_rect(data=body_rects,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            fill=body_rects$fill,colour=P$body_line,linewidth=0.3)+
  geom_rect(data=hdr_rects,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),
            fill=hdr_rects$fill,colour=NA)+
  lapply(geom_cache,function(g)
    annotate("rect",xmin=g$xL,xmax=g$xR,ymin=g$yBot,ymax=g$yTop,
             fill=NA,colour=P$body_line,linewidth=0.7))+
  geom_segment(data=rel,aes(x=sx_from,y=cy_from,xend=sx_to,yend=cy_to),
               colour=P$arrow_col,linewidth=0.5,arrow=arr)+
  geom_segment(data=cf,aes(x=x,y=y,xend=xend,yend=yend),
               colour=P$arrow_col,linewidth=0.5)+
  geom_text(data=hdr_txt,aes(x=x,y=y,label=label),
            colour=P$hdr_text,fontface="bold",size=4.4)+
  geom_text(data=col_txt,aes(x=x,y=y,label=label),
            colour=ifelse(col_txt$key,P$key_text,P$col_text),
            fontface=ifelse(col_txt$key,"bold","plain"),hjust=0,size=3.4)+
  geom_text(data=rel,aes(x=lx,y=ly,label="aggregate + left-join"),
            colour=P$join_lbl,size=2.9,fontface="italic")+
  annotate("text",x=4,y=99.5,hjust=0,fontface="bold",size=5,colour="#222222",
           label="Figure 3.2  Claims data schema and join structure")+
  annotate("text",x=4,y=3,hjust=0,size=3.1,colour="#555555",
           label="* = key column (File / CaseID identify a claim).  One claim \u2192 many child rows; each child is aggregated to one row per claim, then left-joined onto the claims base.")+
  coord_equal(xlim=c(2,100),ylim=c(1,101),expand=FALSE)+
  theme_void()

ggsave("schema_erd.pdf",p,width=10,height=9,units="in")
ggsave("schema_erd.png",p,width=10,height=9,units="in",dpi=300)
# SVG: install.packages("svglite"); ggsave("schema_erd.svg",p,width=10,height=9)
cat("Done -> schema_erd.{pdf,png}\n")



# --- 1. Build the per-field missingness data from your master table ---
# Exclude keys; compute proportion MISSING per field (1 - presence).
keys <- c("File", "Input.Accident.CaseID", "Input.Stage")

miss_df <- out$master_claim %>%
  dplyr::select(-dplyr::any_of(keys)) %>%
  dplyr::summarise(dplyr::across(everything(),
                                 ~mean(is.na(.)))) %>%
  tidyr::pivot_longer(everything(),
                      names_to = "field", values_to = "pct_miss") %>%
  dplyr::mutate(
    presence = 1 - pct_miss,
    band = dplyr::case_when(
      presence >= 0.80 ~ "Mostly present",
      presence <= 0.20 ~ "Mostly absent",
      TRUE             ~ "Variable"
    )
  ) %>%
  dplyr::arrange(pct_miss) %>%
  dplyr::mutate(field = factor(field, levels = field))   # lock ordering

# --- 2. Report band counts (these go in your caption) ---
band_counts <- miss_df %>% dplyr::count(band)
print(band_counts)
cat("Total flag columns =", nrow(miss_df), "\n")

# --- 3. Ordered horizontal bar chart, coloured by 80/20 band ---
p <- ggplot(miss_df, aes(x = pct_miss * 100, y = field, fill = band)) +
  # shaded threshold regions (20% and 80% MISSING boundaries)
  annotate("rect", xmin = 0,  xmax = 20,  ymin = -Inf, ymax = Inf,
           alpha = 0.06, fill = "forestgreen") +
  annotate("rect", xmin = 80, xmax = 100, ymin = -Inf, ymax = Inf,
           alpha = 0.06, fill = "firebrick") +
  geom_col(width = 0.75) +
  geom_vline(xintercept = c(20, 80), linetype = "dashed",
             colour = "grey40", linewidth = 0.3) +
  scale_fill_manual(values = c("Mostly present" = "#2c7fb8",
                               "Mostly absent"  = "#d95f02",
                               "Variable"       = "#7570b3"),
                    name = "80/20 band") +
  scale_x_continuous(limits = c(0, 100), expand = c(0, 0),
                     breaks = seq(0, 100, 20)) +
  labs(x = "Missing (%)", y = NULL) +
  theme_minimal(base_size = 9) +
  theme(
    axis.text.y = element_text(size = 5),    # many fields -> small labels
    panel.grid.major.y = element_blank(),
    legend.position = "top"
  )

# --- 4. Save at a size that fits a thesis page ---
ggsave("missingness_pattern.pdf", p, width = 6.5, height = 8, units = "in")
ggsave("missingness_pattern.png", p, width = 6.5, height = 8, units = "in", dpi = 300)

# --- 1. Count features by manifest source ---
comp <- out$feature_manifest %>%
  dplyr::count(source, name = "n") %>%
  dplyr::mutate(
    # readable labels for the plot
    source_label = dplyr::recode(source,
                                 "onehot_categorical"       = "One-hot categorical",
                                 "missingness_flag"         = "Missingness flag",
                                 "numeric"                  = "Numeric",
                                 "freq_encoded_categorical" = "Frequency-encoded categorical"),
    pct = round(100 * n / sum(n), 1)
  ) %>%
  dplyr::arrange(dplyr::desc(n))

total_features <- sum(comp$n)
print(comp)
cat("Total =", total_features, "\n")

# --- 2. Single horizontal stacked bar ---
p <- ggplot(comp, aes(x = n, y = "Features", fill = source_label)) +
  geom_col(width = 0.5) +
  # label each segment with its count + percentage
  geom_text(aes(label = paste0(n, "\n(", pct, "%)")),
            position = position_stack(vjust = 0.5),
            size = 3, colour = "white", fontface = "bold") +
  scale_fill_manual(values = c(
    "One-hot categorical"           = "#2c7fb8",
    "Missingness flag"              = "#d95f02",
    "Numeric"                       = "#1b9e77",
    "Frequency-encoded categorical" = "#7570b3"),
    name = NULL) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = paste0("Number of features (total = ", total_features, ")"),
       y = NULL) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.y     = element_blank(),
    panel.grid      = element_blank(),
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(nrow = 2))

ggsave("feature_composition.pdf", p, width = 7, height = 2.6, units = "in")
ggsave("feature_composition.png", p, width = 7, height = 2.6, units = "in", dpi = 300)

examples_tbl <- out$feature_manifest %>%
  dplyr::group_by(source) %>%
  dplyr::summarise(
    count    = dplyr::n(),
    examples = paste(head(feature, 3), collapse = ", "),
    .groups  = "drop"
  ) %>%
  dplyr::mutate(source = dplyr::recode(source,
                                       "onehot_categorical"       = "One-hot categorical",
                                       "missingness_flag"         = "Missingness flag",
                                       "numeric"                  = "Numeric",
                                       "freq_encoded_categorical" = "Frequency-encoded categorical")) %>%
  dplyr::arrange(dplyr::desc(count))

print(examples_tbl)




# --- 0. Ensure both score vectors exist, aligned to out$X row order ---
# AIDA scores: `scores` (already in session)
# iForest scores: re-run if not in session --
#   library(isotree)
#   fit_if <- isolation.forest(out$X, ntrees = 200, sample_size = 256, ndim = 1)
#   scores_iforest <- predict(fit_if, out$X, type = "score")

stopifnot(length(scores_aida) == nrow(out$X),
          length(scores_iforest) == nrow(out$X))

# --- 1. Convert scores to RANKS (rank 1 = most anomalous) ---
# Higher score = more anomalous, so rank descending.
rank_df <- tibble::tibble(
  aida_score = scores_aida,
  if_score   = scores_iforest
) %>%
  dplyr::mutate(
    aida_rank = rank(-aida_score, ties.method = "average"),
    if_rank   = rank(-if_score,   ties.method = "average"),
    in_aida_top100 = aida_rank <= 100,
    in_if_top100   = if_rank   <= 100,
    shared_top100  = in_aida_top100 & in_if_top100
  )

# --- 2. Compute the statistics for annotation ---
rho     <- cor(rank_df$aida_rank, rank_df$if_rank, method = "spearman")
overlap <- sum(rank_df$shared_top100)
cat("Spearman rho =", round(rho, 3), " | top-100 overlap =", overlap, "\n")

n <- nrow(rank_df)

# --- 3. Scatter: AIDA rank (x) vs Isolation Forest rank (y) ---
p <- ggplot(rank_df, aes(x = aida_rank, y = if_rank)) +
  # shaded top-100 bands for each method (rank <= 100)
  annotate("rect", xmin = 0.5, xmax = 100.5, ymin = -Inf, ymax = Inf,
           alpha = 0.07, fill = "#2c7fb8") +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.5, ymax = 100.5,
           alpha = 0.07, fill = "#d95f02") +
  # all claims
  geom_point(alpha = 0.18, size = 0.5, colour = "grey35") +
  # the shared top-100 claim(s), highlighted
  geom_point(data = dplyr::filter(rank_df, shared_top100),
             colour = "#e7298a", size = 2.6, shape = 18) +
  # band boundary lines
  geom_vline(xintercept = 100, linetype = "dashed",
             colour = "#2c7fb8", linewidth = 0.3) +
  geom_hline(yintercept = 100, linetype = "dashed",
             colour = "#d95f02", linewidth = 0.3) +
  # annotations
  annotate("text", x = n * 0.55, y = n * 0.93,
           label = paste0("Spearman~rho == ", round(rho, 2)),
           parse = TRUE, size = 3.4, hjust = 0) +
  annotate("text", x = n * 0.55, y = n * 0.86,
           label = paste0("Top-100 overlap: ", overlap, " claim",
                          ifelse(overlap == 1, "", "s")),
           size = 3.4, hjust = 0) +
  labs(x = "AIDA rank (1 = most anomalous)",
       y = "Isolation Forest rank (1 = most anomalous)") +
  coord_fixed() +                       # equal scales -> 1:1 line is meaningful
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank())
p
ggsave("rank_agreement.pdf", p, width = 6, height = 6, units = "in")
ggsave("rank_agreement.png", p, width = 6, height = 6, units = "in", dpi = 300)




# --- 1. Total absolute contribution per feature across all 100 claims ---
# Sum |contribution| down each column, then map to manifest source.
feat_total <- colSums(abs(tix_contrib))

contrib_by_source <- tibble::tibble(
  feature = colnames(tix_contrib),
  total   = feat_total
) %>%
  dplyr::left_join(out$feature_manifest, by = "feature") %>%
  dplyr::group_by(source) %>%
  dplyr::summarise(total_contribution = sum(total), .groups = "drop") %>%
  dplyr::mutate(
    share = 100 * total_contribution / sum(total_contribution),
    source_label = dplyr::recode(source,
                                 "onehot_categorical"       = "One-hot categorical",
                                 "missingness_flag"         = "Missingness flag",
                                 "numeric"                  = "Numeric",
                                 "freq_encoded_categorical" = "Frequency-encoded categorical")
  ) %>%
  dplyr::arrange(dplyr::desc(share))

print(contrib_by_source)   # these shares go in your caption

# --- 2. Bar chart of attribution share by source ---
p <- ggplot(contrib_by_source,
            aes(x = reorder(source_label, share), y = share, fill = source_label)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(share, 1), "%")),
            hjust = -0.15, size = 3.4) +
  scale_fill_manual(values = c(
    "One-hot categorical"           = "#2c7fb8",
    "Missingness flag"              = "#d95f02",
    "Numeric"                       = "#1b9e77",
    "Frequency-encoded categorical" = "#7570b3")) +
  scale_y_continuous(limits = c(0, max(contrib_by_source$share) * 1.12),
                     expand = c(0, 0)) +
  coord_flip() +
  labs(x = NULL, y = "Share of total TIX contribution (%)") +
  theme_minimal(base_size = 10) +
  theme(panel.grid.major.y = element_blank())
p
ggsave("tix_attribution.pdf", p, width = 6.5, height = 3, units = "in")
ggsave("tix_attribution.png", p, width = 6.5, height = 3, units = "in", dpi = 300)



#isolation forest
p <- ggplot(data.frame(score = scores_iforest), aes(x = score)) +
  geom_histogram(bins = 50, fill = "#a6cee3", colour = "grey40",
                 linewidth = 0.2) +
  labs(x = "Anomaly score", y = "Frequency",
       title = "Isolation Forest anomaly score distribution") +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(size = 11))
p
ggsave("iforest_score_distribution.pdf", p, width = 6, height = 4.5, units = "in")



# ============================================================
# Rank-agreement scatter (AIDA vs Isolation Forest)
# Presentation-grade version, regenerated from the REAL ranks
# ============================================================
# Produces: rank_agreement_slide.png (8 x 8 in, 300 dpi)
#
# The stats (rho, overlap) are COMPUTED from the data, not
# hardcoded, so the annotation can never drift from the plot.

library(ggplot2)
library(dplyr)

# ---- 1. INPUT — plug in your two score vectors -------------
# Replace the two lines below with your actual score objects
# from AIDA.R (higher score = more anomalous for both methods).

scores_aida    <- scores_aida      # <- your AIDA scores
scores_iforest <- scores_iforest   # <- your Isolation Forest scores

ranks <- tibble(
  aida_rank = rank(-scores_aida,    ties.method = "first"),
  if_rank   = rank(-scores_iforest, ties.method = "first")
)

# ---- 2. Style knobs ----------------------------------------
font <- ""            # "" = default; set to your deck font if installed
pal <- list(
  points = "#33736B", # cloud  (deck teal — tweak to your template)
  band   = "#33736B", # top-100 shading
  accent = "#D85A30", # the shared claim + callout
  ink    = "#2C2C2A", # titles / axis titles
  muted  = "#6A6965", # axis text / subtitle
  grid   = "#E8E6E1"
)
k <- 100              # top-k band

# ---- 3. Computed annotations -------------------------------
n        <- nrow(ranks)
rho      <- cor(ranks$aida_rank, ranks$if_rank, method = "spearman")
shared   <- ranks %>% filter(aida_rank <= k, if_rank <= k)
n_shared <- nrow(shared)

callout_lab <- if (n_shared == 1) {
  "the only claim in both top-100s"
} else {
  sprintf("%d claims in both top-100s", n_shared)
}

# ---- 4. Plot -----------------------------------------------
p <- ggplot(ranks, aes(aida_rank, if_rank)) +
  # shaded top-100 bands (replaces the faint dashed guides)
  annotate("rect", xmin = 0, xmax = k, ymin = 0, ymax = n,
           fill = pal$band, alpha = 0.07) +
  annotate("rect", xmin = 0, xmax = n, ymin = 0, ymax = k,
           fill = pal$band, alpha = 0.07) +
  annotate("segment", x = k, xend = k, y = 0, yend = n,
           colour = pal$band, alpha = 0.35, linewidth = 0.3) +
  annotate("segment", x = 0, xend = n, y = k, yend = k,
           colour = pal$band, alpha = 0.35, linewidth = 0.3) +
  # perfect-agreement diagonal
  annotate("segment", x = 0, y = 0, xend = n, yend = n,
           colour = pal$ink, linetype = "22",
           linewidth = 0.4, alpha = 0.45) +
  # the cloud
  geom_point(size = 0.55, alpha = 0.28, colour = pal$points, stroke = 0) +
  # the shared top-100 claim(s)
  geom_point(data = shared, shape = 18, size = 3.4, colour = pal$accent) +
  annotate("curve",
           x = n * 0.30, y = n * 0.13,
           xend = shared$aida_rank[1] + n * 0.015,
           yend = shared$if_rank[1] + n * 0.015,
           curvature = -0.2, colour = pal$accent, linewidth = 0.5,
           arrow = arrow(length = unit(6, "pt"))) +
  annotate("text", x = n * 0.315, y = n * 0.13, hjust = 0,
           label = callout_lab, colour = pal$accent,
           fontface = "bold", size = 5, family = font) +
  # stats box (computed, not typed)
  annotate("label", x = n * 0.985, y = n * 0.985, hjust = 1, vjust = 1,
           label = sprintf("rank agreement: \u03c1 = %.2f\ntop-100 overlap: %d of %d",
                           rho, n_shared, k),
           size = 5, colour = pal$ink, family = font,
           fill = "white", alpha = 0.9, label.size = 0, lineheight = 1.15) +
  scale_x_continuous(labels = scales::comma,
                     expand = expansion(mult = 0.02)) +
  scale_y_continuous(labels = scales::comma,
                     expand = expansion(mult = 0.02)) +
  coord_fixed() +
  labs(
    # delete title/subtitle if the slide supplies its own
    title    = "Two detectors, two different opinions",
    subtitle = sprintf(
      "Each point is one of %s claims, placed by its rank under each method.\nIf the methods agreed, points would hug the dashed diagonal \u2014 instead they fill the plane.",
      format(n, big.mark = ",")),
    x = "AIDA rank (1 = most anomalous)",
    y = "Isolation Forest rank (1 = most anomalous)"
  ) +
  theme_minimal(base_size = 18, base_family = font) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = pal$grid, linewidth = 0.4),
    axis.title  = element_text(colour = pal$ink,  size = 16),
    axis.text   = element_text(colour = pal$muted, size = 13),
    plot.title  = element_text(face = "bold", size = 23, colour = pal$ink),
    plot.subtitle = element_text(size = 13.5, colour = pal$muted,
                                 lineheight = 1.2,
                                 margin = margin(t = 4, b = 14)),
    plot.title.position = "plot",
    plot.margin = margin(12, 16, 10, 10)
  )

# ---- 5. Export for PowerPoint ------------------------------
ggsave("rank_agreement_slide.png", p,
       width = 8, height = 8, dpi = 300, bg = "white")

# Insert into PowerPoint at up to ~6.5 in tall without upscaling.
