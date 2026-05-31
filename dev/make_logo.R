# Build the lssdoc hex logo natively with grid + ragg (no SVG renderer
# dependency). Two palettes: "petrol" (package identity) and "lime"
# (LimeSurvey). Mirrors dev/logo_source.svg.

library(grid)

.lssdoc_logo_palettes <- list(
  petrol = list(border = c("#3A7C8C", "#0E2C3D"),
                bg     = c("#1C5468", "#0F2E3E"),
                ink    = "#F4ECD8"),
  lime   = list(border = c("#A6D93C", "#2C6E16"),
                bg     = c("#4E9A1E", "#234E10"),
                ink    = "#F7F4E3"),
  # Inverted lime: darkest on the border, lightest in the field. The
  # field is bright lime, so the ink flips to dark green for legibility.
  lime_inv = list(border = c("#2C6E16", "#143407"),
                  bg     = c("#B6E04A", "#7FBE2E"),
                  ink    = "#1E4310")
)

# y flips SVG (top-down) coordinates into grid native (bottom-up).
.yf <- function(y) 1024 - y

# Vertices of a regular pointy-top hexagon (point at top and bottom,
# vertical left/right edges) centred at (cx, cy) with circumradius R.
.hex_pts <- function(cx = 512, cy = 512, R = 432) {
  ang <- (c(90, 30, -30, -90, -150, 150)) * pi / 180
  list(x = cx + R * cos(ang), y = cy + R * sin(ang))
}

# Inner hexagon = outer scaled toward the centre by k. For a regular
# polygon every edge is the same distance (the apothem a = R*cos(30))
# from the centre, so scaling the vertices by k moves every edge inward
# by exactly a*(1-k) -- a perfectly uniform border of width d.
.hex_inset <- function(p, d, cx = 512, cy = 512, R = 432) {
  a <- R * cos(pi / 6)
  k <- 1 - d / a
  list(x = cx + k * (p$x - cx), y = cy + k * (p$y - cy))
}

.draw_bar <- function(x, y, w, h, ink, alpha) {
  grid.roundrect(
    x = unit(x + w / 2, "native"), y = unit(.yf(y + h / 2), "native"),
    width = unit(w, "native"), height = unit(h, "native"),
    r = unit(2.4, "pt"),
    gp = gpar(fill = ink, col = NA, alpha = alpha)
  )
}

lssdoc_make_logo <- function(out, palette = "petrol", px = 1024, border = TRUE,
                             border_d = 14) {
  pal <- .lssdoc_logo_palettes[[palette]]
  ragg::agg_png(out, width = px, height = px, units = "px",
                background = "transparent", res = 72)
  on.exit(dev.off(), add = TRUE)

  pushViewport(viewport(xscale = c(0, 1024), yscale = c(0, 1024)))

  border_grad <- linearGradient(pal$border, x1 = 0, y1 = 1, x2 = 1, y2 = 0)
  bg_grad <- radialGradient(pal$bg, cx1 = .5, cy1 = .56, r1 = 0,
                            cx2 = .5, cy2 = .56, r2 = .72)

  outer <- .hex_pts()
  if (isTRUE(border)) {
    # Border ring (outer hex) under the face (inner hex). The inner hex
    # is a true uniform inset, so the border is the same width (border_d
    # px) on every side -- verticals and diagonals alike.
    inner <- .hex_inset(outer, d = border_d)
    grid.polygon(unit(outer$x, "native"), unit(.yf(outer$y), "native"),
                 gp = gpar(fill = border_grad, col = NA))
    grid.polygon(unit(inner$x, "native"), unit(.yf(inner$y), "native"),
                 gp = gpar(fill = bg_grad, col = NA))
  } else {
    # Borderless: a single hex carries the field gradient -- the
    # hexagon edge is its own boundary.
    grid.polygon(unit(outer$x, "native"), unit(.yf(outer$y), "native"),
                 gp = gpar(fill = bg_grad, col = NA))
  }

  # Watermark motif: 3 language columns of text lines.
  ink <- pal$ink
  .draw_bar(430, 322, 3, 150, ink, 0.10)
  .draw_bar(591, 322, 3, 150, ink, 0.10)
  cols <- c(292, 452, 612)
  for (ry in c(334, 372, 410)) for (cx in cols) .draw_bar(cx, ry, 120, 20, ink, 0.17)
  last <- list(c(292, 86), c(452, 110), c(612, 72))
  for (b in last) .draw_bar(b[1], 448, b[2], 20, ink, 0.17)

  # Wordmark.
  grid.text("lssdoc",
            x = unit(512, "native"), y = unit(.yf(566), "native"),
            gp = gpar(fontfamily = "Georgia", fontface = "bold",
                      fontsize = 165, col = ink))
  popViewport()
  invisible(out)
}

# Scalable SVG twin of lssdoc_make_logo(), built from the same regular
# hexagon + uniform inset so PNG and SVG stay in lock-step.
lssdoc_write_logo_svg <- function(out, palette = "petrol", border = TRUE,
                                  border_d = 14) {
  pal <- .lssdoc_logo_palettes[[palette]]
  sy <- function(y) 1024 - y                       # hex math-y -> SVG y-down
  pts <- function(p) paste(sprintf("%.1f,%.1f", p$x, sy(p$y)), collapse = " ")
  outer <- .hex_pts(); inner <- .hex_inset(outer, d = border_d)
  hex <- if (isTRUE(border)) {
    sprintf('  <polygon points="%s" fill="url(#border)"/>\n  <polygon points="%s" fill="url(#bg)"/>',
            pts(outer), pts(inner))
  } else {
    sprintf('  <polygon points="%s" fill="url(#bg)"/>', pts(outer))
  }
  rows <- ""
  bar <- function(x, y, w, h = 20) sprintf('    <rect x="%g" y="%g" width="%g" height="%g" rx="6"/>\n', x, y, w, h)
  for (ry in c(334, 372, 410)) for (cx in c(292, 452, 612)) rows <- paste0(rows, bar(cx, ry, 120))
  rows <- paste0(rows, bar(292, 448, 86), bar(452, 448, 110), bar(612, 448, 72))
  svg <- sprintf('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 1024 1024">
  <defs>
    <linearGradient id="border" x1="0%%" y1="0%%" x2="100%%" y2="100%%">
      <stop offset="0%%" stop-color="%s"/><stop offset="100%%" stop-color="%s"/>
    </linearGradient>
    <radialGradient id="bg" cx="50%%" cy="44%%" r="72%%">
      <stop offset="0%%" stop-color="%s"/><stop offset="100%%" stop-color="%s"/>
    </radialGradient>
  </defs>
%s
  <g opacity="0.17" fill="%s">
    <rect x="430" y="322" width="3" height="150" rx="1.5" opacity="0.6"/>
    <rect x="591" y="322" width="3" height="150" rx="1.5" opacity="0.6"/>
%s  </g>
  <text x="512" y="622" text-anchor="middle" font-family="Georgia, \'Times New Roman\', serif" font-size="165" font-weight="700" fill="%s">lssdoc</text>
</svg>\n',
    pal$border[1], pal$border[2], pal$bg[1], pal$bg[2], hex, pal$ink, rows, pal$ink)
  writeLines(svg, out)
  invisible(out)
}

if (sys.nframe() == 0L) {
  d <- "dev/logo_variants"
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  variants <- list(
    list(f = "petrol",          pal = "petrol",   bd = TRUE),
    list(f = "petrol_noborder", pal = "petrol",   bd = FALSE),
    list(f = "lime",            pal = "lime",     bd = TRUE),
    list(f = "lime_noborder",   pal = "lime",     bd = FALSE),
    list(f = "lime_inverted",   pal = "lime_inv", bd = TRUE)
  )
  for (v in variants) {
    lssdoc_make_logo(file.path(d, paste0("logo_", v$f, ".png")), v$pal, border = v$bd)
    lssdoc_write_logo_svg(file.path(d, paste0("logo_", v$f, ".svg")), v$pal, border = v$bd)
  }
  cat("variants written to", d, "\n")
}
