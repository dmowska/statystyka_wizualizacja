# ---------------------------------------------------------------------------
# Dodatkowe schematy do skryptu
#
#   2_workflow.png          - przebieg ćwiczenia: od pliku do wniosku (rozdz. 2)
#   6_anatomia_ggplot.png   - budowa wywołania ggplot() (rozdz. 6)
#   7_schemat_wykresow.png  - dobór wykresu do typu danych (rozdz. 7)
#
# Uruchomienie z katalogu projektu:  Rscript figures/schematy_dodatkowe.R
# Wymagane pakiety: DiagrammeR, DiagrammeRsvg, rsvg
#
# UWAGA: fontem jest "Helvetica" - Graphviz zna jego metryki, dzięki czemu
# ramki są dopasowane do tekstu (patrz komentarz w 13_schematy.R).
# ---------------------------------------------------------------------------

library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

NAVY   <- "#1F3864"
PYT    <- "#FFE699"
PARAM  <- "#4472C4"
NIEPAR <- "#8EAADB"
ZIEL   <- "#C5E0B4"
SZARY  <- "#D9D9D9"
RAMKA  <- "#2F528F"
RAMKA2 <- "#6E9B55"
FONT   <- "Helvetica"

kat <- if (dir.exists("figures")) "figures" else "."

zapisz <- function(dot, nazwa, szerokosc) {
  plik <- file.path(kat, nazwa)
  rsvg::rsvg_png(charToRaw(DiagrammeRsvg::export_svg(DiagrammeR::grViz(dot))),
                 file = plik, width = szerokosc)
  cat("  zapisano:", plik, "\n")
}

# =========================================================== PRZEBIEG ĆWICZENIA
dot_workflow <- sprintf('digraph workflow {
  graph [rankdir = TB, bgcolor = "white", nodesep = 0.25, ranksep = 0.38, pad = 0.25]
  node  [fontname = "%s", fontsize = 12, shape = box, style = "filled,rounded",
         color = "%s", penwidth = 1.1, margin = "0.20,0.12", width = 3.4]
  edge  [color = "%s", arrowsize = 0.7, penwidth = 1.1]

  k1 [label = "1.  Wczytaj dane\\nread.csv()", fillcolor = "%s"]
  k2 [label = "2.  Obejrzyj strukturę\\nstr()   head()   summary()", fillcolor = "%s"]
  k3 [label = "3.  Sprawdź braki danych\\ncolSums(is.na())", fillcolor = "%s"]
  k4 [label = "4.  Policz statystyki opisowe\\nmean()   median()   sd()   summarize()", fillcolor = "%s"]
  k5 [label = "5.  Zwizualizuj dane\\nggplot()", fillcolor = "%s"]
  k6 [label = "6.  Sprawdź założenia\\nshapiro.test()   leveneTest()", fillcolor = "%s"]
  k7 [label = "7.  Wykonaj test lub zbuduj model\\nt.test()   aov()   cor.test()   lm()", fillcolor = "%s"]
  k8 [label = "8.  Odczytaj i zinterpretuj wynik\\np-value, przedział ufności, wielkość różnicy", fillcolor = "%s"]
  k9 [label = "9.  Opisz wynik w raporcie\\npełnym zdaniem, z liczbami", fillcolor = "%s", fontcolor = "white"]

  k1 -> k2 -> k3 -> k4 -> k5 -> k6 -> k7 -> k8 -> k9

  u1 [label = "poznajesz dane", shape = plaintext, fillcolor = "white", fontcolor = "%s", fontsize = 11]
  u2 [label = "przygotowujesz analizę", shape = plaintext, fillcolor = "white", fontcolor = "%s", fontsize = 11]
  u3 [label = "wyciągasz wnioski", shape = plaintext, fillcolor = "white", fontcolor = "%s", fontsize = 11]
  {rank = same; k2; u1}
  {rank = same; k5; u2}
  {rank = same; k8; u3}
  u1 -> u2 -> u3 [style = invis]
}', FONT, RAMKA, RAMKA,
    SZARY, SZARY, SZARY, PYT, PYT, NIEPAR, PARAM, ZIEL, NAVY,
    RAMKA, RAMKA, RAMKA)

# ============================================================ ANATOMIA GGPLOT
dot_ggplot <- sprintf('digraph anatomia {
  graph [rankdir = TB, bgcolor = "white", nodesep = 0.45, ranksep = 0.55, pad = 0.25]
  node  [fontname = "%s", fontsize = 12, shape = box, style = "filled,rounded",
         color = "%s", penwidth = 1.1, margin = "0.18,0.11"]
  edge  [color = "%s", arrowsize = 0.7, penwidth = 1.1]

  kod [label = "ggplot(data = dane2007, aes(x = gdpPercap, y = lifeExp)) + geom_point()",
       fillcolor = "white", fontname = "Courier", fontsize = 14, penwidth = 1.4, shape = box, style = "filled"]

  d1 [label = "DATA\\nzbiór danych, z którego\\nbierzemy wartości", fillcolor = "%s"]
  d2 [label = "AES (mapowanie)\\nktóra zmienna trafia na oś x,\\nktóra na oś y, która na kolor", fillcolor = "%s"]
  d3 [label = "GEOM\\nco rysujemy:\\npunkty, słupki, pudełka...", fillcolor = "%s"]

  kod -> d1 [label = "  data  "]
  kod -> d2 [label = "  aes()  "]
  kod -> d3 [label = "  + geom_  "]

  dod [label = "Kolejne warstwy dodajemy znakiem  +  :\\nlabs() - podpisy osi i tytuł    ·    scale_*() - skale i kolory\\nfacet_wrap() - multiwykres    ·    theme_*() - wygląd wykresu",
       fillcolor = "%s", fontsize = 11]
  d1 -> dod [style = invis]
  d2 -> dod [style = invis]
  d3 -> dod [style = invis]
}', FONT, RAMKA, RAMKA, PYT, PYT, PYT, SZARY)

# ======================================================== DOBÓR TYPU WYKRESU
dot_wykresy <- sprintf('digraph wykresy {
  graph [rankdir = LR, bgcolor = "white", nodesep = 0.30, ranksep = 0.55, pad = 0.25]
  node  [fontname = "%s", fontsize = 12, shape = box, style = "filled,rounded",
         color = "%s", penwidth = 1.1, margin = "0.16,0.10"]
  edge  [fontname = "%s", fontsize = 11, color = "%s", arrowsize = 0.7, penwidth = 1.1]

  start [label = "Co chcę pokazać?", fillcolor = "%s", fontcolor = "white", fontsize = 14, penwidth = 0]

  a [label = "ROZKŁAD\\njednej zmiennej", fillcolor = "%s"]
  b [label = "PORÓWNANIE\\nmiędzy grupami", fillcolor = "%s"]
  c [label = "ZALEŻNOŚĆ\\nmiędzy zmiennymi", fillcolor = "%s"]
  d [label = "ZMIANĘ\\nw czasie", fillcolor = "%s"]

  a1 [label = "zmienna ilościowa\\nhistogram  geom_histogram()\\nwykres gęstości  geom_density()\\nwykres pudełkowy  geom_boxplot()", fillcolor = "%s"]
  a2 [label = "zmienna jakościowa\\nwykres słupkowy  geom_bar()", fillcolor = "%s", color = "%s"]

  b1 [label = "ilościowa wg grup\\nwykres pudełkowy  geom_boxplot()\\nwykres skrzypcowy  geom_violin()", fillcolor = "%s"]
  b2 [label = "dwie zmienne jakościowe\\nwykres udziałów  geom_bar(position = \\"fill\\")", fillcolor = "%s", color = "%s"]

  c1 [label = "dwie zmienne ilościowe\\nwykres rozrzutu  geom_point()", fillcolor = "%s"]
  c2 [label = "trzecia zmienna na wykresie\\nkolor, kształt lub wielkość punktu\\naes(color = ...)", fillcolor = "%s"]

  d1 [label = "wykres liniowy\\ngeom_line()", fillcolor = "%s"]

  fac [label = "Chcę to samo osobno dla każdej grupy?\\nmultiwykres  facet_wrap(~ grupa)", fillcolor = "%s"]

  start -> a [label = "  rozkład  "]
  start -> b [label = "  porównanie  "]
  start -> c [label = "  zależność  "]
  start -> d [label = "  czas  "]

  a -> a1; a -> a2
  b -> b1; b -> b2
  c -> c1; c -> c2
  d -> d1

  a1 -> fac [style = dashed, arrowsize = 0.6]
  b1 -> fac [style = dashed, arrowsize = 0.6]
  c1 -> fac [style = dashed, arrowsize = 0.6]
}', FONT, RAMKA, FONT, RAMKA, NAVY,
    PYT, PYT, PYT, PYT,
    NIEPAR, ZIEL, RAMKA2,
    NIEPAR, ZIEL, RAMKA2,
    NIEPAR, NIEPAR,
    NIEPAR, SZARY)

zapisz(dot_workflow, "2_workflow.png",         1100)
zapisz(dot_ggplot,   "6_anatomia_ggplot.png",  1500)
zapisz(dot_wykresy,  "7_schemat_wykresow.png", 1500)
