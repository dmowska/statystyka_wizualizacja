# ---------------------------------------------------------------------------
# Schematy wyboru testu statystycznego - rozdział "Testy statystyczne"
#
# Skrypt generuje dwa pliki PNG do folderu figures/:
#   13_schemat_ogolny.png       - od pytania badawczego do rodziny testów
#   13_schemat_porownanie.png   - szczegółowy wybór testu do porównywania grup
#
# Uruchomienie z katalogu projektu:  Rscript figures/13_schematy.R
# Wymagane pakiety: DiagrammeR, DiagrammeRsvg, rsvg
#
# UWAGA: fontem jest "Helvetica" - Graphviz zna jego metryki, dzięki czemu
# ramki są dopasowane do tekstu. Przy fontach spoza listy Graphviza
# (np. "DejaVu Sans") tekst wychodzi poza ramki.
# ---------------------------------------------------------------------------

library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

# paleta zgodna z dotychczasowymi rycinami w skrypcie
NAVY   <- "#1F3864"   # punkt wyjścia
PYT    <- "#FFE699"   # pytanie / decyzja (schemat szczegółowy)
PYT_IL <- "#FFE699"   # pytanie o zmienną ILOŚCIOWĄ
PYT_JA <- "#C5E0B4"   # pytanie o zmienną JAKOŚCIOWĄ
TLO_IL <- "#FFF8E3"   # tło grupy pytań ilościowych
TLO_JA <- "#EFF6E9"   # tło grupy pytań jakościowych
PARAM  <- "#4472C4"   # test parametryczny
NIEPAR <- "#8EAADB"   # test nieparametryczny
POMOC  <- "#D9D9D9"   # test pomocniczy (sprawdzanie założeń)
RAMKA  <- "#2F528F"
RAMKA2 <- "#6E9B55"   # obramowanie pól jakościowych
PODP2  <- "#44682F"   # podpis grupy jakościowej (ciemniejszy - lepszy kontrast)
FONT   <- "Helvetica"

kat <- if (dir.exists("figures")) "figures" else "."

zapisz <- function(dot, nazwa, szerokosc) {
  plik <- file.path(kat, nazwa)
  rsvg::rsvg_png(charToRaw(DiagrammeRsvg::export_svg(DiagrammeR::grViz(dot))),
                 file = plik, width = szerokosc)
  cat("  zapisano:", plik, "\n")
}

# ============================================================ SCHEMAT OGÓLNY
dot_ogolny <- sprintf('digraph schemat_ogolny {
  graph [rankdir = LR, bgcolor = "white", nodesep = 0.18, ranksep = 1.0, pad = 0.25]
  node  [fontname = "%s", fontsize = 12, shape = box, style = "filled,rounded",
         color = "%s", penwidth = 1.1, margin = "0.18,0.11"]
  edge  [color = "%s", arrowsize = 0.7, penwidth = 1.1]

  start [label = "Jakie mam\\npytanie badawcze?", fillcolor = "%s", fontcolor = "white",
         fontsize = 14, penwidth = 0]

  subgraph cluster_il {
    label = "  ZMIENNE ILOŚCIOWE  "
    labeljust = "l"
    fontname = "%s"
    fontsize = 13
    fontcolor = "%s"
    style = "filled,rounded"
    fillcolor = "%s"
    color = "%s"
    penwidth = 1.3
    margin = 14

    q3 [label = "Czy grupy różnią się przeciętnym\\npoziomem zmiennej ilościowej?", fillcolor = "%s"]
    q2 [label = "Czy wariancje w dwóch grupach\\nsą takie same?", fillcolor = "%s"]
    q1 [label = "Czy rozkład zmiennej jest\\nzbliżony do normalnego?", fillcolor = "%s"]
    q6 [label = "Czy dwie zmienne ilościowe\\nsą ze sobą powiązane?", fillcolor = "%s"]
    q7 [label = "Czy mogę przewidywać zmienną\\nna podstawie innych zmiennych?", fillcolor = "%s"]
  }

  subgraph cluster_ja {
    label = "  ZMIENNE JAKOŚCIOWE  "
    labeljust = "l"
    fontname = "%s"
    fontsize = 13
    fontcolor = "%s"
    style = "filled,rounded"
    fillcolor = "%s"
    color = "%s"
    penwidth = 1.3
    margin = 14

    q4 [label = "Czy rozkład zmiennej jakościowej\\nzgadza się z teoretycznym?", fillcolor = "%s", color = "%s"]
    q5 [label = "Czy dwie zmienne jakościowe\\nsą ze sobą powiązane?", fillcolor = "%s", color = "%s"]
  }

  a3 [label = "SCHEMAT SZCZEGÓŁOWY\\n(rycina poniżej)\\nrozdz. 14-16", fillcolor = "%s",
      fontcolor = "white", penwidth = 2.5]
  a2 [label = "test F  -  var.test()\\ntest Levene’a  -  leveneTest()\\nrozdz. 14", fillcolor = "%s"]
  a1 [label = "test Shapiro-Wilka\\nshapiro.test()\\nrozdz. 9", fillcolor = "%s"]
  a6 [label = "korelacja Pearsona lub Spearmana\\ncor.test()\\nrozdz. 18", fillcolor = "%s"]
  a7 [label = "regresja liniowa\\nlm()\\nrozdz. 19", fillcolor = "%s", fontcolor = "white"]
  a4 [label = "test zgodności chi-kwadrat\\nchisq.test(x, p = ...)\\nrozdz. 17", fillcolor = "%s"]
  a5 [label = "test niezależności chi-kwadrat\\nchisq.test(tabela)\\npróby zależne: mcnemar.test()\\nrozdz. 17", fillcolor = "%s"]

  start -> q3 -> a3
  start -> q2 -> a2
  start -> q1 -> a1
  start -> q6 -> a6
  start -> q7 -> a7
  start -> q4 -> a4
  start -> q5 -> a5
}', FONT, RAMKA, RAMKA, NAVY,
    FONT, RAMKA, TLO_IL, RAMKA,
    PYT_IL, PYT_IL, PYT_IL, PYT_IL, PYT_IL,
    FONT, PODP2, TLO_JA, RAMKA2,
    PYT_JA, RAMKA2, PYT_JA, RAMKA2,
    NAVY, POMOC, POMOC, NIEPAR, PARAM, NIEPAR, NIEPAR)

# ======================================================= SCHEMAT SZCZEGÓŁOWY
dot_porownanie <- sprintf('digraph schemat_porownanie {
  graph [rankdir = TB, bgcolor = "white", nodesep = 0.28, ranksep = 0.55, pad = 0.25]
  node  [fontname = "%s", fontsize = 12, shape = box, style = "filled,rounded",
         color = "%s", penwidth = 1.1, margin = "0.16,0.10"]
  edge  [fontname = "%s", fontsize = 11, color = "%s", arrowsize = 0.7, penwidth = 1.1]

  start [label = "Porównuję przeciętny poziom\\nzmiennej ILOŚCIOWEJ", fillcolor = "%s",
         fontcolor = "white", penwidth = 0]
  g  [label = "Ile grup?", fillcolor = "%s"]
  n1 [label = "Rozkład\\nnormalny?", fillcolor = "%s"]
  d  [label = "Próby niezależne\\nczy zależne?", fillcolor = "%s"]
  w  [label = "Próby niezależne\\nczy zależne?", fillcolor = "%s"]
  dn [label = "Rozkład\\nnormalny?", fillcolor = "%s"]
  dz [label = "Rozkład RÓŻNIC\\nnormalny?", fillcolor = "%s"]
  wn [label = "Rozkład normalny,\\nwariancje podobne?", fillcolor = "%s"]

  t1  [label = "test t-Studenta\\ndla jednej próby", fillcolor = "%s", fontcolor = "white"]
  t2  [label = "test kolejności par\\nWilcoxona", fillcolor = "%s"]
  dnt [label = "test t-Studenta\\ndla prób niezależnych", fillcolor = "%s", fontcolor = "white"]
  dnn [label = "test U\\nManna-Whitneya", fillcolor = "%s"]
  dzt [label = "test t-Studenta\\ndla prób zależnych", fillcolor = "%s", fontcolor = "white"]
  dzn [label = "test kolejności par\\nWilcoxona", fillcolor = "%s"]
  wnt [label = "jednoczynnikowa\\nANOVA", fillcolor = "%s", fontcolor = "white"]
  wnn [label = "test\\nKruskala-Wallisa", fillcolor = "%s"]
  wz  [label = "test Friedmana", fillcolor = "%s"]
  ph1 [label = "post-hoc:\\ntest Tukeya", fillcolor = "%s", fontcolor = "white", style = "filled,dashed,rounded"]
  ph2 [label = "post-hoc:\\ntest Dunna", fillcolor = "%s", style = "filled,dashed,rounded"]

  start -> g
  g -> n1 [label = " 1 próba wobec\\n zadanej wartości "]
  g -> d  [label = " 2 "]
  g -> w  [label = " 3 i więcej "]
  n1 -> t1 [label = " tak "]
  n1 -> t2 [label = " nie "]
  d -> dn [label = " niezależne "]
  d -> dz [label = " zależne "]
  dn -> dnt [label = " tak "]
  dn -> dnn [label = " nie "]
  dz -> dzt [label = " tak "]
  dz -> dzn [label = " nie "]
  w -> wn [label = " niezależne "]
  w -> wz [label = " zależne "]
  wn -> wnt [label = " tak "]
  wn -> wnn [label = " nie "]
  wnt -> ph1
  wnn -> ph2
}', FONT, RAMKA, FONT, RAMKA, NAVY,
    PYT, PYT, PYT, PYT, PYT, PYT, PYT,
    PARAM, NIEPAR, PARAM, NIEPAR, PARAM, NIEPAR, PARAM, NIEPAR, NIEPAR,
    PARAM, NIEPAR)

zapisz(dot_ogolny,     "13_schemat_ogolny.png",     1700)
zapisz(dot_porownanie, "13_schemat_porownanie.png", 1700)
