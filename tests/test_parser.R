source("stocksScrapeAsFunction.R")

table_html <- function(rows) {
  paste0("<table class='w728'>", paste(rows, collapse = ""), "</table>")
}
pair <- function(label, value) {
  paste0("<td class='label'>", label, "</td><td class='data'>", value, "</td>")
}
row_html <- function(...) paste0("<tr>", paste0(..., collapse = ""), "</tr>")

fixture <- function(ticker, bank = FALSE) {
  identity <- table_html(c(
    row_html(pair("Papel", ticker), pair("Cotação", "1.234,56")),
    row_html(pair("Empresa", "Empresa Exemplo"),
             pair("Data últ cot", "02/10/2026"))
  ))
  market <- table_html(row_html(
    pair("Valor de mercado", "1.200.000"),
    pair("Últ balanço processado", "30/06/2026")
  ))
  indicators <- table_html(c(
    row_html("<td>Indicadores fundamentalistas</td>"),
    row_html(pair("Div. Yield", "7,1%"),
             pair("P/L", "-"), pair("ROE", "12,3%"))
  ))
  balance <- if (bank) {
    table_html(row_html(pair("Ativo", "10.000"),
                        pair("Depósitos", "8.000"),
                        pair("Cart. de Crédito", "5.000")))
  } else {
    table_html(row_html(pair("Ativo", "10.000"),
                        pair("Dív. Líquida", "300"),
                        pair("Dív. Bruta", "500")))
  }
  income <- if (bank) {
    table_html(c(
      row_html("<td>Dados demonstrativos de resultados</td>"),
      row_html(pair("Result Int Financ", "900"),
               pair("Result Int Financ", "200")),
      row_html(pair("Rec Serviços", "400"),
               pair("Rec Serviços", "100")),
      row_html(pair("Lucro Líquido", "120"),
               pair("Lucro Líquido", "30"))
    ))
  } else {
    table_html(c(
      row_html("<td>Dados demonstrativos de resultados</td>"),
      row_html(pair("Receita Líquida", "2.000"),
               pair("Receita Líquida", "500")),
      row_html(pair("EBIT", "400"), pair("EBIT", "100")),
      row_html(pair("Lucro Líquido", "200"),
               pair("Lucro Líquido", "50"))
    ))
  }
  xml2::read_html(paste0("<html><body>", identity, market, indicators,
                         balance, income, "</body></html>"))
}

stopifnot(identical(parse_br_number(c("1.234,56", "-2,3%", "-")),
                    c(1234.56, -2.3, NA_real_)))

company <- parse_stock_page(fixture("TEST3"),
                            source_url = "https://example.org/TEST3")
bank <- parse_stock_page(fixture("BANK4", bank = TRUE))
stopifnot(
  identical(company$papel, "TEST3"),
  identical(company$cotacao, 1234.56),
  identical(company$perc_DivYield, 7.1),
  is.na(company$P_L),
  identical(company$receitaLiquida12, 2000),
  identical(company$receitaLiquida3, 500),
  identical(company$EBIT12, 400),
  identical(company$divLiquida, 300),
  is.na(company$depositos),
  inherits(company$dataUltCotacao, "Date"),
  identical(bank$resultIntFinanc12, 900),
  identical(bank$resultIntFinanc3, 200),
  identical(bank$recServicos12, 400),
  is.na(bank$EBIT12),
  identical(bank$depositos, 8000)
)

candidates <- screen_dividend_candidates(company)
stopifnot(nrow(candidates) == 1L,
          identical(candidates$dividaLiquida_EBIT12, 0.75),
          nrow(screen_dividend_candidates(bank)) == 0L)

invalid <- tryCatch(parse_stock_page(xml2::read_html("<html></html>")),
                    error = identity)
stopifnot(inherits(invalid, "error"))
cat("Parser e triagem: OK\n")

