# Coleta de indicadores do Fundamentus. Carregar o arquivo não acessa a rede.
stock_field_map <- c(
  "Papel" = "papel", "Cotação" = "cotacao", "Tipo" = "tipo",
  "Data últ cot" = "dataUltCotacao", "Empresa" = "empresa",
  "Min 52 sem" = "min52Semanas", "Setor" = "setor",
  "Max 52 sem" = "max52Semanas", "Subsetor" = "subsetor",
  "Vol $ méd (2m)" = "volMedioReais2m",
  "Valor de mercado" = "valorDeMercado",
  "Últ balanço processado" = "ultimoBalancoProcessado",
  "Valor da firma" = "valorDaFirma", "Nro. Ações" = "numeroDeAcoes",
  "Dia" = "var_perc_dia", "Mês" = "var_perc_mes",
  "30 dias" = "var_perc_trintaDias", "12 meses" = "var_perc_dozeMeses",
  "P/L" = "P_L", "P/VP" = "P_VP", "P/EBIT" = "P_EBIT",
  "PSR" = "PSR", "P/Ativos" = "P_Ativos",
  "P/Cap. Giro" = "P_CapitalGiro",
  "P/Ativ Circ Liq" = "P_AtivoCircLiquido",
  "Div. Yield" = "perc_DivYield", "EV / EBITDA" = "EV_EBITDA",
  "EV / EBIT" = "EV_EBIT", "Cres. Rec (5a)" = "perc_CrescRec5Anos",
  "LPA" = "LPA", "VPA" = "VPA",
  "Marg. Bruta" = "perc_MargBruta", "Marg. EBIT" = "perc_MargEBIT",
  "Marg. Líquida" = "perc_MargLiquida", "EBIT / Ativo" = "perc_EBIT_Ativo",
  "ROIC" = "perc_ROIC", "ROE" = "perc_ROE",
  "Liquidez Corr" = "LiquidezCorr",
  "Dív Líq / Patrim" = "divLiquidaPatrimLiq",
  "Giro Ativos" = "GiroAtivos", "Ativo" = "ativo",
  "Patrim. Líq" = "patrimLiquido", "Depósitos" = "depositos",
  "Cart. de Crédito" = "cartDeCredito",
  "Disponibilidades" = "disponibilidades",
  "Ativo Circulante" = "ativoCirculante",
  "Dív. Bruta" = "divBruta", "Dív. Líquida" = "divLiquida"
)

income_fields <- c(
  "receitaLiquida12", "receitaLiquida3", "EBIT12", "EBIT3",
  "lucroLiquido12", "lucroLiquido3", "resultIntFinanc12",
  "resultIntFinanc3", "recServicos12", "recServicos3"
)
date_fields <- c("dataUltCotacao", "ultimoBalancoProcessado")
text_fields <- c("papel", "tipo", "empresa", "setor", "subsetor")
numeric_fields <- setdiff(c(unname(stock_field_map), income_fields),
                          c(text_fields, date_fields))

new_stock_row <- function() {
  fields <- unique(c(unname(stock_field_map), income_fields,
                     "fonte_url", "coletadoEm"))
  row <- as.data.frame(as.list(stats::setNames(
    rep(NA_character_, length(fields)), fields
  )), stringsAsFactors = FALSE, check.names = FALSE)
  for (field in c(numeric_fields, income_fields)) row[[field]] <- NA_real_
  for (field in date_fields) row[[field]] <- as.Date(NA_character_)
  row$coletadoEm <- as.POSIXct(NA, tz = "UTC")
  row
}

clean_label <- function(x) {
  x <- gsub("\u00a0", " ", x, fixed = TRUE)
  trimws(gsub("[[:space:]]+", " ", x))
}

parse_br_number <- function(x) {
  x <- clean_label(as.character(x))
  x[x %in% c("", "-", "?", "N/D", "n/d")] <- NA_character_
  x <- gsub("[R$%[:space:]]", "", x)
  x <- gsub(".", "", x, fixed = TRUE)
  x <- sub(",", ".", x, fixed = TRUE)
  result <- suppressWarnings(as.numeric(x))
  if (any(!is.na(x) & is.na(result))) {
    warning("Um valor numérico não pôde ser interpretado.", call. = FALSE)
  }
  result
}

extract_pairs <- function(table) {
  rows <- rvest::html_elements(table, "tr")
  pairs <- lapply(rows, function(row) {
    cells <- rvest::html_elements(row, "td")
    classes <- xml2::xml_attr(cells, "class")
    classes[is.na(classes)] <- ""
    labels <- which(grepl("(^|[[:space:]])label([[:space:]]|$)", classes))
    lapply(labels, function(i) {
      if (i == length(cells) ||
          !grepl("(^|[[:space:]])data([[:space:]]|$)", classes[i + 1L])) {
        return(NULL)
      }
      c(label = clean_label(rvest::html_text2(cells[[i]])),
        value = clean_label(rvest::html_text2(cells[[i + 1L]])))
    })
  })
  pairs <- unlist(pairs, recursive = FALSE)
  if (!length(pairs)) {
    return(data.frame(label = character(), value = character()))
  }
  as.data.frame(do.call(rbind, pairs), stringsAsFactors = FALSE)
}

parse_stock_page <- function(page, source_url = NA_character_,
                             collected_at = Sys.time()) {
  tables <- rvest::html_elements(page, "table.w728")
  if (length(tables) < 4L) {
    stop("As tabelas de detalhes não foram encontradas. O site pode ter mudado.",
         call. = FALSE)
  }
  pairs <- do.call(rbind, lapply(tables, extract_pairs))
  row <- new_stock_row()
  for (label in names(stock_field_map)) {
    values <- pairs$value[pairs$label == label]
    if (length(values)) row[[stock_field_map[[label]]]] <- values[[1L]]
  }

  # Na DRE, o primeiro valor é de 12 meses e o segundo é de 3 meses.
  income_index <- which(vapply(tables, function(table) {
    grepl("Dados demonstrativos de resultados",
          rvest::html_text2(table), fixed = TRUE)
  }, logical(1)))
  if (length(income_index)) {
    income <- extract_pairs(tables[[income_index[[1L]]]])
    labels <- c("Receita Líquida" = "receitaLiquida",
                "EBIT" = "EBIT", "Lucro Líquido" = "lucroLiquido",
                "Result Int Financ" = "resultIntFinanc",
                "Rec Serviços" = "recServicos")
    for (label in names(labels)) {
      values <- income$value[income$label == label]
      if (length(values) >= 1L) row[[paste0(labels[[label]], "12")]] <- values[[1L]]
      if (length(values) >= 2L) row[[paste0(labels[[label]], "3")]] <- values[[2L]]
    }
  }

  if (is.na(row$papel) || !nzchar(row$papel)) {
    stop("A página não trouxe um ticker válido.", call. = FALSE)
  }
  for (field in c(numeric_fields, income_fields)) {
    row[[field]] <- parse_br_number(row[[field]])
  }
  for (field in date_fields) {
    row[[field]] <- as.Date(row[[field]], format = "%d/%m/%Y")
  }
  if (is.na(row$cotacao) || is.na(row$valorDeMercado)) {
    stop("Cotação ou valor de mercado ausente na página.", call. = FALSE)
  }
  row$fonte_url <- source_url
  row$coletadoEm <- as.POSIXct(collected_at, tz = "UTC")
  row
}

fetch_stock_html <- function(url, timeout_seconds = 20, max_attempts = 2L) {
  for (attempt in seq_len(max_attempts)) {
    response <- tryCatch(
      httr::GET(url, httr::timeout(timeout_seconds),
                httr::user_agent("R-Web-Scraping-Stock-Market-Data/2.0")),
      error = identity
    )
    if (!inherits(response, "error")) {
      status <- httr::status_code(response)
      if (status == 200L) {
        return(xml2::read_html(httr::content(response, as = "raw"),
                               encoding = "ISO-8859-1"))
      }
      if (!(status == 429L || status >= 500L)) {
        stop(sprintf("HTTP %s em %s", status, url), call. = FALSE)
      }
      message_text <- sprintf("HTTP %s em %s", status, url)
    } else {
      message_text <- conditionMessage(response)
    }
    if (attempt < max_attempts) Sys.sleep(attempt * 2)
  }
  stop(message_text, call. = FALSE)
}

list_stock_tickers <- function(page) {
  links <- rvest::html_elements(page, "#test1 tbody tr td:first-child a")
  tickers <- toupper(clean_label(rvest::html_text2(links)))
  unique(tickers[grepl("^[A-Z][A-Z0-9]{3,7}$", tickers)])
}

scrapeStocks <- function(tickers = NULL,
                         base_url = "https://www.fundamentus.com.br/detalhes.php",
                         pause_seconds = 1.5, max_attempts = 2L,
                         timeout_seconds = 20) {
  if (!is.numeric(pause_seconds) || length(pause_seconds) != 1L ||
      is.na(pause_seconds) || pause_seconds < 1) {
    stop("pause_seconds deve ser pelo menos 1 segundo.", call. = FALSE)
  }
  if (!is.numeric(max_attempts) || length(max_attempts) != 1L ||
      is.na(max_attempts) || max_attempts < 1 ||
      max_attempts != as.integer(max_attempts)) {
    stop("max_attempts deve ser um inteiro positivo.", call. = FALSE)
  }
  if (!is.numeric(timeout_seconds) || length(timeout_seconds) != 1L ||
      is.na(timeout_seconds) || timeout_seconds <= 0) {
    stop("timeout_seconds deve ser positivo.", call. = FALSE)
  }
  if (!is.character(base_url) || length(base_url) != 1L ||
      !grepl("^https://", base_url)) {
    stop("base_url deve ser uma URL HTTPS.", call. = FALSE)
  }
  base_url <- sub("\\?.*$", "", base_url)
  make_url <- function(ticker) {
    paste0(base_url, "?interface=classic&papel=",
           utils::URLencode(ticker, reserved = TRUE))
  }

  if (is.null(tickers)) {
    listing <- fetch_stock_html(paste0(base_url, "?interface=classic"),
                                timeout_seconds, max_attempts)
    tickers <- list_stock_tickers(listing)
    if (!length(tickers)) {
      stop("Nenhum ticker foi encontrado na listagem.", call. = FALSE)
    }
  } else {
    tickers <- unique(toupper(clean_label(tickers)))
    if (!length(tickers) || anyNA(tickers) ||
        any(!grepl("^[A-Z][A-Z0-9]{3,7}$", tickers))) {
      stop("Informe tickers válidos, por exemplo PETR4 ou ITUB4.", call. = FALSE)
    }
  }

  rows <- list()
  errors <- data.frame(papel = character(), motivo = character())
  consecutive_failures <- 0L
  for (ticker in tickers) {
    Sys.sleep(pause_seconds)
    url <- make_url(ticker)
    result <- tryCatch({
      row <- parse_stock_page(fetch_stock_html(url, timeout_seconds, max_attempts),
                              source_url = url)
      if (!identical(row$papel[[1L]], ticker)) {
        stop("O ticker retornado não corresponde ao solicitado.", call. = FALSE)
      }
      row
    }, error = identity)
    if (inherits(result, "error")) {
      errors <- rbind(errors, data.frame(papel = ticker,
                                         motivo = conditionMessage(result)))
      consecutive_failures <- consecutive_failures + 1L
      if (consecutive_failures >= 5L) {
        warning("Coleta interrompida após cinco falhas seguidas.", call. = FALSE)
        break
      }
      next
    }
    rows[[length(rows) + 1L]] <- result
    consecutive_failures <- 0L
  }
  data <- if (length(rows)) do.call(rbind, rows) else new_stock_row()[FALSE, ]
  rownames(data) <- NULL
  attr(data, "errors") <- errors
  data
}

write_stock_data <- function(data, path = "stocks_atualizados.xlsx") {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("Instale o pacote openxlsx para exportar a planilha.", call. = FALSE)
  }
  openxlsx::write.xlsx(data, file = path, overwrite = TRUE)
  invisible(path)
}

screen_dividend_candidates <- function(data, min_yield = 6,
                                       max_net_debt_to_ebit = 1) {
  required <- c("papel", "empresa", "setor", "perc_DivYield",
                "EBIT12", "divLiquida")
  if (!all(required %in% names(data))) {
    stop("A base não contém as colunas necessárias para a triagem.", call. = FALSE)
  }
  ratio <- data$divLiquida / data$EBIT12
  selected <- !is.na(data$perc_DivYield) & data$perc_DivYield >= min_yield &
    !is.na(data$EBIT12) & data$EBIT12 > 0 &
    !is.na(ratio) & ratio <= max_net_debt_to_ebit
  result <- data[selected, required, drop = FALSE]
  result$dividaLiquida_EBIT12 <- ratio[selected]
  result[order(-result$perc_DivYield), , drop = FALSE]
}
