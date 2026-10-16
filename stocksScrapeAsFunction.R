scrapeStocks <- function(domain, request){
  
  library(tidyverse)
  library(openxlsx)
  library(rvest)
  options(warn=-1)
  
  # https://www.fundamentus.com.br/detalhes.php ---> table with Stock Market Abbreviations
  domain <- "https://www.fundamentus.com.br/detalhes.php"
  request <- "?papel="
  
  # Stock Market Abbreviations ---> the request parameter that changes the given information
  main_page <- read_html(domain)
  patterns <- main_page %>% html_nodes(xpath = "//*[@id='test1']/tbody/tr/td[1]/a") %>% html_text()
  
  # The first parameter had an added whitespace into the string, that was causing error
  patterns <- str_replace_all(patterns, fixed(" "), "")
  
  # Building a dataframe with the entire url
  webpages <- data.frame(str_c(domain, request, patterns[1:length(patterns)]))
  colnames(webpages) <- "urls"
  
  # Storing all the informations from every stock in a list
  informations <- list()
  for(i in 1:nrow(webpages)){
    webpage <- read_html(webpages$urls[i])
    informations[i] <- list(webpage %>% html_nodes(xpath = "/html/body/div[1]/div[2]/table") %>% html_text())
    
    informations[[i]] <- str_replace_all(informations[[i]], fixed("\t"), "")
    informations[[i]] <- str_replace_all(informations[[i]], fixed("?"), "")
    informations[[i]] <- str_replace_all(informations[[i]], fixed("\n"), "SPACE")
    informations[[i]] <- str_split(informations[[i]], 'SPACE')
  }
  
  
  
  # Hadling data
  informations <- informations[sapply(informations, length)>0]
  for(i in 1:length(informations)){
    informations[[i]][[1]] <- informations[[i]][[1]][1:20]
    informations[[i]][[2]] <- informations[[i]][[2]][1:8]
    informations[[i]][[3]] <- str_replace_all(informations[[i]][[3]], fixed("          "), "")
    informations[[i]][[3]] <- informations[[i]][[3]][!nchar(informations[[i]][[3]]) == 0]
    informations[[i]][[3]] <- informations[[i]][[3]][3:length(informations[[i]][[3]])]
    informations[[i]][[4]] <- str_replace_all(informations[[i]][[4]], fixed(" "), "")
    informations[[i]][[4]] <- str_replace_all(informations[[i]][[4]], fixed(" "), "")
    informations[[i]][[4]] <- informations[[i]][[4]][2:(length(informations[[i]][[4]])-1)]
    informations[[i]][[5]] <- str_replace_all(informations[[i]][[5]], fixed(" "), "")
    informations[[i]][[5]] <- informations[[i]][[5]][2:(length(informations[[i]][[5]])-1)]
  }
  
  # There are two types of stocks
  # One of them has 8 balance sheet variables
  # The other one has 12 balance sheet variables
  balanceSheet <- list()
  
  for(i in 1:length(informations)){
    if(length(informations[[i]][[4]]) == 8){
      balanceSheet[i] <- list(c(informations[[i]][[1]][2], informations[[i]][[4]][2], 
                                informations[[i]][[4]][8], informations[[i]][[4]][4], 
                                informations[[i]][[4]][6], NA, NA, NA, NA))
    }else if(length(informations[[i]][[4]]) == 12){
      balanceSheet[i] <- list(c(informations[[i]][[1]][2], informations[[i]][[4]][2], 
                                informations[[i]][[4]][12], NA, NA, 
                                informations[[i]][[4]][6], informations[[i]][[4]][10], 
                                informations[[i]][[4]][4], informations[[i]][[4]][8]))
    }
  }
  
  balanceSheet <- data.frame(matrix(unlist(balanceSheet), nrow=length(balanceSheet), byrow=T))
  
  colnames(balanceSheet) <- c('papel', 'ativo', 
                              'patrimLiquido', 'depositos', 
                              'cartDeCredito','disponibilidades', 
                              'ativoCirculante','divBruta', 'divLiquida')
  
  # Creating a dataframe and organizing the data
  stockData <- list()
  for(i in 1:length(informations)){
    stockData[i] <- list(c(informations[[i]][[1]][which(informations[[i]][[1]] == 'Papel')+1], 
                           informations[[i]][[1]][which(informations[[i]][[1]] == 'Cotação')+1], 
                           informations[[i]][[1]][which(informations[[i]][[1]] == 'Tipo')+1], 
                           informations[[i]][[1]][which(informations[[i]][[1]] == 'Data últ cot')+1], 
                           informations[[i]][[1]][which(informations[[i]][[1]] == 'Empresa')+1], 
                           informations[[i]][[1]][which(informations[[i]][[1]] == 'Min 52 sem')+1], 
                           informations[[i]][[1]][which(informations[[i]][[1]] == 'Setor')+1], 
                           informations[[i]][[1]][which(informations[[i]][[1]] == 'Max 52 sem')+1], 
                           informations[[i]][[1]][which(informations[[i]][[1]] == 'Subsetor')+1], 
                           informations[[i]][[1]][which(informations[[i]][[1]] == 'Vol $ méd (2m)')+1], 
                           informations[[i]][[2]][which(informations[[i]][[2]] == 'Valor de mercado')+1],
                           informations[[i]][[2]][which(informations[[i]][[2]] == 'Últ balanço processado')+1], 
                           informations[[i]][[2]][which(informations[[i]][[2]] == 'Valor da firma')+1], 
                           informations[[i]][[2]][which(informations[[i]][[2]] == 'Nro. Ações')+1], 
                           informations[[i]][[3]][which(informations[[i]][[3]] == 'Dia')+1], 
                           informations[[i]][[3]][which(informations[[i]][[3]] == 'Mês')+1], 
                           informations[[i]][[3]][which(informations[[i]][[3]] == '30 dias')+1], 
                           informations[[i]][[3]][which(informations[[i]][[3]] == '12 meses')+1], 
                           informations[[i]][[3]][which(informations[[i]][[3]] == 'P/L')+1], 
                           informations[[i]][[3]][which(informations[[i]][[3]] == 'P/VP')+1],
                           informations[[i]][[3]][which(informations[[i]][[3]] == 'P/EBIT')+1], 
                           informations[[i]][[3]][which(informations[[i]][[3]] == 'PSR')+1], 
                           informations[[i]][[3]][which(informations[[i]][[3]] == 'P/Ativos')+1], 
                           informations[[i]][[3]][which(informations[[i]][[3]] == 'P/Cap. Giro')+1], 
                           informations[[i]][[3]][which(informations[[i]][[3]] == 'P/Ativ Circ Liq')+1], 
                           informations[[i]][[3]][which(informations[[i]][[3]] == 'Div. Yield')+1], 
                           informations[[i]][[3]][which(informations[[i]][[3]] == 'EV / EBITDA')+1], 
                           informations[[i]][[3]][which(informations[[i]][[3]] == ' EV / EBITDA')+1], 
                           informations[[i]][[3]][which(informations[[i]][[3]] == 'EV / EBIT')+1], 
                           informations[[i]][[3]][which(informations[[i]][[3]] == 'Cres. Rec (5a)')+1], 
                           informations[[i]][[3]][which(informations[[i]][[3]] == 'LPA')+1], 
                           informations[[i]][[3]][which(informations[[i]][[3]] == 'VPA')+1], 
                           informations[[i]][[3]][which(informations[[i]][[3]] == 'Marg. Bruta')+1], 
                           informations[[i]][[3]][which(informations[[i]][[3]] == 'Marg. EBIT')+1], 
                           informations[[i]][[3]][which(informations[[i]][[3]] == 'Marg. Líquida')+1], 
                           informations[[i]][[3]][which(informations[[i]][[3]] == 'EBIT / Ativo')+1], 
                           informations[[i]][[3]][which(informations[[i]][[3]] == ' EBIT / Ativo')+1], 
                           informations[[i]][[3]][which(informations[[i]][[3]] == 'ROIC')+1], 
                           informations[[i]][[3]][which(informations[[i]][[3]] == 'ROE')+1], 
                           informations[[i]][[3]][which(informations[[i]][[3]] == 'Liquidez Corr')+1], 
                           informations[[i]][[3]][which(informations[[i]][[3]] == 'Div Br/ Patrim')+1], 
                           informations[[i]][[3]][which(informations[[i]][[3]] == 'Giro Ativos')+1],
                           informations[[i]][[5]][6], informations[[i]][[5]][10], informations[[i]][[5]][14],
                           informations[[i]][[5]][4], informations[[i]][[5]][8], informations[[i]][[5]][12]))
  }
  
  stockData <- data.frame(matrix(unlist(stockData), nrow=length(stockData), byrow=T))
  
  colnames(stockData) <- c("papel", "cotacao", "tipo", "dataUltCotacao", "empresa", "min52Semanas", 
                           "setor", "max52Semanas", "subsetor", "volMedioDolares2meses", 
                           "valorDeMercado", "ultimoBalancoProcessado", "valorDaFirma", "numeroDeAcoes",
                           'var_perc_dia', 'var_perc_mes', 'var_perc_trintaDias', 
                           'var_perc_dozeMeses', 'P_L', 'P_VP', 'P_EBIT', 'PSR', 'P_Ativos', 'P_CapitalGiro', 
                           'P_AtivoCircLiquido', 'perc_DivYield', 'EV_EBITDA', 
                           'EV_EBIT', 'perc_CrescRec5Anos', 'LPA', 'VPA', 'perc_MargBruta', 
                           'perc_MargEBIT', 'perc_MargLiquida', 'perc_EBIT_Ativo', 
                           'perc_ROIC', 'perc_ROE', 'LiquidezCorr', 'DivBrutaTotal_PatrimLiq', 'GiroAtivos',
                           'receitaLiquida3', 'EBIT3', 'lucroLiquido3',
                           'receitaLiquida12', 'EBIT12', 'lucroLiquido12')
  
  
  stockData <- left_join(stockData, balanceSheet, 
                         by = "papel")
  
  #############################################
  # Hadling data - Project Stocks - Second Part
  
  
  # Variation - oscilações - porcentagem
  
  stockData$var_perc_dia <- stockData$var_perc_dia %>% str_replace("%", "") %>% str_replace(",", ".")
  stockData$var_perc_dia <- as.numeric(stockData$var_perc_dia)
  
  stockData$var_perc_mes <- stockData$var_perc_mes %>% str_replace("%", "") %>% str_replace(",", ".")
  stockData$var_perc_mes <- as.numeric(stockData$var_perc_mes)
  
  stockData$var_perc_trintaDias <- stockData$var_perc_trintaDias %>% str_replace("%", "") %>% str_replace(",", ".")
  stockData$var_perc_trintaDias <- as.numeric(stockData$var_perc_trintaDias)
  
  stockData$var_perc_dozeMeses <- stockData$var_perc_dozeMeses %>% str_replace("%", "") %>% str_replace(",", ".")
  stockData$var_perc_dozeMeses <- as.numeric(stockData$var_perc_dozeMeses)
  
  # Income Statement - DRE - 3 months
  
  stockData$receitaLiquida3 <- stockData$receitaLiquida3 %>% str_replace_all("\\.", "")
  stockData$receitaLiquida3 <- as.numeric(stockData$receitaLiquida3)
  
  stockData$EBIT3 <- stockData$EBIT3 %>% str_replace_all("\\.", "")
  stockData$EBIT3 <- as.numeric(stockData$EBIT3)
  
  stockData$lucroLiquido3 <- stockData$lucroLiquido3 %>% str_replace_all("\\.", "")
  stockData$lucroLiquido3 <- as.numeric(stockData$lucroLiquido3)
  
  
  # Income Statement - DRE - 12 months
  
  stockData$receitaLiquida12 <- stockData$receitaLiquida12 %>% str_replace_all("\\.", "")
  stockData$receitaLiquida12 <- as.numeric(stockData$receitaLiquida12)
  
  stockData$EBIT12 <- stockData$EBIT12 %>% str_replace_all("\\.", "")
  stockData$EBIT12 <- as.numeric(stockData$EBIT12)
  
  stockData$lucroLiquido12 <- stockData$lucroLiquido12 %>% str_replace_all("\\.", "")
  stockData$lucroLiquido12 <- as.numeric(stockData$lucroLiquido12)
  
  
  # Indicadores fundamentalistas - fundamental indicators
  
  stockData$P_L <- stockData$P_L %>% str_replace("\\.","") %>% str_replace(",",".")
  stockData$P_L <- as.numeric(stockData$P_L)
  
  stockData$P_VP <- stockData$P_VP %>% str_replace("\\.","") %>% str_replace(",",".")
  stockData$P_VP <- as.numeric(stockData$P_VP)
  
  stockData$P_EBIT <- stockData$P_EBIT %>% str_replace("\\.","") %>% str_replace(",",".")
  stockData$P_EBIT[stockData$P_EBIT == '-'] <- NA
  stockData$P_EBIT <- as.numeric(stockData$P_EBIT)
  
  stockData$P_Ativos <- stockData$P_Ativos %>% str_replace("\\.","") %>% str_replace(",",".")
  stockData$P_Ativos[stockData$P_Ativos == '-'] <- NA
  stockData$P_Ativos <- as.numeric(stockData$P_Ativos)
  
  stockData$P_CapitalGiro <- stockData$P_CapitalGiro %>% str_replace("\\.","") %>% str_replace(",",".")
  stockData$P_CapitalGiro[stockData$P_CapitalGiro == '-'] <- NA
  stockData$P_CapitalGiro <- as.numeric(stockData$P_CapitalGiro)
  
  stockData$P_AtivoCircLiquido <- stockData$P_AtivoCircLiquido %>% str_replace("\\.","") %>% str_replace(",",".")
  stockData$P_AtivoCircLiquido[stockData$P_AtivoCircLiquido == '-'] <- NA
  stockData$P_AtivoCircLiquido <- as.numeric(stockData$P_AtivoCircLiquido)
  
  stockData$PSR <- stockData$PSR %>% str_replace("\\.","") %>% str_replace(",",".")
  stockData$PSR[stockData$PSR == '-'] <- NA
  stockData$PSR <- as.numeric(stockData$PSR)
  
  stockData$EV_EBITDA <- stockData$EV_EBITDA %>% str_replace("\\.","") %>% str_replace(",",".")
  stockData$EV_EBITDA[is.na(as.numeric(stockData$EV_EBITDA))] <- NA
  stockData$EV_EBITDA <- as.numeric(stockData$EV_EBITDA)
  
  stockData$EV_EBIT <- stockData$EV_EBIT %>% str_replace("\\.","") %>% str_replace(",",".")
  stockData$EV_EBIT[is.na(as.numeric(stockData$EV_EBIT))] <- NA
  stockData$EV_EBIT <- as.numeric(stockData$EV_EBIT)
  
  stockData$LPA <- stockData$LPA %>% str_replace("\\.","") %>% str_replace(",",".")
  stockData$LPA <- as.numeric(stockData$LPA)
  
  stockData$VPA <- stockData$VPA %>% str_replace("\\.","") %>% str_replace("\\.","") %>% str_replace(",",".")
  stockData$VPA <- as.numeric(stockData$VPA)
  
  stockData$LiquidezCorr <- stockData$LiquidezCorr %>% str_replace("\\.","") %>% str_replace(",",".")
  stockData$LiquidezCorr[is.na(as.numeric(stockData$LiquidezCorr))] <- NA
  stockData$LiquidezCorr <- as.numeric(stockData$LiquidezCorr)
  
  stockData$DivBrutaTotal_PatrimLiq <- stockData$DivBrutaTotal_PatrimLiq %>% str_replace("\\.","") %>% str_replace(",",".")
  stockData$DivBrutaTotal_PatrimLiq[is.na(as.numeric(stockData$DivBrutaTotal_PatrimLiq))] <- NA
  stockData$DivBrutaTotal_PatrimLiq <- as.numeric(stockData$DivBrutaTotal_PatrimLiq)
  
  stockData$GiroAtivos <- stockData$GiroAtivos %>% str_replace("\\.","") %>% str_replace(",",".")
  stockData$GiroAtivos[is.na(as.numeric(stockData$GiroAtivos))] <- NA
  stockData$GiroAtivos <- as.numeric(stockData$GiroAtivos)
  
  stockData$perc_DivYield <- stockData$perc_DivYield %>% str_replace("%", "") %>% str_replace("\\.","") %>% str_replace(",",".")
  stockData$perc_DivYield[is.na(as.numeric(stockData$perc_DivYield))] <- NA
  stockData$perc_DivYield <- as.numeric(stockData$perc_DivYield)
  
  stockData$perc_CrescRec5Anos <- stockData$perc_CrescRec5Anos %>% str_replace("%", "") %>% str_replace("\\.","") %>% str_replace(",",".")
  stockData$perc_CrescRec5Anos[is.na(as.numeric(stockData$perc_CrescRec5Anos))] <- NA
  stockData$perc_CrescRec5Anos <- as.numeric(stockData$perc_CrescRec5Anos)
  
  stockData$perc_MargBruta <- stockData$perc_MargBruta %>% str_replace("%", "") %>% str_replace("\\.","") %>% str_replace(",",".")
  stockData$perc_MargBruta[stockData$perc_MargBruta == '-'] <- NA
  stockData$perc_MargBruta <- as.numeric(stockData$perc_MargBruta)
  
  stockData$perc_MargEBIT <- stockData$perc_MargEBIT %>% str_replace("%", "") %>% str_replace("\\.","") %>% str_replace(",",".")
  stockData$perc_MargEBIT[stockData$perc_MargEBIT == '-'] <- NA
  stockData$perc_MargEBIT <- as.numeric(stockData$perc_MargEBIT)
  
  stockData$perc_MargLiquida <- stockData$perc_MargLiquida %>% str_replace("%", "") %>% str_replace("\\.","") %>% str_replace(",",".")
  stockData$perc_MargLiquida[stockData$perc_MargLiquida == '-'] <- NA
  stockData$perc_MargLiquida <- as.numeric(stockData$perc_MargLiquida)
  
  stockData$perc_EBIT_Ativo <- stockData$perc_EBIT_Ativo %>% str_replace("%", "") %>% str_replace("\\.","") %>% str_replace(",",".")
  stockData$perc_EBIT_Ativo[is.na(as.numeric(stockData$perc_EBIT_Ativo))] <- NA
  stockData$perc_EBIT_Ativo <- as.numeric(stockData$perc_EBIT_Ativo)
  
  stockData$perc_ROIC <- stockData$perc_ROIC %>% str_replace("%", "") %>% str_replace("\\.","") %>% str_replace(",",".")
  stockData$perc_ROIC[is.na(as.numeric(stockData$perc_ROIC))] <- NA
  stockData$perc_ROIC <- as.numeric(stockData$perc_ROIC)
  
  stockData$perc_ROE <- stockData$perc_ROE %>% str_replace("%", "") %>% str_replace("\\.","") %>% str_replace("\\.","") %>% str_replace(",",".")
  stockData$perc_ROE[is.na(as.numeric(stockData$perc_ROE))] <- NA
  stockData$perc_ROE <- as.numeric(stockData$perc_ROE)
  
  
  # Informações gerais
  
  stockData$cotacao <- stockData$cotacao %>% str_replace("\\.","") %>% str_replace(",",".")
  stockData$cotacao <- as.numeric(stockData$cotacao)
  
  stockData$min52Semanas <- stockData$min52Semanas %>%  str_replace(",",".")
  stockData$min52Semanas <- as.numeric(stockData$min52Semanas)
  
  stockData$max52Semanas <- stockData$max52Semanas %>% str_replace(",",".")
  stockData$max52Semanas <- as.numeric(stockData$max52Semanas)
  
  stockData$volMedioDolares2meses <- stockData$volMedioDolares2meses %>% str_replace_all("\\.", "")
  stockData$volMedioDolares2meses <- as.numeric(stockData$volMedioDolares2meses)
  
  stockData$numeroDeAcoes <- stockData$numeroDeAcoes %>% str_replace_all("\\.", "")
  stockData$numeroDeAcoes <- as.numeric(stockData$numeroDeAcoes)
  
  stockData$valorDeMercado <- stockData$valorDeMercado %>% str_replace_all("\\.", "")
  stockData$valorDeMercado <- as.numeric(stockData$valorDeMercado)
  
  stockData$valorDaFirma <- stockData$valorDaFirma %>% str_replace_all("\\.", "")
  stockData$valorDaFirma[stockData$valorDaFirma == '-'] <- NA
  stockData$valorDaFirma <- as.numeric(stockData$valorDaFirma)
  
  stockData$dataUltCotacao <- as.Date(stockData$dataUltCotacao, format="%d/%m/%Y")
  stockData$ultimoBalancoProcessado <- as.Date(stockData$ultimoBalancoProcessado, format="%d/%m/%Y")
  
  
  # Balanço Patrimonial
  
  stockData$ativo <- stockData$ativo %>% str_replace_all("\\.", "")
  stockData$ativo <- as.numeric(stockData$ativo)
  
  stockData$depositos <- stockData$depositos %>% str_replace_all("\\.", "")
  stockData$depositos <- as.numeric(stockData$depositos)
  
  stockData$cartDeCredito <- stockData$cartDeCredito %>% str_replace_all("\\.", "")
  stockData$cartDeCredito <- as.numeric(stockData$cartDeCredito)
  
  stockData$patrimLiquido <- stockData$patrimLiquido %>% str_replace_all("\\.", "")
  stockData$patrimLiquido <- as.numeric(stockData$patrimLiquido)
  
  stockData$divBruta <- stockData$divBruta %>% str_replace_all("\\.", "")
  stockData$divBruta <- as.numeric(stockData$divBruta)
  
  stockData$disponibilidades <- stockData$disponibilidades %>% str_replace_all("\\.", "")
  stockData$disponibilidades <- as.numeric(stockData$disponibilidades)
  
  stockData$divLiquida <- stockData$divLiquida %>% str_replace_all("\\.", "")
  stockData$divLiquida <- as.numeric(stockData$divLiquida)
  
  stockData$ativoCirculante <- stockData$ativoCirculante %>% str_replace_all("\\.", "")
  stockData$ativoCirculante <- as.numeric(stockData$ativoCirculante)
  
  return(stockData)
  
}

stockData <- scrapeStocks()

write.xlsx(stockData, file = "C:/Users/marco/OneDrive/3. PORTFOLIO-EMPREGO/Data Science/Projects/Personal/R - Web Scraping - Stock Market Data/stocks.xlsx", 
           col.names = TRUE, row.names = FALSE, append = FALSE)

################################################################################################################################################
############################################################# ANÁLISE DE EMPRESAS ##############################################################
################################################################################################################################################

## MÉTODO BAZIN
decioBazin <- data.frame(papel=stockData['papel'][stockData['perc_DivYield'] > 6],
                         empresa=stockData['empresa'][stockData['perc_DivYield'] > 6],
                         setor=stockData['setor'][stockData['perc_DivYield'] > 6],
                         dividendYield=stockData['perc_DivYield'][stockData['perc_DivYield'] > 6],
                         ebit=stockData['EBIT12'][stockData['perc_DivYield'] > 6],
                         dividaLiquida=stockData['divLiquida'][stockData['perc_DivYield'] > 6],
                         # Divida Bruta Total / Patrimonio Liquido
                         DBT_PL=stockData['DivBrutaTotal_PatrimLiq'][stockData['perc_DivYield'] > 6])

decioBazin <- decioBazin[-which(is.na(decioBazin$dividaLiquida)), ]
decioBazin <- decioBazin[-which(decioBazin$ebit == 0), ]
decioBazin <- decioBazin %>% mutate(endividamento=(dividaLiquida/ebit)*100)
decioBazin <- decioBazin[-which(decioBazin$endividamento > 100), ]
