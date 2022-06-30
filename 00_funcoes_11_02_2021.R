#Pacotes necessários


require(lubridate)
require(dplyr)
require(rvest)
require(xml2)
require(XML)
require(padr)
require(tidyr)
require(purrr)
require(stringr)
require(sf)
require(hydroTSM)
require(ggplot2)
require(randtests)
require(Kendall)
require(trend)
require(lfstat)
require(geobr)

sf_use_s2(FALSE)


#######Função inventários estaçòes por estado

###Entrada:

##Nome do estado deve ser inserido com letra maiuscula e acentuado.
##fluviométricas (tipo 1) ou pluviométricas (tipo 2)

###Saída:

##Lista de estações no estado número de estações 


#estado = c("MATO GROSSO", "RIO DE JANEIRO")
#estado = c("ESPÍRITO SANTO")
#i = 1
#tipo = "plu"

inventory <- function(estado, stationType = "plu") {
  serief <- list()

  if (stationType == "flu") {
    stationType <- 1
  } else {
    stationType <- 2
  }

  for (i in 1:length(estado)) {
    estadoG <- gsub(" ", "%20", estado[i])
    
    html_raw1 <- read_html(paste("http://telemetriaws1.ana.gov.br/ServiceANA.asmx/HidroInventario?codEstDE=&codEstATE=&tpEst=", stationType, "&nmEst=&nmRio=&codSubBacia=&codBacia=&nmMunicipio=&nmEstado=", estadoG, "&sgResp=&sgOper=&telemetrica=", sep = ""))

    estac <- as.data.frame(cbind(
      xml_text(xml_contents(xml_find_all(html_raw1, ".//nmestado"))),
      xml_double(xml_contents(xml_find_all(html_raw1, ".//codigo"))),
      xml_double(xml_contents(xml_find_all(html_raw1, ".//latitude"))),
      xml_double(xml_contents(xml_find_all(html_raw1, ".//longitude"))),
      xml_double(xml_find_all(html_raw1, ".//areadrenagem"))
    ))

    estac <- filter(estac, V1 == estado[i]) %>%
      distinct(V2, .keep_all = TRUE) %>%
      set_names(c("state", "station_code", "lat", "long", "area_km2"))

    names(estac) <- c("state", "station_code", "lat", "long", "area_km2")

    serief[[i]] <- estac
    print(estado[i])
  }

  serief <- do.call(rbind, serief)
  
  if (stationType == 2){
    serief = serief %>% 
      mutate(stationType = "pluviometric") %>% 
      select(-area_km2)
    
  } else {serief = serief %>% 
    mutate(stationType = "fluviometric")}
  return(serief)
}



####### Função para baixar dados de várias estações
##Entrada:
#vetor com código das estaçoes
#deletar ou não estações sem dados

##Saída
#lista com dados de cada estação separadas em diferentes elementos

# inventoryResult = invEstFlu
stationsData <- function(inventoryResult, deleteNAstations = TRUE) {
  
  serie <- list()

  if (inventoryResult$stationType[1] == "fluviometric") {
    stationType <- 3
  } else if (inventoryResult$stationType[1] == "pluviometric") {
    stationType <- 2
  } else {
    stop("inexistent stationType parameter")
  }

  for (i in 1:nrow(inventoryResult)) {
    streamflow_station_number <- inventoryResult$station_code[i]

    html_raw <- read_html(paste("http://telemetriaws1.ana.gov.br/ServiceANA.asmx/HidroSerieHistorica?codEstacao=", streamflow_station_number, "&dataInicio=&dataFim=&tipoDados=", stationType, "&nivelConsistencia=", sep = ""))

    streamflow_html_node <- xml_children(xml_find_all(html_raw, ".//documentelement"))

    streamflow_list <- as_list(streamflow_html_node)

    streamflow_list <- lapply(streamflow_list, lapply, function(x) {
      if (length(x) == 0) {
        list(NA)
      } else {
        x
      }
    })

    streamflow_list <- lapply(streamflow_list, unlist)

    streamflow_df <- do.call(rbind.data.frame, streamflow_list)

    colnames(streamflow_df) <- names(streamflow_list[[1]])

    if (ncol(streamflow_df) == 1) {
      serie[[i]] <- "No Data"
    } else {
      streamflow_df$datahora <- as.Date(streamflow_df$datahora)
      streamflow_df <- streamflow_df %>% rename(data = datahora)
      serie[[i]] <- streamflow_df
    }

    print(paste(i, "/", length(inventoryResult$station_code), " (station ", inventoryResult$station_code[i], " Done)", sep = ""))
  }


  names(serie) <- inventoryResult$station_code

  if (deleteNAstations == TRUE) {
    serie <- serie[serie != "No Data"]
  }

  return(serie)
}


#remove(dadosEst)
#dadosEst = stationsData(estac[1:20,])

#######Pré-tratamento dos dados
##Entrada:
#Lista com dados de cada estação (resultado da função anterior)
#Essa função transforma os dados do hidroweb em forma temporal

##Saída:
#dados do hidroweb em forma temporal

##OBS: Há estações com dados repetidos. Foi escolhido o primeito dados como real

##Créditos: https://github.com/ArturLourenco/HidroWebFix

# dados = dadosFlu

organize <- function(dados) {
  
  serie <- list()
  
  if(ncol(dados[[1]] %>% dplyr:: select(starts_with("vazao")))>0){
    stationType = "flu"
  } else {stationType = "plu"} #identificar se estação é flu ou plu
  
  
  if (stationType == "flu") {
    for (i in 1:length(dados)) {
      serie[[i]] <- dados[[i]] %>%
        set_names(~ str_to_lower(.)) %>%
        dplyr::select(estacaocodigo, data, nivelconsistencia, starts_with("vazao"), -ends_with("status")) %>%
        pivot_longer(
          cols = starts_with("vazao"),
          values_drop_na = FALSE
        ) %>%
        dplyr::mutate(day = stringr::str_remove_all(name, "vazao")) %>%
        dplyr::mutate(month = lubridate::month(data), year = lubridate::year(data)) %>%
        unite(data, year, month, day, sep = "-") %>%
        dplyr::mutate(date = as.Date(data)) %>%
        dplyr::mutate(
          streamflow_m3_s = as.numeric(value),
          station_code = estacaocodigo,
          consistency_level = nivelconsistencia
        ) %>%
        filter(!is.na(date)) %>%
        dplyr::select(station_code, consistency_level, date, streamflow_m3_s) %>%
        pad()

      serie[[i]] <- serie[[i]] %>% # retirar datas duplas (preferencias por dados consistidos)
        group_by(date) %>%
        filter(consistency_level == max(consistency_level)) %>%
        ungroup() %>% 
        group_by(date) %>% 
        filter(row_number()==1)
    }
  } else {
    for (i in 1:length(dados)) {
      serie[[i]] <- dados[[i]] %>%
        set_names(~ str_to_lower(.)) %>%
        dplyr::select(estacaocodigo, data, nivelconsistencia, starts_with("chuva"), -ends_with("status")) %>%
        pivot_longer(
          cols = starts_with("chuva"),
          values_drop_na = FALSE
        ) %>%
        dplyr::mutate(day = stringr::str_remove_all(name, "chuva")) %>%
        dplyr::mutate(month = lubridate::month(data), year = lubridate::year(data)) %>%
        unite(data, year, month, day, sep = "-") %>%
        dplyr::mutate(date = as.Date(data)) %>%
        dplyr::mutate(
          rainfall_mm = as.numeric(value),
          station_code = estacaocodigo,
          consistency_level = nivelconsistencia
        ) %>%
        filter(!is.na(date)) %>%
        dplyr::select(station_code, consistency_level, date, rainfall_mm) %>%
        pad()


      serie[[i]] <- serie[[i]] %>% # retirar datas duplas (preferencias por dados consistidos)
        group_by(date) %>%
        filter(consistency_level == max(consistency_level)) %>%
        ungroup() %>% 
        group_by(date) %>% 
        filter(row_number()==1)

    }
  }
  

  names(serie) <- names(dados)

  serie <- serie[lapply(serie, nrow) > 0]

  return(serie)
}


#dadosEstOrg = organize(dadosEst)


#######################################plotar hidrograma para ver ano hidrológico

# organizeResult = dadosFluSerie
mMonthlyPlot = function(organizeResult,
                        maxFailurePorcent = 5,
                        minimalYears = 10,
                        consistedOnly = FALSE){

  if (names(organizeResult[[1]])[4] == "streamflow_m3_s"){
    stationType = "flu"
  } else {stationType = "plu"}
  
  organizeResult = lapply(organizeResult, function(x) {names(x) = c(names(x)[-4], "value"); x})
  
  serie = list()
  
  if (consistedOnly == TRUE){
    for (i in 1:length(organizeResult)){
      organizeResult[[i]] = organizeResult[[i]] %>% filter(consistency_level == 2)
    }
    organizeResult = organizeResult[lapply(organizeResult, nrow)>0]
  }
  
  #Para cada estação:
  
    # identificar anos com mais menos que X porcento de falha e série anual incompleta
    for (i in 1:length(organizeResult)){
      yearsComplete = organizeResult[[i]] %>% 
        mutate(na = is.na(value),                                  #número de NAs e Ano do Dado
               year = lubridate::year(date)) %>% 
        group_by(year) %>%                                         #agrupar por anos
        dplyr::summarize(count = length(date),                       #número leituras no ano
                         na = sum(na)) %>%                         #número de NAs no ao
        mutate(diasTotais = sapply(year, function(x) diy(x, out.type = "nmbr"))) %>%
        mutate(porcNA = na/diasTotais*100,        #porcentagem de dados sem NA
               porcCount = count/diasTotais*100) %>% 
        mutate(estacao = na.omit(unique(organizeResult[[i]]$station_code)),
               porcent = porcNA<=maxFailurePorcent,
               porcent2 = porcCount>=100-maxFailurePorcent) %>% 
        filter(porcent == TRUE & porcent2 == TRUE) %>% 
        dplyr::select(year)
      
    #Se número de anos for superior ao estabelecido, calcular média mensal de precipitação e vazão, caso contrário desconsiderar do gráfico (NULL)
      
      if(nrow(yearsComplete)>=minimalYears){
      
      dataYears = organizeResult[[i]] %>%
        mutate(year = lubridate::year(date)) %>% 
        filter(year %in% yearsComplete$year)
      
      serie[[i]] = dataYears %>% 
        mutate(monthYear = format(as.Date(date), "%Y-%m")) %>% 
        group_by(monthYear) %>%
        summarise(monthYearRaifallSum = sum(value, na.rm = TRUE), monthYearStreamFlowMean = mean(value, na.rm = TRUE)) %>%  #série total mensal e media mensal
        mutate(month = sapply(monthYear, function(x) substring(x, 6,7))) %>%
        group_by(month) %>% 
        summarise(monthYearRaifallSumMean = mean(monthYearRaifallSum, na.rm = TRUE),
                  monthYearStreamFlowMean2 = mean(monthYearStreamFlowMean, na.rm = TRUE)) %>%
        mutate(station_code = na.omit(unique(organizeResult[[i]]$station_code))) %>% 
        dplyr::select(station_code, month, monthYearRaifallSumMean, monthYearStreamFlowMean2)
      } else {serie[[i]] = NULL}
    }
  
  
  serie[sapply(serie, is.null)] <- NULL
  
  #gráficos
  
  if(stationType=="flu"){
  
   res = ggplot(do.call(rbind, serie))+
      aes(month, monthYearStreamFlowMean2, group = 1)+
      facet_wrap(vars(station_code), scale = "free")+
      geom_col() +
      theme_grey(base_size = 14)+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      labs(x = "Month",
           y = "Mean streamflow (m³/s)")
  } else {
    
    res = ggplot(do.call(rbind, serie))+
      aes(month, monthYearRaifallSumMean, group = 1)+
      geom_col() +
      facet_wrap(vars(station_code), scale = "free")+
      theme_grey(base_size = 14)+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      labs(x = "Month",
           y = "Mean rainfall (mm)")
    
  }
  
  return(res)
}



#Selecionar estações ----
# dados = dadosFluSerie

selectStations <- function(dados,
                           mode = "yearly",
                           maxFailurePorcent = 10,
                           minimalYears = 15,
                           month = "Jan",
                           inicialYear = NULL,
                           finalYear = NULL,
                           consistedOnly = FALSE) { #----

  
  serie <- list()
  
  if (names(dados[[1]])[4] == "streamflow_m3_s"){
    stationType = "flu"
  } else {stationType = "plu"}
  
  #filtrar elementos da lista com base no ano inicial
  if (!is.null(inicialYear)){
    dados = lapply(dados, function(x) x %>% dplyr::filter(date >= as.Date(paste("01","01",inicialYear, sep = "-"), format = "%d-%m-%Y")))
    dados = dados[lapply(dados, nrow)>0]
  }
  # #filtrar elementos da lista com base no ano final
  # if (!is.null(finalYear)){
  #   dados = lapply(dados, function(x) x %>% dplyr::filter(date < as.Date(paste("01","01",finalYear+1, sep = "-"), format = "%d-%m-%Y")))
  #   dados = dados[lapply(dados, nrow)>0]
  # }

  # select consisted?

  if (consistedOnly == TRUE) {
    for (i in 1:length(dados)) {
      dados[[i]] <- dados[[i]] %>% filter(consistency_level == 2)
    }
    dados <- dados[lapply(dados, nrow) > 0]
  }


  # create completed time series and set hydrological year

  monthChosen <- base::match(month, month.abb)
  # Para cada estação:

  i <- 1
  for (i in 1:length(dados)) {

    # identificar primeiro e último ano da série

    firstYear <- lubridate::year(dados[[i]]$date[1])
    lastYear <- lubridate::year(tail(dados[[i]]$date, 1))

    # setar primeiro e último dia da série considerando mês de início do ano hidrológico

    firstDate <- as.Date(paste(firstYear, monthChosen, 01, sep = "-"))
    finalDate <- as.Date(paste(lastYear + 1, monthChosen, 01, sep = "-"))


    # se a série tem apenas um ano, firstYear e final Year são iguais, logo firstDate e finalDate também. Caso isso aconteça, setar NULL para a estação
    if (firstYear == lastYear) {
      serie[i] <- list(NULL)
    } else {
      serie[[i]] <- tibble::as_tibble_col(seq(firstDate, finalDate, by = 1),
        column_name = "date"
      ) %>% # construir uma série de datas com base em firstDate e finalDate, ou seja, a partir do Ano hidrológico
        filter(date < finalDate) %>%
        left_join(dados[[i]], by = "date") %>% # join série criada com dados da estação
        mutate(
          wateryear = as.numeric(as.character(lfstat::water_year(date,
            origin = monthChosen,
            assign = "start"
          ))),
          # monthWaterYear = paste(lubridate::month(date, abbr = FALSE), wateryear, sep = "-"),
          monthWaterYear = as.Date(paste("01", lubridate::month(date), wateryear, sep = "-"), format = "%d-%m-%Y"),
          monthYear = paste(lubridate::month(date), lubridate::year(date), sep = "-"),
          civilYear = lubridate::year(date)
        )


      serie[[i]] <- filter(serie[[i]], civilYear <= finalYear) #TODO aqui é gerado um erro se o ano final for menor que o civil
    }
  }

  names(serie) <- names(dados)

  serie[sapply(serie, function(x) is.null(x) == TRUE)] <- NULL

  serie <- lapply(serie, function(x) {
    names(x) <- c(names(x)[c(-4:-8)], "value", names(x)[c(5:8)])
    x
  }) # mudar nome da variável da coluna 4 para "value"


  # failure porcent water year

  # determinar anos que satisfazem porcentagem de falha mínima

  if (mode == "yearly") {
    yearsComplete <- list()

    for (i in 1:length(serie)) {
      yearsComplete[[i]] <- serie[[i]] %>%
        mutate(na = is.na(value)) %>% # verificar NAs
        group_by(wateryear) %>%
        dplyr::summarize(
          first = first(date),
          last = last(date),
          ndays = as.numeric(last(date) - first(date) + 1),
          na = sum(na)
        ) %>%
        mutate(
          porcFail = na / ndays * 100, # porcentagem de dados sem NA
          estacao = na.omit(unique(serie[[i]]$station_code)),
          porcent = porcFail <= maxFailurePorcent
        ) %>%
        filter(porcent == TRUE) %>%
        dplyr::select(wateryear, porcent)

      if (nrow(yearsComplete[[i]]) >= minimalYears) {
        serie[[i]] <- filter(serie[[i]], wateryear %in% yearsComplete[[i]]$wateryear)
      } else {
        serie[i] <- list(NULL)
      }
    }

    serie[sapply(serie, function(x) is.null(x) == TRUE)] <- NULL

    # data_frame contendo falhas das estações

    df <- Reduce(function(x, y) full_join(x, y, by = "wateryear"), yearsComplete)
    range <- as.data.frame(c(min(df$wateryear):max(df$wateryear)))
    names(range) <- "wateryear"
    df <- full_join(range, df, by = "wateryear")
    df <- df[, apply(df, MARGIN = 2, FUN = function(x) sum(x, na.rm = TRUE)) >= minimalYears]
    names(df) <- c("wateryear", names(serie))
    
    df = df %>% 
      dplyr::select(c("wateryear", sort(names(df)[-1])))


    x1 <- reshape2::melt(df, id.vars = "wateryear")

    plot(
      ggplot(x1, aes(wateryear, variable, fill = value)) +
        geom_tile(color = "black") +
        scale_fill_manual(values = c("green", "white")) +
        theme_bw() +
        theme(legend.position = "none")
    )

    # select station with more then X years
  } else {

    # failiure porcent month

    monthComplete <- list()

    for (i in 1:length(serie)) {
      monthComplete[[i]] <- serie[[i]] %>%
        mutate(na = is.na(value)) %>% # verificar NAs
        group_by(monthWaterYear) %>%
        dplyr::summarize(
          date = substr(first(date), 1, 7),
          daysMonth = lubridate::days_in_month(first(monthWaterYear)),
          count = length(na),
          na = sum(na)
        ) %>%
        mutate(
          porcFail = na / daysMonth * 100, # porcentagem de dados sem NA
          estacao = na.omit(unique(serie[[i]]$station_code)),
          porcent = porcFail <= maxFailurePorcent
        ) %>%
        filter(porcent == TRUE) %>%
        arrange(date)


      monthCompleteN <- monthComplete[[i]] %>%
        mutate(mes = substr(monthWaterYear, 6, 7)) %>%
        group_by(mes) %>%
        summarise(n_months = length(mes), goodMonth = n_months >= minimalYears) %>%
        summarise(monthComplete = sum(goodMonth))

      if (monthCompleteN == 12) {
        serie[[i]] <- filter(serie[[i]], monthWaterYear %in% monthComplete[[i]]$monthWaterYear)
      } else {
        serie[i] <- list(NULL)
      }
    }


    nomes <- names(serie[sapply(serie, function(x) is.null(x) == FALSE)])

    monthComplete <- monthComplete[sapply(serie, function(x) is.null(x) == FALSE)]

    serie[sapply(serie, function(x) is.null(x) == TRUE)] <- NULL

    # MonthlyWaterYear sequence


    df <- Reduce(
      function(x, y) full_join(x, y, by = "monthWaterYear"),
      lapply(monthComplete, function(x) {
        dplyr::select(
          x,
          monthWaterYear,
          porcent
        )
      })
    )


    range <- as.data.frame(seq(min(df$monthWaterYear),
      max(df$monthWaterYear),
      by = "1 month"
    ))

    names(range) <- "monthWaterYear"

    df <- full_join(range, df, by = "monthWaterYear")

    names(df) <- c("monthWaterYear", nomes)
    
    df = df %>% 
      dplyr::select(c("monthWaterYear", sort(names(df)[-1])))

    x <- reshape2::melt(df, id.vars = "monthWaterYear")

    print(
      ggplot(x, aes(monthWaterYear, variable, fill = value)) +
        geom_tile(color = "black") +
        scale_fill_manual(values = c("green", "white")) +
        theme_bw() +
        theme(legend.position = "none")
    )
  }

  # select station with more then "Nminimal" for each month



  #

  if (maxFailurePorcent == 0) {
    for (i in 1:length(serie)) {
      serie[[i]] <- serie[[i]] %>% mutate(maxFailurePorcent = "0")
    }
  } else {
    for (i in 1:length(serie)) {
      serie[[i]] <- serie[[i]] %>%
        mutate(maxFailurePorcent = paste("<= ", maxFailurePorcent, "%", sep = ""))
    }
  }

  if (stationType == "flu") {
    serie <- lapply(serie, function(x) {
      names(x) <- c(
        names(x)[c(-4:-9)],
        "stream_flow_m3_s",
        names(x)[c(5:9)]
      )
      x
    })
  } else {
    serie <- lapply(serie, function(x) {
      names(x) <- c(
        names(x)[c(-4:-9)],
        "rainfall_mm",
        names(x)[c(5:9)]
      )
      x
    })
  }

  serie <- list(serie, df)
  names(serie) <- c("serie", "FailureMatrix")
  return(serie)
}



# Estatística Anual Fluviométricas ----------------------------------------

flowStatistics = function(list, statistics = "Qmean", permanencia = 95)
{
  
  #flowStatistics tem como entrada o resultado de "selectStations"
  #Aqui � indentificado se foi realizada uma sele��o com base anual ou mensal
  #se for sele��o Anual, obter estat�stica anual, caso contr�rio ser� mensal
  
  if (names(list$FailureMatrix)[1]=="wateryear"){
    
    period = "wateryear"
    
  } else if(names(list$FailureMatrix)[1]=="monthWaterYear"){
    
    period = "monthWaterYear"
    
  } else {stop ("Please choose \"selectStations\" output result \"Annnual\" or \"Monthly\".")}
  
  list = list$serie
  
  if(statistics == "Qmean"){
    df = lapply(list, function(x) x %>% group_by_at(period) %>% 
                  summarise(Qmean = mean(stream_flow_m3_s, na.rm = TRUE)))
    
  } else if (statistics == "Q7"){
    
    
    df = lapply(list, function(x) x %>%
                  na.omit() %>%
                  group_by_at(period) %>% 
                  summarise(Q7 = min(zoo::rollapply(stream_flow_m3_s, 7, FUN = mean, partial = TRUE, align = "left"))))
    
  } else if (statistics == "Qperm"){
    
    df = lapply(list, function(x) x %>%
                  group_by_at(period) %>% 
                  summarise(Qperm = quantile(stream_flow_m3_s, 1 - permanencia/100, na.rm = TRUE)))
    
  } else if(statistics == "Qmin"){
    df = lapply(list, function(x) x %>% group_by_at(period) %>% 
                  summarise(Qmin = min(stream_flow_m3_s, na.rm = TRUE)))
    
  } else if(statistics == "Qmax"){
    df = lapply(list, function(x) x %>% group_by_at(period) %>% 
                  summarise(Qmax = max(stream_flow_m3_s, na.rm = TRUE)))
    
  } else {stop("Please choose \"statistics\" parameter among \"Qmean\", \"Qmin\", \"Qmax\" or \"Qperm\".")}
  
  # df = lapply(df, function(x) x %>% mutate(wateryear = ) %>% pad())
  
  return(df)  
  
}

# dfMean$`56719998` %>% mutate(wateryear = as.Date(wateryear))

# Fill Gaps ---------------------------------------------------------

fillGaps = function(lista, minimumCor = 0.84, minimunObsPairs = 10){
  
  if (names(lista[[1]])[1]=="wateryear"){
    
    period = "wateryear"
    
  } else if(names(lista[[1]])[1]=="monthWaterYear"){
    
    period = "monthWaterYear"
    minimunObsPairs = minimunObsPairs*12
    
  } else {stop ("Please choose \"selectStations\" output result \"Anual\" or \"Monthly\".")}
  

  resultados = list()
  
  df = Reduce(function(x, y) full_join(x, y, by = period), lista) %>% 
    arrange(across(starts_with(period)))

  names(df) = c(period, names(lista))
  
  corN = as_tibble(cor(df[,-1], use = "pairwise.complete.obs"))
  names(corN)
  preenchidos = list()

  
  for (i in 1:ncol(corN)){
    
    #estacao a ser preenchida
    estPrencher = names(corN)[i]
    estPrencher
    #identificar estação com maior correlação
    
    
    
    ordem = corN[i,] %>%
      t() %>%
      as.data.frame() %>%
      arrange(-V1) %>% 
      t() %>% 
      as_tibble() %>% 
      dplyr::select(-all_of(estPrencher))
      
    
    ordem = names(ordem)[ordem>=abs(minimumCor)]
    ordem
    
    if (length(na.omit(ordem))>0){ #caso exista estação com r > 0.84 #na.omit existe pois pode haver situações que não existe dados pareados gerando NA na correlão entre estações
      
      for (j in 1:length(na.omit(ordem))){ #para todas estações com r > 0.84
        
        if (nrow(na.omit(df[,c(period, estPrencher, ordem[j])]))>=minimunObsPairs){ #se o número de observações pareadas forem maior ou igual a "minimunObsPairs"
          
          regrdf = df[,c(period, estPrencher, ordem[j])] #df com estação a ser preenchida e estação que vai preencher
          
          names(regrdf) = c(period, "y", "x") #renomear para regressão
          
          df2 <- regrdf %>% filter(!is.na(y)) #retirar NAs da estação que será preenchida
          
          fit <- lm(y~x, data = df2) #regressão
          
          if (!exists("df3")){       #df3 é o df com dados preenchidos, mas pode ser que em uma rodada não preencha todos.
            
            df3 <- regrdf %>% 
              mutate(pred = predict(fit, .)) %>%
              # Replace NA with pred in var1
              mutate(preenchido = ifelse(is.na(y), pred, y)) %>% 
              dplyr::select(all_of(period), preenchido)
            
          } else {
            
            df3 = regrdf %>% 
              mutate(pred = predict(fit, .)) %>% 
              mutate(preenchido = ifelse(is.na(df3$preenchido), pred, df3$preenchido)) %>% 
              dplyr::select(all_of(period), preenchido)
            
          }
          
          
          if(sum(is.na(df3)[,2])==0) break
          
        }
      } 
      
      if (exists("df3")){
        
        preenchidos[[i]] = df3
        
        remove(df3) #remover df3 para próxima rodada (estação)
        
      } else {preenchidos[[i]] = df[,c(period, estPrencher)]}
      
      
    } else {preenchidos[[i]] = df[,c(period, estPrencher)]}
    
    names(preenchidos[[i]]) = c(period, names(lista)[i])
    
    preenchidos[[i]] = arrange(preenchidos[[i]], across(starts_with(period)))
  }
  
  names(preenchidos) = names(df[,-1])
  
  resultados[[1]] = preenchidos
  
  preenchidos = Reduce(function(x, y) full_join(x, y, by = period), preenchidos)
  
  
  preenchidos[,2:ncol(preenchidos)] = !is.na(preenchidos[,2:ncol(preenchidos)])
  
  preenchidos = preenchidos %>% 
    dplyr::select(c(all_of(period), sort(names(preenchidos)[-1])))
  
  resultados[[2]] = preenchidos
  
  names(resultados) = c('serie', "FailureMatrix")
  
  x1 = reshape2::melt(preenchidos, id.vars = period)
  
  print(
    ggplot(x1, aes(x1[,1], variable, fill = value)) +
      geom_tile(color="black") +
      scale_fill_manual(values = c("white", "green")) +
      theme_bw() +
      theme(legend.position = "none")
  )
  return(resultados)
}




#Statisticas pluviométricas

rainFallAnualStatit = function(list, statistcs = "Rtotal") 
  
  {#----
  
  if(statistcs == "Rtotal"){
    df = lapply(list, function(x) x %>% group_by(wateryear) %>% 
                  summarise(rainAnualTotal_mm = sum(rainfall_mm, na.rm = TRUE)))
    
  } else   if(statistcs == "Rmax"){
    
    df = lapply(list, function(x) x %>% group_by(wateryear) %>% 
                  summarise(rainAnualMax_mm = max(rainfall_mm, na.rm = TRUE)))
    
  } else {stop("Please choose \"statistics\" parameter among \"Rtotal\" or \"Rmax\"")}
  
  return(df)}



# list = dadosPluSelect

rainStatistics = function(list, statistics = "Rtotal")
{
  
  #flowStatistics tem como entrada o resultado de "selectStations"
  #Aqui � indentificado se foi realizada uma sele��o com base anual ou mensal
  #se for sele��o Anual, obter estat�stica anual, caso contr�rio ser� mensal
  
  if (names(list$FailureMatrix)[1]=="wateryear"){
    
    period = "wateryear"
    
  } else if(names(list$FailureMatrix)[1]=="monthWaterYear"){
    
    period = "monthWaterYear"
    
  } else {stop ("Please choose \"selectStations\" output result \"Annnual\" or \"Monthly\".")}
  
  list = list$serie
  
  if(statistics == "Rtotal"){
    df = lapply(list, function(x) x %>% group_by_at(period) %>% 
                  summarise(rainAnualTotal_mm = sum(rainfall_mm, na.rm = TRUE)))
    
  } else if (statistics == "Rmax"){
    
    
    df = lapply(list, function(x) x %>%
                  na.omit() %>%
                  group_by_at(period) %>% 
                  summarise(rainAnualMax_mm = max(rainfall_mm, na.rm = TRUE)))
    
    } else {stop("Please choose \"statistics\" parameter among \"Rtotal\" or \"Rmax\".")}
  
  return(df)
  }

