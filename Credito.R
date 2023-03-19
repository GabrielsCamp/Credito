### 1. Pacotes -----------------------------------------------------

# 1.1 Pacotes
library(tidyverse)
library(tibble)
library(tseries)
library(quantmod)

### 2.Dados --------------------------------------------------------

# 2.1 Importando Selic
dados_selic <- rbcb::get_series(code = 4189, start_date = "2022-01-01", 
                                     end_date = Sys.Date()) %>% 
  dplyr::rename("selic" = "4189", "data" = "date") %>%
  dplyr::mutate(data = lubridate::ymd(data))

# 2.3 Importandos dados ICC
dados_icc <- rbcb::get_series(code = c("icc total" = 25351, "pj" = 25352, 
                                      "pf" = 25353 ),
                           start_date = "2022-01-01") %>%
  as.data.frame() %>%
  dplyr::select("data" = "icc.total.date", "icc_total" = "icc.total.icc.total",
                "icc_pj" = "pj.pj", "icc_pf" = "pf.pf") %>%
  dplyr::mutate(data = lubridate::ymd(data))

### 2.4 IPCA

# 2.4.1 - IPCA mensal
dados_ipca <- rbcb::get_series(code = 433, start_date = "2022-01-01", 
                                end_date = Sys.Date()) %>% 
  dplyr::rename("ipca_varm" = "433", "data" = "date") %>%
  dplyr::mutate(data = lubridate::ymd(data))

# 2.4.2 - IPCA acumulado 12 meses 
ipca_12m = sidrar::get_sidra(api = "/t/1737/n1/all/v/2265/p/all/d/v2265%202") %>%  
  dplyr::select("data" = "Mês (Código)",
                "ipca_12m" = "Valor") %>% 
  dplyr::mutate(data = lubridate::ym(data)) %>%
  dplyr::filter(data >= "2022-01-01") %>% 
  dplyr::as_tibble()

### 2.5 Inadimplência 

# 2.5.1 Inadimplência PF
inad_pf <- ipeadatar::ipeadata("BM12_CINPF12") %>%
  dplyr::select("inad_pf" = "value", "data" = "date") %>%
  dplyr::mutate(data = lubridate::ymd(data)) %>%
  dplyr::filter(data >= "2022-01-01") 

# 2.5.2 Inadimplência PJ
inad_pj <- ipeadatar::ipeadata("BM12_CINPJ12") %>%
  dplyr::select("inad_pj" = "value", "data" = "date") %>%
  dplyr::mutate(data = lubridate::ymd(data)) %>%
  dplyr::filter(data >= "2022-01-01") 

### 2.6 Concessão de crédito 

#2.6.1 Concessão de crédito PF
credito_pf <- rbcb::get_series(code = 20633, start_date = "2022-01-01", 
                               end_date = Sys.Date()) %>% 
  dplyr::rename("credito_pf" = "20633", "data" = "date") %>%
  dplyr::mutate(data = lubridate::ymd(data)) %>%
  dplyr::mutate(deflateBR::deflate(
    nominal_values = credito_pf,
    nominal_dates = data,
    real_date = "01/2023",
    index = "ipca"
  )) %>%
  dplyr::select(data, "credito_pf" = "deflateBR::deflate(...)")


#2.6.2 Concessão de crédito PJ
credito_pj <- rbcb::get_series(code = 20632, start_date = "2022-01-01", 
                               end_date = Sys.Date()) %>% 
  dplyr::rename("credito_pj" = "20632", "data" = "date") %>%
  dplyr::mutate(data = lubridate::ymd(data)) %>%
  dplyr::mutate(deflateBR::deflate(
    nominal_values = credito_pj,
    nominal_dates = data,
    real_date = "01/2023",
    index = "ipca"
  )) %>%
  dplyr::select(data, "credito_pj" = "deflateBR::deflate(...)")


# 2.6.3 Concessão de crédito total
credito_total <- rbcb::get_series(code = 20631, start_date = "2022-01-01", 
                               end_date = Sys.Date()) %>% 
  dplyr::rename("credito_total" = "20631", "data" = "date") %>%
  dplyr::mutate(data = lubridate::ymd(data)) %>%
  dplyr::mutate(deflateBR::deflate(
    nominal_values = credito_total,
    nominal_dates = data,
    real_date = "01/2023",
    index = "ipca"
  )) %>%
  dplyr::select(data, "credito_total" = "deflateBR::deflate(...)")

  
# 2.6 Juntado dados
dados <- purrr::reduce(
  .x = list(dados_selic, dados_icc, dados_ipca,ipca_12m, inad_pf, inad_pj,credito_total,
            credito_pf, credito_pj),
  .f = dplyr::full_join,
  by = "data"
)


### 3. Plots -------------------------------------------------------------------

# 3.1 Taxa Selic 
ggplot2::ggplot(dados)+
  ggplot2::aes(x = data, y = selic) +
  ggplot2::geom_line(color = "darkgreen")+
  ggplot2::labs(x = "Data", y = "Selic",
                title = " selic")+
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                 panel.grid.major = ggplot2::element_blank())


# 3.2 Custo de Crédito Total
ggplot2::ggplot(dados)+
  ggplot2::aes(x = data, y = inad_pf) +
  ggplot2::geom_line(color = "darkgreen")+
  ggplot2::labs(x = "Data", y = "Índice",
                title = "Custo de Crédito Total")+
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                 panel.grid.major = ggplot2::element_blank())

# 3.2.1 Custo de Crédito Pessoa Física
ggplot2::ggplot(dados)+
  ggplot2::aes(x = data, y = icc_pf) +
  ggplot2::geom_line(color = "darkblue")+
  ggplot2::labs(x = "Data", y = "Índice",
                title =  "Custo de Crédito Pessoa Física")+
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                 panel.grid.major = ggplot2::element_blank())


# 3.2.2 Custo de Crédito Pessoa Jurídica
ggplot2::ggplot(dados)+
  ggplot2::aes(x = data, y = icc_pj) +
  ggplot2::geom_line(color = "darkorange")+
  ggplot2::labs(x = "Data", y = "Índice",
                title = "Custo de crédito PJ") +
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                 panel.grid.major = ggplot2::element_blank())


# 3.3 Concessão de crédito Total
ggplot2::ggplot(dados)+
  ggplot2::aes(x = data, y = credito_total) +
  ggplot2::geom_line(color = "darkgreen")+
  ggplot2::labs(x = "Data", y = "Reais(milhões)",
                title = "Concessão de Crédito Total - deflacionado")+
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                 panel.grid.major = ggplot2::element_blank())

# 3.3.1 Concessão de crédito PF e Pj
ggplot2::ggplot(dados)+
  ggplot2::aes(x = data)+
  ggplot2::geom_line(aes(y = credito_pf, color = "credito Pessoa Física"))+
  ggplot2::geom_line(aes(y = credito_pj, color = "credito Pessoa Jurídica"))+
  ggplot2::scale_color_manual(values = c("darkblue", "darkorange")) +
  ggplot2::labs(x = "Data", y = "Reais(milhões)",
                title = "Concessão de Crédito PF e PJ - deflacionado")+
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                 panel.grid.major = ggplot2::element_blank())

# 3.4 Inadimplência 

# 3.4.1 Inadimplência Pessoa Física
ggplot2::ggplot(dados)+
  ggplot2::aes(x = data, y = inad_pf) +
  ggplot2::geom_line(color = "darkgreen")+
  ggplot2::labs(x = "Data", y = "ìndice",
                title = "Inadimplência Pessoa Física")+
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                 panel.grid.major = ggplot2::element_blank())

# 3.4.2 Inadimplência Pessoa Jurídica
ggplot2::ggplot(dados)+
  ggplot2::aes(x = data, y = inad_pj) +
  ggplot2::geom_line(color = "darkblue")+
  ggplot2::labs(x = "Data", y = "ìndice",
                title = "Inadimplência Pessoa Jurídica")+
  ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                 panel.grid.major = ggplot2::element_blank())


