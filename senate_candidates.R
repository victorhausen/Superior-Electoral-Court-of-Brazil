library(electionsBR)
library(plyr)
library(dplyr)
library(xlsx)

data <- data.frame()

for(i in seq(1998, 2014, 4)){
  fed <- candidate_fed(i, encoding = "Latin-1")
  fed <- fed[c("NOME_CANDIDATO",
               "NUMERO_CANDIDATO",
               "IDADE_DATA_ELEICAO",
               "ANO_ELEICAO",
               "NUMERO_PARTIDO",
               "SIGLA_UF",
               "SIGLA_UF_NASCIMENTO",
               "DESCRICAO_CARGO",
               "DESCRICAO_SEXO",
               "NOME_MUNICIPIO_NASCIMENTO",
               "DESCRICAO_OCUPACAO",
               "DESC_SIT_TOT_TURNO",
               "DATA_NASCIMENTO")] %>%
    filter(DESCRICAO_CARGO == "SENADOR", DESC_SIT_TOT_TURNO == "ELEITO" | DESC_SIT_TOT_TURNO == "NÃO ELEITO")
    for(j in 1:nrow(fed)){
      if(!grepl("/", fed$DATA_NASCIMENTO[j])){
        d <- fed$DATA_NASCIMENTO[j] %>% strsplit("") %>% unlist()
        fed$DATA_NASCIMENTO[j] <- paste0(d[1], d[2], "/", d[3], d[4], "/", d[5], d[6], d[7], d[8])
        fed$IDADE_DATA_ELEICAO[j] <- as.numeric(fed$ANO_ELEICAO[j]) - as.numeric(paste0(d[5], d[6], d[7], d[8]))
      }
    }
    fed <- unique(fed)
    data <- rbind.fill(data, fed)
}

write.xlsx(sheetName = "Senadores", row.names = F, x = data, file = "senate_candidates_1998-2014.xlsx")
