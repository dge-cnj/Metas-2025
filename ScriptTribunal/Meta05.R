
Determinar.Meta05 <- function(tribunal) {
  
    outputFolder <- "output/Painel 2025/"
    if (!dir.exists(outputFolder)) dir.create(outputFolder, recursive = TRUE)      

    tbl_fato <- fread("tbl_fato_R.csv")
    tbl_fato <- tbl_fato %>% select(c(id_orgao_julgador, sigla_tribunal, ramo_justica, sigla_grau, procedimento, anomes, ano, mes, ind3, ind5))
    tbl_fato <- tbl_fato %>% filter(ano == "2023" | ano == "2024" | ano == "2025")
    tbl_fato <- tbl_fato %>% filter(!procedimento %in% c("Outros", "Fase investigatória", "Pré-processual", "Administrativo eleitoral"))
    tbl_fato <- tbl_fato %>% filter(sigla_tribunal == tribunal)
    
    
    unique(tbl_fato$procedimento)

    setDT(tbl_fato)

    tbl_fato <- tbl_fato %>% dtplyr::lazy_dt()
    tbl_fato <- tbl_fato %>% 
        group_by(id_orgao_julgador, sigla_tribunal, ramo_justica, sigla_grau, procedimento, anomes, ano, mes) %>% 
        summarise(
            ind3=sum(ind3, na.rm = TRUE),
            ind5=sum(ind5, na.rm = TRUE),
            .groups = "drop")
    tbl_fato <- tbl_fato %>% collect() %>% data.frame()

    # Flags da meta 5

    tbl_fato <- tbl_fato %>% 
        mutate(
            flg_m5 = if_else(sigla_tribunal == "STJ", 1,
                     if_else(sigla_tribunal == "TST" & procedimento != "Execução fiscal", 1,
                     if_else(ramo_justica == "Justiça Federal" & procedimento != "Execução fiscal", 1,
                     if_else(ramo_justica == "Justiça do Trabalho" & procedimento != "Execução fiscal", 1,
                     if_else(ramo_justica == "Justiça Estadual" & (sigla_grau == "G1" | sigla_grau == "JE") & (procedimento == "Conhecimento criminal" | procedimento == "Conhecimento não criminal"), 1,
                     if_else(sigla_tribunal == "STM" & sigla_grau == "G1" & (procedimento == "Conhecimento criminal" | procedimento == "Conhecimento não criminal"), 1,
                     if_else(ramo_justica == "Justiça Militar Estadual" & sigla_grau == "G1" & (procedimento == "Conhecimento criminal" | procedimento == "Conhecimento não criminal"), 1, 0))))))))

    tbl_fato <- tbl_fato %>% filter(flg_m5 == "1")

    # uf, nome_municipio

    nome_oj <- fread("Orgaos_julgadores_R.csv")
    nome_oj <- nome_oj %>% select(c("id_orgao_julgador","orgao_julgador", "uf_oj", "municipio_oj"))
    nome_oj <- distinct(nome_oj)

    tbl_fato <- tbl_fato %>% left_join(nome_oj, by=c("id_orgao_julgador"))
    tbl_fato <- tbl_fato %>% mutate(data_referencia = "31/12/2025")

    setDT(tbl_fato)

    meta5 <- tbl_fato %>% dtplyr::lazy_dt()
    meta5 <- meta5 %>% 
        group_by(ramo_justica, sigla_tribunal) %>% 
        summarise(
            pendente_liquido_2023 = sum(ind5[which(anomes == "202312")], na.rm = TRUE),
            baixados_2023 = sum(ind3[which(ano == "2023")], na.rm = TRUE),
            pendente_liquido_2024 = sum(ind5[which(anomes == "202412")], na.rm = TRUE),
            baixados_2024 = sum(ind3[which(ano == "2024")], na.rm = TRUE),
            pendente_liquido_2025 = sum(ind5[which(anomes == "202510")], na.rm = TRUE),
            baixados_2025 = sum(ind3[which(ano == "2025")], na.rm = TRUE) + sum(ind3[which(anomes == "202411"|anomes == "202412")], na.rm = TRUE),
            TCLC2025_trt = sum(ind5[which(anomes == "202510" & procedimento == "Conhecimento não criminal")], na.rm = TRUE)/(sum(ind5[which(anomes == "202510" & procedimento == "Conhecimento não criminal")], na.rm = TRUE)+sum(ind3[which(ano == "2025" & procedimento == "Conhecimento não criminal")], na.rm = TRUE) + sum(ind3[which((anomes == "202411"|anomes == "202412") & procedimento == "Conhecimento não criminal")], na.rm = TRUE)),
            TCLNFISC2025_trt = sum(ind5[which(anomes == "202510" & procedimento != "Execução fiscal")], na.rm = TRUE)/(sum(ind5[which(anomes == "202510" & procedimento != "Execução fiscal")], na.rm = TRUE)+sum(ind3[which(ano == "2025" & procedimento != "Execução fiscal")], na.rm = TRUE) + sum(ind3[which((anomes == "202411"|anomes == "202412") & procedimento != "Execução fiscal")], na.rm = TRUE)),
            .groups = "drop")

    meta5 <- meta5 %>%
        mutate(
            TCL2023 = pendente_liquido_2023/(pendente_liquido_2023 + baixados_2023),
            TCL2024 = pendente_liquido_2024/(pendente_liquido_2024 + baixados_2024),
            TCL2025 = pendente_liquido_2025/(pendente_liquido_2025 + baixados_2025))
    meta5 <- meta5 %>% collect() %>% data.frame()
    meta5 <- meta5 %>% 
        mutate(
            cumprimento_meta5 = if_else(sigla_tribunal == "STJ",((TCL2024-0.005)/TCL2025)*100,
                if_else(sigla_tribunal == "TST",((TCL2024-0.005)/TCL2025)*100,
                if_else(ramo_justica == "Justiça Federal",if_else((TCL2024-0.005)/TCL2025>=1,((TCL2024-0.005)/TCL2025)*100,if_else(TCL2025<=0.43,100,((TCL2024-0.005)/TCL2025)*100)),
                if_else(ramo_justica == "Justiça do Trabalho",if_else((TCL2024-0.005)/TCL2025>=1,((TCL2024-0.005)/TCL2025)*100,if_else((TCLC2025_trt<=0.4 & TCLNFISC2025_trt <= 0.65),100,((TCL2024-0.005)/TCL2025)*100)),
                if_else(ramo_justica == "Justiça Estadual",if_else((TCL2024-0.005)/TCL2025>=1,((TCL2024-0.005)/TCL2025)*100,if_else(TCL2025<=0.56,100,((TCL2024-0.005)/TCL2025)*100)),
                if_else(sigla_tribunal == "STM",((TCL2024-0.005)/TCL2025)*100,
                if_else(ramo_justica == "Justiça Militar Estadual",((TCL2023-0.005)/TCL2025)*100,0))))))))

    data.table::fwrite(tbl_fato, file = paste0(outputFolder, "meta5_2025.csv"), sep = ";", dec = ",", bom = T)
    data.table::fwrite(meta5, file = paste0(outputFolder, "meta5_2025_compilado.csv"), sep = ";", dec = ",", bom = T)

}




