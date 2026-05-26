
# Processamento de dados do STM

# 18/09/2025: Validação da versão paralelizada.

# Observações:

# 1) Dados do consolidado paralelo foram equivalentes aos do consolidado não paralelo, exceto TRT2 e TRT15
# 2) Apesar da diferença, os dados do consolidado paralelo foram equivalentes aos do painel não paralelo

# Meta 02

Trabalho.Meta02 <- function(dge) {
    
    t_f <- "2026-01-01" # Final do período atual
    t_0 <- "2025-01-01" # Início do período atual
    t_D <- "2024-01-01" # Data relativa à distribuição dos processos 
    t_ant <- "2021-01-01"  # Data referente a processos antigos
    
    dge <- dge %>% 
        mutate(
            primeirasentm2_a = if_else(data_total_primeiro_julgamento_sem_pronuncia >= t_D & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm2_a = if_else(data_total_primeira_baixa >= t_D & data_total_primeira_baixa < t_f, 1, 0),
            baixm2_a_ate = if_else(data_total_primeira_baixa < t_D | data_total_primeiro_julgamento_sem_pronuncia < t_D | data_total_primeiro_procedimento_resolvido < t_D, 1, 0),
            decm2_a = if_else(data_total_primeiro_procedimento_resolvido >= t_D & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism2_a = if_else(dt_recebimento < t_D & (data_total_primeiro_julgamento_sem_pronuncia >= t_D | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm2_a == "1") & (baixm2_a_ate == 0 | is.na(baixm2_a_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom2_a = if_else(dism2_a == "1" & (primeirasentm2_a == "1" | decm2_a == "1" | baixm2_a == "1"), 1, 0))
    dge <- dge %>% mutate(susm2_a = if_else(dism2_a == "1" & (julgadom2_a == "0" | is.na(julgadom2_a)) & pendente_meta == "0", 1, 0))
    
    # Mais antigos
    
    dge <- dge %>% 
        mutate(
            primeirasentm2_ant = if_else(data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            baixm2_ant = if_else(data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f, 1, 0),
            baixm2_ant_ate= if_else(data_total_primeira_baixa < t_0 | data_total_primeiro_julgamento_sem_pronuncia < t_0 | data_total_primeiro_procedimento_resolvido < t_0, 1, 0),
            decm2_ant = if_else(data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
            dism2_ant = if_else(dt_recebimento < t_ant & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | baixm2_ant == "1") & (baixm2_ant_ate == 0 | is.na(baixm2_ant_ate)), 1, 0))
    dge <- dge %>% mutate(julgadom2_ant = if_else(dism2_ant == "1" & (primeirasentm2_ant == "1" | decm2_ant == "1" | baixm2_ant == "1"), 1, 0))
    dge <- dge %>% mutate(susm2_ant = if_else(dism2_ant == "1" & (julgadom2_ant == "0" | is.na(julgadom2_ant)) & pendente_meta == "0", 1, 0))
    dge <- dge %>% mutate(desm2_ant = if_else(dism2_ant == "1" & (julgadom2_ant=="0"|is.na(julgadom2_ant)) & (susm2_ant=="0"|is.na(susm2_ant)) & desm1 == "1", 1, 0))
    
    return (dge)

}

# Meta 3 de 2025 – Estimular a Conciliação
# Aumentar o índice de conciliação na fase de conhecimento em 0,5 ponto percentual em relação à média do biênio 2022/2023 ou alcançar, no mínimo, 38% de conciliação.

Trabalho.Meta03 <- function(dge, pre_processual) {

    b_0 <- "2022-01-01" # Início do biênio
    b_f <- "2024-01-01" # Final do biênio
    t_0 <- "2025-01-01" # Início do período atual
    t_f <- "2026-01-01" # Final do período atual

    # Observação:

    # sent_ant: Diz respeito às sentenças não criminais, para o biênio k = 22_23
    # senth_ant: Diz respeito às conciliações, para o biênio k = 22_23
    # sent_atual: Diz respeito às sentenças não criminais, para o período atual
    # senth_atual: Diz respeito às conciliações, para o período atual

    dge <- dge %>% 
        mutate(
            sent22_23 = if_else(sent_arq_des != "1" & !func.procura.array(lista=c(74,110,1269,120,119,193,12226,12227,12228),base=dge,variavel="id_ultima_classe") & sigla_grau == "G1" & data_total_primeiro_julgamento_sem_pronuncia >= b_0 & data_total_primeiro_julgamento_sem_pronuncia < b_f, 1, 0),
            senth22_23 = if_else(!func.procura.array(lista=c(74,110,1269,120,119,193,12226,12227,12228),base=dge,variavel="id_ultima_classe") & sigla_grau == "G1" & dt_primeiro_julgamento_homologatorio >= b_0 & dt_primeiro_julgamento_homologatorio < b_f, 1, 0),
            sent_25 = if_else(sent_arq_des != "1" & !func.procura.array(lista=c(74,110,1269,120,119,193,12226,12227,12228),base=dge,variavel="id_ultima_classe") & sigla_grau == "G1" & data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
            senth_25 = if_else(!func.procura.array(lista=c(74,110,1269,120,119,193,12226,12227,12228),base=dge,variavel="id_ultima_classe") & sigla_grau == "G1" & dt_primeiro_julgamento_homologatorio >= t_0 & dt_primeiro_julgamento_homologatorio < t_f, 1, 0))
  
    meta3 <- dge %>% 
        group_by(sigla_tribunal) %>% 
        summarise(
            IC_ant = sum(senth22_23,na.rm = TRUE)/sum(sent22_23,na.rm = TRUE),
            IC_atual = sum(senth_25,na.rm = TRUE)/sum(sent_25,na.rm = TRUE),
            cumprimento_meta3 = if_else(IC_atual > (IC_ant+0.005),(IC_atual/(IC_ant+0.005))*100,if_else(IC_atual>="0.38",100,(IC_atual/(IC_ant+0.005))*100)))

    return (dge)

}

# Meta 5 de 2025 – Reduzir a taxa de congestionamento

# Reduzir em 0,5 ponto percentual a taxa de congestionamento líquida, exceto execuções fiscais, em relação a 2024. Cláusula de barreira na fase de conhecimento: 40% e Cláusula de barreira na fase de execução: 65%.

Trabalho.Meta05 <- function() {

}

# Hub principal, para a JT

ProcessarDados.Trabalho <- function(path, aux, JusticaTrabalho) {
    
    for (x in JusticaTrabalho) {

        RegistrarLOG(paste0("Processando dados do ", x))
            
        for (i in  seq_along(aux)){
            
            if (grepl(x, aux[i])) {

                RegistrarLOG(paste0("Lendo arquivo", aux[i]))
                
                temp <- iniciarFlags(path, aux, i)
                dge <- temp$dge
                pre_processual <- temp$pre_processual                
                
                dge <- Determinar.Meta01(dge)
                dge <- Trabalho.Meta02(dge)
                dge <- Trabalho.Meta03(dge)

                GerarArquivos(dge, unique(dge$sigla_tribunal)[1])
                rm (temp, pre_processual, dge)
                gc()
                
            }

        } # Fim do for (i in  seq_along(aux))

        RegistrarLOG(paste0("Encerrando processamento dos dados do ", x))
    
    }   
  
}

