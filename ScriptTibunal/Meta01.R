
# Meta 1 de 2025 – Julgar mais processos que os distribuídos
# Julgar quantidade maior de processos de conhecimento do que os distribuídos de 20/12/2024 a 19/12/2025, excluídos os suspensos e sobrestados de 20/12/2024 a 19/12/2025.

# Determinar.Meta01 (filtra processos que constam na meta 01)
# Versão: 18/06/2025

Determinar.Meta01 <- function(dge) {
        
    t_0 <- "2024-12-20"     # 1º dia do período de avaliação da Meta 1
    t_f <- "2025-12-20"     # último dia de avaliação da Meta 1
    ant_0 <- "2024-01-01"   # específico para a Justiça Eleitoral
    
    ref <- 20251220     
    
    dge <- dge %>% mutate(
        cnm1 = if_else((dt_recebimento >= t_0 & dt_recebimento <t_f), 1, 0),
        baixm1_ate = if_else(data_total_primeira_baixa < t_0 & !is.na(data_total_primeira_baixa), "1", "0"),
        primeirasentm1 = if_else(data_total_primeiro_julgamento_sem_pronuncia >= t_0 & data_total_primeiro_julgamento_sem_pronuncia < t_f, 1, 0),
        baixm1 = if_else(data_total_primeira_baixa >= t_0 & data_total_primeira_baixa < t_f & is.na(data_total_primeiro_julgamento_sem_pronuncia), 1, 0),
        decm1 = if_else(data_total_primeiro_procedimento_resolvido >= t_0 & data_total_primeiro_procedimento_resolvido < t_f, 1, 0),
        mpum1 = if_else(mpu_meta == TRUE & dt_primeira_medida_protetiva_meta >= t_0 & dt_primeira_medida_protetiva_meta < t_f, 1, 0))
    dge <- dge %>% mutate(
      cnm1 = if_else((id_ultima_classe == "12377" | id_ultima_classe == "12193") & dt_recebimento >= ant_0 & dt_recebimento < t_0 & (data_total_primeiro_julgamento_sem_pronuncia >= t_0 | is.na(data_total_primeiro_julgamento_sem_pronuncia) | data_total_primeira_baixa >= t_0), 1 , cnm1),
      cnm1 = if_else((id_ultima_classe == "12377" | id_ultima_classe == "12193") & dt_recebimento >= t_0 & dt_recebimento < t_f & (is.na(data_total_primeiro_julgamento_sem_pronuncia) | data_total_primeiro_julgamento_sem_pronuncia >= t_f), 0 , cnm1))
    dge <- dge %>% mutate (julgadom1 = if_else(baixm1_ate != "1" & (primeirasentm1 == "1" | decm1 == "1" | baixm1 == "1" | mpum1 == "1"), 1, 0))
    dge <- dge %>% mutate(desm1 = if_else(cnm1 != "1" & dt_recebimento < t_0 & flg_dessobrestado == "1" & (dt_resolvido >= t_0 | is.na(dt_resolvido)) & dt_resolvido >= ymd(ultimo_dessobrestado), 1, 0))
    dge <- dge %>% mutate(susm1 = if_else((cnm1 == "1" | desm1 == "1") & (julgadom1 == "0" | is.na(julgadom1)) & pendente_meta == "0", 1, 0))
        
    ref <- 20260101  ### Alterado para a meta 1
    
    return(dge)
  
}


